(ns beverage-factory.core
  (:require [clojure.string :as str])
  (:import (java.util List)))

(def menu-items {:chai             {:price       4
                                    :ingredients {:main     :tea
                                                  :optional #{:milk :sugar :water}}}
                 :coffee           {:price       5
                                    :ingredients {:main     :coffee
                                                  :optional #{:milk :sugar :water}}}
                 :banana-smoothie  {:price       6
                                    :ingredients {:main     :banana
                                                  :optional #{:milk :sugar :water}}}
                 :strawberry-shake {:price       7
                                    :ingredients {:main     :strawberry
                                                  :optional #{:milk :sugar :water}}}
                 :mojito           {:price       7.5
                                    :ingredients {:main     :lemon
                                                  :optional #{:soda :sugar :water :mint}}}})


(def ingredients-price {:milk  1
                        :sugar 0.5
                        :soda  0.5
                        :mint  0.5
                        :water 0.5})

(def operations {:+ +, :- -})

(defn- retrieve-exclusion [customization]
  (->> customization
       (filter #(= :- (last %)))
       (map first)
       (into #{})))

(defn- contain-non-customized-ingredient? [menu-item customization]
  (->> customization
       (map first)
       (into #{})
       (#(% (get-in menu-items [menu-item :ingredients :main])))))

(defn- prepare-error-response [msg]
  (throw (ex-info msg {})))

(defn- contain-all-exclusions? [menu-item customization]
  (let [all-ingredients (get-in menu-items [menu-item :ingredients :optional])]
    (= all-ingredients (retrieve-exclusion customization)))
  )

(defn- contain-invalid-customization? [menu-item customization]
  (let [all-ingredients (get-in menu-items [menu-item :ingredients :optional])]
    (not-every? all-ingredients (map first customization)))
  )

(defn- validate-order [menu-item customization]
  (cond
    (nil? (menu-items menu-item)) (prepare-error-response "Order should contain menu item at first place")
    (contain-non-customized-ingredient? menu-item customization) (prepare-error-response "Invalid Order. Chief ingredients can't be excluded")
    (contain-all-exclusions? menu-item customization) (prepare-error-response "All the ingredients can't be excluded")
    (contain-invalid-customization? menu-item customization) (prepare-error-response "Invalid Customization")))

(defn- extract-customization
  "Return a list of the customization with the operation to be performed over it"
  [order-item]
  (->> order-item
      rest
        (map str/trim)
        (map (juxt #(subs % 1) #(subs % 0 1)))
        (map #(map keyword %))))


(defprotocol Process-Order
  "Processes the order"
  (process-order [order]))

(defn- calculate-price [menu-item ingredients]
  (let [menu-item-cost (get-in menu-items [menu-item :price])]
    (reduce (fn [total-price ingredient]
              (let [ingredient-price (ingredients-price (first ingredient))
                    operation (operations (last ingredient))]
                (operation total-price ingredient-price))
              ) menu-item-cost ingredients)))

(defn- refine-order [order]
  "split the order and lowercase all the items"
  (-> order
        (str/split  #",")
        (#(map str/lower-case %))))

(extend-protocol Process-Order
  String
  (process-order [order]
    (let [refined-order (refine-order order)
          menu-item (-> refined-order first keyword)
          customization (extract-customization refined-order)]
      (validate-order menu-item customization)
      (calculate-price menu-item customization)))

  List
  (process-order [order]
    (map process-order order)
    ))

(defn place-order [order]
  (process-order order))