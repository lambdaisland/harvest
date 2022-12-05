(ns lambdaisland.harvest-test
  (:require [clojure.test :refer [deftest testing is are]]
            [lambdaisland.harvest :as h]
            [lambdaisland.harvest.kernel :as hk]))

(h/defactory user
  {:name "Arne"})

(deftest basic-attributes
  (testing "factories can be built explicitly"
    (is (= {:name "Arne"} (h/build-val user))))
  (testing "overriding attributes"
    (is (= {:name "John"} (h/build-val user {:with {:name "John"}}))))
  (testing "additional attributes"
    (is (= {:name "Arne" :age 39} (h/build-val user {:with {:age 39}})))))

(h/defactory post
  {:title "Things To Do"
   :author (user {:with {:name "Tobi"}})})

(h/defactory post2
  {:author (h/build-val user {:with {:name "Tobi"}})})

(h/defactory admin
  :inherit user
  {:admin? true})

(deftest association-test
  (testing "expansion of nested factories is deferred"
    (is (hk/deferred-build? (get-in post [:harvest.factory/template :author])))
    (is (hk/deferred-build? (get-in post2 [:harvest.factory/template :author]))))

  (is (= {:title "Things To Do", :author {:name "Tobi"}}
         (h/build-val post)))

  (is (= {:title "Things To Do", :author {:name "Arne", :admin? true}}
         (h/build-val post {:with {:author admin}}))))

(deftest inheritance
  (is (= {:name "Arne", :admin? true} (h/build-val admin))))

(h/defactory line-item
  {:description "widget"
   :quantity 1
   :price 9.99}

  :traits {:discounted
           {:with
            {:price 0.99
             :discount "5%"}}})

(deftest traits
  (is (= {:description "widget", :quantity 1, :price 0.99, :discount "5%"}
         (h/build-val line-item :traits [:discounted]))))

(h/defactory dice-roll
  {:dice-type (constantly 6)
   :number-of-dice (constantly 2)})

(deftest evaluate-functions
  (is (= {:dice-type 6 :number-of-dice 2} (h/build-val dice-roll))))

(deftest selector-test
  (let [res (h/build post)]
    (is (= {:name "Tobi"} (h/sel1 res [:author])))))

(h/defactory multiple-hooks
  {:bar 1}

  :traits
  {:some-trait
   {:with {:bar 2}
    :after-build
    (fn [ctx]
      (h/update-result ctx update :bar inc))}}

  :after-build
  (fn [ctx]
    (h/update-result ctx update :bar #(- %))))

(deftest multiple-hooks-test
  (is (= {:bar -1} (h/build-val multiple-hooks)))

  (testing "hook is applied after :with overrides"
    (is (= {:bar -5} (h/build-val multiple-hooks {:with {:bar 5}}))))

  (testing "trait provides both override and hook, order is override > trait hook > top-level hook"
    (is (= {:bar -3} (h/build-val multiple-hooks {:traits [:some-trait]}))))

  (testing "trait and option override, trait override is ignored but hooks fire in right order and see override value"
    (is (= {:bar -10} (h/build-val multiple-hooks {:with {:bar 9} :traits [:some-trait]})))))

(h/defactory product
  {:sku "123"
   :price 12.99})

(h/defactory product-line-item
  {:product product
   :quantity 1}
  :traits
  {:balloon
   {:with
    {:product
     (product
      {:with {:sku "BAL" :price 0.99}})}}}
  :after-build
  (fn [ctx]
    (h/update-result
     ctx
     (fn [{:as res :keys [product quantity]}]
       (assoc res :total (* (:price product) quantity))))))

(deftest rules-test
  (is (= {:product {:sku "123" :price 7.5}
          :quantity 3
          :total 22.5}
         (h/build-val product-line-item {:rules {:quantity 3
                                                 :price 7.5}})))

  (is (= {:product {:sku "XYZ", :price 0.99}, :quantity 1, :total 0.99}
         (h/build-val product-line-item {:traits [:balloon]
                                         :rules {:sku "XYZ"}})))

  (is (= {:product {:price 1}, :quantity 1, :total 1}
         (h/build-val product-line-item {:rules {:product {:price 1}}}))))

(h/defactory f-a
  {:a #(rand-int 100)})

(h/defactory f-b
  {:a1 f-a
   :a2 f-a
   :b "b"})

(h/defactory f-c
  {:b1 f-b
   :b2 f-b
   :c "c"})

(deftest unification-test
  (is
   (let [v (f-c {:rules {[f-a] (h/unify)}})]
     (apply =
            (map #(get-in v %)
                 [[:b1 :a1 :a]
                  [:b1 :a2 :a]
                  [:b2 :a1 :a]
                  [:b2 :a2 :a]])))))
