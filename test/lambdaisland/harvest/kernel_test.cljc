(ns lambdaisland.harvest.kernel-test
  (:require [clojure.test :refer [deftest testing is are]]
            [lambdaisland.harvest.kernel :as hk]))

(def my-factory ^{:type :harvest/factory} {:harvest.factory/id `my-factory})

(deftest path-match?-test
  (testing "matches if the last element of the path matches"
    (is (hk/path-match? [:a :b :c] [:c]))
    (is (not (hk/path-match? [:a :b :c] [:b]))))
  (testing "intermediate path elements don't need to match"
    (is (hk/path-match? [:a :b :c] [:a :c])))
  (testing "supports direct descendant with :>"
    (is (hk/path-match? [:a :b :c] [:b :> :c]))
    (is (not (hk/path-match? [:a :b :c] [:a :> :c]))))
  (testing ":> can anchor to the start"
    (is (hk/path-match? [:a :b :c] [:> :a :c])))
  (testing "matches factory instance to factory id"
    (is (hk/path-match? [:a :b `my-factory] [my-factory])))
  (testing "supports wildcard matching with :*"
    (is (hk/path-match? [:a :b :c] [:a :*]))
    (is (hk/path-match? [:a :b :c] [:* :b :> :c]))
    (is (hk/path-match? [:a :a :b :c] [:* :> :b :c]))
    (is (not (hk/path-match? [:a :b :c] [:c :*])))
    (is (not (hk/path-match? [:a :a :b :c] [:a :> :* :> :a :c])))
    (is (hk/path-match? [:a :a :b :c] [:> :a :> :a :c])))
  (testing "can match either factory (symbol) or map entry (keyword)"
    (is (hk/path-match? [`f1 :mk1 `f2 :mk2 `f3] [`f3]))
    (is (hk/path-match? [`f1 :mk1 `f2 :mk2 `f3] [:mk2]))
    (is (not (hk/path-match? [`f1 :mk1 `f2 :mk2 `f3] [`f2])))
    (is (not (hk/path-match? [`f1 :mk1 `f2 :mk2 `f3] [:mk1]))))
  (testing "will wrap non-sequence paths into a sequence"
    (is (hk/path-match? [:a :b] :b))
    (is (hk/path-match? [:a :b `c] `c))
    (is (hk/path-match? [:a :b `c] :*))
    (is (hk/path-match? [`my-factory] my-factory)))
  (testing "can match alternatives via sets"
    (is (hk/path-match? [:b] #{:a :b}))
    (is (hk/path-match? [:b] [#{:a :b}]))
    (is (hk/path-match? [:a :b :c] [#{:b} :> :*]))
    (is (hk/path-match? [:a :b :c] [#{:a :b} :> :*]))
    (is (not (hk/path-match? [:c :b] [#{:a :b} :> :*])))))

(deftest match1?-test
  (is (hk/match1? :x :x))
  (is (hk/match1? :x :*))
  (is (hk/match1? :x #{:x}))
  (is (hk/match1? `x ^{:type :harvest/factory} {:harvest.factory/id `x}))
  (is (hk/match1? `x #{^{:type :harvest/factory} {:harvest.factory/id `x}}))

  (is (not (hk/match1? :x :y))))
