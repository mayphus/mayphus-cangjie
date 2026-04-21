(ns mayphus-cangjie.cangjie-test
  (:require [clojure.test :refer [deftest is testing]]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.test-support :as test-support]))

(deftest exact-character-lookup
  (let [dataset @test-support/dataset]
    (is (= "arf" (:code (cangjie/exact-char dataset "照"))))
    (is (= "anr" (:code (cangjie/exact-char dataset "問"))))))

(deftest query-normalization
  (is (= "arf" (cangjie/normalize-query "ARF")))
  (is (= "arf" (cangjie/normalize-query "日口火")))
  (is (= "arf" (cangjie/normalize-query "曰口火")))
  (is (nil? (cangjie/normalize-query "照"))))

(deftest prefix-matching
  (let [dataset @test-support/dataset]
    (is (= (count (:entries dataset))
           (count (cangjie/matches-for-prefix dataset ""))))
    (is (contains? (set (map :char (cangjie/matches-for-prefix dataset "an"))) "問"))
    (is (contains? (set (map :char (cangjie/matches-for-prefix dataset "an"))) "間"))
    (is (contains? (set (map :char (cangjie/matches-for-prefix dataset "arf"))) "照"))))

(deftest prefix-node-groups-next-steps
  (let [dataset @test-support/dataset
        node (cangjie/node-for-prefix dataset "a")
        letters (set (map :letter (:next-steps node)))]
    (is (contains? letters "b"))
    (is (contains? letters "n"))
    (is (contains? letters "r"))))

(deftest prefix-results-remain-stably-sorted
  (let [dataset @test-support/dataset
        matches (take 6 (cangjie/matches-for-prefix dataset "aa"))]
    (is (= (sort-by cangjie/entry-sort-key matches)
           matches))))

(deftest path-step-render-data
  (let [dataset @test-support/dataset
        steps (cangjie/path-steps (cangjie/exact-char dataset "照"))]
    (is (= ["日" "口" "火"] (mapv :glyph steps)))
    (is (= ["a" "ar" "arf"] (mapv :prefix steps)))))
