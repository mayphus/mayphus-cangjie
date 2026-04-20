(ns mayphus-cangjie.cangjie-test
  (:require [clojure.test :refer [deftest is testing]]
            [mayphus-cangjie.features.cangjie :as cangjie]))

(deftest exact-character-lookup
  (is (= "arf" (:code (cangjie/exact-char "照"))))
  (is (= "anr" (:code (cangjie/exact-char "問")))))

(deftest query-normalization
  (is (= "arf" (cangjie/normalize-query "ARF")))
  (is (= "arf" (cangjie/normalize-query "日口火")))
  (is (= "arf" (cangjie/normalize-query "曰口火")))
  (is (nil? (cangjie/normalize-query "照"))))

(deftest prefix-matching
  (is (= #{"問" "間"}
         (set (map :char (cangjie/matches-for-prefix "an")))))
  (is (= #{"照"}
         (set (map :char (cangjie/matches-for-prefix "arf"))))))

(deftest prefix-node-groups-next-steps
  (let [node (cangjie/node-for-prefix "a")
        letters (set (map :letter (:next-steps node)))]
    (is (contains? letters "b"))
    (is (contains? letters "n"))
    (is (contains? letters "r"))))

(deftest path-step-render-data
  (let [steps (cangjie/path-steps (cangjie/exact-char "照"))]
    (is (= ["日" "口" "火"] (mapv :glyph steps)))
    (is (= ["a" "ar" "arf"] (mapv :prefix steps)))))
