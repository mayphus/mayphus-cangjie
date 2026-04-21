(ns mayphus-cangjie.cangjie-tree-test
  (:require [clojure.test :refer [deftest is]]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.tree :as tree]
            [mayphus-cangjie.test-support :as test-support]))

(deftest branch-columns-follow-selected-prefix
  (let [dataset @test-support/dataset
        columns (tree/branch-columns dataset "arf")]
    (is (= "" (:prefix (first columns))))
    (is (= "a" (:selected-prefix (first columns))))
    (is (= "arf" (:prefix (last columns))))))

(deftest tree-layout-stops-at-five-code-rows
  (let [dataset @test-support/dataset
        layout (tree/tree-layout {:dataset dataset
                                  :entry (cangjie/exact-char dataset "磊")
                                  :prefix "mrmrr"})]
    (is (seq (:nodes layout)))
    (is (seq (:edges layout)))
    (is (not (some #(= "磊" (:glyph %)) (:nodes layout))))))

(deftest tree-layout-keeps-ancestor-prefixes-visible
  (let [dataset @test-support/dataset
        layout (tree/tree-layout {:dataset dataset
                                  :entry nil
                                  :prefix "arf"
                                  :expanded-prefixes #{"" "a" "ar" "arf"}})
        prefixes (set (map :prefix (:nodes layout)))]
    (is (contains? prefixes "a"))
    (is (contains? prefixes "ar"))
    (is (contains? prefixes "arf"))))
