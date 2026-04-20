(ns mayphus-cangjie.cangjie-tree-test
  (:require [clojure.test :refer [deftest is]]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.tree :as tree]))

(deftest branch-columns-follow-selected-prefix
  (let [columns (tree/branch-columns "arf")]
    (is (= "" (:prefix (first columns))))
    (is (= "a" (:selected-prefix (first columns))))
    (is (= "arf" (:prefix (last columns))))))

(deftest tree-layout-produces-leaf-for-exact-entry
  (let [layout (tree/tree-layout {:entry (cangjie/exact-char "照")
                                  :prefix "arf"})]
    (is (seq (:nodes layout)))
    (is (seq (:edges layout)))
    (is (some #(= "照" (:glyph %)) (:nodes layout)))))
