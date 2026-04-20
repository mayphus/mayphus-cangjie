(ns mayphus-cangjie.data-import-test
  (:require [clojure.test :refer [deftest is testing]]
            [mayphus-cangjie.data-import :as data-import]
            [mayphus-cangjie.test-support :as test-support]))

(deftest parser-skips-header-and-extracts-rows
  (let [content (str "# encoding: utf-8\n"
                     "---\n"
                     "name: cangjie6\n"
                     "...\n"
                     "# comment\n"
                     "日\ta\t1\t\n"
                     "昌\taa\t2\taa'aa\n")
        entries (data-import/parse-dictionary content)]
    (is (= [{:char "日" :code "a" :weight 1 :stem nil}
            {:char "昌" :code "aa" :weight 2 :stem "aa'aa"}]
           entries))))

(deftest generated-json-exists-and-covers-known-entries
  (let [dataset @test-support/dataset]
    (testing "generated asset is present and large enough to be the real dictionary"
      (is (> (count (:entries dataset)) 100000)))
    (testing "known entries survive import"
      (is (= "arf" (:code ((:entry-by-char dataset) "照"))))
      (is (= "anr" (:code ((:entry-by-char dataset) "問"))))
      (is (= "a" (:code ((:entry-by-char dataset) "日")))))))
