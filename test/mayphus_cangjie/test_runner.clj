(ns mayphus-cangjie.test-runner
  (:require [clojure.test :as t]
            mayphus-cangjie.data-import-test
            mayphus-cangjie.cangjie-test
            mayphus-cangjie.cangjie-tree-test))

(defn -main [& _]
  (let [{:keys [error fail]} (t/run-tests 'mayphus-cangjie.data-import-test
                                          'mayphus-cangjie.cangjie-test
                                          'mayphus-cangjie.cangjie-tree-test)]
    (System/exit (if (zero? (+ error fail)) 0 1))))
