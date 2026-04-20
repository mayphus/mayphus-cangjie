(ns mayphus-cangjie.test-support
  (:require [clojure.data.json :as json]
            [mayphus-cangjie.features.cangjie :as cangjie]))

(def dataset
  (delay
    (-> "public/data/cangjie6.json"
        slurp
        (json/read-str :key-fn keyword)
        cangjie/build-dataset-from-payload)))
