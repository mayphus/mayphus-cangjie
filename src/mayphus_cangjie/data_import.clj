(ns mayphus-cangjie.data-import
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def source-path "data/cangjie6.dict.yaml")
(def output-path "public/data/cangjie6.json")
(def source-url "https://github.com/rime-aca/rime-cangjie6/blob/master/cangjie6.dict.yaml")
(def source-license "GPL")

(defn parse-weight [value]
  (when-let [text (some-> value str/trim not-empty)]
    (Long/parseLong text)))

(defn parse-entry-line [line]
  (let [[text code weight stem] (str/split line #"\t" 4)
        normalized-code (some-> code str/trim str/lower-case)]
    (when (and (seq text)
               (re-matches #"[a-y]+" (or normalized-code "")))
      {:char text
       :code normalized-code
       :weight (parse-weight weight)
       :stem (some-> stem str/trim not-empty)})))

(defn parse-dictionary [content]
  (->> (str/split-lines content)
       (drop-while #(not= "..." %))
       next
       (remove #(or (str/blank? %)
                    (str/starts-with? % "#")))
       (keep parse-entry-line)
       vec))

(defn compact-entry [{:keys [char code weight stem]}]
  [char code weight stem])

(defn build-payload [entries]
  {:meta {:title "Rime Cangjie 6"
          :source-url source-url
          :license source-license
          :entry-count (count entries)}
   :entries (mapv compact-entry entries)})

(defn generate-json! []
  (let [content (slurp source-path)
        entries (parse-dictionary content)
        output-file (io/file output-path)]
    (.mkdirs (.getParentFile output-file))
    (spit output-file (json/write-str (build-payload entries)))
    {:entries (count entries)
     :output output-path}))

(defn -main [& _]
  (let [{:keys [entries output]} (generate-json!)]
    (println (str "Generated " output " with " entries " entries."))))
