(ns mayphus-cangjie.features.cangjie
  (:require [clojure.string :as str]))

(def root-defs
  [{:letter "a" :glyph "日" :aliases ["曰"] :family "日 / 曰" :hint "像日框或扁框時，先回到 A 這支比對。"}
   {:letter "b" :glyph "月" :aliases [] :family "月" :hint "月、肉旁都常落在 B。"}
   {:letter "c" :glyph "金" :aliases [] :family "金" :hint "金屬、釒旁先看 C。"}
   {:letter "d" :glyph "木" :aliases [] :family "木" :hint "木、朩、林系常從 D 展開。"}
   {:letter "e" :glyph "水" :aliases [] :family "水" :hint "水、氵通常歸到 E。"}
   {:letter "f" :glyph "火" :aliases [] :family "火" :hint "火、灬通常歸到 F。"}
   {:letter "g" :glyph "土" :aliases [] :family "土" :hint "土、士形先看 G。"}
   {:letter "h" :glyph "竹" :aliases [] :family "竹" :hint "竹字頭與細長筆勢常落在 H。"}
   {:letter "i" :glyph "戈" :aliases [] :family "戈" :hint "戈、弋、戊類筆形先看 I。"}
   {:letter "j" :glyph "十" :aliases [] :family "十" :hint "十、車、門框常從 J 展開。"}
   {:letter "k" :glyph "大" :aliases [] :family "大" :hint "大、人展開的寬形多看 K。"}
   {:letter "l" :glyph "中" :aliases [] :family "中" :hint "豎穿中的形通常歸到 L。"}
   {:letter "m" :glyph "一" :aliases [] :family "一" :hint "橫畫主導時先看 M。"}
   {:letter "n" :glyph "弓" :aliases [] :family "弓" :hint "弓、勹、子系常落在 N。"}
   {:letter "o" :glyph "人" :aliases [] :family "人" :hint "人、亻、入系先看 O。"}
   {:letter "p" :glyph "心" :aliases [] :family "心" :hint "心、忄通常歸到 P。"}
   {:letter "q" :glyph "手" :aliases [] :family "手" :hint "手、扌通常歸到 Q。"}
   {:letter "r" :glyph "口" :aliases [] :family "口" :hint "口與各種方框內口形先看 R。"}
   {:letter "s" :glyph "尸" :aliases [] :family "尸" :hint "尸、戶系先看 S。"}
   {:letter "t" :glyph "廿" :aliases [] :family "廿" :hint "草字頭與開口橫框常落在 T。"}
   {:letter "u" :glyph "山" :aliases [] :family "山" :hint "山、凵、屮形先看 U。"}
   {:letter "v" :glyph "女" :aliases [] :family "女" :hint "女與絞絲系常從 V 展開。"}
   {:letter "w" :glyph "田" :aliases [] :family "田" :hint "田、四角封閉形先看 W。"}
   {:letter "x" :glyph "難" :aliases ["重"] :family "難 / 重" :hint "X 是補助碼位，常見於重組或難拆字。"}
   {:letter "y" :glyph "卜" :aliases [] :family "卜" :hint "卜、丶、文系筆形常落在 Y。"}])

(def root-by-letter
  (into {} (map (juxt :letter identity) root-defs)))

(def glyph->letter
  (reduce
   (fn [acc {:keys [aliases glyph letter]}]
     (reduce #(assoc %1 %2 letter)
             (assoc acc glyph letter)
             aliases))
   {}
   root-defs))

(def featured-characters
  ["學" "照" "醫" "體" "問" "間" "國" "樂"])

(def empty-dataset
  {:meta nil
   :entries []
   :entry-by-char {}
   :featured-entries []
   :entries-by-prefix {}
   :prefix-groups {}})

(defn enrich-entry [{:keys [char code weight stem] :as entry}]
  (assoc entry
         :char char
         :code (str/lower-case code)
         :weight weight
         :stem stem
         :roots (mapv (fn [letter]
                        (get root-by-letter (str letter)
                             {:letter (str letter)
                              :glyph (str/upper-case (str letter))
                              :aliases []
                              :family "未知"
                              :hint ""}))
                      code)))

(defn entry-sort-key [{:keys [char code weight]}]
  [(count code)
   (- (or weight 0))
   char
   code])

(defn choose-better-entry [current candidate]
  (if (or (nil? current)
          (neg? (compare (entry-sort-key candidate)
                         (entry-sort-key current))))
    candidate
    current))

(defn- add-example [examples value]
  (let [with-value (if (some #(= value %) examples)
                     examples
                     (conj examples value))]
    (if (> (count with-value) 4)
      (subvec with-value 0 4)
      with-value)))

(defn- update-prefix-groups [prefix-groups {:keys [char code]}]
  (reduce (fn [acc prefix-length]
            (let [prefix (subs code 0 prefix-length)
                  next-letter (subs code prefix-length (inc prefix-length))]
              (update-in acc [prefix next-letter]
                         (fn [stats]
                           {:count (inc (or (:count stats) 0))
                            :examples (add-example (or (:examples stats) []) char)}))))
          prefix-groups
          (range 0 (count code))))

(defn- update-entries-by-prefix [entries-by-prefix {:keys [code] :as entry}]
  (reduce (fn [acc prefix-length]
            (update acc
                    (subs code 0 prefix-length)
                    (fnil conj [])
                    entry))
          entries-by-prefix
          (range 0 (inc (count code)))))

(defn- finalize-prefix-groups [prefix-groups]
  (into {}
        (map (fn [[prefix groups]]
               [prefix
                (->> groups
                     (map (fn [[letter {:keys [count examples]}]]
                            (let [root (get root-by-letter letter)]
                              {:letter letter
                               :glyph (:glyph root)
                               :family (:family root)
                               :hint (:hint root)
                               :count count
                               :examples examples
                               :prefix (str prefix letter)})))
                     (sort-by (juxt (comp - :count) :letter))
                     vec)]))
        prefix-groups))

(defn build-dataset
  ([entries]
   (build-dataset nil entries))
  ([meta entries]
   (let [prepared-entries (->> entries
                               (map enrich-entry)
                               (sort-by entry-sort-key)
                               vec)
         entry-by-char (reduce (fn [acc entry]
                                 (update acc (:char entry) choose-better-entry entry))
                               {}
                               prepared-entries)
         entries-by-prefix (reduce update-entries-by-prefix
                                   {}
                                   prepared-entries)
         prefix-groups (->> prepared-entries
                            (reduce update-prefix-groups {})
                            finalize-prefix-groups)
         featured-entries (->> featured-characters
                              (keep entry-by-char)
                              vec)]
     {:meta meta
      :entries prepared-entries
      :entry-by-char entry-by-char
      :entries-by-prefix entries-by-prefix
      :prefix-groups prefix-groups
      :featured-entries featured-entries})))

(defn compact-entry->entry [[char code weight stem]]
  {:char char
   :code code
   :weight weight
   :stem stem})

(defn build-dataset-from-payload [{:keys [entries meta]}]
  (build-dataset meta (map compact-entry->entry entries)))

(defn exact-char [dataset value]
  (get (:entry-by-char dataset) value))

(defn normalize-query [value]
  (let [trimmed (str/trim (or value ""))]
    (when-not (str/blank? trimmed)
      (loop [chars (seq trimmed)
             acc []]
        (if-let [ch (first chars)]
          (let [text (str/lower-case (str ch))]
            (cond
              (re-matches #"[a-y]" text)
              (recur (next chars) (conj acc text))

              (contains? glyph->letter (str ch))
              (recur (next chars) (conj acc (get glyph->letter (str ch))))

              (re-matches #"[\\s\\-_]+" text)
              (recur (next chars) acc)

              :else
              nil))
          (when (seq acc)
            (apply str acc)))))))

(defn matches-for-prefix [dataset prefix]
  (let [normalized (normalize-query prefix)
        actual-prefix (or normalized "")]
    (get (:entries-by-prefix dataset) actual-prefix [])))

(defn next-step-groups [dataset prefix]
  (get (:prefix-groups dataset) (or (normalize-query prefix) "") []))

(defn node-for-prefix [dataset prefix]
  (let [normalized (or (normalize-query prefix) "")]
    {:prefix normalized
     :entries (matches-for-prefix dataset normalized)
     :next-steps (next-step-groups dataset normalized)}))

(defn path-steps [{:keys [char code roots]}]
  (mapv (fn [index root]
          {:index (inc index)
           :prefix (subs code 0 (inc index))
           :letter (:letter root)
           :glyph (:glyph root)
           :family (:family root)
           :char char})
        (range (count roots))
        roots))
