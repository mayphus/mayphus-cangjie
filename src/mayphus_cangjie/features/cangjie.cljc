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

(def common-entries
  [{:char "日" :code "a"}
   {:char "月" :code "b"}
   {:char "金" :code "c"}
   {:char "木" :code "d"}
   {:char "水" :code "e"}
   {:char "火" :code "f"}
   {:char "土" :code "g"}
   {:char "戈" :code "i"}
   {:char "十" :code "j"}
   {:char "大" :code "k"}
   {:char "中" :code "l"}
   {:char "一" :code "m"}
   {:char "弓" :code "n"}
   {:char "人" :code "o"}
   {:char "心" :code "p"}
   {:char "手" :code "q"}
   {:char "口" :code "r"}
   {:char "尸" :code "s"}
   {:char "廿" :code "t"}
   {:char "山" :code "u"}
   {:char "女" :code "v"}
   {:char "田" :code "w"}
   {:char "卜" :code "y"}
   {:char "明" :code "ab" :note "先取日，再補月，適合拿來理解雙部件字如何往下走。"}
   {:char "好" :code "vnd" :note "女旁後接弓，再補木，能看到三層分支如何收斂。"}
   {:char "你" :code "onf"}
   {:char "我" :code "hqi"}
   {:char "是" :code "amyo"}
   {:char "時" :code "agdi"}
   {:char "間" :code "ana" :note "A 開頭後進到 N 支，再回到 A，可和「問」一起比較。"}
   {:char "問" :code "anr" :note "與「間」共用 AN 開頭，第三碼分到口。"}
   {:char "國" :code "wirm"}
   {:char "語" :code "yrmdr"}
   {:char "林" :code "dd"}
   {:char "森" :code "ddd"}
   {:char "想" :code "dup"}
   {:char "看" :code "hqbu"}
   {:char "說" :code "yrcru"}
   {:char "謝" :code "yrhhi"}
   {:char "學" :code "xbnd" :note "用 X 當補助碼開頭，是很好的特殊例子。"}
   {:char "愛" :code "bbphe"}
   {:char "家" :code "jmso"}
   {:char "照" :code "arf" :note "先從 A 的日／曰系分支進來，再沿著口、火走到葉節點。"}
   {:char "書" :code "lga"}
   {:char "電" :code "mzlwu"}
   {:char "體" :code "bbtwt"}
   {:char "醫" :code "semcw"}
   {:char "樂" :code "vid"}])

(defn enrich-entry [{:keys [char code] :as entry}]
  (assoc entry
         :code (str/lower-case code)
         :roots (mapv (fn [letter]
                        (get root-by-letter (str letter)
                             {:letter (str letter)
                              :glyph (str/upper-case (str letter))
                              :aliases []
                              :family "未知"
                              :hint ""}))
                      code)))

(def entries
  (mapv enrich-entry common-entries))

(def entry-by-char
  (into {} (map (juxt :char identity) entries)))

(def featured-characters ["照" "問" "間" "國" "學"])

(def ^:private featured-entries
  (mapv entry-by-char featured-characters))

(defn root-path [code]
  (mapv (fn [letter]
          (get root-by-letter (str letter)))
        code))

(defn exact-char [value]
  (get entry-by-char value))

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

(defn- code-query? [value]
  (boolean (normalize-query value)))

(defn matches-for-prefix [prefix]
  (let [normalized (normalize-query prefix)
        actual-prefix (or normalized "")]
    (->> entries
         (filter #(str/starts-with? (:code %) actual-prefix))
         (sort-by (juxt :code :char))
         vec)))

(defn next-step-groups [prefix]
  (let [matches (matches-for-prefix prefix)
        prefix-length (count (or (normalize-query prefix) ""))]
    (->> matches
         (keep (fn [{:keys [code] :as entry}]
                 (when (> (count code) prefix-length)
                   (let [next-letter (subs code prefix-length (inc prefix-length))
                         root (get root-by-letter next-letter)]
                     {:letter next-letter
                      :glyph (:glyph root)
                      :family (:family root)
                      :hint (:hint root)
                      :examples [(:char entry)]}))))
         (group-by :letter)
         (map (fn [[letter items]]
                (let [root (get root-by-letter letter)]
                  {:letter letter
                   :glyph (:glyph root)
                   :family (:family root)
                   :hint (:hint root)
                   :count (count items)
                   :examples (->> items (mapcat :examples) distinct (take 4) vec)
                   :prefix (str (or (normalize-query prefix) "") letter)})))
         (sort-by (juxt (comp - :count) :letter))
         vec)))

(defn node-for-prefix [prefix]
  (let [normalized (or (normalize-query prefix) "")]
    {:prefix normalized
     :path (root-path normalized)
     :entries (matches-for-prefix normalized)
     :next-steps (next-step-groups normalized)}))

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
