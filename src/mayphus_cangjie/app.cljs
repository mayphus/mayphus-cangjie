(ns mayphus-cangjie.app
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.tree :as tree]
            [reagent.core :as r]
            ["react-dom/client" :as react-dom-client]))

(def translations
  {:en {:site-title "Cangjie 6 Tree"
        :site-description "Search the full Cangjie 6 dictionary with a tree view that highlights the active path."
        :nav-home "Home"
        :nav-dictionary "Dictionary"
        :tool-kicker "Tool"
        :page-title "Cangjie 6 Tree"
        :lead "Explore Cangjie structures visually instead of memorizing them as a flat table. Type a character, root, or code and the active path updates immediately."
        :entry-count "Full dictionary entries: %s."
        :search-label "Search character or code"
        :search-placeholder "For example: 照 / arf / 日口火 / 問"
        :search-help "Supports characters, letter codes, and root aliases."
        :graph-hint "Click a node to expand or collapse its children."
        :graph-root "Showing the first layer of root branches."
        :graph-prefix "Prefix %s · %s matches"
        :button-fit "Fit"
        :button-reset "Reset"
        :loading-kicker "Loading Dictionary"
        :loading-title "Loading the Cangjie 6 dictionary"
        :loading-copy "The static dictionary asset loads first, then the tree becomes interactive."
        :error-kicker "Load Failed"
        :error-title "The dictionary could not be loaded"
        :error-copy "Please refresh and try again."
        :footer-prefix "Source:"
        :footer-middle ". License preserved as "
        :footer-suffix ", with the dictionary compiled into a static browser asset."
        :locale-en "EN"
        :locale-zh "繁"
        :start "Start"
        :start-secondary "Start"
        :leaf "Leaf"
        :count-suffix "entries"
        :root-families {"a" "Sun / Say"
                        "b" "Moon"
                        "c" "Metal"
                        "d" "Wood"
                        "e" "Water"
                        "f" "Fire"
                        "g" "Earth"
                        "h" "Bamboo"
                        "i" "Halberd"
                        "j" "Ten"
                        "k" "Big"
                        "l" "Center"
                        "m" "One"
                        "n" "Bow"
                        "o" "Person"
                        "p" "Heart"
                        "q" "Hand"
                        "r" "Mouth"
                        "s" "Corpse"
                        "t" "Twenty"
                        "u" "Mountain"
                        "v" "Woman"
                        "w" "Field"
                        "x" "Difficult / Heavy"
                        "y" "Divination"}}
   :zh-Hant {:site-title "倉頡六代樹圖"
             :site-description "用完整倉頡六代字表搭配樹狀分支與高亮路徑，搜尋單字、字根與字碼。"
             :nav-home "Home"
             :nav-dictionary "Dictionary"
             :tool-kicker "Tool"
             :page-title "倉頡六代樹圖"
             :lead "Explore Cangjie structures visually instead of memorizing them as a flat table. 輸入單字、字根或英文字碼，畫面會同步更新高亮路徑。"
             :entry-count "完整字表條目 %s 筆。"
             :search-label "查詢字或字碼"
             :search-placeholder "例如：照 / arf / 日口火 / 問"
             :search-help "支援單字、英文字碼與字根別名。"
             :graph-hint "點節點可展開或收合下一層分支。"
             :graph-root "目前顯示第一層部件分支。"
             :graph-prefix "前綴 %s · %s 個符合字"
             :button-fit "適中"
             :button-reset "重設"
             :loading-kicker "Loading Dictionary"
             :loading-title "正在載入倉頡六代字表"
             :loading-copy "靜態資產會先載入完整碼表，再展開搜尋與樹圖。"
             :error-kicker "Load Failed"
             :error-title "字表沒有成功載入"
             :error-copy "請重新整理頁面再試一次。"
             :footer-prefix "資料來源："
             :footer-middle "。字表授權沿用 "
             :footer-suffix "，本站將原始碼表轉成靜態查詢資產後提供瀏覽。"
             :locale-en "EN"
             :locale-zh "繁"
             :start "開始"
             :start-secondary "Start"
             :leaf "葉節點"
             :count-suffix "字"
             :root-families {}}})

(defn detect-locale []
  (let [language (some-> js/navigator .-language str/lower-case)]
    (if (and language (str/starts-with? language "zh"))
      :zh-Hant
      :en)))

(defonce app-state*
  (r/atom {:status :loading
           :dataset cangjie/empty-dataset
           :error nil
           :locale (detect-locale)
           :raw-query ""
           :selected-prefix nil
           :expanded-prefixes #{""}}))

(defonce load-started? (atom false))

(defn format-number [locale value]
  (.toLocaleString value (if (= locale :zh-Hant) "zh-Hant-TW" "en-US")))

(defn tr [locale key]
  (get-in translations [locale key]))

(defn tformat [template & values]
  (reduce (fn [result value]
            (str/replace-first result "%s" (str value)))
          template
          values))

(defn path-prefixes [prefix]
  (into #{""}
        (map #(subs prefix 0 %))
        (range 1 (inc (count (or prefix ""))))))

(defn tree-svg-content [{:keys [nodes on-select]}]
   [:g
   (for [{:keys [glyph height id prefix selected? width x y] :as node} nodes
         :when (not (str/blank? prefix))
         :let [roomy? (and (> width 72) (> height 54))
               label-x (+ x (if roomy? 12 (/ width 2)))
               glyph-y (+ y (/ height 2))]]
     ^{:key id}
     [:g {:class (str "cangjie-icicle-node" (if selected? " is-active" " is-muted"))
          :on-click #(when prefix (on-select node))}
      [:rect {:class "cangjie-icicle-rect"
              :x x
              :y y
              :rx 10
              :ry 10
              :width (max 0 width)
              :height (max 0 height)}]
      (when (and (> width 28) (> height 22))
        [:text {:class "cangjie-icicle-label"
                :x label-x
                :y glyph-y
                :text-anchor (if roomy? "start" "middle")
                :dominant-baseline "central"}
         glyph])])])

(defn interactive-tree-svg [{:keys [nodes view-box on-select]}]
  [:svg {:class "cangjie-tree-svg"
         :viewBox (str "0 0 " (:width view-box) " " (:height view-box))
         :preserveAspectRatio "xMidYMid meet"}
   [tree-svg-content {:nodes nodes
                      :on-select on-select}]])

(defn select-entry! [entry]
  (swap! app-state* assoc
         :raw-query (:char entry)
         :selected-prefix (:code entry)
         :expanded-prefixes (path-prefixes (:code entry))))

(defn current-view []
  (let [{:keys [dataset expanded-prefixes raw-query selected-prefix locale]} @app-state*
        trimmed (str/trim raw-query)
        normalized (cangjie/normalize-query trimmed)
        exact-entry (when-not normalized
                      (cangjie/exact-char dataset trimmed))
        effective-prefix (or selected-prefix
                             normalized
                             (some-> exact-entry :code)
                             "")
        featured-entries (or (seq (:featured-entries dataset))
                             (take 6 (:entries dataset)))
        active-entry (or exact-entry (first featured-entries))
        matches (when-not (str/blank? effective-prefix)
                  (cangjie/matches-for-prefix dataset effective-prefix))
        exact-matches (when-not (str/blank? effective-prefix)
                        (->> matches
                             (filter #(= (:code %) effective-prefix))
                             vec))]
    {:dataset dataset
     :exact-entry exact-entry
     :active-entry active-entry
     :effective-prefix effective-prefix
     :featured-entries featured-entries
     :matches exact-matches
     :locale locale
     :expanded-prefixes (into (or expanded-prefixes #{""})
                              (path-prefixes effective-prefix))
     :raw-query raw-query}))

(defn collapse-prefixes [expanded-prefixes prefix]
  (into #{""}
        (remove #(and (not= % "") (str/starts-with? % prefix)))
        expanded-prefixes))

(defn toggle-node! [{:keys [expandable? prefix]}]
  (swap! app-state*
         (fn [state]
           (let [selected-prefix (:selected-prefix state)
                 next-expanded (cond
                                 (str/blank? prefix) #{""}
                                 (not expandable?) (path-prefixes prefix)
                                 (= prefix selected-prefix) (into #{""} (butlast (vec (path-prefixes prefix))))
                                 :else (path-prefixes prefix))]
             (assoc state
                    :selected-prefix (if (and expandable?
                                              (= prefix selected-prefix))
                                       (last (sort-by count next-expanded))
                                       prefix)
                    :expanded-prefixes next-expanded)))))

(defn candidate-strip [locale matches active-entry]
  (let [candidates (->> matches
                        (take 18)
                        vec)]
    (when (seq candidates)
      [:div {:class "cangjie-candidate-strip"}
       [:div {:class "cangjie-candidate-row"}
        (for [{:keys [char code] :as entry} candidates]
          ^{:key (str char "-" code)}
          [:button {:class (str "cangjie-candidate"
                                (when (= code (:code active-entry)) " is-active"))
                    :type "button"
                    :title (str char " " (str/upper-case code))
                    :on-click #(select-entry! entry)}
           [:span {:class "cangjie-candidate-char"} char]
           [:span {:class "cangjie-candidate-code"} (str/upper-case code)]])]])))

(defn tree-panel []
  (let [{:keys [dataset active-entry exact-entry effective-prefix expanded-prefixes locale matches]} (current-view)
        {:keys [edges nodes view-box]}
        (tree/tree-layout {:dataset dataset
                           :entry exact-entry
                           :prefix effective-prefix
                           :expanded-prefixes expanded-prefixes})]
    [:section {:class "atlas-card atlas-tree-card"}
     [candidate-strip locale matches active-entry]
     [:div {:class "cangjie-tree-canvas"}
      [interactive-tree-svg {:edges edges
                             :locale locale
                             :nodes nodes
                             :view-box view-box
                             :on-select toggle-node!}]]]))

(defn footer-panel []
  (let [{:keys [locale]} @app-state*
        {:keys [meta]} (:dataset @app-state*)]
    [:footer {:class "atlas-footer"}
     [:p
      (tr locale :footer-prefix)
      [:a {:href (:source-url meta)
           :rel "noreferrer"
           :target "_blank"}
       "rime-cangjie6"]
      (tr locale :footer-middle)
      (or (:license meta) "GPL")
      (tr locale :footer-suffix)]
     [:a {:class "site-link"
          :href "https://mayphus.org/"
          :rel "noreferrer"
          :target "_blank"}
      "mayphus.org"]]))

(defn ready-page []
  (let [{:keys [dataset locale]} @app-state*]
    [:div {:class "page landing-page"}
     [:header {:class "landing-header"}
      [:div {:class "landing-header-row"}
       [:a {:class "landing-brand"
            :href "https://mayphus.org/"
            :rel "noreferrer"
            :target "_blank"}
        "Mayphus"]
       [:nav {:class "landing-nav"}
        [:a {:href "https://mayphus.org/"
             :rel "noreferrer"
             :target "_blank"}
         (tr locale :nav-home)]
        [:a {:href "https://github.com/rime-aca/rime-cangjie6"
             :rel "noreferrer"
             :target "_blank"}
         (tr locale :nav-dictionary)]
        [:div {:class "locale-switch"}
         [:button {:class (str "locale-button" (when (= locale :en) " is-active"))
                   :type "button"
                   :on-click #(swap! app-state* assoc :locale :en)}
          (tr locale :locale-en)]
         [:button {:class (str "locale-button" (when (= locale :zh-Hant) " is-active"))
                   :type "button"
                   :on-click #(swap! app-state* assoc :locale :zh-Hant)}
          (tr locale :locale-zh)]]]]]
     [:section {:class "home-section tool-chart-section"}
      [tree-panel]]
     [footer-panel]]))

(defn loading-page []
  (let [{:keys [locale]} @app-state*]
    [:div {:class "page landing-page"}
     [:div {:class "atlas-loading"}
      [:p {:class "eyebrow"} (tr locale :loading-kicker)]
      [:h1 {:class "landing-title"} (tr locale :loading-title)]
      [:p {:class "landing-lead"} (tr locale :loading-copy)]]]))

(defn error-page []
  (let [{:keys [locale]} @app-state*]
    [:div {:class "page landing-page"}
     [:div {:class "atlas-loading"}
      [:p {:class "eyebrow"} (tr locale :error-kicker)]
      [:h1 {:class "landing-title"} (tr locale :error-title)]
      [:p {:class "landing-lead"} (or (:error @app-state*) (tr locale :error-copy))]]]))

(defn load-dataset! []
  (when-not @load-started?
    (reset! load-started? true)
    (-> (js/fetch "/data/cangjie6.json")
        (.then (fn [response]
                 (if (.-ok response)
                   (.json response)
                   (js/Promise.reject
                    (js/Error. (str "HTTP " (.-status response)))))))
        (.then (fn [payload]
                 (swap! app-state* assoc
                        :status :ready
                        :dataset (cangjie/build-dataset-from-payload
                                  (js->clj payload :keywordize-keys true))
                        :error nil)))
        (.catch (fn [error]
                  (swap! app-state* assoc
                         :status :error
                         :error (or (.-message error) "Unknown error")))))))

(defn shell []
  (let [{:keys [locale status]} @app-state*]
    (set! (.-lang (.-documentElement js/document)) (name locale))
    (set! (.-title js/document) (tr locale :site-title))
    [:main {:class "app-shell"}
     (case status
       :loading [loading-page]
       :error [error-page]
       [ready-page])]))

(defn init []
  (load-dataset!)
  (when-let [container (.getElementById js/document "app")]
    (.render (react-dom-client/createRoot container)
             (r/as-element [shell]))))
