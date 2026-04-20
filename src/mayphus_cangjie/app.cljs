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
        :graph-hint "Drag to move, scroll to zoom, and click a node to expand or collapse its children."
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
             :graph-hint "拖曳移動，滾輪縮放，點節點可展開或收合下一層分支。"
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
           :expanded-prefixes #{""}
           :zoom-state nil
           :drag-state nil}))

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

(defn default-zoom-state []
  {:x 0
   :y 0
   :k 1})

(defn transform-string [{:keys [x y k]}]
  (str "translate(" x "," y ") scale(" k ")"))

(defn fit-transform [{:keys [height width]}]
  (let [viewport-width (max 960 (min 1240 width))
        viewport-height (max 560 (min 860 height))
        scale (max 0.72 (min 1.02 (min (/ (- viewport-width 48) width)
                                        (/ (- viewport-height 48) height))))
        scaled-height (* height scale)
        scaled-width (* width scale)
        translate-x (/ (- viewport-width scaled-width) 2)
        translate-y (/ (- viewport-height scaled-height) 2)]
    {:x translate-x
     :y translate-y
     :k scale}))

(defn clamp-scale [value]
  (-> value
      (max 0.65)
      (min 2.4)))

(defn wheel-zoom-state [event {:keys [x y k] :as zoom-state}]
  (let [direction (if (pos? (.-deltaY event)) -1 1)
        next-k (clamp-scale (+ k (* direction 0.14)))
        scale-ratio (if (zero? k) 1 (/ next-k k))
        svg-rect (.getBoundingClientRect (.-currentTarget event))
        cursor-x (- (.-clientX event) (.-left svg-rect))
        cursor-y (- (.-clientY event) (.-top svg-rect))]
    {:x (- cursor-x (* (- cursor-x x) scale-ratio))
     :y (- cursor-y (* (- cursor-y y) scale-ratio))
     :k next-k}))

(defn drag-delta [event last-point]
  {:x (- (.-clientX event) (:x last-point))
   :y (- (.-clientY event) (:y last-point))})

(defn tree-svg-content [{:keys [edges nodes on-select transform]}]
  [:g {:transform transform}
   (for [{:keys [from to]} edges]
     ^{:key (str (:id from) "->" (:id to))}
     [:path {:class (str "cangjie-tree-edge" (when (:selected? to) " is-active"))
             :d (str "M " (:x from) " " (:y from)
                     " C " (:x from) " " (/ (+ (:y from) (:y to)) 2)
                     ", " (:x to) " " (/ (+ (:y from) (:y to)) 2)
                     ", " (:x to) " " (:y to))}])
   (for [{:keys [glyph id prefix selected? x y] :as node} nodes]
     ^{:key id}
     [:g {:class (str "cangjie-tree-visual-node" (when selected? " is-active"))
          :transform (str "translate(" x "," y ")")
          :on-click #(when prefix (on-select node))}
      [:circle {:class "cangjie-tree-node-frame"
                :r (if selected? 9 6)}]
      [:text {:class "cangjie-tree-node-line cangjie-tree-node-line-primary"
              :x (- (:label-x node) x)
              :y (- (:label-y node) y)
              :text-anchor (:text-anchor node)}
       glyph]])])

(defn interactive-tree-svg [{:keys [edges locale nodes view-box on-select zoom-state* drag-state*]}]
  (let [svg-node* (atom nil)
        wheel-handler* (atom nil)]
    (r/create-class
     {:display-name "cangjie-tree-svg"
      :component-did-mount
      (fn [_]
        (when-let [svg-node @svg-node*]
          (let [wheel-handler (fn [event]
                                (.preventDefault event)
                                (swap! zoom-state* #(wheel-zoom-state event %)))]
            (reset! wheel-handler* wheel-handler)
            (.addEventListener svg-node "wheel" wheel-handler #js {:passive false}))))
      :component-will-unmount
      (fn [_]
        (when-let [svg-node @svg-node*]
          (when-let [wheel-handler @wheel-handler*]
            (.removeEventListener svg-node "wheel" wheel-handler))))
      :reagent-render
      (fn [{:keys [edges locale nodes view-box on-select zoom-state* drag-state*]}]
        [:svg {:class "cangjie-tree-svg"
               :ref #(reset! svg-node* %)
               :viewBox (str "0 0 " (:width view-box) " " (:height view-box))
               :preserveAspectRatio "xMidYMid meet"
               :on-pointer-down (fn [event]
                                  (let [node-target (.closest (.-target event) ".cangjie-tree-visual-node")]
                                    (when (and (= 0 (.-button event))
                                               (nil? node-target))
                                    (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                                    (reset! drag-state* {:x (.-clientX event)
                                                         :y (.-clientY event)}))))
               :on-pointer-move (fn [event]
                                  (when-let [last-point @drag-state*]
                                    (let [{:keys [x y]} (drag-delta event last-point)]
                                      (swap! zoom-state* update :x + x)
                                      (swap! zoom-state* update :y + y)
                                      (reset! drag-state* {:x (.-clientX event)
                                                           :y (.-clientY event)}))))
               :on-pointer-up (fn [event]
                                (when @drag-state*
                                  (.releasePointerCapture (.-currentTarget event) (.-pointerId event))
                                  (reset! drag-state* nil)))
               :on-pointer-cancel #(reset! drag-state* nil)}
         [tree-svg-content {:edges edges
                            :nodes nodes
                            :on-select on-select
                            :transform (transform-string @zoom-state*)}]])})))

(defn select-entry! [entry]
  (swap! app-state* assoc
         :raw-query (:char entry)
         :selected-prefix (:code entry)
         :expanded-prefixes (path-prefixes (:code entry))
         :zoom-state nil
         :drag-state nil))

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
                  (cangjie/matches-for-prefix dataset effective-prefix))]
    {:dataset dataset
     :exact-entry exact-entry
     :active-entry active-entry
     :effective-prefix effective-prefix
     :featured-entries featured-entries
     :matches matches
     :locale locale
     :expanded-prefixes (into (or expanded-prefixes #{""})
                              (path-prefixes effective-prefix))
     :raw-query raw-query}))

(defn graph-controls [locale prefix match-count]
  [:div {:class "cangjie-graph-toolbar"}
   [:div {:class "cangjie-graph-copy"}
    [:p {:class "cangjie-graph-hint"} (tr locale :graph-hint)]
    [:p {:class "cangjie-graph-meta"}
     (if (str/blank? prefix)
       (tr locale :graph-root)
       (tformat (tr locale :graph-prefix)
                (str/upper-case prefix)
                (format-number locale match-count)))]]
   [:div {:class "cangjie-graph-actions"}
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click #(swap! app-state* assoc :zoom-state nil :drag-state nil)}
     (tr locale :button-fit)]
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click #(swap! app-state* assoc
                                :zoom-state (default-zoom-state)
                                :drag-state nil)}
     (tr locale :button-reset)]]])

(defn collapse-prefixes [expanded-prefixes prefix]
  (into #{""}
        (remove #(and (not= % "") (str/starts-with? % prefix)))
        expanded-prefixes))

(defn toggle-node! [{:keys [expandable? prefix]}]
  (swap! app-state*
         (fn [state]
           (let [expanded-prefixes (or (:expanded-prefixes state) #{""})
                 next-expanded (cond
                                 (not expandable?) expanded-prefixes
                                 (contains? expanded-prefixes prefix) (collapse-prefixes expanded-prefixes prefix)
                                 :else (conj expanded-prefixes prefix))]
             (assoc state
                    :selected-prefix prefix
                    :expanded-prefixes next-expanded
                    :zoom-state nil
                    :drag-state nil)))))

(defn tree-panel []
  (let [{:keys [dataset active-entry effective-prefix expanded-prefixes locale matches]} (current-view)
        zoom-state* (r/cursor app-state* [:zoom-state])
        drag-state* (r/cursor app-state* [:drag-state])
        {:keys [edges nodes view-box]}
        (tree/tree-layout {:dataset dataset
                           :entry active-entry
                           :prefix effective-prefix
                           :expanded-prefixes expanded-prefixes})
        zoom-state (or @zoom-state* (fit-transform view-box))]
    (when (nil? @zoom-state*)
      (reset! zoom-state* zoom-state))
    [:section {:class "atlas-card atlas-tree-card"}
     [graph-controls locale effective-prefix (count matches)]
     [:div {:class "cangjie-tree-canvas"}
      [interactive-tree-svg {:edges edges
                             :locale locale
                             :nodes nodes
                             :view-box view-box
                             :on-select toggle-node!
                             :zoom-state* zoom-state*
                             :drag-state* drag-state*}]]]))

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
  (let [{:keys [dataset locale raw-query]} @app-state*]
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
          (tr locale :locale-zh)]]]]
      [:div {:class "landing-copy"}
       [:p {:class "eyebrow"} (tr locale :tool-kicker)]
       [:h1 {:class "landing-title"} (tr locale :page-title)]
       [:p {:class "landing-lead"}
        (tr locale :lead)]
       [:p {:class "landing-meta"}
        (tformat (tr locale :entry-count)
                 (format-number locale (or (get-in dataset [:meta :entry-count])
                                           (count (:entries dataset)))))]]
      [:div {:class "tool-search-block"}
       [:label {:class "cangjie-search"}
        [:span {:class "cangjie-search-label"} (tr locale :search-label)]
        [:input {:class "cangjie-search-input"
                 :id "cangjie-query"
                 :name "cangjie-query"
                 :type "text"
                 :placeholder (tr locale :search-placeholder)
                 :value raw-query
                 :on-change #(swap! app-state* assoc
                                    :raw-query (.. % -target -value)
                                    :selected-prefix nil
                                    :expanded-prefixes #{""}
                                    :zoom-state nil
                                    :drag-state nil)}]]
       [:p {:class "landing-meta"}
        (tr locale :search-help)]]]
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
