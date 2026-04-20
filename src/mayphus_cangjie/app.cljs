(ns mayphus-cangjie.app
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.tree :as tree]
            [reagent.core :as r]
            ["react-dom/client" :as react-dom-client]))

(defonce app-state*
  (r/atom {:status :loading
           :dataset cangjie/empty-dataset
           :error nil
           :raw-query ""
           :selected-prefix nil
           :zoom-state nil
           :drag-state nil}))

(defonce load-started? (atom false))

(defn format-number [value]
  (.toLocaleString value "zh-Hant-TW"))

(defn default-zoom-state []
  {:x 0
   :y 0
   :k 1})

(defn transform-string [{:keys [x y k]}]
  (str "translate(" x "," y ") scale(" k ")"))

(defn fit-transform [{:keys [height width]}]
  (let [viewport-width (max 960 (min 1240 width))
        viewport-height (max 460 (min 760 height))
        scale (max 0.74 (min 1.08 (min (/ (- viewport-width 64) width)
                                        (/ (- viewport-height 64) height))))
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

(defn tree-node-label [{:keys [family glyph]}]
  (if (#{"開始" "起點"} glyph)
    ["開始" "Start"]
    [glyph family]))

(defn tree-svg-content [{:keys [edges node-height node-width nodes y-offset on-select transform]}]
  [:g {:transform transform}
   (for [{:keys [from to]} edges]
     ^{:key (str (:id from) "->" (:id to))}
     [:path {:class (str "cangjie-tree-edge" (when (:selected? to) " is-active"))
             :d (let [from-x (+ (:x from) node-width)
                      from-y (+ y-offset (:y from) (/ node-height 2))
                      to-x (:x to)
                      to-y (+ y-offset (:y to) (/ node-height 2))
                      mid-x (/ (+ from-x to-x) 2)]
                  (str "M " from-x " " from-y
                       " C " mid-x " " from-y
                       ", " mid-x " " to-y
                       ", " to-x " " to-y))}])
   (for [{:keys [count id prefix selected? x y] :as node} nodes
         :let [[line-1 line-2] (tree-node-label node)]]
     ^{:key id}
     [:g {:class (str "cangjie-tree-visual-node" (when selected? " is-active"))
          :transform (str "translate(" x "," (+ y y-offset) ")")
          :on-click #(when prefix (on-select prefix))}
      [:rect {:class "cangjie-tree-node-frame"
              :rx "18"
              :ry "18"
              :width node-width
              :height node-height}]
      [:text {:class "cangjie-tree-node-line cangjie-tree-node-line-primary"
              :x (/ node-width 2)
              :y 20}
       line-1]
      [:text {:class "cangjie-tree-node-line cangjie-tree-node-line-secondary"
              :x (/ node-width 2)
              :y 38}
       (if count
         (str line-2 " · " count " 字")
         line-2)]])])

(defn interactive-tree-svg [{:keys [edges node-height node-width nodes view-box y-offset on-select zoom-state* drag-state*]}]
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
      (fn [{:keys [edges node-height node-width nodes view-box y-offset on-select zoom-state* drag-state*]}]
        [:svg {:class "cangjie-tree-svg"
               :ref #(reset! svg-node* %)
               :viewBox (str "0 0 " (max 960 (:width view-box)) " " (max 460 (min 760 (:height view-box))))
               :preserveAspectRatio "xMidYMid meet"
               :on-pointer-down (fn [event]
                                  (when (= 0 (.-button event))
                                    (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                                    (reset! drag-state* {:x (.-clientX event)
                                                         :y (.-clientY event)})))
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
                            :node-height node-height
                            :node-width node-width
                            :nodes nodes
                            :y-offset y-offset
                            :on-select on-select
                            :transform (transform-string @zoom-state*)}]])})))

(defn select-entry! [entry]
  (swap! app-state* assoc
         :raw-query (:char entry)
         :selected-prefix (:code entry)
         :zoom-state nil
         :drag-state nil))

(defn current-view []
  (let [{:keys [dataset raw-query selected-prefix]} @app-state*
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
     :raw-query raw-query}))

(defn graph-controls [prefix match-count]
  [:div {:class "cangjie-graph-toolbar"}
   [:div {:class "cangjie-graph-copy"}
    [:p {:class "cangjie-graph-hint"} "拖曳移動，滾輪縮放，點節點可直接沿著字碼往下走。"]
    [:p {:class "cangjie-graph-meta"}
     (if (str/blank? prefix)
       "目前顯示第一層部件分支。"
       (str "前綴 " (str/upper-case prefix) " · "
            (format-number match-count) " 個符合字"))]]
   [:div {:class "cangjie-graph-actions"}
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click #(swap! app-state* assoc :zoom-state nil :drag-state nil)}
     "適中"]
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click #(swap! app-state* assoc
                                :zoom-state (default-zoom-state)
                                :drag-state nil)}
     "重設"]]])

(defn tree-panel []
  (let [{:keys [dataset active-entry effective-prefix matches]} (current-view)
        zoom-state* (r/cursor app-state* [:zoom-state])
        drag-state* (r/cursor app-state* [:drag-state])
        {:keys [edges node-height node-width nodes view-box y-offset]}
        (tree/tree-layout {:dataset dataset
                           :entry active-entry
                           :prefix effective-prefix})
        zoom-state (or @zoom-state* (fit-transform view-box))]
    (when (nil? @zoom-state*)
      (reset! zoom-state* zoom-state))
    [:section {:class "atlas-card atlas-tree-card"}
     [graph-controls effective-prefix (count matches)]
     [:div {:class "cangjie-tree-canvas"}
      [interactive-tree-svg {:edges edges
                             :node-height node-height
                             :node-width node-width
                             :nodes nodes
                             :view-box view-box
                             :y-offset y-offset
                             :on-select #(swap! app-state* assoc
                                                :selected-prefix (cangjie/normalize-query %)
                                                :zoom-state nil
                                                :drag-state nil)
                             :zoom-state* zoom-state*
                             :drag-state* drag-state*}]]]))

(defn footer-panel []
  (let [{:keys [meta]} (:dataset @app-state*)]
    [:footer {:class "atlas-footer"}
     [:p
      "資料來源："
      [:a {:href (:source-url meta)
           :rel "noreferrer"
           :target "_blank"}
       "rime-cangjie6"]
      "。字表授權沿用 "
      (or (:license meta) "GPL")
      "，本站將原始碼表轉成靜態查詢資產後提供瀏覽。"]
     [:a {:class "site-link"
          :href "https://mayphus.org/"
          :rel "noreferrer"
          :target "_blank"}
      "mayphus.org"]]))

(defn ready-page []
  (let [{:keys [dataset raw-query]} @app-state*]
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
         "Home"]
        [:a {:href "https://github.com/rime-aca/rime-cangjie6"
             :rel "noreferrer"
             :target "_blank"}
         "Dictionary"]]]
      [:div {:class "landing-copy"}
       [:p {:class "eyebrow"} "Tool"]
       [:h1 {:class "landing-title"} "倉頡六代樹圖"]
       [:p {:class "landing-lead"}
        "Explore Cangjie structures visually instead of memorizing them as a flat table. 輸入單字、字根或英文字碼，畫面會同步更新高亮路徑。"]
       [:p {:class "landing-meta"}
        (str "完整字表條目 "
             (format-number (or (get-in dataset [:meta :entry-count])
                                (count (:entries dataset))))
             " 筆。")]]
      [:div {:class "tool-search-block"}
       [:label {:class "cangjie-search"}
        [:span {:class "cangjie-search-label"} "查詢字或字碼"]
        [:input {:class "cangjie-search-input"
                 :id "cangjie-query"
                 :name "cangjie-query"
                 :type "text"
                 :placeholder "例如：照 / arf / 日口火 / 問"
                 :value raw-query
                 :on-change #(swap! app-state* assoc
                                    :raw-query (.. % -target -value)
                                    :selected-prefix nil
                                    :zoom-state nil
                                    :drag-state nil)}]]
       [:p {:class "landing-meta"}
        "支援單字、英文字碼與字根別名。"]]]
     [:section {:class "home-section tool-chart-section"}
      [tree-panel]]
     [footer-panel]]))

(defn loading-page []
  [:div {:class "page landing-page"}
   [:div {:class "atlas-loading"}
    [:p {:class "eyebrow"} "Loading Dictionary"]
    [:h1 {:class "landing-title"} "正在載入倉頡六代字表"]
    [:p {:class "landing-lead"} "靜態資產會先載入完整碼表，再展開搜尋與樹圖。"]]])

(defn error-page []
  [:div {:class "page landing-page"}
   [:div {:class "atlas-loading"}
    [:p {:class "eyebrow"} "Load Failed"]
    [:h1 {:class "landing-title"} "字表沒有成功載入"]
    [:p {:class "landing-lead"} (or (:error @app-state*) "請重新整理頁面再試一次。")]]])

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
  [:main {:class "app-shell"}
   (case (:status @app-state*)
     :loading [loading-page]
     :error [error-page]
     [ready-page])])

(defn init []
  (load-dataset!)
  (when-let [container (.getElementById js/document "app")]
    (.render (react-dom-client/createRoot container)
             (r/as-element [shell]))))
