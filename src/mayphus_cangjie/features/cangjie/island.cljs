(ns mayphus-cangjie.features.cangjie.island
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.tree :as tree]
            [reagent.core :as r]))

(defn tree-node-label [{:keys [family glyph]}]
  (if (#{"開始" "起點"} glyph)
    ["開始" "Start"]
    [glyph family]))

(defn default-zoom-state []
  {:x 0
   :y 0
   :k 1})

(defn transform-string [{:keys [x y k]}]
  (str "translate(" x "," y ") scale(" k ")"))

(defn fit-transform [{:keys [height width]}]
  (let [viewport-width 960
        viewport-height (max 420 (min 640 height))
        scale (max 0.72 (min 1.05 (min (/ (- viewport-width 64) width)
                                        (/ (- viewport-height 64) height))))
        scaled-height (* height scale)
        translate-x 24
        translate-y (/ (- viewport-height scaled-height) 2)]
    {:x translate-x
     :y translate-y
     :k scale}))

(defn graph-controls [{:keys [on-fit on-reset]}]
  [:div {:class "cangjie-graph-toolbar"}
   [:p {:class "cangjie-graph-hint"} "拖曳移動，滾輪縮放。"]
   [:div {:class "cangjie-graph-actions"}
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click on-fit}
     "適中"]
    [:button {:class "cangjie-graph-button"
              :type "button"
              :on-click on-reset}
     "重設"]]])

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

(defn tree-svg-content [{:keys [edges node-height node-width nodes y-offset on-select transform]}]
  [:g {:transform transform}
   (for [{:keys [from to]} edges]
     ^{:key (str (:id from) "->" (:id to))}
     [:path {:class "cangjie-tree-edge"
             :d (str "M " (+ (:x from) node-width) " " (+ y-offset (:y from) (/ node-height 2))
                     " C " (+ (:x from) node-width 48) " " (+ y-offset (:y from) (/ node-height 2))
                     ", " (- (:x to) 48) " " (+ y-offset (:y to) (/ node-height 2))
                     ", " (:x to) " " (+ y-offset (:y to) (/ node-height 2)))}])
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

(defn static-tree-svg [{:keys [edges node-height node-width nodes view-box y-offset on-select transform]}]
  [:svg {:class "cangjie-tree-svg"
         :viewBox (str "0 0 960 " (max 420 (min 640 (:height view-box))))
         :preserveAspectRatio "xMinYMin meet"}
   [tree-svg-content {:edges edges
                      :node-height node-height
                      :node-width node-width
                      :nodes nodes
                      :y-offset y-offset
                      :on-select on-select
                      :transform transform}]])

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
               :viewBox (str "0 0 960 " (max 420 (min 640 (:height view-box))))
               :preserveAspectRatio "xMinYMin meet"
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

(defn cangjie-tree-graph [{:keys [entry prefix on-select zoom-state*]}]
  (let [{:keys [edges node-height node-width nodes view-box y-offset]} (tree/tree-layout {:entry entry :prefix prefix})
        zoom-state (or @zoom-state* (fit-transform view-box))]
    (when (nil? @zoom-state*)
      (reset! zoom-state* zoom-state))
    (when-let [drag-state* (:drag-state* (meta zoom-state*))]
      (when (nil? @drag-state*)
        (reset! drag-state* nil)))
    [:section {:class "cangjie-tree-card"}
     [:div {:class "cangjie-section-head"}
      [:h2 {:class "rime-section-title"} "樹狀分支圖"]
      [:p {:class "cangjie-section-copy"} "點節點往下走。搜尋可以直接跳到某個字或某段字碼。"]]
     [graph-controls {:on-fit #(reset! zoom-state* (fit-transform view-box))
                      :on-reset #(reset! zoom-state* (default-zoom-state))}]
     [:div {:class "cangjie-tree-canvas"}
      (if (exists? js/document)
        [interactive-tree-svg {:edges edges
                               :node-height node-height
                               :node-width node-width
                               :nodes nodes
                               :view-box view-box
                               :y-offset y-offset
                               :on-select on-select
                               :zoom-state* zoom-state*
                               :drag-state* (:drag-state* (meta zoom-state*))}]
        [static-tree-svg {:edges edges
                          :node-height node-height
                          :node-width node-width
                          :nodes nodes
                          :view-box view-box
                          :y-offset y-offset
                          :on-select on-select
                          :transform (transform-string zoom-state)}])]]))

(defn cangjie-ready-view [{:keys [featured-entry raw-query selected-prefix on-query-change on-select-prefix zoom-state*]}]
  (let [trimmed (str/trim raw-query)
        normalized (cangjie/normalize-query trimmed)
        exact-entry (when-not normalized
                      (cangjie/exact-char trimmed))
        effective-prefix (or selected-prefix
                             normalized
                             (some-> exact-entry :code)
                             "")]
    [:div {:class "cangjie-shell"}
     [:section {:class "cangjie-hero-card"}
      [:div {:class "cangjie-hero-copy"}
       [:p {:class "eyebrow"} "倉頡樹圖"]
       [:h1 {:class "page-title"} "搜尋，然後沿著樹走"]
       [:p {:class "page-description"}
        "輸入單字、英文字母碼，或直接輸入字根。預設先看第一層，然後一層一層往下探索。"]]
      [:label {:class "cangjie-search"}
       [:span {:class "cangjie-search-label"} "查詢"]
       [:input {:class "cangjie-search-input"
                :id "cangjie-query"
                :name "cangjie-query"
                :type "text"
                :placeholder "例如：照 / arf / 日口火 / 曰"
                :value raw-query
                :on-change #(on-query-change (.. % -target -value))}]]]
     [cangjie-tree-graph {:entry (or exact-entry featured-entry)
                          :prefix effective-prefix
                          :on-select on-select-prefix
                          :zoom-state* zoom-state*}]]))

(defn cangjie-island [{:keys [featured-entry]}]
  (let [raw-query* (r/atom "")
        selected-prefix* (r/atom nil)
        zoom-state* (with-meta (r/atom nil) {:drag-state* (r/atom nil)})]
    (fn [_]
      [cangjie-ready-view
       {:featured-entry featured-entry
        :raw-query @raw-query*
        :selected-prefix @selected-prefix*
        :on-query-change (fn [value]
                           (reset! raw-query* value)
                           (reset! selected-prefix* nil)
                           (reset! zoom-state* nil))
        :on-select-prefix #(do
                             (reset! selected-prefix* (cangjie/normalize-query %))
                             (reset! zoom-state* nil))
        :zoom-state* zoom-state*}])))
