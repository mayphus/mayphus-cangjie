(ns mayphus-cangjie.features.cangjie.tree
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            #?(:cljs ["d3-hierarchy" :as d3])))

(defn branch-columns [active-code]
  (let [active-code (or active-code "")
        prefixes (into [""]
                       (map #(subs active-code 0 %))
                       (range 1 (inc (count active-code))))]
    (mapv (fn [prefix]
            {:prefix prefix
             :selected-prefix (when (> (count active-code) (count prefix))
                                (subs active-code 0 (inc (count prefix))))
             :options (cangjie/next-step-groups prefix)})
          prefixes)))

(defn leaf-nodes [active-code exact-entry]
  (let [matches (when-not (str/blank? active-code)
                  (cangjie/matches-for-prefix active-code))]
    (cond
      exact-entry [exact-entry]
      (and (seq matches) (<= (count matches) 8) (>= (count active-code) 2)) matches
      :else [])))

(def node-width 120)
(def node-height 44)
(def x-step 176)
(def y-step 64)
(def root-id "root")

(defn root-node []
  {:id root-id
   :prefix ""
   :glyph "開始"
   :family "起點"
   :count nil
   :selected? true})

(defn manual-tree-layout [{:keys [entry prefix]}]
  (let [active-code (or prefix "")
        columns (branch-columns active-code)
        leaves (leaf-nodes active-code entry)
        root-node (assoc (root-node) :x 24 :y 132)]
    (loop [remaining columns
           depth 1
           parent-node root-node
           nodes [root-node]
           edges []]
      (if-let [{:keys [options selected-prefix]} (first remaining)]
        (let [count-options (count options)
              parent-center (+ (:y parent-node) (/ node-height 2))
              center-index (/ (dec (max count-options 1)) 2)
              laid-out (mapv (fn [index option]
                               (let [delta (* (- index center-index) y-step)
                                     y (- (+ parent-center delta) (/ node-height 2))]
                                 (assoc option
                                        :id (str "step-" depth "-" (:prefix option))
                                        :x (+ 90 (* depth x-step))
                                        :y y
                                        :selected? (= (:prefix option) selected-prefix))))
                             (range)
                             options)
              next-parent (or (first (filter :selected? laid-out))
                              parent-node)
              next-edges (into edges
                               (map (fn [node]
                                      {:from parent-node :to node}))
                               laid-out)]
          (recur (next remaining)
                 (inc depth)
                 next-parent
                 (into nodes laid-out)
                 next-edges))
        (let [leaf-x (+ (:x parent-node) 210)
              leaf-count (count leaves)
              leaf-center-index (/ (dec (max leaf-count 1)) 2)
              leaf-nodes (mapv (fn [index {:keys [char code]}]
                                 (let [delta (* (- index leaf-center-index) y-step)
                                       y (- (+ (:y parent-node) (/ node-height 2) delta)
                                            (/ node-height 2))]
                                   {:id (str "leaf-" char "-" code)
                                    :prefix code
                                    :glyph char
                                    :family "葉節點"
                                    :count nil
                                    :x leaf-x
                                    :y y
                                    :selected? (= code active-code)}))
                               (range)
                               leaves)
              all-nodes (into nodes leaf-nodes)
              all-edges (into edges
                              (map (fn [node]
                                     {:from parent-node :to node}))
                              leaf-nodes)
              ys (map :y all-nodes)
              min-y (- (apply min ys) 72)
              max-y (+ (apply max ys) 72)]
          {:node-width node-width
           :node-height node-height
           :view-box {:width (+ (apply max (map #(+ (:x %) node-width 40) all-nodes)) 40)
                      :height (- max-y min-y)}
           :y-offset (- min-y)
           :nodes all-nodes
           :edges all-edges})))))

(defn- layout-tree-data [active-code columns leaves]
  (letfn [(build-level [remaining depth parent]
            (if-let [{:keys [options selected-prefix]} (first remaining)]
              (let [children (mapv (fn [option]
                                     (let [selected? (= (:prefix option) selected-prefix)
                                           node {:id (str "step-" depth "-" (:prefix option))
                                                 :prefix (:prefix option)
                                                 :glyph (:glyph option)
                                                 :family (:family option)
                                                 :count (:count option)
                                                 :selected? selected?}]
                                       (if selected?
                                         (assoc node :children [])
                                         node)))
                                   options)
                    selected-node (some #(when (:selected? %) %) children)
                    nested-selected (when selected-node
                                      (build-level (next remaining) (inc depth) selected-node))
                    rewritten-children (if nested-selected
                                         (mapv (fn [child]
                                                 (if (= (:id child) (:id nested-selected))
                                                   nested-selected
                                                   child))
                                               children)
                                         children)]
                (assoc parent :children rewritten-children))
              (let [leaf-children (mapv (fn [{:keys [char code]}]
                                          {:id (str "leaf-" char "-" code)
                                           :prefix code
                                           :glyph char
                                           :family "葉節點"
                                           :count nil
                                           :selected? (= code active-code)})
                                        leaves)]
                (assoc parent :children leaf-children))))]
    (build-level columns 1 (root-node))))

#?(:cljs
   (defn- js-call [obj method]
     (js* "(~{}[~{}]).call(~{})" obj method obj)))

#?(:cljs
   (defn- d3-tree-layout [{:keys [entry prefix]}]
     (let [active-code (or prefix "")
           columns (branch-columns active-code)
           leaves (leaf-nodes active-code entry)
           tree-data (clj->js (layout-tree-data active-code columns leaves))
           hierarchy (.hierarchy d3 tree-data)
           layout (doto (.tree d3)
                    (.nodeSize #js [y-step x-step]))
           laid-out-hierarchy (layout hierarchy)
           descendants (array-seq (js-call laid-out-hierarchy "descendants"))
           nodes (mapv (fn [node]
                         (let [data (js->clj (.-data node) :keywordize-keys true)]
                           (assoc data
                                  :x (+ 24 (.-y node))
                                  :y (+ 132 (.-x node)))))
                       descendants)
           nodes-by-id (into {} (map (juxt :id identity) nodes))
           edges (mapv (fn [link]
                         (let [source-id (.. link -source -data -id)
                               target-id (.. link -target -data -id)]
                           {:from (get nodes-by-id source-id)
                            :to (get nodes-by-id target-id)}))
                       (array-seq (js-call laid-out-hierarchy "links")))
           ys (map :y nodes)
           min-y (- (apply min ys) 72)
           max-y (+ (apply max ys) 72)]
       {:node-width node-width
        :node-height node-height
        :view-box {:width (+ (apply max (map #(+ (:x %) node-width 40) nodes)) 40)
                   :height (- max-y min-y)}
        :y-offset (- min-y)
        :nodes nodes
        :edges edges})))

(defn tree-layout [{:keys [entry prefix] :as opts}]
  #?(:cljs (d3-tree-layout opts)
     :clj (manual-tree-layout opts)))
