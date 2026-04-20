(ns mayphus-cangjie.features.cangjie.tree
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            #?(:cljs ["d3-hierarchy" :as d3])))

(def radial-radius 300)
(def radial-center-x 420)
(def radial-center-y 360)
(def radial-label-gap 18)

(defn branch-columns [dataset active-code]
  (let [active-code (or active-code "")
        prefixes (into [""]
                       (map #(subs active-code 0 %))
                       (range 1 (inc (count active-code))))]
    (mapv (fn [prefix]
            {:prefix prefix
             :selected-prefix (when (> (count active-code) (count prefix))
                                (subs active-code 0 (inc (count prefix))))
             :options (cangjie/next-step-groups dataset prefix)})
          prefixes)))

(defn leaf-nodes [dataset active-code exact-entry]
  (let [matches (when-not (str/blank? active-code)
                  (cangjie/matches-for-prefix dataset active-code))]
    (cond
      exact-entry [exact-entry]
      (and (seq matches) (<= (count matches) 8) (>= (count active-code) 2)) matches
      :else [])))

(defn root-node []
  {:id "root"
   :prefix ""
   :glyph "開始"
   :family "起點"
   :count nil
   :selected? true})

(defn manual-tree-layout [{:keys [dataset entry prefix]}]
  (let [active-code (or prefix "")
        columns (branch-columns dataset active-code)
        leaves (leaf-nodes dataset active-code entry)
        root-node (assoc (root-node) :x 24 :y 220)]
    (loop [remaining columns
           depth 1
           parent-node root-node
           nodes [root-node]
           edges []]
      (if-let [{:keys [options selected-prefix]} (first remaining)]
        (let [count-options (count options)
              parent-center (+ (:y parent-node) 14)
              center-index (/ (dec (max count-options 1)) 2)
              laid-out (mapv (fn [index option]
                               (let [delta (* (- index center-index) 52)
                                     y (- (+ parent-center delta) 14)]
                                 (assoc option
                                        :id (str "step-" depth "-" (:prefix option))
                                        :x (+ 110 (* depth 140))
                                        :y y
                                        :selected? (= (:prefix option) selected-prefix))))
                             (range)
                             options)
              next-parent (or (first (filter :selected? laid-out))
                              parent-node)]
          (recur (next remaining)
                 (inc depth)
                 next-parent
                 (into nodes laid-out)
                 (into edges (map (fn [node] {:from parent-node :to node})) laid-out)))
        (let [leaf-nodes (mapv (fn [index {:keys [char code]}]
                                 {:id (str "leaf-" char "-" code)
                                  :prefix code
                                  :glyph char
                                  :family "葉節點"
                                  :count nil
                                  :x (+ (:x parent-node) 160)
                                  :y (+ (:y parent-node) (* index 52))
                                  :selected? (= code active-code)})
                               (range)
                               leaves)
              all-nodes (into nodes leaf-nodes)
              all-edges (into edges (map (fn [node] {:from parent-node :to node})) leaf-nodes)]
          {:view-box {:width 980 :height 720}
           :y-offset 0
           :nodes all-nodes
           :edges all-edges})))))

(defn- build-tree-data [active-code columns leaves]
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
   (defn- radial-point [angle radius]
     (let [adjusted-angle (- angle (/ js/Math.PI 2))]
       {:x (+ radial-center-x (* radius (js/Math.cos adjusted-angle)))
        :y (+ radial-center-y (* radius (js/Math.sin adjusted-angle)))})))

#?(:cljs
   (defn- radial-tree-layout [{:keys [dataset entry prefix]}]
     (let [active-code (or prefix "")
           columns (branch-columns dataset active-code)
           leaves (leaf-nodes dataset active-code entry)
           tree-data (clj->js (build-tree-data active-code columns leaves))
           hierarchy (.hierarchy d3 tree-data)
           max-depth (max 1 (apply max (map #(.-depth %) (array-seq (js-call hierarchy "descendants")))))
           layout (doto (.tree d3)
                    (.size #js [(* 2 js/Math.PI) radial-radius])
                    (.separation (fn [a b]
                                   (/ (if (= (.-parent a) (.-parent b)) 1 2)
                                      (max 1 (.-depth a))))))
           laid-out-hierarchy (layout hierarchy)
           descendants (array-seq (js-call laid-out-hierarchy "descendants"))
           nodes (mapv (fn [node]
                         (let [data (js->clj (.-data node) :keywordize-keys true)
                               point (radial-point (.-x node) (.-y node))
                               label-point (radial-point (.-x node) (+ (.-y node) radial-label-gap))
                               outward-right? (< (.-x node) js/Math.PI)]
                           (assoc data
                                  :x (:x point)
                                  :y (:y point)
                                  :angle (.-x node)
                                  :radius (.-y node)
                                  :label-x (:x label-point)
                                  :label-y (:y label-point)
                                  :text-anchor (if outward-right? "start" "end")
                                  :label-rotate (if outward-right?
                                                  0
                                                  180))))
                       descendants)
           nodes-by-id (into {} (map (juxt :id identity) nodes))
           edges (mapv (fn [link]
                         (let [source-id (.. link -source -data -id)
                               target-id (.. link -target -data -id)]
                           {:from (get nodes-by-id source-id)
                            :to (get nodes-by-id target-id)}))
                       (array-seq (js-call laid-out-hierarchy "links")))]
       {:view-box {:width 840
                   :height 720}
        :y-offset 0
        :nodes nodes
        :edges edges})))

(defn tree-layout [{:keys [dataset entry prefix] :as opts}]
  #?(:cljs (radial-tree-layout opts)
     :clj (manual-tree-layout opts)))
