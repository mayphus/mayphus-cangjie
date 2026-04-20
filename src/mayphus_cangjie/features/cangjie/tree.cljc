(ns mayphus-cangjie.features.cangjie.tree
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]))

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

(def node-width 112)
(def node-height 42)
(def x-step 128)
(def y-step 52)
(def root-x 32)
(def spine-y 260)
(def root-id "root")

(defn root-node []
  {:id root-id
   :prefix ""
   :glyph "開始"
   :family "起點"
   :count nil
   :selected? true})

(defn- alternating-offsets [count]
  (take count
        (mapcat (fn [step] [(- step) step])
                (iterate inc 1))))

(defn- layout-column [options selected-prefix x]
  (let [selected-option (some #(when (= (:prefix %) selected-prefix) %) options)
        anchor-option (or selected-option (first options))
        sibling-options (remove #(= (:prefix %) (:prefix anchor-option)) options)
        baseline-y (- spine-y (/ node-height 2))
        positioned-anchor (assoc anchor-option
                                 :id (str "step-" (:prefix anchor-option))
                                 :x x
                                 :y baseline-y
                                 :selected? (= (:prefix anchor-option) selected-prefix))
        positioned-siblings (mapv (fn [offset option]
                                    (assoc option
                                           :id (str "step-" (:prefix option))
                                           :x x
                                           :y (+ baseline-y (* offset y-step))
                                           :selected? (= (:prefix option) selected-prefix)))
                                  (alternating-offsets (count sibling-options))
                                  sibling-options)]
    {:nodes (vec (cons positioned-anchor positioned-siblings))
     :anchor (or (some #(when (:selected? %) %) (cons positioned-anchor positioned-siblings))
                 positioned-anchor)}))

(defn- layout-leaf-row [leaves active-code x]
  (when (seq leaves)
    (let [selected-leaf (some #(when (= (:code %) active-code) %) leaves)
          anchor-leaf (or selected-leaf (first leaves))
          sibling-leaves (remove #(= (:code %) (:code anchor-leaf)) leaves)
          baseline-y (- spine-y (/ node-height 2))
          positioned-anchor {:id (str "leaf-" (:char anchor-leaf) "-" (:code anchor-leaf))
                             :prefix (:code anchor-leaf)
                             :glyph (:char anchor-leaf)
                             :family "葉節點"
                             :count nil
                             :x x
                             :y baseline-y
                             :selected? (= (:code anchor-leaf) active-code)}
          positioned-siblings (mapv (fn [offset {:keys [char code]}]
                                      {:id (str "leaf-" char "-" code)
                                       :prefix code
                                       :glyph char
                                       :family "葉節點"
                                       :count nil
                                       :x x
                                       :y (+ baseline-y (* offset y-step))
                                       :selected? (= code active-code)})
                                    (alternating-offsets (count sibling-leaves))
                                    sibling-leaves)]
      (vec (cons positioned-anchor positioned-siblings)))))

(defn tree-layout [{:keys [dataset entry prefix]}]
  (let [active-code (or prefix "")
        columns (branch-columns dataset active-code)
        leaves (leaf-nodes dataset active-code entry)
        root-node (assoc (root-node) :x root-x :y (- spine-y (/ node-height 2)))]
    (loop [remaining columns
           depth 1
           parent-node root-node
           nodes [root-node]
           edges []]
      (if-let [{:keys [options selected-prefix]} (first remaining)]
        (let [{column-nodes :nodes anchor :anchor} (layout-column options
                                                                  selected-prefix
                                                                  (+ 84 (* depth x-step)))
              next-edges (into edges
                               (map (fn [node]
                                      {:from parent-node :to node}))
                               column-nodes)]
          (recur (next remaining)
                 (inc depth)
                 anchor
                 (into nodes column-nodes)
                 next-edges))
        (let [leaf-nodes (or (layout-leaf-row leaves active-code (+ (:x parent-node) 154))
                             [])
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
