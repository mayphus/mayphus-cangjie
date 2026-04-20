(ns mayphus-cangjie.features.cangjie.tree
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]))

(def icicle-width 1200)
(def icicle-row-height 112)
(def icicle-row-gap 8)
(def icicle-padding 0)

(defn path-prefixes [prefix]
  (into #{""}
        (map #(subs prefix 0 %))
        (range 1 (inc (count (or prefix ""))))))

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
   :selected? true
   :expandable? true})

(defn- selected-prefix? [active-code prefix]
  (or (str/blank? prefix)
      (= prefix active-code)
      (and (seq active-code)
           (str/starts-with? active-code prefix))))

(defn- step-node [active-code option]
  (assoc option
         :id (str "step-" (:prefix option))
         :selected? (selected-prefix? active-code (:prefix option))
         :expandable? (seq (:prefix-groups option))))

(defn- leaf-node [active-code {:keys [char code]}]
  {:id (str "leaf-" char "-" code)
   :prefix code
   :glyph char
   :family "葉節點"
   :count nil
   :selected? (= code active-code)
   :expandable? false})

(defn- build-prefix-groups [dataset prefix]
  (mapv #(assoc % :prefix-groups (cangjie/next-step-groups dataset (:prefix %)))
        (cangjie/next-step-groups dataset prefix)))

(defn- option-for-prefix [dataset prefix]
  (when (seq prefix)
    (let [parent-prefix (subs prefix 0 (dec (count prefix)))]
      (or (some #(when (= prefix (:prefix %)) %) (build-prefix-groups dataset parent-prefix))
          (let [letter (subs prefix (dec (count prefix)))
                root (get cangjie/root-by-letter letter)]
            {:letter letter
             :glyph (:glyph root)
             :family (:family root)
             :hint (:hint root)
             :count 0
             :examples []
             :prefix prefix
             :prefix-groups (cangjie/next-step-groups dataset prefix)})))))

(defn- build-tree-data
  [{:keys [active-code dataset exact-entry expanded-prefixes]}]
  (letfn [(children-for [prefix]
            (let [options (build-prefix-groups dataset prefix)
                  expanded? (contains? expanded-prefixes prefix)
                  branch-children (when expanded?
                                    (mapv build-node options))
                  leaf-children (when expanded?
                                  (let [leaves (leaf-nodes dataset prefix (when (= prefix active-code) exact-entry))]
                                    (when (and (seq leaves) (empty? options))
                                      (mapv #(leaf-node active-code %) leaves))))]
              (vec (concat branch-children leaf-children))))
          (build-node [option]
            (let [node (step-node active-code option)
                  children (children-for (:prefix option))]
              (cond-> node
                (seq children) (assoc :children children))))
          (build-path-node [prefixes]
            (let [prefix (first prefixes)
                  option (option-for-prefix dataset prefix)
                  node (step-node active-code option)]
              (if-let [next-prefixes (next prefixes)]
                (assoc node :children [(build-path-node next-prefixes)])
                (let [children (children-for prefix)]
                  (cond-> node
                    (seq children) (assoc :children children))))))]
    (if (str/blank? active-code)
      (assoc (root-node) :children (mapv build-node (build-prefix-groups dataset "")))
      (build-path-node (vec (rest (sort-by count (path-prefixes active-code))))))))

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

(defn- max-depth [{:keys [children]}]
  (if (seq children)
    (inc (reduce max (map max-depth children)))
    1))

(defn- layout-icicle [root]
  (let [nodes* (atom [])
        edges* (atom [])]
    (letfn [(visit [node visible-depth x0 x1 parent]
              (let [children (:children node)
                    display? (not (str/blank? (:prefix node)))
                    y (+ icicle-padding (* visible-depth icicle-row-height))
                    next-visible-depth (if display?
                                         (inc visible-depth)
                                         visible-depth)
                    laid-out (assoc node
                                    :x x0
                                    :y y
                                    :width (max 0 (- x1 x0))
                                    :height (- icicle-row-height icicle-row-gap)
                                    :depth visible-depth)]
                (when display?
                  (swap! nodes* conj laid-out)
                  (when parent
                    (swap! edges* conj {:from parent :to laid-out})))
                (when (seq children)
                  (let [child-count (count children)
                        span (- x1 x0)]
                    (loop [remaining children
                           cursor x0]
                      (when-let [child (first remaining)]
                        (let [child-width (if (next remaining)
                                            (/ span child-count)
                                            (- x1 cursor))
                              next-cursor (+ cursor child-width)]
                          (visit child next-visible-depth cursor next-cursor (when display? laid-out))
                          (recur (next remaining) next-cursor))))))))]
      (visit root 0 icicle-padding (- icicle-width icicle-padding) nil)
      (let [visible-depth (max 1 (dec (max-depth root)))]
        {:view-box {:width icicle-width
                    :height (+ (* 2 icicle-padding)
                               (* visible-depth icicle-row-height))}
         :y-offset 0
         :nodes @nodes*
         :edges @edges*}))))

(defn tree-layout [{:keys [dataset entry prefix expanded-prefixes]}]
  (let [active-code (or prefix "")
        root (build-tree-data {:active-code active-code
                               :dataset dataset
                               :exact-entry entry
                               :expanded-prefixes (into (or expanded-prefixes #{""})
                                                        (path-prefixes active-code))})]
    (layout-icicle root)))
