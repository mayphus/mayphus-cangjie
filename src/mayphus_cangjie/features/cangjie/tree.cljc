(ns mayphus-cangjie.features.cangjie.tree
  (:require [clojure.string :as str]
            [mayphus-cangjie.features.cangjie :as cangjie]
            #?(:cljs ["d3-hierarchy" :as d3])))

(def radial-radius 300)
(def radial-center-x 420)
(def radial-center-y 360)
(def radial-label-gap 18)

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
                (seq children) (assoc :children children))))]
    (assoc (root-node) :children (mapv build-node (build-prefix-groups dataset "")))))

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

(defn- layout-tree-manual [root]
  (let [nodes* (atom [])
        edges* (atom [])
        next-y* (atom 40)]
    (letfn [(visit [node depth parent]
              (let [children (:children node)
                    child-positions (mapv #(visit % (inc depth) node) children)
                    y (if (seq child-positions)
                        (/ (+ (apply min child-positions) (apply max child-positions)) 2)
                        (let [current @next-y*]
                          (swap! next-y* + 56)
                          current))
                    laid-out (assoc node :x (+ 32 (* depth 140)) :y y)]
                (swap! nodes* conj laid-out)
                (when parent
                  (swap! edges* conj {:from parent :to laid-out}))
                y))]
      (visit root 0 nil)
      {:view-box {:width 980
                  :height (max 560 @next-y*)}
       :y-offset 0
       :nodes @nodes*
       :edges @edges*})))

#?(:cljs
   (defn- js-call [obj method]
     (js* "(~{}[~{}]).call(~{})" obj method obj)))

#?(:cljs
   (defn- radial-point [angle radius]
     (let [adjusted-angle (- angle (/ js/Math.PI 2))]
       {:x (+ radial-center-x (* radius (js/Math.cos adjusted-angle)))
        :y (+ radial-center-y (* radius (js/Math.sin adjusted-angle)))})))

#?(:cljs
   (defn- radial-tree-layout [root]
     (let [tree-data (clj->js root)
           hierarchy (.hierarchy d3 tree-data)
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
                                  :label-rotate (if outward-right? 0 180))))
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

(defn tree-layout [{:keys [dataset entry prefix expanded-prefixes]}]
  (let [active-code (or prefix "")
        root (build-tree-data {:active-code active-code
                               :dataset dataset
                               :exact-entry entry
                               :expanded-prefixes (into (or expanded-prefixes #{""})
                                                        (path-prefixes active-code))})]
    #?(:cljs (radial-tree-layout root)
       :clj (layout-tree-manual root))))
