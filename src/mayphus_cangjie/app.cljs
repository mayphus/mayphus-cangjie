(ns mayphus-cangjie.app
  (:require [mayphus-cangjie.features.cangjie :as cangjie]
            [mayphus-cangjie.features.cangjie.island :as island]
            [reagent.core :as r]
            ["react-dom/client" :as react-dom-client]))

(defn shell []
  [:main {:class "app-shell"}
   [:header {:class "site-head"}
    [:div {:class "brand-block"}
     [:p {:class "brand-kicker"} "Cangjie Explorer"]
     [:h1 {:class "brand-title"} "倉頡樹圖"]
     [:p {:class "brand-summary"}
      "用樹狀分支而不是死背表格來查倉頡碼。輸入字、字碼或字根，然後沿著每一步往下走。"]]
   [:a {:class "site-link"
         :href "https://mayphus.org/"
         :rel "noreferrer"
         :target "_blank"}
     "mayphus.org"]]
   [island/cangjie-island {:featured-entry (cangjie/exact-char "照")}]])

(defn init []
  (when-let [container (.getElementById js/document "app")]
    (.render (react-dom-client/createRoot container)
             (r/as-element [shell]))))
