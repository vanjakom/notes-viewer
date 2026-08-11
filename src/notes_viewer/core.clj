(ns notes-viewer.core
  (:import
   org.apache.commons.lang.StringEscapeUtils)
  (:use clj-common.clojure)
  (:require
   [compojure.core :as compojure]
   [hiccup.core :as hiccup]

   [clj-common.as :as as]
   [clj-common.context :as context]
   [clj-common.http-server :as server]
   [clj-common.io :as io]
   [clj-common.localfs :as fs]
   [clj-common.notemd :as notemd]
   [clj-common.path :as path]))

(def notes-path-seq
  [
   [["Users" "vanja" "projects" "notes" "notes.md"] #{"#note"}]
   [["Users" "vanja" "projects" "notes" "boxes.md"] #{"#note"}]

   [["Users" "vanja" "projects" "notes" "sf-todo.md"] #{"#todo" "#sf"}]
   [["Users" "vanja" "projects" "notes" "inicijative" "ravni.md"] #{"#todo" "#ravni"}]
   [["Users" "vanja" "projects" "notes" "inicijative" "pss.md"] #{"#todo" "#pss"}]

   [["Users" "vanja" "projects" "notes" "inicijative" "mungolab.md"] #{"#todo" "#ml"}]
   [["Users" "vanja" "projects" "notes" "inicijative" "okokuce.md"] #{"#todo"}]
   
   [["Users" "vanja" "projects" "notes" "inicijative" "trek-mate.md"] #{"#todo" "#tm"}]
   [["Users" "vanja" "projects" "notes" "inicijative" "supplyframe.md"] #{"#todo" "#sf"}]
   [["Users" "vanja" "projects" "notes" "todo.md"] #{"#todo"}]
   ])

(def notes (atom []))

;; reread notes
(defn reload-all []
  (swap! notes (constantly (mapcat
                            #(with-open [is (fs/input-stream (first %))]
                               (notemd/read-notes is (second %)))
                            notes-path-seq)))
  nil)

(reload-all)

#_(run!
 println
 (take 5 (read-notes ["Users" "vanja" "projects" "notes" "boxes.md"])))
#_(search "note" (deref notes) ["#box50"])

(defn render-note-info [note]
  [:tr
   [:td {:style "border: 1px solid black; padding: 5px;"}
    (clojure.string/join " " (:tags note))]
   [:td {:style "border: 1px solid black; padding: 5px;"}
    [:a
     {:href (str "/view/" (:id note)) :target "_blank"}
     "view"]]])

;; chatgpt with alter
(defn replace-http-links-with-anchor [s]
  "Replaces http:// links outside of <pre> blocks"
  (clojure.string/replace
   s
   #"(?:<pre>[\s\S]*?</pre>)|http://[^\s<]+"
   (fn [match]
     (if (.startsWith match "<pre>")
       match ;; Leave content inside <pre> blocks unchanged
       (str "<a target=\"_blank\" href=\"" match "\">" match "</a>")))))

;; chatgpt with alter
(defn replace-https-links-with-anchor [s]
  "Replaces https:// links outside of <pre> blocks"
  (clojure.string/replace
   s
   #"(?:<pre>[\s\S]*?</pre>)|https://[^\s<]+"
   (fn [match]
     (if (.startsWith match "<pre>")
       match ;; Leave content inside <pre> blocks unchanged
       (str "<a target=\"_blank\" href=\"" match "\">" match "</a>")))))

(defn replace-file-links-with-anchor [s]
  "Replaces file:// links outside of <pre> blocks"
  (clojure.string/replace
   s
   #"(?:<pre>[\s\S]*?</pre>)|file://[^\s<]+"
   (fn [match]
     (if (.startsWith match "<pre>")
       match ;; Leave content inside <pre> blocks unchanged
       (str "<a target=\"_blank\" href=\"" match "\">" match "</a>")))))

;; chatgpt
(defn replace-newlines-with-br [s]
  (clojure.string/replace s #"\n" "<br>"))

;; chatgpt, with alter
(defn replace-code-blocks-with-pre [s]
  (clojure.string/replace
   s
   #"```([\s\S]*?)```"
   (fn [match]
     (str "<pre>" (StringEscapeUtils/escapeHtml (second match)) "</pre>"))))

#_(replace-http-links-with-anchor "test http://google.com<br>")
;; "test <a target=\"_blank\" href=\"http://google.com<br>\">http://google.com<br></a>"

(defn render-note [note]
  (list
   [:b (:header note)]
   " "
   [:a
    {:href (str
            "/"
            (clojure.string/join
             "/"
             (map #(.substring % 1) (:tags note))))}
    "share"]
   [:br]
   [:div
    (->
     (:content note)
     replace-code-blocks-with-pre
     replace-http-links-with-anchor
     replace-https-links-with-anchor
     replace-file-links-with-anchor
     replace-newlines-with-br)]
   [:br]))

(defn preview-note [note]
  (list
   [:b (:header note)]
   [:br]
   [:br]))

#_(date {:tags #{"#todo" "#20240909"}})

;; todo use search from notemd
(defn schedule [dataset search-tags]
  (let [search-tags-set (into #{"todo"} search-tags)
        notes (sort-by
               notemd/date
               (filter
                (fn [note]
                  (=
                   (count search-tags-set)
                   (count
                    (filter
                     #(or
                       (contains? (:tags note) (str "@" %))
                       (contains? (:tags note) (str "#" %)))
                     search-tags-set))))
                (filter
                 #(some? (notemd/date %))
                 dataset)))
        tags (reduce
              (fn [state tag]
                (if (not (contains? search-tags-set tag))
                  (update-in
                   state
                   [tag]
                   #(inc (or % 0)))
                  state))
              {}
              (map
               #(.substring % 1)
               (mapcat
                :tags
                notes)))]
    (println "[schedule]" search-tags)
    {
     :status 200
     :headers {
               "Content-Type" "text/html; charset=utf-8"}
     :body (hiccup/html
               [:head
                [:meta {:charset "UTF-8"}]]
               [:body {:style "font-family:arial; max-width:100%; overflow-x:hidden;"}
                [:table {:style "border-collapse:collapse;"}
                 ;; 20260317 no need for grouping on schedule
                 #_(map
                    (fn [[tag count]]
                      (list
                       [:a
                        {
                         :href (str
                                "/"
                                (clojure.string/join
                                 "/"
                                 (conj search-tags tag)))}
                        (str
                         (clojure.string/join "/" (conj search-tags tag))
                         " (" count ")")]
                       [:br]))
                    (filter
                     #(> (second %) 1)
                     (sort-by first tags)))]
                #_[:br]
                (map
                 render-note
                 notes)])}))

#_(schedule "todo" (deref todos) #{"log"})

;; todo use search from notemd
(defn search [dataset search-tags preview]
  (let [search-tags-set (into #{} search-tags)
        notes (filter
               (fn [note]
                 (=
                  (count search-tags)
                  (count
                   (filter
                    #(or
                      (contains? (:tags note) (str "@" %))
                      (contains? (:tags note) (str "#" %)))
                    search-tags))))
               dataset)
        tags (reduce
              (fn [state tag]
                (if (not (contains? search-tags-set tag))
                  (update-in
                   state
                   [tag]
                   #(inc (or % 0)))
                  state))
              {}
              (map
               #(.substring % 1)
               (mapcat
                :tags
                notes)))]
    (println "[search]" search-tags)
    {
     :status 200
     :headers {
               "Content-Type" "text/html; charset=utf-8"}
     :body (hiccup/html
               [:head
                [:meta {:charset "UTF-8"}]]
               [:body {:style "font-family:arial; max-width:100%; overflow-x:hidden;"}
                [:table {:style "border-collapse:collapse;"}
                 (map
                  (fn [[tag count]]
                    (list
                     [:a
                      {
                       :href (str
                              "/"
                              (clojure.string/join
                               "/"
                               (conj search-tags tag)))}
                      (str
                       (clojure.string/join "/" (conj search-tags tag))
                       " (" count ")")]
                     [:br]))
                  (filter
                   #(> (second %) 1)
                   (sort-by first tags)))]
                [:br]
                (if preview
                  (map preview-note notes)
                  (map render-note notes))])}))

#_(search (deref notes) #{"icloud"} false)

(defn start-server []
  (println "starting server")
  (server/create-server
   7099
   (compojure.core/routes
    (compojure.core/GET
        "/view/:id"
        [id]
        (if-let [note (first (filter #(= (:id %) id) (deref notes)))]
          {
           :status 200
           :body (str (:header note) "\n" (:content note))}
          {:status 404}))
    (compojure.core/GET
        "/refresh"
        _
        (do
          (reload-all)
          {
           :status 200
           :body "ok"}))
    (compojure.core/GET
        "/schedule*"
        request
        (let [search-tags (into
                           []
                           (filter
                            (complement empty?)
                            (.split
                             (or (get-in request [:params :*]) "")
                             "/")))]
          ;; 20260317 refresh on each search
          (reload-all)
          (schedule (deref notes) #{})))
    (compojure.core/GET
        "/preview*"
        request
        (let [search-tags (into
                           []
                           (filter
                            (complement empty?)
                            (.split
                             (or (get-in request [:params :*]) "")
                             "/")))]
          ;; 20260317 refresh on each search
          (reload-all)
          (search (deref notes) search-tags true)))
    (compojure.core/GET
        "/*"
        request
        (let [search-tags (into
                           []
                           (filter
                            (complement empty?)
                            (.split
                             (or (get-in request [:params :*]) "")
                             "/")))]
          ;; 20260317 refresh on each search
          (reload-all)
          (search (deref notes) search-tags false)))
    ;; deprecated, was using notes to summarize tags
    #_(compojure.core/GET
          "/list*"
          request
          (let [search-tags (filter
                             (complement empty?)
                             (.split
                              (or (get-in request [:params :*]) "")
                              "/"))]
            (println "[list]" search-tags)
            {
             :status 200
             :body (hiccup/html
                       [:body {:style "font-family:arial;"}
                        [:table {:style "border-collapse:collapse;"}
                         (map
                          render-note-info
                          (filter
                           (fn [note]
                             (=
                              (count search-tags)
                              (count
                               (filter
                                #(or
                                  (contains? (:tags note) (str "@" %))
                                  (contains? (:tags note) (str "#" %)))
                                search-tags))))
                     
                           (deref notes)))]])}))))

  ;; 20260317 refresh on each search
  ;; refresh notes on minute interval
  #_(println "starting cron")
  #_(.start
   (new
    Thread
    #(while true
       (reload-all)
       (println "[refresh]" (System/currentTimeMillis))
       (Thread/sleep 60000)))))

#_(start-server)
#_(clj-common.http-server/stop-server 7099)
