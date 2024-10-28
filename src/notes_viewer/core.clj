(ns notes-viewer.core
  (:import
   org.apache.commons.lang.StringEscapeUtils)
  (:use clj-common.clojure)
  (:require
   [compojure.core :as compojure]
   [hiccup.core :as hiccup]

   [clj-common.as :as as]
   [clj-common.http-server :as server]
   [clj-common.io :as io]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]))

(def notes-path-seq
  [
   ["Users" "vanja" "projects" "notes" "notes.md"]
   ["Users" "vanja" "projects" "notes" "boxes.md"]])
(def todo-path ["Users" "vanja" "projects" "notes" "todo.md"])

(defn parse-tags [line]
  (let [line (if (.startsWith line "# ")
               (.substring line 2)
               line)
        tags (loop [line line
                    tags #{}
                    current ""]
               (if-let [c (first line)]
                 (if (or (= c \#) (= c \@))
                   (if (not (empty? current))
                     (recur
                      (rest line)
                      (conj tags (.trim current))
                      (str c))
                     (recur
                      (rest line)
                      tags
                      (str c)))
                   ;; in case of space report tag
                   (if (= c \ )
                     (recur
                      (rest line)
                      (if (not (empty? current))
                        (conj tags (.trim current))
                        tags)
                      "")
                    (recur
                     (rest line)
                     tags
                     (str current c))))
                 (if (not (empty? current))
                   (conj tags (.trim current))
                   tags)))]
    (into #{} (filter #(or (.startsWith % "#") (.startsWith % "@")) tags) )))

#_(parse-tags "# #list #divcibare #to") ;; #{"#divcibare" "#to" "#list"}
#_(parse-tags "# notes concept") ;; #{}
;; fix on 20240919
(parse-tags "# #20240917 #disha geospatial") ;; #{"#20240917" "#disha"}
(parse-tags "# #a #b c d #e") ;; #{"#a" "#b" "#e"}

(defn read-notes [path]
  (with-open [is (fs/input-stream path)]
    (loop [lines (io/input-stream->line-seq is)
           notes []
           tags #{}
           buffer []]
      (if-let [line (first lines)]
        (if (.startsWith line "# ")
          (let [new-tags (parse-tags line)]
            (if (not (empty? buffer))
              (recur
               (rest lines)
               (conj
                notes
                {
                 :id (uuid)
                 :tags tags
                 :header (first buffer)
                 :content (clojure.string/join "\n" (rest buffer))})
               new-tags
               [line])
              (recur
               (rest lines)
               notes
               new-tags
               [])))
          (recur
           (rest lines)
           notes
           tags
           (conj buffer line)))
        (if (not (empty? buffer))
          (conj
           notes
           {
            :id (uuid)
            :tags tags
            :header (first buffer)
            :content (clojure.string/join "\n" (rest buffer))})
          notes)))))


(def notes (atom []))
(def todos (atom []))

;; reread notes
(defn reload-all []
  (swap! notes (constantly (mapcat
                            #(read-notes %)
                            notes-path-seq)))
  (swap! todos (constantly (read-notes todo-path)))
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

(defn parse-date [tag]
  ;; #20240909
  (when
      (and
       (= (count tag) 9)
       ;; todo
       ;; check month and day range
       (.startsWith tag "#2"))
    (as/as-long (.substring tag 1))))

(defn date [note]
  (first (filter some? (map parse-date (:tags note)))))

#_(date {:tags #{"#todo" "#20240909"}})

(defn schedule [dataset-name dataset search-tags]
  (let [search-tags-set (into #{} search-tags)
        notes (sort-by
               date
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
                (filter
                 #(some? (date %))
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
    (println "[schedule][" dataset-name "]" search-tags)
    {
     :status 200
     :body (hiccup/html
            [:body {:style "font-family:arial; max-width:100%; overflow-x:hidden;"}
             [:table {:style "border-collapse:collapse;"}
              (map
               (fn [[tag count]]
                 (list
                  [:a
                   {
                    :href (str
                           "/" dataset-name "/"
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
             (map
              (fn [note]
                (list
                 [:b (:header note)]
                 [:br]
                 [:pre (StringEscapeUtils/escapeHtml (:content note))]))
              notes)])}))

#_(schedule "todo" (deref todos) #{"log"})

(defn search [dataset-name dataset search-tags]
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
    (println "[" dataset-name "]" search-tags)
    {
     :status 200
     :body (hiccup/html
            [:body {:style "font-family:arial; max-width:100%; overflow-x:hidden;"}
             [:table {:style "border-collapse:collapse;"}
              (map
               (fn [[tag count]]
                 (list
                  [:a
                   {
                    :href (str
                           "/" dataset-name "/"
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
             (map
              (fn [note]
                (list
                 [:b (:header note)]
                 [:br]
                 [:pre (StringEscapeUtils/escapeHtml (:content note))]))
              notes)])}))

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
     "/note*"
     request
     (let [search-tags (into
                        []
                        (filter
                         (complement empty?)
                         (.split
                          (or (get-in request [:params :*]) "")
                          "/")))]
       (search "note" (deref notes) search-tags)))
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
       (schedule "todo" (deref todos) #{})))  
    (compojure.core/GET
     "/todo*"
     request
     (let [search-tags (into
                        []
                        (filter
                         (complement empty?)
                         (.split
                          (or (get-in request [:params :*]) "")
                          "/")))]
       (search "todo" (deref todos) search-tags)))  
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
  
  ;; refresh notes on minute interval
  (println "starting cron")
  (.start
   (new
    Thread
    #(while true
       (reload-all)
       (println "[refresh]" (System/currentTimeMillis))
       (Thread/sleep 60000)))))
