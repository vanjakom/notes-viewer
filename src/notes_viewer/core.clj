(ns notes-viewer.core
  (:import
   org.apache.commons.lang.StringEscapeUtils)
  (:use clj-common.clojure)
  (:require
   [compojure.core :as compojure]
   [hiccup.core :as hiccup]
   
   [clj-common.http-server :as server]
   [clj-common.io :as io]
   [clj-common.localfs :as fs]
   [clj-common.path :as path]))

(def notes-path ["Users" "vanja" "projects" "notes" "notes.md"])

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
                   (recur
                    (rest line)
                    tags
                    (str current c)))
                 (if (not (empty? current))
                   (conj tags (.trim current))
                   tags)))]
    (into #{} (filter #(or (.startsWith % "#") (.startsWith % "@")) tags) )))

#_(parse-tags "# #list #divcibare #to")
#_(parse-tags "# notes concept")

(defn read-notes []
  (with-open [is (fs/input-stream notes-path)]
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
                 :content (clojure.string/join "\n" buffer)})
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
            :content (clojure.string/join "\n" buffer)})
          notes)))))

(def notes (atom (read-notes)))

;; reread notes
(do
  (swap! notes (constantly (read-notes)))
  nil)

(defn render-note-info [note]
  [:tr
   [:td {:style "border: 1px solid black; padding: 5px;"}
    (clojure.string/join " " (:tags note))]
   [:td {:style "border: 1px solid black; padding: 5px;"}
    [:a
     {:href (str "/view/" (:id note)) :target "_blank"}
     "view"]]])


(server/create-server
 7099
 (compojure.core/routes
  (compojure.core/GET
   "/view/:id"
   [id]
   (if-let [note (first (filter #(= (:id %) id) (deref notes)))]
     {
      :status 200
      :body (:content note)}
     {:status 404}))
  (compojure.core/GET
   "/explore*"
   request
   (let [search-tags (into
                      []
                      (filter
                       (complement empty?)
                       (.split
                        (or (get-in request [:params :*]) "")
                        "/")))
         search-tags-set (into #{} search-tags)
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
                (deref notes))
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
     (println "[explore]" search-tags)
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
                            "/explore/"
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
               (fn [note] [:pre (StringEscapeUtils/escapeHtml (:content note))])
               notes)])}))
  (compojure.core/GET
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


(defn -main [& args]
  (println "running"))
