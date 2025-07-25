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
#_(parse-tags "# #20240917 #disha geospatial") ;; #{"#20240917" "#disha"}
#_(parse-tags "# #a #b c d #e") ;; #{"#a" "#b" "#e"}

(defn create-note [tags header content]
  {
   :id (uuid)
   :tags tags
   :header header
   :content content})

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
                (create-note tags (first buffer)
                             (clojure.string/join "\n" (rest buffer))))
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
           (create-note tags (first buffer)
                        (clojure.string/join "\n" (rest buffer))))
          notes)))))

(defn process-notes-path
  "Reads notes file respecting structure and order and gives possibility
  to process-fn to alter note. Output is stored in output path.
  Note: currently state of process fn is not supported, must be stored
  externally. This could be useful if order of notes is required."
  ;; todo maybe to support output over process-fn
  ;; todo multiple args process fn ( start, process, close )
  ;; todo support same path / output-path ( in place process )
  [context]
  (let [configuration (context/configuration context)
        path (get configuration :path)
        output-path (get configuration :output-path)
        process-fn (get configuration :process-fn)]
    (context/trace context (str "processing: " path))
    (context/trace context (str "writing to: " output-path))
    (with-open [is (fs/input-stream path)
                os (fs/output-stream output-path)]
      ;; copy with modifications of read-notes
      (let [write-note-fn (fn [note]
                            ;; writing header instead of serializing tags
                            (io/write-line
                             os
                             (:header note))
                            (io/write-line
                             os
                             (:content note)))]
        (loop [lines (io/input-stream->line-seq is)
               tags #{}
               buffer []]
          (if-let [line (first lines)]
            (if (.startsWith line "# ")
              (let [new-tags (parse-tags line)]
                (if (not (empty? buffer))
                  (let [note (process-fn
                              (create-note
                               tags
                               (first buffer)
                               (clojure.string/join "\n" (rest buffer))))]
                    (when note (write-note-fn note))
                    (recur
                     (rest lines)
                     new-tags
                     [line]))
                  (recur
                   (rest lines)
                   new-tags
                   [])))
              (recur
               (rest lines)
               tags
               (conj buffer line)))
            (if (not (empty? buffer))
              (let [note (process-fn
                          (create-note
                           tags
                           (first buffer)
                           (clojure.string/join "\n" (rest buffer))))]
                (when note (write-note-fn note))))))))
    (context/trace context "processing finished")))

;; test extract for zanimljiva-geografija:kako-mapirati
#_(process-notes-path
   (context/create-stdout-context
    {
     :path ["Users" "vanja" "projects" "notes" "notes.md"]
     :output-path ["Users" "vanja" "projects" "zanimljiva-geografija" "blog" "tags.md"]
     :process-fn (fn [note]
                   (when (and
                          (contains? (:tags note) "#osm")
                          (contains? (:tags note) "#map")
                          (contains? (:tags note) "#tag")
                          (contains? (:tags note) "#export"))
                     note))}))

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
                 render-note
                 notes)])}))

#_(schedule "todo" (deref todos) #{"log"})

(defn search [dataset-name dataset search-tags preview]
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
                (if preview
                  (map preview-note notes)
                  (map render-note notes))])}))

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
       (search "note" (deref notes) search-tags false)))
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
     "/preview*"
     request
     (let [search-tags (into
                        []
                        (filter
                         (complement empty?)
                         (.split
                          (or (get-in request [:params :*]) "")
                          "/")))]
       (search "todo" (deref todos) search-tags true)))
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
       (search "todo" (deref todos) search-tags false)))
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

#_(start-server)
#_(clj-common.http-server/stop-server 7099)
