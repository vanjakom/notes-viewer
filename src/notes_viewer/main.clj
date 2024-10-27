(ns notes-viewer.main
  (:require [notes-viewer.core :as core]))

(defn -main []
  (println "starting notes viewer server and scheduler")
  (core/start-server)
  (println "server started"))

#_(-main)
