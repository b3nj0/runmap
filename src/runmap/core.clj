(ns runmap.core
  (:use [clojure.java.io :as jio]))

(defn fit-files [dir]
  (filter (memfn isFile) (file-seq (file dir))))

(def ff (first (fit-files "resources/fit-files")))

(defn record-mesg-listener [fn & args]
  (proxy [com.garmin.fit.RecordMesgListener] []
    (onMesg [m] (apply fn m args))))

(defn collect [msgs]
  (fn [m]
    (swap! msgs conj m)))

(defn record-collector [msgs]
  (record-mesg-listener (collect msgs)))

(defn parse [file listeners]
  (let [dec (com.garmin.fit.Decode.)
        brd (com.garmin.fit.MesgBroadcaster. dec)
        fin (jio/input-stream file)]
    (doall (map #(.addListener brd %) listeners))
    (.run brd fin)))

(defn record-mesgs [file]
  (let [msgs (atom [])]
    (parse file (collect msgs))
    @msgs))

(defn -main [& args]
  (let [dir "resources"
        files (fit-files dir)]
    (println files)))
