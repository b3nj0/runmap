(ns runmap.fitparser
  (:require [clojure.java.io :as jio]))

;; parsing fit files

(defn record-mesg-listener [fn & args]
  (proxy [com.garmin.fit.RecordMesgListener] []
    (onMesg [m] (apply fn m args))))

(defn collect [msgs f]
  "add messages transformed by f to msgs"
  (fn [m]
    (swap! msgs conj (f m))))

(defn record-collector [msgs]
  (record-mesg-listener (collect msgs identity)))

(defn parse [file lnrs]
  "parse file using listeners lnrs"
  (let [dec (com.garmin.fit.Decode.)
        brd (com.garmin.fit.MesgBroadcaster. dec)
        fin (jio/input-stream file)]
    (doall (map #(.addListener brd %) lnrs))
    (.run brd fin)))

(defn fit-file->record-mesgs [file]
  "extract RecordMesgs from file"
  (let [msgs (atom [])]
    (parse (jio/file file) [(record-collector msgs)])
    @msgs))

(defn fit-files->record-mesgs [files]
  (mapcat fit-file->record-mesgs files))

(defn semicircles->degrees [sc]
  (* sc (/ 180 (Math/pow 2 31))))

(defn record-mesgs->latlngs [recs]
  (->> recs
       (map #(assoc {} :lat (.getPositionLat %1) :lng (.getPositionLong %1)))
       (filter (comp not nil? :lat))
       (filter (comp not nil? :lng))
       (map #(assoc {} :lat  (semicircles->degrees (:lat %1)) :lng (semicircles->degrees (:lng %1))))))

(defn fit-files->latlngs [files]
  (record-mesgs->latlngs (fit-files->record-mesgs files)))
