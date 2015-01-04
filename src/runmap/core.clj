(ns runmap.core
  (:use [clojure.java.io :as jio]))

(defn fit-files [dir]
  (filter (memfn isFile) (file-seq (file dir))))

(def ff (first (fit-files "resources/fit-files")))

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

(defn record-mesgs [file]
  "extract RecordMesgs from file"
  (let [msgs (atom [])]
    (parse file [(record-collector msgs)])
    @msgs))

(defn latlngs [recs]
  "extract lat and lngs from seq of RecordMesgs"
  (map #(assoc {} :lat (.getPositionLat %1) :lng (.getPositionLong %1)) recs))

(defn bounds [lls]
  "bounding box for lat lngs"
  (let [lats (remove nil? (map :lat lls))
        lngs (remove nil? (map :lng lls))
        latmin (apply min lats)
        latmax (apply max lats)
        lngmin (apply min lngs)
        lngmax (apply max lngs)]
    {:latmin latmin :latmax latmax :lngmin lngmin :lngmax lngmax}))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (take 2 (fit-files dir))
        recs (mapcat record-mesgs files)
        lls (latlngs recs)]
    (count lls)))
