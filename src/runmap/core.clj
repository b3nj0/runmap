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
  (->> recs
       (map #(assoc {} :lat (.getPositionLat %1) :lng (.getPositionLong %1)))
       (filter (comp not nil? :lat))
       (filter (comp not nil? :lng))))

(defn bounds [lls]
  "bounding box for lat lngs"
  (let [lats (remove nil? (map :lat lls))
        lngs (remove nil? (map :lng lls))
        latmin (apply min lats)
        latmax (apply max lats)
        lngmin (apply min lngs)
        lngmax (apply max lngs)]
    {:min {:lat latmin :lng lngmin} :max {:lat latmax :lng lngmax}}))

(defn scale [val domain range]
  (let [[dommin dommax] domain
        dom (- dommax dommin)
        [rngmin rngmax] range
        rng (double (- rngmax rngmin))]
    (+ (* (- val dommin) (/ rng dom)) rngmin)))

(defn scale-latlngs [lls x y]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]
        latrng [0 x]
        lngrng [0 y]]
    (map #(vector (scale (:lat %1) latdom latrng) (scale (:lng %1) lngdom lngrng)) lls)))

(defn runmap [files dim]
  (let [recs (mapcat record-mesgs files)
        lls (latlngs recs)
        lls (scale-latlngs lls (:x dim) (:y dim))]
    lls))

(defn runmap->bitmap [rm dim]
  (let [img (java.awt.image.BufferedImage. (:x dim) (:y dim) java.awt.image.BufferedImage/TYPE_BYTE_GRAY)
        g (.createGraphics img)]
    (doall (map (fn [[x y]] (.drawRect g x y 1 1)) rm))
    img))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (fit-files dir)
        dim {:x 2048 :y 2048}
        rm (runmap files dim)
        bmp (runmap->bitmap rm dim)]
    (javax.imageio.ImageIO/write bmp "png" (file "runmap.png"))))
