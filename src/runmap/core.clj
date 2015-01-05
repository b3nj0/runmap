(ns runmap.core
  (:use [clojure.java.io :as jio]))

(defn semicircles->degrees [sc]
  (* sc (/ 180 (Math/pow 2 31))))

(defn degrees->semicircles [dg]
  (/ dg (/ 180 (Math/pow 2 31))))

(defn runmap-bounds []
  {:min {:lat (degrees->semicircles 51.323264) :lng (degrees->semicircles -0.396881)} :max {:lat (degrees->semicircles 51.729952) :lng (degrees->semicircles 0.379028)}})

(defn fit-files [dir]
  (filter (memfn isFile) (file-seq (file dir))))

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
  (let [lats (map :lat lls)
        lngs (map :lng lls)
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

(defn filter-latlngs [lls bnds]
  (->> lls
       (filter #(> (:lat %) (get-in bnds [:min :lat])))
       (filter #(< (:lat %) (get-in bnds [:max :lat])))
       (filter #(> (:lng %) (get-in bnds [:min :lng])))
       (filter #(< (:lng %) (get-in bnds [:max :lng])))))

(defn scale-latlngs [lls x y]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]
        latrng [0 x]
        lngrng [0 y]]
    (map #(vector (scale (:lng %1) lngdom lngrng) (scale (:lat %1) latdom latrng)) lls)))

(defn runmap [files dim]
  (let [recs (mapcat record-mesgs files)
        lls (latlngs recs)
        lls (filter-latlngs lls (runmap-bounds))
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
        dim {:x 1024 :y 1024}
        rm (runmap files dim)
        bmp (runmap->bitmap rm dim)]
    (javax.imageio.ImageIO/write bmp "png" (file "runmap.png"))))
