(ns runmap.core
  (:use [clojure.java.io :as jio]))

(defn semicircles->degrees [sc]
  (* sc (/ 180 (Math/pow 2 31))))

(defn degrees->semicircles [dg]
  (/ dg (/ 180 (Math/pow 2 31))))

(defn degrees->radians [dg]
  (Math/toRadians dg))

(defn haversine [ll1 ll2]
  (let [earth-radius 6371
        lat1 (degrees->radians (:lat ll1))
        lat2 (degrees->radians (:lat ll2))
        dellat (degrees->radians (- (:lat ll2) (:lat ll1)))
        dellng (degrees->radians (- (:lng ll2) (:lng ll1)))
        a (+ (Math/pow (Math/sin (/ dellat 2)) 2) (* (Math/cos lat1) (Math/cos lat2) (Math/pow (Math/sin (/ dellng 2)) 2)))
        c (* 2 (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))]
    (* earth-radius c)))

(defn runmap-limit []
  {:min {:lat 51.323264 :lng -0.396881} :max {:lat 51.729952 :lng 0.379028}})

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

(defn fit-file->record-mesgs [file]
  "extract RecordMesgs from file"
  (let [msgs (atom [])]
    (parse file [(record-collector msgs)])
    @msgs))

(defn fit-files->record-mesgs [files]
  (mapcat fit-file->record-mesgs files))

(defn latlngs [recs]
  "extract lat and lngs from seq of RecordMesgs"
  (->> recs
       (map #(assoc {} :lat (.getPositionLat %1) :lng (.getPositionLong %1)))
       (filter (comp not nil? :lat))
       (filter (comp not nil? :lng))
       (map #(assoc {} :lat (semicircles->degrees (:lat %1)) :lng (semicircles->degrees (:lng %1))))))

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

(defn runmap-scale [latdom lngdom size]
  (let [latlen (- (apply max latdom) (apply min latdom))
        lnglen (- (apply max lngdom) (apply min lngdom))
        maxlen (max latlen lnglen)]
    {:min {:lat 0 :lng 0} :max {:lat (* size (/ latlen maxlen)) :lng (* size (/ lnglen maxlen))}}))

(defn latlngs-domain [lls]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]]
    [latdom lngdom]))

(defn distances [lls]
  "distances from min (lat lng) to (lat lng)"
  (let [minll (:min (bounds lls))
        latdist #(haversine  {:lng (:lng %1) :lat (:lat minll)} %1)
        lngdist #(haversine  {:lat (:lat %1) :lng (:lng minll)} %1)]
    (map #(assoc {} :lat (latdist %1) :lng (lngdist %1)) lls)))


(defn scale-latlngs [lls size]
  (let [[latdom lngdom] (latlngs-domain lls)
        rmscale (runmap-scale latdom lngdom size)
        latrng [(get-in rmscale [:min :lat]) (get-in rmscale [:max :lat])]
        lngrng [(get-in rmscale [:min :lng]) (get-in rmscale [:max :lng])]]
    (map #(assoc {} :lng (scale (:lng %1) lngdom lngrng) :lat (scale (:lat %1) latdom latrng)) lls)))

(defn runmap [recs size]
  (let [lls (latlngs recs)
        lls (filter-latlngs lls (runmap-limit))
        lls (distances lls)
        lls (scale-latlngs lls size)]
    lls))

(defn runmap->bitmap [rm]
  (let [xys (map #(vector (:lng %) (:lat %)) rm)
        maxx (apply max (map #(get % 0) xys))
        maxy (apply max (map #(get % 1) xys))
        img (java.awt.image.BufferedImage. maxx maxy java.awt.image.BufferedImage/TYPE_BYTE_GRAY)
        g (.createGraphics img)]
    (doall (map (fn [[x y]] (.drawRect g x (- maxy y) 1 1)) xys))
    img))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (fit-files dir)
        recs (fit-files->record-mesgs files)
        size 1024
        rm (runmap recs size)
        bmp (runmap->bitmap rm)]
    (javax.imageio.ImageIO/write bmp "png" (file "runmap.png"))))
