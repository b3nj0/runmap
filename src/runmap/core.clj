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

(defn runmap-scale [latdom lngdom size]
  (let [latlen (- (apply max latdom) (apply min latdom))
        lnglen (- (apply max lngdom) (apply min lngdom))
        maxlen (max latlen lnglen)]
    {:y (* 2 size (/ latlen maxlen)) :x (* size (/ lnglen maxlen))}))

(defn scale-latlngs [lls size]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]
        rmscale (runmap-scale latdom lngdom size)
        latrng [0 (:x rmscale)]
        lngrng [0 (:y rmscale)]]
    (map #(vector (scale (:lng %1) lngdom lngrng) (scale (:lat %1) latdom latrng)) lls)))

(defn runmap [recs size]
  (let [lls (latlngs recs)
        lls (filter-latlngs lls (runmap-limit))
        lls (scale-latlngs lls size)]
    lls))

(defn runmap->bitmap [rm]
  (let [maxx (apply max (map #(get % 0) rm))
        maxy (apply max (map #(get % 1) rm))
        img (java.awt.image.BufferedImage. maxx maxy java.awt.image.BufferedImage/TYPE_BYTE_GRAY)
        g (.createGraphics img)]
    (doall (map (fn [[x y]] (.drawRect g x (- maxy y) 1 1)) rm))
    img))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (fit-files dir)
        recs (fit-files->record-mesgs files)
        size 1024
        rm (runmap recs size)
        bmp (runmap->bitmap rm)]
    (javax.imageio.ImageIO/write bmp "png" (file "runmap.png"))))
