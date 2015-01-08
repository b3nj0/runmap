(ns runmap.core
  (:require [runmap.fitparser :as fitparser]
            [clojure.java.io :as jio]))

(defrecord LatLng [lat lng])

(defn semicircles->degrees [sc]
  (* sc (/ 180 (Math/pow 2 31))))

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
  {:min (LatLng. 51.323264 -0.396881) :max (LatLng. 51.729952 0.379028)})

(defn latlngs [recs]
  "extract lat and lngs from seq of RecordMesgs"
  (->> recs
       (map #(LatLng. (.getPositionLat %1) (.getPositionLong %1)))
       (filter (comp not nil? :lat))
       (filter (comp not nil? :lng))
       (map #(LatLng. (semicircles->degrees (:lat %1)) (semicircles->degrees (:lng %1))))))

(defn bounds [lls]
  "bounding box for lat lngs"
  (let [lats (map :lat lls)
        lngs (map :lng lls)
        latmin (apply min lats)
        latmax (apply max lats)
        lngmin (apply min lngs)
        lngmax (apply max lngs)]
    {:min (LatLng. latmin lngmin) :max (LatLng. latmax lngmax)}))

(defn scale [val domain range]
  (let [[dommin dommax] domain
        dom (- dommax dommin)
        [rngmin rngmax] range
        rng (double (- rngmax rngmin))]
    (+ (* (- val dommin) (/ rng dom)) rngmin)))

(defn latlngs-within [lls bnds]
  (->> lls
       (filter #(> (:lat %) (get-in bnds [:min :lat])))
       (filter #(< (:lat %) (get-in bnds [:max :lat])))
       (filter #(> (:lng %) (get-in bnds [:min :lng])))
       (filter #(< (:lng %) (get-in bnds [:max :lng])))))

(defn runmap-scale [latdom lngdom size]
  (let [latlen (- (apply max latdom) (apply min latdom))
        lnglen (- (apply max lngdom) (apply min lngdom))
        maxlen (max latlen lnglen)]
    {:min (LatLng. 0 0) :max (LatLng. (* size (/ latlen maxlen)) (* size (/ lnglen maxlen)))}))

(defn latlngs-domain [lls]
  (let [bnds (bounds lls)
        latdom [(get-in bnds [:min :lat]) (get-in bnds [:max :lat])]
        lngdom [(get-in bnds [:min :lng]) (get-in bnds [:max :lng])]]
    [latdom lngdom]))

(defn distances [lls]
  "distances from min (lat lng) to (lat lng)"
  (let [minll (:min (bounds lls))
        latdist #(haversine (LatLng. (:lat minll) (:lng %1)) %1)
        lngdist #(haversine (LatLng. (:lat %1) (:lng minll)) %1)]
    (map #(LatLng. (latdist %1) (lngdist %1)) lls)))


(defn scale-latlngs [lls size]
  (let [[latdom lngdom] (latlngs-domain lls)
        rmscale (runmap-scale latdom lngdom size)
        latrng [(get-in rmscale [:min :lat]) (get-in rmscale [:max :lat])]
        lngrng [(get-in rmscale [:min :lng]) (get-in rmscale [:max :lng])]]
    (map #(LatLng. (scale (:lat %1) latdom latrng) (scale (:lng %1) lngdom lngrng)) lls)))

(defn runmap [recs size]
  (let [lls (latlngs recs)
        lls (latlngs-within lls (runmap-limit))
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

(defn fit-files [dir]
  (filter (memfn isFile) (file-seq (jio/file dir))))

(defn -main [& args]
  (let [dir "resources/fit-files"
        files (fit-files dir)
        recs (fitparser/fit-files->record-mesgs files)
        size 1024
        rm (runmap recs size)
        bmp (runmap->bitmap rm)]
    (javax.imageio.ImageIO/write bmp "png" (jio/file "runmap.png"))))
