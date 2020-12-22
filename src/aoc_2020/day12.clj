(ns aoc-2020.day12
  (:require [clojure.string :as str]))

(defn- input->vals
  [input]
  (->> input
       (str/split-lines)
       (mapv (fn [line]
               (let [[_ dir len] (re-find #"([NESWLRF])(\d+)" line)]
                 {:dir (keyword dir)
                  :len (Long/parseLong len)})))))

(defmulti move (fn [_location {:keys [dir]}]
                 dir))

(defmethod move :N
  [location {:keys [len]}]
  (update location :y + len))

(defmethod move :S
  [location {:keys [len]}]
  (update location :y - len))

(defmethod move :E
  [location {:keys [len]}]
  (update location :x + len))

(defmethod move :W
  [location {:keys [len]}]
  (update location :x - len))

(defmethod move :F
  [{:keys [heading] :as location} step]
  (move location (assoc step :dir heading)))

(defn rotate-clockwise
  [heading ^long degrees]
  (case degrees
    0 heading
    90 ({:N :E
         :E :S
         :S :W
         :W :N} heading)
    180 ({:N :S
          :S :N
          :E :W
          :W :E} heading)
    270 ({:N :W
          :W :S
          :S :E
          :E :N} heading)
    360 heading))

(defmethod move :R
  [location {:keys [len]}]
  (update location :heading rotate-clockwise len))

(defmethod move :L
  [location {:keys [^long len]}]
  (update location :heading rotate-clockwise (- 360 len)))

(defn travel
  [path]
  (let [initial-location {:x       0
                          :y       0
                          :heading :E}]
    (reduce
      (fn [location step] (move location step))
      initial-location
      path)))

(defmulti move-waypoint (fn [_location {:keys [dir]}]
                          dir))

(defmethod move-waypoint :N
  [location {:keys [len]}]
  (update location :wy + len))

(defmethod move-waypoint :S
  [location {:keys [len]}]
  (update location :wy - len))

(defmethod move-waypoint :E
  [location {:keys [len]}]
  (update location :wx + len))

(defmethod move-waypoint :W
  [location {:keys [len]}]
  (update location :wx - len))

(defmethod move-waypoint :F
  [{:keys [^long wx ^long wy] :as location} {:keys [^long len]}]
  (-> location
      (update :x + (* wx len))
      (update :y + (* wy len))))

(defn rotate-angle-counter-clockwise
  [^long x ^long y ^long degrees]
  (let [theta     (case degrees
                    0 0
                    90 (* (Math/PI) 0.5)
                    180 (Math/PI)
                    270 (* (Math/PI) 1.5)
                    360 0)
        cos-theta (Math/cos theta)
        sin-theta (Math/sin theta)
        xr        (- (* x cos-theta)
                     (* y sin-theta))
        yr        (+ (* x sin-theta)
                     (* y cos-theta))
        round     (fn [^double v]
                    (Math/round v))]
    [(round xr) (round yr)]))

(defmethod move-waypoint :R
  [{:keys [wx wy] :as location} {:keys [^long len]}]
  (let [[wxr wyr] (rotate-angle-counter-clockwise wx wy (- 360 len))]
    (assoc location :wx wxr
                    :wy wyr)))

(defmethod move-waypoint :L
  [{:keys [wx wy] :as location} {:keys [len]}]
  (let [[wxr wyr] (rotate-angle-counter-clockwise wx wy len)]
    (assoc location :wx wxr
                    :wy wyr)))

(defn travel-towards-waypoint
  [path]
  (let [initial-location {:x       0
                          :y       0
                          :wx      10
                          :wy      1
                          :heading :E}]
    (reduce
      (fn [location step]
        (move-waypoint location step))
      initial-location
      path)))

(defn distance
  [{:keys [^long x ^long y]}]
  (+ (Math/abs x)
     (Math/abs y)))

(defn data->distance
  [data]
  (-> data
      (input->vals)
      (travel)
      (distance)))

(defn data->waypoint-distance
  [data]
  (-> data
      (input->vals)
      (travel-towards-waypoint)
      (distance)))
