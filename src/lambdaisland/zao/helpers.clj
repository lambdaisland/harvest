(ns lambdaisland.zao.helpers)

(defn days-ago [n]
  (.minusDays
   (java.time.ZonedDateTime/now (java.time.ZoneId/of "UTC")) n))

(defn days-from-now [n]
  (.plusDays
   (java.time.ZonedDateTime/now (java.time.ZoneId/of "UTC")) n))

(defn sequence
  ([]
   (let [cnt (volatile! 0)]
     #(vswap! cnt inc)))
  ([f]
   (let [s (sequence)] #(f (s)))))
