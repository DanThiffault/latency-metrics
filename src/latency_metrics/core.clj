(ns latency-metrics.core
  (:require [clojure.java.io :as io])
  (:use [clojure.string :only (join split)])
  (:use [incanter core stats charts datasets])
  (:import [java.text SimpleDateFormat ParsePosition])
  (:import [java.util Date]))

;; process a file from the following command
;; ping 203.212.24.81 | while read pong; do echo "$(date): $pong"; done >> ping.log

;; Run the following from a repl
;(def plot-data (generate-plot-data (generate-results input-file-name)))
;(plot-data-to-csv output-file-name plot-data)

(def input-file-name "ping.log")
(def output-file-name "latency-metrics.csv")
(def log-date-format (SimpleDateFormat. "yyyy-MMM-dd-HH:mm:ss z"))
(def out-date-format (SimpleDateFormat. "yyyy-MMM-dd-HH:mm"))
(def min-per-sample 30)

(defn extract-time [segments]
  (let [segment (nth segments 12)]
    (Double/valueOf (subs segment 5 (count segment)))))

(defn extract-seq-no [segments]
  (let [segment (nth segments 10)]
    (Integer/valueOf (subs segment 9 (count segment)))))

(defn extract-ip [segments]
  (let [segment (nth segments 9)]
    (subs segment 0 (dec (count segment)))))

(defn extract-timestamp [segments]
  (str 
   (join "-" [(subs (nth segments 5) 0 (dec (count (nth segments 5))))
              (nth segments 1)
              (nth segments 2)
              (nth segments 3)])
   " " (nth segments 4)))

(defn create-date [time-string]
  (.parse log-date-format time-string (ParsePosition. 0)))

(defn convert-timestamp [segments]
  (-> segments 
      extract-timestamp
      create-date))

(defmulti parse-segments 
  (fn [segments] 
    (if (or (and (< 13 (count segments)) (= "Unreachable" (nth segments 13))) 
            (and (< 6 (count segments)) (or (= "---" (nth segments 6)) (= "round-trip" (nth segments 6))))) :default
    (case (count segments)
      14 :success
      11 :timeout
      :default))))

(defmethod parse-segments :timeout [segments] 
  {:timestamp (convert-timestamp segments)
   :seq-no    (Integer/valueOf (nth segments 10))   
   :timed-out true
   :time      2000})

(defmethod parse-segments :success [segments]
  {:timestamp (convert-timestamp segments)
   :ip        (extract-ip segments)
   :seq-no    (extract-seq-no segments)
   :time      (extract-time segments)})

(defmethod parse-segments :default [segments]
  {:parse-error (join " " segments)})

(defn parse-line [line] (parse-segments (split line #"\s")))

(defn process-log-file
  "Read the log file and convert it into a vector of information"
  [filename]
  (with-open [rdr (java.io.BufferedReader. 
                 (java.io.FileReader. filename))]
  (let [seq (line-seq rdr)]
    (doall (map parse-line seq)))))

(defn add-time-grouping  [entry] 
  "Add a time grouping to a log entry"
  (conj entry         
         (let [millis (.getTime (:timestamp entry))
               rounded-millis (- millis (mod millis (* 1000 60 min-per-sample)))] 
           {:millis rounded-millis
            :time-group (Date. rounded-millis)})))

(defn generate-box-data [data]
  "Generate statistical data for group log entries"
  (let [grouped (group-by :time-group data)]
    (for [[k v] grouped] 
               ((fn [v]
                     (let [_values (map :time v)
                           _mean    (mean _values)
                           _sd      (sd _values)]
                       {:timeouts (count (filter :timed-out v))
                        :samples (count _values)
                        :mean _mean 
                        :sd _sd
                        :timestamp (.format out-date-format k)
                        :first-quartile (- _mean _sd)
                        :max            (min (apply max _values) 2000)
                        :min            (apply min _values)
                        :third-quartile (+ _mean _sd)})) v))))

(defn generate-plot-data [data]
  "Generate split counts"
  (let [grouped (group-by :time-group data)]
    (for [[k v] grouped] 
               ((fn [v]
                     (let [_values (map :time v)
                           _mean    (mean _values)
                           _sd      (sd _values)]
                       {:timeouts (count (filter :timed-out v))
                        :time-500(count (filter #(and (not (contains? %1 :timed-out)) (< 500 (:time %1))) v))
                        :time-250(count (filter #(< 250 (:time %1) 500) v))
                        :time-100(count (filter #(< 100 (:time %1) 250) v))                        
                        :time-50(count (filter #(< 50 (:time %1) 100) v))
                        :time-25(count (filter #(< 25 (:time %1) 50) v))
                        :time-10(count (filter #(< 10 (:time %1) 25) v))
                        :time-0(count (filter #(> 10 (:time %1)) v))
                        :samples (count _values)
                        :mean _mean 
                        :timestamp (.format out-date-format k)})) v))))

(defn generate-normalized-time-split [data split-time]
  "Generate parameterized split count"
  (let [grouped (group-by :time-group data)]
    (for [[k v] grouped] 
               ((fn [v]
                     (let [_values (map :time v)]
                       {:timestamp k
                        :timeouts (count (filter :timed-out v))
                        :samples (count _values)
                        :count-below (count (filter #(> split-time (:time %1)) v))
                        :count-above (count (filter #(<= split-time (:time %1)) v))
                        })) v))))

(defn write-plot-data-line [out-file entry]
  "Write a line preserving ordering"
  (.write out-file (str (join "," [(:timestamp entry)
                              (:mean entry)
                              (:samples entry)
                              (:time-0 entry)
                              (:time-10 entry)
                              (:time-25 entry)
                              (:time-50 entry)
                              (:time-100 entry)
                              (:time-250 entry)
                              (:time-500 entry)
                              (:timeouts entry)]) "\n")))

(defn plot-data-to-csv [filename data]
  "Ouput plot data to a csv"
  (let [values (map #(vals %) data)]
  (with-open [out-file (io/writer filename)]
    (doall (map (partial write-plot-data-line out-file) data)))))

(defn generate-results [file-name] 
  (->> file-name
       process-log-file 
       (filter (complement #(contains? % :parse-error)))
       (map add-time-grouping)))

(defn chart-split [file-name split]
  "Create a chart of % requests >= the supplied split"
  (let [split-data (generate-normalized-time-split (generate-results input-file-name) split)
        sorted-data (sort-by :timestamp split-data)]
    (bar-chart (map :timestamp sorted-data) (map #(* 100 (/ (:count-above %1) (:samples %1))) sorted-data) :y-label "% above" :x-label "time recorded" :series-label "% above" :title (str "% Above " split "ms") )))

(defn view-chart-split [file-name split]
  (view (chart-split file-name split)))

(defn save-chart-split [input-file-name split chart-file-name]
  (save (chart-split input-file-name split) chart-file-name :width 1000 :height 800))
