(ns latency-metrics.core-test
  (:use clojure.test
        latency-metrics.core))
                                                                                                                                           
(deftest parse-line-test
  (testing "Successful ping"
    (let [result (parse-line "Tue Apr 23 16:47:15 IST 2013: 64 bytes from 203.212.24.81: icmp_seq=0 ttl=63 time=3.779 ms")]
      (is (= #inst "2013-04-23T14:47:15.000-00:00" (:timestamp result)))
      (is (= "203.212.24.81" (:ip result)))
      (is (= 0 (:seq-no result)))
      (is (= 3.779 (:time result)))))
  (testing "Timeout"
    (let [result (parse-line "Tue Apr 23 21:52:30 IST 2013: Request timeout for icmp_seq 12835")]
      (is (= #inst "2013-04-23T19:52:30.000-00:00" (:timestamp result)))
      (is (= 12835 (:seq-no result)))
      (is (= true (:timed-out result)))
      (is (= 2000 (:time result)))))
  (testing "Gracefully handle noise"
    (is (contains? (parse-line "Fri Apr 26 18:46:04 IST 2013: --- 203.212.24.241 ping statistics ---" ) :parse-error))
    (is (contains? (parse-line "Fri Apr 26 18:46:04 IST 2013: round-trip min/avg/max/stddev = 2.675/21.858/13005.142/373.103 ms" ) :parse-error))))

(defn gen-result [time]
  { :timestamp #inst "2013-04-23T14:47:15.000-00:00" :ip "203.212.24.81"  :seq-no 0 :time time :millis 1366727400000 :time-group #inst "2013-04-23T14:30:00.000-00:00" })

(def results (into [] (concat (repeat 5 (gen-result 3.779))
                     (repeat 10 (gen-result 13.779))
                     (repeat 25 (gen-result 33.779))
                     (repeat 50 (gen-result 53.779))
                     (repeat 100 (gen-result 153.779))
                     (repeat 250 (gen-result 253.779))
                     (repeat 500 (gen-result 553.779))
                     (repeat 3 {:timestamp #inst "2013-04-23T14:47:15.000-00:00" :seq-no 1 :timed-out true :time 2000 :millis 1366727400000 :time-group #inst "2013-04-23T14:30:00.000-00:00"  }))))

(deftest generate-plot-data-test
  (testing "Generate plot data"
    (let [plot-data (generate-plot-data results)
          data (first plot-data)]
      (is (= 943 (:samples data)))
      (is (< 387   (:mean data) 388))
      (is (= 5   (:time-0 data)))
      (is (= 10   (:time-10 data)))
      (is (= 25   (:time-25 data)))
      (is (= 50   (:time-50 data)))
      (is (= 100   (:time-100 data)))
      (is (= 250   (:time-250 data)))
      (is (= 500   (:time-500 data)))
      (is (= 3   (:timeouts data))))))