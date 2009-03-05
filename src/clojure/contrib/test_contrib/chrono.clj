(ns clojure.contrib.test-contrib.chrono
  (:use clojure.contrib.test-is)
  (:use :reload-all clojure.contrib.chrono))

(def christmas (date 2007 12 25, 3 00 02))
(def new-years (date 2008 1 1))
(def day-one (date 2008 11 21, 11 21 48))

(deftest test-date-creation-lookup
  (is (= 2008 (day-one :year)))
  (is (= 11 (day-one :month)))
  (is (= 21 (day-one :day)))
  (is (= 11 (day-one :hour)))
  (is (= 21 (day-one :minute)))
  (is (= 48 (day-one :second)))
  (is (= 48 (:second day-one)))
  ;; overflows simply roll over to the next month/year/etc.
  (is (= 1 ((date 2008 1 32) :day))))

(deftest test-equality
  (is (= (date 2009 3 2)
         (date 2009 3 2)))
  (is (not (= 25 christmas)))
  (is (not (= christmas 25))))

(deftest test-to-string
  (is (= "2007-12-25 03:00:02" (.toString christmas))))

(deftest test-later
  (is (= (date 2009 11 21 11 21 48)
         (later day-one 1 :year)))
  (is (= (date 2008 10 21 11 21 48)
         (later day-one -1 :month)))
  (is (= (date 2008 11 24 11 21 48)
         (later day-one 3 :day)))
  (is (= (date 2008 11 21 12 21 48)
         (later day-one 1 :hour)))
  (is (= (date 2008 11 21 13 1 48)
         (later day-one 100 :minute)))
  (is (= (date 2008 11 21 11 21 49)
         (later day-one 1 :second)))
  (is (= (later christmas :day)
         (later christmas 1 :day)))
  (let [party (date 2007 12 31, 22 0 0)
        later-party (date 2007 12 31, 23 0 0)]
    (is (= later-party (later party :hour)))))

(deftest test-earlier
  (is (= (date 2008 8 13 11 21 48)
         (earlier day-one 100 :day)))
  (is (= (date 2008 11 23 11 21 48)
         (earlier day-one -2 :day)))
  (is (= (date 2008 11 21 9 21 48)
         (earlier day-one 2 :hour))))

(deftest test-earlier?
  (is (earlier? (date 2008 12 12)
                (date 2009 12 12))))

(deftest test-later?
  (is (later? (date 2008 12 99)
              (date 2009 1 1))))

(deftest test-time-between
  ;; Seconds is the default unit
  (is (= 5 (time-between (date 2009 1 1, 10 10 10)
                            (date 2009 1 1, 10 10 15))))
  (is (= 10 (time-between (date 2009 1 1, 10 10 10)
                          (date 2009 1 1, 10 20 10)
                          :minutes)))
  (is (= 6 (int (time-between christmas new-years :day)))))

(deftest test-date-seq
  (is (= (list christmas
               (date 2007 12 26, 3 0 02)
               (date 2007 12 27, 3 0 02)
               (date 2007 12 28, 3 0 02)
               (date 2007 12 29, 3 0 02)
               (date 2007 12 30, 3 0 02)
               (date 2007 12 31, 3 0 02))
         (date-seq :day christmas new-years)))
  (let [party (date 2007 12 31, 22 0 0)
        party2 (date 2007 12 31, 23 0 0)
        the-seq (date-seq :hour party new-years)]
    (is (= (list party party2)
           (take 2 the-seq)))))


(deftest test-beginning-of
  (is (= (date 2007 12 1) (beginning-of christmas :month)))
  (is (= (date 2007 12 25) (beginning-of christmas :day)))
  ;; (is (= (date 2007 12 30) (beginning-of christmas :week)))
  (is (= new-years (beginning-of new-years :year))))

(deftest test-end-of
  ;; (is (= (date 2008 1 5) (end-of new-years :week)))
  (is (= (date 2007 12 31 23 59 59) (end-of christmas :month)))
  (is (= (date 2007 12 25, 23 59 59) (end-of christmas :day)))
  (is (= (date 2007 12 25, 3 59 59) (end-of christmas :hour))))

(deftest test-java-date-formats
  (is (= "11/21/08" (format-date day-one :short-date)))
  (is (= "Dec 25, 2007" (format-date christmas :medium-date)))
  (is (= "January 1, 2008" (format-date new-years :long-date)))
  (is (= "Thursday, December 25, 2008" (format-date (later christmas 1 :year) :full-date)))
  ;; Time zone might not be the same everywhere, so use .startsWith
  (is (.startsWith (format-date christmas :medium-date-time) "Dec 25, 2007 3:00:02 AM")))

(deftest test-java-date-parsers
  (is (= (date 2008 11 21) (parse-date "11/21/08" :short-date)))
  (is (= (date 2007 12 25) (parse-date "Dec 25, 2007" :medium-date)))
  (is (= new-years (parse-date "January 1, 2008" :long-date)))
  (is (= (date 2008 12 25) (parse-date "Thursday, December 25, 2008" :full-date)))
  (is (= christmas (parse-date "Dec 25, 2007 3:00:02 AM" :medium-date-time))))

(deftest test-iso-date-format
  (is (= (date 2008 12 25) (parse-date "2008-12-25 00:00:00" :iso8601)))
  (is (= "2008-11-21 11:21:48" (format-date day-one :iso8601))))

