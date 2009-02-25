(ns clojure.contrib.test-contrib.chrono
  (:use clojure.contrib.test-is)
  (:use :reload-all clojure.contrib.chrono))

(def christmas (date 2007 12 25, 3 00 02))
(def new-years (date 2008 1 1))
(def day-one (date 2008 11 21, 11 21 48))

(deftest test-date-creation
  ;; TODO: Not sure about the interface here. We may not be able to
  ;; pull off map-like keyword lookup for dates depending on internal
  ;; representation. May need get-year etc. style instead?
  (is (= 2008 (:year day-one)))
  (is (= 11 (:month day-one)))
  (is (= 21 (:day day-one)))
  (is (= 11 (:hour day-one)))
  (is (= 21 (:minute day-one)))
  (is (= 48 (:second day-one)))
  ;; overflows simply roll over to the next month/year/etc.
  (is (= 1 (:day (date 2008 1 32)))))

;; (deftest test-date?
;;   (is (date? day-one))
;;   (is (not (date? 14))))

;; (deftest test-parse-date
;;   ;; TODO: What formats do we handle? Try to be as lenient as possible.
;;   (is (= day-one (parse-date "2008 Nov 21 11:21:48"))))

;; (deftest test-format-date
;;   (is (= "2008 Nov 21 11:21:48" (format-date day-one)))
;;   (is (= "2008-11-21 11:12Z" (format-date day-one :iso))))

;; ;; TODO: test string representations outside format. .toString?

;; This is currently broken. Like nearly everything.

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
         (later christmas 1 :day))))

(deftest test-earlier
  (is (= (date 2008 8 13 11 21 48)
         (earlier day-one 100 :day)))
  (is (= (date 2008 11 23 11 21 48)
         (earlier day-one -2 :day)))
  (is (= (date 2008 11 21 9 21 48)
         (earlier day-one 2 :hour))))

;; Better would be to make > and < etc. work with dates; then >= would
;; be free. But this involves a change to core, I think?

(deftest test-earlier?
  (is (earlier? (date 2008 12 12)
                (date 2009 12 12))))

(deftest test-later?
  (is (later? (date 2008 12 99)
              (date 2009 1 1))))

;; (deftest test-time-between
;;   ;; Leaning towards seconds being the default unit.
;;   (is (= 5 (time-between (date 2009 1 1, 10 10 10)
;;                          (date 2009 1 1, 10 10 15))))
;;   (is (= 10 (time-between (date 2009 1 1, 10 10 10)
;;                           (date 2009 1 1, 10 20 10)
;;                           :minutes)))
;;   ;; This means it rounds... if you ask for days, you get integral
;;   ;; days. Not sure that this is agreeable.
;;   (is (= 7 (time-between christmas new-years :days))))

;; (deftest test-date-seq
;;   ;; TODO: should return a lazy seq of all dates between the two dates
;;   ;; given, with an interval provided.
;;   )

;; (deftest test-beginning-of
;;   (is (= (date 2007 12 1) (beginning-of christmas :month)))
;;   (is (= (date 2007 12 25) (beginning-of christmas :day)))
;;   (is (= new-years (beginning-of new-years :year)))
;;   (is (= (date 2007 12 30) (beginning-of christmas :week))))

;; (deftest test-end-of
;;   (is (= (date 2008 1 5) (end-of new-years :week)))
;;   (is (= (date 2007 12 31 (end-of christmas :month))))
;;   (is (= (date 2007 12 25, 23 59 59) (end-of christmas :day)))
;;   (is (= (date 2007 12 25, 3 59 59) (end-of christmas :hour))))

;; TODO: time zone stuff?

(run-tests)