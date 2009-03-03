;;; chrono.clj --- Because calling it date-utils would be boring.

;; By Matt Moriarity and Phil Hagelberg

;;; Use the date function to create dates. You can look up components
;;; much like you would in a map:
;;
;; (def my-date (date 2009 2 27 12 34 56))
;;
;; (my-date :year)   ;; 2009
;; (my-date :month)  ;; 2
;; (my-date :day)    ;; 27
;; (my-date :hour)   ;; 12
;; (my-date :minute) ;; 34
;; (my-date :second) ;; 56
;;
;; Currently (:day my-date) style is unsupported, but it doesn't look
;; like it would be hard to add; see TODO under the defn of date.
;;
;;; You may omit the time if you like:
;;
;; (def my-other-date (date 2009 2 27))
;; (my-other-date :hour) ;; 0
;;
;;; To get a date relative to another date, use earlier and later:
;;
;; (earlier my-date 100 :minute) ;; 2009 2 27 10:54:56
;; (later my-other-date 10 :day) ;; 2009 3 9
;;
;;; For comparing dates, use earlier? and later?:
;;
;; (earlier? my-date my-other-date) ;; false
;; (later? (later my-date 10 :day) my-date) ;; true
;;
;;; You can see the time between two dates by calling time-between:
;;
;; (time-between my-other-date (date 2009 2 25) :days) ;; 2
;;
;; The date-seq function returns a lazy seq of dates incrementing by
;; the units in its first arg starting from its second arg. The third
;; arg if given dictates the end of the sequence.
;;
;; (date-seq :hours my-other-date my-date) ;; (a seq of twelve hours)
;; (take 4 (date-seq :years my-date))
;; ;; (date 2009 2 27 12 34 56) (date 2010 2 27 12 34 56)
;; ;; (date 2011 2 27 12 34 56) (date 2012 2 27 12 34 56) [...]
;;
;; See test_contrib/chrono.clj for more details.
;;

(ns clojure.contrib.chrono
  (:import (java.util Calendar TimeZone)
           (java.text DateFormat SimpleDateFormat)))

;; Use to resolve keywords
(def this-ns (str *ns*))

(def #^{:doc "Conversion of Calendar weekdays to keywords"}
     weekday-map
     {Calendar/SUNDAY :sunday
      Calendar/MONDAY :monday
      Calendar/TUESDAY :tuesday
      Calendar/WEDNESDAY :wednesday
      Calendar/THURSDAY :thursday
      Calendar/FRIDAY :friday
      Calendar/SATURDAY :saturday})

(def #^{:doc "Conversion of unit keywords to Calendar units"}
     units-to-calendar-units
     {:year Calendar/YEAR,
      :month Calendar/MONTH,
      :day Calendar/DATE,
      :hour Calendar/HOUR_OF_DAY,
      :minute Calendar/MINUTE,
      :second Calendar/SECOND})

(def #^{:doc "Number of milliseconds in each unit"}
     units-in-milliseconds
     {:year 31557600000,
      :month 2592000000,
      :week 67929088,
      :day 86400000,
      :hour 3600000,
      :minute 60000,
      :second 1000,
      :millisecond 1})

(defn- make-calendar
  "Given some date values, create a Java Calendar object with only that data."
  ([] (doto (Calendar/getInstance)
        (.clear)
        (.setLenient true)))
  ([calendar]
     (.clone calendar))
  ([year month day]
     (doto (make-calendar)
       (.set year (dec month) day)))
  ([year month day hours minutes]
     (doto (make-calendar)
       (.set year (dec month) day hours minutes)))
  ([year month day hours minutes seconds]
     (doto (make-calendar)
       (.set year (dec month) day hours minutes seconds))))

(defn- get-unit [calendar unit]
  (.get calendar (units-to-calendar-units unit)))

(defmulti
  #^{:doc "Take in a date and a format (either a keyword or
a string) and return a string with the formatted date."}
  format-date (fn [date form] form))

(defmulti
  #^{:doc "Take in a string with a formatted date and a format
 (either a keyword or a string) and return a parsed date."}
  parse-date (fn [source form] form))

;; (gen-interface
;;  :name clojure.contrib.chrono.Instant
;;  :extends [clojure.lang.IFn])

(defn date
  "Returns a new date object. Takes year, month, and day as args as
  well as optionally hours, minutes, and seconds."
  [& args]
  (let [calendar (apply make-calendar args)]
    ;; TODO: if we implement Associative, then we can use
    ;; (:day my-date) as well as (my-date :day)
    (proxy [clojure.lang.IFn] []
      (toString [] (str "#<ChronoDate "
                        (format-date this :iso8601)
                        ">"))
      ;; look up :year, :month, :date, :weekday, etc.
      (equals [other-date]
              (.equals calendar (other-date :calendar)))
      (invoke [unit]
              (cond (= :calendar unit) calendar ;; mostly for internal use
                    (= :month unit) (inc (get-unit calendar :month))
                    true (get-unit calendar unit))))))

;;; Relative functions

(defn later
  "Returns a date that is later than the-date by amount units."
  ([the-date amount units]
     (date (doto (.clone (the-date :calendar))
             (.set (units-to-calendar-units units)
                   (+ (.get (the-date :calendar)
                            (units-to-calendar-units units))
                      amount)))))
  ([the-date units]
     (later the-date 1 units)))

(defn earlier
  "Returns a date that is earlier than the-date by amount units."
  ([the-date amount units]
     (later the-date (- amount) units))
  ([the-date units]
     (later the-date -1 units)))

(defn later? [date-a date-b]
  "Is date-a later than date-b?"
  (.after (date-a :calendar) (date-b :calendar)))

(defn earlier? [date-a date-b]
  "Is date-a earlier than date-b?"
  (.before (date-a :calendar) (date-b :calendar)))

(defn time-between
  "How many units between date-a and date-b? Units defaults to milliseconds."
  ;; TODO: should we default to milliseconds just because that's what
  ;; the underlying implementation uses? Is it a leaky abstraction?
  ([date-a date-b]
     (java.lang.Math/abs
      (- (.getTimeInMillis (date-a :calendar))
         (.getTimeInMillis (date-b :calendar)))))
  ([date-a date-b units]
     ;; TODO: should we move plural support up to
     ;; units-in-milliseconds and units-to-calendar-units?
     (let [units (if (re-find #"s$" (name units)) ;; Allow plurals
                   ;; This relies on the patched subs defn below
                   (keyword (subs (name units) 0 -1))
                   units)]
       (/ (time-between date-a date-b)
          (units-in-milliseconds units)))))

(defn date-seq
  "Returns a lazy seq of dates starting with from up until to in
  increments of units. If to is omitted, returns an infinite seq."
  ([units from to]
     (lazy-seq
       (when (or (nil? to) (earlier? from to))
         (cons from (date-seq units (later from units) to)))))
  ([units from] (date-seq units from nil)))

(defmacro def-date-format [fname [arg] & body]
  `(defmethod format-date ~(keyword (name fname)) [~arg ~'_]
     ~@body))

(defmacro def-date-parser [fname [arg] & body]
  `(defmethod parse-date ~(keyword (name fname)) [~arg ~'_]
     ~@body))

(defmacro def-java-date-format [fname formatter]
  `(do
     (def-date-format ~fname [date#]
       (.format ~formatter
                (.getTime (date# :calendar))))
     (def-date-parser ~fname [source#]
       (date
        (doto (make-calendar)
          (.setTime (.parse ~formatter source#)))))))

(def-java-date-format short-date
  (DateFormat/getDateInstance DateFormat/SHORT))

(def-java-date-format medium-date
  (DateFormat/getDateInstance DateFormat/MEDIUM))

(def-java-date-format long-date
  (DateFormat/getDateInstance DateFormat/LONG))

(def-java-date-format full-date
  (DateFormat/getDateInstance DateFormat/FULL))

(def-java-date-format short-date-time
  (DateFormat/getDateTimeInstance
   DateFormat/SHORT
   DateFormat/SHORT))

(def-java-date-format medium-date-time
  (DateFormat/getDateTimeInstance
   DateFormat/MEDIUM
   DateFormat/MEDIUM))

(def-java-date-format long-date-time
  (DateFormat/getDateTimeInstance
   DateFormat/LONG
   DateFormat/LONG))

(def-java-date-format full-date-time
  (DateFormat/getDateTimeInstance
   DateFormat/FULL
   DateFormat/FULL))

;; TODO: we should support ISO 8601 formats.

;;; Formats dates with a custom string format
(defmethod format-date :default [date form]
  (.format (SimpleDateFormat. form)
           (.getTime (date :calendar))))

;;; Parse a date from a string format
(defmethod parse-date :default [source form]
  (date
   (doto (make-calendar)
     (.setTime (.parse (SimpleDateFormat. form) source)))))

(def-date-format iso8601 [date]
  (format-date date "yyyy-MM-dd HH:mm:ss"))

(def-date-parser iso8601 [source]
  (parse-date source "yyyy-MM-dd HH:mm:ss"))

;; Redefine subs to allow for negative indices.
;; TODO: This should be submitted as a patch to Clojure.
(in-ns 'clojure.core)
(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([#^String s start] (subs s start (count s)))
  ([#^String s start end]
     (let [count-back #(if (< % 0) (+ (count s) %) %)]
       (.substring s (count-back start) (count-back end)))))
