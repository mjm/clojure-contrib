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
;;; For converting between strings and dates, use format-date and
;;; parse-date
;;
;; (format-date my-date :short-date-time) ;; 2/27/09 12:34 PM
;; (format-date my-other-date :long-date) ;; February 27, 2009
;; (parse-date "12/25/09" :short-date) ;; (date 2009 12 25)
;; (parse-date "January 1, 2008 1:45:23 PM EST" :long-date-time)
;; ;; (date 2008 1 1 13 45 23)
;;
;; Supported date formats are:
;;   iso8601
;;   short-date
;;   medium-date
;;   long-date
;;   full-date
;;   short-date-time
;;   medium-date-time
;;   long-date-time
;;   full-date-time
;;
;; Both format-date and parse-date also support a string for the
;; format argument, which will use the string as the format for a
;; SimpleDateFormat (see the javadocs of that class for how to write
;; these formats).
;;
;; See test_contrib/chrono.clj for more details.
;;
;;; TODO:
;;
;; * Timezones
;; * More support for week-based units
;; * Various others scattered through code
;;

(ns clojure.contrib.chrono
  (:import (java.util Calendar TimeZone)
           (java.text DateFormat SimpleDateFormat)))

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
  "Given some date values, create a Java Calendar object."
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
  format-date (fn [date & form] (first form)))

(defmulti
  #^{:doc "Take in a string with a formatted date and a format
 (either a keyword or a string) and return a parsed date."}
  parse-date (fn [source & form] (first form)))

(defn date
  "Returns a new date object. Takes year, month, and day as args as
  well as optionally hours, minutes, and seconds."
  [& args]
  (let [calendar (apply make-calendar args)]
    (proxy [clojure.lang.IFn clojure.lang.Associative] []
      (toString [] (format-date this :iso8601))
      (equals [other-date]
              (and (instance? (.getClass this) other-date)
                   (.equals calendar (other-date :calendar))))
      ;; look up :year, :month, :date, etc.
      (invoke [unit]
              (cond (= :calendar unit) calendar ;; mostly for internal use
                    (= :month unit) (inc (get-unit calendar :month))
                    true (get-unit calendar unit)))
      ;; These (along with implementing Associative) allow us to use
      ;; (:month my-date), etc. Good idea? Not sure.
      (valAt [unit] (.invoke this unit))
      (equiv [o] (.equals this o)))))

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

(defn- args-for "Allow round-tripping through date function"
  [date]
  [(date :year) (date :month) (date :day)
   (date :hour) (date :minute) (date :second)])

(defn beginning-of
  "Return a date at the beginning of the month, year, day, etc. from the-date."
  [the-date unit]
  ;; TODO: clean up!
  (let [position ({:year 1 :month 2 :day 3 :hour 4 :minute 5 :second 6} unit)]
    (apply date (concat (take position (args-for the-date))
                        (drop position [1970 1 1 0 0 0])))))

(defn end-of
  "Return a date at the end of the month, year, day, etc. from the-date."
  [the-date unit]
  ;; TODO: this is kinda ugly too?
  (earlier (later (beginning-of the-date unit) unit) :second))

(defn date-seq
  "Returns a lazy seq of dates starting with from up until to in
  increments of units. If to is omitted, returns an infinite seq."
  ([units from to]
     (lazy-seq
       (when (or (nil? to) (earlier? from to))
         (cons from (date-seq units (later from units) to)))))
  ([units from] (date-seq units from nil)))

;;; Formatting and Parsing

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

;;; Formats dates with a custom string format
(defmethod format-date :default [date form]
  (.format (SimpleDateFormat. form)
           (.getTime (date :calendar))))

;;; Parse a date from a string format
(defmethod parse-date :default [source form]
  (date
   (doto (make-calendar)
     (.setTime (.parse (SimpleDateFormat. form) source)))))

(defmethod format-date nil [date]
  (format-date date :iso8601))

(defmacro def-simple-date-format [fname form]
  `(do
     (def-date-format ~fname [date#]
       (format-date date# ~form))
     (def-date-parser ~fname [source#]
       (parse-date source# ~form))))

(def-simple-date-format iso8601 "yyyy-MM-dd HH:mm:ss")

;; TODO: parse-date should be able to guess at the format

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
