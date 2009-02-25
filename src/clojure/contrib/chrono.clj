;;; A date library that follows principle of least surprise
;;;
;;; A few usage examples:
;;;
;;;   user> (now)
;;;   {:second 24, :minute 10, :hour 0, :type
;;;   :clojure.contrib.date/DateTime, :year 2009, :month 1, :day 23
;;;   :zone "America/New_York"}
;;;
;;;   user> (today)
;;;   {:type :clojure.contrib.date/Date, :year 2009, :month 1, :day
;;;   23, :zone "America/New_York"}
;;;
;;;   user> (date 2008 12 25)
;;;   {:type :clojure.contrib.date/Date, :year 2008, :month 12, :day
;;;   25, :zone "America/New_York"}
;;;
;;;   user> (date 2008 12 25 4 25 36)
;;;   {:second 36, :minute 25, :hour 4, :type
;;;   :clojure.contrib.date/DateTime, :year 2008, :month 12, :day 25
;;;   :zone "America/New_York"}
;;;
;;;   user> (format-date (now) :short)
;;;   "1/23/09 12:18 AM"
;;;
;;;   user> (format-date (today) :short)
;;;   "1/23/09"
;;;
;;;   user> (format-date (date 2008 12 25) :long)
;;;   "December 25, 2008"
;;;
;;;   user> (parse-date "Jan 1, 2009" :medium-date)
;;;   {:type :clojure.contrib.date/Date, :year 2009, :month 1, :day 1
;;;   :zone "America/New_York"}
;;;
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
      :hour Calendar/HOUR,
      :minute Calendar/MINUTE,
      :second Calendar/SECOND})

(defn- make-calendar
  "Given some date values, create a Java Calendar object with only that data."
  ([] (doto (Calendar/getInstance)
        (.clear)
        (.setLenient true)))
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

(gen-interface
 :name clojure.contrib.chrono.Instant
 :extends [clojure.lang.IFn])

(defn date
  "Creates a Date or Time object with exactly the given information."
  [& args]
  (let [jcalendar (apply make-calendar args)]
    (proxy [clojure.contrib.chrono.Instant] []
      (toString [] (str "#<ChronoDate"
                        ;; TODO: formatted stuff here
                        ">"))
      ;; look up :year, :month, :date, :weekday, etc.
      (invoke [unit]
              (cond (= :jcalendar unit) jcalendar
                    (= :month unit) (inc (get-unit jcalendar :month))
                    true (get-unit jcalendar unit))))))

;; TODO: how would this work with proxy?
;; (defn date?
;;   "Is obj a date?"
;;   [obj] (#{::Date ::DateTime} (:type obj)))

;;; Relative functions

(defn later
  "Returns a date that is later than the-date by amount units."
  ([the-date amount units]
     ;; TODO: can't clone these proxy objects. ugh. We may need
     ;; to gen-interface so we can implement more than just what IFn
     ;; provides. =(
      (doto (.clone the-date)
        (.set (units-to-calendar-units units)
              (+ (.get (units-to-calendar-units units) the-date)
                 amount))))
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
  (.after (:jcalendar date-a) (:jcalendar date-b)))

(defn earlier? [date-a date-b]
  "Is date-a earlier than date-b?"
  (.before (:jcalendar date-a) (:jcalendar date-b)))

(defn date-dispatcher [date] nil)
(defn to-calendar [date] nil)
(defn to-date [cal] nil)

(defmulti
  #^{:doc "Take in a date and a format (either a keyword or
a string) and return a string with the formatted date."}
  format-date (fn [date form] [(date-dispatcher date) form]))

(defmulti
  #^{:doc "Take in a string with a formatted date and a format
 (either a keyword or a string) and return a parsed date."}
  parse-date (fn [source form] form))

(defn- camelcase
  "Takes a string that is lowercase and dash-separated and
converts it to CamelCase."
  [string]
  (apply str
         (map (fn [x]
                (str (.toUpperCase (subs x 0 1))
                     (subs x 1)))
              (into [] (.split string "-")))))

(defn- sanitize-options
  "Turn the options passed in to def-date-format into a map"
  [options]
  (apply hash-map
         (apply concat
                (map (fn [decl]
                       (if (> (count decl) 2)
                         (cons (first decl)
                               (list (rest decl)))
                         decl))
                     options))))

(defmacro def-date-format
  "Defines a new date format for use with format-date and parse-date.
The formatter and parser can be arbitrary code. Both are optional
although it's not all that useful if neither is specified. If
the :append-type is true, the parser name is formed by joining the
format name and the format type with a dash. Otherwise, the parser
name is just the format name.

Syntax:
  (def-date-format date-format-name date-type
    (:append-type true) ;; if the format name is used for multiple types
    (:formatter [date] code-to-format-to-string)
    (:parser [string] code-to-parse-to-date))"
  [fname ftype & options]
  (let [resolved-type (keyword this-ns (camelcase (name ftype)))
        option-map (sanitize-options options)
        append? (:append-type option-map)
        format-name (keyword (if append?
                               (str (name fname) "-" (name ftype))
                               (name fname)))]
    [resolved-type option-map append? format-name]
    `(do
       ~(if-let [f (:formatter option-map)]
          `(defmethod format-date
             [~resolved-type ~(keyword (name fname))]
             [~(ffirst f) ~'_]
             ~@(rest f)))
       ~(if-let [p (:parser option-map)]
          `(defmethod parse-date
             ~format-name
             [~(ffirst p) ~'_]
             ~@(rest p))))))

(defmacro def-java-date-format
  "Defines a date format that delegates to a Java DateFormat.
The body is simply an expression that will return a DateFormat."
  [fname ftype formatter]
  `(def-date-format ~fname ~ftype
     (:append-type true)
     (:formatter [date#]
                 (.format ~formatter
                          (.getTime (to-calendar date#))))
     (:parser [source#]
              (to-date
               (doto (make-calendar)
                 (.setTime (.parse
                            ~formatter
                            source#)))))))

(def-java-date-format short date
  (DateFormat/getDateInstance DateFormat/SHORT))

(def-java-date-format medium date
  (DateFormat/getDateInstance DateFormat/MEDIUM))

(def-java-date-format long date
  (DateFormat/getDateInstance DateFormat/LONG))

(def-java-date-format full date
  (DateFormat/getDateInstance DateFormat/FULL))

(def-java-date-format short date-time
  (DateFormat/getDateTimeInstance
   DateFormat/SHORT
   DateFormat/SHORT))

(def-java-date-format medium date-time
  (DateFormat/getDateTimeInstance
   DateFormat/MEDIUM
   DateFormat/MEDIUM))

(def-java-date-format long date-time
  (DateFormat/getDateTimeInstance
   DateFormat/LONG
   DateFormat/LONG))

(def-java-date-format full date-time
  (DateFormat/getDateTimeInstance
   DateFormat/FULL
   DateFormat/FULL))

;;; Formats dates with a custom string format
(defmethod format-date :default [date form]
  (.format (SimpleDateFormat. form)
           (.getTime (to-calendar date))))

;;; Parse a date from a string format
(defmethod parse-date :default [source form]
  (to-date
   (doto (make-calendar)
     (.setTime (.parse (SimpleDateFormat. form) source)))))
