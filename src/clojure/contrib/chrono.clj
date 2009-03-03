;;; chrono.clj --- A date library that follows principle of least surprise.

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
;; (later? (later my-date 10 :day)  ;; true
;;
;;; You can see the time between two dates by calling time-between:
;;
;; (time-between my-other-date (date 2009 2 25) :days) ;; 2
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
      :hour Calendar/HOUR,
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
     calendar)
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

;; (gen-interface
;;  :name clojure.contrib.chrono.Instant
;;  :extends [clojure.lang.IFn])

(defn date
  "Returns a new date object. Takes year, month, and day as args as
  well as optionally hours, minutes, and seconds."
  [& args]
  (let [calendar (apply make-calendar args)]
    (proxy [clojure.lang.IFn] []
      (toString [] (str "#<ChronoDate "
                        ;; TODO: use prettier formatting; ugh.
                        (this :year) "-" (this :month) "-" (this :day) " "
                        (this :hour) ":" (this :minute) ":" (this :second)
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

(declare date-dispatcher)

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

;; (defmacro def-java-date-format
;;   "Defines a date format that delegates to a Java DateFormat.
;; The body is simply an expression that will return a DateFormat."
;;   [fname ftype formatter]
;;   `(def-date-format ~fname ~ftype
;;      (:append-type true)
;;      (:formatter [date#]
;;                  (.format ~formatter
;;                           (.getTime (to-calendar date#))))
;;      (:parser [source#]
;;               (to-date
;;                (doto (make-calendar)
;;                  (.setTime (.parse
;;                             ~formatter
;;                             source#)))))))

;; (def-java-date-format short date
;;   (DateFormat/getDateInstance DateFormat/SHORT))

;; (def-java-date-format medium date
;;   (DateFormat/getDateInstance DateFormat/MEDIUM))

;; (def-java-date-format long date
;;   (DateFormat/getDateInstance DateFormat/LONG))

;; (def-java-date-format full date
;;   (DateFormat/getDateInstance DateFormat/FULL))

;; (def-java-date-format short date-time
;;   (DateFormat/getDateTimeInstance
;;    DateFormat/SHORT
;;    DateFormat/SHORT))

;; (def-java-date-format medium date-time
;;   (DateFormat/getDateTimeInstance
;;    DateFormat/MEDIUM
;;    DateFormat/MEDIUM))

;; (def-java-date-format long date-time
;;   (DateFormat/getDateTimeInstance
;;    DateFormat/LONG
;;    DateFormat/LONG))

;; (def-java-date-format full date-time
;;   (DateFormat/getDateTimeInstance
;;    DateFormat/FULL
;;    DateFormat/FULL))

;; ;;; Formats dates with a custom string format
;; (defmethod format-date :default [date form]
;;   (.format (SimpleDateFormat. form)
;;            (.getTime (to-calendar date))))

;; ;;; Parse a date from a string format
;; (defmethod parse-date :default [source form]
;;   (to-date
;;    (doto (make-calendar)
;;      (.setTime (.parse (SimpleDateFormat. form) source)))))

;; Redefine subs to allow for negative indices. This should be
;; submitted as a patch to Clojure.
(in-ns 'clojure.core)
(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  ([#^String s start] (subs s start (count s)))
  ([#^String s start end]
     (let [count-back #(if (< % 0) (+ (count s) %) %)]
       (.substring s (count-back start) (count-back end)))))
