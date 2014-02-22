;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               unary.clj
;;;;
;;;;   Started:            Thu May  9 02:03:01 2013
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Explore abstract (yet completely inefficient) representation of numbers
;;;;     as sequences of X's, e.g., 3 => (X X X). All of the basic numeric operations
;;;;     are implemented using this representation, such as addition, multiplication,
;;;;     comparison operators, etc... For convenience, the constants zero, one, and two
;;;;     are predefined.
;;;;
;;;;   Example:
;;;;   (def five (unary 5))
;;;;   (+ two five) => (X X X X X X X)
;;;;

(ns unary)
;;   (:use clojure.math.numeric-tower
;;         clojure.test
;;         [clojure.pprint :only (cl-format)]))

(def ^:const tally 'X)

(defn number [n]
  (count n))

(defn unary [n]
  (repeat n tally))

(defn zero? [n]
  (empty? n))
  
(defn inc [n]
  (cons tally n))

(defn dec [n]
  (if (zero? n)
    'error
    (rest n)))

(def ^:const zero '())
(def ^:const one (inc zero))
(def ^:const two (inc one))

(defn == [m n]
  (cond (zero? m) (zero? n)
        (zero? n) false
        :else (recur (dec m) (dec n))))
;;         :else (== (dec m) (dec n))))

(defn > [m n]
  (cond (zero? m) false
        (zero? n) true
        :else (> (dec m) (dec n))))
;;         :else (> (dec m) (dec n))))

(defn < [m n]
  (cond (zero? n) false
        (zero? m) true
        :else (recur (dec m) (dec n))))
;;         :else (< (dec m) (dec n))))

(defn + [m n]
  (if (zero? n)
    m
    (recur (inc m) (dec n))))
;;     (+ (inc m) (dec n))))

(defn - [m n]
  (cond (zero? n) m
        (zero? m) 'error
        :else (recur (dec m) (dec n)))) ; Craig Andera
;;         :else (- (dec m) (dec n))))

(defn * [m n]
  (cond (zero? n) zero
        (zero? m) zero
        :else (+ m (* m (dec n)))) )

(defn / [m n]
  (cond (zero? n) 'error
        (< m n) zero
        :else (inc (/ (- m n) n))))

(defn mod [m n]
  (cond (zero? n) 'error
        (< m n) m
        :else (recur (- m n) n)))
;;         :else (mod (- m n) n)))

(defn pos? [n]
  (> n zero))

(defn even? [n]
  (zero? (mod n two)))

(defn odd? [n]
  (not (even? n)))
