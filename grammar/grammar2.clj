;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               grammar2.clj
;;;;
;;;;   Started:            Wed Jul 11 16:30:06 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;   This is a variation on the original grammars from Peter Norvig's
;;;;   _Paradigms of Artificial Intelligence Programming_ ch. 2
;;;;   
;;;;   This version defines Rule and Grammar records to represent the components of
;;;;   a grammar and also defines a couple of macros to simplify the syntax.
;;;;

(ns grammar2)

(defn random-elt [choices]
  (nth choices (rand-int (count choices))))

(def delimiter '->)

(defrecord Rule [lhs rhs])

(defn make-rule [lhs rhs]
  (Rule. lhs rhs))

;; Examples:
;; (defrule s -> (np vp))
;; (defrule n -> man ball woman table)
(defmacro defrule [lhs delim & rhs]
  (if (= delim delimiter)
      `(make-rule '~lhs '~rhs)
      (throw (Exception. "Unrecognized rule delimiter"))))

(defrecord Grammar [rules])

(defn make-grammar [& rules]
  (Grammar. (apply hash-map (mapcat (fn [rule] [(:lhs rule) rule]) rules))))

;;
;;    This defines a grammar and associates it with the given name.
;;    
(defmacro defgrammar [name & body]
  (let [rules (map (fn [form] `(defrule ~@form)) body)]
    `(def ~name (make-grammar ~@rules))))

(defgrammar simple-grammar
  (s -> (np vp))
  (np -> (d n))
  (vp -> (v np))
  (d -> the a)
  (n -> man ball woman table)
  (v -> hit took saw liked))

(def grammar simple-grammar)

(defn rule-rewrites [grammar symbol]
  (let [rewrites (symbol (:rules grammar))]
    (if (nil? rewrites)
        rewrites
        (:rhs rewrites))))

(defn generate [grammar phrase]
  (if (list? phrase)
      (mapcat (fn [term] (generate grammar term)) phrase)
      (let [rewrites (rule-rewrites grammar phrase)]
        (if (nil? rewrites)
            (list phrase)
            (generate grammar (random-elt rewrites)))) ))

(defgrammar bigger-grammar
  (s -> (np vp))
  (np -> (d adj* n pp*) (name) (pronoun))
  (vp -> (v np pp*))
  (pp* -> () (pp pp*))
  (adj* -> () (adj adj*))
  (pp -> (p np))
  (p -> to in by with on)
  (adj -> big little blue green adiabatic)
  (d -> the a)
  (name -> pat kim lee terry rubin)
  (n -> man ball woman table)
  (v -> hit took saw liked)
  (pronoun -> he she it these those that))

;; Examples:
;; (generate simple-grammar 'np) => (a table)
;; (generate simple-grammar 's) => (a man took the ball)
;; (generate bigger-grammar 's) => (lee liked pat in those)
;; (generate bigger-grammar 's) => (a blue man took she to terry on a woman with a blue big blue woman by those)
