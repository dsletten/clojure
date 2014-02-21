;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               grammar.clj
;;;;
;;;;   Started:            Wed Jul 11 13:38:10 2012
;;;;
;;;;   Notes: See _Paradigms of Artificial Intelligence Programming_ by Peter Norvig ch. 2
;;;;
;;;;

(ns grammar)

(defn random-elt [choices]
  (nth choices (rand-int (count choices))))

(defn one-of [set]
  (list (random-elt set)))

;;;
;;;    Simple grammar
;;;
;;;    Given this rudimentary grammar, we implement it directly as a group of functions:
;;;    (Adjacent symbols are to be concatenated. Choices are separated by commas. E.g.,
;;;     A sentence s consists of a noun phrase np followed by a verb phrase vp. Whereas
;;;     a noun n may be any one of the terminal symbols.)
;;;     
;;;    s -> np vp [sentence]
;;;    np -> d n  [noun phrase]
;;;    vp -> v np [verb phrase]
;;;    d -> the, a [determiner]
;;;    n -> man, ball, woman, table [noun]
;;;    v -> hit, took, saw, liked [verb]
;;;    
(defn determiner []
  (one-of '(the a)))

(defn noun []
  (one-of '(man ball woman table)))

(defn verb []
  (one-of '(hit took saw liked)))

(defn noun-phrase []
  (concat (determiner) (noun)))

(defn verb-phrase []
   (concat (verb) (noun-phrase)))  

(defn sentence []
  (concat (noun-phrase) (verb-phrase)))

;; Examples:
;; (sentence) => (a table took a man)
;; (sentence) => (a ball took a table)
;; (sentence) => (a man saw a table)
;; (noun-phrase) => (the ball)
;; (verb-phrase) => (took a woman)

;;;
;;;    Extended grammar
;;;
;;;    Add the ability to handle adjectival and prepositional phrases of arbitrary length.
;;;    An adjectival phrase involves zero or more adjectives and likewise for prepositional phrases.
;;;    We must also redefine the noun-phrase function. In the degenerate case of 0 adjectives and 0
;;;    prepositions we have the definition of noun phrase as above. {} below represents an empty choice.
;;;
;;;    s -> np vp
;;;    np -> d adj* n pp*
;;;    vp -> v np
;;;    adj* -> {}, adj adj*
;;;    pp* -> {}, pp pp*
;;;    pp -> p np
;;;    d -> the, a
;;;    n -> man, ball, woman, table
;;;    v -> hit, took, saw, liked
;;;    adj -> big, little, blue, green, adiabatic
;;;    p -> to, in, by, with, on
;;;    
(defn adj []
  (one-of '(big little blue green adiabatic)))  

(defn prep []
  (one-of '(to in by with on)))

;;;
;;;    adj* and pp* must capture the ability to handle an arbitrary number of adjectives/prepositional phrases
;;;    yet still avoid infinite loops. Here are 3 possible ways to do that with adj*. The same techniques would
;;;    work for pp* too:
(defn adj* []
  (if (zero? (rand-int 2))
    '()
    (concat (adj) (adj*))))

(defn adj* []
  (if (random-elt '(true false))
    (concat (adj) (adj*))
    '()))

;;;
;;;    This one is a little less obvious. We randomly choose a function that either terminates or continues
;;;    recursively.
;;;    
(defn adj* []
  ((random-elt (list (fn [] '())
                     (fn [] (concat (adj) (adj*)))) )))  

;;;
;;;    Forward declaration!!!
;;;    'prepp' below only works since we've already introduced noun-phrase above.
;;;    There is a circular dependency among prepp, prepp*, and noun-phrase:
;;;    -prepp requires noun-phrase to be defined
;;;    -noun-phrase (below) requires prepp* to be defined
;;;    -prepp* requires prepp to be defined
;;;
;;;    Under other circumstances we could resolve this via 'declare':
;;;    (declare noun-phrase prepp prepp*)
;;;
(defn prepp []
  (concat (prep) (noun-phrase)))

(defn prepp* []
  (if (random-elt '(true false))
    (concat (prepp) (prepp*))
    '()))

(defn noun-phrase []
  (concat (determiner) (adj*) (noun) (prepp*)))

;; Examples:
;; (sentence) => (the blue man liked the woman)
;; (noun-phrase) => (the man on a ball)
;; (verb-phrase) => (hit a ball by a blue green table)

;;;
;;;    Below is a completely different approach to solving the problem. We define the grammar as a set of rules
;;;    and then write code to interpret those rules. (Data-driven programming)
;;;
;;;    Above we have individual function, sentence, noun-phrase, etc. for each category. Here we define a single
;;;    function 'generate' which takes the category as an argument:
;;;    (sentence) ; Above
;;;    vs.
;;;    (generate 's) ; Below

;;;
;;;    The grammar is a list of lists.
;;;    
(def simple-grammar
  '((s -> (np vp))
    (np -> (d n))
    (vp -> (v np))
    (d -> the a)
    (n -> man ball woman table)
    (v -> hit took saw liked)))

;;;
;;;    This variable represents whatever grammar we are currently using (e.g., simple-grammar or bigger-grammar)
;;;    
(def grammar simple-grammar)

;;;
;;;    Some utility functions to provide an abstraction layer over our grammar rather than simply treating it as
;;;    a list of lists.
;;;    
(defn lookup [category grammar]
  (cond (empty? grammar) nil
        (= (first (first grammar)) category) (first grammar)
        :else (lookup category (rest grammar))))

(defn rule-lhs [rule]
  (first rule))

(defn rule-rhs [rule]
  (rest (rest rule)))

(defn rewrites [category]
  (rule-rhs (lookup category grammar)))

;;;
;;;    This is not actually used. mapcat suffices.
;;;    
;; (defn mappend [f & lists]
;;   (apply concat (apply map f lists)))

;;;
;;;    This will cause problems if any terminal symbol is the same as some nonterminal, e.g., we can't use 'a' as
;;;    an abbreviation for adjective and as the indefinite article too.
;;;    

;;;
;;;    'phrase' is either a symbol or a list.
;;;    1. As a symbol, 'phrase' is either a terminal symbol or a nonterminal.
;;;       a. Return a terminal as a singleton list. It will be concatenated into the final result.
;;;       b. A nonterminal has rewrites, either a set of terminals:
;;;            (the a)
;;;          or nested lists of nonterminals:
;;;            ((v np))
;;;          In the first case, simply choose one. In the second case, recursively call 'generate' with one of the nested lists chosen at random.
;;;    2. As a list, 'phrase' comes from a recursive call to 'generate' from the rewrite rules of a nonterminal. Recursively
;;;       call 'generate' with each of the nonterminals in 'phrase' and concatenate the results.
;;;
;;;    Note: I would prefer to use 'nil?' as the test for a terminal symbol here. I expect 'rewrites' to return 'nil' for a terminal symbol since
;;;    that's what 'lookup' returns. However, for some reason 'rest' turns 'nil' into an empty list! (rest nil) => ()
;;;    So it seems like 'empty?' is the proper choice...
;;;    
(defn generate [phrase]
  (if (list? phrase)
    (mapcat generate phrase)
    (let [rew (rewrites phrase)]
;        (if (nil? rew)
      (if (empty? rew)
        (list phrase)
        (generate (random-elt rew)))) ))

;;;
;;;    Mike Liu's version. Equivalent to the above.
;;;    
(defn generate [phrase]
  (if (list? phrase)
    (concat (generate (first phrase)) (generate (rest phrase)))
    (let [rew (rewrites phrase)]
;        (if (nil? rew)
      (if (empty? rew)
        (list phrase)
        (generate (random-elt rew)))) ))


;;;
;;;    Here's a different way of framing the function. We define a predicate for terminal symbols.
;;;    
(defn terminal? [phrase]
  (empty? (rewrites phrase)))

(defn generate [phrase]
  (if (terminal? phrase)
    (list phrase)
    (let [rew (random-elt (rewrites phrase))]
;        (if (list? rew)
      (if (coll? rew)
;            (mappend generate rew)
        (mapcat generate rew)
        (list rew)))) )

;;;
;;;    One advantage of this second rule-based approach is that we can apply our code to a more complex grammar
;;;    without changing the program. This grammar handles adjectival/prepositional phrases as well as pronouns and
;;;    proper names. The same 'generate' function above works just fine. Simply set 'grammar' to this new grammar:
;;;    (def grammar bigger-grammar)
;;;
;;;    On the downside:
;;;    - The program does not observe the proper case with pronouns as objects: (generate 'np) => (a woman with he)
;;;    - Many of the sentences are obviously nonsense. The program does not consider semantics at all:
;;;      (generate 's) => (the man by those liked these with a big man by rubin by the woman to terry on lee)
;;;    
(def bigger-grammar
  '((s -> (np vp))
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
    (pronoun -> he she it these those that)))
