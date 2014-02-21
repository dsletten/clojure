;;;;
;;;;
;;;;   Clojure feels like a general-purpose language beamed back
;;;;     from the near future.
;;;;   -- Stu Halloway
;;;;
;;;;   Name:               tic-tac-toe.clj
;;;;
;;;;   Started:            Tue Mar 20 01:02:26 2012
;;;;   Modifications:
;;;;
;;;;   Example:
;;;;   java -cp clojure-1.4.0.jar clojure.main
;;;;   (load-file "tic-tac-toe.clj")
;;;;   (in-ns 'tic-tac-toe)
;;;;   (play-game)
;;;;
;;;;   Notes:
;;;;   -This is nearly a purely functional implementation. The only mutable state is the board, which is a Java array of arrays.
;;;;    Individual cells are modified as plays are made. But even the counter of the current round is updated via 'assoc', i.e.,
;;;;    a new game is created on each round.
;;;;   -Try representing board using vector!
;;;;
;;;;   Board positions:
;;;;
;;  0 | 1 | 2  
;; -----------
;;  3 | 4 | 5  
;; -----------
;;  6 | 7 | 8

(ns tic-tac-toe
  (:use [clojure.pprint :only (cl-format)]))

(def human-char \X)
(def computer-char \O)
(def n-rows 3)
(def n-cols 3)

(def *diagnostics* false)

(defn report [& msgs]
  (when *diagnostics*
    (if (string? (first msgs))
      (apply cl-format true (first msgs) (rest msgs))
      (cl-format true "Diagnostic:~{ ~A~}~%" msgs))))

(defrecord Game [board round rows columns])

(defn make-board []
  (let [board (make-array Character/TYPE n-rows n-cols)]
    (dotimes [i n-rows]
      (dotimes [j n-cols]
        (aset board i j \space)))
    board))

(defn make-game []
  (Game. (make-board) 0 n-rows n-cols))
;  (tic-tac-toe.Game. (make-board) 0 n-rows n-cols))

;;;
;;;    Translate between 1-dimensional and 2-dimensional view of the board.
;;;    
;;  0 | 1 | 2       0,0 | 0,1 | 0,2
;; -----------     -----------------
;;  3 | 4 | 5  vs.  1,0 | 1,1 | 1,2  
;; -----------     -----------------
;;  6 | 7 | 8       2,0 | 2,1 | 2,2
;;
;;    or
;;    
;; [0 1 2 3 4 5 6 7 8 9] vs. [0,0 0,1 0,2 1,0 1,1 1,2 2,0 2,1 2,2]
;; 
(defn row-major-index [i j]
  (+ (* i n-cols) j))

(defn board-row-index [i]
  (quot i n-cols))

(defn board-column-index [i]
  (rem i n-cols))

(defn player-char [player]
  (case player
        :human human-char
        :computer computer-char))

(defn board-contents
  ([game i] (board-contents game (board-row-index i) (board-column-index i)))
  ([game i j] (aget (:board game) i j)))

(defn print-board [game]
  (dotimes [i (:rows game)]
    (dotimes [j (:columns game)]
      (when-not (zero? j)
        (cl-format true "|"))
      (cl-format true " ~C " (board-contents game i j)))
    (cl-format true "~%")
    (when-not (== i (dec (:rows game)))
      (cl-format true "-----------~%"))))

(defn compare-position
  ([game player i] (compare-position game player (board-row-index i) (board-column-index i)))
  ([game player i j] (= (board-contents game i j) (player-char player))))

;;;
;;;    Does the given player occupy the specified position(s)?
;;;    
(defn player-occupies? [game player & positions]
  (if (empty? positions)
    false
    (every? (fn [i] (compare-position game player i)) positions)))

(defn human-occupies? [game & positions]
  (apply player-occupies? game :human positions))

(defn computer-occupies? [game & positions]
  (apply player-occupies? game :computer positions))

(defn empty-position? [game i]
  (not (or (human-occupies? game i)
           (computer-occupies? game i))))

;;;
;;;    This implements the Common Lisp sequence function POSITION.
;;;    Why is it missing from Clojure?!
;;;    
(defn position [elt seq]
  (loop [i 0
         seq seq]
    (cond (empty? seq) false
          (= elt (first seq)) i
          :else (recur (inc i) (rest seq)))) )

;;
;;    Seth's version.
;;    
(defn position [elt seq]
  (loop [i 0
         [head & tail] seq]
    (cond (= elt head) i
          (empty? tail) false
          :else (recur (inc i) tail))))
         
(defn two-out-of-three-filled? [player contents]
  (and (== (count (filter (fn [ch] (= ch (player-char player))) contents)) 2)
       (position \space contents)))

(defn three-out-of-three-filled? [player contents]
  (every? (fn [ch] (= ch (player-char player))) contents))

(defn collect-row [i game]
  (loop [j 0
         result '()]
    (if (== j (:columns game))
      (reverse result)
      (recur (inc j) (cons (board-contents game i j) result)))) )

(defn collect-column [j game]
  (loop [i 0
         result '()]
    (if (== i (:rows game))
      (reverse result)
      (recur (inc i) (cons (board-contents game i j) result)))) )

(defn collect-diagonal-1 [game]
  (loop [i 0
         result '()]
    (if (== i (:rows game))
      (reverse result)
      (recur (inc i) (cons (board-contents game i i) result)))) )

(defn collect-diagonal-2 [game]
  (loop [i 0
         result '()]
    (if (== i (:rows game))
      (reverse result)
      (recur (inc i) (cons (board-contents game (- (:rows game) i 1) i) result)))) )

(defn horizontal-win? [game player]
  (loop [i 0]
    (cond (== i (:rows game)) false
          (three-out-of-three-filled? player (collect-row i game)) player
          :else (recur (inc i)))) )

(defn vertical-win? [game player]
  (loop [j 0]
    (cond (== j (:columns game)) false
          (three-out-of-three-filled? player (collect-column j game)) player
          :else (recur (inc j)))) )

(defn diagonal-win? [game player]
  (cond (or (three-out-of-three-filled? player (collect-diagonal-1 game))
            (three-out-of-three-filled? player (collect-diagonal-2 game)))
        player
        :else false))

(defn wins? [game player]
  (or (horizontal-win? game player)
      (vertical-win? game player)
      (diagonal-win? game player)))

(defn game-over? [game]
  (cond (wins? game :human) :human
        (wins? game :computer) :computer
        (== (:round game) 4) :tie
        :else false))

(defn find-open-positions [game positions]
  (loop [result '()
         positions positions]
    (cond (empty? positions) result
          (empty-position? game (first positions)) (recur (cons (first positions) result) (rest positions))
          :else (recur result (rest positions)))) )

(defn take-position [game open-positions]
  (if (empty? open-positions)
    false
    (rand-nth open-positions)))

(defn take-center [game]
  (if (empty-position? game 4)
    4
    false))

(defn take-corner [game]
  (take-position game (find-open-positions game '(0 2 6 8))))

(defn take-edge [game]
  (take-position game (find-open-positions game '(1 3 5 7))))

(defn make-random-move [game]
  (take-position game (find-open-positions game (range 9))))

(defn find-2-in-row [game player]
  (loop [row 0]
    (if (== row (:rows game))
      false
      (let [row-hole (two-out-of-three-filled? player (collect-row row game))]
        (if row-hole
          (row-major-index row row-hole)
          (recur (inc row)))) )))

(defn find-2-in-column [game player]
  (loop [column 0]
    (if (== column (:columns game))
      false
      (let [column-hole (two-out-of-three-filled? player (collect-column column game))]
        (if column-hole
          (row-major-index column-hole column)
          (recur (inc column)))) )))

(defn find-2-in-diagonal [game player]
  (let [diagonal-1-hole (two-out-of-three-filled? player (collect-diagonal-1 game))
        diagonal-2-hole (two-out-of-three-filled? player (collect-diagonal-2 game))]
    (if diagonal-1-hole
      (row-major-index diagonal-1-hole diagonal-1-hole)
      (if diagonal-2-hole
        (row-major-index (- (:rows game) diagonal-2-hole 1) diagonal-2-hole)
        false))))
    
(defn fill-2-in-a-row [game self]
  (or (find-2-in-row game self)
      (find-2-in-column game self)
      (find-2-in-diagonal game self)))

(defn defend-2-in-a-row [game opponent]
  (or (find-2-in-row game opponent)
      (find-2-in-column game opponent)
      (find-2-in-diagonal game opponent)))

;;  O |   |   
;; -----------
;;    | X |   
;; -----------
;;    |   | X
;;
;;  Human has center and corner opposite computer, otherwise would have triggered
;;  defend-2-in-a-row.
;;  
(defn sneak-attack-diagonal [game]
  (if (human-occupies? game 4)
    (cond (human-occupies? game 0) 2
          (human-occupies? game 2) 8
          (human-occupies? game 6) 0
          (human-occupies? game 8) 6
          :else false)
    false))

;;    | X |   
;; -----------
;;  X | O |   
;; -----------
;;    |   |
;;
;; Computer opens with center when possible.
;; 
(defn sneak-attack-double-edge [game]
  (cond (human-occupies? game 1 5) 2
        (human-occupies? game 1 3) 0
        (human-occupies? game 7 5) 8
        (human-occupies? game 7 3) 6
        :else false))

;;  X |   |   
;; -----------
;;    | O |   
;; -----------
;;    | X |   
;;
;; Computer opens with center when possible.
;; 
(defn sneak-attack-l [game]
  (cond (human-occupies? game 0 7) 6
        (human-occupies? game 2 7) 8
        (human-occupies? game 0 5) 2
        (human-occupies? game 6 5) 8
        (human-occupies? game 6 1) 0
        (human-occupies? game 8 1) 2
        (human-occupies? game 2 3) 0
        (human-occupies? game 8 3) 6
        :else false))

;;  X |   |   
;; -----------
;;    | O |   
;; -----------
;;    |   | X
;;
;; Computer opens with center when possible.
;; 
(defn sneak-attack-opposite-corners [game]
  (if (or (human-occupies? game 0 8)
          (human-occupies? game 6 2))
      (take-edge game)
      false))

(defn detect-sneak-attack [game]
  (if (== (:round game) 1)
    (or (sneak-attack-diagonal game)
        (sneak-attack-double-edge game)
        (sneak-attack-l game)
        (sneak-attack-opposite-corners game))
    false))

(defn get-computer-move [game]
  (or (fill-2-in-a-row game :computer)
      (defend-2-in-a-row game :human)
      (detect-sneak-attack game)
      (take-center game)
      (take-corner game)
      (make-random-move game)))

(defn valid-num?
  ([obj] (number? obj))
  ([obj test-fn]
     (if (number? obj)
       (if test-fn
         (test-fn obj)
         true)
      false)))

(defn prompt-read [prompt]
  (cl-format true "~A" prompt)
  (flush)
;  (.readLine *in*))
  (read-line))

(defn get-num
  ([prompt] (get-num prompt nil))
  ([prompt test-fn]
     (try
       (let [num (read-string (prompt-read prompt))]
         (if (valid-num? num test-fn)
           num
           (get-num prompt test-fn)))
       (catch Exception e
         (get-num prompt test-fn)))) )

(defn get-human-move [game]
  (get-num "Enter move: " (fn [n] (and (integer? n)
                                       (<= 0 n 8)
                                       (empty-position? game n)))) )

(defn make-move
  ([game player i]
     (if (empty-position? game i)
       (make-move game player (board-row-index i) (board-column-index i))
       (throw (Exception. (cl-format nil "~A tried to play ~D." player i)))) )
  ([game player i j]
     (aset (:board game) i j (player-char player)))) ; No error checking here?!

(defn play-move [game player move]
  (make-move game player move)
  (let [game-over (game-over? game)]
    (when (or game-over (= player :computer))
      (print-board game))
    game-over))

(defn play-game []
  (loop [game (make-game)]
    (or (play-move game :human (get-human-move game))
        (play-move game :computer (get-computer-move game))
        (recur (assoc game :round (inc (:round game)))) )))
