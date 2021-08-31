
(def start-board
  (into [] (concat
   [:r :n :b :q :k :b :n :r]
   (take 8 (repeat :p))
   (take 32 (repeat :-))
   (take 8 (repeat :P))
   [:R :N :B :Q :K :B :N :R])))

(defn print-board [board]
  (doseq [x (partition 8 board)]
    (println x)))

(defn move [board start stop]
  (assoc board
         start :-
         stop (get board start)))

(def diag [-7 -8 7 9])
(def square [-8 -1 1 8])
(def knight-moves [-17 -15 -10 -6 6 10 15 17])

(def piece-moves
  (apply hash-map (apply concat (for [[piece [lines moves captures]]
        [[:p [[] [8] [7 9]]]
         [:P [[] [-8] [-7 -9]]]
         [:r [square [] []]]
         [:R [square [] []]]
         [:n [[] knight-moves knight-moves]]
         [:N [[] knight-moves knight-moves]]
         [:b [diag [] []]]
         [:B [diag [] []]]
         [:q [(concat diag square) [] []]]
         [:Q [(concat diag square) [] []]]
         [:k [[] (concat diag square) (concat diag square)]]
         [:K [[] (concat diag square) (concat diag square)]]]]
    [piece {:lines lines :moves moves :captures captures}]))))

(defn white? [piece]
  (contains? #{:P :R :N :B :Q :K} piece))

(defn differ? [x y]
  (not= (white? x) (white? y)))

;; (def opposite {:black :white
;;                :white :black})

(defn in-bounds? [pos]
  (<= 0 pos 63))

(defn move-line
  ([board start line] (move-line board start line []))
  ([board start line acc]
   (let [piece (get board start)
         target (+ line start)]
     (if (in-bounds? target)
       (let [targetPiece (get board target)
             thisMove (move board start target)
             accAndMe (conj acc thisMove)]
         (cond
           (= targetPiece :-) (concat accAndMe
                                      (move-line thisMove target line acc))
           (differ? piece targetPiece) accAndMe
           :else acc))
       acc))))

(defn move-lines [board pos]
  (let [piece (get board pos)]
    (println "hi " (get piece-moves :Q))
    (apply concat
           (for [line (:lines (get piece-moves piece))]
             (move-line board pos line)))))

(def queen-nine (assoc (into [] (take 64 (repeat :-))) 9 :Q))

;; (print-board queen-nine)

;; (move-line queen-nine 9 9)

(doseq [board (move-line queen-nine 9 7)]
  (print-board board)
  (println))
