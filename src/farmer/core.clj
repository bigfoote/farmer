(ns farmer.core
  (:gen-class)
  (:require [clojure.set :as set]))

(def start-state {:left #{:c :d :g} :right #{} :farmer-side :left})

(defn other-side [state]
  (if (= (:farmer-side state) :left) :right :left))

(defn search
  [state path history]
  (println "state: " state "path: " path "history: " history)
  (cond (= state {:left #{} :right #{:c :d :g} :farmer-side :right}) path
        (state history) (do (println "already been here") false)
        (bad? state) (do (println "no good!") false)
        :else (try-moves (gen-moves state) state path history)))

(defn try-moves
  [moves state farmer-side path history]
  (if (not (seq moves))
    false
    (or (search (next-state (first moves) state)
                (other-side state)
                (conj path (first moves))
                (conj history state))
        (try-moves (rest moves) state farmer-side path history))))

(defn bad?
  [state]
  (let [other-group (state (other-side (:farmer-side state)))]
    (and (:c other-group) (or (:g other-group) (:d other-group)))))

(defn gen-moves
  [state]
  (let [farmer-group ((:farmer-side state) state)]
    (conj (map #(set (list %)) farmer-group) #{})))

(defn next-state
  [move state]
  {(:farmer-side state) (apply disj (:farmer-side state) move)
   (other-side state) (set/union (other-side (:farmer-side state)) move)
   :farmer-side (other-side (:farmer-side state))})

(defn print-path
  ([path] path)
  ([path option]
   (doseq [p path]
     (println "Carry " (second p) " across"))))

(defn solve []
  (print-path (search start-state :left [] #{})))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
