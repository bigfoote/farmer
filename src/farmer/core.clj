(ns farmer.core
  (:gen-class)
  (:require [clojure.set :as set]))

(def start-state {:left #{:f :c :d :g} :right #{}})

(defn search
  [state path history]
  (println "state: " state "path: " path "history: " history)
  (cond (= state {:left #{} :right #{:f :c :d :g}}) path
        (some identity (map #(= state %) history)) (do (println "already been here") false)
        (fail? state) (do (println "no good!") false)
        :else (try-moves (gen-moves state) state path history)))

(defn try-moves
  [moves state path history]
  (if (not (seq moves))
    false
    (or (search (next-state (first moves) state) (conj path (first moves)) (conj history state))
        (try-moves (rest moves) state path history))))

(defn fail?
  [state]
  (let [farmerless-group (if (:f (:right state)) (:left state) (:right state))]
    (and (:c farmerless-group) (or (:g farmerless-group) (:d farmerless-group)))))

(defn gen-moves
  [state]
  (let [farmer-group (if (:f (:right state)) (:right state) (:left state))]
    (map (fn [x] (set/union #{:f} #{x})) farmer-group)))

(defn next-state
  [move state]
  (let [farmer-side (if (:f (:right state)) :right :left)
        other-side (if (:f (:right state)) :left :right)]
    {farmer-side (apply disj (farmer-side state) move) other-side (set/union (other-side state) move)}))

(defn print-path
  ([path] path)
  ([path option]
   (doseq [p path]
     (println "Carry " (second p) " across"))))

(defn solve []
  (print-path (search start-state [] [])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
