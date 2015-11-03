;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns mellow-briars
  (:require [clojure.repl :as repl]
            [gorilla-plot.core :as plot]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [gorilla-plot.core :as plot]
            [gorilla-repl.table :as table]
            [gorilla-repl.html :as html]
            [sr-basics.render :as render]
            [darwin.core :as darwin]
            [darwin.evolution.metrics :as metrics]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn hw-random
  [functions terminals depth]
  (if (= depth 0)
    ;; note that the elements of terminals are _functions_ that generate the terminal, hence the
    ;; double brackets on the line below
    ((rand-nth terminals))
    (let [func (rand-nth functions)
          leaves (repeatedly (:arity func) #(random-full-tree functions terminals (- depth 1)))]
      (conj leaves (:name func)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;mellow-briars/hw-random</span>","value":"#'mellow-briars/hw-random"}
;; <=

;; @@
hw-crossover
;; @@

;; @@
(defn score
  [data ex]
  (let [f (functionalise ex)]
    (* -1 (apply + (map #(Math/abs (- (f (first %)) (second %))) data)))))
;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@

;; @@
(def result (darwin/evolve hw-score hw-crossover hw-mutate hw-random 5000))
;; @@
