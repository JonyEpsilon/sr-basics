;; gorilla-repl.fileformat = 1

;; **
;;; # Joel Darwin 
;;; 
;;;           
;; **

;; **
;;; **Required inputs for the Darwin System**
;;; - a score function which measures how well a candidate individual is doing at meeting the goal;
;;; - a function to create a random individual;
;;; - a function to mutate an individual;
;;; - a function to breed two individuals to make two more.
;; **

;; @@
(ns joel-darwin
  (:require [clojure.repl :as repl]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [gorilla-plot.core :as plot]
            [gorilla-repl.table :as table]
            [gorilla-repl.html :as html]
            [sr-basics.render :as render]
    		[darwin.core :as darwin]
            [darwin.evolution.metrics :as metrics]
            [algebolic.expression.core :as expression]
            [algebolic.expression.tree :as tree]
            [algebolic.expression.genetics :as genetics]
            [algebolic.expression.score :as score]
            [algebolic.expression.render :as spea-render]
            [algebolic.expression.interpreter :as interpreter]
            [darwin.evolution.core :as evolution]
            [darwin.evolution.metrics :as metrics]
            [darwin.evolution.reproduction :as reproduction]
            [darwin.evolution.scoring :as scoring]
            [darwin.evolution.selection :as selection]
            [darwin.evolution.transform :as transform]
            [darwin.evolution.pareto :as pareto]
            [darwin.algorithms.spea2 :as spea2]
            [criterium.core :as criterium]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Sybolic Regression - Imports
;;; 
;;; Imports from the Introduction to Sybolic reression worksheet.
;; **

;; **
;;; Modify the division operator to sidestep diving by zero.
;; **

;; @@
(defn pdiv [x y] (if (zero? y) 1 (/ x y)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/pdiv</span>","value":"#'joel-darwin/pdiv"}
;; <=

;; **
;;; List fucntions (as maps) and define thier arity (arguements taken). Simplified to the fact that each operatory has a fixed arity.
;; **

;; @@
(def functions
  [{:name '+ :arity 2}
   {:name '- :arity 2}
   {:name '* :arity 2}
   {:name 'pdiv :arity 2}])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/functions</span>","value":"#'joel-darwin/functions"}
;; <=

;; **
;;; Generate terminals which act as the arguements which functions opeator on. Define terminals as constructor functions that when evaluated return the value of the terminal. Implemented here to be a rnadom number between 0-1  rand and varaible 'x'.
;; **

;; @@
(def terminals
  [(constantly 'x)
   rand])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/terminals</span>","value":"#'joel-darwin/terminals"}
;; <=

;; **
;;; ## Initial population generator
;;; 
;;; Our solutions represented as trees, in usual LISP s-expr way. To generate an initial, random population of trees we'll start with a function to generate a random tree of a given depth. This function generates a particular kind of tree, one where all of the terminal nodes are at the maximum depth. There are other ways you can think of to generate random trees, but this one will suffice for now.
;;; 
;;; We use a simple recursive implementation.
;; **

;; @@
(defn random-full-tree
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/random-full-tree</span>","value":"#'joel-darwin/random-full-tree"}
;; <=

;; **
;;; 
;; **

;; @@
(defn functionalise [ex] (eval (list 'fn '[x] ex)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/functionalise</span>","value":"#'joel-darwin/functionalise"}
;; <=

;; **
;;; In the accompanying project to this worksheet I've defined a function `mathematician-view` which formats the expression in a more traditional format. Let's look at a sampling of random expressions and their graphs.
;; **

;; **
;;; Making an innital population, with n nodes and a max depth.
;; **

;; @@
(defn make-initial-population
  [n max-depth]
  (repeatedly n #(random-full-tree functions terminals (+ 1 (rand-int (- max-depth 1))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/make-initial-population</span>","value":"#'joel-darwin/make-initial-population"}
;; <=

;; **
;;; ## Scoring solutions
;; **

;; **
;;; Modified @@\chi^2@@ type score to indicate how well each proposed solution fits. Rather than squaring the residuals, as in a @@\chi^2@@ score, that the absolute values:
;;; 
;;; $$ S(f) = - \sum_{i} |f(x_i) - y_i| $$
;;; 
;;; @@i@@ runs over the data points, @@f@@ is the function under test and @@(x_i, y_i)@@ is the @@i@@th data point. Notice that we've made the score negative, so the maximum score is the best. This form of the scoring function is commonly used, and less aggressive for functions that are way off the mark. I've never tested whether it really makes any difference to the convergence of the genetic algorithm search though.
;;; 
;;; - *Darwin*
;;; 
;;; ***For the Darwin run I invert this negative convention as the darwin evolve function is expected to be passed a score function that yields positive scores with the attempt of minimisng the score to 0***
;; **

;; @@
(defn score
  [data ex]
  (let [f (functionalise ex)]
    (* -1 (apply + (map #(Math/abs (- (f (first %)) (second %))) data)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/score</span>","value":"#'joel-darwin/score"}
;; <=

;; **
;;; ** Target Data Set **
;; **

;; @@
(def pi 3.14)   ;Crude attempt at pi,   (couldn't find the clojure pi call)
Math/pi
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/pi</span>","value":"#'joel-darwin/pi"}
;; <=

;; @@
(def data (doall (map (fn [x] [x (Math/sin x)]) (range 0 (* 3 pi) 0.5))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/data</span>","value":"#'joel-darwin/data"}
;; <=

;; **
;;; **Parsimony Pressure score**
;; **

;; @@
(defn count-nodes
  [ex]
  (if (seq? ex)
  	(+ 1 (apply + (map count-nodes (rest ex))))
    1))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/count-nodes</span>","value":"#'joel-darwin/count-nodes"}
;; <=

;; @@
(defn score-pp
  [data ex pressure-coeff]
  (+ (score data ex) (* pressure-coeff (count-nodes ex))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/score-pp</span>","value":"#'joel-darwin/score-pp"}
;; <=

;; **
;;; ## Genetic operations
;;; 
;;;  `clojure.zip`
;;;  
;;;  library to do the tree manipulation. We define a zipper-constructor for expressions, that will treat the expression in the way we'd expect (namely, that branch nodes are functions). You might need to revise the `clojure.zip` documentation to make sense of this function.
;; **

;; @@
(defn expr-zip
  [expr]
  (zip/zipper
    (constantly true)
    (fn [node]
      (if (seq? node) 
                 (rest node) 
                 nil))
    (fn [node children] (with-meta (conj children (first node)) (meta node)))
    expr))
 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/expr-zip</span>","value":"#'joel-darwin/expr-zip"}
;; <=

;; **
;;; `tree-replace` 
;;; 
;;; function, replaces a part of the tree, specified by its depth-first index, with another given tree. We simply walk the tree to the target index, and then replace the sub-tree with the new tree.
;; **

;; @@
(defn tree-replace
  [tree index new-tree]
  (let [subtree-z (nth (iterate zip/next (expr-zip tree)) index)
        new-zipper (zip/replace subtree-z new-tree)]
    (zip/root new-zipper)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/tree-replace</span>","value":"#'joel-darwin/tree-replace"}
;; <=

;; **
;;; $$ \textbf{MUTATION} $$
;; **

;; @@
(defn mutate-expr
  [expr new-tree-func]
  (let [size (count-nodes expr)
        target (rand-int size)]
    (tree-replace expr target (new-tree-func))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/mutate-expr</span>","value":"#'joel-darwin/mutate-expr"}
;; <=

;; **
;;; $$ \textbf{CROSSOVER} $$
;; **

;; **
;;; Takes two expressions, and returns two new expressions, constructed by picking a random subtree from both expressions and swapping them. 
;;; - helper, which gets the sub-tree for a given depth first index.
;; **

;; @@
(defn sub-tree
  [tree index]
  (zip/node (nth (iterate zip/next (expr-zip tree)) index)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/sub-tree</span>","value":"#'joel-darwin/sub-tree"}
;; <=

;; @@
(defn crossover-expr
  [expr1 expr2]
  (let [size1 (count-nodes expr1)
        target1 (rand-int size1)
        size2 (count-nodes expr2)
        target2 (rand-int size2)
        subtree1 (sub-tree expr1 target1)
        subtree2 (sub-tree expr2 target2)]
    [(tree-replace expr1 target1 subtree2) (tree-replace expr2 target2 subtree1)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/crossover-expr</span>","value":"#'joel-darwin/crossover-expr"}
;; <=

;; **
;;; ## The genetic algorithm
;;; 
;;; Implement the genetic algorithm itself.
;;; 
;;; - Take a population of expressions, 
;;; - score them, 
;;; - build a new population by cloning, mutating and breeding the better expressions from the old population.
;;; 
;;; Implement a **tournament** selection. generate each expression in the new population by picking a few expressions from the old population, and then select the best one and copy it, mutate it, or breed it. 
;;; 
;;; To avoid wasting time scoring the same individuals over and over again, we'll first score every expression in the population, to produce a 'scored population' map.
;; **

;; @@
(defn score-population
  [population score-func]
  (map (fn [expr] {:expr expr :score (score-func expr)}) population))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/score-population</span>","value":"#'joel-darwin/score-population"}
;; <=

;; **
;;; *Tournament selector*, which picks an expression from a scored population.
;; **

;; @@
(defn tournament-selector
  [scored-popn tournament-size]
  (let [competitors (repeatedly tournament-size #(rand-nth scored-popn))]
    (:expr (apply max-key :score competitors))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/tournament-selector</span>","value":"#'joel-darwin/tournament-selector"}
;; <=

;; **
;;; Function which takes a population, and creates the next generation. 
;;; We configure this with three integers:
;;; - the number of clones to make, 
;;; - the number of mutations to make, 
;;; - the number of crossover pairs to make. 
;;; 
;;; Stable population --> `clone-n`, `mutate-n`, and twice `crossover-n` add up to the initial size.
;; **

;; @@
(defn evolve
  [population config]
  (let [{:keys [score-func mutation-new-tree-func tournament-size clone-n mutate-n crossover-n]} config
        scored-popn (score-population population score-func)
        clones (repeatedly clone-n #(tournament-selector scored-popn tournament-size))
        mutations (repeatedly mutate-n 
                              #(mutate-expr 
                                (tournament-selector scored-popn tournament-size)
                                mutation-new-tree-func))
        crossovers (reduce into (repeatedly crossover-n
                               #(crossover-expr
                                 (tournament-selector scored-popn tournament-size)
                                 (tournament-selector scored-popn tournament-size))))]
    (into clones (into mutations crossovers))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/evolve</span>","value":"#'joel-darwin/evolve"}
;; <=

;; **
;;; Configure the population
;; **

;; @@
(def config {:score-func (memoize #(score-pp data % -0.1))
             :mutation-new-tree-func #(random-full-tree functions terminals 3)
             :tournament-size 6
             :clone-n 20
             :crossover-n 35
             :mutate-n 60})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/config</span>","value":"#'joel-darwin/config"}
;; <=

;; **
;;; Create an initial population.
;; **

;; @@
(def init-pop (repeatedly 150 #(random-full-tree functions terminals 3))) 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/init-pop</span>","value":"#'joel-darwin/init-pop"}
;; <=

;; **
;;; And evolve for a number of generations - for simplicity we just run for a fixed number of generations and then stop. We generate a list of the full population at each generation.
;; **

;; @@
(time (def run-data (doall (take 150 (iterate #(evolve % config) init-pop)))))
;; @@
;; ->
;;; &quot;Elapsed time: 122311.316461 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/run-data</span>","value":"#'joel-darwin/run-data"}
;; <=

;; **
;;; Look at the best individual in each generation.
;; **

;; @@
(defn best-in-generation
  [pop score-func]
  (apply max-key :score (score-population pop score-func)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/best-in-generation</span>","value":"#'joel-darwin/best-in-generation"}
;; <=

;; @@
(def bests (doall (map #(best-in-generation % (:score-func config)) run-data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/bests</span>","value":"#'joel-darwin/bests"}
;; <=

;; **
;;; It's interesting to look at how the fitness of the best individual improved through the generations. Here we see that it improves very rapidly for this simple target function.
;; **

;; @@
(plot/list-plot (map :score bests) :joined true)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3","values":[{"x":0,"y":-12.026843407984208},{"x":1,"y":-11.027994061149435},{"x":2,"y":-11.018377169724818},{"x":3,"y":-11.018377169724818},{"x":4,"y":-10.998605768021866},{"x":5,"y":-10.998605768021866},{"x":6,"y":-10.998605768021866},{"x":7,"y":-10.998605768021866},{"x":8,"y":-10.998605768021866},{"x":9,"y":-10.998605768021866},{"x":10,"y":-10.998605768021866},{"x":11,"y":-10.998605768021866},{"x":12,"y":-10.998605768021866},{"x":13,"y":-10.998605768021866},{"x":14,"y":-10.998605768021866},{"x":15,"y":-10.998605768021866},{"x":16,"y":-10.998605768021866},{"x":17,"y":-10.998605768021866},{"x":18,"y":-10.998605768021866},{"x":19,"y":-10.998605768021866},{"x":20,"y":-10.998605768021866},{"x":21,"y":-10.998605768021866},{"x":22,"y":-10.998605768021866},{"x":23,"y":-10.998605768021866},{"x":24,"y":-10.998605768021866},{"x":25,"y":-10.998605768021866},{"x":26,"y":-10.998605768021866},{"x":27,"y":-10.998605768021866},{"x":28,"y":-10.998605768021866},{"x":29,"y":-10.998605768021866},{"x":30,"y":-10.998605768021866},{"x":31,"y":-10.998605768021866},{"x":32,"y":-10.998605768021866},{"x":33,"y":-10.998605768021866},{"x":34,"y":-10.998605768021866},{"x":35,"y":-10.998605768021866},{"x":36,"y":-10.998605768021866},{"x":37,"y":-10.998605768021866},{"x":38,"y":-10.998605768021866},{"x":39,"y":-10.998605768021866},{"x":40,"y":-10.998605768021866},{"x":41,"y":-10.998605768021866},{"x":42,"y":-10.998605768021866},{"x":43,"y":-10.998605768021866},{"x":44,"y":-10.998605768021866},{"x":45,"y":-10.998605768021866},{"x":46,"y":-10.998605768021866},{"x":47,"y":-10.998605768021866},{"x":48,"y":-10.998605768021866},{"x":49,"y":-10.998605768021866},{"x":50,"y":-10.998605768021866},{"x":51,"y":-10.998605768021866},{"x":52,"y":-10.998605768021866},{"x":53,"y":-10.998605768021866},{"x":54,"y":-10.998605768021866},{"x":55,"y":-10.998605768021866},{"x":56,"y":-10.998605768021866},{"x":57,"y":-10.998605768021866},{"x":58,"y":-10.998605768021866},{"x":59,"y":-10.998605768021866},{"x":60,"y":-10.998605768021866},{"x":61,"y":-10.998605768021866},{"x":62,"y":-10.998605768021866},{"x":63,"y":-10.998605768021866},{"x":64,"y":-10.998605768021866},{"x":65,"y":-10.998605768021866},{"x":66,"y":-10.998605768021866},{"x":67,"y":-10.998605768021866},{"x":68,"y":-10.998605768021866},{"x":69,"y":-10.998605768021866},{"x":70,"y":-10.998605768021866},{"x":71,"y":-10.998605768021866},{"x":72,"y":-10.998605768021866},{"x":73,"y":-10.998605768021866},{"x":74,"y":-10.998605768021866},{"x":75,"y":-10.998605768021866},{"x":76,"y":-10.998605768021866},{"x":77,"y":-10.998605768021866},{"x":78,"y":-10.998605768021866},{"x":79,"y":-10.998605768021866},{"x":80,"y":-10.998605768021866},{"x":81,"y":-10.998605768021866},{"x":82,"y":-10.998605768021866},{"x":83,"y":-10.998605768021866},{"x":84,"y":-10.998605768021866},{"x":85,"y":-10.998605768021866},{"x":86,"y":-10.998605768021866},{"x":87,"y":-10.998605768021866},{"x":88,"y":-10.998605768021866},{"x":89,"y":-10.998605768021866},{"x":90,"y":-10.998605768021866},{"x":91,"y":-10.998605768021866},{"x":92,"y":-10.998605768021866},{"x":93,"y":-10.998605768021866},{"x":94,"y":-10.998605768021866},{"x":95,"y":-10.998605768021866},{"x":96,"y":-10.998605768021866},{"x":97,"y":-10.998605768021866},{"x":98,"y":-10.998605768021866},{"x":99,"y":-10.998605768021866},{"x":100,"y":-10.998605768021866},{"x":101,"y":-10.998605768021866},{"x":102,"y":-10.998605768021866},{"x":103,"y":-10.998605768021866},{"x":104,"y":-10.998605768021866},{"x":105,"y":-10.998605768021866},{"x":106,"y":-10.998605768021866},{"x":107,"y":-10.998605768021866},{"x":108,"y":-10.998605768021866},{"x":109,"y":-10.998605768021866},{"x":110,"y":-10.998605768021866},{"x":111,"y":-10.998605768021866},{"x":112,"y":-10.998605768021866},{"x":113,"y":-10.998605768021866},{"x":114,"y":-10.998605768021866},{"x":115,"y":-10.998605768021866},{"x":116,"y":-10.998605768021866},{"x":117,"y":-10.998605768021866},{"x":118,"y":-10.998605768021866},{"x":119,"y":-10.998605768021866},{"x":120,"y":-10.998605768021866},{"x":121,"y":-10.998605768021866},{"x":122,"y":-10.998605768021866},{"x":123,"y":-10.998605768021866},{"x":124,"y":-10.998605768021866},{"x":125,"y":-10.998605768021866},{"x":126,"y":-10.998605768021866},{"x":127,"y":-10.98887559699967},{"x":128,"y":-10.998605768021866},{"x":129,"y":-10.998605768021866},{"x":130,"y":-10.998605768021866},{"x":131,"y":-10.998605768021866},{"x":132,"y":-10.998605768021866},{"x":133,"y":-10.998605768021866},{"x":134,"y":-10.998605768021866},{"x":135,"y":-10.998605768021866},{"x":136,"y":-10.998605768021866},{"x":137,"y":-10.998605768021866},{"x":138,"y":-10.998605768021866},{"x":139,"y":-10.998605768021866},{"x":140,"y":-10.998605768021866},{"x":141,"y":-10.998605768021866},{"x":142,"y":-10.998605768021866},{"x":143,"y":-10.998605768021866},{"x":144,"y":-10.998605768021866},{"x":145,"y":-10.998605768021866},{"x":146,"y":-10.998605768021866},{"x":147,"y":-10.998605768021866},{"x":148,"y":-10.998605768021866},{"x":149,"y":-10.998605768021866}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"df6ce58b-b7ff-4a1c-a4ff-0e97c37cb0b3\", :values ({:x 0, :y -12.026843407984208} {:x 1, :y -11.027994061149435} {:x 2, :y -11.018377169724818} {:x 3, :y -11.018377169724818} {:x 4, :y -10.998605768021866} {:x 5, :y -10.998605768021866} {:x 6, :y -10.998605768021866} {:x 7, :y -10.998605768021866} {:x 8, :y -10.998605768021866} {:x 9, :y -10.998605768021866} {:x 10, :y -10.998605768021866} {:x 11, :y -10.998605768021866} {:x 12, :y -10.998605768021866} {:x 13, :y -10.998605768021866} {:x 14, :y -10.998605768021866} {:x 15, :y -10.998605768021866} {:x 16, :y -10.998605768021866} {:x 17, :y -10.998605768021866} {:x 18, :y -10.998605768021866} {:x 19, :y -10.998605768021866} {:x 20, :y -10.998605768021866} {:x 21, :y -10.998605768021866} {:x 22, :y -10.998605768021866} {:x 23, :y -10.998605768021866} {:x 24, :y -10.998605768021866} {:x 25, :y -10.998605768021866} {:x 26, :y -10.998605768021866} {:x 27, :y -10.998605768021866} {:x 28, :y -10.998605768021866} {:x 29, :y -10.998605768021866} {:x 30, :y -10.998605768021866} {:x 31, :y -10.998605768021866} {:x 32, :y -10.998605768021866} {:x 33, :y -10.998605768021866} {:x 34, :y -10.998605768021866} {:x 35, :y -10.998605768021866} {:x 36, :y -10.998605768021866} {:x 37, :y -10.998605768021866} {:x 38, :y -10.998605768021866} {:x 39, :y -10.998605768021866} {:x 40, :y -10.998605768021866} {:x 41, :y -10.998605768021866} {:x 42, :y -10.998605768021866} {:x 43, :y -10.998605768021866} {:x 44, :y -10.998605768021866} {:x 45, :y -10.998605768021866} {:x 46, :y -10.998605768021866} {:x 47, :y -10.998605768021866} {:x 48, :y -10.998605768021866} {:x 49, :y -10.998605768021866} {:x 50, :y -10.998605768021866} {:x 51, :y -10.998605768021866} {:x 52, :y -10.998605768021866} {:x 53, :y -10.998605768021866} {:x 54, :y -10.998605768021866} {:x 55, :y -10.998605768021866} {:x 56, :y -10.998605768021866} {:x 57, :y -10.998605768021866} {:x 58, :y -10.998605768021866} {:x 59, :y -10.998605768021866} {:x 60, :y -10.998605768021866} {:x 61, :y -10.998605768021866} {:x 62, :y -10.998605768021866} {:x 63, :y -10.998605768021866} {:x 64, :y -10.998605768021866} {:x 65, :y -10.998605768021866} {:x 66, :y -10.998605768021866} {:x 67, :y -10.998605768021866} {:x 68, :y -10.998605768021866} {:x 69, :y -10.998605768021866} {:x 70, :y -10.998605768021866} {:x 71, :y -10.998605768021866} {:x 72, :y -10.998605768021866} {:x 73, :y -10.998605768021866} {:x 74, :y -10.998605768021866} {:x 75, :y -10.998605768021866} {:x 76, :y -10.998605768021866} {:x 77, :y -10.998605768021866} {:x 78, :y -10.998605768021866} {:x 79, :y -10.998605768021866} {:x 80, :y -10.998605768021866} {:x 81, :y -10.998605768021866} {:x 82, :y -10.998605768021866} {:x 83, :y -10.998605768021866} {:x 84, :y -10.998605768021866} {:x 85, :y -10.998605768021866} {:x 86, :y -10.998605768021866} {:x 87, :y -10.998605768021866} {:x 88, :y -10.998605768021866} {:x 89, :y -10.998605768021866} {:x 90, :y -10.998605768021866} {:x 91, :y -10.998605768021866} {:x 92, :y -10.998605768021866} {:x 93, :y -10.998605768021866} {:x 94, :y -10.998605768021866} {:x 95, :y -10.998605768021866} {:x 96, :y -10.998605768021866} {:x 97, :y -10.998605768021866} {:x 98, :y -10.998605768021866} {:x 99, :y -10.998605768021866} {:x 100, :y -10.998605768021866} {:x 101, :y -10.998605768021866} {:x 102, :y -10.998605768021866} {:x 103, :y -10.998605768021866} {:x 104, :y -10.998605768021866} {:x 105, :y -10.998605768021866} {:x 106, :y -10.998605768021866} {:x 107, :y -10.998605768021866} {:x 108, :y -10.998605768021866} {:x 109, :y -10.998605768021866} {:x 110, :y -10.998605768021866} {:x 111, :y -10.998605768021866} {:x 112, :y -10.998605768021866} {:x 113, :y -10.998605768021866} {:x 114, :y -10.998605768021866} {:x 115, :y -10.998605768021866} {:x 116, :y -10.998605768021866} {:x 117, :y -10.998605768021866} {:x 118, :y -10.998605768021866} {:x 119, :y -10.998605768021866} {:x 120, :y -10.998605768021866} {:x 121, :y -10.998605768021866} {:x 122, :y -10.998605768021866} {:x 123, :y -10.998605768021866} {:x 124, :y -10.998605768021866} {:x 125, :y -10.998605768021866} {:x 126, :y -10.998605768021866} {:x 127, :y -10.98887559699967} {:x 128, :y -10.998605768021866} {:x 129, :y -10.998605768021866} {:x 130, :y -10.998605768021866} {:x 131, :y -10.998605768021866} {:x 132, :y -10.998605768021866} {:x 133, :y -10.998605768021866} {:x 134, :y -10.998605768021866} {:x 135, :y -10.998605768021866} {:x 136, :y -10.998605768021866} {:x 137, :y -10.998605768021866} {:x 138, :y -10.998605768021866} {:x 139, :y -10.998605768021866} {:x 140, :y -10.998605768021866} {:x 141, :y -10.998605768021866} {:x 142, :y -10.998605768021866} {:x 143, :y -10.998605768021866} {:x 144, :y -10.998605768021866} {:x 145, :y -10.998605768021866} {:x 146, :y -10.998605768021866} {:x 147, :y -10.998605768021866} {:x 148, :y -10.998605768021866} {:x 149, :y -10.998605768021866})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; And how does the best individual of the last generation look?
;; **

;; @@
(plot/compose
 (plot/list-plot data)
 (plot/plot (functionalise (:expr (last bests))) [0 (* 3 pi)]))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"a5634990-b956-4438-a6a7-5f0c8e4ed7bd","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"a5634990-b956-4438-a6a7-5f0c8e4ed7bd","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"a5634990-b956-4438-a6a7-5f0c8e4ed7bd","values":[{"x":0,"y":0.0},{"x":0.5,"y":0.479425538604203},{"x":1.0,"y":0.8414709848078965},{"x":1.5,"y":0.9974949866040544},{"x":2.0,"y":0.9092974268256817},{"x":2.5,"y":0.5984721441039564},{"x":3.0,"y":0.1411200080598672},{"x":3.5,"y":-0.35078322768961984},{"x":4.0,"y":-0.7568024953079282},{"x":4.5,"y":-0.977530117665097},{"x":5.0,"y":-0.9589242746631385},{"x":5.5,"y":-0.7055403255703919},{"x":6.0,"y":-0.27941549819892586},{"x":6.5,"y":0.21511998808781552},{"x":7.0,"y":0.6569865987187891},{"x":7.5,"y":0.9379999767747389},{"x":8.0,"y":0.9893582466233818},{"x":8.5,"y":0.7984871126234903},{"x":9.0,"y":0.4121184852417566}]},{"name":"21d41f63-6e19-4fbe-9526-db284b7a5bc7","values":[{"x":0,"y":0.4289752946300115},{"x":0.094200000166893,"y":0.4289752946300115},{"x":0.188400000333786,"y":0.4289752946300115},{"x":0.282600000500679,"y":0.4289752946300115},{"x":0.376800000667572,"y":0.4289752946300115},{"x":0.471000000834465,"y":0.4289752946300115},{"x":0.565200001001358,"y":0.4289752946300115},{"x":0.659400001168251,"y":0.4289752946300115},{"x":0.753600001335144,"y":0.4289752946300115},{"x":0.847800001502037,"y":0.4289752946300115},{"x":0.94200000166893,"y":0.4289752946300115},{"x":1.036200001835823,"y":0.4289752946300115},{"x":1.130400002002716,"y":0.4289752946300115},{"x":1.224600002169609,"y":0.4289752946300115},{"x":1.318800002336502,"y":0.4289752946300115},{"x":1.413000002503395,"y":0.4289752946300115},{"x":1.507200002670288,"y":0.4289752946300115},{"x":1.601400002837181,"y":0.4289752946300115},{"x":1.695600003004074,"y":0.4289752946300115},{"x":1.789800003170967,"y":0.4289752946300115},{"x":1.88400000333786,"y":0.4289752946300115},{"x":1.9782000035047531,"y":0.4289752946300115},{"x":2.072400003671646,"y":0.4289752946300115},{"x":2.166600003838539,"y":0.4289752946300115},{"x":2.260800004005432,"y":0.4289752946300115},{"x":2.355000004172325,"y":0.4289752946300115},{"x":2.449200004339218,"y":0.4289752946300115},{"x":2.543400004506111,"y":0.4289752946300115},{"x":2.637600004673004,"y":0.4289752946300115},{"x":2.731800004839897,"y":0.4289752946300115},{"x":2.82600000500679,"y":0.4289752946300115},{"x":2.920200005173683,"y":0.4289752946300115},{"x":3.014400005340576,"y":0.4289752946300115},{"x":3.108600005507469,"y":0.4289752946300115},{"x":3.202800005674362,"y":0.4289752946300115},{"x":3.297000005841255,"y":0.4289752946300115},{"x":3.391200006008148,"y":0.4289752946300115},{"x":3.485400006175041,"y":0.4289752946300115},{"x":3.579600006341934,"y":0.4289752946300115},{"x":3.673800006508827,"y":0.4289752946300115},{"x":3.76800000667572,"y":0.4289752946300115},{"x":3.862200006842613,"y":0.4289752946300115},{"x":3.9564000070095062,"y":0.4289752946300115},{"x":4.050600007176399,"y":0.4289752946300115},{"x":4.144800007343292,"y":0.4289752946300115},{"x":4.239000007510185,"y":0.4289752946300115},{"x":4.333200007677078,"y":0.4289752946300115},{"x":4.427400007843971,"y":0.4289752946300115},{"x":4.521600008010864,"y":0.4289752946300115},{"x":4.615800008177757,"y":0.4289752946300115},{"x":4.71000000834465,"y":0.4289752946300115},{"x":4.804200008511543,"y":0.4289752946300115},{"x":4.898400008678436,"y":0.4289752946300115},{"x":4.992600008845329,"y":0.4289752946300115},{"x":5.086800009012222,"y":0.4289752946300115},{"x":5.181000009179115,"y":0.4289752946300115},{"x":5.275200009346008,"y":0.4289752946300115},{"x":5.369400009512901,"y":0.4289752946300115},{"x":5.463600009679794,"y":0.4289752946300115},{"x":5.557800009846687,"y":0.4289752946300115},{"x":5.65200001001358,"y":0.4289752946300115},{"x":5.746200010180473,"y":0.4289752946300115},{"x":5.840400010347366,"y":0.4289752946300115},{"x":5.934600010514259,"y":0.4289752946300115},{"x":6.028800010681152,"y":0.4289752946300115},{"x":6.123000010848045,"y":0.4289752946300115},{"x":6.217200011014938,"y":0.4289752946300115},{"x":6.311400011181831,"y":0.4289752946300115},{"x":6.405600011348724,"y":0.4289752946300115},{"x":6.499800011515617,"y":0.4289752946300115},{"x":6.59400001168251,"y":0.4289752946300115},{"x":6.688200011849403,"y":0.4289752946300115},{"x":6.782400012016296,"y":0.4289752946300115},{"x":6.876600012183189,"y":0.4289752946300115},{"x":6.970800012350082,"y":0.4289752946300115},{"x":7.065000012516975,"y":0.4289752946300115},{"x":7.159200012683868,"y":0.4289752946300115},{"x":7.253400012850761,"y":0.4289752946300115},{"x":7.347600013017654,"y":0.4289752946300115},{"x":7.441800013184547,"y":0.4289752946300115},{"x":7.53600001335144,"y":0.4289752946300115},{"x":7.630200013518333,"y":0.4289752946300115},{"x":7.724400013685226,"y":0.4289752946300115},{"x":7.8186000138521194,"y":0.4289752946300115},{"x":7.9128000140190125,"y":0.4289752946300115},{"x":8.007000014185905,"y":0.4289752946300115},{"x":8.101200014352798,"y":0.4289752946300115},{"x":8.195400014519691,"y":0.4289752946300115},{"x":8.289600014686584,"y":0.4289752946300115},{"x":8.383800014853477,"y":0.4289752946300115},{"x":8.47800001502037,"y":0.4289752946300115},{"x":8.572200015187263,"y":0.4289752946300115},{"x":8.666400015354156,"y":0.4289752946300115},{"x":8.76060001552105,"y":0.4289752946300115},{"x":8.854800015687943,"y":0.4289752946300115},{"x":8.949000015854836,"y":0.4289752946300115},{"x":9.043200016021729,"y":0.4289752946300115},{"x":9.137400016188622,"y":0.4289752946300115},{"x":9.231600016355515,"y":0.4289752946300115},{"x":9.325800016522408,"y":0.4289752946300115}]}],"marks":[{"type":"symbol","from":{"data":"a5634990-b956-4438-a6a7-5f0c8e4ed7bd"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"line","from":{"data":"21d41f63-6e19-4fbe-9526-db284b7a5bc7"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"a5634990-b956-4438-a6a7-5f0c8e4ed7bd\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"a5634990-b956-4438-a6a7-5f0c8e4ed7bd\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"a5634990-b956-4438-a6a7-5f0c8e4ed7bd\", :values ({:x 0, :y 0.0} {:x 0.5, :y 0.479425538604203} {:x 1.0, :y 0.8414709848078965} {:x 1.5, :y 0.9974949866040544} {:x 2.0, :y 0.9092974268256817} {:x 2.5, :y 0.5984721441039564} {:x 3.0, :y 0.1411200080598672} {:x 3.5, :y -0.35078322768961984} {:x 4.0, :y -0.7568024953079282} {:x 4.5, :y -0.977530117665097} {:x 5.0, :y -0.9589242746631385} {:x 5.5, :y -0.7055403255703919} {:x 6.0, :y -0.27941549819892586} {:x 6.5, :y 0.21511998808781552} {:x 7.0, :y 0.6569865987187891} {:x 7.5, :y 0.9379999767747389} {:x 8.0, :y 0.9893582466233818} {:x 8.5, :y 0.7984871126234903} {:x 9.0, :y 0.4121184852417566})} {:name \"21d41f63-6e19-4fbe-9526-db284b7a5bc7\", :values ({:x 0, :y 0.4289752946300115} {:x 0.094200000166893, :y 0.4289752946300115} {:x 0.188400000333786, :y 0.4289752946300115} {:x 0.282600000500679, :y 0.4289752946300115} {:x 0.376800000667572, :y 0.4289752946300115} {:x 0.471000000834465, :y 0.4289752946300115} {:x 0.565200001001358, :y 0.4289752946300115} {:x 0.659400001168251, :y 0.4289752946300115} {:x 0.753600001335144, :y 0.4289752946300115} {:x 0.847800001502037, :y 0.4289752946300115} {:x 0.94200000166893, :y 0.4289752946300115} {:x 1.036200001835823, :y 0.4289752946300115} {:x 1.130400002002716, :y 0.4289752946300115} {:x 1.224600002169609, :y 0.4289752946300115} {:x 1.318800002336502, :y 0.4289752946300115} {:x 1.413000002503395, :y 0.4289752946300115} {:x 1.507200002670288, :y 0.4289752946300115} {:x 1.601400002837181, :y 0.4289752946300115} {:x 1.695600003004074, :y 0.4289752946300115} {:x 1.789800003170967, :y 0.4289752946300115} {:x 1.88400000333786, :y 0.4289752946300115} {:x 1.9782000035047531, :y 0.4289752946300115} {:x 2.072400003671646, :y 0.4289752946300115} {:x 2.166600003838539, :y 0.4289752946300115} {:x 2.260800004005432, :y 0.4289752946300115} {:x 2.355000004172325, :y 0.4289752946300115} {:x 2.449200004339218, :y 0.4289752946300115} {:x 2.543400004506111, :y 0.4289752946300115} {:x 2.637600004673004, :y 0.4289752946300115} {:x 2.731800004839897, :y 0.4289752946300115} {:x 2.82600000500679, :y 0.4289752946300115} {:x 2.920200005173683, :y 0.4289752946300115} {:x 3.014400005340576, :y 0.4289752946300115} {:x 3.108600005507469, :y 0.4289752946300115} {:x 3.202800005674362, :y 0.4289752946300115} {:x 3.297000005841255, :y 0.4289752946300115} {:x 3.391200006008148, :y 0.4289752946300115} {:x 3.485400006175041, :y 0.4289752946300115} {:x 3.579600006341934, :y 0.4289752946300115} {:x 3.673800006508827, :y 0.4289752946300115} {:x 3.76800000667572, :y 0.4289752946300115} {:x 3.862200006842613, :y 0.4289752946300115} {:x 3.9564000070095062, :y 0.4289752946300115} {:x 4.050600007176399, :y 0.4289752946300115} {:x 4.144800007343292, :y 0.4289752946300115} {:x 4.239000007510185, :y 0.4289752946300115} {:x 4.333200007677078, :y 0.4289752946300115} {:x 4.427400007843971, :y 0.4289752946300115} {:x 4.521600008010864, :y 0.4289752946300115} {:x 4.615800008177757, :y 0.4289752946300115} {:x 4.71000000834465, :y 0.4289752946300115} {:x 4.804200008511543, :y 0.4289752946300115} {:x 4.898400008678436, :y 0.4289752946300115} {:x 4.992600008845329, :y 0.4289752946300115} {:x 5.086800009012222, :y 0.4289752946300115} {:x 5.181000009179115, :y 0.4289752946300115} {:x 5.275200009346008, :y 0.4289752946300115} {:x 5.369400009512901, :y 0.4289752946300115} {:x 5.463600009679794, :y 0.4289752946300115} {:x 5.557800009846687, :y 0.4289752946300115} {:x 5.65200001001358, :y 0.4289752946300115} {:x 5.746200010180473, :y 0.4289752946300115} {:x 5.840400010347366, :y 0.4289752946300115} {:x 5.934600010514259, :y 0.4289752946300115} {:x 6.028800010681152, :y 0.4289752946300115} {:x 6.123000010848045, :y 0.4289752946300115} {:x 6.217200011014938, :y 0.4289752946300115} {:x 6.311400011181831, :y 0.4289752946300115} {:x 6.405600011348724, :y 0.4289752946300115} {:x 6.499800011515617, :y 0.4289752946300115} {:x 6.59400001168251, :y 0.4289752946300115} {:x 6.688200011849403, :y 0.4289752946300115} {:x 6.782400012016296, :y 0.4289752946300115} {:x 6.876600012183189, :y 0.4289752946300115} {:x 6.970800012350082, :y 0.4289752946300115} {:x 7.065000012516975, :y 0.4289752946300115} {:x 7.159200012683868, :y 0.4289752946300115} {:x 7.253400012850761, :y 0.4289752946300115} {:x 7.347600013017654, :y 0.4289752946300115} {:x 7.441800013184547, :y 0.4289752946300115} {:x 7.53600001335144, :y 0.4289752946300115} {:x 7.630200013518333, :y 0.4289752946300115} {:x 7.724400013685226, :y 0.4289752946300115} {:x 7.8186000138521194, :y 0.4289752946300115} {:x 7.9128000140190125, :y 0.4289752946300115} {:x 8.007000014185905, :y 0.4289752946300115} {:x 8.101200014352798, :y 0.4289752946300115} {:x 8.195400014519691, :y 0.4289752946300115} {:x 8.289600014686584, :y 0.4289752946300115} {:x 8.383800014853477, :y 0.4289752946300115} {:x 8.47800001502037, :y 0.4289752946300115} {:x 8.572200015187263, :y 0.4289752946300115} {:x 8.666400015354156, :y 0.4289752946300115} {:x 8.76060001552105, :y 0.4289752946300115} {:x 8.854800015687943, :y 0.4289752946300115} {:x 8.949000015854836, :y 0.4289752946300115} {:x 9.043200016021729, :y 0.4289752946300115} {:x 9.137400016188622, :y 0.4289752946300115} {:x 9.231600016355515, :y 0.4289752946300115} {:x 9.325800016522408, :y 0.4289752946300115})}), :marks ({:type \"symbol\", :from {:data \"a5634990-b956-4438-a6a7-5f0c8e4ed7bd\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"line\", :from {:data \"21d41f63-6e19-4fbe-9526-db284b7a5bc7\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@
(pprint/pprint (:expr (last bests)))

;; @@
;; ->
;;; 0.4289752946300115
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; # Darwin
;; **

;; **
;;; Implementing Darwin Evolution mechanism with the symbolic regression operations.
;;; 
;; **

;; **
;;; In the Darwin Evolution there are tunable paramaters which can be inputted into the search to refine/specify the regression landscape. 
;;; 
;;; ###Paramaters
;; **

;; @@
; Parsimony Pressure
; Used by 			dwin-score

(def pars-pressure -0.02)

; Terminal Depth (Depths of new branches generate)
; Used by:     		dwin-mutate		dwin-new

(def term-dep 3)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/interval</span>","value":"#'joel-darwin/interval"}
;; <=

;; **
;;; **dwin-new**
;; **

;; @@
(defn dwin-new [] (random-full-tree functions terminals term-dep))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-new</span>","value":"#'joel-darwin/dwin-new"}
;; <=

;; @@
(dwin-new)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-double'>0.5035399573371343</span>","value":"0.5035399573371343"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(- 0.5035399573371343 x)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-double'>0.8936210532637157</span>","value":"0.8936210532637157"},{"type":"html","content":"<span class='clj-double'>0.9889747654696438</span>","value":"0.9889747654696438"}],"value":"(+ 0.8936210532637157 0.9889747654696438)"}],"value":"(* (- 0.5035399573371343 x) (+ 0.8936210532637157 0.9889747654696438))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(- x x)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-double'>0.07551029568758572</span>","value":"0.07551029568758572"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(pdiv 0.07551029568758572 x)"}],"value":"(pdiv (- x x) (pdiv 0.07551029568758572 x))"}],"value":"(pdiv (* (- 0.5035399573371343 x) (+ 0.8936210532637157 0.9889747654696438)) (pdiv (- x x) (pdiv 0.07551029568758572 x)))"}
;; <=

;; **
;;; **dwin-score**
;; **

;; @@
(defn dwin-score [ex] 
    (* -1 (score-pp data ex pars-pressure)))

; Made score positive to fit darwin
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-score</span>","value":"#'joel-darwin/dwin-score"}
;; <=

;; @@
(let [expr1 (dwin-new)]
      (println "darwin score:" (dwin-score expr1))
      (print expr1))
;; @@
;; ->
;;; darwin score: 3059.351664070272
;;; (* (- (* x 0.191607473674258) (* x x)) (- (+ x x) (+ 0.9767632339296739 x)))
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; **dwin-cross**
;; **

;; @@
(defn dwin-cross [ex1 ex2] (crossover-expr ex1 ex2))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-cross</span>","value":"#'joel-darwin/dwin-cross"}
;; <=

;; @@
(dwin-cross (dwin-new) (dwin-new))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-double'>0.14373118167708054</span>","value":"0.14373118167708054"},{"type":"html","content":"<span class='clj-double'>0.42888093950264794</span>","value":"0.42888093950264794"}],"value":"(- 0.14373118167708054 0.42888093950264794)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(pdiv x x)"}],"value":"(* (- 0.14373118167708054 0.42888093950264794) (pdiv x x))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.2655217623901891</span>","value":"0.2655217623901891"}],"value":"(- x 0.2655217623901891)"}],"value":"(pdiv (* (- 0.14373118167708054 0.42888093950264794) (pdiv x x)) (- x 0.2655217623901891))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-double'>0.5710345829771953</span>","value":"0.5710345829771953"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 0.5710345829771953 x)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-double'>0.28626497025223585</span>","value":"0.28626497025223585"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 0.28626497025223585 x)"}],"value":"(+ (* 0.5710345829771953 x) (* 0.28626497025223585 x))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.261142059335719</span>","value":"0.261142059335719"}],"value":"(- x 0.261142059335719)"}],"value":"(- (+ (* 0.5710345829771953 x) (* 0.28626497025223585 x)) (- x 0.261142059335719))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.10056548353379591</span>","value":"0.10056548353379591"}],"value":"(+ x 0.10056548353379591)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(+ x x)"}],"value":"(- (+ x 0.10056548353379591) (+ x x))"}],"value":"(pdiv (- (+ (* 0.5710345829771953 x) (* 0.28626497025223585 x)) (- x 0.261142059335719)) (- (+ x 0.10056548353379591) (+ x x)))"}],"value":"[(pdiv (* (- 0.14373118167708054 0.42888093950264794) (pdiv x x)) (- x 0.2655217623901891)) (pdiv (- (+ (* 0.5710345829771953 x) (* 0.28626497025223585 x)) (- x 0.261142059335719)) (- (+ x 0.10056548353379591) (+ x x)))]"}
;; <=

;; **
;;; **dwin-mutate**
;; **

;; @@
(defn dwin-mutate [ex] (mutate-expr ex #(random-full-tree functions terminals term-dep)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-mutate</span>","value":"#'joel-darwin/dwin-mutate"}
;; <=

;; @@
(dwin-mutate [1])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.3605437058712132</span>","value":"0.3605437058712132"}],"value":"(* x 0.3605437058712132)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.1969759746701515</span>","value":"0.1969759746701515"}],"value":"(- x 0.1969759746701515)"}],"value":"(- (* x 0.3605437058712132) (- x 0.1969759746701515))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>-</span>","value":"-"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(pdiv x x)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-double'>0.8191862586036777</span>","value":"0.8191862586036777"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(pdiv 0.8191862586036777 x)"}],"value":"(- (pdiv x x) (pdiv 0.8191862586036777 x))"}],"value":"(pdiv (- (* x 0.3605437058712132) (- x 0.1969759746701515)) (- (pdiv x x) (pdiv 0.8191862586036777 x)))"}
;; <=

;; **
;;; # Tests
;; **

;; @@
;Interval of target dataset

(def interval 0.1)

;Number of generations 

(def gen 500)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/gen</span>","value":"#'joel-darwin/gen"}
;; <=

;; **
;;; **Target Data Set**
;; **

;; @@
(def data (doall (map (fn [x] [x (Math/sin x)]) (range 0 (* 3 pi) interval))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/data</span>","value":"#'joel-darwin/data"}
;; <=

;; **
;;; # dwin-Results 
;; **

;; **
;;; Evolve the population using...:   `darwin/evolve`
;; **

;; @@
(def result (darwin/evolve dwin-score dwin-cross dwin-mutate dwin-new gen))
;; @@
;; ->
;;; ..........................................................................................................................................................................
;; <-

;; **
;;; This took a very long time -- 30 mins
;;; - Reduce interval spacing of target Data set.
;; **

;; **
;;; 
;; **

;; @@
(first (sort-by :score (:rabble result)))
;; @@

;; **
;;; Rabble calls/creates the zeitgeist of the population
;; **

;; @@
(plot/compose
 (plot/list-plot data)
 (plot/plot (functionalise ((first (sort-by (:score (:rabble result))) :genotype)) [0 (* 3 pi)]))
;; @@

;; **
;;; How the best individual in the population's error came down as a function of generation number.
;; **

;; @@
(plot/list-plot (mapv #(Math/log (+ % 1)) (:min (:score @metrics/metrics))) :joined true)
;; @@

;; @@
(plot/list-plot (mapv #(Math/log (+ % 1)) (:max (:score @metrics/metrics))) :joined true)
;; @@

;; @@
(plot/list-plot (mapv #(Math/log (+ % 1)) (:max (:score @metrics/metrics))) :joined true)
;; @@

;; **
;;; #SPEA 2
;; **

;; **
;;; **Data**
;; **

;; @@
(def spea2-range (* 2 3.14))

(def spea2-interval 0.1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/spea2-interval</span>","value":"#'joel-darwin/spea2-interval"}
;; <=

;; **
;;; Algeboblic requires data as a vector of coordinate vectors. (In this case one dimensional)
;; **

;; @@
(def xs (range 0.0 spea2-range spea2-interval))
(def coords (mapv (fn [x] [x]) xs))

;print xs

;print coords
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/coords</span>","value":"#'joel-darwin/coords"}
;; <=

;; @@
(def spea2-data (map (fn [x] (Math/sin x)) xs))

;print spea-data
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/spea-data</span>","value":"#'joel-darwin/spea-data"}
;; <=

;; @@
(plot/list-plot spea-data)   ; A visual rep of data

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"6739dd51-4e95-40c6-9242-3de6e60ee26e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"6739dd51-4e95-40c6-9242-3de6e60ee26e","field":"data.y"}}],"marks":[{"type":"symbol","from":{"data":"6739dd51-4e95-40c6-9242-3de6e60ee26e"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"data":[{"name":"6739dd51-4e95-40c6-9242-3de6e60ee26e","values":[{"x":0,"y":0.0},{"x":1,"y":0.09983341664682815},{"x":2,"y":0.19866933079506122},{"x":3,"y":0.2955202066613396},{"x":4,"y":0.3894183423086505},{"x":5,"y":0.479425538604203},{"x":6,"y":0.5646424733950354},{"x":7,"y":0.644217687237691},{"x":8,"y":0.7173560908995227},{"x":9,"y":0.7833269096274833},{"x":10,"y":0.8414709848078964},{"x":11,"y":0.8912073600614353},{"x":12,"y":0.9320390859672263},{"x":13,"y":0.963558185417193},{"x":14,"y":0.9854497299884603},{"x":15,"y":0.9974949866040544},{"x":16,"y":0.9995736030415051},{"x":17,"y":0.9916648104524686},{"x":18,"y":0.973847630878195},{"x":19,"y":0.9463000876874144},{"x":20,"y":0.9092974268256815},{"x":21,"y":0.8632093666488735},{"x":22,"y":0.8084964038195899},{"x":23,"y":0.7457052121767197},{"x":24,"y":0.6754631805511504},{"x":25,"y":0.5984721441039558},{"x":26,"y":0.5155013718214634},{"x":27,"y":0.42737988023382895},{"x":28,"y":0.33498815015590383},{"x":29,"y":0.23924932921398112},{"x":30,"y":0.1411200080598659},{"x":31,"y":0.04158066243328916},{"x":32,"y":-0.05837414342758142},{"x":33,"y":-0.15774569414324996},{"x":34,"y":-0.25554110202683294},{"x":35,"y":-0.3507832276896215},{"x":36,"y":-0.44252044329485407},{"x":37,"y":-0.5298361409084948},{"x":38,"y":-0.6118578909427207},{"x":39,"y":-0.6877661591839753},{"x":40,"y":-0.7568024953079294},{"x":41,"y":-0.8182771110644114},{"x":42,"y":-0.8715757724135886},{"x":43,"y":-0.9161659367494552},{"x":44,"y":-0.9516020738895161},{"x":45,"y":-0.977530117665097},{"x":46,"y":-0.9936910036334644},{"x":47,"y":-0.9999232575641008},{"x":48,"y":-0.9961646088358408},{"x":49,"y":-0.9824526126243328},{"x":50,"y":-0.958924274663139},{"x":51,"y":-0.9258146823277331},{"x":52,"y":-0.8834546557201545},{"x":53,"y":-0.8322674422239027},{"x":54,"y":-0.7727644875559894},{"x":55,"y":-0.7055403255703945},{"x":56,"y":-0.6312666378723244},{"x":57,"y":-0.5506855425976414},{"x":58,"y":-0.4646021794137613},{"x":59,"y":-0.37387666483024096},{"x":60,"y":-0.27941549819893097},{"x":61,"y":-0.18216250427210112},{"x":62,"y":-0.0830894028175026}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"6739dd51-4e95-40c6-9242-3de6e60ee26e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"6739dd51-4e95-40c6-9242-3de6e60ee26e\", :field \"data.y\"}}], :marks [{:type \"symbol\", :from {:data \"6739dd51-4e95-40c6-9242-3de6e60ee26e\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :data [{:name \"6739dd51-4e95-40c6-9242-3de6e60ee26e\", :values ({:x 0, :y 0.0} {:x 1, :y 0.09983341664682815} {:x 2, :y 0.19866933079506122} {:x 3, :y 0.2955202066613396} {:x 4, :y 0.3894183423086505} {:x 5, :y 0.479425538604203} {:x 6, :y 0.5646424733950354} {:x 7, :y 0.644217687237691} {:x 8, :y 0.7173560908995227} {:x 9, :y 0.7833269096274833} {:x 10, :y 0.8414709848078964} {:x 11, :y 0.8912073600614353} {:x 12, :y 0.9320390859672263} {:x 13, :y 0.963558185417193} {:x 14, :y 0.9854497299884603} {:x 15, :y 0.9974949866040544} {:x 16, :y 0.9995736030415051} {:x 17, :y 0.9916648104524686} {:x 18, :y 0.973847630878195} {:x 19, :y 0.9463000876874144} {:x 20, :y 0.9092974268256815} {:x 21, :y 0.8632093666488735} {:x 22, :y 0.8084964038195899} {:x 23, :y 0.7457052121767197} {:x 24, :y 0.6754631805511504} {:x 25, :y 0.5984721441039558} {:x 26, :y 0.5155013718214634} {:x 27, :y 0.42737988023382895} {:x 28, :y 0.33498815015590383} {:x 29, :y 0.23924932921398112} {:x 30, :y 0.1411200080598659} {:x 31, :y 0.04158066243328916} {:x 32, :y -0.05837414342758142} {:x 33, :y -0.15774569414324996} {:x 34, :y -0.25554110202683294} {:x 35, :y -0.3507832276896215} {:x 36, :y -0.44252044329485407} {:x 37, :y -0.5298361409084948} {:x 38, :y -0.6118578909427207} {:x 39, :y -0.6877661591839753} {:x 40, :y -0.7568024953079294} {:x 41, :y -0.8182771110644114} {:x 42, :y -0.8715757724135886} {:x 43, :y -0.9161659367494552} {:x 44, :y -0.9516020738895161} {:x 45, :y -0.977530117665097} {:x 46, :y -0.9936910036334644} {:x 47, :y -0.9999232575641008} {:x 48, :y -0.9961646088358408} {:x 49, :y -0.9824526126243328} {:x 50, :y -0.958924274663139} {:x 51, :y -0.9258146823277331} {:x 52, :y -0.8834546557201545} {:x 53, :y -0.8322674422239027} {:x 54, :y -0.7727644875559894} {:x 55, :y -0.7055403255703945} {:x 56, :y -0.6312666378723244} {:x 57, :y -0.5506855425976414} {:x 58, :y -0.4646021794137613} {:x 59, :y -0.37387666483024096} {:x 60, :y -0.27941549819893097} {:x 61, :y -0.18216250427210112} {:x 62, :y -0.0830894028175026})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Import of config from SPEA-2 worksheet
;; **

;; @@
(def generation-config
  (let [size-limit 80                    ; let used to spell out params ect.
        min-size 15
        ea-config (spea2/spea2-config
                    {:goals [:error :complexity]
                     :archive-size 50
                     :binary-ops [{:op (partial genetics/ninety-ten-sl-crossover size-limit) :repeat 23}]
                     :unary-ops [{:op (partial genetics/random-tree-mutation functions terminals 3) :repeat 4}]})
        score-functions {:complexity tree/count-nodes
                         :error (fn [ex] (score/abs-error vars coords spea2-data ex))}]
    {:ea-config              ea-config
     :transformations        [(partial transform/apply-to-fraction-of-genotypes
                                       (partial transform/hill-descent
                                                genetics/twiddle-constants
                                                (:error score-functions))
                                       0.5)
                              (partial transform/apply-to-all-genotypes
                                       (partial genetics/trim-hard size-limit))
                              (partial transform/apply-to-all-genotypes
                                       (partial genetics/boost-hard min-size
                                                #(genetics/random-full-tree functions terminals 3)))]
     :score-functions        score-functions
     :reporting-function     (fn [z] (print ".") (flush))}))
;; @@

;; @@

;; @@
