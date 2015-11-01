;; gorilla-repl.fileformat = 1

;; **
;;; # Joel Darwin 
;;; 
;;; 
;;; **Log**
;;; 
;;; 
;;; 1/11/15   - Created this worksheet
;;; 		  - 
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
            [darwin.evolution.metrics :as metrics]))
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
(def pi 3.14)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/pi</span>","value":"#'joel-darwin/pi"}
;; <=

;; @@
(def data (doall (map (fn [x] [x (Math/sin x)]) (range 0 (* 3 pi) 0.1))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/data</span>","value":"#'joel-darwin/data"}
;; <=

;; @@
(plot/list-plot data)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"be0a1c7b-2418-43c4-84dd-f55a40c6546f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"be0a1c7b-2418-43c4-84dd-f55a40c6546f","field":"data.y"}}],"marks":[{"type":"symbol","from":{"data":"be0a1c7b-2418-43c4-84dd-f55a40c6546f"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"data":[{"name":"be0a1c7b-2418-43c4-84dd-f55a40c6546f","values":[{"x":0,"y":0.0},{"x":0.1,"y":0.09983341664682815},{"x":0.2,"y":0.19866933079506122},{"x":0.30000000000000004,"y":0.2955202066613396},{"x":0.4,"y":0.3894183423086505},{"x":0.5,"y":0.479425538604203},{"x":0.6,"y":0.5646424733950354},{"x":0.7,"y":0.644217687237691},{"x":0.7999999999999999,"y":0.7173560908995227},{"x":0.8999999999999999,"y":0.7833269096274833},{"x":0.9999999999999999,"y":0.8414709848078964},{"x":1.0999999999999999,"y":0.8912073600614353},{"x":1.2,"y":0.9320390859672263},{"x":1.3,"y":0.963558185417193},{"x":1.4000000000000001,"y":0.9854497299884603},{"x":1.5000000000000002,"y":0.9974949866040544},{"x":1.6000000000000003,"y":0.9995736030415051},{"x":1.7000000000000004,"y":0.9916648104524686},{"x":1.8000000000000005,"y":0.973847630878195},{"x":1.9000000000000006,"y":0.9463000876874144},{"x":2.0000000000000004,"y":0.9092974268256815},{"x":2.1000000000000005,"y":0.8632093666488735},{"x":2.2000000000000006,"y":0.8084964038195899},{"x":2.3000000000000007,"y":0.7457052121767197},{"x":2.400000000000001,"y":0.6754631805511504},{"x":2.500000000000001,"y":0.5984721441039558},{"x":2.600000000000001,"y":0.5155013718214634},{"x":2.700000000000001,"y":0.42737988023382895},{"x":2.800000000000001,"y":0.33498815015590383},{"x":2.9000000000000012,"y":0.23924932921398112},{"x":3.0000000000000013,"y":0.1411200080598659},{"x":3.1000000000000014,"y":0.04158066243328916},{"x":3.2000000000000015,"y":-0.05837414342758142},{"x":3.3000000000000016,"y":-0.15774569414324996},{"x":3.4000000000000017,"y":-0.25554110202683294},{"x":3.5000000000000018,"y":-0.3507832276896215},{"x":3.600000000000002,"y":-0.44252044329485407},{"x":3.700000000000002,"y":-0.5298361409084948},{"x":3.800000000000002,"y":-0.6118578909427207},{"x":3.900000000000002,"y":-0.6877661591839753},{"x":4.000000000000002,"y":-0.7568024953079294},{"x":4.100000000000001,"y":-0.8182771110644114},{"x":4.200000000000001,"y":-0.8715757724135886},{"x":4.300000000000001,"y":-0.9161659367494552},{"x":4.4,"y":-0.9516020738895161},{"x":4.5,"y":-0.977530117665097},{"x":4.6,"y":-0.9936910036334644},{"x":4.699999999999999,"y":-0.9999232575641008},{"x":4.799999999999999,"y":-0.9961646088358408},{"x":4.899999999999999,"y":-0.9824526126243328},{"x":4.999999999999998,"y":-0.958924274663139},{"x":5.099999999999998,"y":-0.9258146823277331},{"x":5.1999999999999975,"y":-0.8834546557201545},{"x":5.299999999999997,"y":-0.8322674422239027},{"x":5.399999999999997,"y":-0.7727644875559894},{"x":5.4999999999999964,"y":-0.7055403255703945},{"x":5.599999999999996,"y":-0.6312666378723244},{"x":5.699999999999996,"y":-0.5506855425976414},{"x":5.799999999999995,"y":-0.4646021794137613},{"x":5.899999999999995,"y":-0.37387666483024096},{"x":5.999999999999995,"y":-0.27941549819893097},{"x":6.099999999999994,"y":-0.18216250427210112},{"x":6.199999999999994,"y":-0.0830894028175026},{"x":6.299999999999994,"y":0.016813900484343496},{"x":6.399999999999993,"y":0.11654920485048659},{"x":6.499999999999993,"y":0.21511998808780858},{"x":6.5999999999999925,"y":0.3115413635133711},{"x":6.699999999999992,"y":0.40484992061659103},{"x":6.799999999999992,"y":0.4941133511386012},{"x":6.8999999999999915,"y":0.5784397643881929},{"x":6.999999999999991,"y":0.6569865987187824},{"x":7.099999999999991,"y":0.7289690401258698},{"x":7.19999999999999,"y":0.7936678638491472},{"x":7.29999999999999,"y":0.8504366206285593},{"x":7.39999999999999,"y":0.8987080958116223},{"x":7.499999999999989,"y":0.9379999767747351},{"x":7.599999999999989,"y":0.9679196720314837},{"x":7.699999999999989,"y":0.9881682338769986},{"x":7.799999999999988,"y":0.9985433453746043},{"x":7.899999999999988,"y":0.9989413418397726},{"x":7.999999999999988,"y":0.9893582466233836},{"x":8.099999999999987,"y":0.9698898108450894},{"x":8.199999999999987,"y":0.9407305566797773},{"x":8.299999999999986,"y":0.9021718337562995},{"x":8.399999999999986,"y":0.8545989080882879},{"x":8.499999999999986,"y":0.7984871126234988},{"x":8.599999999999985,"y":0.734397097874123},{"x":8.699999999999985,"y":0.662969230082194},{"x":8.799999999999985,"y":0.5849171928917747},{"x":8.899999999999984,"y":0.5010208564578985},{"x":8.999999999999984,"y":0.41211848524177114},{"x":9.099999999999984,"y":0.3190983623493673},{"x":9.199999999999983,"y":0.22288991410026324},{"x":9.299999999999983,"y":0.12445442350707933},{"x":9.399999999999983,"y":0.024775425453375525}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"be0a1c7b-2418-43c4-84dd-f55a40c6546f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"be0a1c7b-2418-43c4-84dd-f55a40c6546f\", :field \"data.y\"}}], :marks [{:type \"symbol\", :from {:data \"be0a1c7b-2418-43c4-84dd-f55a40c6546f\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :data [{:name \"be0a1c7b-2418-43c4-84dd-f55a40c6546f\", :values ({:x 0, :y 0.0} {:x 0.1, :y 0.09983341664682815} {:x 0.2, :y 0.19866933079506122} {:x 0.30000000000000004, :y 0.2955202066613396} {:x 0.4, :y 0.3894183423086505} {:x 0.5, :y 0.479425538604203} {:x 0.6, :y 0.5646424733950354} {:x 0.7, :y 0.644217687237691} {:x 0.7999999999999999, :y 0.7173560908995227} {:x 0.8999999999999999, :y 0.7833269096274833} {:x 0.9999999999999999, :y 0.8414709848078964} {:x 1.0999999999999999, :y 0.8912073600614353} {:x 1.2, :y 0.9320390859672263} {:x 1.3, :y 0.963558185417193} {:x 1.4000000000000001, :y 0.9854497299884603} {:x 1.5000000000000002, :y 0.9974949866040544} {:x 1.6000000000000003, :y 0.9995736030415051} {:x 1.7000000000000004, :y 0.9916648104524686} {:x 1.8000000000000005, :y 0.973847630878195} {:x 1.9000000000000006, :y 0.9463000876874144} {:x 2.0000000000000004, :y 0.9092974268256815} {:x 2.1000000000000005, :y 0.8632093666488735} {:x 2.2000000000000006, :y 0.8084964038195899} {:x 2.3000000000000007, :y 0.7457052121767197} {:x 2.400000000000001, :y 0.6754631805511504} {:x 2.500000000000001, :y 0.5984721441039558} {:x 2.600000000000001, :y 0.5155013718214634} {:x 2.700000000000001, :y 0.42737988023382895} {:x 2.800000000000001, :y 0.33498815015590383} {:x 2.9000000000000012, :y 0.23924932921398112} {:x 3.0000000000000013, :y 0.1411200080598659} {:x 3.1000000000000014, :y 0.04158066243328916} {:x 3.2000000000000015, :y -0.05837414342758142} {:x 3.3000000000000016, :y -0.15774569414324996} {:x 3.4000000000000017, :y -0.25554110202683294} {:x 3.5000000000000018, :y -0.3507832276896215} {:x 3.600000000000002, :y -0.44252044329485407} {:x 3.700000000000002, :y -0.5298361409084948} {:x 3.800000000000002, :y -0.6118578909427207} {:x 3.900000000000002, :y -0.6877661591839753} {:x 4.000000000000002, :y -0.7568024953079294} {:x 4.100000000000001, :y -0.8182771110644114} {:x 4.200000000000001, :y -0.8715757724135886} {:x 4.300000000000001, :y -0.9161659367494552} {:x 4.4, :y -0.9516020738895161} {:x 4.5, :y -0.977530117665097} {:x 4.6, :y -0.9936910036334644} {:x 4.699999999999999, :y -0.9999232575641008} {:x 4.799999999999999, :y -0.9961646088358408} {:x 4.899999999999999, :y -0.9824526126243328} {:x 4.999999999999998, :y -0.958924274663139} {:x 5.099999999999998, :y -0.9258146823277331} {:x 5.1999999999999975, :y -0.8834546557201545} {:x 5.299999999999997, :y -0.8322674422239027} {:x 5.399999999999997, :y -0.7727644875559894} {:x 5.4999999999999964, :y -0.7055403255703945} {:x 5.599999999999996, :y -0.6312666378723244} {:x 5.699999999999996, :y -0.5506855425976414} {:x 5.799999999999995, :y -0.4646021794137613} {:x 5.899999999999995, :y -0.37387666483024096} {:x 5.999999999999995, :y -0.27941549819893097} {:x 6.099999999999994, :y -0.18216250427210112} {:x 6.199999999999994, :y -0.0830894028175026} {:x 6.299999999999994, :y 0.016813900484343496} {:x 6.399999999999993, :y 0.11654920485048659} {:x 6.499999999999993, :y 0.21511998808780858} {:x 6.5999999999999925, :y 0.3115413635133711} {:x 6.699999999999992, :y 0.40484992061659103} {:x 6.799999999999992, :y 0.4941133511386012} {:x 6.8999999999999915, :y 0.5784397643881929} {:x 6.999999999999991, :y 0.6569865987187824} {:x 7.099999999999991, :y 0.7289690401258698} {:x 7.19999999999999, :y 0.7936678638491472} {:x 7.29999999999999, :y 0.8504366206285593} {:x 7.39999999999999, :y 0.8987080958116223} {:x 7.499999999999989, :y 0.9379999767747351} {:x 7.599999999999989, :y 0.9679196720314837} {:x 7.699999999999989, :y 0.9881682338769986} {:x 7.799999999999988, :y 0.9985433453746043} {:x 7.899999999999988, :y 0.9989413418397726} {:x 7.999999999999988, :y 0.9893582466233836} {:x 8.099999999999987, :y 0.9698898108450894} {:x 8.199999999999987, :y 0.9407305566797773} {:x 8.299999999999986, :y 0.9021718337562995} {:x 8.399999999999986, :y 0.8545989080882879} {:x 8.499999999999986, :y 0.7984871126234988} {:x 8.599999999999985, :y 0.734397097874123} {:x 8.699999999999985, :y 0.662969230082194} {:x 8.799999999999985, :y 0.5849171928917747} {:x 8.899999999999984, :y 0.5010208564578985} {:x 8.999999999999984, :y 0.41211848524177114} {:x 9.099999999999984, :y 0.3190983623493673} {:x 9.199999999999983, :y 0.22288991410026324} {:x 9.299999999999983, :y 0.12445442350707933} {:x 9.399999999999983, :y 0.024775425453375525})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
;;; $$ \textbf{CROSS OVER} $$
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
(time (def run-data (doall (take 100 (iterate #(evolve % config) init-pop)))))
;; @@
;; ->
;;; &quot;Elapsed time: 10832.391368 msecs&quot;
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
;;; {"type":"vega","content":{"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"70cbd5c6-23b1-4241-8c37-056eed46fcbc","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"70cbd5c6-23b1-4241-8c37-056eed46fcbc","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"70cbd5c6-23b1-4241-8c37-056eed46fcbc"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"70cbd5c6-23b1-4241-8c37-056eed46fcbc","values":[{"x":0,"y":-57.96217481519868},{"x":1,"y":-54.32815638495764},{"x":2,"y":-54.305049497939265},{"x":3,"y":-54.31090945506101},{"x":4,"y":-54.31090945506101},{"x":5,"y":-54.31090945506101},{"x":6,"y":-54.31090945506101},{"x":7,"y":-54.31090945506101},{"x":8,"y":-54.31090945506101},{"x":9,"y":-54.31090945506101},{"x":10,"y":-54.31090945506101},{"x":11,"y":-54.31090945506101},{"x":12,"y":-54.31090945506101},{"x":13,"y":-54.31090945506101},{"x":14,"y":-54.31090945506101},{"x":15,"y":-54.31090945506101},{"x":16,"y":-54.31090945506101},{"x":17,"y":-54.31090945506101},{"x":18,"y":-54.31090945506101},{"x":19,"y":-54.31090945506101},{"x":20,"y":-54.31090945506101},{"x":21,"y":-54.31090945506101},{"x":22,"y":-54.31090945506101},{"x":23,"y":-54.31090945506101},{"x":24,"y":-54.31090945506101},{"x":25,"y":-54.31090945506101},{"x":26,"y":-54.31090945506101},{"x":27,"y":-54.31090945506101},{"x":28,"y":-54.31090945506101},{"x":29,"y":-54.31090945506101},{"x":30,"y":-54.31090945506101},{"x":31,"y":-54.31090945506101},{"x":32,"y":-54.31090945506101},{"x":33,"y":-54.31090945506101},{"x":34,"y":-54.31090945506101},{"x":35,"y":-54.31090945506101},{"x":36,"y":-54.31090945506101},{"x":37,"y":-54.31090945506101},{"x":38,"y":-54.31090945506101},{"x":39,"y":-54.31090945506101},{"x":40,"y":-54.31090945506101},{"x":41,"y":-54.31090945506101},{"x":42,"y":-54.31090945506101},{"x":43,"y":-54.31090945506101},{"x":44,"y":-54.31090945506101},{"x":45,"y":-54.31090945506101},{"x":46,"y":-54.31090945506101},{"x":47,"y":-54.31090945506101},{"x":48,"y":-54.31090945506101},{"x":49,"y":-54.31090945506101},{"x":50,"y":-54.31090945506101},{"x":51,"y":-54.31090945506101},{"x":52,"y":-54.31090945506101},{"x":53,"y":-54.31090945506101},{"x":54,"y":-54.31090945506101},{"x":55,"y":-54.31090945506101},{"x":56,"y":-54.31090945506101},{"x":57,"y":-54.31090945506101},{"x":58,"y":-54.31090945506101},{"x":59,"y":-54.31090945506101},{"x":60,"y":-54.31090945506101},{"x":61,"y":-54.31090945506101},{"x":62,"y":-54.31090945506101},{"x":63,"y":-54.31090945506101},{"x":64,"y":-54.31090945506101},{"x":65,"y":-54.31090945506101},{"x":66,"y":-54.31090945506101},{"x":67,"y":-54.31090945506101},{"x":68,"y":-54.31090945506101},{"x":69,"y":-54.31090945506101},{"x":70,"y":-54.31090945506101},{"x":71,"y":-54.31090945506101},{"x":72,"y":-54.31090945506101},{"x":73,"y":-54.31090945506101},{"x":74,"y":-54.31090945506101},{"x":75,"y":-54.31090945506101},{"x":76,"y":-54.31090945506101},{"x":77,"y":-54.31090945506101},{"x":78,"y":-54.31090945506101},{"x":79,"y":-54.31090945506101},{"x":80,"y":-54.31090945506101},{"x":81,"y":-54.31090945506101},{"x":82,"y":-54.31090945506101},{"x":83,"y":-54.31090945506101},{"x":84,"y":-54.31090945506101},{"x":85,"y":-54.31090945506101},{"x":86,"y":-54.31090945506101},{"x":87,"y":-54.31090945506101},{"x":88,"y":-54.31090945506101},{"x":89,"y":-54.31090945506101},{"x":90,"y":-54.31090945506101},{"x":91,"y":-54.31090945506101},{"x":92,"y":-54.31090945506101},{"x":93,"y":-54.31090945506101},{"x":94,"y":-54.31090945506101},{"x":95,"y":-54.31090945506101},{"x":96,"y":-54.31090945506101},{"x":97,"y":-54.31090945506101},{"x":98,"y":-54.31090945506101},{"x":99,"y":-54.31090945506101}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"70cbd5c6-23b1-4241-8c37-056eed46fcbc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"70cbd5c6-23b1-4241-8c37-056eed46fcbc\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"70cbd5c6-23b1-4241-8c37-056eed46fcbc\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"70cbd5c6-23b1-4241-8c37-056eed46fcbc\", :values ({:x 0, :y -57.96217481519868} {:x 1, :y -54.32815638495764} {:x 2, :y -54.305049497939265} {:x 3, :y -54.31090945506101} {:x 4, :y -54.31090945506101} {:x 5, :y -54.31090945506101} {:x 6, :y -54.31090945506101} {:x 7, :y -54.31090945506101} {:x 8, :y -54.31090945506101} {:x 9, :y -54.31090945506101} {:x 10, :y -54.31090945506101} {:x 11, :y -54.31090945506101} {:x 12, :y -54.31090945506101} {:x 13, :y -54.31090945506101} {:x 14, :y -54.31090945506101} {:x 15, :y -54.31090945506101} {:x 16, :y -54.31090945506101} {:x 17, :y -54.31090945506101} {:x 18, :y -54.31090945506101} {:x 19, :y -54.31090945506101} {:x 20, :y -54.31090945506101} {:x 21, :y -54.31090945506101} {:x 22, :y -54.31090945506101} {:x 23, :y -54.31090945506101} {:x 24, :y -54.31090945506101} {:x 25, :y -54.31090945506101} {:x 26, :y -54.31090945506101} {:x 27, :y -54.31090945506101} {:x 28, :y -54.31090945506101} {:x 29, :y -54.31090945506101} {:x 30, :y -54.31090945506101} {:x 31, :y -54.31090945506101} {:x 32, :y -54.31090945506101} {:x 33, :y -54.31090945506101} {:x 34, :y -54.31090945506101} {:x 35, :y -54.31090945506101} {:x 36, :y -54.31090945506101} {:x 37, :y -54.31090945506101} {:x 38, :y -54.31090945506101} {:x 39, :y -54.31090945506101} {:x 40, :y -54.31090945506101} {:x 41, :y -54.31090945506101} {:x 42, :y -54.31090945506101} {:x 43, :y -54.31090945506101} {:x 44, :y -54.31090945506101} {:x 45, :y -54.31090945506101} {:x 46, :y -54.31090945506101} {:x 47, :y -54.31090945506101} {:x 48, :y -54.31090945506101} {:x 49, :y -54.31090945506101} {:x 50, :y -54.31090945506101} {:x 51, :y -54.31090945506101} {:x 52, :y -54.31090945506101} {:x 53, :y -54.31090945506101} {:x 54, :y -54.31090945506101} {:x 55, :y -54.31090945506101} {:x 56, :y -54.31090945506101} {:x 57, :y -54.31090945506101} {:x 58, :y -54.31090945506101} {:x 59, :y -54.31090945506101} {:x 60, :y -54.31090945506101} {:x 61, :y -54.31090945506101} {:x 62, :y -54.31090945506101} {:x 63, :y -54.31090945506101} {:x 64, :y -54.31090945506101} {:x 65, :y -54.31090945506101} {:x 66, :y -54.31090945506101} {:x 67, :y -54.31090945506101} {:x 68, :y -54.31090945506101} {:x 69, :y -54.31090945506101} {:x 70, :y -54.31090945506101} {:x 71, :y -54.31090945506101} {:x 72, :y -54.31090945506101} {:x 73, :y -54.31090945506101} {:x 74, :y -54.31090945506101} {:x 75, :y -54.31090945506101} {:x 76, :y -54.31090945506101} {:x 77, :y -54.31090945506101} {:x 78, :y -54.31090945506101} {:x 79, :y -54.31090945506101} {:x 80, :y -54.31090945506101} {:x 81, :y -54.31090945506101} {:x 82, :y -54.31090945506101} {:x 83, :y -54.31090945506101} {:x 84, :y -54.31090945506101} {:x 85, :y -54.31090945506101} {:x 86, :y -54.31090945506101} {:x 87, :y -54.31090945506101} {:x 88, :y -54.31090945506101} {:x 89, :y -54.31090945506101} {:x 90, :y -54.31090945506101} {:x 91, :y -54.31090945506101} {:x 92, :y -54.31090945506101} {:x 93, :y -54.31090945506101} {:x 94, :y -54.31090945506101} {:x 95, :y -54.31090945506101} {:x 96, :y -54.31090945506101} {:x 97, :y -54.31090945506101} {:x 98, :y -54.31090945506101} {:x 99, :y -54.31090945506101})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2cd17667-bb33-46cc-bdc8-2a9e28781346","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2cd17667-bb33-46cc-bdc8-2a9e28781346","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"2cd17667-bb33-46cc-bdc8-2a9e28781346","values":[{"x":0,"y":0.0},{"x":0.1,"y":0.09983341664682815},{"x":0.2,"y":0.19866933079506122},{"x":0.30000000000000004,"y":0.2955202066613396},{"x":0.4,"y":0.3894183423086505},{"x":0.5,"y":0.479425538604203},{"x":0.6,"y":0.5646424733950354},{"x":0.7,"y":0.644217687237691},{"x":0.7999999999999999,"y":0.7173560908995227},{"x":0.8999999999999999,"y":0.7833269096274833},{"x":0.9999999999999999,"y":0.8414709848078964},{"x":1.0999999999999999,"y":0.8912073600614353},{"x":1.2,"y":0.9320390859672263},{"x":1.3,"y":0.963558185417193},{"x":1.4000000000000001,"y":0.9854497299884603},{"x":1.5000000000000002,"y":0.9974949866040544},{"x":1.6000000000000003,"y":0.9995736030415051},{"x":1.7000000000000004,"y":0.9916648104524686},{"x":1.8000000000000005,"y":0.973847630878195},{"x":1.9000000000000006,"y":0.9463000876874144},{"x":2.0000000000000004,"y":0.9092974268256815},{"x":2.1000000000000005,"y":0.8632093666488735},{"x":2.2000000000000006,"y":0.8084964038195899},{"x":2.3000000000000007,"y":0.7457052121767197},{"x":2.400000000000001,"y":0.6754631805511504},{"x":2.500000000000001,"y":0.5984721441039558},{"x":2.600000000000001,"y":0.5155013718214634},{"x":2.700000000000001,"y":0.42737988023382895},{"x":2.800000000000001,"y":0.33498815015590383},{"x":2.9000000000000012,"y":0.23924932921398112},{"x":3.0000000000000013,"y":0.1411200080598659},{"x":3.1000000000000014,"y":0.04158066243328916},{"x":3.2000000000000015,"y":-0.05837414342758142},{"x":3.3000000000000016,"y":-0.15774569414324996},{"x":3.4000000000000017,"y":-0.25554110202683294},{"x":3.5000000000000018,"y":-0.3507832276896215},{"x":3.600000000000002,"y":-0.44252044329485407},{"x":3.700000000000002,"y":-0.5298361409084948},{"x":3.800000000000002,"y":-0.6118578909427207},{"x":3.900000000000002,"y":-0.6877661591839753},{"x":4.000000000000002,"y":-0.7568024953079294},{"x":4.100000000000001,"y":-0.8182771110644114},{"x":4.200000000000001,"y":-0.8715757724135886},{"x":4.300000000000001,"y":-0.9161659367494552},{"x":4.4,"y":-0.9516020738895161},{"x":4.5,"y":-0.977530117665097},{"x":4.6,"y":-0.9936910036334644},{"x":4.699999999999999,"y":-0.9999232575641008},{"x":4.799999999999999,"y":-0.9961646088358408},{"x":4.899999999999999,"y":-0.9824526126243328},{"x":4.999999999999998,"y":-0.958924274663139},{"x":5.099999999999998,"y":-0.9258146823277331},{"x":5.1999999999999975,"y":-0.8834546557201545},{"x":5.299999999999997,"y":-0.8322674422239027},{"x":5.399999999999997,"y":-0.7727644875559894},{"x":5.4999999999999964,"y":-0.7055403255703945},{"x":5.599999999999996,"y":-0.6312666378723244},{"x":5.699999999999996,"y":-0.5506855425976414},{"x":5.799999999999995,"y":-0.4646021794137613},{"x":5.899999999999995,"y":-0.37387666483024096},{"x":5.999999999999995,"y":-0.27941549819893097},{"x":6.099999999999994,"y":-0.18216250427210112},{"x":6.199999999999994,"y":-0.0830894028175026},{"x":6.299999999999994,"y":0.016813900484343496},{"x":6.399999999999993,"y":0.11654920485048659},{"x":6.499999999999993,"y":0.21511998808780858},{"x":6.5999999999999925,"y":0.3115413635133711},{"x":6.699999999999992,"y":0.40484992061659103},{"x":6.799999999999992,"y":0.4941133511386012},{"x":6.8999999999999915,"y":0.5784397643881929},{"x":6.999999999999991,"y":0.6569865987187824},{"x":7.099999999999991,"y":0.7289690401258698},{"x":7.19999999999999,"y":0.7936678638491472},{"x":7.29999999999999,"y":0.8504366206285593},{"x":7.39999999999999,"y":0.8987080958116223},{"x":7.499999999999989,"y":0.9379999767747351},{"x":7.599999999999989,"y":0.9679196720314837},{"x":7.699999999999989,"y":0.9881682338769986},{"x":7.799999999999988,"y":0.9985433453746043},{"x":7.899999999999988,"y":0.9989413418397726},{"x":7.999999999999988,"y":0.9893582466233836},{"x":8.099999999999987,"y":0.9698898108450894},{"x":8.199999999999987,"y":0.9407305566797773},{"x":8.299999999999986,"y":0.9021718337562995},{"x":8.399999999999986,"y":0.8545989080882879},{"x":8.499999999999986,"y":0.7984871126234988},{"x":8.599999999999985,"y":0.734397097874123},{"x":8.699999999999985,"y":0.662969230082194},{"x":8.799999999999985,"y":0.5849171928917747},{"x":8.899999999999984,"y":0.5010208564578985},{"x":8.999999999999984,"y":0.41211848524177114},{"x":9.099999999999984,"y":0.3190983623493673},{"x":9.199999999999983,"y":0.22288991410026324},{"x":9.299999999999983,"y":0.12445442350707933},{"x":9.399999999999983,"y":0.024775425453375525}]},{"name":"55822f5a-af0e-4a38-808f-0ef7925176d6","values":[{"x":0,"y":0.3965540870630395},{"x":0.094200000166893,"y":0.3965540870630395},{"x":0.188400000333786,"y":0.3965540870630395},{"x":0.282600000500679,"y":0.3965540870630395},{"x":0.376800000667572,"y":0.3965540870630395},{"x":0.471000000834465,"y":0.3965540870630395},{"x":0.565200001001358,"y":0.3965540870630395},{"x":0.659400001168251,"y":0.3965540870630395},{"x":0.753600001335144,"y":0.3965540870630395},{"x":0.847800001502037,"y":0.3965540870630395},{"x":0.94200000166893,"y":0.3965540870630395},{"x":1.036200001835823,"y":0.3965540870630395},{"x":1.130400002002716,"y":0.3965540870630395},{"x":1.224600002169609,"y":0.3965540870630395},{"x":1.318800002336502,"y":0.3965540870630395},{"x":1.413000002503395,"y":0.3965540870630395},{"x":1.507200002670288,"y":0.3965540870630395},{"x":1.601400002837181,"y":0.3965540870630395},{"x":1.695600003004074,"y":0.3965540870630395},{"x":1.789800003170967,"y":0.3965540870630395},{"x":1.88400000333786,"y":0.3965540870630395},{"x":1.9782000035047531,"y":0.3965540870630395},{"x":2.072400003671646,"y":0.3965540870630395},{"x":2.166600003838539,"y":0.3965540870630395},{"x":2.260800004005432,"y":0.3965540870630395},{"x":2.355000004172325,"y":0.3965540870630395},{"x":2.449200004339218,"y":0.3965540870630395},{"x":2.543400004506111,"y":0.3965540870630395},{"x":2.637600004673004,"y":0.3965540870630395},{"x":2.731800004839897,"y":0.3965540870630395},{"x":2.82600000500679,"y":0.3965540870630395},{"x":2.920200005173683,"y":0.3965540870630395},{"x":3.014400005340576,"y":0.3965540870630395},{"x":3.108600005507469,"y":0.3965540870630395},{"x":3.202800005674362,"y":0.3965540870630395},{"x":3.297000005841255,"y":0.3965540870630395},{"x":3.391200006008148,"y":0.3965540870630395},{"x":3.485400006175041,"y":0.3965540870630395},{"x":3.579600006341934,"y":0.3965540870630395},{"x":3.673800006508827,"y":0.3965540870630395},{"x":3.76800000667572,"y":0.3965540870630395},{"x":3.862200006842613,"y":0.3965540870630395},{"x":3.9564000070095062,"y":0.3965540870630395},{"x":4.050600007176399,"y":0.3965540870630395},{"x":4.144800007343292,"y":0.3965540870630395},{"x":4.239000007510185,"y":0.3965540870630395},{"x":4.333200007677078,"y":0.3965540870630395},{"x":4.427400007843971,"y":0.3965540870630395},{"x":4.521600008010864,"y":0.3965540870630395},{"x":4.615800008177757,"y":0.3965540870630395},{"x":4.71000000834465,"y":0.3965540870630395},{"x":4.804200008511543,"y":0.3965540870630395},{"x":4.898400008678436,"y":0.3965540870630395},{"x":4.992600008845329,"y":0.3965540870630395},{"x":5.086800009012222,"y":0.3965540870630395},{"x":5.181000009179115,"y":0.3965540870630395},{"x":5.275200009346008,"y":0.3965540870630395},{"x":5.369400009512901,"y":0.3965540870630395},{"x":5.463600009679794,"y":0.3965540870630395},{"x":5.557800009846687,"y":0.3965540870630395},{"x":5.65200001001358,"y":0.3965540870630395},{"x":5.746200010180473,"y":0.3965540870630395},{"x":5.840400010347366,"y":0.3965540870630395},{"x":5.934600010514259,"y":0.3965540870630395},{"x":6.028800010681152,"y":0.3965540870630395},{"x":6.123000010848045,"y":0.3965540870630395},{"x":6.217200011014938,"y":0.3965540870630395},{"x":6.311400011181831,"y":0.3965540870630395},{"x":6.405600011348724,"y":0.3965540870630395},{"x":6.499800011515617,"y":0.3965540870630395},{"x":6.59400001168251,"y":0.3965540870630395},{"x":6.688200011849403,"y":0.3965540870630395},{"x":6.782400012016296,"y":0.3965540870630395},{"x":6.876600012183189,"y":0.3965540870630395},{"x":6.970800012350082,"y":0.3965540870630395},{"x":7.065000012516975,"y":0.3965540870630395},{"x":7.159200012683868,"y":0.3965540870630395},{"x":7.253400012850761,"y":0.3965540870630395},{"x":7.347600013017654,"y":0.3965540870630395},{"x":7.441800013184547,"y":0.3965540870630395},{"x":7.53600001335144,"y":0.3965540870630395},{"x":7.630200013518333,"y":0.3965540870630395},{"x":7.724400013685226,"y":0.3965540870630395},{"x":7.8186000138521194,"y":0.3965540870630395},{"x":7.9128000140190125,"y":0.3965540870630395},{"x":8.007000014185905,"y":0.3965540870630395},{"x":8.101200014352798,"y":0.3965540870630395},{"x":8.195400014519691,"y":0.3965540870630395},{"x":8.289600014686584,"y":0.3965540870630395},{"x":8.383800014853477,"y":0.3965540870630395},{"x":8.47800001502037,"y":0.3965540870630395},{"x":8.572200015187263,"y":0.3965540870630395},{"x":8.666400015354156,"y":0.3965540870630395},{"x":8.76060001552105,"y":0.3965540870630395},{"x":8.854800015687943,"y":0.3965540870630395},{"x":8.949000015854836,"y":0.3965540870630395},{"x":9.043200016021729,"y":0.3965540870630395},{"x":9.137400016188622,"y":0.3965540870630395},{"x":9.231600016355515,"y":0.3965540870630395},{"x":9.325800016522408,"y":0.3965540870630395}]}],"marks":[{"type":"symbol","from":{"data":"2cd17667-bb33-46cc-bdc8-2a9e28781346"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"line","from":{"data":"55822f5a-af0e-4a38-808f-0ef7925176d6"},"properties":{"enter":{"x":{"field":"data.x","scale":"x"},"y":{"field":"data.y","scale":"y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2cd17667-bb33-46cc-bdc8-2a9e28781346\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2cd17667-bb33-46cc-bdc8-2a9e28781346\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"2cd17667-bb33-46cc-bdc8-2a9e28781346\", :values ({:x 0, :y 0.0} {:x 0.1, :y 0.09983341664682815} {:x 0.2, :y 0.19866933079506122} {:x 0.30000000000000004, :y 0.2955202066613396} {:x 0.4, :y 0.3894183423086505} {:x 0.5, :y 0.479425538604203} {:x 0.6, :y 0.5646424733950354} {:x 0.7, :y 0.644217687237691} {:x 0.7999999999999999, :y 0.7173560908995227} {:x 0.8999999999999999, :y 0.7833269096274833} {:x 0.9999999999999999, :y 0.8414709848078964} {:x 1.0999999999999999, :y 0.8912073600614353} {:x 1.2, :y 0.9320390859672263} {:x 1.3, :y 0.963558185417193} {:x 1.4000000000000001, :y 0.9854497299884603} {:x 1.5000000000000002, :y 0.9974949866040544} {:x 1.6000000000000003, :y 0.9995736030415051} {:x 1.7000000000000004, :y 0.9916648104524686} {:x 1.8000000000000005, :y 0.973847630878195} {:x 1.9000000000000006, :y 0.9463000876874144} {:x 2.0000000000000004, :y 0.9092974268256815} {:x 2.1000000000000005, :y 0.8632093666488735} {:x 2.2000000000000006, :y 0.8084964038195899} {:x 2.3000000000000007, :y 0.7457052121767197} {:x 2.400000000000001, :y 0.6754631805511504} {:x 2.500000000000001, :y 0.5984721441039558} {:x 2.600000000000001, :y 0.5155013718214634} {:x 2.700000000000001, :y 0.42737988023382895} {:x 2.800000000000001, :y 0.33498815015590383} {:x 2.9000000000000012, :y 0.23924932921398112} {:x 3.0000000000000013, :y 0.1411200080598659} {:x 3.1000000000000014, :y 0.04158066243328916} {:x 3.2000000000000015, :y -0.05837414342758142} {:x 3.3000000000000016, :y -0.15774569414324996} {:x 3.4000000000000017, :y -0.25554110202683294} {:x 3.5000000000000018, :y -0.3507832276896215} {:x 3.600000000000002, :y -0.44252044329485407} {:x 3.700000000000002, :y -0.5298361409084948} {:x 3.800000000000002, :y -0.6118578909427207} {:x 3.900000000000002, :y -0.6877661591839753} {:x 4.000000000000002, :y -0.7568024953079294} {:x 4.100000000000001, :y -0.8182771110644114} {:x 4.200000000000001, :y -0.8715757724135886} {:x 4.300000000000001, :y -0.9161659367494552} {:x 4.4, :y -0.9516020738895161} {:x 4.5, :y -0.977530117665097} {:x 4.6, :y -0.9936910036334644} {:x 4.699999999999999, :y -0.9999232575641008} {:x 4.799999999999999, :y -0.9961646088358408} {:x 4.899999999999999, :y -0.9824526126243328} {:x 4.999999999999998, :y -0.958924274663139} {:x 5.099999999999998, :y -0.9258146823277331} {:x 5.1999999999999975, :y -0.8834546557201545} {:x 5.299999999999997, :y -0.8322674422239027} {:x 5.399999999999997, :y -0.7727644875559894} {:x 5.4999999999999964, :y -0.7055403255703945} {:x 5.599999999999996, :y -0.6312666378723244} {:x 5.699999999999996, :y -0.5506855425976414} {:x 5.799999999999995, :y -0.4646021794137613} {:x 5.899999999999995, :y -0.37387666483024096} {:x 5.999999999999995, :y -0.27941549819893097} {:x 6.099999999999994, :y -0.18216250427210112} {:x 6.199999999999994, :y -0.0830894028175026} {:x 6.299999999999994, :y 0.016813900484343496} {:x 6.399999999999993, :y 0.11654920485048659} {:x 6.499999999999993, :y 0.21511998808780858} {:x 6.5999999999999925, :y 0.3115413635133711} {:x 6.699999999999992, :y 0.40484992061659103} {:x 6.799999999999992, :y 0.4941133511386012} {:x 6.8999999999999915, :y 0.5784397643881929} {:x 6.999999999999991, :y 0.6569865987187824} {:x 7.099999999999991, :y 0.7289690401258698} {:x 7.19999999999999, :y 0.7936678638491472} {:x 7.29999999999999, :y 0.8504366206285593} {:x 7.39999999999999, :y 0.8987080958116223} {:x 7.499999999999989, :y 0.9379999767747351} {:x 7.599999999999989, :y 0.9679196720314837} {:x 7.699999999999989, :y 0.9881682338769986} {:x 7.799999999999988, :y 0.9985433453746043} {:x 7.899999999999988, :y 0.9989413418397726} {:x 7.999999999999988, :y 0.9893582466233836} {:x 8.099999999999987, :y 0.9698898108450894} {:x 8.199999999999987, :y 0.9407305566797773} {:x 8.299999999999986, :y 0.9021718337562995} {:x 8.399999999999986, :y 0.8545989080882879} {:x 8.499999999999986, :y 0.7984871126234988} {:x 8.599999999999985, :y 0.734397097874123} {:x 8.699999999999985, :y 0.662969230082194} {:x 8.799999999999985, :y 0.5849171928917747} {:x 8.899999999999984, :y 0.5010208564578985} {:x 8.999999999999984, :y 0.41211848524177114} {:x 9.099999999999984, :y 0.3190983623493673} {:x 9.199999999999983, :y 0.22288991410026324} {:x 9.299999999999983, :y 0.12445442350707933} {:x 9.399999999999983, :y 0.024775425453375525})} {:name \"55822f5a-af0e-4a38-808f-0ef7925176d6\", :values ({:x 0, :y 0.3965540870630395} {:x 0.094200000166893, :y 0.3965540870630395} {:x 0.188400000333786, :y 0.3965540870630395} {:x 0.282600000500679, :y 0.3965540870630395} {:x 0.376800000667572, :y 0.3965540870630395} {:x 0.471000000834465, :y 0.3965540870630395} {:x 0.565200001001358, :y 0.3965540870630395} {:x 0.659400001168251, :y 0.3965540870630395} {:x 0.753600001335144, :y 0.3965540870630395} {:x 0.847800001502037, :y 0.3965540870630395} {:x 0.94200000166893, :y 0.3965540870630395} {:x 1.036200001835823, :y 0.3965540870630395} {:x 1.130400002002716, :y 0.3965540870630395} {:x 1.224600002169609, :y 0.3965540870630395} {:x 1.318800002336502, :y 0.3965540870630395} {:x 1.413000002503395, :y 0.3965540870630395} {:x 1.507200002670288, :y 0.3965540870630395} {:x 1.601400002837181, :y 0.3965540870630395} {:x 1.695600003004074, :y 0.3965540870630395} {:x 1.789800003170967, :y 0.3965540870630395} {:x 1.88400000333786, :y 0.3965540870630395} {:x 1.9782000035047531, :y 0.3965540870630395} {:x 2.072400003671646, :y 0.3965540870630395} {:x 2.166600003838539, :y 0.3965540870630395} {:x 2.260800004005432, :y 0.3965540870630395} {:x 2.355000004172325, :y 0.3965540870630395} {:x 2.449200004339218, :y 0.3965540870630395} {:x 2.543400004506111, :y 0.3965540870630395} {:x 2.637600004673004, :y 0.3965540870630395} {:x 2.731800004839897, :y 0.3965540870630395} {:x 2.82600000500679, :y 0.3965540870630395} {:x 2.920200005173683, :y 0.3965540870630395} {:x 3.014400005340576, :y 0.3965540870630395} {:x 3.108600005507469, :y 0.3965540870630395} {:x 3.202800005674362, :y 0.3965540870630395} {:x 3.297000005841255, :y 0.3965540870630395} {:x 3.391200006008148, :y 0.3965540870630395} {:x 3.485400006175041, :y 0.3965540870630395} {:x 3.579600006341934, :y 0.3965540870630395} {:x 3.673800006508827, :y 0.3965540870630395} {:x 3.76800000667572, :y 0.3965540870630395} {:x 3.862200006842613, :y 0.3965540870630395} {:x 3.9564000070095062, :y 0.3965540870630395} {:x 4.050600007176399, :y 0.3965540870630395} {:x 4.144800007343292, :y 0.3965540870630395} {:x 4.239000007510185, :y 0.3965540870630395} {:x 4.333200007677078, :y 0.3965540870630395} {:x 4.427400007843971, :y 0.3965540870630395} {:x 4.521600008010864, :y 0.3965540870630395} {:x 4.615800008177757, :y 0.3965540870630395} {:x 4.71000000834465, :y 0.3965540870630395} {:x 4.804200008511543, :y 0.3965540870630395} {:x 4.898400008678436, :y 0.3965540870630395} {:x 4.992600008845329, :y 0.3965540870630395} {:x 5.086800009012222, :y 0.3965540870630395} {:x 5.181000009179115, :y 0.3965540870630395} {:x 5.275200009346008, :y 0.3965540870630395} {:x 5.369400009512901, :y 0.3965540870630395} {:x 5.463600009679794, :y 0.3965540870630395} {:x 5.557800009846687, :y 0.3965540870630395} {:x 5.65200001001358, :y 0.3965540870630395} {:x 5.746200010180473, :y 0.3965540870630395} {:x 5.840400010347366, :y 0.3965540870630395} {:x 5.934600010514259, :y 0.3965540870630395} {:x 6.028800010681152, :y 0.3965540870630395} {:x 6.123000010848045, :y 0.3965540870630395} {:x 6.217200011014938, :y 0.3965540870630395} {:x 6.311400011181831, :y 0.3965540870630395} {:x 6.405600011348724, :y 0.3965540870630395} {:x 6.499800011515617, :y 0.3965540870630395} {:x 6.59400001168251, :y 0.3965540870630395} {:x 6.688200011849403, :y 0.3965540870630395} {:x 6.782400012016296, :y 0.3965540870630395} {:x 6.876600012183189, :y 0.3965540870630395} {:x 6.970800012350082, :y 0.3965540870630395} {:x 7.065000012516975, :y 0.3965540870630395} {:x 7.159200012683868, :y 0.3965540870630395} {:x 7.253400012850761, :y 0.3965540870630395} {:x 7.347600013017654, :y 0.3965540870630395} {:x 7.441800013184547, :y 0.3965540870630395} {:x 7.53600001335144, :y 0.3965540870630395} {:x 7.630200013518333, :y 0.3965540870630395} {:x 7.724400013685226, :y 0.3965540870630395} {:x 7.8186000138521194, :y 0.3965540870630395} {:x 7.9128000140190125, :y 0.3965540870630395} {:x 8.007000014185905, :y 0.3965540870630395} {:x 8.101200014352798, :y 0.3965540870630395} {:x 8.195400014519691, :y 0.3965540870630395} {:x 8.289600014686584, :y 0.3965540870630395} {:x 8.383800014853477, :y 0.3965540870630395} {:x 8.47800001502037, :y 0.3965540870630395} {:x 8.572200015187263, :y 0.3965540870630395} {:x 8.666400015354156, :y 0.3965540870630395} {:x 8.76060001552105, :y 0.3965540870630395} {:x 8.854800015687943, :y 0.3965540870630395} {:x 8.949000015854836, :y 0.3965540870630395} {:x 9.043200016021729, :y 0.3965540870630395} {:x 9.137400016188622, :y 0.3965540870630395} {:x 9.231600016355515, :y 0.3965540870630395} {:x 9.325800016522408, :y 0.3965540870630395})}), :marks ({:type \"symbol\", :from {:data \"2cd17667-bb33-46cc-bdc8-2a9e28781346\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"line\", :from {:data \"55822f5a-af0e-4a38-808f-0ef7925176d6\"}, :properties {:enter {:x {:field \"data.x\", :scale \"x\"}, :y {:field \"data.y\", :scale \"y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; Pretty good! If we look at it, we see (with a bit of thinking) that it's found exactly the right function, in a slightly unorthodox form.
;; **

;; @@
(pprint/pprint (:expr (last bests)))

;; @@
;; ->
;;; 0.3965540870630395
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; # Darwin
;; **

;; **
;;; Immitating the Darwin worksheet 
;;; 
;; **

;; **
;;; **dwin-score**
;; **

;; @@
(defn dwin-score[ex] (score data ex))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-score</span>","value":"#'joel-darwin/dwin-score"}
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

;; **
;;; **dwin-mutate**
;; **

;; @@
(defn dwin-mutate [ex] (mutate-expr ex (random-full-tree functions terminals 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-mutate</span>","value":"#'joel-darwin/dwin-mutate"}
;; <=

;; **
;;; **dwin-new**
;; **

;; @@
(defn dwin-new [] (random-full-tree functions terminals 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;joel-darwin/dwin-new</span>","value":"#'joel-darwin/dwin-new"}
;; <=

;; @@
(dwin-new)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-double'>0.19100906549521557</span>","value":"0.19100906549521557"},{"type":"html","content":"<span class='clj-double'>0.1125733601719332</span>","value":"0.1125733601719332"}],"value":"(pdiv 0.19100906549521557 0.1125733601719332)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"},{"type":"html","content":"<span class='clj-double'>0.9676603606942065</span>","value":"0.9676603606942065"}],"value":"(+ x 0.9676603606942065)"}],"value":"(* (pdiv 0.19100906549521557 0.1125733601719332) (+ x 0.9676603606942065))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>+</span>","value":"+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>pdiv</span>","value":"pdiv"},{"type":"html","content":"<span class='clj-double'>0.5669489187782056</span>","value":"0.5669489187782056"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(pdiv 0.5669489187782056 x)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>*</span>","value":"*"},{"type":"html","content":"<span class='clj-double'>0.29505786799265366</span>","value":"0.29505786799265366"},{"type":"html","content":"<span class='clj-symbol'>x</span>","value":"x"}],"value":"(* 0.29505786799265366 x)"}],"value":"(+ (pdiv 0.5669489187782056 x) (* 0.29505786799265366 x))"}],"value":"(* (* (pdiv 0.19100906549521557 0.1125733601719332) (+ x 0.9676603606942065)) (+ (pdiv 0.5669489187782056 x) (* 0.29505786799265366 x)))"}
;; <=

;; @@
(def result (darwin/evolve dwin-score dwin-cross dwin-mutate dwin-new 500))
;; @@

;; @@

;; @@
