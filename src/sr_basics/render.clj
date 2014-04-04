(ns sr-basics.render
  (:require [clojure.string :as str]
            [gorilla-renderable.core :as render]))

(defn latexify
  [expr]
  (if (seq? expr)
    (case (first expr)
      +  (str "(" (str/join " + " (map latexify (rest expr))) ")")
      -  (str "(" (str/join " - " (map latexify (rest expr))) ")")
      *  (str/join " \\cdot " (map latexify (rest expr)))
      pdiv (str "\\frac{" (latexify (first (rest expr))) " }{ " (latexify (second (rest expr))) "}")
      / (str "\\frac{" (latexify (first (rest expr))) " }{ " (latexify (second (rest expr))) "}")
      ** (str "(" (latexify (first (rest expr))) ")^{ " (latexify (second (rest expr))) "}"))
    (if (float? expr) (format "%.3f" expr) (pr-str expr))))

(defrecord ExprLatexView [expr])

(defn mathematician-view
  [expr]
  (ExprLatexView. expr))

(extend-type ExprLatexView
  render/Renderable
  (render [self]
    {:type :latex
     :content (latexify (:expr self))
     :value (pr-str self)}))