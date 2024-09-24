(ns question-time.core
  (:gen-class)
  (:require [question-time.parsing :as parsing]
            [question-time.llm :as llm]
            [clj-wrap-indent.core :as wrap]))




(defn judge-questions [n data]
  (->> (parsing/questions data)
       (take n)
       (map (fn [q]
              (llm/judge-question (:plaintext q) :terminal)))))

(defn printall [strs]
  (doseq [s strs]
    (println)
    (println "----------------------------------------")
    (wrap/println s 80)))

(defn printss [judgements]
  (doseq [j judgements]
    (llm/print-judgement j)))


(defn print-judgments [path n]
  (->> (parsing/path->hansard-data path)
       (judge-questions n)
       (printss)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-judgments "resources/hansards/Senate_2024_02_06_Official.xml" 10)
  ;; (llm/test-q)
  )
