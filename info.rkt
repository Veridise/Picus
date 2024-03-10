#lang info

(define deps '("base"
               "rosette"
               "csv-reading"
               "graph"))

(define compile-omit-paths
  '("picus-cex-uniqueness.rkt"
    "picus/algorithms/cex0.rkt"
    "picus/algorithms/lemmas/copy-lemma.rkt"))
