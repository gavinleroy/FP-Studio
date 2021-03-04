#lang racket/base
(require racket/set)

(provide unique-elements)

(define (unique-elements l)
  (set->list (list->set l)))
