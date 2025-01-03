#lang sicp

;(define (list-of-values exps env)
; (if (no-operands? exps)
;      '()
;      (cons (eval (first-operand exp) env)
;            (list-of-values (rest-operands exp) env))))

;Evaluates operans from left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))
; Evaluates operands from right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-values))))
