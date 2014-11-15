;Allen Aronis
;convert.scm
;Converts units to other units

;;; Function appears to evaluate correctly in all tests
;;; The only noted bug as of now is that it will still
;;; attempt conversions that are physically impossible
;;; such as converting from force to just speed or
;;; between multiple dimensions


;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.

(define source (with-input-from-file "units.dat" read-file))


;Simply evaluates the value to true or false
;It is my way to define base units


(define (is-base? x)
  (cond ((equal? x 'm)
         #t)
        ((equal? x 'sec)
         #t)
        ((equal? x 'kg)
         #t)
        (else #f)))



(define (convert from to)
  (define var (car from))
  (define normalized_from (remove-paren (convert-to-base (cdr from))))
  (define normalized_to (remove-paren (convert-to-base to)))
  (define mult (combine-mults normalized_from 1))
  (define divisor (combine-mults normalized_to 1))
  (define dividend (mult-elem (list mult (no-numbers normalized_from)) var))
  (cons (divide-units dividend divisor) to))
 

;Function evaluates to a list of just (base exponent)
;Used by the parent procedure to cons it with just
;the multiplie
(define (no-numbers unitlist)
   (cond ((null? unitlist)
           unitlist)
          ((number? (car unitlist))
           (no-numbers (cdr unitlist)))
          (else (cons (car unitlist)
                      (no-numbers (cdr unitlist))))))


;function goes through and multiplies the base unit values
;Provides the multiplier to be cons'd with just the 
;list of (base exponent) pairs
(define (combine-mults unitlist mult)
  (cond ((null? unitlist)
         mult)
        ((number? (car unitlist))
         (combine-mults (cdr unitlist) (* (car unitlist) mult)))
        (else (combine-mults (cdr unitlist) mult))))
                  
               

;simple function to convert
;dividend/divisor
;if there is no dividend in (car divisor)
;then just use 1
(define (divide-units dividend divisor)
  (if (number?  divisor)
      (/ (car dividend) divisor)
      (/ (car dividend) 1)))

;Function iterates through, skipping if there is
;a number in the list of units to be converted to.
;Does a simple comparison, and if there is a pair that is
;not equal, then it is not a valid conversion
;(define (is-equal? list1 list2)
;  (cond ((null? list1)
;         list2)
;        ((null? list2)
;         list1)
;        ((number? (car list2))
;         (is-equal? list1 (cdr list2)))
;        ((equal? (car list1) (car list2))
 ;       (is-equal? (cdr list1) (cdr list2)))
 ;       (else #f)))
                


;function iterates through, and if it finds a number and
;multiplies it 

(define (mult-elem unitlist var)
  (if (null? unitlist) unitlist)
  (if (number? (car unitlist))
      (cons (* (car unitlist) var) (cdr unitlist))
      (cdr unitlist)))
               




;Convert-to-base would return lists with an extra ()
;this function simply rebuilds the list without it

(define (remove-paren inlist)
  (cond ((null? inlist) '())
        ((list? (car inlist))
         (if (number? (caar inlist))
             (append (car inlist) (remove-paren (cdr inlist)))
             (append (list (car inlist)) (remove-paren (cdr inlist)))))))

  
;Function creates a list of base-units from a more complex unit
;ie (joule 1)=(1 (kg 1) (m 2) (sec -2))

(define (convert-to-base unitlist)
  (cond ((null? unitlist) unitlist)
        ((is-base? (caar unitlist))
         (cons (car unitlist) (convert-to-base (cdr unitlist))))
        (else
         (cons (distribute-exponent (cadr (assoc (caar unitlist) source))(cadar
unitlist))
               (convert-to-base (cdr unitlist))))))

;Function goes through and applies the exponent to each of the elements
;in the list.
;ie (joule 2)=(1 (kg 2) (m 4) (sec -4))

(define (distribute-exponent unitlist n)
  (cond ((null? unitlist)
         unitlist)
        ((number? (car unitlist))
         (cons (expt (car unitlist) n)
               (distribute-exponent (cdr unitlist) n)))
        ((list? (car unitlist))
         (cons (list (caar unitlist) (* (cadar unitlist) n))
               (distribute-exponent (cdr unitlist) n)))))


;Broken function to merge exponents together
; (m 1)(m 2)(m 3)=(m 6)
;Function does not work

;(define (combine value element unitlist)
;  (cond ((null? unitlist)
;         unitlist);
;        ((null? element)
;         element)
;        ((number? element)
;         (cons element (combine 0 (cdr element) (cdr unitlist))))
;        ((or (null? (caar element)) (null? (cadar element)))
;         (display* 'nullelem)
;         element)
;        ((or (null? (caar unitlist)) (null? (cadar element)))
;          (display* 'nullunitlist)
;          unitlist)
;         ((equal? (caar element) (caar unitlist))
       ;   (display* (cadar element)(cadar unitlist))
;          (cons (car element)
;                (list (+ (cadar unitlist) (cadar element) value)
;                      (combine (+  (cadar unitlist) value) 
;                               element (cdr unitlist)))))
       
;         (else (cons element (combine 0 element (cdr unitlist))))))

;Helper function that returns
;(base-unit highest-exponent)
;This was built to help fix the mess that is evaluated by
;combine.  Ultimately it did not work either

;(define (highest base large unitlist)
 ; (cond   ((null? unitlist)
 ;          (list base large))
        ;  ((null? (cadr unitlist))
        ;   (list base large))
        ;  ((null? (caddr unitlist))
        ;   (list base large))
 ;          (if (null? (caddr unitlist))
 ;              (list base large)
 ;              (highest base large (caddr unitlist)))
 ;         ((> (cadr unitlist) large)
 ;          (highest base (cadr unitlist) (caddr unitlist)))
 ;          ((< (cadr unitlist) large)
 ;           (highest base large (caddr unitlist)))
 ;          ((equal? (cadr unitlist) large)
 ;           (highest base large (caddr unitlist)))
 ;          (else (list base large))))



;

; TESTS
(convert '(1 (furlong 1)) '((m 1)))
      ;   (201.168 (m 1))

(convert '(1 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
;         (0.0003720238095238096 (mi 1) (hr -1))

(convert '(1 (furlong 1)(fortnight -1)) '((mph 1)))
 ;        (0.0003720238095238096 (mph 1))

(convert '(1 (fortnight -1)(furlong 1)) '((mph 1)))
  ;       (0.0003720238095238096 (mph 1))

(convert '(3 (lbm 1)(furlong 2)(min -2)) '((joule 1)(sec 2)))
   ;      Error: units are not compatible

(convert '(3 (lbm 1)(furlong 2)(min -2)) '((BTU 1)))
    ;     (0.01449862579696719 (btu 1))

(convert '(3 (lbm 2)(furlong 2)(min -5)(furlong 2)(min 1))
                  '((BTU -1)(BTU 4)(BTU -1)))
     ;    (7.007005000016083e-05 (btu -1) (btu 4) (btu -1))
      ;   or
       ;  (7.007005000016083e-05 (btu 2))

(convert '(1 (bushel 1)) '((peck 1)))
      ;   (4.0 (peck 1)

(convert '(1 (watt 1)) '((kg 1)(m 2)(sec -3)))
       ;  (1.0 (kg 1) (m 2) (sec -3))

(convert '(1 (hp 1)) '((ft 1)(lbf 1)(min -1)))
        ; (32999.99998332519 (ft 1) (lbf 1) (min -1))

(convert '(1 (cord 1)) '((ft 3)))
;         (127.9999998666517 (ft 3))
         

(convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
 ;        (0.01023065476190476 (mi 1) (hr -1))

(convert '(25 (BTU 2)(dyn -1)(m -2)) '((n 1)))
  ;       (2782857116548.055 (n 1))





