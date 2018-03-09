#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  
;;

; Hash table
(define *symbol-table* (make-hash))
(define (symbol-put! key value)
    (hash-set! *symbol-table* key value))
(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (log10_2 0.301029995663981195213738894724493026768189881)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+ ,+) (- ,-) (* ,*) (/ ,/)
        (<> ,(lambda (x y) (not (= x y))))
        (<= ,<=) (>= ,>=) (= ,=) (> ,>) (< ,<)
        (abs ,abs) (^ ,expt) (exp ,exp)
        (sin ,sin) (cos ,cos) (tan ,tan)
        (asin ,asin) (acos ,acos) (atan ,atan)
        (ceil ,ceiling) (floor ,floor)
        (round ,round)(log ,log)(sqrt ,sqrt)
        )
    )
; function table
(define *function-table* (make-hash))
; label table
(define *label-table* (make-hash))
; kind value
(define (kind value)
    (cond
        ((string? value) value)
        ((number? value) (if (= value 0) 0.0 value))
        ((hash-has-key? *symbol-table* value)
            (hash-ref *symbol-table* value))
        ((list? value)
            (if (hash-has-key? *symbol-table* (car value))
                (let((first (hash-ref *symbol-table*
                    (car value))))
                    (cond
                        ((procedure? first)
                         (apply first (map (lambda (x)
                            (kind x)) (cdr value))))
                        ((vector? first) (vector-ref first
                            (cadr value)))
                        ((number? first) first)
                        (else
                            (die "Error: dead symbol-table."))))
                    (die (list "Error: "
                        (car value)
                        " not a symbol table.\n"))))))
; PRINT 
(define (sbi-print value)
    (map (lambda (x) (display (kind x))) value)
    (newline))
; DIM 
(define (sbi-dim value)
    (set! value (car value))
    (let((array (make-vector
        (kind (cadr value)) (car value))))
        (symbol-put! (car value)
            (+ (kind (cadr value)) 1))))
; LET 
(define (sbi-let value)
    (symbol-put! (car value) (kind (cadr value))))

(define (sbi-input2 value count)
    (if (null? value) count
        (let ((in (read)))
            (if (eof-object? in) -1
                (begin
                    (symbol-put! (car value) in)
                    (set! count (+ 1 count))
                    (sbi-input2 (cdr value) count))))))
; INPUT 
(define (sbi-input value)
    (symbol-put! 'count 0)
    (if (null? (car value))
        (symbol-put! 'count -1)
        (begin
        (symbol-put! 'count (sbi-input2 value 0)))))

; execute a line passed by function parse-line
(define (execute-line instruction program line-number)
    ; die if the key is invalid
    (when (not (hash-has-key? *function-table* (car instruction)))
        (die "~s is a invalid instruction." (car instruction)))
    (cond
        ((eq? (car instruction) 'goto)
         (parse-line program (hash-ref *label-table*
            (cadr instruction))))
        ((eq? (car instruction) 'if)
         (if (kind (car (cdr instruction)))
             (parse-line program (hash-ref *label-table*
                (cadr (cdr instruction))))
             (parse-line program (+ line-number 1))))
        ((eq? (car instruction) 'print)
         (if (null? (cdr instruction))
             (newline)
             (sbi-print (cdr instruction)))
             (parse-line program (+ line-number 1)))
        (else
             ((hash-ref *function-table* (car instruction))
              (cdr instruction))
             (parse-line program (+ line-number 1)))))


(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
        (if (not (input-port? inputfile))
            (die `(,*run-file* ": " ,filename ": open failed"))
            (let ((program (read inputfile)))
                (close-input-port inputfile)
                       program))))
; get length
(define length
    (lambda (l) (if (null? l) 0 (+ (length (cdr l)) 1))))
; parse line
(define (parse-line program line-number)
    (when (> (length program) line-number)
        (let((line (list-ref program line-number)))
        (cond
            ((= (length line) 3)
             (set! line (cddr line))
             (execute-line (car line) program line-number))
            ((and (= (length line) 2) (list? (cadr line)))
             (set! line (cdr line))
             (execute-line (car line) program line-number))
            (else
              (parse-line program (+ line-number 1)))))))
; store labels to hash-table
(define (store-labels program)
    (map (lambda (l)
         (when (not (null? l))
            (when (or (= 3 (length l))
                (and (= 2 (length l))
                    (not (list? (cadr l)))))
                    (hash-set! *label-table* (cadr l)
                               (- (car l) 1 ))
                 ))) program))
; main function
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
              (store-labels program)
              (parse-line program 0))))
(for-each
    (lambda (pair) (hash-set! *function-table* (car pair) (cadr pair)))
    `(
        (print ,sbi-print)
        (input ,sbi-input)
        (dim   ,sbi-dim)
        (let   ,sbi-let)
        (if    (void))
        (goto  (void))))
(main (vector->list (current-command-line-arguments)))
