#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; Annie Shen (ashen7) && Ruben Ayestas (rayestas)
;; CMPS 112 - Spring 2018
;; Asg1 - Functionally Scheme

;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

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

(define (write-program-by-line filename program)
    ; (printf "==================================================~n")
    ; (printf "~a: ~s~n" *run-file* filename)
    ; (printf "==================================================~n")
    ; (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    ;(printf ")~n")
)

; Main passes in the commandline args and call it arglist here.
; The whole arg is interpreted as a giant list.
(define (main arglist)
    ; Check if the file passed in is null or empty.
    (if (or (null? arglist) (not (null? (cdr arglist))))
        ; if true:
        (usage-exit)
        ; if false:
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
              ;(write-program-by-line sbprogfile program)
               ; Start interpreting the file from line number 1.
               (stepThrough 1 program)))
)


;; Added functionalities below

;; To dos:
;; - do init scan of the test file looking for labels and store into hash (recur on cdr)
;;

;; Recurse on cdr until null. "Goto" may take you back to the top of the code though!
;; Check if it's a pair by: (pair ? {list})

;; Below are sudo codes from MSI
; (define (mytable make-hash))
;   (for-each
;     (lambda (pair)
;       (function-put? (car pair)(cdr pair))))

; (define (step-through program)
;   (if null? program) 0
;     (let line1 (car program))
;     (let rest (cdr program))
;     ;; check if I just evaluated a "goto"
;
;     ;; recurse through rest, which is the cdr
; )

; Set up three hash tables for labels, variables, functions.
; *function-table* Holds all functions, including operators.
(define *function-table* (make-hash))
; *label-table* Holds address of each line, one level above statements.
(define *label-table* (make-hash))
; *variable-table* Holds the value of all variables
(define *variable-table* (make-hash))
(define (symbol-get key) (hash-ref *function-table* key) )
(define (symbol-put! key value) (hash-set! *function-table* key value) )

; Store needed variables into hash table. Example from symbols.scm
(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (abs     ,abs)
        (sin     ,sin)
        (cos     ,cos)
        (tan     ,tan)
        (asin    ,asin)
        (acos    ,acos)
        (atan    ,atan)
        (sqrt    ,sqrt)
        (round   ,round)
        (trunc   ,truncate)
        (log     ,log)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log10_2 , 0.301029995663981195213738894724493026768189881)
        ;(sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       , 2.718281828459045235360287471352662497757247093)
        (pi      , 3.141592653589793238462643383279502884197169399)
        ;(div     ,(lambda (x y) (floor (/ x y))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
    )
)

; Put new labels into *label-table*
(define (setNewLabel line)
    (if (not (null? (cdr line)))
        ; If true, then check if there is no nill
        (if (not (pair? (cadr line)))
            (hash-set! *label-table* (cadr line) line)
            (void); If false, do nothing.
        )
        (void)
))

; Prints out hash table for testing purposes!
(define (printHash hash)
    (map (lambda (key) (printf "key:~s value:~s~n" key (hash-ref hash key))) (hash-keys hash))
)

; Check if the list being passed in has a label.
(define (lineHasLabel list)
    (hash-has-key? *label-table* (cadr list))
)

; ---------------------------------------------------------------------

; Evaluate the expression by using loop that goes through each line
; of the file and treats the file as a huge link list.
(define (evalExpression expr)
    ; Symbol is an atomic value that prints like identifier preceded with <'>.
    (if (symbol? expr)
        ; true, expr is an symbol.
        ; then if *variable-table* already has the current expr.
        (if (hash-has-key? *variable-table* expr)
            (hash-ref *variable-table* expr) ; true
            (printf "~s does not exist~n" expr)) ; false
        ; false, expr is NOT an symbol.
        (if (number? expr)
            ; true, expr is a number
            expr
            ; false, expr is NOT a number.
            (let (
                ; Store the symbol, which is the car of expr
                (op (symbol-get (car expr)))
                ; Store rest of the expression and loop on that
                (tail (cdr expr)))
            (apply op (map evalExpression tail)))
)))


; ---------------------------------------------------------------------


; When the statment of the line is using 'print', come here and be able to
; print out what the statement wanted.
(define (printInStatement s)
    ; (cond [(statement) (ONLY GET HERE IF STATEMENT IS TRUE)]) ; Move on if false
    (cond
        [ (null? (cdr s)) (printf "~n") ] ; If print "", print nothing
        [ (string? (cadr s))
            (if (null? (cddr s))
                ; If true, just print our the state
                (printf "~s" (cadr s))
                ; If false, need to calculate the mathatic statement
                (printf "~s~s" (cadr s) (evalExpression(cadr s)))
            )
            (printf "~n")
        ] ; END OF STRING?
        [else (printf "~s~n" (evalExpression(cadr s)))]
    ) ; END OF COND
)
; ---------------------------------------------------------------------
    ; (define (print-stmt stmt)
    ;     (cond
    ;        ((null? (cdr stmt)) (printf "~n"))
    ;        ((string? (cadr stmt))
    ;     (if (null? (cddr stmt))
    ;         (printf "~s" (cadr stmt))
    ;         (printf "~s~s" (cadr stmt) (eval-expr(caddr stmt)))
    ;         )
    ;         (printf "~n"))
    ;        (else (printf "~s~n" (eval-expr (cadr stmt))))
    ;     )
    ; )
; ---------------------------------------------------------------------

;
(define (hasPrintInSmt line)
    (if (null? (cdr line)) ; If there's no second == a line w/o label & stmt
        (void)
        (if (null? (cddr line)) ; If there's no third == a line w/o label
            (if (eqv? (car(car(cdr line))) 'print)
                (printInStatement(car(cdr line)))
                (void)
            )
            ; If false == a line w/ all three
            (if (eqv? (car(car(cddr line))) 'print)
                (printInStatement(car(cddr line)))
                (void)
            )
        )
    )
)

; Function Start determines what each line/list of the file do.
(define (stepThrough linenum file)
    ; Check if current line is Null
    (if (null? file)
        (exit 1) ; If true, file is finish reading
        (void) ; If false, continue.
    )

    ; line = top list in file.
    ; rest = rest of the lists in file.
    (let ( (line (car file)) (rest (cdr file)) )
        (
        ;(printf "line: ~s~n" line)

        ; Line has label or not, will go through this function.
        ; If line has label, add label into *label-table*,
        ; if not, do nothing!
        ;(setNewLabel line)
        ;(printHash *label-table*)

        ; If the line has statement wanting to print, check here and call
        ; the printInStatement function!
        (hasPrintInSmt line)

        ; Recurse on the rest of the lists in file.
        (stepThrough (+ linenum 1) rest))
    ) ; END OF LET
)



(main (vector->list (current-command-line-arguments)))
