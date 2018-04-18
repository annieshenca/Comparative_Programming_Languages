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
        (log10_2 0.301029995663981195213738894724493026768189881)
        ;(sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        ;(div     ,(lambda (x y) (floor (/ x y))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
    )
)

; Evaluate the expression by using loop that goes through each line
; of the file and treats the file as a huge link list.
(define (evalExpression expr)
    ; Symbol is an atomic value that prints like identifier preceded with <'>.
    (if (symbol? expr)
        ; true, expr is an symbol.
        ; then if the hash table <variable> already has the current expr.
        (if (hash-has-key? *variable* expr)
            (hash-ref *variable* expr) ; true
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
        )
    )
)

;
(define (setNewLabel line)
    ; Check if the cdr of line is Null, if so, set and return void
    (cond ((null? (cdr line))
            (void))
        ; Is the cadr of line a pair? If so, return void
        ((pair? (cadr line))
            (void))
        (else
        ; If neither the above, add cadr of line into label hash table
            (hash-set! *label* (cadr line) line)))
)

; Prints out hash table.
(define (print-hash hash)
    (map (lambda (key) (printf "key:~s value:~s~n" key (hash-ref hash key))) (hash-keys hash))
)

; Check if the list being passed in has an existed label in our label hash table.
; (define (hasLabel list)
;     (hash-has-key? *label* (cadr list))
; )
;
; ;
; (define (if-statement stmt linenum)
;
; )
;
; ;
; (define (print-stmt stmt)
;
; )
;
; ;
; (define (evalStmt stmt)
;
; )

; Function Start determines what each line/list of the file do.
(define (stepThrough linenum file)
    ; Check if current line is Null
    (if (null? file)
        ; If true, file is finish reading
        ((printf "Finish reading the file!~n") (exit 1))
        ; If false, continue.
        (void)
    )

    (define line (car file)) ; line = top list in file
    (define rest (cdr file)) ; rest = rest of the lists in file

    (printf "~s~n" line)

    (stepThrough (+ linenum 1) rest) ; Recurse on the rest of the lists in file.
)




(main (vector->list (current-command-line-arguments)))
