#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;;
;; Annie Shen (ashen7) && Ruben Ayestas (rayestas)
;; CMPS 112 - Spring 2018
;; Asg1 - Functionally Scheme
;;

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
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))


;;
;; Added functionalities below
;;
;; To dos:
;; - set up hash table
;;    - function hash
;;    - variable hash
;;    - label hash
;; - do init scan of the test file looking for labels and store into hash (recur on cdr)
;;
;;
;;
;/
;; Recurse on cdr until null. "Goto" may take you back to the top of the code though!
;; Check if it's a pair by: (pari ? {list})


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























;;
