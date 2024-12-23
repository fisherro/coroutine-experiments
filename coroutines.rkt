#lang racket
(require racket/control) ; for abbreviations: e.g. call/prompt

;; I'm aiming towards being able to write something along the lines of...
;; (The implicit loop in the routine might be a bad idea.)
;
; routine iota(start increment) {
;     yield(start);
;     start <- start + increment;
; }
;
; routine main() {
;     i <- iota(1, 1);
;     loop {
;         println(i);
;         i <- iota();
;         if (i > 10) done;
;     }
; }

;; A "closer to Racket" version might be:
'(routine (iota start increment)
          (yield start)
          (set! start (+ start increment)))

'(do ((i (iota 1 1) (iota)))
   ((> i 10) (display i)(newline))
   (display i)(newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A generic test program
(define (test name proc)
  (displayln name)
  (do ((i (proc 1 1) (proc)))
    ((> i 10) (displayln i))
    (displayln i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Racket version w/o continuations:
(define iota
  (let ((current 0)
        (increment 0))
    (lambda args
      (if (= 0 (length args))
          (set! current (+ current increment))
          (begin
            (set! current (first args))
            (set! increment (second args))))
      current)))

(test "iota" iota)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First attempt at using continuations:

(define saved-k #f)
(define (iota2-body start increment)
  (let loop ((current start))
    (let ((k (call/cc (lambda (k) k))))
      (if (procedure? k)
          (begin
            (set! saved-k k)
            current)
          (loop (+ current increment))))))
(define (iota2 . args)
  (if saved-k
      (saved-k #f)
      (apply iota2-body args)))

(test "iota2" iota2)

;; Next: Capture the call/cc into a yield function
;;       Make saved-k local

;; We need the eqivalent of a "try" to let the yield escape the loop?
;; That's where a prompt would come into the picture?
;; Yep!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using call/prompt and abort/cc:

(define iota3
  (let ((yield-tag (make-continuation-prompt-tag))
        (resume-k #f))
    (lambda args
      (call-with-continuation-prompt ; aka call/prompt
       (lambda (args)
         (if resume-k
             (resume-k #f)
             (let ((start (first args))
                   (increment (second args)))
               (do () (#f)
                 (call-with-current-continuation ; aka call/cc
                  (lambda (k)
                    (set! resume-k k)
                    (abort-current-continuation ; aka abort/cc
                     yield-tag
                     start))
                  yield-tag)
                 (set! start (+ start increment))))))
       yield-tag
       (lambda (result)
         result)
       args))))

(test "iota3" iota3)

;; But I still haven't created a yield function
;; Should I use call/comp instead of call/cc?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yield is now a procedure:

(define iota4
  (let* ((yield-tag (make-continuation-prompt-tag))
         (resume-k #f)
         (yield (lambda (result)
                  (call/cc (lambda (k)
                             (set! resume-k k)
                             (abort/cc yield-tag result))
                           yield-tag))))
    (lambda args
      (call/prompt
       (lambda (args)
         (if resume-k
             (resume-k #f)
             (let ((start (first args))
                   (increment (second args)))
               (do () (#f)
                 (yield start)
                 (set! start (+ start increment))))))
       yield-tag
       (lambda (result)
         result)
       args))))

(test "iota4" iota4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Factor out the boilerplate:

(define (make-coroutine5 proc)
   (let* ((yield-tag (make-continuation-prompt-tag))
          (resume-k #f)
          (yield (lambda (result)
                   (call/cc (lambda (k)
                              (set! resume-k k)
                              (abort/cc yield-tag result))
                            yield-tag))))
     (lambda args
       (call/prompt
        (lambda (args)
          (if resume-k
              (resume-k #f)
              (apply proc (cons yield args))))
        yield-tag
        (lambda (result)
          result)
        args))))

(define (iota5-body yield start increment)
  (do () (#f)
    (yield start)
    (set! start (+ start increment))))

(test "iota5" (make-coroutine5 iota5-body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use call/comp instead of call/cc:

(define (make-coroutine6 proc)
   (let* ((yield-tag (make-continuation-prompt-tag))
          (resume-k #f)
          (yield (lambda (result)
                   (call/comp (lambda (k)
                              (set! resume-k k)
                              (abort/cc yield-tag result))
                            yield-tag))))
     (lambda args
       (call/prompt
        (lambda (args)
          (if resume-k
              (resume-k #f)
              (apply proc (cons yield args))))
        yield-tag
        (lambda (result)
          result)
        args))))

(define (iota6-body yield start increment)
  (do () (#f)
    (yield start)
    (set! start (+ start increment))))

(test "iota6" (make-coroutine6 iota6-body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make it possible to pass a value in on
;; subsequent calls and have yield return it.

(define (make-coroutine7 proc)
   (let* ((yield-tag (make-continuation-prompt-tag))
          (resume-k #f)
          (yield (lambda (result)
                   (call/comp (lambda (k)
                              (set! resume-k k)
                              (abort/cc yield-tag result))
                            yield-tag))))
     (lambda args
       (call/prompt
        (lambda (args)
          (if resume-k
              (apply resume-k args)
              (apply proc (cons yield args))))
        yield-tag
        (lambda (result)
          result)
        args))))

(define iota7
  (make-coroutine7 (lambda (yield start increment)
                     (do () (#f)
                       (yield start)
                       (set! start (+ start increment))))))

(define sum7 (make-coroutine7 (lambda (yield)
                               (let loop ((total 0))
                                 (loop (+ total (yield total)))))))

(do ((x (iota7 1 1) (iota7))
     (y (sum7) (sum7 x)))
  ((> x 10) (displayln "Done"))
  (printf "x: ~a; y: ~a\n" x y))