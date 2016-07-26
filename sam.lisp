;; 2016-07-05.142600+0200CEST.txt:(04:38:38 PM) rme: mrottenkolber: I fiddled
;; around with a crude userland-based sampling profiler a while ago.  I pasted
;; it as http://paste.lisp.org/display/319875 if you happen to feel like toying
;; with it.  It is crude: it just samples every millisecond, and records the
;; name of the function that the program counter is in.
(in-package :ccl)

(defun sample-process-loop (process hash)
  (loop
    (map-call-frames (lambda (p context)
                       (declare (ignore context))
                       (incf (gethash (cfp-lfun p) hash 0)))
                     :process process
                     :count 1)
    (sleep 0.001)))


(defun report-samples (h)
  (let ((results (make-array (hash-table-count h) :adjustable t
                             :fill-pointer 0)))
    (maphash (lambda (k v)
               (vector-push-extend (cons k v) results))
             h)
    (setq results (sort results #'> :key 'cdr))
    (dotimes (i (length results))
      (let ((item (aref results i)))
        (format t "~&~5d  ~s" (cdr item) (car item))))))

(defun call-with-sampling (fn)
  (let* ((h (make-hash-table))
         (sampling-process (process-run-function "sampler"
                                                  'sample-process-loop
                                                  *current-process*
                                                  h)))
    (unwind-protect
         (funcall fn)
      (process-kill sampling-process))
    h))
