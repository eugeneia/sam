;; 2016-07-05.142600+0200CEST.txt:(04:38:38 PM) rme: mrottenkolber: I fiddled
;; around with a crude userland-based sampling profiler a while ago.  I pasted
;; it as http://paste.lisp.org/display/319875 if you happen to feel like toying
;; with it.  It is crude: it just samples every millisecond, and records the
;; name of the function that the program counter is in.

(defpackage sam
  (:documentation
   "Tiny, concurrent sampling profiler for Clozure Common Lisp. Sam peeks into
    the stack of a running process repeatedly to figure out where time is
    spent.

    *Example:*

    #code#
    (sam:profile ()
      (loop for i from 1 to 1e8 sum i))
    ▷ 39%  CCL::>-2  <no source>
    ▷ 19%  CCL::FIXNUM-SFLOAT-COMPARE  <no source>
    ▷ 15%  #<Anonymous Function #x30200B10843F>  <no source>
    ▷ 12%  SAM:CALL-WITH-SAMPLING  (defun call-with-sampling (fn ...
    ▷ 11%  CCL::INTEGER-DECODE-SHORT-FLOAT  <no source>
    ▷  5%  CCL::FIXNUM-DECODE-SHORT-FLOAT  <no source>
    #

    *Credits:*

    The core functionality of Sam was kindly provided by R. Matthew Emerson.")
  (:use :cl :ccl)
  (:export :call-with-sampling
           :sample-process
           :report-samples
           :profile
           :profile-process))

(in-package :sam)

(defparameter *default-sample-interval* 0.001)

(defparameter *default-cutoff* 3)

(defun sample-process-loop (process hash interval)
  (loop
    (map-call-frames (lambda (p context)
                       (declare (ignore context))
                       (incf (gethash (ccl::cfp-lfun p) hash 0)))
                     :process process
                     :count 1)
    (sleep interval)))

(defun call-with-sampling (fn &key (interval *default-sample-interval*))
  "*Arguments and Values:*

   _fn_—a _function designator_.

   _interval_—a positive _number_ designating a time in seconds. The default is
   0.001.

   *Description:*

   {call-with-sampling} profiles _fn_, and returns a _hash table_ containing
   the collected samples. _Interval_ specifies the time between samples
   collected."
  (check-type interval (real 0 *))
  (let* ((h (make-hash-table))
         (sampling-process (process-run-function "sampler"
                                                  'sample-process-loop
                                                  *current-process*
                                                  h
                                                  interval)))
    (unwind-protect
         (funcall fn)
      (process-kill sampling-process))
    h))

(defun sample-process (process duration
                       &key (interval *default-sample-interval*))
  "*Arguments and Values:*

   _process_—a {process}.

   _duration_—a positive _number_ designating a time in seconds.

   _interval_—a positive _number_ designating a time in seconds. The default is
   0.001.

   *Description:*

   {sample-process} profiles _process_ for _duration_, and returns a _hash
   table_ containing the collected samples. _Interval_ is the time between
   samples collected."
  (check-type interval (real 0 *))
  (let* ((h (make-hash-table))
         (sampling-process (process-run-function "sampler"
                                                 'sample-process-loop
                                                 process
                                                 h
                                                 interval)))
    (unwind-protect (sleep duration)
      (process-kill sampling-process))
    h))

(defun percent (n total) (* 100 (/ n total)))

(defun format-source (text &key (truncate 30))
  (if text
      (format nil "~a~@[...~]"
              (ccl::string-sans-most-whitespace text truncate)
              (> (length text) truncate))
      "<no source>"))

(defun report-samples (h &key (cutoff *default-cutoff*))
  "*Arguments and Values:*

   _h_—a _hash table_ containing samples.

   _cutoff_—a _number_ of _type_ {(real 0 100)} designating a percentage. The
   default is 3.

   *Description:*

   {report-samples} prints a summary of the samples in _h_, omitting samples
   that constitute below _cutoff_ percent of the total samples."
  (check-type cutoff (real 0 100))
  (let* ((cf (/ cutoff 100))
         (total (loop for n being the hash-values in h sum n))
         (results (loop for f being the hash-keys in h using (hash-value n)
                     when (> (/ n total) cf) collect (cons f n))))
    (loop for (f . n) in (sort results #'> :key 'cdr) do
         (format t "~2d%  ~s  ~a~%"
                 (round (percent n total))
                 (or (function-name f) f)
                 (format-source (ignore-errors
                                  (source-note-text
                                   (function-source-note f)))))))
  (values))

(defmacro profile ((&key (interval *default-sample-interval*)
                         (cutoff *default-cutoff*))
                   &body forms)
  "*Arguments and Values:*

   _interval_—a positive _number_ designating a time in seconds. The default is
   0.001.

   _cutoff_—a _number_ of _type_ {(real 0 100)} designating a percentage. The
   default is 3.

   _forms_—_forms_ to be evaluated.

   *Description:*

   {profile} profiles the evaluation of _forms_ and prints a summary of the
   results, omitting samples that constitute below _cutoff_ percent of the
   total samples. _Interval_ specifies the time between samples collected."
  `(report-samples (call-with-sampling (lambda () ,@forms) :interval ,interval)
                   :cutoff ,cutoff))

(defun profile-process (process duration
                        &key (interval *default-sample-interval*)
                          (cutoff *default-cutoff*))
  "*Arguments and Values:*

   _process_—a {process}.

   _duration_—a positive _number_ designating a time in seconds.

   _interval_—a positive _number_ designating a time in seconds. The default is
   0.001.

   _cutoff_—a _number_ of _type_ {(real 0 100)} designating a percentage. The
   default is 3.

   *Description:*

   {profile-process} profiles _process_ for _duration_, and prints a summary of
   the results, omitting samples that constitute below _cutoff_ percent of the
   total samples. _Interval_ specifies the time between samples collected."
  (report-samples (sample-process process duration :interval interval)
                  :cutoff cutoff))
