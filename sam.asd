(defsystem sam
  :description
  "Tiny sampling profiler for Clozure Common Lisp. Sam peeks into the stack of
   a process repeatedly to figure out where time is spent."
  :author "Max Rottenkolber <max@mr.gy>"
  :license "MIT"
  :components ((:file "sam")))
