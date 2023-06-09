;;; -*- mode:lisp; coding:utf-8 -*-

;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(/debug "loading runner/runner.lisp!")

(defun now-ms ()
  ((jscl::oget (jscl::make-new #j:Date) "getTime")))

(defvar console-info (jscl::make-stream :write-fn #j:console:info))
(defvar console-error (jscl::make-stream :write-fn #j:console:error))

(defun compile-script (src script-source)
  (let ((in (make-string-input-stream script-source))
        (out (make-string-output-stream))
        (jscl::*compiling-file* t)
        (jscl::*compile-print-toplevels* nil)
        (jscl::*literal-table* nil)
        (jscl::*variable-counter* 0)
        (jscl::*literal-counter* 0)
        (start-ms (now-ms)))
    (format console-info "Compiling ~A...~%" src)
    (format out "(function(jscl){~%")
    (format out "'use strict';~%")
    (format out "(function(values, internals){~%")
    (loop with eof = (gensym)
          for x = (jscl::ls-read in nil eof)
          until (eq x eof)
          do (write-string (jscl::compile-toplevel x) out))
    (format out "})(jscl.internals.pv, jscl.internals);~%")
    (format out "})(window.jscl)~%")
    (format console-info "Compiled ~A in ~A ms" src (- (now-ms) start-ms))
    (get-output-stream-string out)))

(defun fetch-load (src)
  (let ((fetch-cfg (jscl::make-new #j:Object)))
    (setf (jscl::oget fetch-cfg "cache") "reload")
    ((jscl::oget (#j:fetch src fetch-cfg) "then")
     (lambda (resp)
       ((jscl::oget ((jscl::oget resp "text")) "then")
        (lambda (text)
          (let* ((js-string (jscl::lisp-to-js (compile-script src text)))
                 (func (make-new #j:Function js-string)))
            ((jscl::oget func "call")))))))))

(defun lisp-script? (child)
  (and
   (string= "SCRIPT" (jscl::oget child "nodeName"))
   (string= "text/lisp" (jscl::oget child "type"))))
  
(defun load-lisp-scripts ()
  (loop for child across #j:document:head:children
        when (lisp-script? child) 
          do (fetch-load (jscl::oget child "src"))))

(#j:window:addEventListener "DOMContentLoaded"
                            (lambda (&rest args)
                              (load-lisp-scripts)))

