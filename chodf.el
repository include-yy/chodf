;;; chodf.el --- cho45's jsdeferred implemented in emacs lisp -*- lexical-binding: t;-*-

;; Author: include-yy https://github.com/include-yy
;; Version: 0.1
;; Keywords: deferred, async
;; Package-Requires: ((emacs "28.2"))
;; URL: https://github.com/include-yy/chodf

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 'chodf.el' is a simple library for asynchronous tasks.
;; inspired by jsdeferred and deferred.el
;; [https://github.com/cho45/jsdeferred]
;; [https://github.com/kiwanami/emacs-deferred]

;; The API is almost the same as JSDeferred written by cho45. See the
;; JSDeferred and Mochikit.Async web sites for further documentations.
;; [https://github.com/cho45/jsdeferred]
;; [http://mochikit.com/doc/html/MochiKit/Async.html]

;; A good introduction document (JavaScript)
;; [http://cho45.stfuawsc.com/jsdeferred/doc/intro.en.html]
;; [https://cho45.stfuawsc.com/jsdeferred/]

(defun chodf-ok (x)
  "Default callback function"
  x)

(defun chodf-ng (x)
  "Default errorback function"
  (signal (car x) (cdr x)))

(cl-defstruct (chodf (:constructor chodf-new)
		     (:copier nil))
  "Data struct used to reprensent a deferred object."
  (okcb 'chodf-ok
	:documentation "the success callback")
  (ngcb 'chodf-ng
	:documentation "the fail callback")
  (n nil :documentation "points to the next deferred object or nil"))

(defun chodf--post (odf okng fun)
  "[internal] create a deferred object, make OBF points to it.
if OKNG is `:ok', then obj's OKCB is FUN, otherwise NGCB."
  (let ((new (chodf-new)))
    (pcase okng
      (:ok (setf (chodf-okcb new) fun))
      (:ng (setf (chodf-ngcb new) fun)))
    (setf (chodf-n odf) new)
    new))

(defun chodf-next (odf fun)
  "Create new deferred and sets FUN as its ok callback, \
then connect ODF to it"
  (chodf--post odf :ok fun))
(defun chodf-error (odf fun)
  "Create new deferred and sets FUN as its err callback, then connect ODF to it.
if FUN does not signal an error but just returns normal value,
deferred treats the give error is recovery and continue chain"
  (chodf--post odf :ng fun))
(defun chodf-ner (odf okfn ngfn)
  "Create new deferred and sets okcb to OKFN, ngcb to NGFN, then connect ODF to it.
this function doens't exist in jsdeferred, I add it just for fun :p"
  (let ((new (chodf-new)))
    (setf (chodf-okcb new) okfn)
    (setf (chodf-ngcb new) ngfn)
    (setf (chodf-n odf) new)
    new))

(defun chodf--fire (odf okng value)
  "[internal] Executing deferred callback chosen by OKNG.
OKNG can either be `:ok' or `:ng'. VALUE is arg for callback.
If an error is signaled by ok or ng function, and deferred's `n' exists, then\
the next deferred object's ngcb function will be called"
  (let ((next :ok))
    (condition-case err
	(pcase okng
	  (:ok (setq value (funcall (chodf-okcb odf) value)))
	  (:ng (setq value (funcall (chodf-ngcb odf) value)))
	  (_ (error "chodf--fire ~ unknown choice: `%s'" okng)))
      (error
       (setq next :ng)
       (setq value err)))
    (if (chodf-p value)
	(setf (chodf-n value) (chodf-n odf))
      (when (chodf-n odf)
	(chodf--fire (chodf-n odf) next value)))
    odf))

(defun chodf-call (odf val)
  "Invokes self callback chain."
  (chodf--fire odf :ok val))
(defun chodf-fail (odf val)
  "Invokes self errorback chain.
Use this function for explicit errors. (eg. HTTP request failed)"
  (chodf--fire odf :ng val))

(defun chodf-sync! (d)
  "Wait for the given deferred task. For test and debugging.
Error is raised if it is not processed within deferred chain D."
  (let* ((random-symbol (gensym))
	 (last-value random-symbol)
         uncaught-error)
    (chodf-ner d
	       (lambda (x) (setq last-value x))
	       (lambda (err) (setq uncaught-error err)))
    (while (and (eq random-symbol last-value)
		(not uncaught-error))
      (sit-for 0.05)
      (sleep-for 0.05))
    (when uncaught-error
      (chodf-ng uncaught-error))
    last-value))

(defun chodf-nextx (&optional fun)
  "Shorthand for creating new deferred which is called after current task"
  (let ((d (chodf-new)))
    (when fun (setf (chodf-okcb d) fun))
    (run-at-time 0 nil
		 (lambda () (chodf-call d nil)))
    d))

(defun chodf-callx (fun &rest args)
  "Calls function asynchronous. A shorthand for `chodf-nextx'"
  (chodf-nextx (lambda (_)
		 (apply fun args))))

(defun chodf-wait (n)
  "Wait returns deferred that will be called after N seconds elapsed"
  (let ((d (chodf-new))
	(ti (float-time)))
    (run-at-time n nil
		 (lambda ()
		   (chodf-call d (- (float-time) ti))))
    d))

(defun chodf-parallel (&rest fun-or-d-ls)
  "Parallel wraps up deferredlist to one deferred.
FUN-OR-D-LS's element can be either function (include symbol) or deferred object.
return value is a vector, each cell corresponds to function or deferred object in arglist.
This is useful when some asynchronous resources are required."
  (let* ((ret (chodf-new))
	 (len (length fun-or-d-ls))
	 (values (make-vector len nil))
	 (num 0))
    (seq-do-indexed
     (lambda (f i)
       (let* ((d (if (chodf-p f) f (chodf-nextx f))))
	 (chodf-ner
	  d (lambda (v)
	      (aset values i v)
	      (when (<= (cl-decf num) 0)
		(chodf-call ret values)))
	  (lambda (e)
	    (chodf-fail ret e)))
	 (cl-incf num)))
     fun-or-d-ls)
    (when (= num 0)
      (chodf-nextx (lambda (_) (chodf-call ret []))))
    ret))

(defun chodf-chain (&rest funs-or-err)
  "Construct deferred chain and return its deferred.
Shorthand for construct deferred chains.
If function is after a :err symbol, then it will be errback, otherwise callback"
  (let ((chain (chodf-nextx))
	(flag nil))
    (mapc (lambda (obj)
	    (cond
	     ((eq obj :err) (setq flag t))
	     ((functionp obj)
	      (if flag (setq chain (chodf-error chain obj)
			     flag nil)
		(setq chain (chodf-next chain obj))))
	     (t (error "chodf-chain: unknown type in process chains"))))
	  funs-or-err)
    chain))

(defun chodf-earlier (&rest chodfs)
  "Continue process when one deferred in deferredlist has completed. \
Others will be canceled. parallel ('and' processing) <=> earlier ('or' processing)
return value is cons (index . value), index is the earlist deferred's order in arglist"
  (let* ((ret (chodf-new))
	 (num 0)
	 (flag t))
    (seq-do-indexed
     (lambda (d i)
       (chodf-ner
	d (lambda (v)
	    (when flag
	      (setq flag nil)
	      (chodf-call ret (cons i v))))
	(lambda (e)
	  (when flag
	    (setq flag nil)
	    (chodf-fail ret e))))
       (cl-incf num))
     chodfs)
    (when (= num 0)
      (chodf-nextx (lambda (_) (chodf-call ret nil))))
    ret))

(defun chodf-loop (n fun)
  "Provides non-blocking loop.
This loop is slow but not stop emacs's appearance.
This function waits a deferred returned by loop function."
  (let (it ret)
    (setq it (lambda (_)
	       (if (< (cl-decf n) 0) ret
		 (setq ret (funcall fun))
		 (if (chodf-p ret)
		     (chodf-next
		      ret (lambda (r)
			    (setq ret r)
			    (chodf-nextx it)))
		   (chodf-nextx it)))))
    (when (> n 0) (chodf-nextx it))))

(defun chodf-repeat (n fun)
  "* Loop `n` times with `fun`.
* This function automatically returns UI-control to browser, if the loop spends over 20msec.
* This is useful for huge loop not to block browser UI.
* This function can't wait a deferred returned by loop function, compared with Deferred.loop."
  (let ((i 0) it)
    (chodf-nextx
     (setq it (lambda (_)
		(let ((ti (float-time)))
		  (while (and (< i n)
			      (< (- (float-time) ti) 0.02)) ; 20ms
		    (cl-incf i)
		    (funcall fun))
		  (when (< i n) (chodf-callx it nil))))))))

(defun chodf-retry (cnt fun &optional wait)
  "Try func (returns Deferred) till it finish without exceptions."
  (setq wait (or wait 0))
  (let ((d (chodf-new)) retry)
    (setq retry
	  (lambda ()
	    (let ((m (funcall fun)))
	      (setq m (chodf-ner
		       m
		       (lambda (mes) (chodf-call d mes))
		       (lambda (e)
			 (if (<= (cl-decf cnt) 0)
			     (chodf-fail e)
			   (run-at-time wait nil retry))))))))
    (run-at-time 0 nil retry)
    d))

(defun chodf-connect (fun &optional testfn)
  (setq testfn (or testfn (lambda (_) t)))
  (let ((d (chodf-new)))
    (let ((callback (lambda (x)
		      (if (funcall testfn x)
			  (chodf-call d x)
			(chodf-fail d x)))))
      (lambda (&rest args)
	(apply fun `(,@args ,callback))))))

;;; chodf.el ends here
