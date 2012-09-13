#|
  This file is a part of cl-envy project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-envy
  (:use :cl)
  (:export :/.
           :tree-mapleaf
           :tree-doleaf
           :tree-count-if
           :tree-find-if
           :->
           :->$))
(in-package :cl-envy)

(defun /.-split (body &optional head)
  (if body
    (if (let ((first (car body)))
          (and (symbolp first) (string= "->" (symbol-name first))))
      (values (reverse head) (cdr body))
      (if (null (cdr body))
        (values (reverse head) body)
        (/.-split (cdr body) (cons (car body) head))))
    (values (reverse head) body)))

(defmacro /. (&body body)
  (multiple-value-bind (lambda-list body) (/.-split body)
    (let* ((params (mapcar
                    (lambda (x) (if (and (symbolp x)
                                          (string= (symbol-name x) "_"))
                                  (let ((gensym (gensym "UNUSED")))
                                    (cons gensym gensym))
                                  (cons x nil)))
                    lambda-list)))
      `(lambda (,@(mapcar #'car params))
        ,@(mapcar (lambda (x) `(declare (ignore ,x)))
          (remove-if #'not (mapcar #'cdr params)))
        ,@body))))

(defun tree-mapleaf (function tree)
  (if (consp tree)
    (cons (tree-mapleaf function (car tree)) 
          (if (cdr tree) (tree-mapleaf function (cdr tree))))
    (funcall function tree)))

(defun tree-doleaf (function tree)
  (if (consp tree)
    (progn (tree-doleaf function (car tree)) 
           (if (cdr tree) (tree-doleaf function (cdr tree))))
    (funcall function tree))
  tree)

(defun tree-count-if (predicate tree)
  (let ((count 0))
    (tree-doleaf (/. x (if (funcall predicate x) (incf count))) tree)
    count))

(defun tree-find-if (predicate tree)
  (block nil
         (tree-mapleaf (/. x (if (funcall predicate x) (return (values x T)))) 
                      tree)
         (values nil nil)))

(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
    (if more
      `(-> (-> ,x ,form) ,@more)
      (if (listp form)
        `(,(car form) ,x ,@(cdr form))
        (list form x)))
    x))

(defmacro ->$ (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
    (if more
      `(->$ (->$ ,x ,form) ,@more)
      (if (listp form)
        (labels ((is-dollar-symbol (y)
                   (and (symbolp y) (string= (symbol-name y) "$"))))
          (if (tree-find-if #'is-dollar-symbol form)
            (let ((gensym (gensym)))
              `(let ((,gensym ,x))
                (,@(tree-mapleaf (/. leaf (if (is-dollar-symbol leaf) 
                                            gensym leaf))
                           form))))
            `(,(car form) ,x ,@(cdr form)))) 
        (list form x)))
    x))
