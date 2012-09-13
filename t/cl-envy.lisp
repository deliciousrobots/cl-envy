#|
  This file is a part of cl-envy project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-envy-test
  (:use :cl
        :cl-envy
        :cl-test-more))
(in-package :cl-envy-test)

(plan nil)

(is (funcall (/. _ x _ (* x x)) 10 20 30) 400)

(finalize)
