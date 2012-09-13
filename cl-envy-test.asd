#|
  This file is a part of cl-envy project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-envy-test-asd
  (:use :cl :asdf))
(in-package :cl-envy-test-asd)

(defsystem cl-envy-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:cl-envy
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-envy"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
