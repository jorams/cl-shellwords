(defsystem :cl-shellwords-test
  :description "Tests for cl-shellwords"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:cl-shellwords #:fiasco)
  :components ((:file "test"))
  :perform (test-op :after (op component)
                    (funcall (intern #.(string :run-package-tests) :fiasco)
                             :package :cl-shellwords-test)))
