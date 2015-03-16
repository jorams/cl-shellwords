(defsystem :cl-shellwords
  :description "Common Lisp port of Ruby's shellwords.rb, for escaping and
splitting strings to be passed to a shell."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:cl-ppcre)
  :components ((:file "shellwords"))
  :in-order-to ((test-op
                 (load-op :cl-shellwords-test)
                 (test-op :cl-shellwords-test))))
