(fiasco:define-test-package :cl-shellwords-test
  (:use :cl :cl-shellwords :fiasco))
(in-package :cl-shellwords-test)

(deftest test-split ()
  (let ((string "echo test\\ string 'with multiple' \"escaped arguments\" woohoo")
        (results '("echo"
                   "test string"
                   "with multiple"
                   "escaped arguments"
                   "woohoo")))
    (is (= (length (split string)) 5))

    (loop for part in (split string)
          for result in results
          do (is (string= part result)))))

(deftest test-split-unmatched ()
  (is (handler-case
          (progn (split "test 'string for testing")
                 nil)
        (unmatched-quote-error () t)))
  (is (handler-case
          (progn (split "'\"'\"\"'\"\"'\"")
                 nil)
        (unmatched-quote-error () t))))

(deftest test-escape ()
  (is (string= (escape "") "''"))
  (is (string= (escape "It's an example string")
               "It\\'s\\ an\\ example\\ string"))
  (is (string= (escape "NothingWrongHere")
               "NothingWrongHere"))
  (is (string= (escape (format nil "~A" #\linefeed))
               (format nil "'~A'" #\linefeed))))

(deftest test-join ()
  (is (string= (join '("This" "the future?"))
               "This the\\ future\\?"))
  (is (string= (join #("or" "is this" "the past?"))
               "or is\\ this the\\ past\\?"))
  (is (handler-case
          (progn (join "This is now.")
                 nil)
        (type-error () t))))

