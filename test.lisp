(defpackage :cl-shellwords-test
  (:use :cl :cl-shellwords :prove))
(in-package :cl-shellwords-test)

(plan 3)

(deftest "Testing string splitting"
  (plan 8)
  (let ((string "echo test\\ string 'with multiple' \"escaped arguments\" woohoo")
        (results '("echo"
                   "test string"
                   "with multiple"
                   "escaped arguments"
                   "woohoo")))
    (is (length (split string)) 5)

    (loop for part in (split string)
          for result in results
          do (is part result)))

  (is-error (split "test 'string for testing")
            'unmatched-quote-error)
  (is-error (split "'\"'\"\"'\"\"'\"")
            'unmatched-quote-error))

(deftest "Testing escaping"
  (plan 4)
  (is (escape "") "''")
  (is (escape "It's an example string")
      "It\\'s\\ an\\ example\\ string")
  (is (escape "NothingWrongHere")
      "NothingWrongHere")
  (is (escape (format nil "~A" #\linefeed))
      (format nil "'~A'" #\linefeed)))

(deftest "Testing join"
  (plan 3)
  (is (join '("This" "the future?"))
      "This the\\ future\\?")
  (is (join #("or" "is this" "the past?"))
      "or is\\ this the\\ past\\?")
  (is-error (join "This is now.")
            'type-error))
