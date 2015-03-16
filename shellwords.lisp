(defpackage :cl-shellwords
  (:use :cl)
  (:nicknames :shellwords)
  (:import-from :ppcre
                #:regex-replace-all
                #:do-register-groups
                #:create-scanner)
  (:export #:split
           #:escape
           #:join
           #:unmatched-quote-error
           #:unmatched-quote-error-string))
(in-package :shellwords)

(define-condition unmatched-quote-error (simple-error)
  ((string :initarg :string
           :reader unmatched-quote-error-string))
  (:report (lambda (condition stream)
             (format stream "Unmatched quote: ~S"
                     (unmatched-quote-error-string condition)))))

(defparameter +split-regexp+
  (create-scanner
   "\\s*(?>([^\\s\\\\\\'\\\"]+)|'([^\\']*)'|\"((?:[^\\\"\\\\]|\\\\.)*)\"|(\\\\.?)|(\\S))(\\s|\\z)?"
   :single-line-mode t))

(defun split (string)
  "Split STRING into a list of words, handling escaping the same way a shell
like the Bourne shell does.

Whitespace normally acts as a word separator, except when preceded by a
backslash or enclosed in single- or double quotes.

Examples:
  (split \"example string\")             ;=> (\"example\"  \"string\")
  (split \"example\\ escaped string\")   ;=> (\"example escaped\" \"string\")
  (split \"example 'escaped string'\")   ;=> (\"example\" \"escaped string\")
  (split \"example \"escaped string\"\") ;=> (\"example\" \"escaped string\")

If STRING contains non-matching single- or double quotes, an error of type
UNMATCHED-QUOTE-ERROR is signaled. STRING can be retrieved from the error
object using UNMATCHED-QUOTE-ERROR-STRING."
  (check-type string string)
  (let ((words ())
        (field ""))
    (do-register-groups (word sq dq esc garbage sep)
        (+split-regexp+ string)
      (when garbage
        (error 'unmatched-quote-error
               :string string))
      (setf field
            (concatenate 'string
                         field
                         (or word sq
                             (regex-replace-all "\\\\(.)" (or dq esc)
                                                "\\1"))))
      (when sep
        (push field words)
        (setf field "")))
    (reverse words)))

(defun escape (string)
  "Escape STRING so that it is safe when used as an argument in a shell like
the Bourne shell.

- If STRING is an empty string, a pair of single quotes is returned.
- An LF character is escaped by placing it in single quotes.
- All other special characters are escaped with a backslash.

Examples:
  (escape \"\")                       ;=> \"''\"
  (escape \"It's an example string\") ;=> \"It\\s\\ an\\ example\\ string\"
  (escape \"NothingWrongHere\")       ;=> \"NothingWrongHere\"
  (escape \"{LF}\")                   ;=> \"'{LF}'\" ({LF} = #\\linefeed)"
  (check-type string string)
  (if (zerop (length string))
      "''"
      (regex-replace-all "([^A-Za-z0-9_\\-.,:\\/@\\n])|(\\n)"
                         string
                         (lambda (match reg-1 reg-2)
                           (declare (ignore match))
                           (if reg-1
                               (format nil "\\~A"  reg-1)
                               (format nil "'~A'" reg-2)))
                         :simple-calls t)))

(defun join (sequence)
  "Join the elements of SEQUENCE together, separated by spaces. The elements
are first passed to ESCAPE for proper escaping.

SEQUENCE should be a list or vector of strings."
  (check-type sequence (or list (vector string)))
  (format nil "~{~A~^ ~}" (etypecase sequence
                            (list (mapcar #'escape sequence))
                            (vector (loop for elt across sequence
                                          collect (escape elt))))))
