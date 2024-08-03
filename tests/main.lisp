(defpackage cl-fast-xml/tests/main
  (:use :cl
        :cl-fast-xml
        :rove))
(in-package :cl-fast-xml/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-fast-xml)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
