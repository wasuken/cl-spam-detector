(defpackage cl-spam-detector/tests/main
  (:use :cl
        :cl-spam-detector
        :rove))
(in-package :cl-spam-detector/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-spam-detector)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
