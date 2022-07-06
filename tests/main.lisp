(defpackage jpmap/tests/main
  (:use :cl
        :jpmap
        :rove))
(in-package :jpmap/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :jpmap)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
