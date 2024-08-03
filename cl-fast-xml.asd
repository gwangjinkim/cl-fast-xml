(defsystem "cl-fast-xml"
  :long-name "common-lisp-fast-xml"
  :version "0.1.1"
  :author "Gwang-Jin Kim"
  :maintainer "Gwang-Jin Kim"
  :mailto "gwang.jin.kim.phd@gmail.com"
  :license "MIT"
  :homepage "https://github.com/gwangjinkim/cl-fast-xml"
  :bug-tracker "https://github.com/gwangjinkim/cl-fast-xml/issues"
  :source-control "https://github.com/gwangjinkim/cl-fast-xml.git"
  :depends-on ("cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Fast parsing of XML file - translated package from racket-fast-xml"
  :long-description "Common Lisp translation of the package racket-fast-xml https://docs.racket-lang.org/fast-xml/index.html and source code https://github.com/simmone/racket-fast-xml"
  :in-order-to ((test-op (test-op "cl-fast-xml/tests"))))

(defsystem "cl-fast-xml/tests"
  :author "Gwang-Jin Kim"
  :license "MIT"
  :depends-on ("cl-fast-xml"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-fast-xml"
  :perform (test-op (op c) (symbol-call :rove :run c)))
