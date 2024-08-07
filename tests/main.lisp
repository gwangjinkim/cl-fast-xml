(ql:quickload :uiop)
(ql:quickload :cl-fast-xml)
(ql:quickload :fiveam)

(defpackage cl-fast-xml/tests/main
  (:use :cl
        :cl-fast-xml
	:uiop
        :fiveam)
  (:shadowing-import-from :cl-fast-xml
			  #:defs-to-hash
			  #:from-special-chars
			  #:to-special-chars
			  #:from-keys-to-pure-key
			  #:from-keys-to-count-key
			  #:from-keys-to-value-key
			  #:xml-to-hash
			  #:xml-file-to-hash
			  #:xml-port-to-hash
			  #:lists-to-xml
			  #:lists-to-compact-xml
			  #:to-compact))

(in-package :cl-fast-xml/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-fast-xml)' in your Lisp.


(defun stream-to-string (stream)
  "Reads all content from the given input stream and returns it as a string."
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line do (princ line out)
			(terpri out))))

(defun file-to-string (filename)
  "Return file content to string."
  (with-open-file (out filename :direction :input)
    (stream-to-string out)))

(def-suite lib
  :description "Test lib functions")
(in-suite lib)


(test test-defs-to-hash
  (let ((def-hash (defs-to-hash '("topic.v"))))
    (is (= (hash-table-count def-hash) 2))
    (is (eq (gethash "topic" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "topic.v" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h1.h2.h3.topic"))))
    (is (= (hash-table-count def-hash) 4))
    (is (eq (gethash "h1" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3.topic" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h1.h2.h3.topic" "h3.h4.h5.v"))))
    (is (= (hash-table-count def-hash) 8))
    (is (eq (gethash "h1" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3.topic" def-hash) 'cl-fast-xml::v))
    (is (eq (gethash "h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h3.h4" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h3.h4.h5" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h3.h4.h5.v" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h1.h2.h3.topic" "h1.h2.h4.topic"))))
    (is (= (hash-table-count def-hash) 6))
    (is (eq (gethash "h1" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h4" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3.topic" def-hash) 'cl-fast-xml::v))
    (is (eq (gethash "h1.h2.h4.topic" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h1.h2.h3.topic" "h1.h2"))))
    (is (= (hash-table-count def-hash) 4))
    (is (eq (gethash "h1" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2" def-hash) 'cl-fast-xml::kv))
    (is (eq (gethash "h1.h2.h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3.topic" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h1.h2" "h1.h2.h3.topic" "h1.h2.h3.content"))))
    (is (= (hash-table-count def-hash) 5))
    (is (eq (gethash "h1" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2" def-hash) 'cl-fast-xml::kv))
    (is (eq (gethash "h1.h2.h3" def-hash) 'cl-fast-xml::k))
    (is (eq (gethash "h1.h2.h3.topic" def-hash) 'cl-fast-xml::v))
    (is (eq (gethash "h1.h2.h3.content" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h" "h.a" "h.b"))))
    (is (= (hash-table-count def-hash) 3))
    (is (eq (gethash "h" def-hash) 'cl-fast-xml::kv))
    (is (eq (gethash "h.a" def-hash) 'cl-fast-xml::v))
    (is (eq (gethash "h.b" def-hash) 'cl-fast-xml::v)))

  (let ((def-hash (defs-to-hash '("h.a" "h" "h.b"))))
    (is (= (hash-table-count def-hash) 3))
    (is (eq (gethash "h" def-hash) 'cl-fast-xml::kv))
    (is (eq (gethash "h.a" def-hash) 'cl-fast-xml::v))
    (is (eq (gethash "h.b" def-hash) 'cl-fast-xml::v))))

(test test-from-special-chars
  (is (string= (from-special-chars "1&lt;2") "1<2"))
  (is (string= (from-special-chars "1&gt;2") "1>2"))
  (is (string= (from-special-chars "1&amp;2") "1&2"))
  (is (string= (from-special-chars "1&apos;2") "1'2"))
  (is (string= (from-special-chars "1&quot;2") "1\"2"))
  (is (string= (from-special-chars "1&quot;&quot;2&lt;3&gt;4&amp;5&apos;6") "1\"\"2<3>4&5'6")))

(test test-to-special-chars
  (is (string= (to-special-chars "1<2") "1&lt;2"))
  (is (string= (to-special-chars "1>2") "1&gt;2"))
  (is (string= (to-special-chars "1&2") "1&amp;2"))
  (is (string= (to-special-chars "1'2") "1&apos;2"))
  (is (string= (to-special-chars "1\"2") "1&quot;2"))
  (is (string= (to-special-chars "1\"\"2<3>4&5'6") "1&quot;&quot;2&lt;3&gt;4&amp;5&apos;6")))

(test test-from-keys-to-pure-key
  (is (string= (from-keys-to-pure-key '(("a" . 1))) "a"))
  (is (string= (from-keys-to-pure-key '(("b" . 1) ("a" . 1))) "a.b"))
  (is (string= (from-keys-to-pure-key '(("c" . 2) ("b" . 1) ("a" . 2))) "a.b.c"))
  (is (string= (from-keys-to-pure-key '(("d" . 3) ("c" . 2) ("b" . 1) ("a" . 2))) "a.b.c.d")))

(test test-from-keys-to-count-key
  (is (string= (from-keys-to-count-key '(("a" . 1))) "a"))
  (is (string= (from-keys-to-count-key '(("b" . 1) ("a" . 1))) "a1.b"))
  (is (string= (from-keys-to-count-key '(("c" . 2) ("b" . 1) ("a" . 2))) "a2.b1.c"))
  (is (string= (from-keys-to-count-key '(("d" . 3) ("c" . 2) ("b" . 1) ("a" . 2))) "a2.b1.c2.d")))

(test test-from-keys-to-value-key
  (is (string= (from-keys-to-value-key '(("a" . 1))) "a1"))
  (is (string= (from-keys-to-value-key '(("b" . 1) ("a" . 1))) "a1.b1"))
  (is (string= (from-keys-to-value-key '(("c" . 2) ("b" . 1) ("a" . 2))) "a2.b1.c2"))
  (is (string= (from-keys-to-value-key '(("d" . 3) ("c" . 2) ("b" . 1) ("a" . 2))) "a2.b1.c2.d3")))


             

(def-suite special-chars
  :description "Test special character replacements")
(in-suite special-chars)

(defparameter *special1* "special1.xml")
(defparameter *special2* "special2.xml")

(defun ht2al (ht)
  (let ((alist '()))
    (maphash (lambda (k v) (push (cons k v) alist)) ht)
    alist))

(test test-special-xml
  (let ((xml-hash (xml-file-to-hash *special1* '("basic1.attr" "basic1.value"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (string= (gethash "basic11.attr1" xml-hash) "<2>"))
    (is (string= (gethash "basic11.value1" xml-hash) "<1>"))))

(test write-special1-xml
  (let ((xml '("basic1" ("attr" . "<2>") ("value" "<1>"))))
    (with-open-file (p *special1* :direction :input)
      (is (string= (lists-to-xml xml) (stream-to-string p))))))

(test test-special-char
  (let ((xml-hash (xml-file-to-hash *special2* '("sst.si.t"))))
    (is (string= (gethash "sst1.si1.t1" xml-hash) "<test>"))
    (is (string= (gethash "sst1.si2.t1" xml-hash) "<foo> "))
    (is (string= (gethash "sst1.si3.t1" xml-hash) " <baz>"))
    (is (string= (gethash "sst1.si4.t1" xml-hash) "< bar>"))
    (is (string= (gethash "sst1.si5.t1" xml-hash) "< fro >"))
    (is (string= (gethash "sst1.si6.t1" xml-hash) "<bas >"))
    (is (string= (gethash "sst1.si7.t1" xml-hash) "<maybe"))
    (is (string= (gethash "sst1.si8.t1" xml-hash) "<< not >>"))
    (is (string= (gethash "sst1.si9.t1" xml-hash) "show>"))))

(def-suite test-lists-to-xml
  :description "Test lists-to-xml function.")
(in-suite test-lists-to-xml)

;; files
(defparameter *empty-xml-file* "empty.xml")
(defparameter *one-node-xml-file* "one-node.xml")
(defparameter *property-xml-file* "property.xml")
(defparameter *content-xml-file* "content.xml")
(defparameter *symbol-content-xml-file* "symbol-content.xml")
(defparameter *children-xml-file* "children_.xml")
(defparameter *children-compact-xml-file* "children-compact.xml")
(defparameter *no-header-children-xml-file* "no-header-children.xml")
(defparameter *no-header-children-compact-xml-file* "no-header-children-compact.xml")
(defparameter *three-xml-file* "three.xml")
(defparameter *three-compact-xml-file* "three-compact.xml")
(defparameter *parallel-xml-file* "parallel.xml")
(defparameter *parallel-compact-xml-file* "parallel-compact.xml")
(defparameter *more-complex-xml-file* "more-complex.xml")
(defparameter *more-complex-compact-xml-file* "more-complex-compact.xml")

(test test-empty-xml
  (let ((xml '()))
    (is (string= (lists-to-xml xml) (file-to-string *empty-xml-file*)))))

(test test-one-node-xml
  (let ((xml '("H1")))
    (is (string= (lists-to-xml xml) (file-to-string *one-node-xml-file*)))))

(test test-property-xml
  (let ((xml '("H1" ("color" . "red"))))
    (is (string= (lists-to-xml xml) (file-to-string *property-xml-file*)))))

(test test-content-xml
  (let ((xml '("H1" ("color" . "red") "Hello XML")))
    (is (string= (lists-to-xml xml) (file-to-string *content-xml-file*)))))

(test test-symbol-content-xml
  (let ((xml '(H1 (color . red) "Hello XML")))
    (is  (string= (lists-to-xml xml) (file-to-string *content-xml-file*)))))

(test test-children-xml
  (let ((xml '("H1" ("color" . "red") ("height" . "5") ("H2" ("color" . "black") "Simple XML"))))
    (is (string= (lists-to-xml xml) (file-to-string *children-xml-file*)))
    (is (string= (lists-to-compact-xml xml) (file-to-string *children-compact-xml-file*)))))

(test test-three-xml
  (let ((xml '("H1" ("color" . "red") (height . "5") ("H2" ("color" . "black") ("H3" ("color" . "pink") "Simple XML")))))
    (is (string= (lists-to-xml xml) (file-to-string *three-xml-file*)))
    (is (string= (lists-to-compact-xml xml) (file-to-string *three-compact-xml-file*)))))

(test test-parallel-xml
  (let ((xml '("H1" ("color" . "red") ("height" . "5") ("H2" ("color" . "black") "Simple XML") ("H3" ("color" . "pink") "Haha"))))
    (is (string= (lists-to-xml xml) (file-to-string *parallel-xml-file*)))
    (is (string= (lists-to-compact-xml xml) (file-to-string *parallel-compact-xml-file*)))))
;; I changed this test instead of the symbol `Haha` I am using a string.
;; this XML version should work only with String Texts.

(test test-more-complex-xml
  (let ((xml '("H1" ("color" . "red") ("height" . "5")
	       ("H2" ("color" . "black") ("H3" ("color" . "pink") "Simple XML"))
	       ("H2" ("color" . "red") ("H3" ("color" . "yellow") "Peace")))))
    (is (string= (lists-to-xml xml) (file-to-string *more-complex-xml-file*)))
    (is (string= (lists-to-compact-xml xml) (file-to-string *more-complex-compact-xml-file*)))))

(test test-no-header
  (let ((xml '("H1" ("color" . "red") ("height" . "5") ("H2" ("color" . "black") "Simple XML"))))
    (is (string= (lists-to-xml-content xml) (file-to-string *no-header-children-xml-file*)))
    (is (string= (to-compact (lists-to-xml-content xml)) (file-to-string *no-header-children-compact-xml-file*)))))

(defparameter *children1-xml-file* "children.xml")
(defparameter *complex-shells-xml-file* "complex-shells.xml")
(defparameter *app-test-xml-file* "app_test.xml")
(defparameter *list1* "list1.xml")
(defparameter *list2* "list2.xml")
(defparameter *list3* "list3.xml")
(defparameter *list4* "list4.xml")


(test read-children-xml
  (let ((xml-hash (xml-file-to-hash *children1-xml-file* '("children.child1"
							   "children.child1.attr1"
							   "children.child2"
							   "children.child2.attr1"))))
    (is (string= (gethash "children1.child11" xml-hash) "c1"))
    (is (string= (gethash "children1.child12" xml-hash) ""))
    (is (string= (gethash "children1.child11.attr11" xml-hash) "a1"))
    (is (string= (gethash "children1.child21" xml-hash) "c2"))
    (is (string= (gethash "children1.child22" xml-hash) "c3"))
    (is (string= (gethash "children1.child21.attr11" xml-hash) "a2"))
    (is (= (gethash "children's count" xml-hash) 1))
    (is (= (gethash "children1.child1's count" xml-hash) 2))
    (is (= (gethash "children1.child2's count" xml-hash) 2))
    (is (= (hash-table-count xml-hash) 9))))
                    
(test write-children-xml
  (let ((xml '("children" ("child1" ("attr1" . "a1") "c1") ("child2" ("attr1" . "a2") "c2") ("child1") ("child2" "c3"))))
    (is (string= (lists-to-xml xml) (file-to-string *children1-xml-file*)))))

(test test-complex-shells-xml
  (let ((xml-hash (xml-file-to-hash *complex-shells-xml-file* '("worksheet.cols.col.min"))))
    (is (string= (gethash "worksheet1.cols1.col1.min1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.cols1.col2.min1" xml-hash) "3"))
    (is (string= (gethash "worksheet1.cols1.col3.min1" xml-hash) "4"))
    (is (string= (gethash "worksheet1.cols1.col4.min1" xml-hash) "8"))
    (is (string= (gethash "worksheet1.cols1.col5.min1" xml-hash) "13"))
    (is (string= (gethash "worksheet1.cols1.col6.min1" xml-hash) "14"))
    (is (string= (gethash "worksheet1.cols1.col7.min1" xml-hash) "16"))
    (is (string= (gethash "worksheet1.cols1.col8.min1" xml-hash) "17"))
    (is (string= (gethash "worksheet1.cols1.col9.min1" xml-hash) "18"))
    (is (string= (gethash "worksheet1.cols1.col10.min1" xml-hash) "22"))
    (is (string= (gethash "worksheet1.cols1.col11.min1" xml-hash) "25"))
    (is (string= (gethash "worksheet1.cols1.col12.min1" xml-hash) "27"))
    (is (string= (gethash "worksheet1.cols1.col13.min1" xml-hash) "31"))
    (is (string= (gethash "worksheet1.cols1.col14.min1" xml-hash) "33"))
    (is (string= (gethash "worksheet1.cols1.col15.min1" xml-hash) "38"))
    (is (string= (gethash "worksheet1.cols1.col16.min1" xml-hash) "39"))
    (is (string= (gethash "worksheet1.cols1.col17.min1" xml-hash) "40"))
    (is (string= (gethash "worksheet1.cols1.col18.min1" xml-hash) "47"))
    (is (string= (gethash "worksheet1.cols1.col19.min1" xml-hash) "48"))))

(test test-docProc
  (let ((xml-hash (xml-file-to-hash *app-test-xml-file* '("Properties.TitlesOfParts.vt:vector.vt:lpstr"))))
    (is (= (gethash "Properties1.TitlesOfParts1.vt:vector's count" xml-hash) 1))
    (is (= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr's count" xml-hash) 6))

    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr1" xml-hash) "数据页面"))
    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr2" xml-hash) "Sheet2"))
    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr3" xml-hash) "Sheet3"))
    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr4" xml-hash) "Chart1"))
    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr5" xml-hash) "Chart4"))
    (is (string= (gethash "Properties1.TitlesOfParts1.vt:vector1.vt:lpstr6" xml-hash) "Chart5"))))

(test test-list1
  (let ((xml-hash (xml-file-to-hash *list1* '("list.child" "list.child.attr"))))
    (is (= (hash-table-count xml-hash) 8))
    (is (string= (gethash "list1.child1" xml-hash) "c1"))
    (is (string= (gethash "list1.child2" xml-hash) "c2"))
    (is (string= (gethash "list1.child3" xml-hash) "c3"))
    (is (string= (gethash "list1.child1.attr1" xml-hash) "a1"))
    (is (string= (gethash "list1.child2.attr1" xml-hash) "a2"))
    (is (string= (gethash "list1.child3.attr1" xml-hash) "a3"))))

(test write-list1
  (let ((xml '("list"
	      ("child" ("attr" . "a1") "c1")
	      ("child" ("attr" . "a2") "c2")
	       ("child" ("attr" . "a3") "c3"))))
    (is (string= (lists-to-xml xml) (file-to-string *list1*)))))

(test read-list2
  (let ((xml-hash (xml-file-to-hash *list2* '("list.child" "list.child.attr"))))
    (is (= (hash-table-count xml-hash) 15))
    (is (= (gethash "list1.child's count" xml-hash) 3))
    
    (is (string= (gethash "list1.child1" xml-hash) "c1"))
    (is (string= (gethash "list1.child2" xml-hash) "c2"))
    (is (string= (gethash "list1.child3" xml-hash) "c3"))

    (is (= (gethash "list2.child's count" xml-hash) 3))
    (is (string= (gethash "list2.child1" xml-hash) "c4"))
    (is (string= (gethash "list2.child2" xml-hash) "c5"))
    (is (string= (gethash "list2.child3" xml-hash) "c6"))
    
    (is (string= (gethash "list1.child1.attr1" xml-hash) "a1"))
    (is (string= (gethash "list1.child2.attr1" xml-hash) "a2"))
    (is (string= (gethash "list1.child3.attr1" xml-hash) "a3"))

    (is (string= (gethash "list2.child1.attr1" xml-hash) "a4"))
    (is (string= (gethash "list2.child2.attr1" xml-hash) "a5"))
    (is (string= (gethash "list2.child3.attr1" xml-hash) "a6"))))

(test read-list3
  (let ((xml-hash (xml-file-to-hash *list3* '("list.child"))))
    (is (= (hash-table-count xml-hash) 5))
    (is (string= (gethash "list1.child1" xml-hash) "c1"))
    (is (string= (gethash "list1.child2" xml-hash) ""))
    (is (string= (gethash "list1.child3" xml-hash) "c3")))
  (let ((xml-hash (xml-file-to-hash *list3* '("list.child.attr"))))
    (is (= (hash-table-count xml-hash) 5))
    (is (string= (gethash "list1.child1.attr1" xml-hash) "a1"))
    (is (string= (gethash "list1.child2.attr1" xml-hash) "a2"))
    (is (string= (gethash "list1.child3.attr1" xml-hash) "a3"))))
    
(test read-list4
  (let ((xml-hash (xml-file-to-hash *list4* '("list.child" "list.child.attr"))))
    (is (= (hash-table-count xml-hash) 14))
    (is (string= (gethash "list1.child1" xml-hash) "c1"))
    (is (string= (gethash "list1.child2" xml-hash) ""))
    (is (string= (gethash "list1.child3" xml-hash) "c3"))
    (is (string= (gethash "list1.child4" xml-hash) ""))
    (is (string= (gethash "list1.child5" xml-hash) "c4"))
    (is (string= (gethash "list1.child6" xml-hash) ""))
    (is (string= (gethash "list1.child7" xml-hash) ""))
    (is (string= (gethash "list1.child8" xml-hash) ""))
    (is (string= (gethash "list1.child1.attr1" xml-hash) "a1"))
    (is (string= (gethash "list1.child2.attr1" xml-hash) "a2"))
    (is (string= (gethash "list1.child3.attr1" xml-hash) "a3"))
    (is (string= (gethash "list1.child8.attr1" xml-hash) "a6"))))

(defparameter *sheet* "sheet.xml")
(defparameter *workbook* "workbook.xml")
(defparameter *broken* "broken.xml")

(test test-sheet
  (let ((xml-hash (xml-file-to-hash *sheet*
				    '("worksheet.xmlns"
				      "worksheet.xmlns:r"
				      "worksheet.dimension.ref"
				      "worksheet.sheetViews.sheetView.selection"
				      "worksheet.sheetViews.sheetView.selection.pane"
				      "worksheet.sheetViews.sheetView.workbookViewId"
				      "worksheet.sheetViews.sheetView.pane"
				      "worksheet.sheetViews.sheetView.pane.ySplit"
				      "worksheet.sheetViews.sheetView.pane.xSplit"
				      "worksheet.sheetViews.sheetView.pane.topLeftCell"
				      "worksheet.sheetViews.sheetView.pane.activePane"
				      "worksheet.sheetViews.sheetView.pane.state"
				      "worksheet.sheetFormatPr.defaultRowHeight"
				      "worksheet.cols.col.min"
				      "worksheet.cols.col.max"
				      "worksheet.cols.col.width"
				      "worksheet.sheetData.row.r"
				      "worksheet.sheetData.row.spans"
				      "worksheet.sheetData.row.c.r"
				      "worksheet.sheetData.row.c.t"
				      "worksheet.sheetData.row.c.v"
				      "worksheet.phoneticPr"
				      "worksheet.phoneticPr.fontId"
				      "worksheet.phoneticPr.type"
				      "worksheet.pageMargins"
				      "worksheet.pageMargins.left"
				      "worksheet.pageMargins.right"
				      "worksheet.pageMargins.top"
				      "worksheet.pageMargins.bottom"
				      "worksheet.pageMargins.header"
				      "worksheet.pageMargins.footer"
				      "worksheet.pageSetup"
				      "worksheet.pageSetup.paperSize"
				      "worksheet.pageSetup.orientation"
				      "worksheet.pageSetup.horizontalDpi"
				      "worksheet.pageSetup.verticalDpi"
				      "worksheet.pageSetup.r:id"))))
    (is (string= (gethash "worksheet1.xmlns1" xml-hash) "http://schemas.openxmlformats.org/spreadsheetml/2006/main"))
    (is (string= (gethash "worksheet1.xmlns:r1" xml-hash) "http://schemas.openxmlformats.org/officeDocument/2006/relationships"))

    (is (string= (gethash "worksheet1.dimension1.ref1" xml-hash) "A1:F4"))

    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection1" xml-hash) ""))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection2" xml-hash) ""))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection3" xml-hash) ""))

    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection1.pane1" xml-hash) "bottomLeft"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection2.pane1" xml-hash) "topRight"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.selection3.pane1" xml-hash) "bottomRight"))

    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.workbookViewId1" xml-hash) "0"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1" xml-hash) ""))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1.ySplit1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1.xSplit1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1.topLeftCell1" xml-hash) "B2"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1.activePane1" xml-hash) "bottomRight"))
    (is (string= (gethash "worksheet1.sheetViews1.sheetView1.pane1.state1" xml-hash) "frozen"))

    (is (string= (gethash "worksheet1.sheetFormatPr1.defaultRowHeight1" xml-hash) "13.5"))

    (is (string= (gethash "worksheet1.cols1.col1.min1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.cols1.col2.min1" xml-hash) "3"))
    (is (string= (gethash "worksheet1.cols1.col3.min1" xml-hash) "5"))
    (is (string= (gethash "worksheet1.cols1.col4.min1" xml-hash) "6"))

    (is (string= (gethash "worksheet1.cols1.col1.max1" xml-hash) "2"))
    (is (string= (gethash "worksheet1.cols1.col2.max1" xml-hash) "4"))
    (is (string= (gethash "worksheet1.cols1.col3.max1" xml-hash) "5"))
    (is (string= (gethash "worksheet1.cols1.col4.max1" xml-hash) "6"))

    (is (string= (gethash "worksheet1.cols1.col1.width1" xml-hash) "50"))
    (is (string= (gethash "worksheet1.cols1.col2.width1" xml-hash) "8"))
    (is (string= (gethash "worksheet1.cols1.col3.width1" xml-hash) "14"))
    (is (string= (gethash "worksheet1.cols1.col4.width1" xml-hash) "8"))

    (is (string= (gethash "worksheet1.sheetData1.row1.r1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.sheetData1.row2.r1" xml-hash) "2"))
    (is (string= (gethash "worksheet1.sheetData1.row3.r1" xml-hash) "3"))
    (is (string= (gethash "worksheet1.sheetData1.row4.r1" xml-hash) "4"))

    (is (string= (gethash "worksheet1.sheetData1.row1.spans1" xml-hash) "1:6"))
    (is (string= (gethash "worksheet1.sheetData1.row2.spans1" xml-hash) "1:6"))
    (is (string= (gethash "worksheet1.sheetData1.row3.spans1" xml-hash) "1:6"))
    (is (string= (gethash "worksheet1.sheetData1.row4.spans1" xml-hash) "1:6"))

    (is (string= (gethash "worksheet1.sheetData1.row1.c1.r1" xml-hash) "A1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c2.r1" xml-hash) "B1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c3.r1" xml-hash) "C1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c4.r1" xml-hash) "D1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c5.r1" xml-hash) "E1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c6.r1" xml-hash) "F1"))

    (is (string= (gethash "worksheet1.sheetData1.row2.c1.r1" xml-hash) "A2"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c2.r1" xml-hash) "B2"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c3.r1" xml-hash) "C2"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c4.r1" xml-hash) "D2"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c5.r1" xml-hash) "E2"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c6.r1" xml-hash) "F2"))

    (is (string= (gethash "worksheet1.sheetData1.row3.c1.r1" xml-hash) "A3"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c2.r1" xml-hash) "B3"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c3.r1" xml-hash) "C3"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c4.r1" xml-hash) "D3"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c5.r1" xml-hash) "E3"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c6.r1" xml-hash) "F3"))

    (is (string= (gethash "worksheet1.sheetData1.row4.c1.r1" xml-hash) "A4"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c2.r1" xml-hash) "B4"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c3.r1" xml-hash) "C4"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c4.r1" xml-hash) "D4"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c5.r1" xml-hash) "E4"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c6.r1" xml-hash) "F4"))

    (is (string= (gethash "worksheet1.sheetData1.row1.c1.t1" xml-hash) "s"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c2.t1" xml-hash) "s"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c3.t1" xml-hash) "s"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c4.t1" xml-hash) "s"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c5.t1" xml-hash) "s"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c6.t1" xml-hash) "s"))

    (is (string= (gethash "worksheet1.sheetData1.row2.c1.t1" xml-hash) "s"))

    (is (string= (gethash "worksheet1.sheetData1.row3.c1.t1" xml-hash) "s"))

    (is (string= (gethash "worksheet1.sheetData1.row4.c1.t1" xml-hash) "s"))

    (is (string= (gethash "worksheet1.sheetData1.row1.c1.v1" xml-hash) "16"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c2.v1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c3.v1" xml-hash) "2"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c4.v1" xml-hash) "3"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c5.v1" xml-hash) "4"))
    (is (string= (gethash "worksheet1.sheetData1.row1.c6.v1" xml-hash) "5"))

    (is (string= (gethash "worksheet1.sheetData1.row2.c1.v1" xml-hash) "8"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c2.v1" xml-hash) "100"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c3.v1" xml-hash) "300"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c4.v1" xml-hash) "200"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c5.v1" xml-hash) "0.6934"))
    (is (string= (gethash "worksheet1.sheetData1.row2.c6.v1" xml-hash) "43360"))

    (is (string= (gethash "worksheet1.sheetData1.row3.c1.v1" xml-hash) "13"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c2.v1" xml-hash) "200"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c3.v1" xml-hash) "400"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c4.v1" xml-hash) "300"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c5.v1" xml-hash) "139999.89223"))
    (is (string= (gethash "worksheet1.sheetData1.row3.c6.v1" xml-hash) "43361"))

    (is (string= (gethash "worksheet1.sheetData1.row4.c1.v1" xml-hash) "6"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c2.v1" xml-hash) "300"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c3.v1" xml-hash) "500"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c4.v1" xml-hash) "400"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c5.v1" xml-hash) "23.34"))
    (is (string= (gethash "worksheet1.sheetData1.row4.c6.v1" xml-hash) "43362"))

    (is (string= (gethash "worksheet1.phoneticPr1" xml-hash) ""))
    (is (string= (gethash "worksheet1.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "worksheet1.phoneticPr1.type1" xml-hash) "noConversion"))

    (is (string= (gethash "worksheet1.pageMargins1" xml-hash) ""))
    (is (string= (gethash "worksheet1.pageMargins1.left1" xml-hash) "0.7"))
    (is (string= (gethash "worksheet1.pageMargins1.right1" xml-hash) "0.7"))
    (is (string= (gethash "worksheet1.pageMargins1.top1" xml-hash) "0.75"))
    (is (string= (gethash "worksheet1.pageMargins1.bottom1" xml-hash) "0.75"))
    (is (string= (gethash "worksheet1.pageMargins1.header1" xml-hash) "0.3"))
    (is (string= (gethash "worksheet1.pageMargins1.footer1" xml-hash) "0.3"))

    (is (string= (gethash "worksheet1.pageSetup1" xml-hash) ""))
    (is (string= (gethash "worksheet1.pageSetup1.paperSize1" xml-hash) "9"))
    (is (string= (gethash "worksheet1.pageSetup1.orientation1" xml-hash) "portrait"))
    (is (string= (gethash "worksheet1.pageSetup1.horizontalDpi1" xml-hash) "200"))
    (is (string= (gethash "worksheet1.pageSetup1.verticalDpi1" xml-hash) "200"))
    (is (string= (gethash "worksheet1.pageSetup1.r:id1" xml-hash) "rId1"))))
    
(test write-sheet
  (let ((xml '("worksheet"
                 ("xmlns" . "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
                 ("xmlns:r" . "http://schemas.openxmlformats.org/officeDocument/2006/relationships")
                 ("dimension" ("ref". "A1:F4"))
                 ("sheetViews"
                  ("sheetView"
                   ("workbookViewId" . "0")
                   ("pane" ("ySplit" . "1") ("xSplit" . "1") ("topLeftCell" . "B2") ("activePane" . "bottomRight") ("state" . "frozen"))
                   ("selection" ("pane" . "bottomLeft"))
                   ("selection" ("pane" . "topRight"))
                   ("selection" ("pane" . "bottomRight"))))
                 ("sheetFormatPr" ("defaultRowHeight" . "13.5"))
                 ("cols"
                  ("col" ("min" . "1") ("max" . "2") ("width" . "50"))
                  ("col" ("min" . "3") ("max" . "4") ("width" . "8"))
                  ("col" ("min" . "5") ("max" . "5") ("width" . "14"))
                  ("col" ("min" . "6") ("max" . "6") ("width" . "8")))
                 ("sheetData"
                  ("row" ("r" . "1") ("spans" . "1:6")
                   ("c" ("r" . "A1") ("t" . "s") ("v" "16"))
                   ("c" ("r" . "B1") ("t" . "s") ("v" "1"))
                   ("c" ("r" . "C1") ("t" . "s") ("v" "2"))
                   ("c" ("r" . "D1") ("t" . "s") ("v" "3"))
                   ("c" ("r" . "E1") ("t" . "s") ("v" "4"))
                   ("c" ("r" . "F1") ("t" . "s") ("v" "5")))

                  ("row" ("r" . "2") ("spans" . "1:6")
                   ("c" ("r" . "A2") ("t" . "s") ("v" "8"))
                   ("c" ("r" . "B2") ("v" "100"))
                   ("c" ("r" . "C2") ("v" "300"))
                   ("c" ("r" . "D2") ("v" "200"))
                   ("c" ("r" . "E2") ("v" "0.6934"))
                   ("c" ("r" . "F2") ("v" "43360")))

                  ("row" ("r" . "3") ("spans" . "1:6")
                   ("c" ("r" . "A3") ("t" . "s") ("v" "13"))
                   ("c" ("r" . "B3") ("v" "200"))
                   ("c" ("r" . "C3") ("v" "400"))
                   ("c" ("r" . "D3") ("v" "300"))
                   ("c" ("r" . "E3") ("v" "139999.89223"))
                   ("c" ("r" . "F3") ("v" "43361")))

                  ("row" ("r" . "4") ("spans" . "1:6")
                   ("c" ("r" . "A4") ("t" . "s") ("v" "6"))
                   ("c" ("r" . "B4") ("v" "300"))
                   ("c" ("r" . "C4") ("v" "500"))
                   ("c" ("r" . "D4") ("v" "400"))
                   ("c" ("r" . "E4") ("v" "23.34"))
                   ("c" ("r" . "F4") ("v" "43362"))))

                 ("phoneticPr" ("fontId" . "1") ("type" . "noConversion"))
                 ("pageMargins" ("left" . "0.7") ("right" . "0.7") ("top" . "0.75") ("bottom" . "0.75") ("header" . "0.3") ("footer" . "0.3"))
	       ("pageSetup" ("paperSize" . "9") ("orientation" . "portrait") ("horizontalDpi" . "200") ("verticalDpi" . "200") ("r:id" . "rId1")))))
    (is (string= (lists-to-xml xml) (file-to-string *sheet*)))))

(test test-workbook
  (let ((xml-hash (xml-file-to-hash *workbook*
				    '("workbook.xmlns"
				      "workbook.xmlns:r"
				      "workbook.fileVersion.appName"
				      "workbook.fileVersion.lastEdited"
				      "workbook.fileVersion.lowestEdited"
				      "workbook.fileVersion.rupBuild"
				      "workbook.workbookPr.filterPrivacy"
				      "workbook.workbookPr.defaultThemeVersion"
				      "workbook.bookViews.workbookView.xWindow"
				      "workbook.bookViews.workbookView.yWindow"
				      "workbook.bookViews.workbookView.windowWidth"
				      "workbook.bookViews.workbookView.windowHeight"
				      "workbook.calcPr.calcId"
				      "workbook.sheets.sheet.name"
				      "workbook.sheets.sheet.sheetId"
				      "workbook.sheets.sheet.r:id"))))
    (is (string= (gethash "workbook1.xmlns1" xml-hash) "http://schemas.openxmlformats.org/spreadsheetml/2006/main"))
    (is (string= (gethash "workbook1.xmlns:r1" xml-hash) "http://schemas.openxmlformats.org/officeDocument/2006/relationships"))

    (is (string= (gethash "workbook1.fileVersion1.appName1" xml-hash) "xl"))
    (is (string= (gethash "workbook1.fileVersion1.lastEdited1" xml-hash) "4"))
    (is (string= (gethash "workbook1.fileVersion1.lowestEdited1" xml-hash) "4"))
    (is (string= (gethash "workbook1.fileVersion1.rupBuild1" xml-hash) "4505"))

    (is (string= (gethash "workbook1.workbookPr1.filterPrivacy1" xml-hash) "1"))

    (is (string= (gethash "workbook1.workbookPr1.defaultThemeVersion1" xml-hash) "124226"))
    (is (string= (gethash "workbook1.bookViews1.workbookView1.xWindow1" xml-hash) "0"))
    (is (string= (gethash "workbook1.bookViews1.workbookView1.yWindow1" xml-hash) "90"))
    (is (string= (gethash "workbook1.bookViews1.workbookView1.windowWidth1" xml-hash) "19200"))
    (is (string= (gethash "workbook1.bookViews1.workbookView1.windowHeight1" xml-hash) "10590"))

    (is (string= (gethash "workbook1.calcPr1.calcId1" xml-hash) "124519"))

    (is (string= (gethash "workbook1.sheets1.sheet1.name1" xml-hash) "DataSheet"))
    (is (string= (gethash "workbook1.sheets1.sheet2.name1" xml-hash) "DataSheetWithStyle"))
    (is (string= (gethash "workbook1.sheets1.sheet3.name1" xml-hash) "DataSheetWithStyle2"))
    (is (string= (gethash "workbook1.sheets1.sheet4.name1" xml-hash) "LineChart1"))
    (is (string= (gethash "workbook1.sheets1.sheet5.name1" xml-hash) "LineChart2"))
    (is (string= (gethash "workbook1.sheets1.sheet6.name1" xml-hash) "LineChart3D"))
    (is (string= (gethash "workbook1.sheets1.sheet7.name1" xml-hash) "BarChart"))
    (is (string= (gethash "workbook1.sheets1.sheet8.name1" xml-hash) "BarChart3D"))
    (is (string= (gethash "workbook1.sheets1.sheet9.name1" xml-hash) "PieChart"))
    (is (string= (gethash "workbook1.sheets1.sheet10.name1" xml-hash) "PieChart3D"))

    (is (string= (gethash "workbook1.sheets1.sheet1.sheetId1" xml-hash) "1"))
    (is (string= (gethash "workbook1.sheets1.sheet2.sheetId1" xml-hash) "2"))
    (is (string= (gethash "workbook1.sheets1.sheet3.sheetId1" xml-hash) "3"))
    (is (string= (gethash "workbook1.sheets1.sheet4.sheetId1" xml-hash) "4"))
    (is (string= (gethash "workbook1.sheets1.sheet5.sheetId1" xml-hash) "5"))
    (is (string= (gethash "workbook1.sheets1.sheet6.sheetId1" xml-hash) "6"))
    (is (string= (gethash "workbook1.sheets1.sheet7.sheetId1" xml-hash) "7"))
    (is (string= (gethash "workbook1.sheets1.sheet8.sheetId1" xml-hash) "8"))
    (is (string= (gethash "workbook1.sheets1.sheet9.sheetId1" xml-hash) "9"))
    (is (string= (gethash "workbook1.sheets1.sheet10.sheetId1" xml-hash) "10"))

    (is (string= (gethash "workbook1.sheets1.sheet1.r:id1" xml-hash) "rId1"))
    (is (string= (gethash "workbook1.sheets1.sheet2.r:id1" xml-hash) "rId2"))
    (is (string= (gethash "workbook1.sheets1.sheet3.r:id1" xml-hash) "rId3"))
    (is (string= (gethash "workbook1.sheets1.sheet4.r:id1" xml-hash) "rId4"))
    (is (string= (gethash "workbook1.sheets1.sheet5.r:id1" xml-hash) "rId5"))
    (is (string= (gethash "workbook1.sheets1.sheet6.r:id1" xml-hash) "rId6"))
    (is (string= (gethash "workbook1.sheets1.sheet7.r:id1" xml-hash) "rId7"))
    (is (string= (gethash "workbook1.sheets1.sheet8.r:id1" xml-hash) "rId8"))
    (is (string= (gethash "workbook1.sheets1.sheet9.r:id1" xml-hash) "rId9"))
    (is (string= (gethash "workbook1.sheets1.sheet10.r:id1" xml-hash) "rId10"))))

(test test-broken
  (let ((xml-hash (xml-file-to-hash *broken* '("worksheet.xmlns"))))
    (is (string= (gethash "worksheet1.xmlns1" xml-hash) "http://schemas.openxmlformats.org/spreadsheetml/2006/main")))
  
  (with-open-file (file-in *broken* :direction :input)
    (let ((xml-hash (xml-port-to-hash file-in '("worksheet.xmlns"))))
      (is (string= (gethash "worksheet1.xmlns1" xml-hash)  "http://schemas.openxmlformats.org/spreadsheetml/2006/main")))))


(def-suite basic
  :description "Test basic functions.")
(in-suite basic)

(defparameter *attr1* "attr1.xml")

(test test-attr1-xml
  (let ((xml-hash (xml-file-to-hash *attr1* '("worksheet.xmlns"
					      "worksheet.cols.test"
					      "worksheet.cols.col.collapsed"))))
    (is (= (hash-table-count xml-hash) 7))

    (is (= (gethash "worksheet's count" xml-hash) 1))

    (is (= (gethash "worksheet1.cols's count" xml-hash) 1))

    (is (= (gethash "worksheet1.cols1.col's count" xml-hash) 3))

    (is (string= (gethash "worksheet1.xmlns1" xml-hash) "http://schemas.openxmlformats.org/spreadsheetml/2006/main"))

    (is (string= (gethash "worksheet1.cols1.test1" xml-hash) "2"))

    (is (string= (gethash "worksheet1.cols1.col1.collapsed1" xml-hash) "1"))

    (is (string= (gethash "worksheet1.cols1.col2.collapsed1" xml-hash) "2"))))

(defparameter *empty1* "empty1.xml")
(defparameter *empty2* "empty2.xml")

(test test-empty1
  (let ((xml-hash (xml-file-to-hash *empty1* '("data.empty" "data.empty.attr1" "data.empty.attr2"))))
    (is (= (gethash "data's count" xml-hash) 1))
    (is (= (gethash "data1.empty's count" xml-hash) 5))
    (is (string= (gethash "data1.empty1" xml-hash) "1"))
    (is (string= (gethash "data1.empty2" xml-hash) "3"))
    (is (string= (gethash "data1.empty3" xml-hash) ""))
    (is (string= (gethash "data1.empty4" xml-hash) "4"))
    (is (string= (gethash "data1.empty5" xml-hash) ""))
    (is (string= (gethash "data1.empty1.attr11" xml-hash) "a1"))
    (is (string= (gethash "data1.empty1.attr21" xml-hash) "a2"))))

(test test-empty2
  (let ((xml-hash (xml-file-to-hash *empty2* '("empty" "empty.attr1" "empty.attr2"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (= (gethash "empty's count" xml-hash) 1))
    (is (string= (gethash "empty1" xml-hash) ""))
    (is (string= (gethash "empty1.attr11" xml-hash) "a1"))
    (is (string= (gethash "empty1.attr21" xml-hash) "a2"))))

(test write-empty
  (let ((xml '("data"
	       ("empty" "1" ("attr1" . "a1") ("attr2" . "a2"))
	       ("empty" "3")
	       ("empty" "")
	       ("empty" "4")
	       ("empty" ""))))
    (is (string= (lists-to-xml xml) (file-to-string *empty1*)))))

(defparameter *shared-strings* "sharedStrings.xml")
(defparameter *shared-strings-formatted* "sharedStrings_formated.xml")
(defparameter *shared-strings-compact* "sharedStrings_compact.xml")

(test test-shared-string
  (let ((xml-hash (xml-file-to-hash *shared-strings* '("sst.count"
						       "sst.uniqueCount"
						       "sst.xmlns"
						       "sst.si.t"
						       "sst.si.phoneticPr.fontId"
						       "sst.si.phoneticPr.type"))))
    (is (string= (gethash "sst1.count1" xml-hash) "17"))
    (is (string= (gethash "sst1.uniqueCount1" xml-hash) "17"))
    (is (string= (gethash "sst1.xmlns1" xml-hash) "http://schemas.openxmlformats.org/spreadsheetml/2006/main"))

    (is (string= (gethash "sst1.si1.t1" xml-hash) ""))
    (is (string= (gethash "sst1.si2.t1" xml-hash) "201601"))
    (is (string= (gethash "sst1.si3.t1" xml-hash) "201602"))
    (is (string= (gethash "sst1.si4.t1" xml-hash) "201603"))
    (is (string= (gethash "sst1.si5.t1" xml-hash) "201604"))
    (is (string= (gethash "sst1.si6.t1" xml-hash) "201605"))
    (is (string= (gethash "sst1.si7.t1" xml-hash) "Asics"))
    (is (string= (gethash "sst1.si8.t1" xml-hash) "Bottom"))
    (is (string= (gethash "sst1.si9.t1" xml-hash) "CAT"))
    (is (string= (gethash "sst1.si10.t1" xml-hash) "Center"))
    (is (string= (gethash "sst1.si11.t1" xml-hash) "Center/Middle"))
    (is (string= (gethash "sst1.si12.t1" xml-hash) "Left"))
    (is (string= (gethash "sst1.si13.t1" xml-hash) "Middle"))
    (is (string= (gethash "sst1.si14.t1" xml-hash) "Puma"))
    (is (string= (gethash "sst1.si15.t1" xml-hash) "Right"))
    (is (string= (gethash "sst1.si16.t1" xml-hash) "Top"))
    (is (string= (gethash "sst1.si17.t1" xml-hash) "month/brand"))

    (is (string= (gethash "sst1.si1.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si2.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si3.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si4.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si5.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si6.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si7.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si8.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si9.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si10.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si11.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si12.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si13.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si14.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si15.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si16.phoneticPr1.fontId1" xml-hash) "1"))
    (is (string= (gethash "sst1.si17.phoneticPr1.fontId1" xml-hash) "1"))

    (is (string= (gethash "sst1.si1.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si2.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si3.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si4.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si5.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si6.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si7.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si8.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si9.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si10.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si11.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si12.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si13.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si14.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si15.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si16.phoneticPr1.type1" xml-hash) "noConversion"))
    (is (string= (gethash "sst1.si17.phoneticPr1.type1" xml-hash) "noConversion"))))

(test write-shared-string
  (let ((xml '("sst"
	       ("xmlns" . "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
	       ("count" . "17")
	       ("uniqueCount" . "17")
	       ("si" ("t") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "201601") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "201602") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "201603") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "201604") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "201605") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Asics") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Bottom") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "CAT") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Center") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Center/Middle") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Left") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Middle") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Puma") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Right") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "Top") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion")))
	       ("si" ("t" "month/brand") ("phoneticPr" ("fontId" . "1") ("type" . "noConversion"))))))
    (is (string= (lists-to-xml xml) (file-to-string *shared-strings-formatted*)))
    (is (string= (lists-to-compact-xml xml) (file-to-string *shared-strings-compact*)))))

(defparameter *simple1* "simple1.xml")
(defparameter *simple2* "simple2.xml")
(defparameter *simple3* "simple3.xml")
(defparameter *simple4* "simple4.xml")
(defparameter *simple5* "simple5.xml")

(test test-simple
  (let ((xml-hash (xml-file-to-hash *simple1* '("?xml.version" "?xml.encoding" "basic1.value"))))
    (is (= (hash-table-count xml-hash) 6))
    (is (= (gethash "?xml's count" xml-hash) 1))
    (is (string= (gethash "?xml1.version1" xml-hash) "1.0"))
    (is (string= (gethash "?xml1.encoding1" xml-hash) "UTF-8"))
    (is (= (gethash "basic1's count" xml-hash) 1))
    (is (= (gethash "basic11.value's count" xml-hash) 1))
    (is (string= (gethash "basic11.value1" xml-hash) "1")))

  (let ((xml-hash (xml-file-to-hash *simple2* '("h1.h2.topic"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (= (gethash "h1's count" xml-hash) 1))
    (is (= (gethash "h11.h2's count" xml-hash) 1))
    (is (= (gethash "h11.h21.topic's count" xml-hash) 1))
    (is (string= (gethash "h11.h21.topic1" xml-hash) "cx")))

  (let ((xml-hash (xml-file-to-hash *simple3* '("h1.h2.topic"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (= (gethash "h1's count" xml-hash) 1))
    (is (= (gethash "h11.h2's count" xml-hash) 1))
    (is (= (gethash "h11.h21.topic's count" xml-hash) 1))
    (is (string= (gethash "h11.h21.topic1" xml-hash) "cx")))

  (let ((xml-hash  (xml-file-to-hash *simple4* '("h1.h2.topic" "h1"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (= (gethash "h1's count" xml-hash) 1))
    (is (= (gethash "h11.h2's count" xml-hash) 1))
    (is (= (gethash "h11.h21.topic's count" xml-hash) 1))
    (is (string= (gethash "h11.h21.topic1" xml-hash) "cx")))

  (let ((xml-hash (xml-file-to-hash *simple5* '("h1.h2.topic"))))
    (is (= (hash-table-count xml-hash) 4))
    (is (= (gethash "h1's count" xml-hash) 1))
    (is (= (gethash "h11.h2's count" xml-hash) 1))
    (is (= (gethash "h11.h21.topic's count" xml-hash) 1))
    (is (string= (gethash "h11.h21.topic1" xml-hash) " cx "))))

(run! 'lib)
(run! 'special-chars)
(run! 'test-lists-to-xml)
(run! 'basic)


