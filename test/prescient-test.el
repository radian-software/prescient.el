;;; prescient-test.el --- ERT tests -*- lexical-binding: t -*-

(require 'prescient)

;; Stolen from straight.el, credit to @progfolio
;; https://github.com/radian-software/straight.el/blob/ff63b154bef1ef8d92c141bd189001bff74f6982/tests/straight-test.el#L10-L74

(eval-and-compile
  (defmacro prescient-test--template (template &optional vars &rest bindings)
    "Return a list of filled TEMPLATEs.
TEMPLATE is an implicitly backquoted form.
VARS should be a list of symbols denoting the destructuring pattern
for BINDINGS."
    (declare (indent 1))
    (if (or (null vars) (null bindings))
        (list template)
      (let ((unbound (mod (length bindings) (length vars))))
        (unless (zerop unbound)
          (error "Unven binding list: %S" (last bindings unbound)))
        (let ((body nil)
              (bindings
               (eval
                `(cl-loop for ,vars on ',bindings
                          by (lambda (l) (nthcdr ,(length vars) l))
                          collect
                          (apply #'append
                                 (cl-mapcar #'list ',vars (list ,@vars)))))))
          (dolist (env bindings (mapcar (lambda (it) (eval it t))
                                        (nreverse body)))
            (unless (mod (length env) 2) (error "Uneven binding list: %S" env))
            (let (e)
              (cl-loop for (var val) on env by #'cddr
                       do (push (list var `(quote ,val)) e))
              (push `(let* ,(nreverse e) (backquote ,template)) body))))))))

(cl-defmacro prescient-deftest (object
                               (&key before-each after-each expected-result
                                     doc tags &allow-other-keys)
                               &rest template)
  "Return auto-tagged and documented `ert-deftest' for OBJECT with TEMPLATE."
  (declare (indent defun))
  (let ((counter 0)
        (autotags
         (delq nil
               (list
                object
                (if (string-match-p "--" (symbol-name object))
                    'private 'public)
                (if (macrop object) 'macro))))
        (tests (when template
                 (macroexpand `(prescient-test--template ,@template)))))
    (setq tags (append autotags tags))
    `(progn
       ,@(mapcar
          (lambda (test)
            `(ert-deftest
                 ,(intern (concat
                           (format "%s/test" object)
                           (when (> (length tests) 1)
                             (format "@%d" (cl-incf counter)))))
                 ()
               ,(or doc (when (fboundp object) (documentation object)))
               ,@(when tags `(:tags ',tags))
               ,@(when expected-result `(:expected-result ,expected-result))
               ,@(when before-each (if (cl-every #'listp before-each)
                                       before-each
                                     (list before-each)))
               ,test
               ,@(when after-each (if (cl-every #'listp after-each)
                                      after-each
                                    (list after-each)))))
          tests))))

(font-lock-add-keywords
 nil
 '(("(\\(\\<prescient-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face nil t)
    (2 font-lock-function-name-face nil t))))

;;; Hacks and utilities

(defmacro prescient-test--stateless (&rest body)
  "Exec BODY ignoring existing recency data."
  (declare (indent 0))
  `(let ((prescient--history (make-hash-table :test 'equal))
         (prescient--frequency (make-hash-table :test 'equal)))
     ,@body))

;;; Begin tests

(prescient-deftest prescient-split-query ()
  (should (equal ,result (prescient-split-query ,query)))
  (query            result)
  "foo"             '("foo")
  "foo bar"         '("foo" "bar")
  "foo  bar"        '("foo bar")
  "foo   bar"       '("foo  bar")
  " foo bar "       '("foo" "bar")
  "  foo bar  "     '(" foo" "bar ")
  "  foo  bar  "    '(" foo bar ")
  "foo bar baz"     '("foo" "bar" "baz")
  "foo  bar baz"    '("foo bar" "baz")
  "foo bar  baz"    '("foo" "bar baz")
  )

(prescient-deftest prescient-filter-regexps ()
  (let ((prescient-filter-method ,methods)
        ;; Simplify the regexes in the test cases by disabling char
        ;; folding
        (prescient-use-char-folding nil))
    (should (equal ,result (prescient-filter-regexps ,query ,with-group ,separated))))
  (methods               with-group separated  query              result)
  '(literal)             nil        nil        "foo"              '("foo")
  '(literal)             nil        nil        "foo bar"          '("foo" "bar")
  '(literal)             nil        nil        "foo  bar"         '("foo bar")
  '(literal)             t          nil        "hello"            '("hello")
  '(literal)             'all       nil        "hello"            '("\\(hello\\)")
  '(literal)             'all       nil        "hello world"      '("\\(hello\\)" "\\(world\\)")
  '(literal)             nil        nil        "**[amaze]**"      '("\\*\\*\\[amaze]\\*\\*")
  '(initialism)          nil        nil        "hai"              '("\\bh\\w*\\W*\\ba\\w*\\W*\\bi\\w*")
  '(initialism)          t          nil        "hai"              '("\\b\\(h\\)\\w*\\W*\\b\\(a\\)\\w*\\W*\\b\\(i\\)\\w*")
  '(literal initialism)  nil        nil        "hai"              '("hai\\|\\bh\\w*\\W*\\ba\\w*\\W*\\bi\\w*")
  '(literal initialism)  t          nil        "hai"              '("hai\\|\\b\\(h\\)\\w*\\W*\\b\\(a\\)\\w*\\W*\\b\\(i\\)\\w*")
  '(literal initialism)  'all       nil        "hai"              '("\\(hai\\)\\|\\b\\(h\\)\\w*\\W*\\b\\(a\\)\\w*\\W*\\b\\(i\\)\\w*")
  '(literal initialism)  nil        t          "hai"              '("hai" "\\bh\\w*\\W*\\ba\\w*\\W*\\bi\\w*")
  '(literal regexp)      nil        nil        "f.*d b[o]*t"      '("f\\.\\*d\\|f.*d" "b\\[o]\\*t\\|b[o]*t")
  '(literal regexp)      nil        t          "f.*d b[o]*t"      '("f\\.\\*d" "f.*d" "b\\[o]\\*t" "b[o]*t")
  '(prefix)              nil        nil        "re"               '("\\<re[[:word:]]*?")
  '(prefix)              t          nil        "re"               '("\\<\\(re\\)[[:word:]]*?")
  '(prefix)              nil        nil        "r-e"              '("\\<r[[:word:]]*-e[[:word:]]*")
  '(prefix)              t          nil        "r-e"              '("\\<\\(r\\)[[:word:]]*-\\(e\\)[[:word:]]*")
  '(prefix)              nil        nil        "r.*e"             '("\\<r[[:word:]]*\\.\\*e[[:word:]]*")
  '(prefix)              nil        nil        ".re"              '("\\.re[[:word:]]*")
  '(prefix)              t          nil        ".re"              '("\\.\\(re\\)[[:word:]]*")
  )

(prescient-deftest prescient-sort-full-matches-first ()
  (let ((prescient-filter-method ,methods)
        (prescient-sort-full-matches-first ,full-match-first))
    (prescient-test--stateless
      (should (equal ,result (prescient-completion-sort
                              (prescient-filter ,query ,candidates))))))
  (candidates methods full-match-first query result)
  '("rebar" "restart-emacs" "barracuda") '(literal initialism) nil "bar" '("rebar" "barracuda")
  '("rebar" "restart-emacs" "barracuda") '(literal initialism) nil "re"  '("rebar" "restart-emacs")
  '("rebar" "restart-emacs" "barracuda") '(literal initialism) t   "re"  '("restart-emacs" "rebar")
  '("rebar" "restart-emacs" "barracuda") '(prefix  initialism) t   "re"  '("restart-emacs" "rebar")
  '("string-rectangle-word" "string-rectangle" "term-string-rectangle") '(prefix) t   "s-r"  '("string-rectangle" "string-rectangle-word" "term-string-rectangle")
  '("test.el" ".elpaignore" "barracuda") '(prefix)             t   ".e"   '(".elpaignore" "test.el")
  '("test.el" ".elpaignore" "barracuda") '(prefix)             nil ".e"   '("test.el" ".elpaignore")
  )

(defun prescient-test--tiebreak-length-increasing (c1 c2)
  (- (length c1) (length c2)))

(defun prescient-test--tiebreak-length-decreasing (c1 c2)
  (- (length c2) (length c1)))

(defun prescient-test--tiebreak-first-letter-index (c1 c2)
  "Weird tiebreaking function.
It prioritizes candidates based on how close the first letter of
the search query is to the front of the candidate. The main
purpose is to test that tiebreak functions are able to access the
search query properly."
  (if (or (null prescient-query) (string-empty-p prescient-query))
      0
    (- (or (string-match (substring prescient-query 0 1) c1) 999)
       (or (string-match (substring prescient-query 0 1) c2) 999))))

(prescient-deftest prescient-tiebreaker ()
  (let ((prescient-sort-length-enable ,len-enable)
        (prescient-tiebreaker ,tiebreaker))
    (prescient-test--stateless
      (should (equal ,result (prescient-completion-sort
                              (prescient-filter ,query ,candidates))))))
  (candidates len-enable query tiebreaker result)
  '("first" "second" "third" "fourth" "fifth" "seventh") nil "" nil '("first" "second" "third" "fourth" "fifth" "seventh")
  '("first" "second" "third" "fourth" "fifth" "seventh") t   "" nil '("first" "third" "fifth" "second" "fourth" "seventh")
  '("first" "second" "third" "fourth" "fifth" "seventh") nil "" #'prescient-test--tiebreak-length-increasing '("first" "third" "fifth" "second" "fourth" "seventh")
  '("first" "second" "third" "fourth" "fifth" "seventh") nil "" #'prescient-test--tiebreak-length-decreasing '("seventh" "second" "fourth" "first" "third" "fifth")
  '("first" "second" "third" "fourth" "fifth" "seventh") nil "h" nil '("third" "fourth" "fifth" "seventh")
  '("first" "second" "third" "fourth" "fifth" "seventh") nil "h" #'prescient-test--tiebreak-first-letter-index '("third" "fifth" "fourth" "seventh")
  )
