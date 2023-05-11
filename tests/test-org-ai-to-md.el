;;; test-org-ai-to-md.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Manuel Odendahl
;;
;; Author: Manuel Odendahl <wesen@ruinwesen.com>
;; Maintainer: Manuel Odendahl <wesen@ruinwesen.com>
;; Created: May 11, 2023
;; Modified: May 11, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wesen/test-org-ai-to-md
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(describe "is-begin-ai-block-p"
  (it "handles empty input string"
    (expect (is-begin-ai-block-p "") :to-be nil))

  (it "handles single space before #+begin_ai"
    (expect (is-begin-ai-block-p " #+begin_ai") :to-be t))

  (it "handles multiple spaces before #+begin_ai"
    (expect (is-begin-ai-block-p "  #+begin_ai") :to-be t))

  (it "handles tab(s) before #+begin_ai"
    (expect (is-begin-ai-block-p "\t#+begin_ai") :to-be t))

  (it "handles leading non-space characters before #+begin_ai"
    (expect (is-begin-ai-block-p "a#+begin_ai") :to-be nil))

  (it "handles mixed whitespace characters before #+begin_ai"
    (expect (is-begin-ai-block-p " \t#+begin_ai") :to-be t))

  (it "handles line containing only #+begin_ai"
    (expect (is-begin-ai-block-p "#+begin_ai") :to-be t))

  (it "handles #+begin_ai in the middle of the line"
    (expect (is-begin-ai-block-p "text #+begin_ai") :to-be nil))

  (it "tests case-insensitive matching"
    (expect (is-begin-ai-block-p "#+BEGIN_AI") :to-be t)
    (expect (is-begin-ai-block-p "#+Begin_AI") :to-be t))

  (it "handles multiple appearances of #+begin_ai"
    (expect (is-begin-ai-block-p "#+begin_ai some text #+begin_ai") :to-be t))

  (it "handles whitespace-only or empty lines"
    (expect (is-begin-ai-block-p "  ") :to-be nil)
    (expect (is-begin-ai-block-p "\n") :to-be nil))

  (it "handles whitespace after #+begin_ai"
    (expect (is-begin-ai-block-p "#+begin_ai ") :to-be t)
    (expect (is-begin-ai-block-p "#+begin_ai\t") :to-be t)
    (expect (is-begin-ai-block-p "#+begin_ai  \t") :to-be t))

  (it "handles further text after #+begin_ai"
    (expect (is-begin-ai-block-p "#+begin_ai extra text here") :to-be t)))

(describe "is-markdown-quote-p")

(provide 'test-org-ai-to-md)

;;; test-org-ai-to-md.el ends here
