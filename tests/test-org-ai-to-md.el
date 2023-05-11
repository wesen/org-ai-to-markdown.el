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
;; Package-Requires: ((emacs "24.3") (buttercup "1.29"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'buttercup)
(require 'org-ai-to-md)

(describe "org-ai-to-md--is-begin-ai-block-p"
  (it "handles empty input string"
    (expect (org-ai-to-md--is-begin-ai-block-p "") :to-be nil))

  (it "handles single space before #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p " #+begin_ai") :to-be t))

  (it "handles multiple spaces before #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "  #+begin_ai") :to-be t))

  (it "handles tab(s) before #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "\t#+begin_ai") :to-be t))

  (it "handles leading non-space characters before #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "a#+begin_ai") :to-be nil))

  (it "handles mixed whitespace characters before #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p " \t#+begin_ai") :to-be t))

  (it "handles line containing only #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai") :to-be t))

  (it "handles #+begin_ai in the middle of the line"
    (expect (org-ai-to-md--is-begin-ai-block-p "text #+begin_ai") :to-be nil))

  (it "tests case-insensitive matching"
    (expect (org-ai-to-md--is-begin-ai-block-p "#+BEGIN_AI") :to-be t)
    (expect (org-ai-to-md--is-begin-ai-block-p "#+Begin_AI") :to-be t))

  (it "handles multiple appearances of #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai some text #+begin_ai") :to-be t))

  (it "handles whitespace-only or empty lines"
    (expect (org-ai-to-md--is-begin-ai-block-p "  ") :to-be nil)
    (expect (org-ai-to-md--is-begin-ai-block-p "\n") :to-be nil))

  (it "handles whitespace after #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai ") :to-be t)
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai\t") :to-be t)
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai  \t") :to-be t))

  (it "handles further text after #+begin_ai"
    (expect (org-ai-to-md--is-begin-ai-block-p "#+begin_ai extra text here") :to-be t)))

(describe "is-markdown-quote-p"
  (it "handle empty input string"
    (expect (org-ai-to-md--is-markdown-quote-p "") :to-be nil))

  (it "handle triple backquote at start of line on the line"
    (expect (org-ai-to-md--is-markdown-quote-p "```") :to-be t)
    (expect (org-ai-to-md--is-markdown-quote-p "``` ") :to-be t))

  (it "handle triple backquote with language appended"
    (expect (org-ai-to-md--is-markdown-quote-p "```elisp") :to-be t)
    (expect (org-ai-to-md--is-markdown-quote-p "```elisp ") :to-be t))

  (it "handle after language strings"
    (expect (org-ai-to-md--is-markdown-quote-p "```elisp elisp") :to-be nil))

  (if "handle invalid space before language name"
    (expect (org-ai-to-md--is-markdown-quote-p "``` elisp") :to-be nil))

  (it "handle whitespace in front of triple backquote"
    (expect (org-ai-to-md--is-markdown-quote-p " ```") :to-be t)
    (expect (org-ai-to-md--is-markdown-quote-p "  ```elisp ") :to-be t)))

(describe "get-markdown-language"
  (it "handle empty string"
    (expect (org-ai-to-md--get-markdown-language "") :to-be ""))

  (it "handle no language"
    (expect (org-ai-to-md--get-markdown-language "```") :to-be "")
    (expect (org-ai-to-md--get-markdown-language " ``` ") :to-be ""))

  (it "handle simple language"
    (expect (org-ai-to-md--get-markdown-language "```elisp") :to-equal "elisp")
    (expect (org-ai-to-md--get-markdown-language " ```elisp ") :to-equal "elisp"))

  (it "handle language with - and _"
    (expect (org-ai-to-md--get-markdown-language "```emacs-lisp") :to-equal "emacs-lisp")
    (expect (org-ai-to-md--get-markdown-language " ```emacs_lisp ") :to-equal "emacs_lisp"))

  (it "invalid string with two words"
    (expect (org-ai-to-md--get-markdown-language "```elisp elisp") :to-equal "")))

(describe "test-dialogue-change"
  (it "handle empty string"
    (expect (org-ai-to-md--get-ai-dialogue-change "") :to-be nil))

  (it "handle string with no dialogue change"
    (expect (org-ai-to-md--get-ai-dialogue-change "some text") :to-be nil))

  (it "handle string with dialogue change of valid partner"
    (expect (org-ai-to-md--get-ai-dialogue-change "[AI]:") :to-equal "AI")
    (expect (org-ai-to-md--get-ai-dialogue-change "[ME]:") :to-equal "ME")
    (expect (org-ai-to-md--get-ai-dialogue-change "[SYS]:") :to-equal "SYS"))

  (it "handle valid partner with whitespace up front"
    (expect (org-ai-to-md--get-ai-dialogue-change " [AI]:") :to-equal "AI")
    (expect (org-ai-to-md--get-ai-dialogue-change " [ME]:") :to-equal "ME")
    (expect (org-ai-to-md--get-ai-dialogue-change " [SYS]:") :to-equal "SYS"))

  (it "handle valid partner followed with conversation"
    (expect (org-ai-to-md--get-ai-dialogue-change "[AI]: some text") :to-equal "AI")
    (expect (org-ai-to-md--get-ai-dialogue-change "[ME]: some text") :to-equal "ME")
    (expect (org-ai-to-md--get-ai-dialogue-change "[SYS]: some text") :to-equal "SYS"))

  (it "handle valid partner followed by conversation with [] symbols"
    (expect (org-ai-to-md--get-ai-dialogue-change "[AI]: [some text]") :to-equal "AI")
    (expect (org-ai-to-md--get-ai-dialogue-change "[ME]: [some text]") :to-equal "ME")
    (expect (org-ai-to-md--get-ai-dialogue-change "[SYS]: [some text]") :to-equal "SYS"))

  (it "handle valid partner with preceding text (invalid dialogue)"
    (expect (org-ai-to-md--get-ai-dialogue-change "some text [AI]:") :to-be nil)
    (expect (org-ai-to-md--get-ai-dialogue-change "some text [ME]:") :to-be nil)
    (expect (org-ai-to-md--get-ai-dialogue-change "some text [SYS]:") :to-be nil))

  (it "handle string with [] but not a valid dialogue partner"
    (expect (org-ai-to-md--get-ai-dialogue-change "[some text]") :to-be nil)))

(provide 'test-org-ai-to-md)

;;; test-org-ai-to-md.el ends here
