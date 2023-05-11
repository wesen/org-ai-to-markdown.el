;;; org-ai-to-md.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Manuel Odendahl
;;
;; Author: Manuel Odendahl <wesen@ruinwesen.com>
;; Maintainer: Manuel Odendahl <wesen@ruinwesen.com>
;; Created: May 10, 2023
;; Modified: May 10, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wesen/org-ai-to-markdown
;; Package-Requires: ((emacs "24.3") (s "1.13.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(require 'cl-lib)
(require 's)

(cl-defstruct org-ai-to-md-state
  "Represents the internal state of the state machine used to convert org-ai flavoured org mode files to markdown."
  (state
   :normal
   :type symbol
   :documentation "The state variable.
Can be:
- :normal (no special context)
- :in-dialogue (within a begin_ai block)
- :in-dialogue-src (within a code block inside a dialogue)
- in-src (within a begin_src code block)
- in-results (within the results of evaluating a code block)")
  (current-speaker
   nil
   :type string
   :documentation "The current speaker if dialogue is active and a [..] tag was recognized.")
  (current-src-language
   nil
   :type string
   :documentation "When inside a code block, the current language (used for syntax highlighting)"))

(defun org-ai-to-md--handle-lines (state output-buffer lines)
  "The main driver of the converter.
Iterates over the lines and accumulates them by calling HANDLE-LINE."
  (dolist (line lines state)
    (setq state (org-ai-to-md--handle-lines state output-buffer line))))


(defun org-ai-to-md--is-begin-ai-block-p (s)
  "Return T if the line S is the begin of an org-ai block."
  (s-matches-p "^[\s\t]*#\\+begin_ai" s))

(defun org-ai-to-md--is-end-ai-block-p (s)
  "Return T if the line S is the end of an org-ai block."
  (s-matches-p "^[\s\t]*#\\+end_ai" s))

(defun org-ai-to-md--is-begin-src-block-p (s)
  "Return T if the line S is the beginning of an org-mode source block."
  (s-matches-p "^[\s\t]*#\\+begin_src" s))

(defun org-ai-to-md--is-end-src-block-p (s)
  "Return T if the line S is the end of an org-mode source block."
  (s-matches-p "^[\s\t]*#\\+end_src" s))

(defvar org-ai-to-md--md-regexp
  "^[\s\t]*```\\([a-zA-Z0-9-_]*\\)?[\s\t]*$")

(defun org-ai-to-md--is-markdown-quote-p (s)
  "Return T if the line is a triple-backquote markdown source block."
  (s-matches-p org-ai-to-md--md-regexp s))

(defun org-ai-to-md--get-markdown-language (s)
  "Return the language of the markdown quote S (if no language is present,
returns \"\"."
  (let ((match (s-match org-ai-to-md--md-regexp s)))
    (if match
        (nth 1 match)
      "")))

(defun org-ai-to-md--get-ai-dialogue-change (s &optional partners)
  "Return the speaker change in the dialogue S (in the form [ME]) or nil if no
change took place. PARTNERS is a list of valid partners (default: \"SYS\", \"ME\",
\"AI\")."
  (let* ((valid-partner-re (format "\\(?:%s\\)"
                                   (s-join "\\|" (or partners '("SYS" "ME" "AI")))))
         (re (format "^\s*\\[\\(%s\\)\\]" valid-partner-re))
         (match (s-match re s)))
    (message re)
    (if match
        (nth 1 match)
      nil)))

(provide 'org-ai-to-md)
;;; org-ai-to-md.el ends here
