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
;; Package-Requires: ((emacs "26.1") (s "1.13.1") (dash "2.19.1"))
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
(require 'dash)

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
- :in-markdown-quote
- :in-src (within a begin_src code block)
- :in-results (within the results of evaluating a code block)")
  (current-speaker
   nil
   :type string
   :documentation "The current speaker if dialogue is active and a [..] tag was recognized.")
  (current-src-language
   nil
   :type string
   :documentation "When inside a code block, the current language (used for syntax highlighting)"))

(defun org-ai-to-md--handle-lines (state lines)
  "The main driver of the converter.
Iterates over the lines and processes them by calling HANDLE-LINE."
       (dolist (line lines state)
         (setq state (org-ai-to-md--handle-line state line))))

;;; String helper functions
(defun org-ai-to-md--make-multi-choice-re (choices)
  "Return a regular expression that matches any of the strings in CHOICES."
  (format "\\(?:%s\\)" (s-join "\\|" choices)))

(defun org-ai-to-md--string-to-lower-uppercase-re (s)
  "Converts the string S to a regular expression that matches the string in
lowercase or uppercase."
  (if (equal s "") ""
    (org-ai-to-md--make-multi-choice-re (list (downcase s) (upcase s)))))

;;; Parsing functions

;; These functions are there to recognize the different strings that lead to transitions
;; of the state machine

(defun org-ai-to-md--is-begin-ai-block-p (s)
  "Return T if the line S is the begin of an org-ai block."
  (s-matches-p "^[\s\t]*#\\+begin_ai" s))

(defun org-ai-to-md--is-end-ai-block-p (s)
  "Return T if the line S is the end of an org-ai block."
  (s-matches-p "^[\s\t]*#\\+end_ai" s))

(defun org-ai-to-md--is-begin-src-block-p (s)
  "Return T if the line S is the beginning of an org-mode source block."
  (s-matches-p "^[\s\t]*#\\+begin_src" s))

(defun org-ai-to-md--get-src-language (s)
  "Return the language of the source block S (if no language is present,
returns \"\"."
  (let ((match (s-match "^[\s\t]*#\\+begin_src[\s\t]+\\([a-zA-Z0-9-_]*\\)" s)))
    (if match
        (nth 1 match)
      "")))

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

(defun org-ai-to-md--is-dialogue-change-p (s &optional partners)
  "Return T if the line S is a dialogue change (in the form [ME]) or nil if no."
  (s-matches-p (org-ai-to-md--get-dialogue-partners-re partners) s))

(defun org-ai-to-md--get-dialogue-partners-re (partners)
  "Return a regular expression that matches any of the strings in PARTNERS."
  (format "^\s*\\[\\(%s\\)\\]"
          (org-ai-to-md--make-multi-choice-re (or partners '("SYS" "ME" "AI")))))

(defun org-ai-to-md--get-ai-dialogue-change (s &optional partners)
  "Return the speaker change in the dialogue S (in the form [ME]) or nil if no
change took place. PARTNERS is a list of valid partners (default: \"SYS\", \"ME\",
\"AI\")."
  (let* ((re (org-ai-to-md--get-dialogue-partners-re partners))
         (match (s-match re s)))
    (if match
        (nth 1 match)
      nil)))

(defun org-ai-to-md--is-heading-p (s)
  "Returns T if the line S is an org-mode heading."
  (s-matches-p "^[\s\t]*\\*+" s))

(defun org-ai-to-md--get-heading-and-level (s)
  "Returns a cons cell (HEADING . LEVEL) if the line S is a heading, nil otherwise."
  (let ((match (s-match "^[\s\t]*\\*+[\s\t]+\\(.*\\)$" s)))
    (if match
        (cons (nth 2 match) (length (nth 1 match)))
      nil)))

(defun org-ai-to-md--get-title (s)
  "Returns T if the line S is an org-mode title."
  (let* ((case-fold-search nil)
         (match
          (s-match (format "^[\s\t]*#\\+%s:[\s\t]*\\(.*\\)$"
                           (org-ai-to-md--string-to-lower-uppercase-re "title")) s)))
    (if match
        (nth 1 match)
      "")))

(defun org-ai-to-md--is-title-p (s)
  "Returns the content if the title if the line is a title."
  (let ((case-fold-search nil))
    (s-matches-p (format "^[\s\t]*#\\+%s:" (org-ai-to-md--string-to-lower-uppercase-re "title")) s)))

;;; State machine

(defun org-ai-to-md--keyword-to-package-function (k)
  "Converts K to the symbol `org-ai-to-md--X` and looks up its function value."
  (let* ((name (symbol-name k))
         (function-symbol (intern (format "org-ai-to-md--is-%s-p"
                                          (if (keywordp k)
                                              (s-chop-left 1 name)
                                            name))))
         (function-value (symbol-function function-symbol)))
    (if (functionp function-value)
        function-value
      nil)))

;; NOTE(manuel, 2023-05-11)

;; I am going to make an overengineered state machine thingie just for the fun of it.
;; Each state only needs a few valid transitions to continue. I want each state to specify
;; a symbol of the parser functions it uses, and then use the properly package named function
;; for it.

;; We should make a structure for the actual state (as a handler, not as data) itself,
;; which would have a list of parsing functions it can recognize
;; and a dispatch function. We could replace the state symbol with the state struct itself
;; or we could keep them in a lookup table.

(cl-defstruct org-ai-to-md-state-handler
  name
  transition-checks
  fn)

;; TODO(manuel, 2023-05-12) manuel you idiot, you should really just use a generic method
;; on the state handle struct...

(defun test-pcase (state line line-type)
  (pcase line-type
    (:title (let ((s2 (s-concat "-" line)))
              s2))
    (:foobar (setq (org-ai-to-md-state-state state) :in-dialogue))
    (_ (s-concat "-" line))))

;; All handler functions expect to have a the current buffer be the output markdown
;; buffer.

(defun org-ai-to-md--handle-state-normal (state line line-type)
  "Handle the :NORMAL state. We convert titles and headings to their
corresponding markdown equivalent, and handle AI, src and markdown quote code
blocks by converting them to the same markdown."
  (pcase line-type
    (:title (insert (format "# %s" (org-ai-to-md--get-title line))))
    (:heading (let* ((heading-and-level (org-ai-to-md--get-heading-and-level line))
                     (heading (car heading-and-level))
                     (level (cdr heading-and-level)))
                (insert (format "%s %s\n" (make-string level ?#) heading))))
    (:begin-ai (progn
                 (setq (org-ai-to-md-state-current-speaker state) nil
                       (org-ai-to-md-state-state state) :in-dialogue)))
    (:begin-src (let ((language (org-ai-to-md--get-src-language line)))
                  (setq (org-ai-to-md-state-state state) :in-src)
                  (insert (format "```%s\n" language))))
    (:markdown-quote (let ((language (org-ai-to-md--get-markdown-language line)))
                       (setq (org-ai-to-md-state-state state) :in-markdown-quote)
                       (insert (format "```%s\n" language))))
    (:begin-results
     ;; For now, let's just ignore results
     (insert line))
    (_ (insert line))))

(defun org-ai-to-md--handle-state-in-dialogue (state line line-type)
  "Handle the :IN-DIALOGUE state. We convert dialogue lines to markdown
by prefixing them with the quote > symbol."
  (pcase line-type
    (:end-ai (progn
               (insert "\n")
               (setq (org-ai-to-md-state-state state) :normal)))
    (:dialogue-change
     (let ((speaker (org-ai-to-md--get-ai-dialogue-change line)))
       (setq (org-ai-to-md-state-current-speaker state) speaker))
     ;; insert a new line with no quote symbol when switching speakers
     (insert (format "\n> %s" line)))
    ;; NOTE(manuel, 2023-05-12)
    ;; For now, no special handling of markdown quote in AI responses.
    ;; We could however leverage this in the future to extract code from responses.
    (_ (insert (format "> %s" line)))))

(defvar org-ai-to-md--state-handlers
  (let ((handlers
         (list
          (make-org-ai-to-md-state-handler
           :name :normal
           :transition-checks '(:title :heading :begin-ai :begin-src :markdown-quote :begin-results)
           :fn #'org-ai-to-md--handle-state-normal))))
    (-map #'(lambda (handler)
              (cons (org-ai-to-md-state-handler-name handler) handler))
          handlers)))

(defvar org-ai-to-md--state-transition-checks
  '((:normal . (:title :heading :begin-ai :begin-src :markdown-quote :begin-results))
    (:in-dialogue . (:end-ai :dialogue-change :markdown-quote))
    (:in-dialogue-src . (:markdown-quote))
    (:in-src . (:end-src))
    (:in-results . (:end-results))))

;; (defun org-ai-to-md--handle-line (state output-buffer line)
;;   "Handles the line LINE in the state STATE. Returns the new state."
;;   (let* ((current-state (org-ai-to-md-state-state state))
;;          (transition-checks (alist-get current-state org-ai-to-md--state-transition-checks))
;;          (transition (org-ai-to-md--get-transition transition-checks line)))))

(provide 'org-ai-to-md)
;;; org-ai-to-md.el ends here
