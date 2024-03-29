(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((foob . 3)
     (+format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode tex-mode latex-mode org-msg-edit-mode ess-r-mode poly-markdown+r-mode poly-markdown+R-mode markdown-mode))
     (+format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode tex-mode latex-mode org-msg-edit-mode ess-r-mode poly-markdown+r-mode poly-markdown+R-mode))
     (+format-on-save-enabled-modes
      '(not emacs-lisp-mode sql-mode tex-mode latex-mode org-msg-edit-mode ess-r-mode poly-markdown+r-mode))
     (+org-capture-projects-file . "~/allHail/delphi/projects.org")
     (comment-add . 0)
     (lsp--override-calculate-lisp-indent\? . t)
     (flycheck-disabled-checkers quote
      (emacs-lisp-checkdoc))
     (eval progn
      (let
          ((dirloc-lsp-defun-regexp
            (concat
             (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
             (or
              (bound-and-true-p lisp-mode-symbol-regexp)
              "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
             "\\)")))
        (add-to-list 'imenu-generic-expression
                     (list "Functions" dirloc-lsp-defun-regexp 1)))
      (defvar lsp--override-calculate-lisp-indent\? nil "Whether to override `lisp-indent-function' with
              the updated `calculate-lisp-indent' definition from
              Emacs 28.")
      (defun wrap-calculate-lisp-indent
          (func &optional parse-start)
        "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling `parse-partial-sexp' up to the
beginning of the current line.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
        (if
            (not lsp--override-calculate-lisp-indent\?)
            (funcall func parse-start)
          (save-excursion
            (beginning-of-line)
            (let
                ((indent-point
                  (point))
                 state
                 (desired-indent nil)
                 (retry t)
                 whitespace-after-open-paren calculate-lisp-indent-last-sexp containing-sexp)
              (cond
               ((or
                 (markerp parse-start)
                 (integerp parse-start))
                (goto-char parse-start))
               ((null parse-start)
                (beginning-of-defun))
               (t
                (setq state parse-start)))
              (unless state
                (while
                    (<
                     (point)
                     indent-point)
                  (setq state
                        (parse-partial-sexp
                         (point)
                         indent-point 0))))
              (while
                  (and retry state
                       (>
                        (elt state 0)
                        0))
                (setq retry nil)
                (setq calculate-lisp-indent-last-sexp
                      (elt state 2))
                (setq containing-sexp
                      (elt state 1))
                (goto-char
                 (1+ containing-sexp))
                (if
                    (and calculate-lisp-indent-last-sexp
                         (> calculate-lisp-indent-last-sexp
                            (point)))
                    (let
                        ((peek
                          (parse-partial-sexp calculate-lisp-indent-last-sexp indent-point 0)))
                      (if
                          (setq retry
                                (car
                                 (cdr peek)))
                          (setq state peek)))))
              (if retry nil
                (goto-char
                 (1+ containing-sexp))
                (setq whitespace-after-open-paren
                      (looking-at
                       (rx whitespace)))
                (if
                    (not calculate-lisp-indent-last-sexp)
                    (setq desired-indent
                          (current-column))
                  (parse-partial-sexp
                   (point)
                   calculate-lisp-indent-last-sexp 0 t)
                  (cond
                   ((looking-at "\\s("))
                   ((>
                     (save-excursion
                       (forward-line 1)
                       (point))
                     calculate-lisp-indent-last-sexp)
                    (if
                        (or
                         (=
                          (point)
                          calculate-lisp-indent-last-sexp)
                         whitespace-after-open-paren)
                        nil
                      (progn
                        (forward-sexp 1)
                        (parse-partial-sexp
                         (point)
                         calculate-lisp-indent-last-sexp 0 t)))
                    (backward-prefix-chars))
                   (t
                    (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp
                     (point)
                     calculate-lisp-indent-last-sexp 0 t)
                    (backward-prefix-chars)))))
              (let
                  ((normal-indent
                    (current-column)))
                (cond
                 ((elt state 3)
                  nil)
                 ((and
                   (integerp lisp-indent-offset)
                   containing-sexp)
                  (goto-char containing-sexp)
                  (+
                   (current-column)
                   lisp-indent-offset))
                 (calculate-lisp-indent-last-sexp
                  (or
                   (and lisp-indent-function
                        (not retry)
                        (funcall lisp-indent-function indent-point state))
                   (and
                    (save-excursion
                      (goto-char indent-point)
                      (skip-chars-forward " 	")
                      (looking-at ":"))
                    (save-excursion
                      (goto-char calculate-lisp-indent-last-sexp)
                      (backward-prefix-chars)
                      (while
                          (not
                           (or
                            (looking-back "^[ 	]*\\|([ 	]+"
                                          (line-beginning-position))
                            (and containing-sexp
                                 (>=
                                  (1+ containing-sexp)
                                  (point)))))
                        (forward-sexp -1)
                        (backward-prefix-chars))
                      (setq calculate-lisp-indent-last-sexp
                            (point)))
                    (> calculate-lisp-indent-last-sexp
                       (save-excursion
                         (goto-char
                          (1+ containing-sexp))
                         (parse-partial-sexp
                          (point)
                          calculate-lisp-indent-last-sexp 0 t)
                         (point)))
                    (let
                        ((parse-sexp-ignore-comments t)
                         indent)
                      (goto-char calculate-lisp-indent-last-sexp)
                      (or
                       (and
                        (looking-at ":")
                        (setq indent
                              (current-column)))
                       (and
                        (<
                         (line-beginning-position)
                         (prog2
                             (backward-sexp)
                             (point)))
                        (looking-at ":")
                        (setq indent
                              (current-column))))
                      indent))
                   normal-indent))
                 (desired-indent)
                 (t normal-indent)))))))
      (when
          (< emacs-major-version 28)
        (advice-add #'calculate-lisp-indent :around #'wrap-calculate-lisp-indent))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'list-timers 'disabled nil)
