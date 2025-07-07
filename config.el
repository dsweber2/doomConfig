(load-file (file-name-concat doom-user-dir "localConfig.el"))

(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type `relative)

(after! persp-mode
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function #'(lambda (savelist &rest _)
                      (cl-destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
                        (let ((buf-dir (alist-get 'default-directory vars-list)))
                          (magit-status buf-dir))))))

(add-hook 'prog-mode-hook 'subword-mode)

(setq default-input-method "TeX")

(setq create-lockfiles nil)

(setq electric-pair-mode t)

(setq-default auto-fill-function nil)

(+global-word-wrap-mode +1)

(after! emacs
  (setq fill-column 264))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (ruby . t)
   (C . t)
   (shell . t)
   (mathematica . t)
   (clojure . t)
   (R . t)
   (jupyter . t)
   ;; other languages..
   ))
(setq org-babel-load-langages '())

(after! +word-wrap
  (setq +gobal-word-wrap-mode 't))

(after! poly-markdown
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+r-mode)))

(add-to-list 'safe-local-variable-values '(commend-add . 0))
(add-to-list 'safe-local-variable-values '(foob . integerp))
(add-to-list 'safe-local-variable-values '(+org-capture-projects-file . stringp))
+org-capture-projects-file

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the 
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my-reload-dir-locals-for-current-buffer))))))
(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'my-reload-dir-locals-for-all-buffer-in-this-directory
                        nil t))))

(defun add-electric-pairs (new-pairs)
  (setq-local electric-pair-pairs (append electric-pair-pairs new-pairs)))

(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-N") 'mc/unmark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-P") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c n") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-]") 'mc/mark-sgml-tag-pair)

(setq emacsql-sqlite-executable "/fasterHome/anaconda3/bin/sqlite3")

(after! evil-snipe
  (setq evil-snipe-scope 'visible)
  (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)
  (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
  )

(use-package! ace-window
  :config
 (global-unset-key (kbd "M-o"))
  (global-set-key (kbd "M-o") 'ace-window)
  ;;(global-set-key (kbd "C-x o") 'facemenu-menu)
  (setq aw-dispatch-always 3)
  (setq aw-kes '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?: ?'))
  (setq aw-scome 'frame)
  (setq aw-make-frame-char ?n)
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-move-window "Swap Windows")
          (?c aw-copy-window "Move Window")
          (?b aw-switch-buffer-in-window "Select Buffer")
          (?p aw-flip-window "Flip Window")
          (?o aw-switch-buffer-other-window "Switch Buffer in another window")
          (?r aw-split-window-fair "Split fair window")
          (?z aw-split-window-vert "Split Vertical Window")
          (?v aw-split-window-horz "Split Window horizontally")
          (?? aw-show-dispatch-help)
          ))
  )

(after! company-dict
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (setq backends-for-everywhere '(company-bibtex company-ispell))
  (setq company-backends (append company-backends backends-for-everywhere)))

(after! tex
  (setq-default TeX-master 'dwim)
  (setq TeX-master 'dwim)
  (setq TeX-save-query nil)
  (setq Tex-PDF-mode t)
  (setq reftex-default-bibliography "~/allHail/LaTeX/oneBibToRuleThemAll.bib")
  )

(after! spell-fu
  (setq ispell-personal-dictionary (concat own-doom-home "personal.txt"))
  )

(after! projectile
  :config
  (setq custom-suffixes '(".pdf" ".png" ".svg" ".Rd"))
  (setq projectile-globally-ignored-file-suffixes (append projectile-globally-ignored-file-suffixes custom-suffixes))
  (setq projectile-enable-caching 'nil))

(after! counsel
  (setq counsel-rg-base-command '("rg" "--max-columns" "900" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s")))

(use-package! rainbow-mode
  :config
  (rainbow-mode)
)

(after! indent
  (setq standard-indent 4))

(after! flycheck
  (setq flycheck-checker-error-threshold 'nil))

(use-package! org-ref
  :config
  (setq orgRefDir "~/allHail/LaTeX/")
  (setq reftex-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-bibliography-notes (concat orgRefDir "oneBibToRuleThemAll.org")
        bibtex-completion-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        bibtex-completion-library-path "~/allHail/zoteroFiles"
        bibtex-completion-notes-path (concat orgRefDir "oneBibToRuleThemAll.org"))
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html))
  (setq org-latex-pdf-process (list "latexmk -f -pdf -%latex -interaction=nonstopmode -bibtex -shell-escape -output-directory=%o %f"))
  )
(map! :leader
      :desc "insert a helm reference"
      "i c" 'org-ref-insert-link)

(setq max-lisp-eval-depth 10000)
(setq org-extend-today-until 2)
(after! org
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  ;;(setq org)
  )

(use-package! org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  )

(after! org
  (setq org-table-convert-region-max-lines 999))

(after! org
  (setq org-agenda-files (quote ("~/orgNotes" "~/.doom.d")))
  (setq org-directory "~/orgNotes")
  (setq +org-capture-journal-file "~/orgNotes/journal.org")
  (setq org-priority-faces  '((?A :foreground "#FF6C6B")
                              (?B :foreground "#F97066")
                              (?C :foreground "#F37460")
                              (?D :foreground "#ED785A")
                              (?E :foreground "#E77D54")
                              (?F :foreground "#E1804F")
                              (?G :foreground "#DB8449")
                              (?H :foreground "#D8835B")
                              (?I :foreground "#D48172")
                              (?J :foreground "#D17F8A")
                              (?K :foreground "#CE7DA2")
                              (?L :foreground "#CA7BBA")
                              (?M :foreground "#C779D2")
                              (?N :foreground "#C47BDE")
                              (?O :foreground "#BF82DE")
                              (?P :foreground "#BB88DE")
                              (?Q :foreground "#B68FDF")
                              (?R :foreground "#B196DF")
                              (?S :foreground "#AD9CE1")
                              (?T :foreground "#A69EDD")
                              (?U :foreground "#9A94C9")
                              (?V :foreground "#8D8BB6")
                              (?W :foreground "#8181A3")
                              (?X :foreground "#74768F")
                              (?Y :foreground "#676C7B")
                              (?Z :foreground "#5B6268")))
  )

(setq org-todo-keywords (quote ((sequence "TODO(t@/!)" "PROJ(p)" "STRT(s!/!)" "WAIT(w@/!)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                                (sequence "[ ](T@/!)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-todo-keywords-for-agenda (quote ((sequence "TODO(t@/!)" "PROJ(p)" "STRT(s!/!)" "WAIT(w@/!)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                                           (sequence "[ ](T@/!)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-agenda-todo-ignore-deadlines 'future)
(setq org-deadline-warning-days 0)
(setq org-agenda-todo-ignore-scheduled 'future)

(after! org
  :config
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?Z)
  (setq org-priority-default ?Z))

(after! org
  :config
  (setq org-agenda-sorting-strategy
        '((agenda habit-up deadline-up priority-down time-up scheduled-down)
          (todo priority-down)
          (tags priority-down category-keep)
          (search category-keep)))
  )

(after! org
  :config
  (setq org-agenda-show-future-repeats 'next))

(after! org
  (org-defkey org-agenda-mode-map "j" #'org-agenda-next-line)
  (org-defkey org-agenda-mode-map "k" #'org-agenda-previous-line)
  (org-defkey org-agenda-mode-map "J" #'org-agenda-priority-up)
  (org-defkey org-agenda-mode-map "K" #'org-agenda-priority-down)
  (org-defkey org-agenda-mode-map (kbd "SPC") 'nil)
  )

(after! org
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-into-drawer t))

(after! lsp
  :after ess
  (add-hook 'ess-r-mode-hook #'lsp)
)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\delphi-epidata\\'")
  ;; or
  ;; (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'")
  (setq lsp-ruff-server-command '("ruff" "server" "--preview"))
  (setq lsp-file-watch-threshold 9000)
  )

(after! lsp
  :config
  (push "[/\\\\]\\.PlayOnLinux\\'" lsp-file-watch-ignored-directories)
  (setq lsp-pyright-langserver-command "basedpyright")
  (lsp-workspace-remove-all-folders)
  (add-hook 'ess-julia-mode-hook #'lsp)
  (add-to-list 'lsp-enabled-clients #'pylsp)
  (add-to-list 'lsp-enabled-clients #'ruff)
  (add-to-list 'lsp-enabled-clients #'semgrep)
  (add-to-list 'lsp-enabled-clients #'lsp-r)
  )
;; (after! lsp-pyright
;;   :config
;;   (setq lsp-pyright-multi-root nil)
;;   )

(use-package! pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (pet-def-config-accessor pre-commit-config
                         :file-name "DISABLED.yaml"
                         :parser pet-parse-config-file)
  )

(after! setq '(emacs-lisp-mode))

(after! ein
  (setq ein:output-area-inlined-images t))

(use-package! ox-ipynb)

(setq inferior-ess-r-program "/usr/local/bin/R")
(setq lsp-clients-r-server-command '("/usr/local/bin/R" "-e" "languageserver::run()"))

(after! ess
  (add-to-list 'safe-local-variable-values '(comment-add . 0)))

(after! ess
  (defun my-inferior-ess-init ()
    (setq-local ansi-color-for-comint-mode 'filter)
    (smartparens-mode 1))
  (add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)
  )

(after! ess-tracebug
  (defun ess-debug-command-finish ()
    "Step next in debug mode.
Equivalent to `f' at the R prompt."
    (interactive)
    (ess-force-buffer-current)
    (unless (ess--dbg-is-active-p)
      (error "Debugger is not active"))
    (if (ess--dbg-is-recover-p)
        (progn (ess-send-string (ess-get-process) "0")
               (ess-send-string (ess-get-process) "f"))
      (ess-send-string (ess-get-process) "f")))
  (defun ess-debug-command-step ()
    "Step next in debug mode.
Equivalent to `s' at the R prompt."
    (interactive)
    (ess-force-buffer-current)
    (unless (ess--dbg-is-active-p)
      (error "Debugger is not active"))
    (if (ess--dbg-is-recover-p)
        (progn (ess-send-string (ess-get-process) "0")
               (ess-send-string (ess-get-process) "s"))
      (ess-send-string (ess-get-process) "s")))

  (defun ess-debug-command-resume ()
    "Step next in debug mode.
Equivalent to `r' at the R prompt."
    (interactive)
    (ess-force-buffer-current)
    (unless (ess--dbg-is-active-p)
      (error "Debugger is not active"))
    (if (ess--dbg-is-recover-p)
        (progn (ess-send-string (ess-get-process) "0")
               (ess-send-string (ess-get-process) "r"))
      (ess-send-string (ess-get-process) "r")))

  (defun ess-debug-command-where ()
    "Step next in debug mode.
Equivalent to `where' at the R prompt."
    (interactive)
    (ess-force-buffer-current)
    (unless (ess--dbg-is-active-p)
      (error "Debugger is not active"))
    (ess-send-string (ess-get-process) "where"))


  (defun ess-debug-command-help ()
    "Step next in debug mode.
Equivalent to `where' at the R prompt."
    (interactive)
    (ess-force-buffer-current)
    (unless (ess--dbg-is-active-p)
      (error "Debugger is not active"))
    (ess-send-string (ess-get-process) "help"))
  (define-key ess-debug-minor-mode-map (kbd "M-S") #'ess-debug-command-step)
  (define-key ess-debug-minor-mode-map (kbd "M-W") #'ess-debug-command-where)
  (define-key ess-debug-minor-mode-map (kbd "M-F") #'ess-debug-command-finish)
  (define-key ess-debug-minor-mode-map (kbd "M-H") #'ess-debug-command-help)
  (define-key ess-debug-minor-mode-map (kbd "M-R") #'ess-debug-command-resume))

(after! ess-r-mode
  (defun ess-r-style-pkg (&optional arg)
  "Interface for `styler::style_pkg()'."
  (interactive "P")
  (ess-r-package-eval-linewise
   "styler::style_pkg(%s)\n" "styling %s" arg
   ))
  (defun ess-r-build-site (&optional arg)
  "Interface for `pkgdown::build_site()'.
With no prefix ARG, build with `lazy = FALSE'."
  (interactive "P")
  (ess-r-package-eval-linewise
   "pkgdown::build_site(%s, lazy=TRUE)\n" "building site for %s" arg
   ))
  (keymap-set ess-r-package-dev-map "f" #'ess-r-style-pkg)
  (keymap-set ess-r-package-dev-map "S" #'ess-r-build-site)
  )

(use-package! quarto-mode
  )

(after! julia-repl
  (setenv "JULIA_NUM_THREADS" "5"))

(use-package! lsp-julia
  :after julia-repl eshell lsp
  :config
  (setenv "PATH"
          (concat
           julia-root "bin:"
           (getenv "PATH")))
  (add-hook 'julia-mode-hook 'lsp)
  (add-hook 'ess-julia-mode-hook #'lsp)
  (setq lsp-julia-default-environment (concat "~/.julia/environments/v" julia-version))
  (setq lsp-julia-package-dir (concat "~/.julia/environments/v" julia-version))
  (setq lsp-julia-command julia-binary)
  (setq lsp-julia-flags (list (concat "--project=" julia-root "environments/v" julia-version) "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-command julia-binary)
  (setq lsp-julia-timeout 12000)
  (setq lsp-enable-folding t)
  (setq julia-indent-offset 4)

  (setq lsp-julia-format-indents true)
  (setq lsp-enable-indentation true)
  (setq julia-indent-mapping '((julia-mode . julia-indent-offset)))
  (setq lsp--formatting-indent-alist '((c-mode                     . c-basic-offset)                   ; C
                                       (c++-mode                   . c-basic-offset)                   ; C++
                                       (csharp-mode                . c-basic-offset)                   ; C#
                                       (csharp-tree-sitter-mode    . csharp-tree-sitter-indent-offset) ; C#
                                       (d-mode                     . c-basic-offset)                   ; D
                                       (java-mode                  . c-basic-offset)                   ; Java
                                       (jde-mode                   . c-basic-offset)                   ; Java (JDE)
                                       (js-mode                    . js-indent-level)                  ; JavaScript
                                       (js2-mode                   . js2-basic-offset)                 ; JavaScript-IDE
                                       (js3-mode                   . js3-indent-level)                 ; JavaScript-IDE
                                       (json-mode                  . js-indent-level)                  ; JSON
                                       (lua-mode                   . lua-indent-level)                 ; Lua
                                       (objc-mode                  . c-basic-offset)                   ; Objective C
                                       (php-mode                   . c-basic-offset)                   ; PHP
                                       (perl-mode                  . perl-indent-level)                ; Perl
                                       (cperl-mode                 . cperl-indent-level)               ; Perl
                                       (raku-mode                  . raku-indent-offset)               ; Perl6/Raku
                                       (erlang-mode                . erlang-indent-level)              ; Erlang
                                       (ada-mode                   . ada-indent)                       ; Ada
                                       (sgml-mode                  . sgml-basic-offset)                ; SGML
                                       (nxml-mode                  . nxml-child-indent)                ; XML
                                       (pascal-mode                . pascal-indent-level)              ; Pascal
                                       (typescript-mode            . typescript-indent-level)          ; Typescript
                                       (sh-mode                    . sh-basic-offset)                  ; Shell Script
                                       (ruby-mode                  . ruby-indent-level)                ; Ruby
                                       (enh-ruby-mode              . enh-ruby-indent-level)            ; Ruby
                                       (crystal-mode               . crystal-indent-level)             ; Crystal (Ruby)
                                       (css-mode                   . css-indent-offset)                ; CSS
                                       (rust-mode                  . rust-indent-offset)               ; Rust
                                       (rustic-mode                . rustic-indent-offset)             ; Rust
                                       (scala-mode                 . scala-indent:step)                ; Scala
                                       (powershell-mode            . powershell-indent)                ; PowerShell
                                       (ess-mode                   . ess-indent-offset)                ; ESS (R)
                                       (yaml-mode                  . yaml-indent-offset)               ; YAML
                                       (hack-mode                  . hack-indent-offset)               ; Hack
                                       (julia-mode                 . julia-indent-offset)
                                       (default                    . standard-indent)))
  )

(after! lsp-julia
  (setq lsp-julia-default-environment (concat "~/.julia/environments/v" julia-version))
  (setq lsp-julia-package-dir (concat "~/.julia/environments/v" julia-version))
  (setq lsp-julia-command julia-binary)
  (setq lsp-julia-flags (list (concat "--project=" julia-root "environments/v" julia-version) "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-command julia-binary)
  (setq lsp-julia-format-kw nil))

(after! julia-repl
  (setq julia-repl-executable-records
        `((default, "/home/dsweber/.julia/juliaup/bin/julia" :basedir
           nil)
          )
        )
  :hook '(julia-repl-mode-hook +word-wrap-mode)
  )

(after! ob-julia
  (setq org-babel-julia-command julia-binary)
  (setq inferior-julia-program-name julia-binary))

(after! ob-ess-julia
  (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ess-julia . t))))
  (setq org-src-lang-modes (append org-src-lang-modes '(("ess-julia" . ess-julia))))
  )

(after! ein
  (setq org-babel-header-args '((:kernel . "julia-1.11") (:async . no)))
  (setq org-babel-default-header-args:jupyter-julia '((:kernel . "julia-1.11") (:async . no))))

(map! :after julia-repl
      :map julia-repl-mode-map
      "C-RET" #'julia-repl-send-line)

(after! evil
  (define-key evil-normal-state-map "M" 'evil-scroll-line-to-center)
  (define-key evil-normal-state-map "L" 'evil-scroll-line-to-bottom)
  (define-key evil-normal-state-map "H" 'evil-scroll-line-to-top)
  (define-key evil-normal-state-map "zM" 'evil-window-middle)
  (define-key evil-normal-state-map "zL" 'evil-window-bottom)
  (define-key evil-normal-state-map "zH" 'evil-window-top)
  (define-key evil-normal-state-map "zl" 'evil-scroll-left)
  (define-key evil-normal-state-map "zH" 'evil-scroll-right)
  (define-key evil-normal-state-map "zH" 'evil-window-top)
  (setq evil-cross-lines t) ;; fF etc go beyond the current line
  (setq evil-want-Y-yank-to-eol 'nil)
  )

(use-package! evil-quickscope
  :config
  (global-evil-quickscope-mode 1)
  (add-hook 'org-agenda-mode-hook 'turn-off-evil-quickscope-mode)
  (setq evil-quickscope-cross-lines t)
  )

(use-package! evil-fringe-mark
  :config
  (global-evil-fringe-mark-mode))

(use-package! evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "zq") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "zQ") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "zz") 'evil-numbers/dec-at-pt)
  )

(use-package! evil-owl
  :after evil
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(setq rmh-elfeed-org-files (list (concat own-doom-home "elfeedSources.org")))

(use-package! elfeed-goodies
  :after elfeed-score
  :config
  (setq elfeed-goodies/entry-pane-position :bottom)
  (setq elfeed-goodies/entry-pane-position :bottom)
  (elfeed-goodies/setup)
  )

(after! elfeed
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date))))

(use-package! elfeed-score
  :config
  (setq elfeed-score/score-file (concat own-doom-home "elfeed.score"))
  (define-key elfeed-search-mode-map (kbd "i") elfeed-score-map)
  (setq elfeed-search-print-entry-function 'elfeed-score-print-entry)
  (elfeed-score-enable t)
  ;; we'll make our own scoring function-- with blackjack and hookers!
  (defcustom elfeed-equalize-random-rate (/ (+ (log 3) (/ (log 11) 2)) 100.0)
    "the coefficient for converting scores to softmax eval. Default maps 100 to a
        correct sorting probability of 99%)"
    :group 'elfeed-equalize)
  (defcustom elfeed-equalize-date-to-score 6048
    "how many seconds correspond to a single score point. Default is 6048 so that
         a week difference gives a score of 100"
    :group 'elfeed-equalize)
  (defcustom elfeed-equalize-random-rate (/ (+ (log 3) (/ (log 11) 2)) 100.0)
    "the coefficient for converting scores to softmax eval. Default maps 100 to a
        correct sorting probability of 99%)"
    :group 'elfeed-equalize)

  (defun softmax (x)
    (let ((term (exp (* 2 x))))
      (/ term (+ term 1)))
    )
  (defun rand ()
    (/ (float (random most-positive-fixnum)) most-positive-fixnum))
  (defun elfeed-score/softmax-sort (a b)
    (let ((flip-prob (softmax (* elfeed-equalize-random-rate (- a b))))
          (rolled-val (rand)))
      (> flip-prob rolled-val)))
  (defun elfeed-score/date-score (sec)
    "convert a number of seconds into a score using rate c"
    (/ sec elfeed-equalize-date-to-score))
  (defun elfeed-score-softmax-swap (a b)
    "Return non-nil if A should sort before B. This is a probabilistic
    comparison that uses the date and the score"

    (let* ((a-score (elfeed-score--get-score-from-entry a))
           (b-score (elfeed-score--get-score-from-entry b))
           (a-date  (elfeed-entry-date a))
           (b-date  (elfeed-entry-date b)))
      (elfeed-score/softmax-sort a-score b-score)
      ))
  (setq elfeed-search-sort-function #'elfeed-score-softmax-swap)
  )
(setq a-date 3425)
(setq b-date 3295)

(setq elfeed-log-level 'debug)
;; (toggle-debug-on-error)
(setq elfeed-protocol-log-trace t)
(use-package! elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-protocol-newsblur-maxpages 20)
  (setq elfeed-curl-extra-arguments (list "--cookie-jar" (concat home "tmp/newsblur-cookie")
                                      "--cookie" (concat home "tmp/newsblur-cookie")))
  (setq elfeed-protocol-tags elfeed-feeds)
  (setq elfeed-feeds '(( "newsblur+https://HerCarverBidesDew@newsblur.com"
                         :password-file "~/.newsblur"
                         :autotags elfeed-protocol-tags)))
  (defadvice elfeed (after configure-elfeed-feeds activate)
    "Make elfeed-org autotags rules works with elfeed-protocol."
    (setq elfeed-protocol-tags elfeed-feeds)
    (setq elfeed-feeds (list
                        (list "newsblur+https://HerCarverBidesDew@newsblur.com"
                              :password-file "~/.newsblur"
                              :autotags elfeed-protocol-tags))))
  (elfeed-protocol-enable)
  )

(defun elfeed-score/toggle-debug-warn-level ()
  (if (eq elfeed-score-log-level 'debug)
      (setq elfeed-score-log-level 'warn)
    (setq elfeed-score-log-level 'debug)))

(map! :leader
      (:prefix ("e" . "elfeed")
       :desc "elfeed-score-map" "m" #'elfeed-score-map
       :desc "open feed"        "f" #'elfeed
       :desc "update elfeed"    "u" #'elfeed-update
       :desc "score entries"    "s" #'elfeed-score/score
       :desc "add score rules"  "r" #'elfeed-score-load-score-file
       :desc "toggle debug"     "d" #'elfeed-score/toggle-debug-warn-level
       )
      )

(use-package! theme-changer
  :after calendar
  :config
  (setq theme-changer-mode 'deftheme)
  (change-theme 'doom-bluloco-light 'doom-henna)
)

(use-package! gptel
  :config
  ;; OPTIONAL configuration
  ;; OPTIONAL configuration
  (setq
   gptel-model 'fastgpt
   gptel-backend (gptel-make-kagi "Kagi"
                   :key (password-store-get 'api_keys/llms/kagi)))
  ;; (setq
  ;;  gptel-model "claude-3-5-sonnet-20241022" ;  "claude-3-opus-20240229" also available
  ;;  gptel-backend (gptel-make-anthropic "Claude"
  ;;                  :stream t
  ;;                  :key (password-store-get 'api_keys/llms/claude)))
  )
