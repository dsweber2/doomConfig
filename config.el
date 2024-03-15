(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type `relative)

(setq own-doom-home "/home/dsweber/.doom.d/")

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
  (setq ispell-dictionary "en-custom")
  (setq ispell-personal-dictionary (concat own-doom-home "personal.txt"))
  )

(after! projectile
  :config
  (setq custom-suffixes '(".pdf" ".png" ".svg" ".Rd"))
  (setq projectile-globally-ignored-file-suffixes (append projectile-globally-ignored-file-suffixes custom-suffixes)))

(after! counsel
  (setq counsel-rg-base-command '("rg" "--max-columns" "900" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s")))

(use-package! rainbow-mode
  :ensure t)

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

(after! org
  (setq forge-org-list (file-name-concat org-directory "forge.org"))

  (defun forge-add-column (which-table)
    "add a column indicating whether it has been added to `\\[forge-org-list]'"
    (condition-case nil
        (forge-sql [:alter-table $s1 :add :column is-in-org] which-table)
      (emacsql-error t)))

  (defun forge-capture-find-headline ()
    "In large part adapted from org-capture-set-target-location, which can't handle substituting a function for a string for the headline"
    (set-buffer (org-capture-target-buffer forge-org-list))
    ;; Org expects the target file to be in Org mode, otherwise
    ;; it throws an error.  However, the default notes files
    ;; should work out of the box.  In this case, we switch it to
    ;; Org mode.
    (unless (derived-mode-p 'org-mode)
      (org-display-warning
       (format "Capture requirement: switching buffer %S to Org mode"
	       (current-buffer)))
      (org-mode))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char (point-min))
    (let ((headline (nth 0 forge-global-tmp)))
      (if (re-search-forward (format org-complex-heading-regexp-format
			             (regexp-quote headline))
			     nil t)
          (beginning-of-line)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " headline "\n")
        (insert ":PROPERTIES:\n:CATEGORY: " headline "\n:END:")
        (beginning-of-line 0)))
    )
  ;; add a template for issues and pull requests
  (nconc org-capture-templates '(("f" "Forge")
                                 ("fi" "Forge issue" entry (function  forge-capture-find-headline) "* %(nth 6 forge-global-tmp) [#A] Issue #%(number-to-string (nth 2 forge-global-tmp)): %(nth 3 forge-global-tmp)\n\nAuthor: %(nth 4 forge-global-tmp)\nCreated: %(nth 5 forge-global-tmp) \n[[orgit-topic:%(nth 7 forge-global-tmp)]]\n" :heading (nth 1 forge-global-tmp) :immediate-finish t)
                                 ("fp" "Forge pull request" entry (function  forge-capture-find-headline) "\n* %(nth 6 forge-global-tmp) [#A] Pull-Req #%(number-to-string (nth 2 forge-global-tmp)): %(nth 3 forge-global-tmp)\n\nAuthor: %(nth 4 forge-global-tmp)\nCreated: %(nth 5 forge-global-tmp)\n" :heading (nth 1 forge-global-tmp) :immediate-finish t :prepend nil)))
  (defun org-add-forge (org-template-type forge-item-type repo-name repo-owner repo-id number title author created state id)
    "add a particular issue to the org-forge file."
    (let ((todo-state (if (equal state 'open) "TODO" "DONE"))
          (created-date (concat "<" (substring created 0 10) ">")))
      (setq forge-global-tmp (list repo-name repo-owner number title author created-date todo-state id repo-id))
      (if (org-capture 4 org-template-type) ; add to the list
          (emacsql (forge-db) [:update $s3 :set is-in-org := 't :where (= id $s2)]
                   (forge--tablist-columns-vector) id forge-item-type)
        (signal 'error "org capture failed for some reason"))
      ))
  (defun org-forge-issues (repo)
    "make a list of org-forge issues that haven't been added yet"
    (let* ((forge (nth 0 repo))
           (repo-id (nth 1 repo))
           (repo-name (nth 2 repo))
           (repo-owner (nth 3 repo))
           (issues (forge-sql [:select [number title author created state id] :from issue :where (and (= repository $s2) (is is-in-org nil))] (forge--tablist-columns-vector) repo-id))
           (pullreqs (forge-sql [:select [number title author created state id] :from pullreq :where (and (= repository $s2) (is is-in-org nil))] (forge--tablist-columns-vector) repo-id))
           )
      (mapcar (lambda (issue) (apply 'org-add-forge "fi" 'issue repo-name repo-owner repo-id issue)) issues) ; apply org-add to each issue
      (mapcar (lambda (pullreq) (apply 'org-add-forge "fp" 'pullreq repo-name repo-owner repo-id pullreq)) pullreqs) ; apply org-add to each pull request
      ))
  (defun org-forge-update-repos ()
    (mapcar #'org-forge-issues (forge-sql [:select [forge id name owner] :from repository :order-by [(asc owner) (asc name)]] (forge--tablist-columns-vector)))
    )
  (defun update-forge-org-timer (&optional interval)
    (let ((interval (or interval "1 hour")))
      (condition-case nil
          (cancel-timer forge-org-timer)
        (void-variable "void variable"))
      (setq forge-org-timer (run-at-time interval nil #'org-forge-update-repos))))
  (update-forge-org-timer "1 hour")

  (after! (:and ob-async org-src)
    (dolist (lang '(python r julia)) ;; FIXME: Replace your prefer language for jupyter.
      (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                  org-src-lang-modes :key #'car)))
  )

(after! ein
  (setq ein:output-area-inlined-images t))

(use-package! ox-ipynb)

(after! python
  (conda-env-activate "baseEmacs"))

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

(after! julia-repl
  (setq juliaVersion "1.9.2")
  (setq juliaPkgVersion "1.9")
  (setenv "JULIA_NUM_THREADS" "5"))

(use-package! lsp-julia
  :after julia-repl eshell lsp
  :config
  (setq juliaVersion "1.9.2")
  (setq juliaPkgVersion "1.9")
  (setenv "PATH"
          (concat
           "/home/dsweber/.julia/juliaup/bin" ":"
           (getenv "PATH")))
  (add-hook 'julia-mode-hook 'lsp)
  (add-hook 'ess-julia-mode-hook #'lsp)
  (setq lsp-julia-default-environment (concat "~/.julia/environments/v" juliaPkgVersion))
  (setq lsp-julia-package-dir (concat "~/.julia/environments/v" juliaPkgVersion))
  (setq lsp-julia-command "/home/dsweber/.julia/juliaup/bin/julia")
  (setq lsp-julia-flags '("--project=/home/dsweber/.julia/environments/v1.9" "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-command "/home/dsweber/.julia/juliaup/bin/julia")
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
  (setq juliaPkgVersion "1.9")
  (setq juliaVersion "1.9.2")
  (setq lsp-julia-default-environment (concat "~/.julia/environments/v" juliaPkgVersion))
  (setq lsp-julia-package-dir (concat "~/.julia/environments/v" juliaPkgVersion))
  (setq lsp-julia-command "/home/dsweber/.julia/juliaup/bin/julia")
  (setq lsp-julia-flags '("--project=/home/dsweber/.julia/environments/v1.9" "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-command "/home/dsweber/.julia/juliaup/bin/julia")
  (setq lsp-julia-format-kw nil))

(after! julia-repl
  (setq julia-repl-executable-records
        `((default ,"/home/dsweber/.julia/juliaup/bin/julia" :basedir
           nil)
          )
        )
  :hook '(julia-repl-mode-hook +word-wrap-mode)
  )

(after! ob-julia
  (setq org-babel-julia-command "/home/dsweber/.julia/juliaup/bin/julia")
  (setq inferior-julia-program-name "/home/dsweber/.julia/juliaup/bin/julia"))

(after! ob-ess-julia
  (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ess-julia . t))))
  (setq org-src-lang-modes (append org-src-lang-modes '(("ess-julia" . ess-julia))))
  )

(after! ein
  (setq org-babel-header-args '((:kernel . "julia-1.9") (:async . no)))
  (setq org-babel-default-header-args:jupyter-julia '((:kernel . "julia-1.9") (:async . no))))

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
(toggle-debug-on-error)
(setq elfeed-protocol-log-trace t)
(use-package! elfeed-protocol
  :config
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-protocol-newsblur-maxpages 20)
  (setq elfeed-curl-extra-arguments '("--cookie-jar" "/home/dsweber/tmp/newsblur-cookie"
                                      "--cookie" "/home/dsweber/tmp/newsblur-cookie"))
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

(setq calendar-location-name "Sacramento, CA")
(setq calendar-latitude 38.70)
(setq calendar-longitude -121.59)
(setq theme-changer-mode 'deftheme)
(setq theme-changer-delay-seconds -3600)
(use-package! theme-changer
  :after doom-themes
)
(after! theme-changer
 (change-theme 'doom-bluloco-light 'doom-dracula)
 )
