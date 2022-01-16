(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type `relative)

(setq own-doom-home "/home/dsweber/.doom.d/")

(add-hook 'prog-mode-hook 'subword-mode)

(setq default-input-method "TeX")

(setq create-lockfiles nil)

(setq electric-pair-mode t)

(after! +word-wrap
  (setq +gobal-word-wrap-mode +1))

(defun add-electric-pairs (new-pairs)
  (setq-local electric-pair-pairs (append electric-pair-pairs new-pairs)))

(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-N") 'mc/unmark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-P") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c n") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-]") 'mc/mark-sgml-tag-pair)

(after! ace-window
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

(use-package! rainbow-mode
  :ensure t)

(after! indent
        (setq standard-indent 4))

(use-package! org-ref
  :config
  (setq orgRefDir "~/allHail/LaTeX/")
  (setq reftex-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-bibliography-notes (concat orgRefDir "oneBibToRuleThemAll.org")
        bibtex-completion-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        bibtex-completion-library-path "~/allHail/zoteroFiles"
        bibtex-completion-notes-path (concat orgRefDir "oneBibToRuleThemAll.org"))
  )
(map! :leader
      :desc "insert a helm reference"
      "i c" 'org-ref-insert-link)

(after! org
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  (defun native-comp-available-p () nil)
  ;;(setq org)
  )

(use-package! org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  )

(after! org
    (setq org-agenda-files (quote ("~/orgNotes")))
    (setq +org-capture-projects-file "~/orgNotes/projects.org")
    (setq org-capture-todo-file "~/orgNotes/todo.org")
    (setq +org-capture-todo-file "~/orgNotes/todo.org")
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
    (setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)

        ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
        ;; {todo,notes,changelog}.org file is found in a parent directory.
        ;; Uses the basename from `+org-capture-todo-file',
        ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry  ; {project-root}/todo.org
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("pn" "Project-local notes" entry  ; {project-root}/notes.org
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)
        ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :prepend t)

        ;; Will use {org-directory}/{+org-capture-projects-file} and store
        ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
        ;; support `:parents' to specify what headings to put them under, e.g.
        ;; :parents ("Projects")
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry
         (function +org-capture-central-project-todo-file)
         "* TODO %?\n %i\n %a"
         :heading "Tasks"
         :prepend nil)
        ("on" "Project notes" entry
         (function +org-capture-central-project-notes-file)
         "* %U %?\n %i\n %a"
         :heading "Notes"
         :prepend t)
        ("oc" "Project changelog" entry
         (function +org-capture-central-project-changelog-file)
         "* %U %?\n %i\n %a"
         :heading "Changelog"
         :prepend t)))
    )

(setq org-todo-keywords (quote ((sequence "TODO(t@/!)" "PROJ(p)" "STRT(s!/!)" "WAIT(w@/!)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                                (sequence "[ ](T@/!)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-todo-keywords-for-agenda (quote ((sequence "TODO(t@/!)" "PROJ(p)" "STRT(s!/!)" "WAIT(w@/!)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                                (sequence "[ ](T@/!)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(setq org-agenda-todo-ignore-deadlines 'near)
(setq org-agenda-todo-ignore-scheduled 'future)

(after! org
  :config
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?Z)
  (setq org-priority-default ?M))

(after! org
  :config
  (setq org-agenda-sorting-strategy
        '((agenda habit-up deadline-up priority-down time-up scheduled-down)
          (todo category-keep priority-down)
          (tags priority-down category-keep)
          (search category-keep)))
  )

(after! org
  (org-defkey org-agenda-mode-map "j" #'org-agenda-next-line)
  (org-defkey org-agenda-mode-map "k" #'org-agenda-previous-line)
  (org-defkey org-agenda-mode-map "J" #'org-agenda-priority-up)
  (org-defkey org-agenda-mode-map "K" #'org-agenda-priority-down)
  (org-defkey org-agenda-mode-map (kbd "SPC") 'nil)
  )

(after! julia-repl
  (setq juliaVersion "1.6.3"))

(use-package! lsp-julia
  :after julia-repl eshell
  :config
  (setenv "PATH"
          (concat
           "/home/dsweber/julia-" juliaVersion "/bin" ":"
           (getenv "PATH")))
  (add-hook 'julia-mode-hook 'lsp)
  (add-hook 'ess-julia-mode-hook #'lsp)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
  (setq lsp-julia-package-dir "~/.julia/environments/v1.6")
  (setq lsp-julia-command (concat
           "/home/dsweber/julia-" juliaVersion "/bin/julia"
           ))
  (setq lsp-julia-flags '("--project=/home/dsweber/.julia/environments/v1.6" "--startup-file=no" "--history-file=no"))
  (setq lsp-julia-timeout 12000)
  (setq lsp-enable-folding t)
  (setq julia-indent-offset 1)

  (setq lsp-julia-format-indents nil)
  (setq lsp-enable-indentation nil)
  )
(setq julia-indent-offset 4)

(after! julia-repl
  (setq julia-repl-executable-records
        `((default ,(concat "/home/dsweber/julia-" juliaVersion
                            "/bin/julia") :basedir
                            nil)
          )
        )
  :hook '(julia-repl-mode-hook +word-wrap-mode)
  )

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

(after! evil-numbers
  (define-key evil-normal-state-map (kbd "zq") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "zq") 'evil-numbers/inc-at-pt)
  )

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

(after! persp-mode
  (persp-def-buffer-save/load
   :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
   :save-vars '(default-directory)
   :load-function #'(lambda (savelist &rest _)
                      (cl-destructuring-bind (buffer-name vars-list &rest _rest) (cdr savelist)
                        (let ((buf-dir (alist-get 'default-directory vars-list)))
                          (magit-status buf-dir))))))

(after! projectile
   :config
   (setq custom-suffixes '(".pdf" ".png" ".svg"))
   (setq projectile-globally-ignored-file-suffixes (append projectile-globally-ignored-file-suffixes custom-suffixes)))

(after! counsel
  (setq counsel-rg-base-command '("rg" "--max-columns" "900" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s")))

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
