

(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type `relative)

(setq own-doom-home "/home/dsweber/.doom.d/")

(add-hook 'prog-mode-hook 'subword-mode)

(setq default-input-method "TeX")

(setq create-lockfiles nil)

(setq electric-pair-mode t)

(setq-default auto-fill-function 'do-auto-fill)

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
  (setq TeX-save-query nil)
  (setq Tex-PDF-mode t)
  (setq reftex-default-bibliography "~/allHail/LaTeX/oneBibToRuleThemAll.bib")
)

(after! spell-fu
  (setq ispell-dictionary "en-custom")
  (setq ispell-personal-dictionary (concat own-doom-home "personal.txt"))
  )

(use-package! org-ref
  :config
  (setq orgRefDir "~/allHail/LaTeX/")
  (setq reftex-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-default-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-bibliography-notes (concat orgRefDir "oneBibToRuleThemAll.org")
        bibtex-completion-bibliography (concat orgRefDir "oneBibToRuleThemAll.bib")
        bibtex-completion-library-path "~/allHail/zoteroFiles"
        bibtex-completion-notes-path (concat orgRefDir "hem-bibtex-notes"))
  )
(map! :leader
      :desc "insert a helm reference"
      "i c" 'org-ref-helm-insert-cite-link)

(after! org
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-startup-with-inline-images t)
  )

(use-package! org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
)

(setq juliaVersion "1.5.3")

(use-package! lsp-julia
  :config
  (setenv "PATH"
          (concat
           "/home/dsweber/julia-" juliaVersion "/bin" ":"
           (getenv "PATH")))
  (add-hook 'julia-mode-hook 'lsp)
  (add-hook 'ess-julia-mode-hook #'lsp)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5")

  (setq lsp-julia-timeout 12000)
  ;; (add-to-list 'lsp-julia-flags (concat "-J /home/dsweber/julia-"
  ;;                                       juliaVersion "/lib/lspSysImage.so"))
  ;; (setq lsp-julia-command (concat "/home/dsweber/julia-"
  ;;                                 juliaVersion "/bin/julia "
  ;;                                 "-q -J /home/dsweber/julia-"
  ;;                                 juliaVersion "/lib/lspSysImage.so"))
  (setq lsp-enable-folding t)
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

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago +unread"))

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(after! elfeed-goodies
  (setq elfeed-goodies/entry-pane-position :bottom)
  (elfeed-goodies/setup)
  )
