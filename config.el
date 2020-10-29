

(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type `relative)

(add-hook 'prog-mode-hook 'subword-mode)

(setq default-input-method "TeX")

(setq create-lockfiles nil)

(defun add-electric-pairs (new-pairs)
  (setq-local electric-pair-pairs (append electric-pair-pairs new-pairs)))

(after! ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (global-set-key (kbd "C-x o") 'facemenu-menu)
  (setq aw-dispatch-always 3)
  (setq aw-kes '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?: ?'))
  (setq aw-scome 'frame)
  (setq aw-make-frame-char ?n)
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-move-window "Swap Windows")
      (?c aw-copy-window "Move Window")
      (?b aw-switch-buffer-in-window "Select Buffer")
      (?p aw-flip-window "Flip Window")
      (?o aw-switch-buffer-other-window "Switch Buffer in another window")
      (?r aw-split-window-fair "Split fair window")
      (?z aw-split-window-vert "Split Vertical Window")
      (?v aw-split-widnow-horz "Split Window")
      ))
  )

(after! company-dict
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (add-to-list 'company-backends 'company-bibtex))

(after! tex
  (setq-default TeX-master nil)
  (setq TeX-save-query nil)
  (setq Tex-PDF-mode t))

(use-package! org-ref
  :config
  (setq orgRefDir "~/allHail/LaTeX/")
  (setq reftex-default-bibliography '(concat orgRefDir "oneBibToRuleThemAll.bib")
        org-ref-bibliograph-notes '(concat orgRefDir "oneBibToRuleThemAll.org")
        bibtex-completion-bibliography '(concat orgRefDir "oneBibToRuleThemAll.bib")
        bibtex-completion-library-path '("~/home/dsweber/allHail/zoteroFiles")
        bibtex-completion-notes-path '(concat orgRefDir "hem-bibtex-notes"))
  )

(after! org
  (defvar org-pairs '((?\$ ?\$)) "Electric pairs needed in org mode not in it")

  (defun add-electric-pairs (new-pairs)
    (setq-local electric-pair-pairs (append electric-pair-pairs new-pairs)))
  (add-hook 'org-mode-hook '(add-electric-pairs org-pairs))
  )

(setq juliaVersion "1.5.2")

(use-package! lsp-julia
  :config
  (setenv "PATH"
          (concat
           "/home/dsweber/julia-" juliaVersion "/bin" ":"
           (getenv "PATH")))
  (add-hook 'julia-mode-hook 'lsp-mode)
  (add-hook 'ess-julia-mode-hook #'lsp-mode)
  (setq lsp-julia-default-environment "~/.julia/environments/newBase")
  (setq lsp-julia-command (concat "~/julia-" juliaVersion "/bin/julia"
                                  "-q/home/dsweber/julia-" juliaVersion "/lib/LspSysImage.so"))
  ;;(setq lsp-folding-range-limit 100)
  )
(setq julia-indent-offset 4)

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
  )
