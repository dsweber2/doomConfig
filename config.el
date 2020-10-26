(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type `relative)

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
  (setq lsp-julia-command (concat "/home/dsweber/julia-" juliaVersion "/bin/julia"
                                  "-q/home/dsweber/julia-" juliaVersion "/lib/LspSysImage.so"))
  ;;(setq lsp-folding-range-limit 100)
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
  )
