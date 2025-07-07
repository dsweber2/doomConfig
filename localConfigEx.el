;; because not every computer is exactly the same
(setq home "/home/dsweber/")
(setq own-doom-home "/home/dsweber/.doom.d/")
(setq julia-up-root "/home/dsweber/.juliaup/")
(setq julia-binary "/home/dsweber/.juliaup/bin/julia")
(setq org-babel-julia-command julia-binary)
(setq julia-root "/home/dsweber/.julia/")
(setq julia-version "1.11")
(concat "--project=" julia-root "environments/v" julia-version)
(setq tmp-lsp-julia-flags (list (concat "--project=" julia-root "environments/v" julia-version) "--startup-file=no" "--history-file=no"))
(setq julia-repl-executable-records
      (list 'default (concat julia-up-root "bin/julia") :basedir
            nil)
      )


;; local Spelling dictionary

(after! spell-fu
  (setq ispell-dictionary "en"))
;;Sunrise/sunset
(use-package! theme-changer
  :after calendar
  :config
  (setq calendar-location-name "City, State")
  (setq calendar-latitude 99.9999)
  (setq calendar-longitude -99.999999)
  (setq theme-changer-delay-seconds -3600)
  )
