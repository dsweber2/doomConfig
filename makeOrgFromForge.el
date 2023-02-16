;;; makeOrgFromForge.el -*- lexical-binding: t; -*-

forge-repository-list-columns
(forge-repository-list-setup #'forge-repository-list-refresh
                               "*Forge Repositories*")
#'forge-repository-list-refresh
(cl-letf (((symbol-function #'tabulated-list-revert) #'ignore)) ; see #229
      (forge-repository-list-mode))
(funcall #'forge-repository-list-refresh)
(forge-repository-list-refresh )
(forge-sql [:select $i1 :from repository
                            :order-by [(asc owner) (asc name)]]
                           (forge--tablist-columns-vector))
(let* ((repos (forge-sql [:select [forge forge-id name owner] :from repository
                            :order-by [(asc owner) (asc name)]]
                         (forge--tablist-columns-vector)))
       (repo (car repos))
       (forge (nth 0 repo))
       (forge-id (nth 1 repo))
       (name (nth 2 repo))
       (owner (nth 3 repo)))
  (print repo)
  (print owner))
((forge-sql [:select * :from repository
                            :order-by [(asc owner) (asc name)]]
                           (forge--tablist-columns-vector)))
(forge-list-issues "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(forge-sql [:select * :from repository
                            :order-by [(asc owner) (asc name)]]
                         (forge--tablist-columns-vector))
(nth 0 (forge-sql [:select [forge forge-id name owner] :from repository
                            :order-by [(asc owner) (asc name)]]
                         (forge--tablist-columns-vector)))


(org-forge-issues (nth 0 (forge-sql [:select [forge id name owner] :from repository
                            :order-by [(asc owner) (asc name)]]
                         (forge--tablist-columns-vector))))
(concat "" #'('(closed)))
(equal '(closed) '(closed))
(setq forge-org-list (file-name-concat org-directory "forge.org"))
(org-capture-set-plist (org-capture-select-template "ot"))
(org-capture-put)
(org-capture-set-target-location)
(let* ((updated "2022-09-06T04:47:59Z"))
  (concat "<" (substring updated 0 10) ">"))
(forge-sql [:select [number title author state created] :from pullreq :where (= repository $s2)] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(nth 0 (forge-sql [:select id :from issue] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ=="))
(forge-sql [:select is-in-org :from issue :where (= repository $s2)] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")

(defun forge-add-column (which-table)
  "add a column indicating whether it has been added to `\\[forge-org-list]'"
  (condition-case nil
    (forge-sql [:alter-table $s1 :add :column is-in-org] which-table)
  (emacsql-error t)))
(forge-add-column 'issue)
(forge-add-column 'pullreq)
(forge-sql [:alter-table pullreq :add :column is-in-org])
(forge-sql [:select * :from issue])
(forge--tablist-columns-vector 'issue)


(forge-sql [:select [number title author updated state is-in-org] :from issue :where (and (= repository $s2) (= state 'open) (not 'is-in-org))] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(forge-sql [:select [number title author updated state is-in-org] :from issue :where (is-not is-in-org nil)] '())
(forge-sql [:select [number title author updated state is-in-org] :from issue :where (is is-in-org nil)])
;; able to update existing issues
;; ability to close issues when they're closed elsewhere
(= 'nil 'nil)
(not nil)








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

(defun org-forge-issues (repo)
  "make a list of org-forge issues that haven't been added yet"
  (let* ((forge (nth 0 repo))
       (repo-id (nth 1 repo))
       (repo-name (nth 2 repo))
       (repo-owner (nth 3 repo))
       (issues (forge-sql [:select [number title author created state id] :from issue :where (and (= repository $s2) (is is-in-org nil))] (forge--tablist-columns-vector) repo-id))
       (pullreqs (forge-sql [:select [number title author state created id] :from pullreq :where (and (= repository $s2) (is is-in-org nil))] (forge--tablist-columns-vector) repo-id))
       )
    (mapcar (lambda (issue) (apply 'org-add-issue repo-name repo-owner repo-id issue)) issues) ; apply org-add-issue to each issue
    ))


(defun org-add-issue (repo-name repo-owner repo-id number title author created state id)
  "add a particular issue to the org-forge file"
  (let ((todo-state (if (equal state 'open) "TODO" "DONE"))
        (created-date (concat "<" (substring created 0 10) ">")))
    (print todo-state)
    (setq forge-global-tmp (list repo-name repo-owner number title author created-date todo-state id repo-id))
    (if (org-capture 4 "fi") ; add to the list
        (emacsql (forge-db) [:update issue :set is-in-org := 't :where (= id $s2)]
                 (forge--tablist-columns-vector) id)
      (print "updating")
      (print repo-id)
      (print id)
        ;; (forge-sql [:update issue :set 'state := t :where (and (= repository $s2) (= id $s3))]
        ;;            (forge--tablist-columns-vector) repo-id id [t])
        ;; (forge-sql [:set state :values '(t) :from issue :where (and (= repository $s2) (= state 'open) (= id $s3))]
                   ;; (forge--tablist-columns-vector) repo-id id)                   ; if it worked, record that its present in the database
        (signal 'error "org capture failed for some reason"))
    )
  )


(org-forge-issues (nth 0 (forge-sql [:select [forge id name owner] :from repository :order-by [(asc owner) (asc name)]] (forge--tablist-columns-vector))))

(defun reset-forge-org (repo)
  "reset the org forge column"
  (emacsql (forge-db) [:update issue :set is-in-org := nil :where (= repository $s2)]
           (forge--tablist-columns-vector) repo)
  (emacsql (forge-db) [:update pullreq :set is-in-org := nil :where (= repository $s2)]
                 (forge--tablist-columns-vector) repo))
(reset-forge-org "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(org-forge-issues (nth 0 (forge-sql [:select [forge id name owner] :from repository :order-by [(asc owner) (asc name)]] (forge--tablist-columns-vector))))

(forge-sql [:select [number title state updated is-in-org] :from issue :where (and (= repository $s2) (is is-in-org nil))] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(forge-sql [:select [number title state updated is-in-org] :from pullreq] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")

(forge-sql [:select state :from issue :where (= repository $s2)] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
(forge-sql [:select [number title state updated is-in-org] :from issue])
(emacsql (forge-db) [:update issue :set is-in-org := nil]
         (forge--tablist-columns-vector))

(forge-sql [:select [number title state updated is-in-org] :from issue, pullreq])
(forge-sql [:select [number title author created state id is-in-org] :from issue :where (and (= repository $s2) (not is-in-org))] (forge--tablist-columns-vector) "Z2l0aHViLmNvbTowMTA6UmVwb3NpdG9yeTI5ODA3MjQ1NQ==")
