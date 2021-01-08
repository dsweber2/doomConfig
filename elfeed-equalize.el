;;; flattenRates.el -*- lexical-binding: t; -*-

;; The core idea here is to add a scoring function that

(-1000 *)
(defgroup elfeed-equalize nil
  "Order the entries for each feed so you aren't swamped by high speed feeds"
  :group 'comm)

(defcustom elfeed-equalize-rate 10
  "the spacing between entries (if this is 10, then second is -10, third is -20)"
  :group 'elfeed-equalize)

(defcustom elfeed-equalize-min-score (* -100 elfeed-equalize-rate)
  "the lowest score that an entry can have; default is -100 by the rate"
  :group 'elfeed-equalize)

(defcustom elfeed-rank-meta-keyword :elfeed-equalize/rank
  "Default keyword for storing scores in Elfeed entry metadata."
  :group 'elfeed-score
  :type 'symbol)
;; for each feed, store the number of entries

(defun elfeed-score--get-score-on-entry (entry score)
  "Set the score on ENTRY to SCORE."
  (setf (elfeed-meta entry elfeed-score-meta-keyword) score))

(defun elfeed-score--get-score-from-entry (entry)
  "Retrieve the score from ENTRY."
  (elfeed-meta entry elfeed-score-meta-keyword elfeed-score-default-score))


(defcustom elfeed-equalize-time-weight)
elfeed-search-date-format
elfeed-time-duration

(defun date-to-int-score (date)
  "convert the date of a elfeed entry to a score adjustment"
  )
(elfeed-entry-link "Exposure to airborne gold nanoparticles: a review of current toxicological data on the respiratory tract")
(elfeed-entry-date "Fito Olivares — Cumbia Caliente [Cumbia] (1992)")
(elfeed-search-format-date (elfeed-entry-date "The Panturas - Fisherman's Slut [Surf Rock] (2017)"))
elfeed-
elfeed-score-score-file
(defun elfeed-score-sort (a b)
  "Return non-nil if A should sort before B.

`elfeed-score' will substitute this for the Elfeed scoring function."

  (let ((a-score (elfeed-score--get-score-from-entry a))
        (b-score (elfeed-score--get-score-from-entry b)))
    (if (> a-score b-score)
        t
      (let ((a-date (elfeed-entry-date a))
            (b-date (elfeed-entry-date b)))
        (and (eq a-score b-score) (> a-date b-date))))))
