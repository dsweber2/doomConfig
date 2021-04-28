;;; flattenRates.el -*- lexical-binding: t; -*-

;; Two things I need to do:
;; for each feed, add a field that stores the number of articles in the past elfeed-equalize-window
;; make a custom sorting function that combines the date with the score, using the number of articles as a rate
(defgroup elfeed-equalize nil
  "Order the entries for each feed so you aren't swamped by high speed feeds"
  :group 'comm)

(defcustom elfeed-equalize-rate 10
  "the spacing between entries (if this is 10, then second is -10, third is -20)"
  :group 'elfeed-equalize)

(defcustom elfeed-equalize-window 30
  "the length of time over which to count articles"
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

;; A simple alternative: switch the order with a probability given by the
;; difference in scores
(defun elfeed-score-softmax-swap (a b)
  "Return non-nil if A should sort before B. This is a probabilistic comparison"

  (let* ((a-score (elfeed-score--get-score-from-entry a))
         (b-score (elfeed-score--get-score-from-entry b))
         (a-date  (elfeed-entry-date a))
         (b-date  (elfeed-entry-date b)))
    (if (softmax-sort (+ a-score (elfeed-score/date-score (- a-date b-date))) b-score))
   ))
(defun rand ()
  (/ (float (random most-positive-fixnum)) most-positive-fixnum))
(exp (- b-score a-score))
(exp (* 2 0.0))
(elfeed-entry-date )
(defun softmax (x)
  (let ((term (exp (* 2 x))))
    (/ term (+ term 1)))
  )
(defcustom elfeed-equalize-date-to-score 6048
  "how many seconds correspond to a single score point. Default is 6048 so that
         a week difference gives a score of 100"
  :group 'elfeed-equalize)
(defcustom elfeed-equalize-random-rate (/ (+ (log 3) (/ (log 11) 2)) 100.0)
  "the coefficient for converting scores to softmax eval. Default maps 100 to a
        correct sorting probability of 99%)"
  :group 'elfeed-equalize)
(setq elfeed-equalize-random-rate (/ (+ (log 3) (/ (log 11) 2)) 100.0))
(setq elfeed-equalize-date-to-score 10)
(softmax (* elfeed-equalize-random-rate (- -100 10)))
(defun elfeed-score/date-score (sec)
       "convert a number of seconds into a score using rate c"
       (/ sec elfeed-equalize-date-to-score))
(elfeed-score/date-score 1023124)
(/ (+ 6048 1) 6048)
(defun elfeed-score/softmax-sort (a b)
  (let ((flip-prob (softmax (* elfeed-equalize-random-rate (- a b))))
        (rolled-val (rand)))
    (> flip-prob rolled-val)))
(softmax (* elfeed-equalize-random-rate (- 1 -1)))
(softmax-sort -1 1 elfeed-equalize-random-rate)
(softmax (* 100 .01))
(/ 3.0 2.0)
(rand)
