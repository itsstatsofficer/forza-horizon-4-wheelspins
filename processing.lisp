; Tools for parsing and processing the data in `wheelspin.md`.
;
;;; File format
;
; The wheelspin results are recorded in order.
;
; Every cash reward is a multiple of one thousand,
; so they are simply recorded like "Rare: Credits - 130k".
; Clothing is recorded like "Common: Clothing - All Black Canvas".
;
; Cars have two options:
; for cars that _can_ be purchased from the Autoshow,
; I record their value and the name of the car
; (like "Legendary: Car, worth 140k credits - 1956 Lotus Eleven").
; Cars that _cannot_ be purchased from the Autoshow say "wheelspin only" after "Car"
; ("Common: Car, wheelspin only, worth 28k credits - 1993 Ford SVT Cobra R").
; The value of the car comes from <https://forza.fandom.com/wiki/Forza_Horizon_4/Cars>.
;
; I do not know the rarity of Forza Edition cars,
; so I simply recorded them as "Forza Edition"
; (e.g. "Forza Edition: Car, wheelspin only, worth 316k credits - 1980 Renault 5 Turbo Forza Edition").
;
; For Super Wheelspins,
; each wheelspin is recorded individually,
; with a prefix saying whether it was the left, center, or right wheelspin
; (e.g. "Left - Common: Credits - 10k").
; The three wheelspins are recorded in sequence.

;;; Utilities
(defun split-list (chunk-length list)
  "Splits the list into consecutive chunks; returns a list of length-n lists."
  ; TODO: optionally return any leftovers if (length l) is not a multiple of chunk-length
  (let ((suffix (nthcdr chunk-length list)))
    (if (not suffix)
        nil
        (cons (subseq list 0 chunk-length)
              (split-list chunk-length suffix)))))

(defun mapcar-alist (function alist)
  "Like mapcar, but considers alist an association list, and applies the function to the values."
  (mapcar (lambda (pair)
            (cons (car pair)
                  (funcall function (cdr pair))))
          alist))

;;; Functions for handling the data file
(defparameter data-file-name "./wheelspin.md")

(defun read-data-file ()
  "Returns the contents of *data-file-name* in a list of strings"
  (with-open-file (filestream data-file-name)
    (loop for line = (read-line filestream nil)
          while line
          collect line)))

(defparameter data-file-sample
 '("Rare: Credits - 130k"
   "Legendary: Car, worth 140k credits - 1956 Lotus Eleven"
   "Common: Car, worth 43.5k credits - 2017 Abarth 124 Spider"
   "Left - Common: Car, wheelspin only, worth 28k credits - 1993 Ford SVT Cobra R"
   "Center - Epic: Clothing - Golden Running Shorts"
   "Right - Common: Credits - 15k"
   "Forza Edition: Car, wheelspin only, worth 330k credits - 1972 Ford Falcon XA GT-HO Forza Edition"
   "Common: Clothing - All Black Canvas"))

;;; Parsing

; Parsing output format: (rarity type value-or-nil wheelspin-only? description wheelspin-type)
; type: one of 'car 'credits 'clothing 'horn
; rarity: one of 'common 'rare 'epic 'legendary 'forza-edition
; value: either a number or nil (for cosmetics)
; wheelspin-only?: T or nil
; description: string describing the object
; wheelspin-type: one of 'left 'center 'right for super wheelspins, 'single for regular wheelspins
(setf (fdefinition 'record-rarity) #'first)
(setf (fdefinition 'record-type) #'second)
(setf (fdefinition 'record-value) #'third)
(setf (fdefinition 'record-wheelspin-only-p) #'fourth)
(setf (fdefinition 'record-description) #'fifth)
(setf (fdefinition 'record-wheelspin-type) #'sixth)

(defun parse-data-line-car (line)
  "Parses \"Car, wheelspin only, worth 28k credits - 1993 Ford SVT Cobra R\" into '(28000 T \"1993 Ford SVT Cobra R\")"
  (let* ((wheelspin-only? (not (not (search "wheelspin only" line))))
         (position-of-word-worth (search "worth" line))
         (credits-value-start (+ position-of-word-worth (length "worth")))
         (credits-value-end (position #\k line :start credits-value-start))
         (value-as-string (subseq line credits-value-start credits-value-end))
         (value (read-from-string value-as-string)) ; TODO: Don't use read-from-string
         (description-start (+ 1 (position #\- line :start credits-value-end)))
         (description (string-trim " " (subseq line description-start))))
    (list (truncate (* 1000 value)) wheelspin-only? description)))

(defun parse-data-line-cosmetic (line)
  "Parses \"Clothing - All Black Canvas\" into '(nil T \"All Black Canvas\")"
  (let* ((description (subseq line (+ 1 (search "-" line)))))
    (list nil T (string-trim " " description))))

(defun parse-data-line-credits (line)
  "Returns '(value nil \"Credits\")"
  (let* ((last-space (position #\Space line :from-end T))
         (value-as-string (subseq line (+ last-space 1) (- (length line) 1))) ; Discards the last "k"
         (value (parse-integer value-as-string)))
    (list (* 1000 value) nil "Credits")))

(defun parse-data-line-helper (line)
  "Assumes no super-wheelspin tag, returns '(rarity type value-or-nil wheelspin-only? description)"
  (let* ((colon-index (search ":" line))
         (rarity-substr (string-trim " " (subseq line 0 colon-index)))
         (rarity (cond ((equalp rarity-substr "Common") 'common)
                       ((equalp rarity-substr "Rare") 'rare)
                       ((equalp rarity-substr "Epic") 'epic)
                       ((equalp rarity-substr "Legendary") 'legendary)
                       ((equalp rarity-substr "Forza Edition") 'forza-edition)
                       (T nil))) ; TODO: error handling
         (rarity-less-substr (string-trim " " (subseq line (+ colon-index 1)))))
    (cond ((eq 0 (search "Clothing" rarity-less-substr))
           (append (list rarity 'clothing) (parse-data-line-cosmetic rarity-less-substr)))
          ((eq 0 (search "Horn" rarity-less-substr))
           (append (list rarity 'horn) (parse-data-line-cosmetic rarity-less-substr)))
          ((eq 0 (search "Credits" rarity-less-substr))
           (append (list rarity 'credits) (parse-data-line-credits rarity-less-substr)))
          ((eq 0 (search "Car" rarity-less-substr))
           (append (list rarity 'car) (parse-data-line-car rarity-less-substr)))
          (T nil)))) ; TODO: error handling

(defun parse-data-line (line)
  (let* ((dash-index (search "-" line))
         (wheelspin-type-prefix (string-trim " " (subseq line 0 dash-index))) ; TODO: Error handling
         (line-suffix (string-trim " " (subseq line (+ dash-index 1)))))
    (cond
      ((equalp wheelspin-type-prefix "Left")   (append (parse-data-line-helper line-suffix) '(left)))
      ((equalp wheelspin-type-prefix "Center") (append (parse-data-line-helper line-suffix) '(center)))
      ((equalp wheelspin-type-prefix "Right")  (append (parse-data-line-helper line-suffix) '(right)))
      (T                                       (append (parse-data-line-helper line) '(single))))))

; Tool to reconstruct the textual representation of tuples of the form
; (rarity type value-or-nil wheelspin-only? description wheelspin-type).
(defun format-record-tuple (record)
  (let* ((super-wheelspin (case (record-wheelspin-type record)
                                   (left "Left - ")
                                   (center "Center - ")
                                   (right "Right - ")
                                   (single "")))
         (rarity (case (record-rarity record)
                   (common "Common: ")
                   (rare "Rare: ")
                   (epic "Epic: ")
                   (legendary "Legendary: ")
                   (forza-edition "Forza Edition: ")))
         (value-as-number (if (record-value record) (record-value record) 0))
         (value (if (eq (mod value-as-number 1000) 0) (/ value-as-number 1000) (/ value-as-number 1000.0)))
         (wheelspin-only (if (record-wheelspin-only-p record) "wheelspin only, " "")))
  (case (record-type record)
    (clothing (format nil "~A~AClothing - ~A" super-wheelspin rarity (record-description record)))
    (horn (format nil "~A~AHorn - ~A" super-wheelspin rarity (record-description record)))
    (credits (format nil "~A~ACredits - ~dk" super-wheelspin rarity value))
    (car (format nil "~A~ACar, ~Aworth ~dk credits - ~A"
                 super-wheelspin rarity wheelspin-only value (record-description record))))))

;;; Actual data processing
(defun cosmetic-record-p (record)
  (or (eq (record-type record) 'clothing)
      (eq (record-type record) 'horn)))
(defun credits-record-p (record)
  (eq (record-type record) 'credits))
(defun autoshow-car-p (record)
  (and (eq (record-type record) 'car)
       (not (record-wheelspin-only-p record))))
(defun wheelspin-exclusive-car-p (record)
  (and (eq (record-type record) 'car)
       (record-wheelspin-only-p record)))

(defun super-wheelspin-p (record)
  (not (eq (record-wheelspin-type record) 'single)))

(defun outcome-summary (record-list)
  "Returns an association list mapping 'cosmetics 'credits 'autoshow 'wheelspin-only
  to the corresponding proportions."
  (let ((record-count (length record-list))
        (cosmetics-count (count-if #'cosmetic-record-p record-list))
        (autoshow-count (count-if #'autoshow-car-p record-list))
        (wheelspin-only-count (count-if #'wheelspin-exclusive-car-p record-list))
        (credits-count (count-if #'credits-record-p record-list)))
    (list (cons 'cosmetics (/ cosmetics-count record-count))
          (cons 'credits (/ credits-count record-count))
          (cons 'autoshow (/ autoshow-count record-count))
          (cons 'wheelspin-only (/ wheelspin-only-count record-count)))))

;;; Some interesting analyses
(defparameter database (mapcar #'parse-data-line (read-data-file)))
(defparameter regular-wheelspin-cosmetics-evolution
  (mapcar #'outcome-summary (split-list 100 (remove-if #'super-wheelspin-p database))))

(defun make-graph-outcome-evolution (outcome-list chunk-length)
  "Returns TikZ + PGFPlots code to display a list of outcome summaries,
  like `regular-wheelspin-cosmetics-evolution`."
  (let* ((tikz-template "
            % Save this to chart.tex, then compile with
            %   pdflatex chart.tex
            %   pdftoppm chart.pdf chart -png -r 300
            \\documentclass{standalone}
            \\usepackage{tikz}
            \\usepackage{pgfplots}
            \\pgfplotsset{compat=1.18}
            \\begin{document}
            \\begin{tikzpicture}
            \\begin{axis}[
                    xbar stacked,
                    ytick = data,
                    nodes near coords,
                    hide x axis = true,
                    legend style = {
                        at = {(0.5,0)},
                        anchor = north,
                    },
                    legend columns = -1,
                    axis y line* = left,
                    y dir = reverse,
                ]
                \\addplot coordinates {~A};
                \\addplot coordinates {~A};
                \\addplot coordinates {~A};
                \\addplot coordinates {~A};
                \\legend{Cosmetics,Credits,Autoshow cars,Wheelspin exclusives}
            \\end{axis}
            \\end{tikzpicture}
            \\end{document}")
         (make-label-list (lambda (key)
                            (apply #'concatenate 'string
                                   (loop for outcome in outcome-list for index from 1
                                         collect (format nil "(~d,~d) "
                                                         (* chunk-length (cdr (assoc key outcome)))
                                                         (* chunk-length index))))))
         (cosmetics-label-list (funcall make-label-list 'cosmetics))
         (credits-label-list (funcall make-label-list 'credits))
         (autoshow-label-list (funcall make-label-list 'autoshow))
         (wheelspin-exclusives-label-list (funcall make-label-list 'wheelspin-only)))
    (format nil tikz-template
            cosmetics-label-list
            credits-label-list
            autoshow-label-list
            wheelspin-exclusives-label-list)))

(defparameter cosmetics-outcome-evolution-tex-chart
  (make-graph-outcome-evolution (regular-wheelspin-cosmetics-evolution 100)))
