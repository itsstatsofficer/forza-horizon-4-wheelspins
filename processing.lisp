; Tools for parsing and processing the data in `wheelspins.txt`.
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

(defun interleave (list1 list2)
  (loop for e1 in list1
        for e2 in list2
        collect e1 collect e2))

;;; Functions for handling the data file
(defparameter data-file-name "./wheelspins.txt")

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
(defun record-cosmetic-p (record)
  (or (eq (record-type record) 'clothing)
      (eq (record-type record) 'horn)))
(defun record-credits-p (record)
  (eq (record-type record) 'credits))
(defun record-car-p (record)
  (eq (record-type record) 'car))
(defun record-autoshow-car-p (record)
  (and (record-car-p record)
       (not (record-wheelspin-only-p record))))
(defun record-wheelspin-exclusive-car-p (record)
  (and (record-car-p record)
       (record-wheelspin-only-p record)))

(defun super-wheelspin-p (record)
  (not (eq (record-wheelspin-type record) 'single)))

(defun make-outcome-summary (record-list label-predicate-alist)
  "Given an association list of labels to predicates,
  returns an association list mapping the labels to a list with two values:
  the first contains the proportion of the records satisfying each predicate,
  and the second contains the actual count of records satisfying each predicate."
  (let ((record-count (length record-list)))
    (mapcar-alist (lambda (predicate)
                    (let ((predicate-satisfying-count (count-if predicate record-list)))
                      (list (/ predicate-satisfying-count record-count) predicate-satisfying-count)))
                  label-predicate-alist)))

(defun outcome-type-summary (record-list)
  "Returns an association list mapping \"Cosmetics\", \"Credits\", \"Autoshow cars\",
  \"Wheelspin exclusives\" to the corresponding proportions."
  (make-outcome-summary record-list
                   (list (cons "Cosmetics" #'record-cosmetic-p)
                         (cons "Credits" #'record-credits-p)
                         (cons "Autoshow cars" #'record-autoshow-car-p)
                         (cons "Wheelspin exclusives" #'record-wheelspin-exclusive-car-p))))

(defun outcome-rarity-summary (record-list)
  "Returns an association list mapping \"Common\", \"Rare\", \"Epic\", \"Legendary\",
  \"Forza Edition\" to the corresponding proportions."
  (make-outcome-summary
    record-list
    (list (cons "Common"        (lambda (record) (eq (record-rarity record) 'common)))
          (cons "Rare"          (lambda (record) (eq (record-rarity record) 'rare)))
          (cons "Epic"          (lambda (record) (eq (record-rarity record) 'epic)))
          (cons "Legendary"     (lambda (record) (eq (record-rarity record) 'legendary)))
          (cons "Forza Edition" (lambda (record) (eq (record-rarity record) 'forza-edition))))))

(defun outcome-value-sum-if (predicate record-list)
  "Returns the sum of the values of the records in the list matching the predicate."
  (reduce #'+ record-list :key (lambda (record)
                                 (if (funcall predicate record) (record-value record) 0))))

(defun outcome-value-average-if (predicate record-list)
  "Returns the average value of the records in the list matching the predicate."
  (round (/ (outcome-value-sum-if predicate record-list)
            (count-if predicate record-list))))

(defun outcome-value-summary (record-list)
  "Returns an association list mapping \"Credits\" and \"Cars\"
  to the average and sum of the corresponding values."
  (list (list "Credits"
              (outcome-value-average-if #'record-credits-p record-list)
              (outcome-value-sum-if #'record-credits-p record-list))
        (list "Cars"
              (outcome-value-average-if #'record-car-p record-list)
              (outcome-value-sum-if #'record-car-p record-list))))

;;; Some interesting analyses
(defparameter database (mapcar #'parse-data-line (read-data-file)))
(defparameter cosmetics-evolution-regular-wheelspin-type
  (mapcar #'outcome-type-summary (split-list 100 (remove-if #'super-wheelspin-p database))))
(defparameter cosmetics-evolution-regular-wheelspin-rarity
  (mapcar #'outcome-rarity-summary (split-list 100 (remove-if #'super-wheelspin-p database))))
(defparameter cosmetics-evolution-regular-wheelspin-value
  (mapcar #'outcome-value-summary (split-list 100 (remove-if #'super-wheelspin-p database))))

(defun make-bar-plot-outcome-evolution (outcome-summary-list chunk-length)
  "Returns TikZ + PGFPlots code to display a list of outcome summaries,
  like the list `cosmetics-evolution-regular-wheelspin-type`.
  Each value is assumed to be a percentage, expressed as a number between 0 and 1."
  (let* ((tikz-template "
            % Save this to chart.tex with e.g.
            %   sbcl --noinform --load processing.lisp --eval '(format T cosmetics-type-outcome-evolution-tex-chart)' --quit > chart.tex
            % then compile with
            %   pdflatex chart.tex
            %   pdftoppm chart.pdf chart -png -r 300
            \\documentclass{standalone}
            \\usepackage{tikz}
            \\usepackage{pgfplots}
            \\pgfplotsset{compat=1.18}
            \\definecolor{fhcommon}{HTML}{44CC77}
            \\definecolor{fhrare}{HTML}{33BBEE}
            \\definecolor{fhepic}{HTML}{BB66EE}
            \\definecolor{fhlegendary}{HTML}{FFCC33}
            \\definecolor{fhfe}{HTML}{FF33FF} % Not quite the gradient but works
            \\begin{document}
            \\begin{tikzpicture}
            \\begin{axis}[
                    xbar stacked,
                    xmin = 0,
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
                    cycle list = {
                        {black!50!fhcommon,fill=fhcommon},
                        {black!50!fhrare,fill=fhrare},
                        {black!50!fhepic,fill=fhepic},
                        {black!50!fhlegendary,fill=fhlegendary},
                        {black!70!fhfe,fill=fhfe} % Darker for slightly better readability
                    },
                ]
                \\legend{~{~A~^,~}}
                ~{\\addplot coordinates {~A};~}
            \\end{axis}
            \\end{tikzpicture}
            \\end{document}")
         (keys (mapcar #'car (car outcome-summary-list))) ; Use the first row to get the key list
         (make-coordinate (lambda (key outcome-summary index)
                            (format nil "(~d,~d)"
                                    (* chunk-length (second (assoc key outcome-summary :test #'equalp)))
                                    (* chunk-length index))))
         (outcome-coordinates-list
           (loop for key in keys collect
                 (format nil "~{~A~^ ~}"
                         (loop for outcome-summary in outcome-summary-list
                               for index from 1
                               collect (funcall make-coordinate key outcome-summary index))))))
    (format nil tikz-template keys outcome-coordinates-list)))

(defparameter cosmetics-type-outcome-evolution-tex-chart
  ; Note: I manually removed the cycle list before generating cosmetics-evolution.png
  (make-bar-plot-outcome-evolution cosmetics-evolution-regular-wheelspin-type 100))

(defparameter cosmetics-rarity-outcome-evolution-tex-chart
  (make-bar-plot-outcome-evolution cosmetics-evolution-regular-wheelspin-rarity 100))

(defun make-linear-plot-outcome-evolution (outcome-summary-list chunk-length)
  "Like make-bar-plot-outcome-evolution, but a line graph instead.
  It is also transposed compared to that function,
  and numbers are not assumed to simply be percentages."
  (let* ((tikz-template "
            \\documentclass{standalone}
            \\usepackage{tikz}
            \\usepackage{pgfplots}
            \\pgfplotsset{compat=1.18}
            \\begin{document}
            \\begin{tikzpicture}
            \\begin{axis}[
                    sharp plot,
                    xtick = data,
                    x tick label style = {rotate = 45, anchor = north east},
                    scaled y ticks = base 10:-3,
                    yticklabel = {\\pgfmathprintnumber{\\tick}k},
                    ytick scale label code/.code = {},
                    axis x line* = left,
                    axis y discontinuity = parallel,
                    axis y line* = left,
                    ylabel = {Value},
                    legend style = {
                        at = {(0.5,-0.15)},
                        anchor = north,
                    },
                    legend columns = -1,
                ]
                \\legend{~{~A~^,~}}
                ~{\\addplot coordinates {~A};~}
            \\end{axis}
            \\end{tikzpicture}
            \\end{document}")
         (keys (mapcar #'car (car outcome-summary-list)))
         (make-coordinate (lambda (key outcome-summary index)
                            (format nil "(~d,~d)"
                                    (* chunk-length index) ; First index, compared to before
                                    (second (assoc key outcome-summary :test #'equalp)))))
         (outcome-coordinates-list
           (loop for key in keys collect
                 (format nil "~{~A~^ ~}"
                         (loop for outcome-summary in outcome-summary-list
                               for index from 1
                               collect (funcall make-coordinate key outcome-summary index))))))
    (format nil tikz-template keys outcome-coordinates-list)))

(defparameter cosmetics-value-outcome-evolution-tex-chart
  ; I manually split this chart in two, for better readability.
  ; I added `ymin = 37500` to the first chart to fix the discontinuity line.
  ; In the second chart,
  ; I also used the PGFPlots option `cycle list shift = 1` to get different colors
  ; and `ymin = 0` to remove axis discontinuity.
  (make-linear-plot-outcome-evolution cosmetics-evolution-regular-wheelspin-value 100))

(defun outlier-p (record)
  "Determines whether a record is worth 1 million credits or more."
  (and (record-value record)
       (>= (record-value record) 1000000)))

(defparameter cosmetics-evolution-regular-wheelspin-value-no-outliers
  ; First split the database in 100-record chunks, then remove the outliers from each chunk.
  (make-linear-plot-outcome-evolution
    (mapcar #'outcome-value-summary
            (mapcar (lambda (record-list) (remove-if #'outlier-p record-list))
                    (split-list 100 (remove-if #'super-wheelspin-p database))))
    100))

(defparameter super-wheelspin-database
  ; Subdatabase with only the superwheelspins. Also removes the (single) cosmetic superwheelspin.
  (remove-if (lambda (r) (or (record-cosmetic-p r) (not (super-wheelspin-p r)))) database))

(defparameter regular-wheelspin-database
  ; Subdatabase with only the regular wheelspins (contains cosmetics)
  (remove-if #'super-wheelspin-p database))

(defparameter early-game-regular-wheelspin-database
  ; First 700 regular wheelspins (contains cosmetics)
  (subseq regular-wheelspin-database 0 700))

(defparameter late-game-regular-wheelspin-database
  ; Remaining regular wheelspins (no cosmetics)
  (subseq regular-wheelspin-database 700))

(defparameter subdatabase-alist
  ; Association list mapping labels to segments of the database
  (list (cons "Regular Wheelspins (all)" regular-wheelspin-database)
        (cons "Regular Wheelspins (early game)" early-game-regular-wheelspin-database)
        (cons "Regular Wheelspins (late game)" late-game-regular-wheelspin-database)
        (cons "Super Wheelspins (all wheels)" super-wheelspin-database)
        (cons "Super Wheelspin (left wheel)"
              (remove-if-not (lambda (r) (eq (record-wheelspin-type r) 'left))
                             super-wheelspin-database))
        (cons "Super Wheelspin (center wheel)"
              (remove-if-not (lambda (r) (eq (record-wheelspin-type r) 'center))
                             super-wheelspin-database))
        (cons "Super Wheelspin (right wheel)"
              (remove-if-not (lambda (r) (eq (record-wheelspin-type r) 'right))
                             super-wheelspin-database))))

(defun make-bar-plot-subdatabase-outcomes (subdatabase-outcomes-alist plot-title)
  "Similar to `make-bar-plot-outcome-evolution`,
  but using the association list labels for the y ticks."
  (let* ((tikz-template "
            \\documentclass{standalone}
            \\usepackage{tikz}
            \\usepackage{pgfplots}
            \\pgfplotsset{compat=1.18}
            \\definecolor{fhcommon}{HTML}{44CC77}
            \\definecolor{fhrare}{HTML}{33BBEE}
            \\definecolor{fhepic}{HTML}{BB66EE}
            \\definecolor{fhlegendary}{HTML}{FFCC33}
            \\definecolor{fhfe}{HTML}{FF33FF} % Not quite the gradient but works
            \\begin{document}
            \\begin{tikzpicture}
            \\begin{axis}[
                    title = {~A},
                    xbar stacked,
                    xmin = 0,
                    ytick = data,
                    nodes near coords = {\\pgfmathprintnumber\\pgfplotspointmeta\\%},
                    hide x axis = true,
                    symbolic y coords = {~A},
                    legend style = {
                        at = {(0.5,0)},
                        anchor = north,
                    },
                    legend columns = -1,
                    axis y line* = none,
                    y dir = reverse,
                    cycle list = {
                        {black!50!fhcommon,fill=fhcommon},
                        {black!50!fhrare,fill=fhrare},
                        {black!50!fhepic,fill=fhepic},
                        {black!50!fhlegendary,fill=fhlegendary},
                        {black!70!fhfe,fill=fhfe} % Darker for slightly better readability
                    },
                ]
                \\legend{~{~A~^,~}}
                ~{\\addplot coordinates {~A};~}
            \\end{axis}
            \\end{tikzpicture}
            \\end{document}")
         (y-label-list (format nil "~{~A~^,~}"
                               (mapcar #'car subdatabase-outcomes-alist)))
         (keys (mapcar #'car (cdar subdatabase-outcomes-alist))) ; Use the first row to get the key list
         (format-percentage (lambda (n) (format nil "~A" (/ (round (* 1000 n)) 10.0))))
         (make-coordinate (lambda (key outcome-summary label)
                            (format nil "(~d,{~d})"
                                    (funcall format-percentage
                                             (second (assoc key outcome-summary :test #'equalp)))
                                    label)))
         (outcome-coordinates-list
           (loop for key in keys collect
                 (format nil "~{~A~^ ~}"
                         (loop for subdatabase-outcome-summary in subdatabase-outcomes-alist
                               for index from 1
                               collect (funcall make-coordinate
                                                key
                                                (cdr subdatabase-outcome-summary)
                                                (car subdatabase-outcome-summary)))))))
    (format nil tikz-template plot-title y-label-list keys outcome-coordinates-list)))

(defparameter subdatabase-rarity-outcome-tex-chart
  ; I manually removed the `nodes near coords` option for readability
  (make-bar-plot-subdatabase-outcomes (mapcar-alist #'outcome-rarity-summary subdatabase-alist)
                                      "Overall Rarity Distribution"))

(defun summary-list-as-table (subdatabase-outcomes-alist)
  "Returns a string formatting the list of outcomes as a Markdown table"
  (let* ((header-data-column-names (mapcar #'car (cdar subdatabase-outcomes-alist)))
         (header-labels (cons "" header-data-column-names)) ; Prepend column, for the row labels
         (format-cell (lambda (alist-pair) ; Actually a triplet
                        (format nil "~D (~D%)"
                                (third alist-pair)
                                (/ (round (* (second alist-pair) 1000)) 10.0))))
         (rows (loop for outcome-summary in subdatabase-outcomes-alist
                     collect (cons (car outcome-summary) ; Prepend the row label
                                   (mapcar format-cell (cdr outcome-summary)))))
         (get-column (lambda (index) ; Get the column, including the header
                       (cons (nth index header-labels)
                             (mapcar (lambda (r) (nth index r)) rows))))
         (column-widths (loop for index from 0 below (length header-labels)
                              collect (apply #'max (mapcar #'length (funcall get-column index)))))
         (format-row (lambda (row)
                       (format nil "| ~{~vA~^ | ~} |" (interleave column-widths row))))
         (divider-row (format nil "|~{~A~^+~}|"
                              (loop for width in column-widths
                                    collect (make-string (+ 2 width) :initial-element #\-))))
         (table (format nil "~A~%~A~%~{~A~%~}"
                        (funcall format-row header-labels)
                        divider-row
                        (mapcar format-row rows))))
    table))

(defparameter subdatabase-rarity-outcome-md-table
  ; This can be printed out using e.g.
  ;   sbcl --noinform --load processing.lisp --eval '(format T subdatabase-rarity-outcome-md-table)' --quit
  (summary-list-as-table (mapcar-alist #'outcome-rarity-summary subdatabase-alist)))

(defparameter subdatabase-type-outcome-tex-chart
  (make-bar-plot-subdatabase-outcomes (mapcar-alist #'outcome-type-summary subdatabase-alist)
                                      "Overall Type Distribution"))

(defparameter subdatabase-type-outcome-md-table
  ; Note: I manually removed the cycle list,
  ; and appended `+[nodes near coords style = right]` to the last `\addplot` command
  (summary-list-as-table (mapcar-alist #'outcome-type-summary subdatabase-alist)))
