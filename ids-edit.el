;;; ids-edit.el --- IDS (Ideographic Description Sequence) editing tool  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tools for Editing Ideographic Variation Sequences
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3"))
;; Keywords: text
;; Namespace: ids-edit-
;; Human-Keywords: Ideographic Description Sequence
;; Version: 1.140227
;; URL: http://github.com/kawabata/ids-edit

;;; Commentary:
;;
;; * IDS Editing Tool for Emacs
;;
;; This tool provides various IDS (Ideographic Description Sequence)
;; editing tools for Emacs. IDS represents a structure of Ideographs. For
;; example, "幡" is decomposed to "⿰巾番" in IDS, and " "⿱艹化" will be
;; composed to "花". "⿰" and "⿱" are called IDCs (Ideographic
;; Description Characters).
;;
;; ** Basic setup
;;
;; Please BYTE-COMPLILE the elisp file. That will integrate data files
;; into compiled lisp file.
;;
;; : (autoload 'ids-edit "ids-edit" nil t) ;; if necessary
;; : (global-set-key (kbd "M-U") 'ids-edit) ;; sample keybinding.
;;
;; ** Decomposing CJK Ideographs to IDS
;;
;; Just `M-x ids-edit' after the CJK Ideographs. There should be no
;; numerics, ideographs or IDCs at the cursor.
;;
;; ** Composing IDS to CJK Ideograph
;;
;; Just `M-x ids-edit' on partial IDS. It may contain a (range of)
;; strokes number. When there are multiple candidates, they will all be
;; inserted to the current position with bracket. Attaching G,T,J,K at
;; the end will limit the candidates to specific charset (namely, GB2312,
;; Big5, JISX0213/0212, KSX1011).
;;
;; For example, "⿰氵20-22J" will be composed to "[灊灝灞灣]".

;;; Code:

(eval-and-compile
(defun ids--edit-addhash (key value table)
  "Add to KEY a VALUE in table TABLE."
  (let* ((old-value (gethash key table)))
    (if old-value (nconc old-value (list value))
      (puthash key (list value) table)))))

(eval-and-compile
(defvar ids-edit-table
  (eval-when-compile
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                             load-file-name
                                             buffer-file-name)))
           (ids-file (expand-file-name "ids.txt" directory))
           (table (make-hash-table)))
    (unless (file-exists-p ids-file) (error "Data file not found!"))
    (with-temp-buffer
      (insert-file-contents ids-file)
      (while (re-search-forward
              "&CDP-\\(..\\)\\(..\\);" nil t)
        (let* ((x (string-to-number (match-string 1) 16))
               (y (string-to-number (match-string 2) 16))
               (x (+ (* (- x #x80) 157)
                     (if (< y 129) (- y 64) (- y 98)))))
          (replace-match (char-to-string x))))
      (goto-char (point-min))
      (while (re-search-forward "\\[.+?\\]" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^U.+?	\\(.\\)	\\(.+\\)" nil t)
        (let* ((char (string-to-char (match-string 1)))
               (ids  (split-string (match-string 2))))
          (puthash char ids table))))
    table))
  "IDS table."))

(defvar ids-edit-component-table
  (eval-when-compile
    (let ((table (make-hash-table)))
      (maphash
       (lambda (char ids-list)
         (dolist (ids ids-list)
           (dolist (component (string-to-list ids))
             (when (< #x3000 component)
               (ids--edit-addhash component char table)))))
       ids-edit-table)
      table))
  "IDS Component table.")

(defvar ids-edit-stroke-table
  (eval-when-compile
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                               load-file-name
                                               buffer-file-name)))
           (strokes-file (expand-file-name "ucs-strokes.txt" directory))
           (table (make-hash-table)))
      (with-temp-buffer
        (insert-file-contents strokes-file)
        (while (re-search-forward "^U\\+\\([0-9A-F]+\\)	\\(.+\\)" nil t)
          (let ((char (string-to-number (match-string 1) 16))
                (strokes (split-string (match-string 2) ",")))
            (dolist (stroke strokes)
              (ids--edit-addhash char (string-to-number stroke) table))))
        table))))

;; Patterns to input.
;; - at least one ideographs.
;; - ⿰山30J
(defconst ivs-edit-regexp
  "\\(\\(?:[⿰-⿻]\\|\\cC\\)+\\)?\\(?:\\([0-9]+\\)\\(-[0-9]+\\)?\\)?\\(\\cC+\\)?\\([CJKT]\\)?"
  "Regular Expression for searching IDS.")

;;;;;###autoload
(defun ids-edit (arg)
  "Insert and Verify IDS after the current point.
Prefix argument ARG forces to decompose previous Ideograph."
  (interactive "P")
  (if (and (null arg)
           (looking-at ivs-edit-regexp)
           (or (match-string 1)
               (match-string 4))) (ids-edit--compose (match-data))
    (when (looking-back "\\cC")
      (let ((ids-list (gethash (char-before (point)) ids-edit-table)))
        (when ids-list
          (delete-char -1)
          (insert (ids-edit--regularize ids-list)))))))

(defun ids-edit--regularize (ids-list)
  "Create Regularized expression of IDS-LIST."
  (let ((regexp
         (replace-regexp-in-string
          "?:" ""
          (replace-regexp-in-string
           "\\\\" "" (regexp-opt ids-list)))))
    (if (string-match "^(\\(.+\\))$" regexp)
        (substring regexp 1 -1)
      regexp)))

(defun ids-edit--cartesian-product (head &rest tails)
  "Make a list of combinations of list in arguments (HEAD and TAILS).
That means to create the all possible combinations of sequences.
For example, if the first sequence contains 3 elements, and the
second one contains 5 elements, then 15 lists of length 2 will be
returned."
  (if tails
      (cl-mapcan (lambda (y) (mapcar (lambda (x) (cons x y)) head))
                 (apply 'ids-edit--cartesian-product tails))
    (mapcar 'list head)))

(defun ids-edit--compose (match-data)
  "Compose MATCH-DATA."
  (set-match-data match-data)
  (let* ((first (match-string 1))
         (strokes (match-string 2))
         (strokes2 (match-string 3))
         (last (match-string 4))
         (flag (match-string 5))
         (char (when (string-match "\\cC" (concat last first))
                 (string-to-char (match-string 0 (concat last first)))))
         (candidates (gethash char ids-edit-component-table))
         max min regexp filtered)
    ;;(message "first=%s str=%s str2=%s last=%s flag=%s char=%s
    ;;         candidates=%s" first strokes strokes2 last flag char
    ;;         (pp-to-string candidates))
    (if strokes (setq min (string-to-number strokes)))
    (setq max (if strokes2 (- (string-to-number strokes2)) min))
    (setq regexp (concat first (if strokes "\\(.+\\)") last))
    (setq filtered
          (cl-remove-if-not
           (lambda (char)
             (and (if flag
                    (cond ((equal flag "J")
                           (or (encode-char char 'japanese-jisx0213-1)
                               (encode-char char 'japanese-jisx0213-2)
                               (encode-char char 'japanese-jisx0212)))
                          ((equal flag "G")
                           (encode-char char 'chinese-gb2301))
                          ((equal flag "T")
                           (or (encode-char char 'chinese-big5-1)
                               (encode-char char 'chinese-big5-2)))
                          ((equal flag "K")
                           (encode-char char 'korean-ksc5601))) t)
                  (ids-edit--filter char regexp min max)))
           candidates))
    (set-match-data match-data)
    (setq filtered (cl-remove-duplicates filtered))
    (when filtered
      (delete-region (match-beginning 0) (match-end 0))
      (insert (if (/= 1 (length filtered)) "[" "")
              (apply 'string filtered)
              (if (/= 1 (length filtered)) "]" "")))))

(defun ids-edit--filter (char regexp min max)
  "T if any Ids of CHAR match REGEXP and strokes are between MIN and MAX.
Strokes are compared with wildcard (match-string 1) part of REGEXP."
  (let ((ids-list (gethash char ids-edit-table))
        return-value)
    (dolist (ids ids-list)
      (when (string-match regexp ids)
        (if min
            (let ((strokes (ids-edit--strokes (match-string 1 ids))))
              (dolist (stroke strokes)
                (when (and (<= min stroke) (<= stroke max))
                  (setq return-value t))))
          (setq return-value t))))
    return-value))

(defun ids-edit--strokes (string)
  "Return total storkes number of STRING."
  (mapcar (lambda (x) (apply '+ x))
          (apply 'ids-edit--cartesian-product
                 (mapcar (lambda (x) (or (gethash x ids-edit-stroke-table) '(0)))
                         (string-to-list string)))))

(provide 'ids-edit)

;;; ids-edit.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
