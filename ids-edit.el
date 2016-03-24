;;; ids-edit.el --- IDS (Ideographic Description Sequence) editing tool  -*- lexical-binding: t -*-

;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Description: Tools for Editing Ideographic Variation Sequences
;; Created: 2014-01-01
;; Package-Requires: ((emacs "24.3"))
;; Keywords: text
;; Namespace: ids-edit-
;; Human-Keywords: Ideographic Description Sequence
;; Version: 1.160325
;; URL: http://github.com/kawabata/ids-edit

;;; Commentary:
;;
;; * IDS Editing Tool for Emacs
;;
;; This file provides IDS (Ideographic Description Sequence)
;; composition/decomposition tools for Emacs. The IDS represents a
;; structure of Ideographs.  For example, "地" can be decomposed to
;; "⿰土也" in IDS, and "⿱艹化" can be composed to "花". "⿰" and "⿱"
;; are called IDCs (Ideographic Description Characters).
;;
;; ** Basic setup
;;
;; Please BYTE-COMPILE the elisp file. That will integrate data files
;; into compiled lisp file.
;;
;; : (autoload 'ids-edit-mode "ids-edit" nil t) ;; if necessary
;; : (autoload 'ids-edit "ids-edit" nil t)
;;
;; If you want to assign other key, you can do it to any keymap.
;; For example:
;;
;; : (global-set-key (kbd "M-u") 'ids-edit)
;;
;; ** IDS edit mode
;;
;; Minor `ids-edit-mode' will let you input IDCs and compose/decompose
;; ideographs by M-U key.
;;
;; | key | command       |
;; |-----+---------------|
;; | M-0 | "⿰" (U+2FF0) |
;; | M-1 | "⿱" (U+2FF1) |
;; | M-2 | "⿲" (U+2FF2) |
;; | M-3 | "⿳" (U+2FF3) |
;; | M-4 | "⿴" (U+2FF4) |
;; | M-5 | "⿵" (U+2FF5) |
;; | M-6 | "⿶" (U+2FF6) |
;; | M-7 | "⿷" (U+2FF7) |
;; | M-8 | "⿸" (U+2FF8) |
;; | M-9 | "⿹" (U+2FF9) |
;; | M-- | "⿺" (U+2FFa) |
;; | M-= | "⿻" (U+2FFb) |
;; | M-U | ids-edit      |
;;
;; You can customize the key bind by defining key to `ids-edit-mode-map'.
;;
;; ** Decomposing CJK Ideographs to IDS
;;
;; Just type `M-U' (meta-shift-u) after the position of target CJK
;; Ideograph. There should be no numerics, ideographs or IDCs at the
;; cursor.  With prefix argument given, it tries to decomposes previous
;; character unconditionally.
;;
;; ** Composing IDS to CJK Ideograph
;;
;; Type `M-U' on partial IDS. It may contain a (range of) strokes
;; number. When there are multiple candidates, they will all be
;; inserted to the current position with bracket. Attaching G,T,J,K at
;; the end will limit the candidates to specific charset (namely,
;; GB2312, Big5, JISX0213/0212, KSX1001).
;;
;; For example, "⿰氵20-22J", with cursor on "⿰" character, will be
;; composed to "[灊灝灞灣]".
;;
;; ** Data Sources
;;
;; Data 'ids.txt' and 'ucs-strokes.txt' are taken from
;; http://github.com/cjkvi/. License follows their terms.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (when (featurep 'ids-edit) (unload-feature 'ids-edit)))

(eval-and-compile
(defun ids-replace-cdp ()
  "Replace &CDP-XXXX; entity references to PUA characters."
  (goto-char (point-min))
  (while (re-search-forward
          "&CDP-\\(..\\)\\(..\\);" nil t)
    (let* ((x (string-to-number (match-string 1) 16))
           (y (string-to-number (match-string 2) 16))
           (x (+ (* (- x #x81) 157)
                 (if (< y 129) (- y 64) (- y 98))
                 #xeeb8)))
      (replace-match (char-to-string x)))))

(defvar ids-edit-table
  (eval-when-compile
    (require 'bytecomp)
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                             load-file-name
                                             buffer-file-name)))
           (ids-file (expand-file-name "ids-cdp.txt" directory))
           (table (make-hash-table)))
    (unless (file-exists-p ids-file) (error "Data file not found!"))
    (with-temp-buffer
      (insert-file-contents ids-file)
      (ids-replace-cdp)
      (goto-char (point-min))
      (while (re-search-forward "\\[.+?\\]" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^[UC].+?	\\(.\\)	\\(.+\\)" nil t)
        (let* ((char (string-to-char (match-string 1)))
               (ids  (split-string (match-string 2))))
          (puthash char ids table))))
    table))
  "IDS table.")
)

(eval-and-compile
(defvar ids-edit--equiv-chars
  ;;(eval-when-compile
    (let ((equivs '((?飠 ?𩙿) (?糸 ?糹) (?子 ?孑) (?戶 ?户 ?戸) (?示 ?礻) (?靑 ?青)
                    (?黑 ?黒) (?黃 ?黄) (?卥 ?𠧧)))
          (table (make-hash-table)))
      (dolist (pair equivs)
        (dolist (char pair)
          (puthash char pair table)))
      table)))

(defvar ids-edit--equiv-regexp
  (eval-when-compile
    (let (chars)
      (maphash (lambda (key _val) (setq chars (cons key chars)))
               ids-edit--equiv-chars)
      (concat "[" (apply 'string chars) "]"))))

(defvar ids-edit-component-table
  (eval-when-compile
    (let ((table (make-hash-table)))
      (maphash
       (lambda (char ids-list)
         (dolist (ids ids-list)
           (dolist (component (string-to-list ids))
             (when (or (< #x3000 component) (< component #x2f00))
               (cl-pushnew char (gethash component table))))))
       ids-edit-table)
      table))
  "IDS Component table.")

(defvar ids-edit-stroke-table
  (eval-when-compile
    (require 'bytecomp)
    (let* ((directory (file-name-directory (or byte-compile-current-file
                                               load-file-name
                                               buffer-file-name)))
           (strokes-file (expand-file-name "ucs-strokes.txt" directory))
           (table (make-hash-table)))
      (with-temp-buffer
        (insert-file-contents strokes-file)
        (ids-replace-cdp)
        (goto-char (point-min))
        (while (re-search-forward "	\\(.\\)	\\(.+\\)" nil t)
          (let ((char (string-to-char (match-string 1)))
                (strokes (split-string (match-string 2) ",")))
            (dolist (stroke strokes)
              (cl-pushnew (string-to-number stroke) (gethash char table)))))
        table))))

;; Patterns to input.
;; - at least one ideographs. (⺀-⻳㐀-鿿-﫿𠀀-𯿽)
;; - ⿰山30J
(defconst ids-edit-regexp
  "\\([⿰-⿻㇀-㇣⺀-⻳㐀-鿿-﫿𠀀-𯿽]+\\)?\\(?:\\([0-9]+\\)\\(-[0-9]+\\)?\\)?\\([㇀-㇣⺀-⻳㐀-鿿-﫿𠀀-𯿽]+\\)?\\([GJKT]\\)?"
  "Regular Expression for searching IDS.")

;;;###autoload
(define-minor-mode ids-edit-mode
  "minor-mode for editing ideographs by Ideographic Description Sequence (IDS)."
  :init-value nil
  :lighter "⿰"
  :keymap
  '(("\M-0" . "⿰") ("\M-1" . "⿱") ("\M-2" . "⿲") ("\M-3" . "⿳")
    ("\M-4" . "⿴") ("\M-5" . "⿵") ("\M-6" . "⿶") ("\M-7" . "⿷")
    ("\M-8" . "⿸") ("\M-9" . "⿹") ("\M--" . "⿺") ("\M-=" . "⿻")
    ("\M-U" . ids-edit)))

(defun ids-edit--turn-on ()
  "Explicitly turn on IDS edit mode."
  (unless (minibufferp) (ids-edit-mode +1)))

;;;###autoload
(define-global-minor-mode global-ids-edit-mode
  ids-edit-mode ids-edit--turn-on)

;;;###autoload
(defun ids-edit (arg)
  "Compose IDS after the point, or decompose previous ideograph.
Prefix argument ARG forces to decompose previous ideograph."
  (interactive "P")
  (if (and (null arg)
           (looking-at ids-edit-regexp)
           (or (match-string 1)
               (match-string 4))) (ids-edit--compose (match-data))
    (when (looking-back "[㇀-㇣⺀-⻳㐀-鿿-﫿𠀀-𯿽]" nil)
      (let ((ids-list (gethash (char-before (point)) ids-edit-table)))
        (if (null ids-list) (message "Can not decompose.")
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

(defun ids-edit--forward-char ()
  "If there is an IDS at the point in buffer, forward the point to next.
Return the IDS tree structure."
  (interactive)
  (cond ((looking-at "[⿰⿱⿴⿵⿶⿷⿸⿹⿺⿻]")
         (forward-char)
         (list (char-before (point))
               (ids-edit--forward-char) (ids-edit--forward-char)))
        ((looking-at "[⿲⿳]")
         (forward-char)
         (list (char-before (point))
               (ids-edit--forward-char) (ids-edit--forward-char) (ids-edit--forward-char)))
        ((eobp) (error "Incomplete IDS! %s" (buffer-string)))
        (t (forward-char) (char-before (point)))))

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
         (char (when (string-match "[㇀-㇣⺀-⻳㐀-鿿-﫿𠀀-𯿽]" (concat last first))
                 (string-to-char (match-string 0 (concat last first)))))
         (chars (or (gethash char ids-edit--equiv-chars) (list char)))
         (candidates-tmp
          (mapcar (lambda (char)
                    (copy-sequence
                     (gethash char ids-edit-component-table)))
                  chars))
         (candidates
          (sort
           (let* ((first (car candidates-tmp))
                  (rest (cdr candidates-tmp)))
             (dolist (each rest) (setq first (cl-nunion first each)))
             first)
           '<))
         max min regexp filtered)
    ;;(message "first=%s str=%s str2=%s last=%s flag=%s char=%s chars=%s
    ;;         candidates=%s" first strokes strokes2 last flag char
    ;;         (apply 'string chars)
    ;;         (apply 'string candidates))
    (if strokes (setq min (string-to-number strokes)))
    (setq max (if strokes2 (- (string-to-number strokes2)) min))
    (setq regexp (concat (ids-edit--expand-regexp first)
                         (if strokes "\\(.+\\)")
                         (ids-edit--expand-regexp last)))
    (setq filtered
          (cl-remove-if-not
           (lambda (char)
             (and (if flag
                    (cond ((equal flag "J")
                           (or (encode-char char 'japanese-jisx0213-1)
                               (encode-char char 'japanese-jisx0213-2)
                               (encode-char char 'japanese-jisx0212)))
                          ((equal flag "G")
                           (encode-char char 'chinese-gb2312))
                          ((equal flag "T")
                           (or (encode-char char 'chinese-big5-1)
                               (encode-char char 'chinese-big5-2)))
                          ((equal flag "K")
                           (encode-char char 'korean-ksc5601))) t)
                  (ids-edit--filter char regexp min max)))
           candidates))
    (set-match-data match-data)
    (setq filtered (cl-remove-duplicates filtered))
    (if (null filtered) (message "Not found!")
      (delete-region (match-beginning 0) (match-end 0))
      (insert (if (/= 1 (length filtered)) "[" "")
              (apply 'string filtered)
              (if (/= 1 (length filtered)) "]" "")))))

(defun ids-edit--expand-regexp (str)
  "Return regexp that match any of equivalent characters in STR.
e.g. \"飠糸\" → \"[飠𩙿][糸糹]\"."
  (when str
    (replace-regexp-in-string
     ids-edit--equiv-regexp
     (lambda (match)
       (concat "["
               (apply 'string
                      (gethash (string-to-char match) ids-edit--equiv-chars))
               "]"))
     str)))

(defun ids-edit--filter (char regexp min max)
  "T if any IDS of CHAR match REGEXP and strokes are between MIN and MAX.
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
