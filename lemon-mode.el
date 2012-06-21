;;; lemon-mode.el --- Major mode for editing lemon grammar files

;; Copyright (C) 2012  mooz

;; Author: mooz <stillpedant@gmail.com>
;; Keywords: lemon

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing LEMON Parser Generator grammar files
;; .
;; LEMON Parser Generator <http://www.hwaci.com/sw/lemon/>
;;

;;; Code:

(require 'font-lock)
(require 'cc-mode)

;; Misc variables

(defvar lemon-base-mode)

;; Syntax highlight support

(defvar lemon-syntax-capture "([A-Z][A-Z0-9]*)")

(defvar lemon-syntax-terminal-symbol
  (concat "\\([A-Z][a-zA-Z_]*\\)[ \t]*\\(" lemon-syntax-capture "\\)?"))

(defvar lemon-syntax-non-terminal-symbol
  (concat "\\([a-z][a-z0-9_]*\\)[ \t]*\\(" lemon-syntax-capture "\\)?"))

(defvar lemon-syntax-lhs-symbol-value "\\$\\$")

(defvar lemon-syntax-rhs-symbol-value "\\$[1-9][0-9]*")

(defvar lemon-syntax-directive "%[a-zA-Z_0-9]+")

(defvar lemon-syntax-rule-lhs
  (concat "[ \t]*" lemon-syntax-non-terminal-symbol "[ \t]*"))

(defvar lemon-syntax-rule-rhs
  (concat "\\(|\\|" lemon-syntax-terminal-symbol "\\|" lemon-syntax-non-terminal-symbol "\\|[ \t]\\)*[ \t]*\\."))

(defvar lemon-syntax-rule
  (concat lemon-syntax-rule-lhs "::=[ \t]*" lemon-syntax-rule-rhs))

(defvar lemon-font-lock-keywords
      `((,lemon-syntax-directive . font-lock-function-name-face)  ; special directive
        (,lemon-syntax-lhs-symbol-value . font-lock-constant-face)      ; $$
        (,lemon-syntax-rhs-symbol-value . font-lock-variable-name-face) ; $1, $2, ...
        (,(concat lemon-syntax-rule-lhs "::=") (1 font-lock-variable-name-face t))))

;; Indentation support

(defun lemon-inside-block-p ()
  "Returns true if `point' is placed between '{' and '}'"
  (save-excursion
    (let (parse-sexp-ignore-comments)
      (and (re-search-backward "\\({\\|}\\)" (point-min) t)
           (or (looking-at "{")
               (progn (goto-char (scan-lists (1+ (point)) -1 0))
                      (lemon-inside-block-p)))))))

(defun lemon-beginning-of-block ()
  (when (lemon-inside-block-p)
    (and (re-search-backward "\\(\\.\\|%[a-z_]+\\)[ \t]*{" (point-min) t)
         (re-search-forward "{" (point-max) t)
         (backward-char))))

(defun lemon-indent-line (&optional syntax quiet ignore-point-pos)
  (cond
   ((lemon-inside-block-p)
    ;; Use C-style indentation
      (save-restriction
        (save-excursion
          (narrow-to-region (progn (lemon-beginning-of-block) (point))
                            (progn (forward-list) (point))))
        (let ((major-mode lemon-base-mode))
          (c-indent-line syntax quiet ignore-point-pos))))
   (t
    ;; Otherwise, use lemon-style indentation
    ;; (FIXME: Currently, indentation is fixed to 0)
    (indent-line-to 0))))

;; Arrange keymap

(defvar lemon-mode-map (make-sparse-keymap))

(defmacro lemon-define-derived-mode (base-mode base-mode-name)
  (let ((derived-mode
         (intern (concat "lemon-" (symbol-name base-mode))))
        (derived-mode-name
         (concat "Lemon/" base-mode-name)))
    `(define-derived-mode ,derived-mode ,base-mode
       ,derived-mode-name
       "Major mode for editing lemon grammar files"
       (setq mode-name ,derived-mode-name)
       (setq major-mode (quote ,derived-mode))
       ;; Indentation
       (make-local-variable 'indent-line-function)
       (setq indent-line-function 'lemon-indent-line)
       (make-local-variable 'indent-region-function)
       (setq indent-region-function nil)
       ;; Record base mode (for correct indentation)
       (make-local-variable 'lemon-derived-mode)
       (setq lemon-base-mode (quote ,base-mode))
       ;; Keymap
       (use-local-map lemon-mode-map)
       ;; Syntax highlight
       (font-lock-add-keywords nil lemon-font-lock-keywords))))

(lemon-define-derived-mode c++-mode "C++")
(lemon-define-derived-mode c-mode "C")

(defalias 'lemon-mode 'lemon-c-mode)

(provide 'lemon-mode)
;;; lemon-mode.el ends here
