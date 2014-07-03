;;; spray.el --- a speed reading mode

;; Copyright (C) 2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Author: Ian Kelling <ian@iankelling.org>
;; Version: 0.0.2

;;; Commentary:

;; Put this script into a "load-path"ed directory, and load it in your
;; init file.
;;
;;   (require 'spray)
;;
;; Then you may run spray with "M-x spray-mode". Binding some keys may
;; also be useful.
;;
;;   (global-set-key (kbd "<f6>") 'spray-mode)
;;
;; For more informations, see Readme.org.

;; Known bugs.
;; repeated words are indistinguishable, for example
;; "going, going, gone" reads like going, gone, with a slight delay.

;;; Change Log:
;; 0.0.0 test release
;; 0.0.1 add spray-set-margins
;; 0.0.2 margin options, speed control, better quit

;;; Code:

(require 'face-remap)

;; * customizable vars

(defvar spray-wpm 400 "words/min")
(defvar spray-height 400 "height of characters")
(defvar spray-margin-top 1 "character margin at top of buffer. Characters are as big as spray text characters.")
(defvar spray-margin-left 1 "character margin at left of buffer. Characters are as big as spray text characters.")

(defvar spray-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "SPC") 'spray-start/stop)
    (define-key km (kbd "h") 'spray-backward-word)
    (define-key km (kbd "l") 'spray-forward-word)
    (define-key km (kbd "<left>") 'spray-backward-word)
    (define-key km (kbd "<right>") 'spray-forward-word)
    (define-key km (kbd "f") 'spray-faster)
    (define-key km (kbd "s") 'spray-slower)
    (define-key km (kbd "q") 'spray-quit)
    (define-key km (kbd "<return>") 'spray-quit)
    km)
  "keymap for spray-mode buffers")

;; * faces

(make-face 'spray-base-face)
(set-face-attribute 'spray-base-face nil
                    :background (face-background 'default)
                    :foreground (face-foreground 'default)
                    :slant 'normal)

(make-face 'spray-accent-face)
(set-face-attribute 'spray-accent-face nil
                    :foreground "red"
                    :overline (face-foreground 'default)
                    :underline (face-foreground 'default)
                    :slant 'normal)

;; * internal vars

(defvar spray--margin-string ""
  "Currently not used.")
(defvar spray--base-overlay nil)
(defvar spray--accent-overlay nil)
(defvar spray--running nil)
(defvar spray--delay 0)
(defvar spray--saved-cursor-type nil)
(defvar spray--saved-buffer-face nil)
(defvar spray--saved-restriction nil)
(defvar spray--saved-smartparens-enabled nil)

;; * utility functions

(defun spray-set-margins ()
  "Setup spray--margin-string"
  (setq spray--margin-string
        (concat (make-string spray-margin-top 10) ;; 10 = ascii newline
                (make-string spray-margin-left 32)))) ;; 32 = ascii space

;; * the mode

;;;###autoload
(define-minor-mode spray-mode
  "spray mode"
  :init nil
  :keymap spray-mode-map
  (cond (spray-mode
         (setq spray--base-overlay (make-overlay (point-min) (point-max))
               spray--accent-overlay (make-overlay 0 0)
               spray--saved-cursor-type cursor-type
               spray--saved-restriction (and (buffer-narrowed-p)
                                             (cons (point-min) (point-max)))
               spray--saved-buffer-face (and (boundp 'buffer-face-mode)
                                             buffer-face-mode
                                             buffer-face-mode-face)
               spray--saved-smartparens-enabled (and (boundp 'smartparens-mode)
                                                     smartparens-mode)
               spray--saved-highlight-symbol-enabled (and (boundp 'highlight-symbol-mode)
                                                     highlight-symbol-mode))
         ;; smartparens wrapping of all letter binds can cause problems.
         ;; for example, it can cause auto-complete to activate
         (and spray--saved-smartparens-enabled (smartparens-mode -1))
         (and spray--saved-highlight-symbol-enabled (highlight-symbol-mode -1))
         (setq cursor-type nil)
         (let ((buffer-face-mode-face `(:height ,spray-height)))
           (buffer-face-mode 1))
         (overlay-put spray--base-overlay 'priority 100)
         (overlay-put spray--base-overlay 'face 'spray-base-face)
         (overlay-put spray--accent-overlay 'priority 101)
         (overlay-put spray--accent-overlay 'face 'spray-accent-face)
         (spray-start))
        (t
         (and spray--saved-smartparens-enabled (smartparens-mode 1))
         (and spray--saved-highlight-symbol-enabled (highlight-symbol-mode 1))
         (setq cursor-type spray--saved-cursor-type)
         (if spray--saved-restriction
             (narrow-to-region (car spray--saved-restriction)
                               (cdr spray--saved-restriction))
           (widen))
         (buffer-face-mode -1)
         (if spray--saved-buffer-face
             (let ((buffer-face-mode-face spray--saved-buffer-face))
               (buffer-face-mode 1)))
         (delete-overlay spray--base-overlay)
         (delete-overlay spray--accent-overlay)
         (spray-stop))))

(defun spray-quit ()
  "Exit spray mode."
  (interactive)
  (spray-mode -1))

(defun spray--word-at-point ()
  (skip-chars-backward "^\s\t\n—")
  (let* ((beg (point))
         (len (+ (skip-chars-forward "^\s\t\n—") (skip-chars-forward "—")))
         (end (point))
         (accent (+ beg (cl-case len
                       ((1) 1)
                       ((2 3 4 5) 2)
                       ((6 7 8 9) 3)
                       ((10 11 12 13) 4)
                       (t 5)))))
    ;; this fairly obfuscated, using magic numbers to store state
    ;; it would be nice to sometime patch this so it is more readable.
    ;; for greater than 9 length, we display for twice as long
    ;; for some punctuation, we display a blank
    (setq spray--delay (+ (if (> len 9) 1 0)
                          (if (looking-at "\n[\s\t\n]") 3 0)
                          (cl-case (char-before)
                            ((?. ?! ?\? ?\;) 3)
                            ((?, ?: ?—) 1)
                            (t 0))))
    (move-overlay spray--accent-overlay (1- accent) accent)
    (move-overlay spray--base-overlay beg end)
    (spray-set-margins)
    (overlay-put spray--base-overlay 'before-string
                 (concat spray--margin-string
                         (make-string (- 5 (- accent beg)) ?\s)))
    (narrow-to-region beg end)))

(defun spray--update ()
  (cond ((not (zerop spray--delay))
         (setq spray--delay (1- spray--delay))
         (when (= spray--delay 2)
           (narrow-to-region (point) (point))))
        (t
         (widen)
         (if (eobp)
             (spray-mode -1)
           (skip-chars-forward "\s\t\n—")
           (spray--word-at-point)))))

;; * interactive commands

(defun spray-start/stop ()
  "Toggle pause/unpause spray."
  (interactive)
  (or (spray-stop) (spray-start)))

(defun spray-stop ()
  "Pause spray.
Returns t if spray was unpaused."
  (interactive)
  (prog1 spray--running
    (when spray--running
      (cancel-timer spray--running)
      (setq spray--running nil))))

(defun spray-start ()
  "Start / resume spray."
  (interactive)
  (setq spray--running
        (run-with-timer 0 (/ 60.0 spray-wpm) 'spray--update)))


(defun spray-forward-word ()
  (interactive)
  (spray-stop)
  (widen)
  (skip-chars-forward "\s\t\n—")
  (spray--word-at-point))

(defun spray-backward-word ()
  (interactive)
  (spray-stop)
  (widen)
  (skip-chars-backward "^\s\t\n—")
  (skip-chars-backward "\s\t\n—")
  (spray--word-at-point))

(defun spray-faster ()
  "Increases speed.

Increases the wpm (words per minute) parameter. See the variable
`spray-wmp'."
  (interactive)
  (spray-inc-wpm 20))

(defun spray-slower ()
  "Decreases speed.

Decreases the wpm (words per minute) parameter. See the variable
`spray-wmp'."
  (interactive)
  (spray-inc-wpm -20))

(defun spray-inc-wpm (delta)
  (let ((was-running spray--running))
    (spray-stop)
    (when (< 10 (+ spray-wpm delta))
      (setq spray-wpm (+ spray-wpm delta)))
    (and was-running (spray-backward-word))
    (message "spray wpm: %d" spray-wpm)
    (when was-running
      (spray-start))))

;; * provide

(provide 'spray)

;;; spray.el ends here
