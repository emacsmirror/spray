(require 'face-remap)

;; custom
(defvar spray-wpm 400 "words/min")
(defvar spray-height 400 "height of characters")
(defvar spray-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "SPC") 'spray-start/stop)
    (define-key km (kbd "h") 'spray-backward-word)
    (define-key km (kbd "<left>") 'spray-backward-word)
    (define-key km (kbd "l") 'spray-forward-word)
    (define-key km (kbd "<right>") 'spray-forward-word)
    km))

(make-face 'spray-base-face)
(set-face-attribute 'spray-base-face nil
                    :background (face-background 'default)
                    :foreground (face-foreground 'default))

(make-face 'spray-orp-face)
(set-face-attribute 'spray-orp-face nil
                    :foreground "red"
                    :overline (face-foreground 'default)
                    :underline (face-foreground 'default))

;; internal variables
(defvar spray--base-overlay nil)
(defvar spray--orp-overlay nil)
(defvar spray--running nil)
(defvar spray--delay 0)
(defvar spray--saved-cursor-type nil)
(defvar spray--saved-buffer-face nil)
(defvar spray--saved-restriction nil)

(define-minor-mode spray-mode
  "spray mode"
  :init nil
  :keymap spray-mode-map
  (cond (spray-mode
         (setq spray--base-overlay (make-overlay (point-min) (point-max))
               spray--orp-overlay (make-overlay 0 0)
               spray--saved-cursor-type cursor-type
               spray--saved-restriction (and (buffer-narrowed-p)
                                             (cons (point-min) (point-max)))
               spray--saved-buffer-face (and (boundp 'buffer-face-mode)
                                             buffer-face-mode
                                             buffer-face-mode-face))
         (setq cursor-type nil)
         (let ((buffer-face-mode-face `(:height ,spray-height)))
           (buffer-face-mode 1))
         (overlay-put spray--base-overlay 'priority 100)
         (overlay-put spray--base-overlay 'face 'spray-base-face)
         (overlay-put spray--orp-overlay 'priority 101)
         (overlay-put spray--orp-overlay 'face 'spray-orp-face)
         (add-hook 'pre-command-hook 'spray--pre-command-handler)
         (spray-start/stop 1))
        (t
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
         (delete-overlay spray--orp-overlay)
         (remove-hook 'pre-command-hook 'spray--pre-command-handler)
         (spray-start/stop -1))))

(defun spray--pre-command-handler ()
  (unless (memq this-command '(spray-forward-word
                               spray-backward-word spray-start/stop))
    (spray-mode -1)))

(defun spray--word-at-point ()
  (skip-chars-backward "^\s\t\n")
  (let* ((beg (point))
         (len (skip-chars-forward "^\s\t\n"))
         (end (point))
         (orp (+ beg (cl-case len
                       ((1) 1)
                       ((2 3 4 5) 2)
                       ((6 7 8 9) 3)
                       ((10 11 12 13) 4)
                       (t 5)))))
    (setq spray--delay (+ (if (> len 9) 1 0)
                          (if (looking-at "\n[\s\t\n]") 3 0)
                          (cl-case (char-before)
                            ((?. ?! ?\? ?\;) 3)
                            ((?, ?:) 1)
                            (t 0))))
    (move-overlay spray--orp-overlay (1- orp) orp)
    (move-overlay spray--base-overlay beg end)
    (overlay-put spray--base-overlay
                 'before-string (make-string (- 5 (- orp beg)) ?\s))
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
           (skip-chars-forward "\s\t\n")
           (spray--word-at-point)))))

;; commands

(defun spray-start/stop (&optional switch)
  (interactive)
  (cond ((and (memql switch '(nil 1))
              (not spray--running))
         (setq spray--running
               (run-with-timer 0 (/ 60.0 spray-wpm) 'spray--update)))
        ((memql switch '(nil -1))
         (cancel-timer spray--running)
         (setq spray--running nil))
        (t
         nil)))

(defun spray-forward-word ()
  (interactive)
  (when spray--running (spray-start/stop -1))
  (widen)
  (skip-chars-forward "\s\t\n")
  (spray--word-at-point))

(defun spray-backward-word ()
  (interactive)
  (when spray--running (spray-start/stop -1))
  (widen)
  (skip-chars-backward "^\s\t\n")
  (skip-chars-backward "\s\t\n")
  (spray--word-at-point))

(provide 'spray)
