;; custom
(defvar spray-wpm 350 "words/min")
(defvar spray-height 500 "height of characters")

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
(defvar spray--timer nil)
(defvar spray--delay 0)
(defvar spray--saved-cursor-type nil)

(define-minor-mode spray-mode
  "spray mode"
  :init nil
  :global nil
  (cond (spray-mode
         (let ((buffer-face-mode-face `(:height ,spray-height)))
           (buffer-face-mode 1))
         (setq spray--base-overlay (make-overlay (point-min) (point-max))
               spray--orp-overlay (make-overlay 0 0)
               spray--timer (run-with-timer 0 (/ 60.0 spray-wpm) 'spray-next)
               spray--saved-cursor-type cursor-type)
         (setq cursor-type nil)
         (overlay-put spray--base-overlay 'priority 100)
         (overlay-put spray--base-overlay 'face 'spray-base-face)
         (overlay-put spray--orp-overlay 'priority 101)
         (overlay-put spray--orp-overlay 'face 'spray-orp-face)
         (add-hook 'pre-command-hook 'turn-off-spray-mode))
        (t
         (buffer-face-mode -1)
         (widen)
         (setq cursor-type spray--saved-cursor-type)
         (delete-overlay spray--base-overlay)
         (delete-overlay spray--orp-overlay)
         (cancel-timer spray--timer)
         (remove-hook 'pre-command-hook 'turn-off-spray-mode))))

(defun turn-on-spray-mode () (interactive) (spray-mode 1))
(defun turn-off-spray-mode () (interactive) (spray-mode -1))

(defun spray-next ()
  (cond ((not (zerop spray--delay))
         (setq spray--delay (1- spray--delay))
         (when (and (<= spray--delay 2)
                    (= (char-before) ?.))
           (narrow-to-region (point) (point))))
        (t
         (widen)
         (if (eobp)
             (turn-off-spray-mode)
           (skip-chars-forward "\s\t\n")
           (let* ((beg (point))
                  (len (skip-chars-forward "^\s\t\n"))
                  (end (point))
                  (orp (+ beg (cl-case len
                                ((1) 1)
                                ((2 3 4 5) 2)
                                ((6 7 8 9) 3)
                                ((10 11 12 13) 4)
                                (t 5)))))
             (setq spray--delay (+ (if (> len 8) 1 0) (cl-case (char-before)
                                                        ((?. ?! ?\? ?\;) 3)
                                                        ((?, ?:) 1)
                                                        (t 0))))
             (move-overlay spray--orp-overlay (1- orp) orp)
             (move-overlay spray--base-overlay beg end)
             (overlay-put spray--base-overlay
                          'before-string (make-string (- 5 (- orp beg)) ?\s))
             (narrow-to-region beg end))))))

(provide 'spray)
