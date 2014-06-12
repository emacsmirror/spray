;; custom
(defvar spray-wpm 400 "words/min")
(defvar spray-text-scale 5)
(defvar spray-orp-face 'error)

;; internal variables for spraying
(defvar spray--padding-overlay nil)
(defvar spray--orp-overlay nil)
(defvar spray--timer nil)
(defvar spray--delay 0)

;; incompatible minor-modes
(defvar spray--saved-global-hl-line-mode nil)
(defvar spray--saved-font-lock-mode nil)
(defvar spray--saved-hl-line-mode nil)
(defvar spray--saved-cursor-type nil)

(define-minor-mode spray-mode
  "spray mode"
  :init nil
  :global nil
  (cond (spray-mode
         (setq spray--orp-overlay (make-overlay 0 0)
               spray--padding-overlay (make-overlay 0 0)
               spray--timer (run-with-timer 0 (/ 60.0 spray-wpm) 'spray-next)
               spray--saved-cursor-type cursor-type)
         (setq cursor-type nil)
         (text-scale-set spray-text-scale)
         (overlay-put spray--orp-overlay 'face spray-orp-face)
         (add-hook 'pre-command-hook 'turn-off-spray-mode)
         ;; disable incompatible minor-modes
         (when (boundp 'global-hl-line-mode)
           (setq spray--saved-global-hl-line-mode global-hl-line-mode)
           (set (make-local-variable 'global-hl-line-mode) nil))
         (when (boundp 'font-lock-mode)
           (setq spray--saved-font-lock-mode font-lock-mode)
           (font-lock-mode -1))
         (when (boundp 'hl-line-mode)
           (setq spray--saved-hl-line-mode hl-line-mode)
           (hl-line-mode -1)))
        (t
         (widen)
         (setq cursor-type spray--saved-cursor-type)
         (text-scale-set 0)
         (delete-overlay spray--orp-overlay)
         (delete-overlay spray--padding-overlay)
         (cancel-timer spray--timer)
         (remove-hook 'pre-command-hook 'turn-off-spray-mode)
         ;; restore incompatible minor-modes
         (when spray--saved-global-hl-line-mode
           (setq global-hl-line-mode spray--saved-global-hl-line-mode))
         (when spray--saved-font-lock-mode
           (font-lock-mode 1))
         (when spray--saved-hl-line-mode
           (hl-line-mode 1)))))

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
             (overlay-put spray--padding-overlay
                          'before-string (make-string (- 5 (- orp beg)) ?\s))
             (move-overlay spray--padding-overlay beg (1+ beg))
             (move-overlay spray--orp-overlay (1- orp) orp)
             (narrow-to-region beg end))))))

(provide 'spray)
