;;; Custom Functions ---------------------------------------------------------
(defun my--change-number-at-point (delta)
  "Change number at point by DELTA.
Supports decimal, hex (0x...), and binary (0b...)."
  (interactive)
  (skip-chars-backward "0-9a-fA-FxXbB")
  (unless (looking-at
           "\\(0[bB]\\([01]+\\)\\|0[xX]\\([0-9a-fA-F]+\\)\\|\\([0-9]+\\)\\)")
    (error "No number at point"))
  (let* ((full (match-string 0))
         (base (cond
                ((match-string 2) 2)
                ((match-string 3) 16)
                (t 10)))
         (digits (or (match-string 2)
                     (match-string 3)
                     (match-string 4)))
         (num (string-to-number digits base))
         (new (+ num delta))
         (formatted
          (cond
           ((= base 2)
            (concat "0b" (format "%b" new)))
           ((= base 16)
            (concat "0x" (format "%x" new)))
           (t
            (number-to-string new)))))
    (replace-match formatted)))

(defun increment-number-at-point ()
  (interactive)
  (my--change-number-at-point 1))

(defun decrement-number-at-point ()
  (interactive)
  (my--change-number-at-point -1))

(provide 'functions)
