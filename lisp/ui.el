;;; UI -----------------------------------------------------------------------
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 110)

(column-number-mode 1)
(global-display-line-numbers-mode 1)

;;; Mode-line (Status-Bar) ---------------------------------------------------
(set-face-attribute 'mode-line nil
                    :background "#303030"
		    :foreground "#c5c8c6"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#242424"
                    :foreground "#707880"
                    :box nil)

(set-face-attribute 'mode-line nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :underline nil)

(setq all-the-icons-scale-factor 0.9)

(set-face-attribute 'mode-line nil
                    :height 1.0
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :height 1.0
                    :box nil)

(defun my-modeline-icon ()
  (let ((all-the-icons-scale-factor 0.85))
    (all-the-icons-icon-for-buffer)))


(setq-default
 mode-line-format
 '("%e"
   mode-line-modified
   " "
   (:eval (my-modeline-icon))
   " "
   mode-name
   " "
   mode-line-buffer-identification
   " "
   ;; "%l:%c"
   (:eval
    (propertize " "
                'display
                `(space :align-to (- right-fringe 20))))
   " "
   mode-line-position
   "  "))

;;; Frame title --------------------------------------------------------------
(setq frame-title-format "%b")

;;; Quality-of-life addons
(electric-pair-mode 1)
(delete-selection-mode 1)

(provide 'ui)
