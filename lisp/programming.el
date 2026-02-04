;;; Projectile ---------------------------------------------------------------
(use-package projectile
  :config
  (projectile-mode +1))

;;; Flycheck -----------------------------------------------------------------
(use-package flycheck
  :init (global-flycheck-mode))

;;; C-Mode defaults ------------------------------------------------------------
(setq c-default-style "linux"
      c-basic-offset 2)

(add-hook 'c-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 2)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 2)))

(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 2)))

;;;  Multi-line block comment -------------------------------------------------
(setq comment-style 'multi-line)


(provide 'programming)
