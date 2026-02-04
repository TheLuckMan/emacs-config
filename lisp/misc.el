;;; Emacs Server (Удали если не хочешь с этим возится)------------------------
(unless (server-running-p)
  (server-start))

;;; Убиваю *scratch* файл Emacs --------------------------------
(setq inhibit-startup-screen t
      initial-scratch-message nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (get-buffer "*scratch*")
              (kill-buffer "*scratch*"))))

;;; Решение русской раскладки (reverse-im) ------------------------------------
(use-package reverse-im
  :config
  ;; стандартные qwerty ↔ ru раскладки
  (reverse-im-activate "russian-computer"))


(provide 'misc)
