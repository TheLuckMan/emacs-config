;;; Dired --------------------------------------------------------------------
;; человеко-читаемые размеры
(require 'dired-x)
(setq dired-listing-switches "-Alh --group-directories-first")

;; подсветка
(add-hook 'dired-mode-hook 'diredfl-mode)

;; дерево
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(with-eval-after-load 'diredfl
  (set-face-attribute 'diredfl-dir-name nil :foreground "#81A2BE" :weight 'bold)          ;; каталоги
  (set-face-attribute 'diredfl-file-name nil :foreground "#C5C8C6")                       ;; обычные файлы
  (set-face-attribute 'diredfl-symlink nil :foreground "#B294BB")                         ;; ссылки
  (set-face-attribute 'diredfl-number nil :foreground "#F0C674")                          ;; числа / размер
  (set-face-attribute 'diredfl-date-time nil :foreground "#8ABEB7")                       ;; дата
  (set-face-attribute 'diredfl-flag-mark nil :background "#373B41" :foreground "#CC6666") ;; пометки
  ;; ------------------------------ Блок с правами -----------------------
  (dolist (face '(diredfl-read-priv
                  diredfl-write-priv
                  diredfl-exec-priv
                  diredfl-no-priv))
    (set-face-attribute face nil
                        :background nil))
  )


(setq dired-omit-files
      (concat dired-omit-files "\\|^#\\|~$\\|\\.bak$"))

(add-hook 'dired-mode-hook #'dired-omit-mode)


(provide 'config-dired)
