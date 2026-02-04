;;; Keybinding    ------------------------------------------------------------
(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(windmove-default-keybindings)

(global-set-key (kbd "C-c <left>")  #'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c <down>")  #'shrink-window)
(global-set-key (kbd "C-c <up>")    #'enlarge-window)

(global-set-key (kbd "C-;")   'comment-dwim)
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c =") 'decrement-number-at-point)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "<mouse-8>") 'previous-buffer)
(global-set-key (kbd "<mouse-9>") 'next-buffer)
(global-set-key (kbd "C-c ,") 'previous-buffer)
(global-set-key (kbd "C-c .") 'next-buffer)


;; добавить курсоры по совпадениям
(global-set-key (kbd "C-c m n") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") #'mc/mark-all-like-this)

;; курсор на каждую строку региона
(global-set-key (kbd "C-c m l") #'mc/edit-lines)

;; отменить всё
(global-set-key (kbd "C-c m q") #'mc/keyboard-quit)

;; Easy-kill
(global-set-key (kbd "M-w") #'easy-kill)
(global-set-key (kbd "C-M-w") #'easy-mark)

;; Cargo
(global-set-key (kbd "C-c r") #'my-cargo-run)
(global-set-key (kbd "C-c R") #'my-cargo-run-workspace)

;; Vterm
(global-set-key (kbd "C-c t") #'vterm)

;; Применение .emacs конфига
(global-set-key (kbd "C-c c") #'(lambda ()
                                  (interactive)
                                  (load-file user-init-file)
                                  (message "init.el reloaded")))


;; vterm
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-q") #'vterm-send-C-q)
  (define-key vterm-mode-map (kbd "C-c C-c") #'vterm-send-C-c))
(define-key vterm-mode-map (kbd "C-y") #'vterm-yank)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)



(provide 'keybinds)
