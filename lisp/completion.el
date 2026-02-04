;;; Company mode (completion) ------------------------------------------------
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))

;;; Which-key ---------------------------------------------------------------
(use-package which-key
  :config
  (which-key-mode))

;;; Consult/Easy-Kill (улучшенный kill-ring) ---------------------------------

(global-set-key (kbd "M-y") #'consult-yank-pop)

(setq select-enable-clipboard t)
(setq select-enable-primary t)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring))

(defun my/dont-kill-file-path ()
  (setq-local kill-ring
              (seq-remove
               (lambda (s)
                 (string-match-p "^/[^ ]+$" s))
               kill-ring)))

(add-hook 'minibuffer-exit-hook #'my/dont-kill-file-path)

;;; No Minibuffer kill-ring for kill-word ------------------------------------
(defun my-minibuffer-backward-kill-word ()
  "Delete word backward without affecting kill-ring."
  (interactive)
  (let ((kill-ring kill-ring))
    (backward-kill-word 1)))

(define-key minibuffer-local-map (kbd "C-<backspace>")
  #'my-minibuffer-backward-kill-word)

(define-key minibuffer-local-filename-completion-map (kbd "C-<backspace>")
  #'my-minibuffer-backward-kill-word)


(provide 'completion)
