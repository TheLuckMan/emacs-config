;;; Adwaita Theme ---------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(adwaita-dark))
 '(custom-safe-themes
   '("a68ec832444ed19b83703c829e60222c9cfad7186b7aea5fd794b79be54146e6"
     default))
 '(package-selected-packages
   '(adwaita-dark-theme all-the-icons all-the-icons-dired clang-format
			company consult dired-subtree diredfl
			easy-kill exec-path-from-shell flycheck forge
			grip-mode lsp-ui lua-mode magit
			markdown-preview-mode multiple-cursors
			powerline projectile reverse-im rust-mode
			vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground "#81A2BE" :weight bold))))
 '(all-the-icons-dired-file-face ((t (:foreground "#C5C8C6"))))
 '(line-number ((t (:inherit default :background "gray15" :foreground "gray25"))))
 '(mode-line ((t (:background "#303030" :foreground "#c5c8c6" :box nil :underline (:color "gray25" :style line :position 1) :slant normal :height 1.0)))))

(provide 'theme)
