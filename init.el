;;; init.el --- TheLuckMan Config for GNU Emacs

;;; Package system ------------------------------------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun ensure-installed (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Essential packages
(mapc #'ensure-installed
      '(use-package
        lsp-mode
        lsp-ui
	rust-mode
	lua-mode
	markdown-mode
	all-the-icons
	markdown-preview-mode
	grip-mode
        company
        which-key
        projectile
        flycheck
        clang-format
	multiple-cursors
	reverse-im
	vterm
	consult
	easy-kill
	exec-path-from-shell
	magit
	forge
        ))

(require 'use-package)

;;; UI -----------------------------------------------------------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 110)

(column-number-mode 1)
(global-display-line-numbers-mode 1)

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

;;; Emacs Server (Удали если не хочешь с этим возится)------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

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


;;; Performance ---------------------------------------------------------------
(setq gc-cons-threshold (* 100 1000 1000))
(setq read-process-output-max (* 1024 1024))

;;; Which-key ---------------------------------------------------------------
(use-package which-key
  :config
  (which-key-mode))

;;; Dired --------------------------------------------------------------------
;; человеко-читаемые размеры
(setq dired-listing-switches "-Alh")

;; подсветка
(package-install 'diredfl)
(add-hook 'dired-mode-hook 'diredfl-mode)

;; дерево
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle))

(package-install 'all-the-icons)
(package-install 'all-the-icons-dired)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(with-eval-after-load 'diredfl
  (set-face-attribute 'diredfl-dir-name nil :foreground "#81A2BE" :weight 'bold)          ;; каталоги
  (set-face-attribute 'diredfl-file-name nil :foreground "#C5C8C6")                       ;; обычные файлы
  (set-face-attribute 'diredfl-symlink nil :foreground "#B294BB")                         ;; ссылки
  (set-face-attribute 'diredfl-number nil :foreground "#F0C674")                          ;; числа / размер
  (set-face-attribute 'diredfl-date-time nil :foreground "#8ABEB7")                       ;; дата
  (set-face-attribute 'diredfl-flag-mark nil :background "#373B41" :foreground "#CC6666") ;; пометки
)

 (with-eval-after-load 'diredfl
  (dolist (face '(diredfl-read-priv
                  diredfl-write-priv
                  diredfl-exec-priv
                  diredfl-no-priv))
    (set-face-attribute face nil
                        :background nil)))


(defun my-startup-dired ()
  "Open dired only if Emacs was started without file arguments."
  (when (and (= (length command-line-args) 1) ; только 'emacs'
             (not (daemonp)))
    (dired default-directory))
  (select-window (get-buffer-window "*dired*")))

;;; Killing *scratch* file when started Emacs --------------------------------
(setq inhibit-startup-screen t
      initial-scratch-message nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (get-buffer "*scratch*")
              (kill-buffer "*scratch*"))))


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

;;; Mode-line (Status-Bar) ---------------------------------------------------

(require 'all-the-icons)

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

;;; Решение русской раскладки (reverse-im) ------------------------------------
(use-package reverse-im
  :config
  ;; стандартные qwerty ↔ ru раскладки
  (reverse-im-activate "russian-computer"))

;;; Projectile ---------------------------------------------------------------
(use-package projectile
  :config
  (projectile-mode +1))


;;; exec-path-from-shell -----------------------------------------------------
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;;; VTerm - Эмулятор терминала -----------------------------------------------
;; (require 'vterm)
;; (setq vterm-shell (getenv "SHELL"))
;; (setq vterm-max-scrollback 10000)

;; (defun my/vterm-new ()
;;   (interactive)
;;   (vterm (generate-new-buffer-name "*vterm*")))

;; (defun my/vterm-here ()
;;   "Open vterm in project root or current directory."
;;   (interactive)
;;   (let ((default-directory
;;          (or (and (fboundp 'project-root)
;;                   (when-let ((proj (project-current)))
;;                     (project-root proj)))
;;              default-directory)))
;;     (my/vterm-new)))

;; (add-hook 'vterm-mode-hook
;;           (lambda ()
;;             (setq-local kill-buffer-hook
;;                         (list (lambda () (vterm-send-C-d))))))


;; (defun my/vterm-kill-buffer-when-exit (buffer _msg)
;;   (when (buffer-live-p buffer)
;;     (kill-buffer buffer)))

;; (add-hook 'vterm-exit-functions #'my/vterm-kill-buffer-when-exit)


;; (setq vterm-buffer-name-string "vterm: %s")
;; (setq vterm-kill-buffer-on-exit t)


(require 'vterm)

(setq vterm-shell (or (getenv "SHELL") "/bin/sh"))
(setq vterm-max-scrollback 10000)
(setq vterm-buffer-name-string "vterm: %s")
(setq vterm-kill-buffer-on-exit t)

;; (defun my/vterm-new ()
;;   (interactive)
;;   (vterm))

(defun my/vterm-new ()
  (interactive)
  (vterm (generate-new-buffer-name "*vterm*")))

(defun my/vterm-here ()
  "Open vterm in project root or current directory."
  (interactive)
  (let ((default-directory
         (or (and (fboundp 'project-root)
                  (when-let ((proj (project-current)))
                    (project-root proj)))
             default-directory)))
    (my/vterm-new)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook #'vterm-send-C-d nil t)))


;;; vterm cargo-run ----------------------------------------------------------

(defun my-cargo-run ()
  "Open vterm and run cargo run for current project."
  (interactive)
  (let* ((project (project-current))
         (root (project-root project))
         (buf (get-buffer-create "*cargo-run*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (vterm-send-string (format "cd %s" root))
      (vterm-send-return)
      (vterm-send-string "cargo run")
      (vterm-send-return))
    (pop-to-buffer buf)))

(defun my-cargo-run-workspace ()
  "Run cargo run -p <crate> in vterm."
  (interactive)
  (let* ((crate (read-string "Crate name: "))
         (project (project-current))
         (root (project-root project))
         (buf (get-buffer-create "*cargo-run*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode))
      (vterm-send-string (format "cd %s" root))
      (vterm-send-return)
      (vterm-send-string (format "cargo run -p %s" crate))
      (vterm-send-return))
    (pop-to-buffer buf)))


(defun my-cargo-run-kill-and-restart ()
  (interactive)
  (when-let ((buf (get-buffer "*cargo-run*")))
    (kill-buffer buf))
  (my-cargo-run))

;;; Consult/Easy-Kill (улучшенный kill-ring) ---------------------------------
(require 'consult)
(require 'easy-kill)

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



;;; Company mode (completion) ------------------------------------------------
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))

;;; Flycheck -----------------------------------------------------------------
(use-package flycheck
  :init (global-flycheck-mode))

;;; Magit/Forge (Github) -----------------------------------------------------------
(use-package magit)
(use-package forge
  :after magit)


;;; LSP (clangd) -------------------------------------------------------------
(use-package lsp-mode
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-idle-delay 0.2)
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--clang-tidy"
          "--completion-style=detailed")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-peek-enable t))

;;; Clang-format --------------------------------------------------------------
(global-set-key (kbd "C-c f") #'clang-format-buffer)

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

;;; Quality-of-life addons

(electric-pair-mode 1)
(delete-selection-mode 1)

;;;  Multi-line block comment -------------------------------------------------
(setq comment-style 'multi-line)

;;; Quality-of-life keybinds --------------------------------------------------
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




;;; End ----------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
