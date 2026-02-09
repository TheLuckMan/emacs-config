;;; exec-path-from-shell -----------------------------------------------------
(require 'vterm)
(require 'project)
(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)



(setq vterm-shell (or (getenv "SHELL") "/bin/sh"))
(setq vterm-max-scrollback 10000)
(setq vterm-buffer-name-string "vterm: %s")
(setq vterm-kill-buffer-on-exit t)

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


(provide 'terminal)
