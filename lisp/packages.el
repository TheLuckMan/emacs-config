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

(require 'server)
(require 'all-the-icons)
(require 'exec-path-from-shell)
(require 'vterm)
(require 'consult)
(require 'easy-kill)

(package-install 'diredfl)
(package-install 'all-the-icons)
(package-install 'all-the-icons-dired)

(provide 'packages)
