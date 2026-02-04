;;; init.el --- TheLuckMan Config for GNU Emacs

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

(load custom-file 'noerror)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(require 'packages)
(require 'ui)
(require 'theme)
(require 'misc)
(require 'config-dired)
(require 'functions)
(require 'programming)
(require 'terminal)
(require 'completion)
(require 'git)
(require 'lsp)
(require 'keybinds)

;;; End ----------------------------------------------------------------------
(provide 'init)
;;; init.el ends here

