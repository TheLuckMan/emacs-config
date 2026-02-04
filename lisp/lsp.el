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


(provide 'lsp)
