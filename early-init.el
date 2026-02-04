;;; Performance ---------------------------------------------------------------
;;(setq gc-cons-threshold (* 100 1000 1000))
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(setq read-process-output-max (* 1024 1024))

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
