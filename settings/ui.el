;; User interface

;; Set default font
;; (set-default-font "Courier")
;;(set-default-font "Lucida Console")
;;(set-default-font "Courier")
;;(set-default-font "Consolas") ;; used in windows
;;;; Fira Code font: https://github.com/tonsky/FiraCode
;;(set-default-font "Fira Code") 


;; Disable tool- and menu- bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode t)

;; Highlight selection
;; (transient-mark-mode t)

;;(set-face-attribute 'default nil :font "Fira Code Retina" :height 125)
(set-face-attribute 'default nil :font "Fira Code Retina" :height 123)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers 'relative)
