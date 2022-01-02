;; setxkbmap -option 'ctrl:nocaps'
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org
;; C-h v describe variable

;; Ripgrep and FZF are called through Counsel/Swiper/ivy
;; Rg (ripgrep)
;; To insert symboal at a point: M-j
;; https://oremacs.com/swiper/#key-bindings
;; C-c k
;; M-j

;; FZF
(setq auto-revert-mode t)
(setq-default fill-column 65)
(setq-default tab-width 4)

(setq use-dialog-box nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;;; http://ergoemacs.org/emacs/emacs_toggle-word-wrap.html
(setq tooggle-word-wrap t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq path-to-emacsd "~/.emacs.d/")
(defun get-full-path (subpath)
  (concat path-to-emacsd subpath))

(setq backup-directory-alist
      `((".*" . , "~/emacstemp/")))

(setq auto-save-file-name-transforms
      `((".*" , "~/emacstemp/" t)))

(add-to-list 'load-path (get-full-path "settings/"))
(add-to-list 'load-path (get-full-path "plugins/"))
(add-to-list 'load-path (get-full-path "plugins/swiper"))
(add-to-list 'load-path (get-full-path "plugins/yasnippet/"))

(require 'yasnippet)
(yas-global-mode 1)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

(load "ui.el")


(setq-default indent-tabs-mode nil)
;;(load-theme 'tango-dark)




;; (define-key swiper-map (kbd "C-*")
;;   (lambda ()
;;     (interactive)
;;     (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))



;; PACKAGE DIRECTORIES MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)





;;Custom key bindings
;; https://github.com/noctuid/evil-guide -> Key bindings in Emacs
;; https://github.com/noctuid/general.el
;; https://github.com/noctuid/evil-guide#keybindings-in-emacs
;;(use-package general
;; :config
;; (general-create-definer efs/leader-keys
;;   :keymaps '(normal insert visual emacs)
;;   :prefix "SPC"))
(use-package general
  :config
  (general-define-key
   :states 'normal
   ;;:keymaps 'override
   :prefix "SPC"
   "w" 'save-buffer
   "k" 'kill-buffer
   "l" 'bookmark-bmenu-list
   "t" 'toggle-window-split
   "." 'dired
   ;;"f" 'counsel-fzf
   "f" 'projectile-find-file
   "g" (lambda ()
         (interactive)
         (let ((current-prefix-arg 4))
           (call-interactively #'counsel-fzf))) ;; https://github.com/abo-abo/swiper/pull/1281
   "r" 'counsel-rg
   "o" 'other-window
   "5" 'my-run-make
   "," 'counsel-switch-buffer))

;; (general-create-definer my-leader-def
;;   :prefix "SPC")
;; (general-create-definer my-local-leader-def
;;   ;; :prefix my-local-leader
;;   :prefix "SPC m")

;; https://github.com/abo-abo/swiper
;; https://oremacs.com/swiper/
(use-package command-log-mode)





;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (define-key python-mode-map "\"" 'electric-pair)
;;             (define-key python-mode-map "\'" 'electric-pair)
;;             (define-key python-mode-map "(" 'electric-pair)
;;             (define-key python-mode-map "[" 'electric-pair)
;;             (define-key python-mode-map "{" 'electric-pair)))

;; Navigation: Swiper + FZF / Counsel
;; https://oremacs.com/swiper/#installing-from-the-git-repository
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Navigation
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)
;; (use-package swiper :ensure t)
;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)	
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))
;; 
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-ivy-deprecated-conf.el
;; https://www.youtube.com/watch?v=IDkx48JwDco
(use-package counsel
  :ensure t
  :after ivy 
  ;; :config
  ;; (defun prot/counsel-fzf-rg-files)
  ;; (defun prot/counsel-fzf-dir (arg))

  )
;;   :bind (("C-M-j" . 'counsel-switch-buffer)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :custom
;;   (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;;   :config
;;   (counsel-mode 1))



(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))




;;(use-package doom-themes :init (load-theme 'doom-palenight t))
;; (use-package doom-themes :init (load-theme 'doom-molokai t))
;; (use-package doom-themes :init (load-theme 'doom-1337 t))
;; (use-package doom-themes :init (load-theme 'doom-dracula t))
;; (use-package doom-themes :init (load-theme 'doom-gruvbox t))
;; (use-package doom-themes :init (load-theme 'doom-xcode t))
(use-package doom-themes :init (load-theme 'doom-horizon t))
;;(use-package doom-themes :init (load-theme 'doom-nord-light t))
;;(load-theme 'doom-gruvbox)




(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(project-local-variables projectile smartparens magit pyvenv rg company lsp-pyright ac-dabbrev auto-complete-config auto-complete lsp-python-ms python-mode dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters use-package swiper doom-modeline command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))




(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))




;; Autocomplete: Use "helpful" below?
;; (use-package auto-complete :init)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (auto-complete-mode t)
;; (global-auto-complete-mode t)


(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable) 
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))



;; Disable evil until C-z hit
(defun rune/evil-hook '()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;;:hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))




;; Python mode
;; https://emacs-lsp.github.io/lsp-mode/
(use-package python-mode)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)



;; https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
  (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\venv\\'"))
(setq lsp-file-watch-threshold 3000)

(use-package pyvenv
  :config (pyvenv-mode 1))




;;
;; My functions
;;
(defun my-run-python ()
  (interactive)
  (async-shell-command
   (format "python3 %s" buffer-file-name)))

(defun my-run-venv ()
  (interactive)
  (async-shell-command
   (format "source %s && python3 %s"
           (expand-file-name "venv/bin/activate")
           buffer-file-name)))

(defun my-run-poetry ()
  (interactive)
  (async-shell-command
   (format "source %s && python %s"
           "/home/al/.cache/pypoetry/virtualenvs/src-qt1p8_0V-py3.8/bin/activate"
           buffer-file-name)))




;; Compile using Makefile
;;
;; https://www.emacswiki.org/emacs/UsingMakefileFromParentDirectory
(defun get-above-makefile ()
  (let ((dir (locate-dominating-file "." "Makefile")))
    (when dir
      (concat dir "Makefile"))))

(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (compile (format "make -f %s" (get-above-makefile)))))

(defun my-run-make ()
  (interactive)
  (compile (format "make -f %s" (get-above-makefile))))

(defun my-run-make-old ()
  (interactive)
  (async-shell-command
   (format "cd %s && source %s && make"
           "/home/al/1/thesis_src/"
           "/home/al/.cache/pypoetry/virtualenvs/src-qt1p8_0V-py3.8/bin/activate"
           )))
  
;; End UsingMakefileFromParentDirectory

;; /home/al/.cache/pypoetry/virtualenvs/src-qt1p8_0V-py3.8

(defun window-split-toggle ()
  ;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;;
;; End my functions
;;

;; (global-set-key (kbd "<SPC-5>") 'my-run-poetry)
;; (map! :leader "3" #'my-run-python)
;; (map! :leader "5" #'my-run-poetry)
;; (map! :leader "4" #'my-run-env)
(setq async-shell-command-display-buffer nil)



(add-to-list 'load-path (get-full-path "plugins/neotree"))
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



(require 'smartparens-config)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'scala-mode-hook #'smartparens-mode)
(add-hook 'elisp-mode-hook #'smartparens-mode)


(load "scala.el")



(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))




(defun toggle-window-split ()
  ;; https://www.emacswiki.org/emacs/ToggleWindowSplit
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

