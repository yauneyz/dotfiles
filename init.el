
;;; Code:

(defun install-if-necessary (package)
  "Install PACKAGE unless it is already installed."
      (unless (package-installed-p package)
	(package-install package)))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; =========== UI Cleanup  ===================
(setq inhibit-startup-message t
      visible-bell t)
(global-display-line-numbers-mode)

;; Command Log
(use-package command-log-mode
 :config
 (global-command-log-mode))

;; Don't show line numbers on some modes like terminal, shell, org
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; All the icons
(use-package all-the-icons)

;;(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;;(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; =========== Packages ===================

;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-undo-system 'undo-redo))


;; Evil-Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Leader Key
(evil-set-leader 'normal (kbd "SPC"))

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; cider
(unless (package-installed-p 'cider)
  (package-install 'cider))

;; paredit
(use-package paredit)
  ;; :config
  ;; (show-paren-mode t)
  ;;  :diminish)

;; Enable paredit mode for Clojure buffers, CIDER mode and CIDER REPL buffers
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
;; Disable paredt in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 0)))


;; NERD Commenter
(install-if-necessary 'evil-nerd-commenter)
;; (evilnc-default-hotkeys)
(evil-define-key '(normal visual) 'global (kbd ",cc") 'evilnc-comment-or-uncomment-lines)
(evil-define-key '(normal visual) 'global (kbd ",cu") 'evilnc-comment-or-uncomment-lines)


;; Helpful
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))


;; Flycheck
(use-package flycheck
	     :ensure t
	     :config
	     (add-hook 'after-init-hook #'global-flycheck-mode))

;; Bind counsel-M-x to C-t C-t
(global-set-key (kbd "C-a") 'counsel-M-x)

;; Counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; Swiper
(use-package swiper)

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ;("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil) ; Don't start searches with ^
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
  (ivy-mode 1))

;; Ivy Rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy))
  :init
  (setq projectile-project-search-path '("~/development/"))
  :config (projectile-mode)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories ".clj-kondo")
  (setq projectile-mode-line "Projectile"))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories ".clj-kondo"))





;; Counsel Projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit)
 ;;  :custom
 ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;(use-package treemacs-magit
  ;:after (treemacs magit)
  ;:ensure t)


;; Command Log
(use-package command-log-mode)

;; Company Mode - configured to use LSP
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("C-l" . company-complete-selection))
  (:map lsp-mode-map
	("C-l" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (define-key company-active-map (kbd "TAB") nil))

;; Company Box
(use-package company-box
  :hook (company-mode . company-box-mode))

;; =========== Language Specific  ===================(

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (evil-define-key 'normal 'global (kbd "<leader>l") lsp-command-map))

;;; Jedi
;(use-package lsp-jedi
  ;:ensure t)

;; LSP UI
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom))

;; LSP Ivy
(use-package lsp-ivy)

;; LSP Treemacs
(use-package lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1))

;; Header breadcrumbs
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


;; Clojure
(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook (clojure-mode . lsp-deferred)
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Cider
; Use ivy for completion
(use-package cider
  :config
  (setq cider-completion-system 'ivy)
  (setq cider-repl-display-help-banner nil))

;; After every save, run cider-format-buffer if in clojure-mode
(add-hook 'before-save-hook
	  (lambda ()
	    (when (eq major-mode 'clojure-mode)
	      (cider-format-buffer))))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred))



;; =========== Config  ===================

;; History saving
(setq history-length 25)
(savehist-mode 1)

;; Place Saving
(save-place-mode 1)

;; =========== Theme  ===================
;; (load-theme 'wombat)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t))


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; =========== Copilot  ===================
;(install-if-necessary 'editorconfig)
;(install-if-necessary 'jsonrpc)
(use-package editorconfig)
(use-package jsonrpc)

(use-package copilot
	:quelpa (copilot :fetcher github
									 :repo "copilot-emacs/copilot.el"
									 :branch "main"
									 :files ("dist" "*.el")))
(add-hook 'prog-mode-hook 'copilot-mode)
(evil-define-key 'insert 'global (kbd "<tab>") 'copilot-accept-completion)
;; Make sure TAB isn't bound to anything else
(define-key evil-insert-state-map (kbd "TAB") nil)
(evil-define-key 'insert 'global (kbd "TAB") 'copilot-accept-completion)
(copilot-mode)
(setq copilot-indent-offset-warning-disable t)

;; =========== Font  ===================
(set-face-attribute 'default nil :font "Fira Code Retina" :height 180)


;; =========== Keybindings  ===================
;; In elisp-mode, C-c C-c will evaluate the buffer
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-c") 'eval-buffer)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Window selection like Vim
(evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)
(evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)


(global-set-key (kbd "C-M-h") 'help-command)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "<leader>SPC") 'projectile-find-file)
(global-set-key (kbd "<leader>g") 'counsel-projectile-rg)

;; paredit commands
;; (evil-define-key 'normal 'global (kbd "C-f") 'paredit-forward)
;; (evil-define-key 'normal 'global (kbd "C-b") 'paredit-backward)
;; (evil-define-key 'normal 'global (kbd "C-d") 'paredit-forward-down)
;; (evil-define-key 'normal 'global (kbd "C-u") 'paredit-backward-up)
;; (evil-define-key 'normal 'global (kbd "C-n") 'paredit-forward-up)
;; (evil-define-key 'normal 'global (kbd "C-p") 'paredit-backward-down)
; Slurp and barf on parentheses and brackets


;; In evil normal mode, gd will go to definition
(evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gl") 'lsp-find-references)

;; Turn off read-only mode
(read-only-mode 0)

(setq lsp-pylsp-plugins-autopep8-enabled t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(exec-path-from-shell winum which-key rainbow-delimiters quelpa-use-package python-mode paredit magit lsp-ui lsp-ivy ivy-rich helpful helm-lsp fzf flycheck evil-nerd-commenter evil-collection elscreen doom-themes doom-modeline dap-mode counsel-projectile company-box command-log-mode cider all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; set red background
;(set-face-attribute 'default nil :background "#1f1f1f")
