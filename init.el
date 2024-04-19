
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

;(when (memq window-system '(mac ns x))
	;(exec-path-from-shell-initialize))

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

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
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

;; Evil Surround
(use-package evil-surround
	:config
	(global-evil-surround-mode 1))

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

;; FZF
(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; use ivy with fzf



;; NERD Commenter
(install-if-necessary 'evil-nerd-commenter)
;; (evilnc-default-hotkeys)
(evil-define-key '(normal visual) 'global (kbd "<leader>cc") 'evilnc-comment-or-uncomment-lines)
(evil-define-key '(normal visual) 'global (kbd "<leader>cu") 'evilnc-comment-or-uncomment-lines)


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

;; Pulsar
(use-package pulsar
	:config
	(setq pulsar-pulse t)
	(setq pulsar-delay 0.055)
	(setq pulsar-iterations 10)
	(setq pulsar-face 'pulsar-magenta)
	(setq pulsar-highlight-face 'pulsar-yellow)
	(pulsar-global-mode 1))

;; ELscreen
(use-package elscreen
	:config
	(elscreen-start))

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
	:after ivy
  :config
  (ivy-rich-mode 1))

;; Flx for fuzzy matching
(use-package flx)
(setq ivy-re-builders-alist
			'((t . ivy--regex-fuzzy)))

;; Also use prescient for sorting
(use-package prescient
	 :config
	 (prescient-persist-mode 1))

(use-package ivy-prescient
	:after (ivy prescient)
	:config
	(ivy-prescient-mode 1))

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
  (setq projectile-mode-line "Projectile")
	(setq projectile-sort-order 'recently-active)
	(setq projectile-generic-command "find . -type f -not -path '*/node_modules/*' -not -path '*/build/*' -not -path '*/.clj-kondo/*' -print0")
)




;; Counsel Projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode)
	:after ivy-prescient)

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
		(treemacs-project-follow-mode t)
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

;; Format All
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
								;; Python
								'((python-mode "black" "isort")
									;; Clojure
									(clojure-mode "cljfmt")
									;;Rust
									(rust-mode "rustfmt")
									;; Emacs Lisp
									(emacs-lisp-mode "emacs-lisp-formatter"))))

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
	;(load-theme 'doom-tokyo-night t)
	(load-theme 'doom-rouge t)
	;(load-theme 'doom-dracula t)
	;(load-theme 'doom-snazzy t)
	;(load-theme 'doom-outrun-electric t)
	)


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

(use-package windmove)

;; Evil keybindings for window movement
(evil-define-key 'normal 'global (kbd "C-h") 'windmove-left)
(evil-define-key 'normal 'global (kbd "C-j") 'windmove-down)
(evil-define-key 'normal 'global (kbd "C-k") 'windmove-up)
(evil-define-key 'normal 'global (kbd "C-l") 'windmove-right)

;; Repeat for treemacs
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "C-h") 'windmove-left)
  (define-key treemacs-mode-map (kbd "C-j") 'windmove-down)
  (define-key treemacs-mode-map (kbd "C-k") 'windmove-up)
  (define-key treemacs-mode-map (kbd "C-l") 'windmove-right))


(global-set-key (kbd "C-M-h") 'help-command)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)

;; paredit commands
;; (evil-define-key 'normal 'global (kbd "C-f") 'paredit-forward)
;; (evil-define-key 'normal 'global (kbd "C-b") 'paredit-backward)
;; (evil-define-key 'normal 'global (kbd "C-d") 'paredit-forward-down)
;; (evil-define-key 'normal 'global (kbd "C-u") 'paredit-backward-up)
;; (evil-define-key 'normal 'global (kbd "C-n") 'paredit-forward-up)
;; (evil-define-key 'normal 'global (kbd "C-p") 'paredit-backward-down)
; Slurp and barf on parentheses and brackets


;; Turn off read-only mode
(read-only-mode 0)

(setq lsp-pylsp-plugins-autopep8-enabled t)

;; Fountain mode
;(use-package fountain-mode)

;; =================== Evil command shortcuts ===================


;; Elscreen
(evil-define-key 'normal 'global (kbd "<leader>sc") 'elscreen-create)
(evil-define-key 'normal 'global (kbd "<leader>sk") 'elscreen-kill)
(evil-define-key 'normal 'global (kbd "<leader>sg") 'elscreen-goto)
(evil-define-key 'normal 'global (kbd "<leader>sp") 'elscreen-previous)
(evil-define-key 'normal 'global (kbd "<leader>sn") 'elscreen-next)
(evil-define-key 'normal 'global (kbd "<leader>sb") 'elscreen-find-and-goto-by-buffer)
(evil-define-key 'normal 'global (kbd "<leader>so") 'elscreen-toggle) ;; Jump to last screen

;; Elscreen with M-h and M-l
(evil-define-key 'normal 'global (kbd "M-h") 'elscreen-previous)
(evil-define-key 'normal 'global (kbd "M-l") 'elscreen-next)

;; Treemacs
(evil-define-key 'normal 'global (kbd "<leader>tt") 'treemacs)
(evil-define-key 'normal 'global (kbd "<leader>tf") 'treemacs-find-file)

;; Magit
(evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)

;; Cider
(evil-define-key 'normal 'global (kbd "<leader>cj") 'cider-jack-in)
(evil-define-key 'normal 'global (kbd "<leader>cl") 'cider-eval-last-sexp)
(evil-define-key 'normal 'global (kbd "<leader>cb") 'cider-eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cf") 'cider-format-buffer)
(evil-define-key 'normal 'global (kbd "<leader>cr") 'cider-restart)

;; Emacs lisp
(evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>el") 'eval-last-sexp)

;; Repl
(evil-define-key 'normal 'global (kbd "<leader>nr") 'cider-switch-to-repl-buffer)
(evil-define-key 'normal 'global (kbd "<leader>nc") 'cider-switch-to-last-clojure-buffer)

;; Buffer control
(evil-define-key 'normal 'global (kbd "<leader>bs") 'counsel-switch-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)

;; File finding

(evil-define-key 'normal 'global (kbd "<leader>SPC") 'fzf-find-file)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'counsel-projectile-rg)

;; LSP commands

(evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "gi") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>rn") 'lsp-rename)

;; Tooltips - glance, hover, etc
(evil-define-key 'normal 'global (kbd "<leader>lg") 'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-ui-doc-show)
(evil-define-key 'normal 'global (kbd "<leader>ls") 'lsp-signature-help)
(evil-define-key 'normal 'global (kbd "<leader>lh") 'lsp-describe-thing-at-point)


;; Help commands
(evil-define-key 'normal 'global (kbd "<leader>hf") 'counsel-describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hv") 'counsel-describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") 'counsel-describe-key)

;; Disable Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
