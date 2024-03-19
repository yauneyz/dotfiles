;;; copilot-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from copilot.el

(autoload 'copilot-complete "copilot" "\
Complete at the current point." t)
(autoload 'copilot-mode "copilot" "\
Minor mode for Copilot.

This is a minor mode.  If called interactively, toggle the
`Copilot mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `copilot-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-copilot-mode 'globalized-minor-mode t)
(defvar global-copilot-mode nil "\
Non-nil if Global Copilot mode is enabled.
See the `global-copilot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-copilot-mode'.")
(custom-autoload 'global-copilot-mode "copilot" nil)
(autoload 'global-copilot-mode "copilot" "\
Toggle Copilot mode in all buffers.
With prefix ARG, enable Global Copilot mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Copilot mode is enabled in all buffers where
`copilot-turn-on-unless-buffer-read-only' would do it.

See `copilot-mode' for more information on Copilot mode.

(fn &optional ARG)" t)
(autoload 'copilot-install-server "copilot" "\
Interactively install server." t)
(autoload 'copilot-reinstall-server "copilot" "\
Interactively re-install server." t)
(autoload 'copilot-uninstall-server "copilot" "\
Delete a Copilot server from `copilot-install-dir'." t)
(register-definition-prefixes "copilot" '("copilot-"))


;;; Generated autoloads from copilot-balancer.el

(register-definition-prefixes "copilot-balancer" '("copilot-balancer-"))

;;; End of scraped data

(provide 'copilot-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; copilot-autoloads.el ends here
