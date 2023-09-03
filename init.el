;;(benchmark-init/activate)

;(add-to-list 'default-frame-alist '(undecorated . t))
;(global-unset-key (kbd "C-s-<down-mouse-1>"))
;(global-unset-key (kbd "C-s-<drag-mouse-1>"))

;; Tmux
(global-unset-key (kbd "C-o"))

;; Customize user interface.
(menu-bar-mode 0)
(tool-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)

(setq ring-bell-function 'ignore)         ; Disable bell sound
(setq frame-resize-pixelwise t)
;
(setq delete-selection-mode 1)                 ; Selected text will be overwritten when you start typing
(setq global-auto-revert-mode t)               ; Auto-update buffer if file has changed on disk
(add-hook 'before-save-hook
	        'delete-trailing-whitespace)    ; Delete trailing whitespace on save
;
;
;
;
(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(global-unset-key (kbd "<mouse-3>"))
(global-unset-key (kbd "<down-mouse-3>"))
;(global-unset-key [mouse-3])

;; Always fill at 78 characters
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(setq-default fill-column 78)
;(global-display-fill-column-indicator-mode)

;; Set loadpath
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
;;(require 'flymake-extension)
;;
;;(push '(face . highlight) (get :note 'flymake-overlay-control))

;; Manpages same window new buffer!
;;(setq-default Man-notify-method 'pushy)

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file t)

;; Load theme after custom.el to emacs wont freak out
;;(load-theme 'ef-light t)
;;(load-theme 'modus-operandi t)
(when (require 'modus-themes nil 'noerror)
(load-theme 'modus-vivendi t)
  )


;; Backup and Autosave Directories
(setq temporary-file-directory "~/.emacs.d/tmp")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Prevent UI dialogs
;(setq use-dialog-box nil)

;; Remember last cursor location of opened files
(save-place-mode 1)


;; Use ESC as universal get me out of here command
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Enable SSH editing
(setq tramp-default-method "ssh")

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or (eq (car before) 1) (eq (car before) -1))
      before)))

(advice-add
 'show-paren--locate-near-paren
 :override #'show-paren--locate-near-paren-ad)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
;;(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Move file to trash instead of removing.
;;(setq-default delete-by-moving-to-trash t)


;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Insane
(defalias 'yes-or-no-p 'y-or-n-p)

;;(require 'gnutls)
;;(push "/usr/pkg/etc/libressl/cert.pem" gnutls-trustfiles)

;; Package configs
(require 'package)
;; Dont check package signatures - this is stupid
(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; We will use 'use-package' to install and configure packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; Packages
(use-package
 evil
 :ensure t
 :init (setq evil-want-keybinding nil)
 )
(use-package
 evil-collection
 :after evil
 :ensure t
 :config (evil-collection-init))

(use-package vertico
  :ensure t
  :init
    (vertico-mode)
    )

;; markdown-mode
(use-package markdown-mode
  :ensure t)

;; Path for daemon
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Dim windows
(use-package
 dimmer
 :ensure t
 :config
 (setq dimmer-fraction 0.2)
 (setq dimmer-adjustment-mode :foreground)
 (setq dimmer-use-colorspace :rgb)

 (dimmer-mode 1))

;; default splits
(defun split-window-config ()
  (interactive)
  ;(recentf-open-files)
  (split-window-right)
  (other-window 1)
  (scratch-buffer)
  ;(split-window-horizontally 80)
  ;(other-window 1)
  (split-window)
  (other-window 1)
  (multi-vterm)
  ;(eshell)
  (other-window -3)
  (setq grb-temporary-window (nth 2 (window-list)))
  (setq special-display-function #'grb-special-display))

(define-key global-map (kbd "<f2>") #'split-window-config)


(use-package ranger :ensure t)
(setq ranger-show-hidden t)
(setq ranger-cleanup-on-disable t)

(use-package diff-hl :ensure t)
(use-package multi-term :ensure t)

;; Dashboard
(use-package dashboard :ensure t :config (load "~/.emacs.d/my-dashboard.el"))

(add-hook 'prog-mode-hook 'evil-local-mode)
(add-hook 'prog-mode-hook 'tab-line-mode)
(add-hook 'prog-mode-hook 'diff-hl-mode)
(add-hook 'term-mode-hook 'tab-line-mode)


;; Paredit
(use-package paredit :ensure t)
(use-package rainbow-delimiters :ensure t)

;; Geiser for LISP
;;(use-package geiser :ensure t)
;(use-package geiser-racket :ensure t)

;; Enable Paredit.
;(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;(add-hook 'ielm-mode-hook 'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)


;; Themes
(use-package ef-themes :ensure t)
(use-package which-key :ensure t)
(use-package doom-themes :ensure t)
(setq custom-safe-themes t)


;;; For packaged versions which must use `require'.
(use-package
 modus-themes
 :ensure t
 :config (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;; Other packages
(use-package orderless :ensure t)
(use-package visual-regexp :ensure t)

;; evil
;(add-hook 'prog-mode-hook 'evil-local-mode)


(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(evil-set-undo-system 'undo-redo)

(add-hook 'dired-sidebar-mode-hook 'evil-normal-state)

;; enable recentf
;;(recentf-mode t)

;; open buffers in new tab not new window
;;(setq pop-up-frames t)
;;(setq display-buffer-base-action '(display-buffer-in-new-tab))

;; Highlight trailing whitespace
;(set-default 'show-trailing-whitespace t)

; GRB: open temporary buffers in a dedicated window split
(setq special-display-regexps
      '("^\\*Completions\\*$"
        "^\\*Help\\*$"
        "^\\*grep\\*$"
        "^\\*Apropos\\*$"
        "^\\*elisp macroexpansion\\*$"
        "^\\*local variables\\*$"
        "^\\*Compile-Log\\*$"
        "^\\*Quail Completions\\*$"
        "^\\*Occur\\*$"
        "^\\*frequencies\\*$"
        "^\\*compilation\\*$"
        "^\\*Locate\\*$"
        "^\\*Colors\\*$"
        "^\\*Ibuffer\\*$"
        "^\\*tumme-display-image\\*$"
        "^\\*SLIME Description\\*$"
        "^\\*.* output\\*$" ; tex compilation buffer
        "^\\*TeX Help\\*$"
        "^\\*Shell Command Output\\*$"
        "^\\*Async Shell Command\\*$"
        "^\\*Backtrace\\*$"))
(setq grb-temporary-window (nth 2 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function #'grb-special-display)


;; Enable tab-bar-mode

(tab-bar-mode 1) ;; enable tab bar
(setq tab-bar-show 1) ;; hide bar if <= 1 tabs open
(setq tab-bar-new-tab-choice "*dashboard*") ;; buffer to show in new tabs
(setq tab-bar-tab-hints t) ;; show tab numbers
(setq tab-bar-select-tab-modifiers "super")

(setq tab-bar-format
      '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "s-1") 'tab-bar-select-tab)
(global-set-key (kbd "s-2") 'tab-bar-select-tab)
(global-set-key (kbd "s-3") 'tab-bar-select-tab)
(global-set-key (kbd "s-4") 'tab-bar-select-tab)
(global-set-key (kbd "s-5") 'tab-bar-select-tab)
(global-set-key (kbd "s-6") 'tab-bar-select-tab)
(global-set-key (kbd "s-7") 'tab-bar-select-tab)
(global-set-key (kbd "s-8") 'tab-bar-select-tab)
(global-set-key (kbd "s-9") 'tab-bar-select-tab)
(global-set-key (kbd "s-0") 'tab-bar-select-tab)

(add-hook
 'tab-bar-tab-post-open-functions
 (lambda (&rest _) (call-interactively #'tab-bar-rename-tab)))

(global-unset-key (kbd "s-q"))
(global-set-key (kbd "s-q") 'kill-current-buffer)


;;(global-tab-line-mode t) ;;makes buffers to tabs -- for noobs
;;(setq tab-line-new-button-show nil)  ;; do not show add-new button
;;(setq tab-line-close-button-show nil)  ;; do not show close button
(setq tab-line-separator "") ;; set it to empty
;;
;;;; Fix scroll behaviour
;;(setq scroll-conservatively 101)
;;
;;;; tab color settings
;;(set-face-attribute 'tab-line-tab nil ;; active tab in another window
;;      :inherit 'tab-line
;;      :foreground "gray40" :background "gray20" :box nil)
;;(set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
;;      :background "#000000" :foreground "white" :box nil)
;;(set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
;;      :background "white" :foreground "black" :box nil)
;;(set-face-attribute 'tab-line-highlight nil ;; mouseover
;;      :background "white" :foreground 'unspecified)


;; exec-path-from-shell
(use-package exec-path-from-shell :ensure t)

;; tramp ivy counsel
(use-package counsel :ensure t)
(use-package ivy :ensure t)
;;(use-package counsel-tramp :ensure t)

;; Org mode stuff
(require 'org)
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.5))
;;(use-package ox-rfc :ensure t)
(setq org-highlight-latex-and-related '(latex script))

;; Improve org mode looks
(setq
 org-startup-indented t
 org-pretty-entities t
 org-hide-emphasis-markers t
 org-startup-with-inline-images t
 org-image-actual-width '(300))

;; Move-text lines around with meta-up/down.
(use-package move-text :ensure t :config (move-text-default-bindings))

;; Enable rainbow delimiters in prog buffers
(use-package rainbow-delimiters :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Never use tabs, use spaces instead.
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)

;; set save location
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Start packages
(which-key-mode)

;; Completion framework
;; Enable completion by narrowing
;; M-x etc..


;; Optionally use the `orderless' completion style.
(use-package
 orderless
 :init
 (setq
  completion-styles '(orderless basic)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

;;; Extended completion utilities
(use-package consult
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :init
    :config
    )

(global-set-key [rebind switch-to-buffer] #'consult-buffer)
(setq
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 completion-ignore-case t)

;; Enable line numbering by default
;(global-display-line-numbers-mode t)
;(setq display-line-numbers 'relative)
;;(global-display-line-numbers-mode 1)
(fringe-mode '(4 . 0))

;(use-package magit :ensure t)
;; Disable line-numbers minor mode for neotree
;;(add-hook 'neo-after-create-hook (lambda (&optional dummy) (display-line-numbers-mode -1)))

(use-package
 counsel-etags
 :ensure t
 :bind (("C-]" . counsel-etags-find-tag-at-point))
 :init
 (setq tags-add-tables nil)
 (add-hook
  'prog-mode-hook
  (lambda ()
    (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags
              'append 'local)))
 :config
 (setq counsel-etags-update-interval 60)
 (push "build" counsel-etags-ignore-directories))

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook
 'prog-mode-hook
 (lambda ()
   (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags
             'append
             'local)))

;(setq counsel-etags-ctags-options-file "~/.ctags")
(with-eval-after-load 'counsel-etags
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (push "build_clang" counsel-etags-ignore-directories)
  (push "build_clang" counsel-etags-ignore-directories)
  ;; counsel-etags-ignore-filenames supports wildcast
  (push "TAGS" counsel-etags-ignore-filenames)
  (push "*.json" counsel-etags-ignore-filenames))


(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; LSP Support
(use-package eglot :ensure t)
(setq eldoc-echo-area-use-multiline-p nil)

(setq lsp-eldoc-enable-hover nil)
(setq eldoc-echo-area-prefer-doc-buffer t)
(setq lsp-signature-auto-activate nil)
;;            eldoc-echo-area-use-multiline-p nil)

;; LSP RUST
(use-package rust-mode :ensure t)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq rust-format-on-save t)
;; (add-hook 'rust-mode-hook
;;          (lambda () (prettify-symbols-mode)))

;;(use-package tree-sitter
;;  :config
;;  (require 'tree-sitter-langs)
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Flycheck
;(use-package flycheck
;               :init
;               :ensure t
;               )
;(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Pop-up auto-completion

(use-package
 company
 :ensure
 :custom
 (company-idle-delay 0.5) ;; how long to wait until popup
 ;; (company-begin-commands nil) ;; uncomment to disable popup
 :bind
 (:map
  company-active-map
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)
  ("M-<" . company-select-first)
  ("M->" . company-select-last)
  ;;("<tab>". tab-indent-or-complete)
  ;;("TAB". tab-indent-or-complete))
  ("<tab>" . company-complete-selection)
  ("TAB" . company-complete-selection))
 (:map
  company-mode-map
  ("<tab>" . tab-indent-or-complete)
  ("TAB" . tab-indent-or-complete)))
;; above is strange, fix later

(use-package
 yasnippet
 :ensure
 :config
 (yas-reload-all)
 (add-hook 'prog-mode-hook 'yas-minor-mode)
 (add-hook 'text-mode-hook 'yas-minor-mode))


;;(define-key yas-keymap [(tab)]       nil)
;;(define-key yas-keymap (kbd "TAB")   nil)
;;(define-key yas-keymap [(shift tab)] nil)
;;(setq yas-next-field-key '("C-j")) ;; or "C-j"

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand) (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>")
        t
      (backward-char 1)
      (if (looking-at "\\.")
          t
        (backward-char 1)
        (if (looking-at "::")
            t
          nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode) (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; Enable Company by default in programming buffers
;(add-hook 'prog-mode-hook #'company-mode)

(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(defalias 'yes-or-no #'y-or-n-p)


;; Customize Rainbow Delimiters.
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666") ; dark gray

;; Drag window
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "C-x z"))
(setq winner-dont-bind-my-keys t)
;(setq winner-mode-map (make-sparse-keymap))
(winner-mode 1)
(setq winner-dont-bind-my-keys t)

(global-set-key (kbd "C-x C-z") 'winner-undo)
(global-set-key (kbd "C-x C-r") 'winner-redo)

;; follow mode

;; WindMove keybinds
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)




;; dired keybinds

(use-package
 dired-sidebar
 :ensure t
 :commands (dired-sidebar-toggle-sidebar)
 :init
 (add-hook
  'dired-sidebar-mode-hook
  (lambda ()
    (unless (file-remote-p default-directory)
      (auto-revert-mode))))
 :config
 (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
 (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
 (setq dired-sidebar-subtree-line-prefix "__")
 (setq dired-sidebar-use-term-integration t)
 (setq dired-use-ls-dired nil))

;; Omit files in dired
(setq dired-omit-files
      (rx
       (or (seq bol (?  ".") "#") ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol) ;; backup-files
           (seq bol "CVS" eol) ;; CVS dirs
           )))

;; Toggle on hide by default
(add-hook 'dired-mode-hook 'dired-omit-mode)

(defun turn-buffer-modeline-green ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "#00ff00"))

(defun turn-buffer-modeline-white ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "#ffffff"))

(defun turn-buffer-modeline-blue ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "orange"))

(defun do--status-bar-change-mode-line-color ()
  (cond
   ((memq evil-state '(hybrid insert emacs))
    (turn-buffer-modeline-green))
   ((memq evil-state '(visual))
    (turn-buffer-modeline-blue))
   (t
    (turn-buffer-modeline-white))))

(add-hook 'post-command-hook 'do--status-bar-change-mode-line-color)
(add-hook 'windmove-do-window-select 'do--status-bar-change-mode-line-color)
(add-hook 'find-file-hook 'do--status-bar-change-mode-line-color)

(defun dotspacemacs/user-config ()
  (add-hook 'evil-hybrid-state-entry-hook 'turn-buffer-modeline-green)
  (add-hook 'evil-hybrid-state-exit-hook 'turn-buffer-modeline-white))

(global-set-key (kbd "C-c d") 'dired-sidebar-toggle-sidebar)

(setq
 modus-themes-custom-auto-reload nil
 modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
 modus-themes-mixed-fonts t
 modus-themes-variable-pitch-ui nil
 modus-themes-italic-constructs t
 modus-themes-bold-constructs nil
 modus-themes-org-blocks nil
 modus-themes-completions '((t . (extrabold)))
 modus-themes-prompts nil
 modus-themes-headings
 '((agenda-structure . (variable-pitch light 2.2))
   (agenda-date . (variable-pitch regular 1.3))
   (t . (regular 1.15))))

(setq modus-themes-common-palette-overrides
      '((cursor magenta-cooler)
        ;; Make the fringe invisible.
        ;(fringe unspecified)
        ;; Make line numbers less intense and add a shade of cyan
        ;; for the current line number.
        ;(fg-line-number-inactive "gray50")
        (fg-line-number-active cyan-cooler)
        ;(bg-line-number-inactive unspecified)
        ;(bg-line-number-active unspecified)
        ;; Make the current line of `hl-line-mode' a fine shade of
        ;; gray (though also see my `lin' package).
        (bg-hl-line bg-dim)
        ;; Make the region have a cyan-green background with no
        ;; specific foreground (use foreground of underlying text).
        ;; "bg-sage" refers to Salvia officinalis, else the common
        ;; sage.
        (bg-region bg-sage)
        (fg-region unspecified)
        ;; Make matching parentheses a shade of magenta.  It
        ;; complements the region nicely.
        (bg-paren-match bg-magenta-intense)
        ;; Make email citations faint and neutral, reducing the
        ;; default four colors to two; make mail headers cyan-blue.
        (mail-cite-0 fg-dim)
        (mail-cite-1 blue-faint)
        (mail-cite-2 fg-dim)
        (mail-cite-3 blue-faint)
        (mail-part cyan-warmer)
        (mail-recipient blue-warmer)
        (mail-subject magenta-cooler)
        (mail-other cyan-warmer)
        ;; Change dates to a set of more subtle combinations.
        (date-deadline magenta-cooler)
        (date-scheduled magenta)
        (date-weekday fg-main)
        (date-event fg-dim)
        (date-now blue-faint)
        ;; Make tags (Org) less colorful and tables look the same as
        ;; the default foreground.
        (prose-done cyan-cooler)
        (prose-tag fg-dim)
        (prose-table fg-main)
        ;; Make headings less colorful (though I never use deeply
        ;; nested headings).
        (fg-heading-2 blue-faint)
        (fg-heading-3 magenta-faint)
        (fg-heading-4 blue-faint)
        (fg-heading-5 magenta-faint)
        (fg-heading-6 blue-faint)
        (fg-heading-7 magenta-faint)
        (fg-heading-8 blue-faint)
        ;; Make the active mode line a fine shade of lavender
        ;; (purple) and tone down the gray of the inactive mode
        ;; lines.
        ;(bg-mode-line-active bg-lavender)
        ;(border-mode-line-active bg-lavender)

        ;(bg-mode-line-inactive bg-dim)
        ;(border-mode-line-inactive bg-inactive)
        ;; Make the prompts a shade of magenta, to fit in nicely with
        ;; the overall blue-cyan-purple style of the other overrides.
        ;; Add a nuanced background as well.
        ;(bg-prompt bg-magenta-nuanced)
        ;(fg-prompt magenta-cooler)
        ;; Tweak some more constructs for stylistic constistency.
        ;(name blue-warmer)
        ;(identifier magenta-faint)
        ;(keybind magenta-cooler)
        ;(accent-0 magenta-cooler)
        ;(accent-1 cyan-cooler)
        ;(accent-2 blue-warmer)
        ;(accent-3 red-cooler)
        ))

;; Make the active mode line have a pseudo 3D effect (this assumes
;; you are using the default mode line and not an extra package).
;(custom-set-faces
; '(mode-line ((t :box (:style released-button)))))


(use-package fontaine :ensure t)

(setq fontaine-presets
      '((small
         :default-family "Hack"
         :default-weight normal
         :default-height 75
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Noto Sans"
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (regular
         :default-family "Iosevka Comfy"
         :default-weight normal
         :default-height 100
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (medium
         :default-family "Source Code Pro"
         :default-weight normal
         :default-height 110
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Source Sans Pro"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight semibold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (large
         :default-family "Iosevka Comfy"
         :default-weight semilight
         :default-height 160
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (presentation
         :default-family "Iosevka Comfy"
         :default-weight semilight
         :default-height 170
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))

;; Set last preset or fall back to desired style from `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;; Persist font configurations while switching themes (doing it with
;; my `modus-themes' and `ef-themes' via the hooks they provide).
(dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
  (add-hook hook #'fontaine-apply-current-preset))

;; Highlight yanked region
;; Source: https://blog.meain.io/2020/emacs-highlight-yanked/
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)

;(defun restore-desktop (frame)
;  "Restores desktop and cancels hook after first frame opens.
;   So the daemon can run at startup and it'll still work"
;  (with-selected-frame frame
;    (desktop-save-mode 1)
;    (desktop-read)
;    (remove-hook 'after-make-frame-functions 'restore-desktop)))
;(add-hook 'after-make-frame-functions 'restore-desktop)

;; All configs that need to apply for
;; each new emacsclient window
;; need to be wrapped inside this hook!
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (fontaine-set-preset 'large)
   ;(set-frame-font "Go Mono 13" nil t)
   (scroll-bar-mode 0)
   ;(split-window-config)
   ;'(restore-desktop)
   ))
;(require 'desktop)
;(setq desktop-restore-forces-onscreen nil)
;(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)

;(if (not (daemonp))
;(split-window-config)
;    )


;; use only one desktop
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
(add-hook
 'desktop-after-read-hook
 '(lambda ()
    ;; desktop-remove clears desktop-dirname
    (setq desktop-dirname-tmp desktop-dirname)
    (desktop-remove)
    (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))


(autoload 'cmake-project-mode "cmake-project" nil t)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

;include a daemon check
;; ask user whether to restore desktop at start-up
;(add-hook 'after-init-hook
;	  '(lambda ()
;	     (if (saved-session)
;		 (if (y-or-n-p "Restore desktop? ")
;		     (session-restore)))))


(defun clear-register ()
  "Clear the contents of an interactively chosen register."
  (interactive)
  (let ((register (register-read-with-preview "Clear register: ")))
    (when register
      (set-register register nil)
      (message "Cleared register %c." register))))
