(setq inhibit-startup-message t)

(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar

(tooltip-mode -1)               ; Disable tooltips
(menu-bar-mode -1)              ; Disable menu bar
(setq split-width-threshold 1)  ; default vertical split
(setq make-backup-files nil)    ; Don't do backups!

; A vim like scrolling expierence
(setq scroll-margin 14)
(setq maximum-scroll-margin 0.5)
(setq scroll-step 1)

(set-face-attribute 'default nil :height 180)

;; Install a better theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

; Install the icons
(use-package all-the-icons)
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

; use doom mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

(column-number-mode)             ; toggle column number(not line number) display in the mode line
(global-display-line-numbers-mode t) ; turn on line numbers

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                treemacs-mode-hook
                shell-mode-hook
                eshell-mode-hook
                helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; Add delimiters for easier reading. Prog mode is all programming mode
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; A better help system
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package treemacs
  :config
  (setq treemacs-width 30))

(use-package clipetty
 :ensure t
 :hook (after-init . global-clipetty-mode))

(set-language-environment "UTF-8") 
(set-default-coding-systems 'utf-8) 
(set-buffer-file-coding-system 'utf-8-unix)

; Install key-chords for some advanced configuration
(use-package key-chord)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

; Use which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

; Install evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-vsplit-windows-right t)
  :config
  (evil-mode 1)
  (key-chord-mode 1) ;; Allow jk to exit
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map  "kj" 'evil-normal-state)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

; A modular evil experience
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

; Undo tree
(use-package undo-tree
  :ensure t
  :after evil
  :init
  (setq undo-tree-auto-save-history nil)
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package general
  :config
  (general-evil-setup t))

;; searching utilities
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "." '(counsel-find-file : which-key "Find-File"))
;; searching utilities
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "s f" '(counsel-projectile-find-file :which-key "Search file")
       "s t" '(counsel-projectile-rg :which-key "Search text")
       "s p" '(counsel-projectile-switch-project : which-key "Search Projects"))

;; neotree
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "t" '(treemacs :which-key "TreeMacs"))

;; Code evaluation
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "e x" '(eval-last-sexp :which-key "Eval-Last-Sexp")
       "e r" '(eval-region :which-key "Eval-Region"))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;; Install Ivy
(require 'use-package)
(setq use-package-always-ensure t)
(use-package ivy
:diminish
:bind (("C-s" . swiper)
        :map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)	
        ;; ("C-l" . ivy-alt-done)
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
(ivy-mode 1))

(use-package ivy-rich
:after ivy
:init
(ivy-rich-mode 1))

; A floating window like expierence
(use-package ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
    `((counsel-M-x                         . ivy-posframe-display-at-frame-center)
      (counsel-projectile-rg               . ivy-posframe-display-at-frame-center)
      (counsel-projectile-switch project   . ivy-posframe-display-at-frame-center)
      (t                       . ivy-posframe-display))
      ivy-posframe-height-alist '((t . 10))
      ivy-posframe-parameters '((:internal-border-width 2)
                                (:internal-border-color . "white")))
      (ivy-posframe-mode 1))

; Make posframe respect original theme
(put 'ivy-posframe 'face-alias 'default)

; A package to utilize the full potential of ivy
(use-package counsel
:bind (("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer)
        ("C-x C-f" . counsel-find-file)
        :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history)))

; Magit Installation
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Better config for dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

;; Project management
(use-package rg) ; searching for text in project
(use-package projectile
  :config (projectile-mode))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Org-mode
(defun howard/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun howard/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . howard/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (howard/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun howard/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'after-save-hook #'howard/org-babel-tangle-config)

(use-package lsp-mode
  :hook ((java-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui)

(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-global-mode 1))

;; Add language servers here
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

;; completion framework
(use-package company
  :hook ((java-mode emacs-lisp-mode) . company-mode)
  :config
    (setq company-delay 0.1)
    (setq company-minimum-prefix-length 1))
