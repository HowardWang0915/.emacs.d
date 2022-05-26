(setq inhibit-startup-message t)

(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar

(tooltip-mode -1)               ; Disable tooltips
(menu-bar-mode -1)              ; Disable menu bar
(setq split-width-threshold 0)  ; default vertical split
(setq make-backup-files nil)    ; Don't do backups!

; A vim like scrolling expierence
(setq scroll-margin 14)
(setq maximum-scroll-margin 0.5)
(setq scroll-step 1)
(setq scroll-conservatively 101)

(setq doom-themes-treemacs-enable-variable-pitch nil)
(defun howard/setup-fonts ()
  (set-face-attribute 'default nil
    :font "Source Code Pro"
    :height 170
    :weight 'medium)
  (set-face-attribute 'variable-pitch nil
    :font "Source Serif Pro"
    :height 190
    :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
    :font "Source Code Pro"
    :height 170
    :weight 'medium)
  ;; Make comments italic
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
    :slant 'italic)
  ;; Set fonts for treemacs
  (setq doom-themes-treemacs-enable-variable-pitch nil))
(add-hook 'after-init-hook 'howard/setup-fonts)
(add-hook 'server-after-make-frame-hook 'howard/setup-fonts)

;; Install a better theme
(use-package treemacs-all-the-icons
  :after treemacs)
(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t)
  :config
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config)
    (doom-themes-org-config))

; Install the icons
(use-package all-the-icons)
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
; use doom mode line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq initial-buffer-choice (lambda() (get-buffer-create "*dashboard*")))
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                              (bookmarks . "book"))))

(column-number-mode)             ; toggle column number(not line number) display in the mode line
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))

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
  (setq treemacs-width 25))
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Using garbage magic hack.
 (use-package gcmh
   :config
   (gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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
;; Unbind S-<Space> to avoid chinese collision
(global-unset-key (kbd "C-SPC"))

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
       "." '(counsel-find-file :which-key "Find File")
       "SPC" '(counsel-M-x :which-key "M-x"))
;; lsp hover
(nvmap :states '(normal visual) :keymaps 'override
       "K" '(lsp-ui-doc-show :which-key "Hover"))

;; switch buffer
(nvmap :states '(normal) :keymaps 'override
       "L" '(lambda() (interactive (next-buffer 1)))
       "H" '(lambda() (interactive (previous-buffer 1))))

;; searching utilities
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "s" '(:ignore s :which-key "Search")
       "s f" '(projectile--find-file :which-key "Search Project file")
       "s t" '(counsel-projectile-rg :which-key "Search text")
       "s c" '(counsel-load-theme :which-key "Search colorscheme")
       "s b" '(counsel-switch-buffer :which-key "Switch buffer")
       "s p" '(projectile-switch-project : which-key "Search Projects"))

;; neotree
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "t" '(treemacs :which-key "TreeMacs"))

;; Elisp evaluation
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "e" '(:ignore e :which-key "Elisp Eval")
       "e l" '(eval-last-sexp :which-key "Eval-Last-Sexp")
       "e r" '(eval-region :which-key "Eval-Region"))

;; Configuration related
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "c" '(:ignore c :which-key "Config")
       "c r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload Emacs config")
       "c e" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Edit config file"))

;; Help system
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "h" '(:ignore h :which-key "help")
       "h f" '(counsel-describe-function :which-key "Describe Function")
       "h v" '(describe-variable :which-key "Describe Variable"))

;; Help system
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "o" '(:ignore o :which-key "Org-Mode")
       "o a" '(org-agenda :which-key "Org Agenda")
       "o c" '(org-roam-capture :which-key "Org Roam Capture")
       "o j" '(org-roam-dailies-goto-today :which-key "Show today's journal")
       "o d" '(org-roam-dailies-capture-today :which-key "Org Roam Dailies"))

;; LSP related
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "l" '(:ignore l :which-key "lsp")
       "l j" '(flycheck-next-error :which-key "Next Diagnostic")
       "l k" '(flycheck-previous-error :which-key "Previous Diagnostic"))
;; git
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "g" '(:ignore g :which-key "git")
       "g d" '(git-gutter:popup-hunk :which-key "Hunk Diff")
       "g g" '(magit :which-key "Magit")
       "g j" '(git-gutter:next-hunk :which-key "Next Hunk")
       "g s" '(git-gutter:stage-hunk :which-key "Stage Hunk")
       "g u" '(git-gutter:revert-hunk :which-key "Unstage Hunk")
       "g k" '(git-gutter:previous-hunk :which-key "Prev Hunk"))

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

; remove ^
(setq ivy-initial-inputs-alist nil)

; Show last used commands first
(use-package smex)
(smex-initialize)

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
      ivy-posframe-parameters '((:internal-border-width . 5)
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

; Magit Installation
  (use-package git-gutter
    :hook (prog-mode . git-gutter-mode)
    :config
    (setq git-gutter:update-interval 0.02)
    (git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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

(use-package writeroom-mode)

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
  (dolist (face '((org-level-1 . 1.35)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Dejavu Sans Mono" :weight 'semi-bold :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-title nil :inherit 'variable-pitch :weight 'semi-bold :height 1.2)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'variable-pitch)
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-block-begin-line nil :inherit '(shadow fixed-pitch)))
(add-hook 'server-after-make-frame-hook 'howard/setup-fonts)

;; Org Mode Config
(use-package org
  :hook (org-mode . howard/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (howard/org-font-setup)
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("/mnt/d/OrgFiles/OrgRoam/journal/Tasks.org"))
  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IDEA(i)" "|" "DONE(d!)")
        (sequence "LATER(l)" "|" "WAIT(w)" "CANCELED(c)")))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
;; Configure custom agenda views
       (howard/org-font-setup))
;; The old template system
(require 'org-tempo)
;; Let org-mode be evil
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/mnt/d/OrgFiles/OrgRoam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journal/")
  (org-roam-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n")
       :unnarrowed t)))
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %?"
       :target (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"))
    ("t" "Task" entry "* %^{Select your option|TODO|LATER|} %?\n SCHEDULE %^T" 
       :target (file+head+olp "Tasks.org"
                          "#+title: Tasks and Ideas"
                          ("Tasks")))
    ("i" "Idea" entry "* IDEA %?" 
       :target (file+head+olp "Tasks.org"
                          "#+title: Tasks and Ideas"
                          ("Ideas")))
    ("j" "journal" entry
       "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
       :target (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"))))
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun howard/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . howard/org-mode-visual-fill))

(defun howard/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'after-save-hook #'howard/org-babel-tangle-config)
;; (org-babel-load-file
;;   (expand-file-name
;;    "Emacs.org"
;;    user-emacs-directory))

(use-package lsp-mode
  :hook ((java-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui)
; debugger-mode
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

(use-package flycheck)

(use-package yasnippet-snippets)
(use-package yasnippet
  :config (yas-global-mode 1))

;; Add language servers here
(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))
; python
(require 'dap-python)
;; lua
(use-package lua-mode)
(use-package markdown-mode)
(custom-set-variables
 '(markdown-command "/usr/sbin/pandoc"))

;; completion framework
(use-package company
  :hook ((python-mode java-mode emacs-lisp-mode) . company-mode)
  :config
    (setq company-delay 0.1)
    (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
        ("<tab>" . company-select-next)
        ("<backtab>" . company-select-previous)))
;; better looking company
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package vterm)
(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
;; Add conda to eshell
(use-package conda)
(conda-env-initialize-eshell)
(custom-set-variables
 '(conda-anaconda-home "~/.local/bin/miniconda3"))
