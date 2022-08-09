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
(require 'use-package)
(setq use-package-always-ensure t)

(setq howard/is-old-laptop (string= system-name "howard-vivobooks15x510uf"))
(setq howard/is-new-laptop (string= system-name "LAPTOP-LDFS2SBR"))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar

(tooltip-mode -1)               ; Disable tooltips
(menu-bar-mode -1)              ; Disable menu bar
(setq split-width-threshold 75)  ; default vertical split
(setq make-backup-files nil)    ; Don't do backups!
(setq pop-up-windows nil)       ; Don't show popup windows
(display-time)                  ; Show the current time in modeline
;; Something about indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

; A vim like scrolling expierence
(setq scroll-margin 14)
(setq maximum-scroll-margin 0.5)
(setq scroll-step 1)
(setq scroll-conservatively 101)

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
  :config 
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))
; A package that can hide the modeline
(use-package hide-mode-line)

(use-package dashboard
  :hook (dashboard-mode . (lambda () (beacon-mode -1)))
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
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
  (setq treemacs-width 30))
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

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
  (evil-set-initial-state 'dashboard-mode 'normal)
  :bind (:map evil-normal-state-map
              ("L" . 'evil-next-buffer)
              ("H" . 'evil-prev-buffer)
              ("Q" . 'image-kill-buffer)
              ("C-J" . 'evil-window-next)
              ("C-K" . 'evil-window-prev)
          :map evil-insert-state-map
              ("C-J" . 'evil-window-next)
              ("C-K" . 'evil-window-prev)))

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
       "." '(dirvish :which-key "Dirvish")
       "SPC" '(counsel-M-x :which-key "M-x"))
;; searching utilities
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "s" '(:ignore t :which-key "Search")
       "s f" '(projectile--find-file :which-key "Search Project file")
       "s t" '(counsel-projectile-rg :which-key "Search text")
       "s c" '(counsel-load-theme :which-key "Search colorscheme")
       "s b" '(persp-counsel-switch-buffer :which-key "Switch buffer")
       "s p" '(projectile-switch-project :which-key "Search Projects"))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "p" '(perspective-map :which-key "perspective"))
;; searching utilities
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "m" '(:ignore t :which-key "EMMS")
       "m m" '(emms :which-key "EMMS Playlist")
       "m l" '(emms-lyrics-toggle :which-key "EMMS Toggle lyrics")
       "m ," '(emms-seek-backward :which-key "EMMS Seek Backward")
       "m ." '(emms-seek-forward :which-key "EMMS Seek Forward")
       "m z" '(emms-toggle-repeat-track :which-key "EMMS Repeat track")
       "m Z" '(emms-toggle-repeat-playlist :which-key "EMMS Repeat playlist")
       "m r" '(emms-toggle-random-playlist :which-key "EMMS Random playlist")
       "m b" '(emms-browser :which-key "EMMS Browser"))
;; neotree
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "e" '(treemacs :which-key "TreeMacs"))

;; Elisp evaluation
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "x" '(:ignore t :which-key "Elisp Eval")
       "x e" '(eval-expression :which-key "Eval expression")
       "x l" '(eval-last-sexp :which-key "Eval-Last-Sexp")
       "x r" '(eval-region :which-key "Eval-Region"))

;; Configuration related
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "c" '(:ignore t :which-key "Config")
       "c r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload Emacs config")
       "c e" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Edit config file"))

;; Help system
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "h" '(:ignore t :which-key "help")
       "h f" '(counsel-describe-function :which-key "Describe Function")
       "h k" '(describe-key :which-key "Describe Key")
       "h p" '(describe-package :which-key "Describe Package")
       "h v" '(describe-variable :which-key "Describe Variable"))

;; Org mode system
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "o" '(:ignore t :which-key "Org-Mode")
       "o r" '(:ignore t :which-key "Org-Roam")
       "o d" '(:ignore t :which-key "Org-Dailies")
       "o a" '(org-agenda :which-key "Org Agenda")
       "o s" '(org-schedule :which-key "Org Schedule")
       "o n" '(org-narrow-to-subtree :which-key "Org Narrow to Tree")
       "o w" '(widen :which-key "Widen")
       "o c" '(org-capture :which-key "Org Capture")
       "o r c" '(org-roam-capture :which-key "Org Roam Capture")
       "o r f" '(org-roam-node-find :which-key "Find Org Roam file")
       "o d t" '(org-roam-dailies-goto-today :which-key "Show Dailies Today")
       "o d c" '(org-roam-dailies-capture-today :which-key "Org Dailies Capture"))

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
;; terminal related
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
       "t" '(:ignore t :which-key "terminal")
       "t v" '(vterm :which-key "Vterm")
       "t e" '(eshell :which-key "Eshell"))

;; Install Ivy
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
  (setq ivy-posframe-width 100)
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

(use-package anki-editor
  ;:after org-noter
  :config
  (setq anki-editor-create-decks 't))

(use-package calfw
  :commands cfw:open-org-calendar
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package calfw-org)

(defun howard/crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;; Better config for dired
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "a" 'dired-create-empty-file
    "q" 'dirvish-quit
    "l" 'dired-find-file))
;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode)
;;   :init
;;   (setq all-the-icons-dired-monochrome nil))
(use-package all-the-icons-dired)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  ;; (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  (dirvish-hide-details t)
  :config
  (dirvish-peek-mode)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  ;; Make sure to use the long name of flags when exists
  ;; eg. use "--almost-all" instead of "-A"
  ;; Otherwise some commands won't work properly
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ;; Dirvish has all the keybindings (except `dired-summary') in `dired-mode-map' already
   :map dirvish-mode-map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ;; ("i" . wdired-change-to-wdired-mode)
   ;; ("." . dired-omit-mode)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

; Magit Installation
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1))

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

(use-package leetcode
  :defer t
  :config
  (setq leetcode-prefer-language "python3"))
(add-to-list 'exec-path "~/.local/bin")

(use-package pdf-tools
  :hook (pdf-view-mode . hide-mode-line-mode)
  :after evil-collection
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; Project management
(use-package rg) ; searching for text in project
(use-package projectile
  :config (projectile-mode))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package shrface
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package eww
  :defer t
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(use-package go-translate
  :defer t
  :config
  (setq gts-translate-list '(("en" "zh") ("zh" "en")))
  (setq get-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render))))

(use-package wttrin
  :config
  (setq wttrin-default-cities '("Taipei" "Hsinchu")))

(use-package writeroom-mode)

;; Org-mode
(defun howard/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (setq-default line-spacing 2)
  (visual-line-mode 1)
  (electric-pair-mode -1))

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

(defun howard/org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

  The current time is used if the entry has no timestamp. If FILE
  is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
    (with-current-buffer (current-buffer)
      (save-excursion
        (org-cut-subtree)
        (if file (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)
        ))
    )
  )
(defun howard/is-project-p ()
  "A task with a 'PROJ' keyword"
  (member (nth 2 (org-heading-components)) '("PROJ")))

(defun howard/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (howard/find-project-task)
      (if (equal (point) task)
          nil t))))

(defun howard/find-project-task ()
  "Any task with a todo keyword that is in a project subtree"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
    (when (member (nth 2 (org-heading-components)) '("PROJ"))
      (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun howard/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading)
                               (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

(defun howard/select-projects ()
  "Selects tasks which are project headers"
  (howard/select-with-tag-function #'howard/is-project-p))
(defun howard/select-project-tasks ()
  "Skips tags which belong to projects (and is not a project itself)"
  (howard/select-with-tag-function
   #'(lambda () (and
                 (not (howard/is-project-p))
                 (howard/is-project-subtree-p)))))
(defvar howard-org-agenda-block--today-schedule
  '(agenda "" ((org-agenda-overriding-header "🗓 Today's Schedule:")
               (org-agenda-span 'day)
               (org-agenda-ndays 1)
               (org-deadline-warning-days 1)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+0d")))
    "A block showing a 1 day schedule.")

(defvar howard-org-agenda-block--weekly-log
  '(agenda "" ((org-agenda-overriding-header "📅 Weekly Log")
               (org-agenda-span 'week)
               (org-agenda-start-day "+1d")))
  "A block showing my schedule and logged tasks for this week.")

(defvar howard-org-agenda-block--three-days-sneak-peek
  '(agenda "" ((org-agenda-overriding-header "3⃣ Next Three Days")
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+1d")
               (org-agenda-span 3)))
  "A block showing what to do for the next three days. ")

(defvar howard-org-agenda-block--active-projects
    '(tags-todo "-INACTIVE-LATER-CANCELLED-REFILEr/!"
                ((org-agenda-overriding-header "📚 Active Projects:")
                 (org-agenda-skip-function 'howard/select-projects)))
    "All active projects: no inactive/someday/cancelled/refile.")

(defvar howard-org-agenda-block--next-tasks
  '(tags-todo "-INACTIVE-LATER-CANCELLED-ARCHIVE/!NEXT"
              ((org-agenda-overriding-header "👉 Next Tasks:")))
  "Next tasks.")
(defvar howard-org-agenda-display-settings
  '((org-agenda-start-with-log-mode t)
    (org-agenda-log-mode-items '(clock))
    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
                                (timeline . "  % s")
                                (todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                (tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
                                (search . "  %i %-12:c"))))
  "Display settings for my agenda views.")

(defvar howard-org-agenda-block--remaining-project-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-WAITING-REFILE-ARCHIVE/!-NEXT"
              ((org-agenda-overriding-header "Remaining Project Tasks:")
               (org-agenda-skip-function 'howard/select-project-tasks)))
  "Non-NEXT TODO items belonging to a project.")

;; Org Mode Config
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(use-package org
  :hook
  (org-mode . howard/org-mode-setup)
  (org-mode . flyspell-mode)
  :config
  (require 'org-tempo)
  (setq org-ellipsis " ▾")
  (howard/org-font-setup)
  (setq org-agenda-start-with-log-mode t)
  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))
  (setq org-log-into-drawer t)
  (setq org-adapt-indentation t)
  (setq org-indent-mode-turns-off-org-adapt-indentation nil)
  (setq org-agenda-window-setup 'only-window)
  (setq org-src-window-setup 'only-window)
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                            "xelatex -interaction nonstopmode %f"))
  (plist-put org-format-latex-options :scale 1.5)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (latex . t)
     (C . t)
     (jupyter . t)))          ; must be last
  (setq org-agenda-files
        (if howard/is-new-laptop
            '("/mnt/d/OrgFiles/OrgRoam/journal/Tasks.org")
          '("~/Documents/Org-Files/Tasks/Tasks.org" "~/Documents/Org-Files/Tasks/Archive.org")))
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "~/Documents/Org-Files/Tasks/Tasks.org" "Tasks")
           "* %^{Select your option|TODO|LATER|} %?\n SCHEDULED: %^T")
          ("p" "Project" entry (file+headline "~/Documents/Org-Files/Tasks/Tasks.org" "Projects")
                                            "* PROJ %?")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "INACTIVE(i)" "LATER(l)" "|" "CANCELED(c@/!)")))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (howard/org-font-setup)
  (setq org-agenda-custom-commands
        `(("d" "Daily Agenda"
           (,howard-org-agenda-block--today-schedule
            ,howard-org-agenda-block--three-days-sneak-peek
            ,howard-org-agenda-block--active-projects
            ,howard-org-agenda-block--next-tasks
            ,howard-org-agenda-block--remaining-project-tasks)))))
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
    :config
    (setq org-roam-directory
          (if howard/is-new-laptop
                "/mnt/d/OrgFiles/OrgRoam"
                "~/Documents/Org-Files/OrgRoam/"))
    :custom
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
    ("j" "journal" entry
        "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
        :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n"))))
    :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (org-roam-db-autosync-mode))

(use-package org-alert
  :config
  (setq alert-default-style 'notifications
        org-alert-interval 900
        org-alert-notification-title "🔔 Org Agenda"
        org-alert-notify-after-event-cutoff 10
        org-alert-notify-cutoff 100)
  (org-alert-enable))

(use-package htmlize)

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

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package lsp-mode
  :hook
  ((java-mode) . lsp-deferred)
  ((python-mode) . lsp-deferred)
  ((lua-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (evil-collection-define-key 'normal 'lsp-mode-map
    "K" 'lsp-ui-doc-show)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode)
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

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))  ; or lsp-deferred
; python
(require 'dap-python)

(use-package jupyter
  :defer t
  :init (org-babel-jupyter-aliases-from-kernelspecs))
;; lua
(use-package lua-mode
  :hook (lua-mode . electric-pair-mode))
(use-package markdown-mode
  :custom (markdown-command "/usr/sbin/pandoc"))
;; add electric mode to all programing mode
(add-hook 'prog-mode-hook 'electric-pair-mode)

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

(use-package vterm
  :config
  (setq shell-file-name "/bin/zsh"
          vterm-max-scrollback 5000))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
;; Add conda to eshell
(use-package conda
  :config
  (conda-env-initialize-eshell)
  (setq conda-env-home-directory (expand-file-name "~/.conda"))
  :custom
  (conda-anaconda-home "/opt/miniconda3"))

;; Keyboard shortcuts
(defun pad-string (str len)
  "Return a string of length LEN starting with STR, truncating or padding as necessary."
  (let* ((str-len (length str))
         (extra-len (- len str-len)))
    (if (>= extra-len 0)
        (concat str (make-string extra-len ? ))
      (concat (substring str 0 (- len 3)) "..."))))

(defun my-emms-track-description-function (track)
  "Detailed track listing for TRACK."
  (let ((type (emms-track-get track 'type))
        (name (emms-track-get track 'name))
        (artist (emms-track-get track 'info-artist))
        (album (emms-track-get track 'info-album))
        (title (emms-track-get track 'info-title))
        (tracknumber (emms-track-get track 'info-tracknumber))
        (year (emms-track-get-year track))
        (timet (emms-track-get track 'info-playing-time)))
    (cond ((eq type 'file)
           ;; If it has a minimum of metadata
           (if (and artist title)
               (concat
                " "
                (pad-string
                 (if title
                     (if tracknumber
                         (concat "[" (format "%02d" (string-to-number tracknumber)) "] " title) title) "Unknown Title") 33)
            "  "
            (pad-string (if timet (format "%02d:%02d" (/ timet 60) (% timet 60)) "") 5)
            "  "
            (pad-string (or artist "Unknown Artist") 18)
            "  "
            (pad-string (if album
                            (if year album) "Unknown Album") 25)
            "  "
            (pad-string (or year "") 4)) name))
          ((eq 'url type)
           (emms-format-url-track-name name))
          ;; E.g. playlists
          (t (concat (symbol-name type) ":" name)))))

(use-package emms
  :commands emms
  :hook ((emms-playlist-mode . (lambda () (beacon-mode -1)))
         (emms-browser-mode . (lambda () (beacon-mode -1))))
  :bind
  (("<f9>" . emms-pause)
   ("<f10>" . emms-stop)
   ("<f11>" . emms-previous)
   ("<f12>" . emms-next))
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-lyrics-display-on-modeline nil)
  (setq emms-lyrics-display-on-minibuffer t)
  (setq emms-track-description-function 'my-emms-track-description-function)
  (setq emms-info-functions '(emms-info-exiftool))
  (setq emms-seek-seconds 5)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  (setq emms-source-file-default-directory "~/Music/"))

(use-package lyrics-fetcher
  :after (emms)
  :config
  (lyrics-fetcher-use-backend 'neteasecloud))
