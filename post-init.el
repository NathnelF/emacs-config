;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :demand t
  :ensure t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))



(use-package buffer-terminator
  :ensure t
  :custom
  ;; Enable/Disable verbose mode to log buffer cleanup events
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))

(setq-default explicit-shell-file-name "/bin/bash")

;; Make RET act as "y" in y-or-n prompts
(define-key y-or-n-p-map (kbd "RET") 'act)
(define-key y-or-n-p-map (kbd "<return>") 'act)

;; --- Platform Specific Configuration ---
(cond
 ;; macOS Settings
 ((eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))


 ;; Windows Settings
 ((eq system-type 'windows-nt)
  (setq default-directory "C:/Users/YourName/Documents/")
  ;; Point to where you installed ripgrep/clangd on Windows
  (add-to-list 'exec-path "C:/Program Files/LLVM/bin")))



(use-package org
  :ensure t
  :config
  (setq org-directory "~/org")  ; Where you keep org files
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Inbox")
           "* TODO %?\n")))

  
  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))
  


(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Two-panel startup with balanced windows
(add-hook 'emacs-startup-hook
          (lambda ()
            (split-window-right)
            (balance-windows)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (require 'eshell)
            (require 'esh-module)))

(electric-pair-mode 1)
(electric-indent-mode 1)

(defun post-init ()
  "Open post-init.el in the appropriate config directory."
  (interactive)
  (find-file
   (cond
    ((eq system-type 'windows-nt)
     "~/.emacs.d/post-init.el")
    (t
     "~/.minimal-emacs.d/post-init.el"))))

(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(setq-default display-line-numbers-type 'relative)


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))  ; Show popup after 0.3 seconds

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'doom-moonlight t)

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

(use-package dired
    :ensure nil
    :commands (dired)
    :hook
    ((dired-mode . dired-hide-details-mode)
     (dired-mode . hl-line-mode)
     (dired-mode . auto-revert-mode))
    :config
    (setq dired-recursive-copies 'always)
    (setq dired-recursive-deletes 'always)
    (setq delete-by-moving-to-trash t)
    (setq dired-dwim-target t)
    (setq dired-kill-when-opening-new-dired-buffer t)

      ;; hjkl navigation
  (define-key dired-mode-map (kbd "j") #'dired-next-line)
  (define-key dired-mode-map (kbd "k") #'dired-previous-line)
  (define-key dired-mode-map (kbd "h") #'dired-up-directory)
  (define-key dired-mode-map (kbd "l") #'dired-find-file)

  (define-key dired-mode-map (kbd "r") #'dired-do-rename)
  (define-key dired-mode-map (kbd "d") #'dired-flag-file-deletion)
  (define-key dired-mode-map (kbd "x") #'dired-do-flagged-delete)
  (define-key dired-mode-map (kbd "D") #'dired-do-delete)

  ;; (define-key dired-mode-map (kbd "c") nil)
  ;; (define-key dired-mode-map (kbd "c d") #'dired-create-directory)
  ;; (define-key dired-mode-map (kbd "c f") #'dired-create-empty-file)
  )

(use-package dired-ranger
  :ensure t
  :after dired
  :bind (:map dired-mode-map
         ("y" . dired-ranger-copy)    
         ("X" . dired-ranger-move)    
         ("p" . dired-ranger-paste)))
  ;; :config
  ;; (advice-add 'dired-ranger-paste :after
  ;;             (lambda (&rest _)
  ;;               (revert-buffer)))
  ;; (advice-add 'dired-ranger-move :after
  ;;             (lambda (&rest _)
;;               (revert-buffer))))

;; refresh after operations.
(with-eval-after-load 'dired
  ;; List of operations that should trigger refresh
  (dolist (func '(dired-do-rename
                  dired-do-flagged-delete
                  dired-do-delete
                  dired-create-directory
                  dired-create-empty-file))
    (advice-add func :after (lambda (&rest _) (revert-buffer)))))

(with-eval-after-load 'dired-ranger
  (dolist (func '(dired-ranger-paste
                  dired-ranger-move))
    (advice-add func :after (lambda (&rest _) (revert-buffer)))))

;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

(with-eval-after-load 'dired
  (defun my/dired-create-file-at-point ()
    "Create file, defaulting to directory at point or parent directory of file at point."
    (interactive)
    (let* ((file-at-point (dired-get-file-for-visit))
           (default-dir (cond
                         ;; If on a directory, use it
                         ((and file-at-point (file-directory-p file-at-point))
                          (file-name-as-directory file-at-point))
                         ;; If on a file, use its parent directory
                         (file-at-point
                          (file-name-directory file-at-point))
                         ;; Otherwise use current directory
                         (t default-directory)))
           (filename (read-file-name "Create file: " default-dir)))
      (write-region "" nil filename)
      (revert-buffer)
      (dired-goto-file filename)))
  
  (defun my/dired-create-directory-at-point ()
    "Create directory, defaulting to directory at point or parent directory of file at point."
    (interactive)
    (let* ((file-at-point (dired-get-file-for-visit))
           (default-dir (cond
                         ;; If on a directory, use it
                         ((and file-at-point (file-directory-p file-at-point))
                          (file-name-as-directory file-at-point))
                         ;; If on a file, use its parent directory
                         (file-at-point
                          (file-name-directory file-at-point))
                         ;; Otherwise use current directory
                         (t default-directory)))
           (dirname (read-directory-name "Create directory: " default-dir)))
      (make-directory dirname t)
      (revert-buffer)
      (dired-goto-file dirname)))
  
  ;; Bindings
  (define-key dired-mode-map (kbd "c") nil)
  (define-key dired-mode-map (kbd "c f") #'my/dired-create-file-at-point)
  (define-key dired-mode-map (kbd "c d") #'my/dired-create-directory-at-point))

  (use-package dired-subtree
    :after dired
    :bind
    ( :map dired-mode-map
      ("<tab>" . dired-subtree-toggle)
      ("TAB" . dired-subtree-toggle)
      ("<backtab>" . dired-subtree-remove)
      ("S-TAB" . dired-subtree-remove))
    :config
    (setq dired-subtree-use-backgrounds nil))



(use-package outline
  :ensure nil
  :commands outline-minor-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
   ;; folds more visually distinctive and readable.
   (outline-minor-mode
    .
    (lambda()
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1)) ;; This enables automatic mode-line display and project-based activation


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp)
         (before-save . lsp-format-buffer))
  :commands lsp
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)  ;; Add this line
  (setq lsp-semantic-tokens-apply-modifiers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-semantic-tokens-enable t)  ; Enable semantic tokens
  (setq lsp-enable-semantic-highlighting t))

(setq-default tab-width 2)
(setq c-basic-offset 4)  ; 2-space indent for C/C++

(defun my/indent-right ()
  "Indent region by tab-width and keep it selected."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) tab-width)
  (setq deactivate-mark nil))

(defun my/indent-left ()
  "Unindent region by tab-width and keep it selected."
  (interactive)
  (indent-rigidly (region-beginning) (region-end) (- tab-width))
  (setq deactivate-mark nil))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(with-eval-after-load 'eglot
  ;; Format on save for C/C++ files
  (add-hook 'c-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

;; Orderless - fuzzy matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult - enhanced commands with previews
(use-package consult
  :ensure t
  :config
  ;; Preview configuration
  (setq consult-preview-key 'any))  ; Preview as you type
  
;; Better project integration
(use-package consult-projectile
  :ensure t
  :after (consult projectile))

(use-package embrace
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package meow
  :ensure t
  :config
  (defun my/meow-setup ()
    ;; Choose a keyboard layout - pick one:
    ;; QWERTY layout
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("M" . embrace-commander)
     '("n" . meow-search)
     '("o" . er/expand-region)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("P" . yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("}" . end-of-line)
     '("{" . beginning-of-line)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("<" . my/indent-left)
     '(">" . my/indent-right)
     '("y" . meow-save)
     '("Y" . kill-ring-save)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  
  (my/meow-setup)
  (meow-global-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'auto)
  (setq projectile-enable-caching t))

(defun my/toggle-eshell ()
  "Toggle eshell and focus."
  (interactive)
  (let ((win (get-buffer-window "*eshell*")))
    (if win
        (if (eq (selected-window) win)
            (delete-window win)  ; If focused, close it
          (select-window win))   ; If not focused, focus it
      (eshell))))  ; Open if doesn't exist

(defun my/toggle-side-windows ()
  "Toggle all side windows. If opening, focus most recent."
  (interactive)
  (if (window-with-parameter 'window-side)
      ;; Side windows exist, close them
      (window-toggle-side-windows)
    ;; No side windows, open them and focus
    (window-toggle-side-windows)
    ;; Focus the first side window we find
    (when-let ((side-win (window-with-parameter 'window-side)))
      (select-window side-win))))


(with-eval-after-load 'meow
  (meow-leader-define-key
   '("p" . projectile-command-map)
   '("f" . find-file)
   '("F" . find-file-other-window)
   '("s" . save-buffer)
   '("/" . consult-ripgrep)
   '("?" . consult-bookmark)
   '("b" . consult-buffer)
   '("B" . consult-buffer-other-window)
   '("S" . projectile-save-project-buffers)
   '("a" . projectile-find-other-file)
   '("A" . projectile-find-other-file-other-window)
   '("k" . kill-buffer)
   '("o" . my/toggle-eshell)
   '("d" . xref-find-definitions)
   '("D" . projectile-find-references)
   '("r" . lsp-rename)
   '("u" . lsp-format-buffer)
   '("E" . query-replace-regexp)
   '("e" . replace-regexp)
   '(";" . comment-or-uncomment-region)
   '("<right>" . next-window-any-frame)
   '("<left>" . previous-window-any-frame)
   '("'" . my/toggle-side-windows)
   '("Y" . org-agenda)
   '("y" . org-capture)
   '("v" . consult-line)
   '("J" . avy-goto-line)
   '("j" . avy-goto-char-2)
   '("-" . outline-hide-subtree)
   '("=" . outline-show-subtree)
   '("]" . end-of-line)
   '("[" . beginning-of-line)
   '("{" . beginning-of-buffer)
   '("}" . end-of-buffer)
   '("RET" . org-open-file)
    '("w" . other-window)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 8)
  (setq vertico-cycle t)
  (define-key vertico-map (kbd "M-j") #'vertico-next)
  (define-key vertico-map (kbd "M-k") #'vertico-previous)
  (require 'vertico-directory)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "C-w") #'backward-kill-word)
  
  ;; RET enters directories
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter))


(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-width 30)
  (setq treemacs-follow-mode t)  ; Auto-follow current file
  (setq treemacs-filewatch-mode t)  ; Auto-refresh on file changes
  
  ;; Keybindings in treemacs buffer
  (define-key treemacs-mode-map (kbd "h") #'treemacs-root-up)
  (define-key treemacs-mode-map (kbd "l") #'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "j") #'treemacs-next-line)
  (define-key treemacs-mode-map (kbd "k") #'treemacs-previous-line))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(global-set-key (kbd "C-c T") #'treemacs)

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; In treemacs config:
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "l") #'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "a") #'treemacs-visit-node-ace))



(use-package hide-mode-line
  :ensure t)

(add-to-list 'display-buffer-alist
             '("\\*eshell\\*"
               (display-buffer-in-side-window)
               (window-height . 0.33)
               (side . bottom)
               (slot . 0)))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-in-side-window)
               (window-height . 0.33)
               (side . bottom)
               (slot . 1)))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (side . bottom)
               (slot . 0)))

(add-to-list 'display-buffer-alist
             '("\\*Messages\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (side . bottom)
               (slot . 0)))

(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (side . bottom)
               (slot . 0)))


;; Hide mode-line for these buffers
(add-hook 'eshell-mode-hook #'hide-mode-line-mode)
(add-hook 'compilation-mode-hook #'hide-mode-line-mode)
(add-hook 'help-mode-hook #'hide-mode-line-mode)


(with-eval-after-load 'meow
  (meow-leader-define-key
   '("'" . my/toggle-side-windows)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  ;; Auto-popup settings
  (setq company-idle-delay 0.115)
  (setq company-minimum-prefix-length 1)

  ;; Other useful settings
  (setq company-selection-wrap-around t)
  (setq company-require-match nil)
  (setq company-tooltip-align-annotations t)

  ;; Prioritize capf (what lsp-mode hooks into)
  (setq company-backends '((company-capf company-dabbrev-code) company-dabbrev))

  ;; Don't use company in eshell
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

  :bind (:map company-active-map
         ("TAB"       . company-select-next)
         ("<tab>"     . company-select-next)
         ("S-TAB"     . company-select-previous)
         ("<backtab>" . company-select-previous)
         ("RET"       . company-complete-selection)
         ("<return>"  . company-complete-selection)))

;;org mode search for todos
(use-package org-ql
  :ensure t)

(defun my/extract-code-todos ()
  "Extract TODO comments from current project and display them."
  (interactive)
  (if (not (project-current))
      (message "Not in a project!")
    (let* ((project-root (project-root (project-current t)))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (output-file (expand-file-name 
                        (format "%s-todos.org" project-name)
                        org-directory))
           (files (project-files (project-current t)))
           (todos '())
           (file-count 0)
           (matched-count 0))
      
      (message "Scanning %d files in %s..." (length files) project-root)
      
      ;; Search through files for TODO comments
      (dolist (file files)
        (let ((is-c    (string-match-p "\\.[ch]\\(pp\\)?$" file))
              (is-py   (string-match-p "\\.py$" file)))
          (when (or is-c is-py)
            (setq file-count (1+ file-count))
            (with-temp-buffer
              (insert-file-contents (expand-file-name file project-root))
              (goto-char (point-min))
              (let ((todo-regex
                     (if is-py
                         "#\\s-*TODO(\\([^)]+\\)):\\s-*\\(.+\\)$"
                       "//\\s-*TODO(\\([^)]+\\)):\\s-*\\(.+\\)$")))
                (while (re-search-forward todo-regex nil t)
                  (setq matched-count (1+ matched-count))
                  (let ((person (match-string 1))
                        (task (match-string 2))
                        (line (line-number-at-pos))
                        (file-path (expand-file-name file project-root)))
                    (push (list person task file-path line) todos))))))))
      
      (message "Searched %d C/C++/Python files, found %d TODOs" file-count matched-count)
      
      ;; Display results
      (if todos
          (progn
            (my/save-todos-to-file todos output-file project-name)
            (my/display-todos-buffer todos)
            (message "TODOs saved to %s" output-file))
        (message "No TODOs found.")))))

(defun my/save-todos-to-file (todos filename project-name)
  "Save TODOs to org file, only replacing the 'Scanned TODOs' section."
  (if (file-exists-p filename)
      ;; File exists - update only the scanned section
      (with-current-buffer (find-file-noselect filename)
        (goto-char (point-min))
        
        ;; Find the "Scanned TODOs" section
        (if (re-search-forward "^\\* Scanned TODOs" nil t)
            (progn
              ;; Delete the entire Scanned TODOs section
              (org-mark-subtree)
              (delete-region (region-beginning) (region-end)))
          ;; If section doesn't exist, go to end
          (goto-char (point-max))
          (insert "\n"))
        
        ;; Insert fresh scanned TODOs
        (insert "* Scanned TODOs\n")
        (insert (format "  :PROPERTIES:\n  :UPDATED: %s\n  :END:\n\n"
                       (format-time-string "%Y-%m-%d %H:%M")))
        
        (let ((grouped (seq-group-by #'car todos)))
          (dolist (group grouped)
            (let ((person (car group))
                  (tasks (cdr group)))
              (insert (format "** Tasks for %s\n" person))  ; No TODO here!
              (dolist (task tasks)
                (let ((desc (nth 1 task))
                      (file (nth 2 task))
                      (line (nth 3 task)))
                  (insert (format "*** TODO %s\n" desc))
                  (insert (format "    [[file:%s::%d][%s:%d]]\n\n" 
                                file line 
                                (file-name-nondirectory file) line)))))))
        
        (save-buffer))
    
    ;; File doesn't exist - create fresh
    (with-temp-file filename
      (insert (format "#+TITLE: %s TODOs\n" project-name))
      (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert "* Scanned TODOs\n")
      (insert (format "  :PROPERTIES:\n  :UPDATED: %s\n  :END:\n\n"
                     (format-time-string "%Y-%m-%d %H:%M")))
      
      (let ((grouped (seq-group-by #'car todos)))
        (dolist (group grouped)
          (let ((person (car group))
                (tasks (cdr group)))
            (insert (format "** Tasks for %s\n" person))  ; No TODO!
            (dolist (task tasks)
              (let ((desc (nth 1 task))
                    (file (nth 2 task))
                    (line (nth 3 task)))
                (insert (format "*** TODO %s\n" desc))
                (insert (format "    [[file:%s::%d][%s:%d]]\n\n" 
                              file line 
                              (file-name-nondirectory file) line))))))))))

(defun my/display-todos-buffer (todos)
  "Display TODOs in a temporary buffer for quick viewing."
  (let ((buf (get-buffer-create "*Project TODOs*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (org-mode)
      
      ;; Group by person
      (let ((grouped (seq-group-by #'car todos)))
        (dolist (group grouped)
          (let ((person (car group))
                (tasks (cdr group)))
            (insert (format "* TODO Tasks for %s\n" person))
            (dolist (task tasks)
              (let ((desc (nth 1 task))
                    (file (nth 2 task))
                    (line (nth 3 task)))
                (insert (format "** TODO %s\n" desc))
                (insert (format "   [[file:%s::%d][%s:%d]]\n\n" 
                              file line 
                              (file-name-nondirectory file) line)))))))
      
      (goto-char (point-min))
      (read-only-mode 1))
    (switch-to-buffer buf)))

;; Bind it
(with-eval-after-load 'meow
  (meow-leader-define-key
   '("t" . my/extract-code-todos)))

;; Add RET binding directly in org-mode
(with-eval-after-load 'org
  (defun my/org-return-current-window ()
    "Follow link in current window if on link, otherwise normal return."
    (interactive)
    (if (org-in-regexp org-link-any-re)
        (let ((org-link-frame-setup 
               (cons '(file . find-file) org-link-frame-setup)))
          (org-open-at-point))
      (org-return)))
  
  (defun my/org-return-other-window ()
    "Follow link in other window if on link, otherwise normal return."
    (interactive)
    (if (org-in-regexp org-link-any-re)
        (org-open-at-point)  ; Default behavior opens in other window
      (org-return)))
  
  ;; Bind them
  (define-key org-mode-map (kbd "RET") #'my/org-return-current-window)
  (define-key org-mode-map (kbd "M-RET") #'my/org-return-other-window))

;; Set font on Windows
(when (eq system-type 'windows-nt)
  (add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-face-attribute 'default nil :font "Cascadia Mono" :height 100)))))

