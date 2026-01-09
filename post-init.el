(setq-default explicit-shell-file-name "/bin/bash")

;; --- Platform Specific Configuration ---
(cond
 ;; macOS Settings
 ((eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

 ;; Linux Settings
 ((eq system-type 'gnu/linux)
  ;; Usually path is fine, but you can add local bins here
  (add-to-list 'exec-path "~/.local/bin"))

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
  
  ;; Agenda files
  (setq org-agenda-files '("~/org"))
  
  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))
  


(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Two-panel startup with balanced windows
(add-hook 'emacs-startup-hook
          (lambda ()
            (split-window-right)
            (balance-windows)))

(electric-pair-mode 1)

(defun post-init ()
  "open post init.el in .minimal-config"
  (interactive)
  (find-file "~/.minimal-emacs.d/post-init.el"))

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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)

  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)      ; Tree-sitter C mode
   (c++-ts-mode . eglot-ensure))

  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-ts-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--header-insertion=iwyu"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--function-arg-placeholders"
                    "--fallback-style=Microsoft"))))

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
     '("n" . meow-search)
     '("o" . meow-block)
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
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("y" . meow-save)
     '("Y" . kill-ring-save)
     '("z" . meow-pop-selection)
     '("X" . (lambda () (interactive)
               (meow--direction-backward)
               (meow--cancel-selection)
               (set-mark (point))
               (back-to-indentation)
               (meow--select)))
     '("C" . (lambda () (interactive)
               (meow--direction-forward)
               (meow--cancel-selection)
               (set-mark (point))
               (end-of-line)
               (skip-syntax-backward " ")
               (meow--select)))
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
   '("r" . eglot-rename)
   '("u" . eglot-format-buffer)
   '("E" . query-replace-regexp)
   '("e" . replace-regexp)
   '(";" . comment-or-uncomment-region)
   '("<right>" . next-window-any-frame)
   '("<left>" . previous-window-any-frame)
   '("'" . my/toggle-side-windows)
   '("y" . org-agenda)
   '("Y" . org-capture)
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

;; (use-package popper
;;   :ensure t
;;   :bind (("C-`" . popper-toggle)
;;          ("M-`" . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*eshell\\*"
;;           "\\*shell\\*"
;;           "\\*term\\*"
;;           "\\*vterm\\*"
;;           help-mode
;;           compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1)
;;   
;;   :config
;;   (setq popper-display-control t)
;;   (setq popper-display-function #'display-buffer-at-bottom)
;;   (setq popper-window-height 0.33)
;;   
;;   ;; Fix for redisplay issues
;;   (setq popper-group-function nil)  ; Disable grouping which can cause issues
;;   
;;   ;; Force redisplay after toggling
;;   (advice-add 'popper-toggle :after 
;;               (lambda (&rest _) 
;;                 (redisplay t))))
;; 
;; ;; Add to meow leader for easy access
;; (with-eval-after-load 'meow
;;   (meow-leader-define-key
;;    '("'" . popper-toggle)))  ; SPC ` to toggle popup

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
               (side . right)
               (slot . 0)))

(add-to-list 'display-buffer-alist
             '("\\*Messages\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (side . right)
               (slot . 0)))

(add-to-list 'display-buffer-alist
             '("\\*Warnings\\*"
               (display-buffer-in-side-window)
               (window-height . 0.4)
               (side . right)
               (slot . 0)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Project TODOs\\*"
;;                (display-buffer-in-side-window)
;;                (window-height . 0.33)
;;                (side . bottom)
;;                (slot . 0)))

;; Hide mode-line for these buffers
(add-hook 'eshell-mode-hook #'hide-mode-line-mode)
(add-hook 'compilation-mode-hook #'hide-mode-line-mode)
(add-hook 'help-mode-hook #'hide-mode-line-mode)


(with-eval-after-load 'meow
  (meow-leader-define-key
   '("'" . my/toggle-side-windows)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  ;; Auto-popup settings
  (setq corfu-auto t)                 ; Enable auto completion
  (setq corfu-auto-delay 0.115)         ; Delay before popup (in seconds)
  (setq corfu-auto-prefix 1)          ; Minimum prefix length to trigger
  
  ;; Other useful settings
  (setq corfu-cycle t)                ; Enable cycling through candidates
  (setq corfu-quit-no-match 'separator) ; Don't quit if no match
  (setq corfu-preview-current nil)    ; Don't preview current candidate
  (setq corfu-preselect 'prompt)      ; Preselect the prompt
  (setq corfu-on-exact-match nil)     ; Don't auto-insert exact matches
  
  ;; Keybindings
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "<tab>") #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map (kbd "<backtab>") #'corfu-previous)
  (define-key corfu-map (kbd "RET") #'corfu-insert))

(add-hook 'eshell-mode-hook
          (lambda ()
            (corfu-mode -1)))


;;org mode search for todos
(use-package org-ql
  :ensure t)

(defun my/extract-code-todos ()
  "Extract TODO comments from current project and display them."
  (interactive)
  (if (not (project-current))
      (message "Not in a project!")
    (let* ((project-root (project-root (project-current t)))
           (files (project-files (project-current t)))
           (todos '())
           (file-count 0)
           (matched-count 0))
      
      (message "Searching %d files in %s..." (length files) project-root)
      
      ;; Search through files for TODO comments
      (dolist (file files)
        (when (string-match-p "\\.[ch]\\(pp\\)?$" file)  ; Fixed regex
          (setq file-count (1+ file-count))
          (with-temp-buffer
            (insert-file-contents (expand-file-name file project-root))
            (goto-char (point-min))
            ;; Match TODO(name): comment
            (while (re-search-forward "//\\s-*TODO(\\([^)]+\\)):\\s-*\\(.+\\)$" nil t)
              (setq matched-count (1+ matched-count))
              (let ((person (match-string 1))
                    (task (match-string 2))
                    (line (line-number-at-pos))
                    (file-path (expand-file-name file project-root)))
                (push (list person task file-path line) todos))))))
      
      (message "Searched %d C/C++ files, found %d TODOs" file-count matched-count)
      
      ;; Display results
      (if todos
          (my/display-todos todos)
        (message "No TODOs found. Make sure comments are formatted as: // TODO(name): description")))))

(defun my/display-todos (todos)
  "Display TODO list grouped by person."
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
  ;; Default RET opens in other window
  (define-key org-mode-map (kbd "M-<return>") #'org-open-at-point)
  
  ;; 'o' opens in current window
  (define-key org-mode-map (kbd "RET") 
    (lambda () 
      (interactive)
      (let ((org-link-frame-setup 
             (cons '(file . find-file) org-link-frame-setup)))
        (org-open-at-point)))))
