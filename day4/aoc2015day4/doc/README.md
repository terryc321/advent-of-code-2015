
# docs

# packages 

for ghci you get enlighten it to realise you need to load extern packages 

```
.ghci 

:set -package text
:set -package bytestring
:set -package containers
```

# stack 

currently we are using stack exclusively 

```
package.yaml 

dependencies 
- text 
- bytestring
- containers

executable 
- text
- bytestring
- containers 

```

# using emacs

do any setup in emacs required 

need to constantly clear the lsp-workspace otherwise it will pull in files from other random folders. 
remove any folders not directly in the current project

```
M-x lsp-describe-session 

M-x -w-f-r-  lsp-workspace-folder-remove 
```

helpful emacs 

```
C-c C-r   reload project
C-c C-k   clear repl window 
C-c C-c   compile the file 
```


# setup 

start project fresh
```
stack new aoc2015day4
cd aoc2015day4
```

we create a dirs local file for emacs - probably doesnt do anythign

```
.dirs.local.el

((haskell-mode
  (haskell-program-name . "stack")
  (haskell-stack-program-name . "stack")
  (haskell-ghci-program-name . "stack ghci")
  (haskell-process-command . "stack repl")))

```

now complete emacs config 

```
;; ============ grok emacs haskell setup =====================
;; ========================================
;;  Complete Haskell + Stack + HLS + Tree-sitter
;; ========================================

;; 1. Base Haskell mode
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-args-stack-ghci '("--ghci-options=-fshow-loaded-modules")))

;; 2. Tree-sitter Core
(use-package treesit
  :ensure nil
  :preface
  (setq treesit-language-source-alist
        '((haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "master" "src"))
          (cabal   . ("https://github.com/tree-sitter/tree-sitter-cabal"   "master" "src"))))
  :config
  (setq treesit-font-lock-level 4)        ; Max detail
  (setq treesit-max-buffer-size 50000000)

  ;; Auto-install grammar
  (unless (treesit-language-available-p 'haskell)
    (message "Installing Haskell Tree-sitter grammar...")
    (treesit-install-language-grammar 'haskell))

  ;; Remap modes
  (add-to-list 'major-mode-remap-alist '(haskell-mode . haskell-ts-mode))
  (add-to-list 'major-mode-remap-alist '(haskell-cabal-mode . haskell-cabal-ts-mode)))

;; 3. Haskell Tree-sitter Mode
(use-package haskell-ts-mode
  :ensure t
  :after (haskell-mode treesit)
  :hook ((haskell-ts-mode . lsp-deferred)
         (haskell-ts-mode . interactive-haskell-mode)
         (haskell-ts-mode . haskell-indentation-mode)
         (haskell-ts-mode . (lambda () (electric-indent-mode -1))))
  :config
  (setq haskell-ts-mode-font-lock-feature-list
        '((comment definition keyword string type)
          (builtin constant function operator variable)
          (bracket delimiter error))))

;; 4. LSP + Haskell Language Server
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (haskell-ts-mode . lsp-deferred)
  :config
  (setq lsp-idle-delay 0.5)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-completion-provider :capf))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))

(use-package lsp-haskell
  :ensure t
  ;; :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  ;; (setq lsp-haskell-server-command "haskell-language-server"
  ;;       lsp-haskell-server-args '("--lsp"))
  (setq lsp-haskell-plugin-imports-global-on t)
  (setq lsp-haskell-plugin-hlint-global-on t)
  (setq lsp-haskell-plugin-eval-global-on t)
  (setq lsp-haskell-formatting-provider "fourmolu"))

;; 5. Quality of Life
(use-package company
  :ensure t
  :config (global-company-mode 1))

(use-package which-key
  :ensure t
  :config (which-key-mode))

```
