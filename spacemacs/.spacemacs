;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; Emacs general
     ibuffer

     ;; Org mode
     (org :variables

          org-enable-roam-support t          ;; Enable org-roam v2
          org-enable-roam-ui t               ;; Enable org-roam-ui for graph visualization
          org-enable-roam-protocol t         ;; Enable org-protocol for external capture
          org-enable-hugo-support t
          org-enable-github-support t

          ;; Org contact
          org-enable-org-contacts-support t   ;; Contact support
          org-contacts-files '("~/workspace/second-brain/org-roam/20250422172405-contacts.org")


          ;; Visual
          org-enable-appear-support t         ;; It toggles visibility of some markers
          org-hide-emphasis-markers t
          org-enable-modern-support t         ;; Enable modern visual enhancements

          ;; Agenda notifications
          org-enable-notifications t
          org-start-notification-daemon-on-startup t

          )

     ;; Typing related
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-return-key-behavior 'complete ;; complete or nil
                      auto-completion-tab-key-behavior 'cycle       ;; complete, cycle, or nil
                      auto-completion-enable-sort-by-usage t
                      )
     (spell-checking :variables
                     spell-checking-enable-auto-dictionary t
                     =enable-flyspell-auto-completion= t
                     )

     syntax-checking

     ;; Search related
     helm

     ;; Development
     git
     (dap :variables
          dap-auto-configure-mode t)
     ;; eglot

     ;; Language support
     gpu ;; Support for GPU related languages like CUDA, OpenCL and various Shader formats
     (lsp :variables
          lsp-rust-server 'rust-analyzer
          lsp-ui-doc-enable t
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-code-actions t)
     emacs-lisp
     (python :variables
             python-backend 'lsp
             python-formatter 'lsp ;; 'black, 'yapf or 'lsp
             python-format-on-save nil ;; t / nil
             python-sort-imports-on-save nil ;; t / nil
             python-test-runner 'pytest)
     (ipython-notebook :variables
                       ein-backend 'lsp
                       python-formatter 'lsp
                       python-sort-imports-on-save nil
                       python-format-on-save t
                       )
     (rust :variables
           rust-backend 'lsp
           lsp-rust-analyzer-cargo-auto-reload t
           lsp-rust-analyzer-proc-macro-enable t
           rustic-format-on-save nil) ;; t / nil
     (c-c++ :variables
            c-c++-adopt-subprojects t
            c-c++-enable-organize-includes-on-save t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c-c++-dap-adapters '(dap-lldb dap-cpptools dap-gdb)
            c-c++-enable-clang-format-on-save t
            c-c++-backend 'lsp-clangd)
     (go :variables
         go-backend 'lsp
         go-tab-width 4
         go-format-before-save nil    ;; Disable layer formatting; Apheleia handles this
         go-use-golangci-lint t
         go-test-args "-v -race"
         godoc-at-point t)
     (cmake :variables
            cmake-backend 'lsp
            cmake-enable-cmake-ide-support t)
     markdown
     (json :variables
           json-fmt-on-save t)
     latex
     markdown
     systemd
     yaml
     (sql  :variables
           sql-backend 'lsp
           sql-capitalize-keywords t
           lsp-sqls-workspace-config-path 'workspace)
     my-ejc-sql  ;; Needed for snowflake
     ansible
     html
     json
     toml
     javascript
     (docker :variables
             docker-dockerfile-backend 'lsp
             )
     (terraform :variables
                terraform-auto-format-on-save t
                terraform-backend 'lsp)
     kubernetes

     ;; RSS
     (elfeed :variables
             rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org")
             )

     ;; IRC
     (rcirc :variables
            rcirc-enable-authinfo-support t
            rcirc-enable-erc-image t
            rcirc-enable-styles t
            rcirc-enable-late-fix t
            )

     ;; LLMs
     (llm-client :variables
                 llm-client-enable-gptel t
                 llm-client-enable-ellama t
                 llm-client-preferred-backends '(ollama))

     ;; Other
     (tree-sitter :variables
                  spacemacs-tree-sitter-hl-black-list '(js2-mode rjsx-mode)
                  tree-sitter-syntax-highlight-enable t
                  tree-sitter-fold-enable t
                  tree-sitter-indent-enable t
                  tree-sitter-fold-indicators-enable nil)
     better-defaults
     neotree
     treemacs
     hackernews
     (shell :variables
            shell-default-height 30
            shell-default-term-shelvl "/bin/zsh"
            shell-default-position 'bottom)
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     ;; Aesthetics
     gruvbox-theme
     beacon

     ;; Org-mode related
     org-sidebar
     org-super-agenda
     org-bullets  ;; Show org-mode bullets as UTF-8 characters
     ox-hugo
     org-kanban
     ;; org-roam-dblocks
     org-roam-ql
     org-transclusion
     org-edna

     ;; Chat
     ement

     ;; Version control
     ;; conventional-commit

     ;; llms
     aidermacs ;; vibe coding

     ;; Language support
     just-mode
     ;; ejc-sql

     ;; LSP and LSP-addons
     apheleia
     ;; flycheck-ruff

     ;; Game engine
     gdscript-mode

     ;; Writing
     writegood-mode

     ;; Multiple cursor
     multiple-cursors

     ;; smart-mode-line

     ;; Transform buffer layouts
     transpose-frame

     ;; Fun
     snow
     fireplace
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   ;;dotspacemacs-install-packages 'all))
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   ;; dotspacemacs-editing-style 'emacs
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox-dark-medium
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-mode-line-theme '(spacemacs :separator arrow :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   ;; dotspacemacs-line-numbers nil
   dotspacemacs-line-numbers t
   dotspacemacs-line-numbers '(
                               :disabled-for-modes dired-mode
                               doc-view-mode
                               markdown-mode
                               org-mode
                               pdf-view-mode
                               text-mode
                               :size-limit-kb 1000)

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   ;; dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep") ;; "pt" failed on ubutnu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; ============================================================================
  ;; Magit
  ;; ============================================================================
  (setq-default git-magit-status-fullscreen t)
  (setq magit-repository-directories '("~/workspace/"))
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; ============================================================================
  ;; Load private secrets from ~/.emacs.d/private/private.el
  ;; ============================================================================
  (let ((private-config (expand-file-name "private/private.el" user-emacs-directory)))
    (when (file-exists-p private-config)
      (load-file private-config)))

  ;; Test to fix dap
  ;; (setq package-user-dir (concat default-directory "configuration/elpa"))



  ;; ============================================================================
  ;; Fixes
  ;; ============================================================================
  ;; Fix for helm-descbinds-mode breaking emacs-which-key
  (helm-descbinds-mode 0)
  ;; Fix for 'Error (use-package): dap-mode/:config: Invalid image type ‘svg’'
  (add-to-list 'image-types 'svg)

  ;; ============================================================================
  ;; Visuals
  ;; ============================================================================
  ;; set default theme
  (setq-default dotspacemacs-themes '(gruvbox-dark-medium))

  ;; Line numbers
  (set-face-attribute 'line-number nil :background nil) ;; nil
  ;; (set-face-attribute 'line-number-current-line nil :background nil)

  ;; Highlight line when switching buffer
  (beacon-mode t)
  (setq-default
   beacon-blink-when-window-changes 1
   beacon-blink-when-point-moves-vertically 1
   beacon-color "#928374"
   )

  ;; ============================================================================
  ;; Godot game engine
  ;; ============================================================================
  (use-package gdscript-mode
    :hook (gdscript-mode . eglot-ensure)
    :custom (gdscript-eglot-version 3))
  (setq gdscript-godot-executable "/usr/local/bin/godot-engine")


  ;; ============================================================================
  ;; LSP
  ;; ============================================================================
  (setq rustic-rustfmt-args "+nightly")
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (with-eval-after-load 'tree-sitter-indent
    ;; Fix for void-variable error in rustic-mode
    ;; We set this to nil to prevent the crash and disable tree-sitter indent for Rust
    (setq tree-sitter-indent-rustic-scopes nil))

  ;; ============================================================================
  ;; Go (Golang) Power Config
  ;; ============================================================================

  ;; Add your Go bin directory to the Emacs execution path
  ;; This helps LSP find 'gopls' and Flycheck find 'golangci-lint'
  (add-to-list 'exec-path "~/go/bin")
  (setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))

  ;; -- Go Mode & LSP Settings --
  (use-package go-mode
    :defer t
    :hook (go-mode . (lambda ()
                       ;; Add import organization to before-save-hook, specifically for Go buffers
                       (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
    :init
    (setq godoc-at-point t)
    :config
    (setq lsp-go-use-gofumpt t)            ;; Request strict formatting from LSP
    (setq lsp-go-use-placeholders t)       ;; Enable placeholder completion
    (setq lsp-go-analyses '((fieldalignment . t)  ;; Struct alignment optimizations
                            (nilness . t)
                            (shadow . t)
                            (unusedparams . t)
                            (unusedwrite . t))))

  ;; -- Debugging (DAP) --
  (use-package dap-go
    :ensure nil ;; Included in dap-mode
    :after (dap-mode go-mode)
    :config
    (dap-go-setup)
    (dap-ui-mode 1)
    (dap-tooltip-mode 0)
    (tooltip-mode 1)
    (setq dap-auto-configure-features '(sessions locals breakpoints controls tooltip))

    (dap-register-debug-template "Go :: Run Test (At Point)"
                                 (list :type "go"
                                       :request "launch"
                                       :name "Go :: Run Test (At Point)"
                                       :mode "test"
                                       :program "${file}"
                                       :args nil
                                       :env nil)))

  ;; ============================================================================
  ;; Alphelia formatter
  ;; ============================================================================
  (use-package apheleia
    :ensure t
    :after go-mode
    :config
    ;; turn on apheleia globally
    (apheleia-global-mode +1)

    ;; -- Python --
    (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))

    ;; -- RUST --
    (setf (alist-get 'rustic-mode apheleia-mode-alist) '(rustfmt))
    (setf (alist-get 'rust-mode apheleia-mode-alist) '(rustfmt))

    (setf (alist-get 'rustfmt apheleia-formatters)
          '("rustfmt" "+nightly" "--quiet" "--emit" "stdout"))

    ;; -- Go --
    ;; Use 'gofumpt' (stricter gofmt) for formatting
    (setf (alist-get 'go-mode apheleia-mode-alist) '(gofumpt))
    (setf (alist-get 'gofumpt apheleia-formatters) '("gofumpt"))

    ;; -- TOML (Taplo) --
    (setf (alist-get 'toml-mode apheleia-mode-alist) '(taplo))
    (setf (alist-get 'taplo apheleia-formatters)
          '("taplo" "fmt" "-"))
    )

  (use-package flycheck
    :ensure t
    :config
    ;; Define the Ruff checker manually (No external package needed)
    (flycheck-define-checker python-ruff
      "A Python syntax and style checker using the ruff utility."
      ;; Command to run ruff on the current file
      :command ("ruff" "check" "--output-format=text" source-original)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message) line-end))
      :modes python-mode)

    ;; This prints a message to the *Messages* buffer when Apheleia formats
    (setq apheleia-log-only-errors nil)

    ;; Add it to the checker list
    (add-to-list 'flycheck-checkers 'python-ruff)

    ;; Force it to run after LSP (Chaining)
    (add-hook 'lsp-after-open-hook
              (lambda ()
                (when (derived-mode-p 'python-mode)
                  ;; This tells flycheck: Run LSP first, then run Ruff
                  (flycheck-add-next-checker 'lsp 'python-ruff)))))

  ;; -------------------------------------------------------
  ;; FIX: Remove conflicting Spacemacs/LSP formatters
  ;; -------------------------------------------------------
  (defun my-remove-conflicting-python-hooks ()
    "Strip standard Spacemacs hooks so Apheleia has exclusive control."
    ;; 1. Stop Spacemacs from running isort
    (remove-hook 'before-save-hook 'spacemacs//python-sort-imports t)
    ;; 2. Stop Spacemacs from triggering LSP format
    (remove-hook 'before-save-hook 'spacemacs//python-lsp-format-on-save t)
    ;; 3. Stop standard LSP-mode from triggering LSP format
    (remove-hook 'before-save-hook 'lsp-format-buffer-before-save t))
  (add-hook 'python-mode-hook 'my-remove-conflicting-python-hooks)

  ;; -------------------------------------------------------
  ;; OPTIONAL: Visual feedback to confirm Apheleia is working
  ;; -------------------------------------------------------

  ;; (use-package flycheck-ruff
  ;;   :ensure t
  ;;   :after flycheck
  ;;   :config
  ;; Configure flycheck to use ruff
  ;; (flycheck-ruff-setup)

  ;; OPTIONAL: If you have a ruff.toml file, you can point to it explicitly
  ;; (setq flycheck-ruff-config-path "/path/to/ruff.toml")
  ;; )

  ;; ============================================================================
  ;; LLMs
  ;; ============================================================================
  ;; LLM
  (use-package llm
    :ensure t
    :config
    (require 'llm-openai))

  ;;;;;;;;;;;;;;;
  ;; Aidermacs ;;
  ;;;;;;;;;;;;;;;
  ;; Make sure to configure aider to connect to LLMs
  (use-package aidermacs
    :ensure t
    :defer t
    :init
    ;; Declare the prefix and give it a name for the which-key popup.
    ;; (spacemacs/declare-prefix "$a" "aidermacs")
    (progn
      (let ((aidermacs-map (make-sparse-keymap)))
        (define-key aidermacs-map "a" 'aidermacs-add-file)
        (define-key aidermacs-map "r" 'aidermacs-run)
        (define-key aidermacs-map "t" 'aidermacs-transient-menu)
        (evil-leader/set-key "$a" aidermacs-map)

        ;; (which-key-add-keymap-based-replacements aidermacs-map "Aidermasc")
        ;; (which-key-add-keymap-based-replacements
        ;;   aidermacs-map '(nil . "Aider"))

        ))
    ;; :bind
    :config
    (progn
      ;; Explicitly tell aidermacs to use the 'aider' command.
      ;; This is usually found automatically if it's in your system's PATH.
      (setq aidermacs-chat-command "aider")
      )
    )
  (setq aidermacs-program (expand-file-name "~/.local/bin/aider"))

  ;;;;;;;;;;;
  ;; Gptel ;;
  ;;;;;;;;;;;
  (pcase my/execution-context
    ('home
     (message "GPTel: Configuring for Home (Ollama)")
     (setq gptel-backend (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :models '(deepseek-coder-v2:16b)
                           :stream t)
           gptel-model 'deepseek-coder-v2:16b
           gptel-format 'org))

    ('work
     (message "GPTel: Configuring for Work (LiteLLM)")
     (setq gptel-litellm-backend
           (gptel-make-openai "LiteLLM-Server"
             :host "aeal-0001.int.tele2.com"
             :protocol "https"
             :endpoint "/v1/chat/completions"
             :stream t
             :key my/litellm-api-key
             ;; :models '(gemini/gemini-2.5-pro))
             :models '(gemini/gemini-2.5-flash))
           )
     (setq gptel-backend gptel-litellm-backend)
     (setq gptel-format 'org)
     ))

  ;;;;;;;;;;;;
  ;; Ellama ;;
  ;;;;;;;;;;;;
  (use-package plz
    :ensure t
    :config
    (setq plz-request-backend 'url-retrieve))

  (pcase my/execution-context
    ('home
     )

    ('work
     (use-package ellama
       :ensure t
       :after (llm plz)
       :defer t
       :init
       (setopt ellama-language "English")
       (require 'llm)
       (require 'llm-ollama)
       (require 'llm-openai)
       :config

       (setq ellama-provider
             (make-llm-openai-compatible
              :chat-model "gemini/gemini-2.5-pro"
              :url "http://aeal-0001.int.tele2.com:4000"
              :key my/litellm-api-key
              ))
       )
     ))

  ;; ============================================================================
  ;; Company mode auto completion
  ;; ============================================================================
  ;; (use-package company
  ;;   :defer 0.1
  ;;   :config
  ;;   (global-company-mode t)
  ;;   (setq-default
  ;;    company-idle-delay 0.05
  ;;    company-require-match nil
  ;;    company-minimum-prefix-length 0

  ;;    ;; get only preview
  ;;    company-frontends '(company-preview-frontend)
  ;;    ;; also get a drop down
  ;;    ;; company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
  ;;    ))


  ;; (use-package terraform-mode-abbrev-table
  ;;   :hook (terraform-mode . eglot-ensure))



  ;; ============================================================================
  ;; Chat
  ;; ============================================================================

  (setq auth-sources '("~/.authinfo.gpg"))

  ;; --- RCIRC ---
  (use-package rcirc
    :defer t
    :config
    ;; Enable the channel tracking in mode-line
    (rcirc-track-minor-mode 1)

    ;; User details
    (setq rcirc-default-nick "celsuss"
          rcirc-default-user-name "celsuss"
          rcirc-default-full-name "Celsuss"
          rcirc-authinfo-file "~/.authinfo.gpg")


    ;; Server configuration with SASL password injection
    (setq rcirc-server-alist
          `(("irc.libera.chat"
             :user-name "celsuss"
             :port 6697
             :encryption tls
             :channels ("#emacs" "#spacemacs" "##llamas" "#archlinux" "#archlinux-offtopic" "#linux" "#Linuxkompis" "#archlinux-nordics" "#archlinux-testing"))
            ("stockholm.se.quakenet.org"
             :user-name "Celsuss"
             :port 6667
             :channels ("#sweclockers" "#stockholm")))))

  ;; --- Element ---
  (use-package ement
    :ensure t
    :custom
    (ement-save-sessions t)
    :config
    (defun my/ement-connect ()
      "Connect to Matrix declaratively using credentials from auth-source."
      (interactive)
      (let* ((user-id "@celsuss:matrix.org")
             (server "matrix.org")
             ;; Search .authinfo.gpg for a matching machine and login
             (auth-entry (car (auth-source-search :host server :user user-id :max 1)))
             (password (if auth-entry
                           (funcall (plist-get auth-entry :secret))
                         nil)))

        (if password
            ;; If password found, connect immediately skipping prompts
            (ement-connect :user-id user-id :password password)
          ;; Fallback: If no password found, let Ement ask you
          (message "Password not found in .authinfo.gpg! Falling back to prompts.")
          (ement-connect :user-id user-id))))

    ;; (defun my/ement-connect ()
    ;;   "Connect to Matrix using the declarative User ID."
    ;;   (interactive)
    ;;   (ement-connect :user-id "@celsus:matrix.org"))
    (spacemacs/set-leader-keys "acm" 'my/ement-connect))

  ;; ============================================================================
  ;; Org-mode Core Configuration
  ;; ============================================================================
  (use-package org
    :defer t
    :config
    ;;;; Org Agenda
    ;; Inhibit time-consuming startup processes for background agenda files.
    (setq org-agenda-inhibit-startup t)
    ;; Disable dimming of blocked tasks, which can be slow.
    (setq org-agenda-dim-blocked-tasks nil)
    ;; Skip deleted files
    (setq org-agenda-skip-unavailable-files t)

    ;; Make buffer horizontal
    (setq org-agenda-window-setup 'reorganize-frame) ;; 'reorganize-frame 'other-window 'current-window
    (setq org-agenda-restore-windows-after-quit t)
    (setq org-agenda-window-frame-fractions '(0.8 . 0.9))

    (defadvice org-agenda (around split-vertically activate)
      (let (
            (split-width-threshold 40)    ; or whatever width makes sense for you
            (split-height-threshold nil)) ; but never horizontally
        ad-do-it))

    ;; Default was: " %i %-12:c%?-12t% s" (The %c is the filename)
    (setq org-agenda-prefix-format
          '((agenda . " %i %?-12t% s")
            (todo   . " %i %?-12t% s")
            (tags   . " %i %?-12t% s")
            (search . " %i %?-12t% s")))

    ;; (setq org-agenda-files (directory-files-recursively org-directory "\\\\.org$"))
    ;; (setq org-agenda-files (directory-files-recursively "~/workspace/second-brain/" "\.org$"))
    (setq org-agenda-files '("~/workspace/second-brain/org-roam/tasks/"
                             "~/workspace/second-brain/org-roam/projects/"))
    (org-super-agenda-mode)

    ;; --- Org habits ---
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)
    (setq org-habit-graph-column 60)
    (setq org-habit-show-habits-only-for-today nil)
    ;; Allow completed habits to stay visible in the agenda log for satisfaction
    (setq org-habit-show-habits-only-for-today nil)
    (setq org-agenda-skip-scheduled-if-done nil)

    ;; Use a dedicated directory for all org files
    (setq org-directory "~/workspace/second-brain/org-roam/")

    ;; Enable advanced dependency tracking
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)

    ;; Org capture templates
    (setq org-capture-templates
          '(
            ("t" "TODO" entry (file "~/workspace/second-brain/org-roam/todo.org")
             "** TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("w" "Work Task" entry (file "~/workspace/second-brain/org-roam/work_tasks.org")
             "** TODO %? :work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("h" "Home Lab Task" entry (file+headline "~/workspace/second-brain/org-roam/homelab_tasks.org" "Tasks")
             "** TODO %? :homelab:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("e" "Emacs Tweak" entry (file "~/workspace/second-brain/org-roam/emacs_tweak_tasks.org")
             "** TODO %? :emacs:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("d" "Dotfiles Tweak" entry (file "~/workspace/second-brain/org-roam/dotfiles_tweak_tasks.org")
             "** TODO %? :dotfiles:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("c" "Curriculum Task" entry (file "~/workspace/second-brain/org-roam/curriculum_tasks.org")
             "** TODO %? :curriculum:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("l" "Link/Read Later" entry
             (file+headline "~/workspace/second-brain/org-roam/reading-list.org" "Reading List")
             "* TODO %a :reading:\nCaptured on: %U\n"
             :empty-lines 1
             :immediate-finish t)

            ;; ("i" "Inbox" entry (file "~/workspace/second-brain/org-roam/inbox.org")
            ;;  "** TODO %? :inbox:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ;; ("p" "Project Task" entry (file "~/workspace/second-brain/org-roam/projects.org")
            ;;  "* TODO %? :project:\\n  :PROPERTIES:\\n  :PROJECT: %(completing-read \\"Project: \\" (org-get-outline-path t))\\n  :CREATED: %U\\n  :END:")
            ))
    )

  ;; ============================================================================
  ;; Org-roam Configuration
  ;; ============================================================================
  (use-package org-roam
    :after org
    :custom
    ;; Set the directory for roam notes, can be the same as org-directory
    (org-roam-directory (file-truename org-directory))
    (org-roam-completion-everywhere t)

    ;; Configure the display of the backlinks buffer
    (org-roam-mode-sections
     (list #'org-roam-backlinks-section
           #'org-roam-reflinks-section
           #'org-roam-unlinked-references-section))

    ;; Configure org-roam-capture-template
    (org-roam-capture-templates '(("d" "default" plain
                                   "%?"
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: Jens Lordén\n#+date: %U\n\n* ${title}")
                                   :unnarrowed t)

                                  ("p" "project" plain
                                   "\n* TODO ${title}
One of [[id:1ae70a1c-485e-43fb-acc2-4c364510d632][my projects]].

** Goal
Describe the outcome of this project.

** Kanban Board
#+BEGIN: kanban :mirrored t
#+END:

** Tasks
*** TODO Setup Project Structure
*** TODO Define milestones
%?"

                                   :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}
#+author: Jens Lordén
#+date: %U
#+filetags: :project:${slug}:
#+SEQ_TODO: TODO STRT WAIT | DONE
#+startup: content
\n
")
                                   :unnarrowed t)


                                  ("t" "tasks" plain
                                   "%?"
                                   :if-new (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}
#+author: Jens Lordén
#+date: %U
#+SEQ_TODO: TODO STRT WAIT | DONE
#+startup: content
\n
* ${title}
\n
** Kanban board
| DONE   | WAIT | STRT | TODO |
|--------+------+------+------|
\n
* Tasks
")
                                   :unnarrowed t)


                                  ("b" "blog-post" plain
                                   "\n
One of my [[id:b0b348f1-7824-4a8c-af56-46ad9372071f][blog post]]s.

* ${title}
:properties:
:export_hugo_section: /posts/
:export_file_name:
:end:"

                                   :if-new (file+head "blog-posts/%<%Y%m%d%H%M%S>-${slug}.org"
                                                      "#+title: ${title}
#+author: Jens Lordén
#+date: %U
#+hugo_base_dir: ../hugo/
\n
")
                                   :unnarrowed t)
                                  ))

    :config
    ;; Configure org-roam-dailies
    (setq org-roam-dailies-directory "~/workspace/second-brain/org-roam/daily")

    (setq org-roam-dailies-capture-templates
          `(("d" "default" entry
             "** %<%H:%M> %?"
             ;; :target (file+head ,(expand-file-name "%<%Y-%m-%d>.org" org-roam-dailies-directory)
             :target (file+head+olp "%<%Y-%m-%d>.org"
                                    "#+title: %<%A %B %d, %Y>
#+filetags: :daily:
#+author: Jens

* Daily notes for %<%A %B %d, %Y>

* Morning Protocol
- [ ] 📅 Review Agenda (Work & Projects)
- [ ] 📧 Check email
- [ ] 🎯 Top 3 Priorities for Today
  1. [ ]
  2. [ ]
  3. [ ]
- 💤 Hours slept:

* Habits
- [ ] 💾 Commit Dotfiles/Emacs Tweaks
- [ ] 📥 Clear Inbox
- [ ] 🏋️ Workout
- [ ] 🇨🇳 Chinese Study
- [ ] 📚 Reading
- [ ] 💊 Supplements
  - [ ] ⚡ Creatine
  - [ ] 🥤 Protein
  - [ ] 🍊 Vitamins

* Nutrition
| Food  | Amount (g) | Kcal/100g | P/100g | Kcal (Tot) | P (Tot) | Meal          |
|-------+------------+-----------+--------+------------+---------+---------------|
|       |            |           |        |          0 |     0.0 | Breakfast     |
|       |            |           |        |          0 |     0.0 | Lunch         |
|       |            |           |        |          0 |     0.0 | Pre workout   |
|       |            |           |        |          0 |     0.0 | Post workout  |
|       |            |           |        |          0 |     0.0 | Dinner        |
|       |            |           |        |          0 |     0.0 | Evening snack |
|-------+------------+-----------+--------+------------+---------+---------------|
| *Total* |            |           |        |          0 |      0. |             |
#+TBLFM: $5=($2/100)*$3;%.0f::$6=($2/100)*$4;%.1f::@>$5=vsum(@I..@II)::@>$6=vsum(@I..@II)


* Log
"
                                    ;; -------------------------------------------------
                                    ;; Target: Place entries under "* Log"
                                    ;; -------------------------------------------------
                                    ("Log"))
             :empty-lines-before 1
             :empty-lines-after 1)))

    )

  (spacemacs/set-leader-keys "aordc" 'org-roam-dailies-capture-today)

  (global-set-key (kbd "C-c n f") 'org-roam-node-find)
  (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
  (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
  (global-set-key (kbd "C-c n p") 'org-roam-alias-add)
  (global-set-key (kbd "C-c n a") 'org-id)
  (global-set-key (kbd "C-c n I") 'org-id-get-create)

  ;; ============================================================================
  ;; Dynamic Blocks and Querying for MOCs
  ;; ============================================================================
  ;; (use-package org-roam-dblocks
  ;;   :after org-roam
  ;;   :config
  ;;   ;; Enable automatic updates for dynamic blocks
  ;;   (org-roam-dblocks-autoupdate-mode))

  (use-package org-roam-ql
    :after org-roam)

  ;; ============================================================================
  ;; Advanced Org Agenda
  ;; ============================================================================
  (use-package org-super-agenda
    :after org
    :config
    ;; org-agenda-dashboards
    (setq org-agenda-custom-commands
          '(("d" "🎯 Dashboard"
             ((agenda ""
                      ((org-agenda-overriding-header "✅ Agenda")
                       (org-super-agenda-groups
                        ;; This uses the main "Action Dashboard" configuration defined earlier
                        '((:name "🔥 Overdue" :deadline past :face 'error :order 1)
                          (:name "🎯 Today" :scheduled today :time-grid t :deadline today :order 2)
                          (:name "❗ Important" :priority "A" :order 3)
                          (:habit t)
                          (:name "🔧 Emacs" :tag "emacs"  :order 6)
                          (:name "🔧 Dotfiles" :tag "dotfiles" :order 7)
                          (:name "🔬 Home Lab" :tag "homelab" :order 8)
                          (:name "🔬 Curriculum" :tag "curriculum" :order 9)
                          (:name "✍️ Blog Posts" :tag "blog" :order 9)
                          (:name "🚀 Projects" :auto-property "PROJECT" :order 10)
                          (:name "🏢 Work" :tag "work" :order 11)
                          ))))
              (todo ""
                    ((org-agenda-overriding-header "✅ Dashboard")
                     (org-super-agenda-groups
                      '(
                        (:name "🔧 Emacs" :tag "emacs"  :order 1)
                        (:name "🔧️ Dotfiles" :tag "dotfiles" :order 2)
                        (:name "🔬 Home Lab" :tag "homelab" :order 3)
                        (:name "🔬 Curriculum" :tag "curriculum" :order 4)
                        (:name "🔬 Blog Posts" :tag "blog" :order 5)
                        (:name "📥 Reading list" :tag "reading" :order 8)
                        (:name "🚀 Project ideas" :tag "project" :order 9)
                        (:name "🚀 Projects" :auto-property "PROJECT" :order 10)
                        ))))))

            ("w" "🏢 Work Focus"
             ((tags-todo "work"
                         ((org-agenda-overriding-header "✅ Work Tasks")
                          (org-super-agenda-groups
                           '(
                             (:name " ⚠️ Overdue" :deadline past :face error :order 1)
                             (:name "🎯 Today" :time-grid t :scheduled today :deadline today :order 2)
                             (:name "Due Today" :deadline today :order 3)
                             (:name "Due Soon" :deadline future :order 4)
                             (:name " ⚡ Important" :priority "A" :order 5)
                             ;; Catch-all for any other work tasks
                             (:name "🚀 Other Projects & Tasks" :order 99)
                             ))))))

            ("p" "🚀 Project Dashboard"
             ((tags "project+level=1"
                    ((org-agenda-overriding-header "🚀 Projects Overview")
                     (org-super-agenda-groups
                      '(
                        (:name "🚀 In Progress"
                               :todo "STRT"
                               :order 1)

                        (:name "✨ Planning"
                               :todo "TODO"
                               :order 2)

                        (:name "⏸ On Hold"
                               :todo "WAIT"
                               :order 3)

                        (:name "✅ Finished"
                               :todo "DONE"
                               :order 4)

                        (:name "📂 Inbox / Uncategorized"
                               :order 99)
                        ))))))

            ("h" "⚡ High Speed Habits"
             ((agenda ""
                      ((org-agenda-span 'day)      ;; Show only today
                       (org-agenda-start-day nil)  ;; Start from today

                       ;; Force this view to ONLY look at your habits file
                       (org-agenda-files '("~/workspace/second-brain/org-roam/habits.org"))

                       (org-agenda-start-with-log-mode t)
                       (org-agenda-log-mode-items '(closed state))

                       ;; Visual Tweaks
                       (org-habit-graph-column 50) ;; Move graph to the right to align nicely
                       (org-agenda-overriding-header " ") ;; Remove default date header for cleanliness

                       ;; Grouping
                       (org-super-agenda-groups
                        '((:name "🚨 Critical / Overdue"
                                 :scheduled past
                                 :order 1)
                          (:name "📅 Morning Routine"
                                 :time-grid t   ;; Keep time-specific habits here
                                 :order 2)
                          (:name "✨ Daily Goals"
                                 :scheduled today
                                 :order 3)
                          (:name "✅ Completed Today"
                                 :log t
                                 :order 4)
                          ))))))
            ))
    )


  ;; --- Org-transclusion ---
  (use-package org-transclusion
    :ensure t
    :after org
    :bind (("C-c n t" . org-transclusion-add)
           ("C-c n T" . org-transclusion-mode))
    :config
    ;; Visual tweaks to make transcluded blocks look distinct in Gruvbox
    (set-face-attribute 'org-transclusion-fringe nil :foreground "#b8bb26" :background nil)
    (set-face-attribute 'org-transclusion-source-inline nil :foreground "#fabd2f" :height 0.8))

  ;; --- Org kanban ---
  (use-package org-kanban
    :ensure t
    :after org)


  ;; Org-agenda and org-super-agenda
  ;; (with-eval-after-load 'org
  ;; TODO Investigate org-roam-db-auto-sync-mode

  ;; ============================================================================
  ;; Alerts
  ;; ============================================================================
  (setq alert-default-style 'notifications)


  ;; ============================================================================
  ;; Readers
  ;; ============================================================================

  ;; --- elfeed ---
  (defun my/update-elfeed-on-save ()
    "Automatically update elfeed feeds when the org file is saved."
    (when (string= (file-name-nondirectory buffer-file-name) "elfeed.org")
      (elfeed-org)
      (message "Elfeed feeds updated from Org file.")))

  (add-hook 'after-save-hook #'my/update-elfeed-on-save)

  (use-package elfeed
    :ensure t
    :defer t
    :init
    ;; Function to toggle between your defined views
    (defun my/elfeed-set-view ()
      "Select a view from `elfeed-search-filter-alist`."
      (interactive)
      (let* ((views (mapcar #'car elfeed-search-filter-alist))
             (selected (completing-read "Select View: " views))
             (filter (alist-get selected elfeed-search-filter-alist nil nil #'string=)))
        (when filter
          (elfeed-search-set-filter filter))))

    :config
    ;; Define your views (The "Bookmark" Strategy)
    (setq elfeed-search-filter-alist
          '(("Blog posts"        . "@6-months-ago +unread +blog")
            ("News Ticker"       . "@2-weeks-ago +unread +news")
            ("FOSS"              . "@6-months-ago +unread +foss")
            ("Gaming"            . "@6-months-ago +unread +games")
            ("All Unread"        . "@6-months-ago +unread")))

    ;; Set the default view to "Deep Dive" (Blogs only) to reduce noise on startup
    (setq elfeed-search-filter "@6-months-ago +unread -news")


    ;; Update feeds immediately when Elfeed loads
    (elfeed-update)

    ;; Set a timer to update every 30 minutes (1800 seconds)
    ;; We check if the timer already exists to avoid duplicates on config reload
    (unless (and (boundp 'my/elfeed-update-timer) my/elfeed-update-timer)
      (setq my/elfeed-update-timer
            (run-at-time nil 1800 #'elfeed-update)))

    :bind
    (:map elfeed-search-mode-map
          ;; Press 'b' to open the menu and pick a view
          ("b" . my/elfeed-set-view)
          ("e" . (lambda () (interactive) (org-capture nil "l"))) ;; Add link to reading-list.org
          ;; Quick hotkeys for your most common modes
          ("J" . (lambda () (interactive) (elfeed-search-set-filter "@6-months-ago +unread -news")))
          ("N" . (lambda () (interactive) (elfeed-search-set-filter "@2-weeks-ago +unread +news"))))
    (:map elfeed-show-mode-map
          ("e" . (lambda () (interactive) (org-capture nil "l"))))

    )

  (use-package elfeed-goodies
    :ensure t
    :after elfeed
    :config
    ;; Activates the split-pane view (header line + entry pane)
    (elfeed-goodies/setup)

    ;; OPTIONS:
    ;; 'bottom (default) - Good for wide screens, reads like a digest
    ;; 'right            - Good for ultrawide screens, reads like email (Outlook style)
    (setq elfeed-goodies/entry-pane-position 'right)

    ;; The split ratio (0.5 means 50% of the screen)
    (setq elfeed-goodies/entry-pane-size 0.7)

    ;; Visual Tweaks:
    ;; Simplify the header line to save vertical space
    (setq elfeed-goodies/show-mode-line nil)
    (setq elfeed-goodies/switch-to-entry-ne-windows t))


  ;; LSP key bindings
  ;; (global-set-key (kbd "<f12>") 'lsp-goto-implementation)

  ;; Smart mode line
  ;; (sml/setup)

  ;; Spell checking
  ;; (setq-default dotspacemacs-configuration-layers '(
  ;;   (spell-checking :variables
  ;;                   spell-checking-enable-auto-dictionary t
  ;;                   =enable-flyspell-auto-completion= t
  ;;                   )))

  ;; ============================================================================
  ;; Powerline
  ;; ============================================================================
  (setq
   spaceline-version-control-p t
   spaceline-org-clock-p t
   spaceline-flycheck-error-p t
   spaceline-flycheck-warning-p t
   )

  ;; ============================================================================
  ;; Multi cursor
  ;; ============================================================================
  ;; (setq-default dotspacemacs-configuration-layers '(
  ;; (multiple-cursors :variables
  ;;      multiple-cursors-backend 'cm
  ;;  )))

  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  ;; ============================================================================
  ;; DAP debugging
  ;; ============================================================================
  (require 'dap-cpptools)
  (require 'gdscript-mode)

  (defun python/pre-init-dap-mode ()
    (when (eq python-backend 'lsp)
      (add-to-list 'spacemacs--dap-supported-modes 'python-mode))
    (add-hook 'python-mode-local-vars-hook 'spacemacs//python-setup-dap))

  (use-package dap-python
    :ensure nil  ;; dap-python is part of dap-mode, don't try to install it separately
    :after dap-mode
    :config
    ;; Use debugpy as your preferred debugger
    (setq dap-python-debugger 'debugpy)

    ;; Register a custom template that breaks on crashes (Uncaught Exceptions)
    (dap-register-debug-template "Python :: Run file (Break on Error)"
                                 (list :type "python"
                                       :args ""
                                       :cwd nil
                                       :program "${file}"
                                       :request "launch"
                                       :name "Python :: Run file (Buffer Break on Error)"
                                       ;; Options: "raised" (all exceptions), "uncaught" (crashes only)
                                       :exceptionBreakpoints '("uncaught"))))


  ;; ============================================================================
  ;; Version control
  ;; ============================================================================
  ;; This activates conventional-commit-mode in the Magit commit buffer
  ;; to help lint commit messages.
  ;; (add-hook 'git-commit-setup-hook 'conventional-commit-mode)

  ;; ============================================================================
  ;; Projectile
  ;; ============================================================================
  (setq projectile-project-search-path '("~/workspace/"))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(custom-safe-themes
     '("72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe"
       "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
   '(evil-want-Y-yank-to-eol nil)
   '(helm-completion-style 'helm-fuzzy)
   '(ispell-dictionary nil)
   '(org-agenda-files nil)
   '(package-selected-packages
     '(ac-ispell ace-jump-helm-line ace-link aggressive-indent alert auto-compile
                 auto-complete auto-highlight-symbol auto-yasnippet autothemer
                 beacon bui cargo centered-cursor-mode cfrs clean-aindent-mode
                 column-enforce-mode company company-emoji counsel counsel-gtags
                 dap-mode define-word devdocs diminish dired-quick-sort
                 doom-modeline dotenv-mode drag-stuff dumb-jump editorconfig eldoc
                 elisp-def elisp-slime-nav emoji-cheat-sheet-plus emr esh-help
                 eshell-prompt-extras eshell-z eval-sexp-fu evil-anzu evil-args
                 evil-cleverparens evil-collection evil-escape
                 evil-evilified-state evil-exchange evil-goggles evil-iedit-state
                 evil-indent-plus evil-lion evil-lisp-state evil-matchit evil-mc
                 evil-nerd-commenter evil-numbers evil-org evil-surround
                 evil-textobj-line evil-tutor evil-unimpaired
                 evil-visual-mark-mode evil-visualstar expand-region eyebrowse
                 fancy-battery flx-ido flycheck-elsa flycheck-golangci-lint
                 flycheck-package flycheck-pos-tip flycheck-rust font-lock+ fuzzy
                 ggtags gh-md gntp gnuplot go-eldoc go-fill-struct go-gen-test
                 go-guru go-impl go-mode go-rename go-tag godoctor golden-ratio
                 google-translate gruvbox-theme helm-ag helm-c-yasnippet
                 helm-company helm-descbinds helm-gtags helm-lsp helm-make
                 helm-mode-manager helm-org helm-org-rifle helm-projectile
                 helm-purpose helm-swoop helm-themes helm-xref help-fns+
                 hide-comnt highlight-indentation highlight-numbers
                 highlight-parentheses hl-todo holy-mode htmlize hungry-delete
                 hybrid-mode indent-guide info+ inspector ivy link-hint log4e
                 lorem-ipsum lsp-docker lsp-mode lsp-origami lsp-treemacs lsp-ui
                 macrostep map markdown-mode markdown-toc mmm-mode multi-line
                 multi-term multi-vterm mwim nameless neotree nhich-key
                 open-junk-file org org-category-capture org-cliplink org-contrib
                 org-download org-mime org-pomodoro org-present org-projectile
                 org-ql org-rich-yank org-roam org-roam-ui org-sidebar
                 org-super-agenda org-superstar orgit orgit-forge origami ov
                 overseer ox-hugo paradox password-generator pcre2el peg pfuture
                 popwin pos-tip project quickrun racer rainbow-delimiters request
                 restart-emacs rich-minority ron-mode rust-mode shell-pop
                 shrink-path smart-mode-line space-doc spaceline-all-the-icons
                 spacemacs-purpose-popwin spacemacs-whitespace-cleanup
                 string-edit-at-point string-inflection swiper symbol-overlay
                 symon term-cursor terminal-here toc-org toml-mode treemacs
                 treemacs-icons-dired treemacs-persp treemacs-projectile ts
                 undo-tree unfill use-package uuidgen valign vi-tilde-fringe
                 vim-powerline vmd-mode volatile-highlights vterm winum
                 writeroom-mode ws-butler xref xterm-color yasnippet
                 yasnippet-snippets))
   '(safe-local-variable-values
     '((helm-make-build-dir . "build/") (javascript-backend . tide)
       (javascript-backend . tern) (javascript-backend . lsp))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
