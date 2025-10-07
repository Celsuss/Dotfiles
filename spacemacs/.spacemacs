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
          org-mesenable-roam-protocol t      ;; Enable org-protocol for external capture
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
     lsp
     emacs-lisp
     (python :variables
             python-backend 'lsp
             python-formatter 'yapf ;; black, yapf or lsp
             python-format-on-save t
             python-sort-imports-on-save t
             python-test-runner 'pytest)
     (ipython-notebook :variables
                       ein-backend 'lsp
                       python-formatter 'lsp
                       python-sort-imports-on-save t
                       python-format-on-save t
                       )
     (rust :variables
           rust-backend 'lsp
           lsp-rust-analyzer-cargo-auto-reload t
           rustic-format-on-save t)
     (c-c++ :variables
            c-c++-adopt-subprojects t
            c-c++-enable-organize-includes-on-save t
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-lsp-enable-semantic-highlight 'rainbow
            c-c++-dap-adapters '(dap-lldb dap-cpptools dap-gdb)
            c-c++-enable-clang-format-on-save t
            c-c++-backend 'lsp-clangd)
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

     ;; llms
     aidermacs ;; vibe coding

     ;; Language support
     ;; ejc-sql

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
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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

  ;; Requires
  (require 'dap-cpptools)
  (require 'dap-python)
  (require 'gdscript-mode)


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
                           :models '(deepseek-r1:14b)
                           :stream t)
           gptel-model 'deepseek-r1:14b
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
  ;; RCIRC
  ;; ============================================================================
  (setq rcirc-default-nick "celsuss")
  (setq rcirc-default-user-name "celsuss")
  (setq rcirc-default-full-name "Celsuss")
  (setq rcirc-authinfo-file "~/.authinfo.gpg")

  (setq rcirc-server-alist
        '(("irc.libera.chat"
           :user-name "celsuss"
           :port 6697
           :encryption tls
           :channels ("#emacs" "#spacemacs" "##llamas" "#archlinux"))
          ("stockholm.se.quakenet.org"
           :user-name "Celsuss"
           :port 6667
           ;; :port 6697
           ;; :encryption tls
           :channels ("#sweclockers" "#stockholm"))))

  (add-hook 'rcirc-mode-hook
            (lambda ()
              (rcirc-track-minor-mode 1)))

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

    ;; (setq org-agenda-files (directory-files-recursively org-directory "\\\\.org$"))
    ;; (setq org-agenda-files (directory-files-recursively "~/workspace/second-brain/" "\.org$"))
    (setq org-agenda-files '("~/workspace/second-brain/org-roam/todo.org"
                             "~/workspace/second-brain/org-roam/work_tasks.org"
                             "~/workspace/second-brain/org-roam/homelab_tasks.org"
                             "~/workspace/second-brain/org-roam/emacs_tweak_tasks.org"
                             "~/workspace/second-brain/org-roam/dotfiles_tweak_tasks.org"
                             "~/workspace/second-brain/org-roam/curriculum_tasks.org"))
    (org-super-agenda-mode)

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

            ("h" "Home Lab Task" entry (file "~/workspace/second-brain/org-roam/homelab_tasks.org")
             "** TODO %? :homelab:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("e" "Emacs Tweak" entry (file "~/workspace/second-brain/org-roam/emacs_tweak_tasks.org")
             "** TODO %? :emacs:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("d" "Dotfiles Tweak" entry (file "~/workspace/second-brain/org-roam/dotfiles_tweak_tasks.org")
             "** TODO %? :dotfiles:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

            ("c" "Curriculum Task" entry (file "~/workspace/second-brain/org-roam/curriculum_tasks.org")
             "** TODO %? :curriculum:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")

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
                                   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+author: Jens\n#+date: %U")
                                   :unnarrowed t)
                                  ))
    :config
    ;; Configure org-roam-dailies
    (setq org-roam-dailies-directory "~/workspace/second-brain/org-roam/daily")

    (setq org-roam-dailies-capture-templates
          `(("d" "default" entry
             "* %<%H:%M> %?"
             :target (file+head ,(expand-file-name "%<%Y-%m-%d>.org" org-roam-dailies-directory)
                                "#+title: %<%A %B %d, %Y>
#+filetags: :daily:
#+author: Jens

* Daily notes for %<%A %B %d, %Y>
** Tasks

** Notes
  ")
             :empty-lines-before 1
             :empty-lines-after 1)))
    )

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
          '(("d" "Dashboard"
             ((agenda ""
                      ((org-agenda-overriding-header "✅ Agenda")
                       (org-super-agenda-groups
                        ;; This uses the main "Action Dashboard" configuration defined earlier
                        '((:name "🔥 Overdue" :deadline past :face 'error :order 1)
                          (:name "🎯 Today" :scheduled today :time-grid t :deadline today :order 2)
                          (:name "❗ Important" :priority "A" :order 3)
                          (:habit t)
                          (:name "🔧 Emacs" :tag "emacs"  :order 6)
                          (:name "🔬 Dotfiles" :tag "dotfiles" :order 7)
                          (:name "🔬 Home Lab" :tag "homelab" :order 8)
                          (:name "🔬 Curriculum" :tag "curriculum" :order 9)
                          (:name "🔬 Blog Posts" :tag "blog" :order 9)
                          (:name "🚀 Projects" :auto-property "PROJECT" :order 10)
                          (:name "🏢 Work" :tag "work" :order 11)
                          ))))
              (todo ""
                    ((org-agenda-overriding-header "✅ Dashboard")
                     (org-super-agenda-groups
                      '(
                        (:name "🔧 Emacs" :tag "emacs"  :order 1)
                        (:name "🔬 Dotfiles" :tag "dotfiles" :order 2)
                        (:name "🔬 Home Lab" :tag "homelab" :order 3)
                        (:name "🔬 Curriculum" :tag "curriculum" :order 4)
                        (:name "🔬 Blog Posts" :tag "blog" :order 5)
                        (:name "🚀 Project ideas" :tag "project" :order 9)
                        (:name "🚀 Projects" :auto-property "PROJECT" :order 10)
                        ))))))

            ("w" "Work Focus"
             ((tags-todo "work"
                         ((org-agenda-overriding-header "✅ Work Tasks")
                          (org-super-agenda-groups
                           '(
                             (:name "🔥 Overdue" :deadline past :face error :order 1)
                             (:name "🎯 Today" :time-grid t :scheduled today :deadline today :order 2)
                             (:name "Due Today" :deadline today :order 3)
                             (:name "Due Soon" :deadline future :order 4)
                             (:name "❗ Important" :priority "A" :order 5)
                             ;; Catch-all for any other work tasks
                             (:name "🚀 Other Projects & Tasks" :order 99)
                             ))))))
            ))
    )



  ;; Org-agenda and org-super-agenda
  ;; (with-eval-after-load 'org
  ;; TODO Investigate org-roam-db-auto-sync-mode
  ;; (require 'org-super-agenda)

  ;; )

  ;; ============================================================================
  ;; Alerts
  ;; ============================================================================
  (setq alert-default-style 'notifications)

  ;; RSS feed
  ;; (setq-default dotspacemacs-configuration-layers '(
  ;;                                                   (elfeed :variables rmh-elfeed-org-files (list "~/.emacs.d/private/elfeed.org"))
  ;;                                                   ))


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
  (defun python/pre-init-dap-mode ()
    (when (eq python-backend 'lsp)
      (add-to-list 'spacemacs--dap-supported-modes 'python-mode))
    (add-hook 'python-mode-local-vars-hook 'spacemacs//python-setup-dap))

  (setq dap-python-debugger 'debugpy)

  ;; ============================================================================
  ;; Projectile
  ;; ============================================================================
  (setq projectile-project-search-path '("~/workspace/"))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(evil-want-Y-yank-to-eol nil)
 '(helm-completion-style 'helm-fuzzy)
 '(package-selected-packages
   '(ox-hugo cargo counsel-gtags counsel swiper ivy dap-mode lsp-docker lsp-treemacs bui treemacs cfrs pfuture flycheck-rust ggtags helm-gtags racer ron-mode rust-mode toml-mode doom-modeline shrink-path smart-mode-line rich-minority org-sidebar org-ql peg ov org-super-agenda map ts flycheck-pos-tip pos-tip helm-lsp lsp-origami origami lsp-ui lsp-mode eldoc esh-help eshell-prompt-extras eshell-z multi-term multi-vterm project xref shell-pop terminal-here vterm xterm-color org-roam org-roam-ui autothemer evil-org gnuplot helm-org-rifle htmlize org-cliplink org-contrib org-download org-mime org-pomodoro alert log4e gntp org-present org-projectile org-category-capture org-rich-yank orgit-forge orgit org ac-ispell auto-complete auto-yasnippet fuzzy helm-c-yasnippet helm-company mwim neotree unfill yasnippet-snippets yasnippet beacon gruvbox-theme company-emoji company emoji-cheat-sheet-plus gh-md markdown-toc markdown-mode mmm-mode valign vmd-mode ws-butler writeroom-mode winum nhich-key volatile-highlights vim-powerline vi-tilde-fringe uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-icons-dired toc-org term-cursor symon symbol-overlay string-inflection string-edit-at-point spacemacs-whitespace-cleanup spacemacs-purpose-popwin spaceline-all-the-icons space-doc restart-emacs request rainbow-delimiters quickrun popwin pcre2el password-generator paradox overseer org-superstar open-junk-file nameless multi-line macrostep lorem-ipsum link-hint inspector info+ indent-guide hybrid-mode hungry-delete holy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org helm-mode-manager helm-make helm-descbinds helm-ag google-translate golden-ratio font-lock+ flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-evilified-state evil-escape evil-collection evil-cleverparens evil-args evil-anzu eval-sexp-fu emr elisp-slime-nav elisp-def editorconfig dumb-jump drag-stuff dotenv-mode dired-quick-sort diminish devdocs define-word column-enforce-mode clean-aindent-mode centered-cursor-mode auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line)))
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
   '(org-agenda-files
     '("~/workspace/second-brain/org-roam/20240926221840-homelab.org"
       "/home/jens/workspace/second-brain/org-roam/20240326100707-tele2_chatbot.org"
       "/home/jens/workspace/second-brain/org-roam/20240120161300-text_classification_with_transformers_using_pytorch_project.org"
       "/home/jens/workspace/second-brain/org-roam/20231122123656-tele2_ai_hub_culture_meeting.org"
       "/home/jens/workspace/second-brain/org-roam/20240227004127-dotfiles.org"
       "/home/jens/workspace/second-brain/org-roam/20230814183302-docker_compose_boilerplates.org"
       "/home/jens/workspace/second-brain/org-roam/20230815091412-tele2_one_on_one_2023_08_15.org"
       "/home/jens/workspace/second-brain/org-roam/20230815133416-tele2_data_engineering_council.org"
       "/home/jens/workspace/second-brain/org-roam/20230815143942-emacs_mu4e_package.org"
       "/home/jens/workspace/second-brain/org-roam/20230815144209-emacs_smudge_package.org"
       "/home/jens/workspace/second-brain/org-roam/20230816093800-tele2_dataiku.org"
       "/home/jens/workspace/second-brain/org-roam/20230816101401-tele2_ai_newsletter.org"
       "/home/jens/workspace/second-brain/org-roam/20230816154947-emacs_sql.org"
       "/home/jens/workspace/second-brain/org-roam/20230816155455-emacs_ejc_sql_package.org"
       "/home/jens/workspace/second-brain/org-roam/20230818130703-tele2_late_data.org"
       "/home/jens/workspace/second-brain/org-roam/20230819184143-emacs_hooks.org"
       "/home/jens/workspace/second-brain/org-roam/20230825094903-sql_platforms.org"
       "/home/jens/workspace/second-brain/org-roam/20230825104632-database.org"
       "/home/jens/workspace/second-brain/org-roam/20230825130003-spacemacs_create_layers.org"
       "/home/jens/workspace/second-brain/org-roam/20230826133001-org_super_agenda.org"
       "/home/jens/workspace/second-brain/org-roam/20230826140255-org_sidebar.org"
       "/home/jens/workspace/second-brain/org-roam/20230826140710-mu4e_dashboard.org"
       "/home/jens/workspace/second-brain/org-roam/20230826154211-snow_emacs_package.org"
       "/home/jens/workspace/second-brain/org-roam/20230826154432-music.org"
       "/home/jens/workspace/second-brain/org-roam/20230826154449-lofi_music.org"
       "/home/jens/workspace/second-brain/org-roam/20230826154931-soda.org"
       "/home/jens/workspace/second-brain/org-roam/20230826233338-i3.org"
       "/home/jens/workspace/second-brain/org-roam/20230827002918-bash_screen_resolution.org"
       "/home/jens/workspace/second-brain/org-roam/20230827183632-zsh.org"
       "/home/jens/workspace/second-brain/org-roam/20230827192303-polybar.org"
       "/home/jens/workspace/second-brain/org-roam/20230827195321-ubuntu_open_terminal.org"
       "/home/jens/workspace/second-brain/org-roam/20230827200630-my_rice.org"
       "/home/jens/workspace/second-brain/org-roam/20230827211721-window_manager.org"
       "/home/jens/workspace/second-brain/org-roam/20230827212000-terminal_emulator.org"
       "/home/jens/workspace/second-brain/org-roam/20230827212022-tilda_terminal_emulator.org"
       "/home/jens/workspace/second-brain/org-roam/20230827231910-dwm.org"
       "/home/jens/workspace/second-brain/org-roam/20230828173740-linux_nvidia_drivers.org"
       "/home/jens/workspace/second-brain/org-roam/20230828173831-linux.org"
       "/home/jens/workspace/second-brain/org-roam/20230828203812-gruvbox_theme.org"
       "/home/jens/workspace/second-brain/org-roam/20230828203824-theme.org"
       "/home/jens/workspace/second-brain/org-roam/20230830190516-xmonad.org"
       "/home/jens/workspace/second-brain/org-roam/20230906121821-python.org"
       "/home/jens/workspace/second-brain/org-roam/20230906121904-python_virtual_environment.org"
       "/home/jens/workspace/second-brain/org-roam/20230906154311-pip.org"
       "/home/jens/workspace/second-brain/org-roam/20230907093252-tele2_notes.org"
       "/home/jens/workspace/second-brain/org-roam/20230908102641-emacs_projects.org"
       "/home/jens/workspace/second-brain/org-roam/20230908234411-rust_command_line_argument.org"
       "/home/jens/workspace/second-brain/org-roam/20230911124950-tele2_models_review.org"
       "/home/jens/workspace/second-brain/org-roam/20230911202844-the_player_of_games.org"
       "/home/jens/workspace/second-brain/org-roam/20230911202925-iain_banks.org"
       "/home/jens/workspace/second-brain/org-roam/20230911203143-consider_phlebas.org"
       "/home/jens/workspace/second-brain/org-roam/20230913105015-tele2_nlp.org"
       "/home/jens/workspace/second-brain/org-roam/20230913131903-vector_database.org"
       "/home/jens/workspace/second-brain/org-roam/20230913131919-llm.org"
       "/home/jens/workspace/second-brain/org-roam/20230913131959-mongodb.org"
       "/home/jens/workspace/second-brain/org-roam/20230913133815-debugging.org"
       "/home/jens/workspace/second-brain/org-roam/20230913133855-software_engineering.org"
       "/home/jens/workspace/second-brain/org-roam/20230914100628-tele2_nba_step_3.org"
       "/home/jens/workspace/second-brain/org-roam/20230915234850-rust_home_dir.org"
       "/home/jens/workspace/second-brain/org-roam/20230915235012-rust_crate.org"
       "/home/jens/workspace/second-brain/org-roam/20230916233412-rust_json.org"
       "/home/jens/workspace/second-brain/org-roam/20230916235235-rust_result.org"
       "/home/jens/workspace/second-brain/org-roam/20230917221001-emacs_dap.org"
       "/home/jens/workspace/second-brain/org-roam/20230919140319-tele2_offer_engine.org"
       "/home/jens/workspace/second-brain/org-roam/20230921004430-game_engines.org"
       "/home/jens/workspace/second-brain/org-roam/20230921004518-bevy_game_engine.org"
       "/home/jens/workspace/second-brain/org-roam/20230922135754-you_must_put_some_deb_src_uris_in_your_sources_list.org"
       "/home/jens/workspace/second-brain/org-roam/20230927124243-tele2_ai_hub_council.org"
       "/home/jens/workspace/second-brain/org-roam/20231003151656-org_go_to_link.org"
       "/home/jens/workspace/second-brain/org-roam/20231004140201-tele2_run_b2c_nba_locally.org"
       "/home/jens/workspace/second-brain/org-roam/20231005134140-programming_problems.org"
       "/home/jens/workspace/second-brain/org-roam/20231005150633-git_change_branch_name.org"
       "/home/jens/workspace/second-brain/org-roam/20231005211418-python_pytest.org"
       "/home/jens/workspace/second-brain/org-roam/20231011103338-tele2_dataiku_office_hour.org"
       "/home/jens/workspace/second-brain/org-roam/20231013111805-vr_avatars.org"
       "/home/jens/workspace/second-brain/org-roam/20231013111813-vr.org"
       "/home/jens/workspace/second-brain/org-roam/20231013112414-tele2_dataiku_standup.org"
       "/home/jens/workspace/second-brain/org-roam/20231016193140-vr_development.org"
       "/home/jens/workspace/second-brain/org-roam/20231017143112-emacs_windows_arrangement.org"
       "/home/jens/workspace/second-brain/org-roam/20231017180459-python_install_local_package.org"
       "/home/jens/workspace/second-brain/org-roam/20231017185850-clean_humidifier.org"
       "/home/jens/workspace/second-brain/org-roam/20231023103708-dbt_custom_generic_tests.org"
       "/home/jens/workspace/second-brain/org-roam/20231023104144-dbt_run.org"
       "/home/jens/workspace/second-brain/org-roam/20231023155727-tele2_code_of_conduct.org"
       "/home/jens/workspace/second-brain/org-roam/20231025133441-ndsml_summit.org"
       "/home/jens/workspace/second-brain/org-roam/20231025133457-converence.org"
       "/home/jens/workspace/second-brain/org-roam/20231029064653-spacemacs_python_environment.org"
       "/home/jens/workspace/second-brain/org-roam/20231030131815-tele2_consolidation_of_feature_stores.org"
       "/home/jens/workspace/second-brain/org-roam/20231031143633-immersed_vr.org"
       "/home/jens/workspace/second-brain/org-roam/20231031143647-immersed_vr_linux.org"
       "/home/jens/workspace/second-brain/org-roam/20231103131024-ubuntu_version.org"
       "/home/jens/workspace/second-brain/org-roam/20231103133510-deep_learning_frameworks.org"
       "/home/jens/workspace/second-brain/org-roam/20231103133743-tensorflow.org"
       "/home/jens/workspace/second-brain/org-roam/20231103133826-pytorch.org"
       "/home/jens/workspace/second-brain/org-roam/20231103141445-self_hosting.org"
       "/home/jens/workspace/second-brain/org-roam/20231103141936-backstage.org"
       "/home/jens/workspace/second-brain/org-roam/20231103142706-immic.org"
       "/home/jens/workspace/second-brain/org-roam/20231103144102-computer_science.org"
       "/home/jens/workspace/second-brain/org-roam/20231106135315-pi_hole.org"
       "/home/jens/workspace/second-brain/org-roam/20231106140137-authelia.org"
       "/home/jens/workspace/second-brain/org-roam/20231106140924-minio.org"
       "/home/jens/workspace/second-brain/org-roam/20231106141030-nextcloud.org"
       "/home/jens/workspace/second-brain/org-roam/20231106141141-homer_dashboard.org"
       "/home/jens/workspace/second-brain/org-roam/20231106142125-home_assistant.org"
       "/home/jens/workspace/second-brain/org-roam/20231106150943-python_pip.org"
       "/home/jens/workspace/second-brain/org-roam/20231106171932-emacs_python.org"
       "/home/jens/workspace/second-brain/org-roam/20231106235935-anaconda.org"
       "/home/jens/workspace/second-brain/org-roam/20231109144323-emacs_search_files.org"
       "/home/jens/workspace/second-brain/org-roam/20231111121230-python_print.org"
       "/home/jens/workspace/second-brain/org-roam/20231111140030-data_normalize.org"
       "/home/jens/workspace/second-brain/org-roam/20231111140053-data_processing.org"
       "/home/jens/workspace/second-brain/org-roam/20231113100309-tele2_dbt_cheatsheet.org"
       "/home/jens/workspace/second-brain/org-roam/20231114135233-graph_neural_network.org"
       "/home/jens/workspace/second-brain/org-roam/20231115095605-tele2_airflow_runs.org"
       "/home/jens/workspace/second-brain/org-roam/20231115104343-build_emacs.org"
       "/home/jens/workspace/second-brain/org-roam/20231115134312-dbt_elementary.org"
       "/home/jens/workspace/second-brain/org-roam/20231116155153-python_pipreqs.org"
       "/home/jens/workspace/second-brain/org-roam/20231118000048-markdown_hyperlink.org"
       "/home/jens/workspace/second-brain/org-roam/20231118004256-python_return_annotation.org"
       "/home/jens/workspace/second-brain/org-roam/20231118144923-python_data_class.org"
       "/home/jens/workspace/second-brain/org-roam/20231119171237-cuda.org"
       "/home/jens/workspace/second-brain/org-roam/20231119171251-cuda_get_version.org"
       "/home/jens/workspace/second-brain/org-roam/20231119171528-cuda_install.org"
       "/home/jens/workspace/second-brain/org-roam/20231121134028-tele2_dataiku_feature_store.org"
       "/home/jens/workspace/second-brain/org-roam/20231121152007-mlops.org"
       "/home/jens/workspace/second-brain/org-roam/20231122100725-dbt_source_freshness.org"
       "/home/jens/workspace/second-brain/org-roam/20231124104244-tele2_backfills_log.org"
       "/home/jens/workspace/second-brain/org-roam/20231130104412-linux_system_resources_monitoring.org"
       "/home/jens/workspace/second-brain/org-roam/20231201140525-dbt_generate_manifest_file.org"
       "/home/jens/workspace/second-brain/org-roam/20231201183226-cicd_pass_artifacts_to_other_stages.org"
       "/home/jens/workspace/second-brain/org-roam/20231201183318-cicd.org"
       "/home/jens/workspace/second-brain/org-roam/20231202002057-python_read_from_file.org"
       "/home/jens/workspace/second-brain/org-roam/20231202003223-python_string_remove_letters_or_numbers.org"
       "/home/jens/workspace/second-brain/org-roam/20231204143114-tele2_dataiku_model_review.org"
       "/home/jens/workspace/second-brain/org-roam/20231205113529-jellyfin.org"
       "/home/jens/workspace/second-brain/org-roam/20231205113654-plex.org"
       "/home/jens/workspace/second-brain/org-roam/20231205114243-gittea.org"
       "/home/jens/workspace/second-brain/org-roam/20231205132315-tele2_year_end_checkin.org"
       "/home/jens/workspace/second-brain/org-roam/20231205133044-tele2_feedback.org"
       "/home/jens/workspace/second-brain/org-roam/20231205171244-dataiku_productionize_model.org"
       "/home/jens/workspace/second-brain/org-roam/20231205223936-career.org"
       "/home/jens/workspace/second-brain/org-roam/20231205224233-career_skills.org"
       "/home/jens/workspace/second-brain/org-roam/20231205233839-career_learning.org"
       "/home/jens/workspace/second-brain/org-roam/20231206191149-games.org"
       "/home/jens/workspace/second-brain/org-roam/20231206191201-slay_the_princess.org"
       "/home/jens/workspace/second-brain/org-roam/20231207233903-fallout_4.org"
       "/home/jens/workspace/second-brain/org-roam/20231210142708-monster_hunter_world.org"
       "/home/jens/workspace/second-brain/org-roam/20231211123906-favorite_games.org"
       "/home/jens/workspace/second-brain/org-roam/20231211151850-tubearchivist.org"
       "/home/jens/workspace/second-brain/org-roam/20231211191139-python_is_number.org"
       "/home/jens/workspace/second-brain/org-roam/20231213110012-ansible.org"
       "/home/jens/workspace/second-brain/org-roam/20231213224631-shell.org"
       "/home/jens/workspace/second-brain/org-roam/20231213224720-oh_my_zsh.org"
       "/home/jens/workspace/second-brain/org-roam/20231214002149-ansible_mode.org"
       "/home/jens/workspace/second-brain/org-roam/20231214113445-rice.org"
       "/home/jens/workspace/second-brain/org-roam/20231214114228-bspwm.org"
       "/home/jens/workspace/second-brain/org-roam/20231214114728-desktop_status_bar.org"
       "/home/jens/workspace/second-brain/org-roam/20231214125450-lemonbar.org"
       "/home/jens/workspace/second-brain/org-roam/20231214220217-ansible_builtin_apt_module.org"
       "/home/jens/workspace/second-brain/org-roam/20231214234934-ansible_download_file_from_http_or_ftp.org"
       "/home/jens/workspace/second-brain/org-roam/20231215101802-machine_learning_vocab.org"
       "/home/jens/workspace/second-brain/org-roam/20231215234408-ansible_stow_module.org"
       "/home/jens/workspace/second-brain/org-roam/20231215234627-stow.org"
       "/home/jens/workspace/second-brain/org-roam/20231217002028-ansible_remove_files.org"
       "/home/jens/workspace/second-brain/org-roam/20231220005707-kerbal_space_program.org"
       "/home/jens/workspace/second-brain/org-roam/20231222011113-factorio.org"
       "/home/jens/workspace/second-brain/org-roam/20231222011720-palia.org"
       "/home/jens/workspace/second-brain/org-roam/20231222013151-dashboards.org"
       "/home/jens/workspace/second-brain/org-roam/20231222013225-personal_dashboards.org"
       "/home/jens/workspace/second-brain/org-roam/20231222014658-flame_dashboard.org"
       "/home/jens/workspace/second-brain/org-roam/20231222015047-dashmachine_dashboard.org"
       "/home/jens/workspace/second-brain/org-roam/20231222020251-homepage_dashboard.org"
       "/home/jens/workspace/second-brain/org-roam/20240103123352-2023_retrospective.org"
       "/home/jens/workspace/second-brain/org-roam/20240103123405-yearly_retrospective.org"
       "/home/jens/workspace/second-brain/org-roam/20240103160332-job_interview.org"
       "/home/jens/workspace/second-brain/org-roam/20240104134912-service_account.org"
       "/home/jens/workspace/second-brain/org-roam/20240105141658-aws.org"
       "/home/jens/workspace/second-brain/org-roam/20240105141724-aws_cli.org"
       "/home/jens/workspace/second-brain/org-roam/20240105141804-aws_cli_get_secret.org"
       "/home/jens/workspace/second-brain/org-roam/20240105145711-aws_cli_authenticate.org"
       "/home/jens/workspace/second-brain/org-roam/20240105153906-tele2_dataiku_model_reruns.org"
       "/home/jens/workspace/second-brain/org-roam/20240108131730-dataiku_use_trigger_date_as_date.org"
       "/home/jens/workspace/second-brain/org-roam/20240109140608-git_change_remote_url.org"
       "/home/jens/workspace/second-brain/org-roam/20240110102328-tele2_model_short_names.org"
       "/home/jens/workspace/second-brain/org-roam/20240110130011-linux_audio.org"
       "/home/jens/workspace/second-brain/org-roam/20240111164215-sql_get_missing_dates.org"
       "/home/jens/workspace/second-brain/org-roam/20240116153357-books_wishlist.org"
       "/home/jens/workspace/second-brain/org-roam/20240116175002-zsh_set_default_shell.org"
       "/home/jens/workspace/second-brain/org-roam/20240116180448-ansible_playbook_cheatsheet.org"
       "/home/jens/workspace/second-brain/org-roam/20240116224504-my_computers.org"
       "/home/jens/workspace/second-brain/org-roam/20240116233210-github_authenticate_ssh.org"
       "/home/jens/workspace/second-brain/org-roam/20240117162603-vaccines.org"
       "/home/jens/workspace/second-brain/org-roam/20240117182603-linux_xrandr.org"
       "/home/jens/workspace/second-brain/org-roam/20240117182815-linux_nm_applet.org"
       "/home/jens/workspace/second-brain/org-roam/20240117185028-i3_toggle_tiling.org"
       "/home/jens/workspace/second-brain/org-roam/20240118153913-tele2_dataiku_get_all_scoring_tables.org"
       "/home/jens/workspace/second-brain/org-roam/20240118185042-alacritty.org"
       "/home/jens/workspace/second-brain/org-roam/20240120164827-emacs_python_notebook.org"
       "/home/jens/workspace/second-brain/org-roam/20240120181143-linux_touchpad.org"
       "/home/jens/workspace/second-brain/org-roam/20240120225855-linux_unzip.org"
       "/home/jens/workspace/second-brain/org-roam/20240121150122-anisble_git.org"
       "/home/jens/workspace/second-brain/org-roam/20240121165706-pandas.org"
       "/home/jens/workspace/second-brain/org-roam/20240122232856-flask.org"
       "/home/jens/workspace/second-brain/org-roam/20240123100509-sql_remove_rows.org"
       "/home/jens/workspace/second-brain/org-roam/20240124232704-my_thought_on_switching_to_vim_blog_post.org"
       "/home/jens/workspace/second-brain/org-roam/20240125145302-pgp_encryption.org"
       "/home/jens/workspace/second-brain/org-roam/20240125160651-node_js.org"
       "/home/jens/workspace/second-brain/org-roam/20240125160811-nvm.org"
       "/home/jens/workspace/second-brain/org-roam/20240125161142-yarn.org"
       "/home/jens/workspace/second-brain/org-roam/20240127001650-linux_bluetooth.org"
       "/home/jens/workspace/second-brain/org-roam/20240130235050-linux_airplane_mode.org"
       "/home/jens/workspace/second-brain/org-roam/20240208101113-minikube.org"
       "/home/jens/workspace/second-brain/org-roam/20240208101141-kubernetes.org"
       "/home/jens/workspace/second-brain/org-roam/20240208122705-kubectl.org"
       "/home/jens/workspace/second-brain/org-roam/20240208124304-helm.org"
       "/home/jens/workspace/second-brain/org-roam/20240209123727-ansible_docker.org"
       "/home/jens/workspace/second-brain/org-roam/20240217134639-vacation.org"
       "/home/jens/workspace/second-brain/org-roam/20240217134754-vietnam_vacation_2024_spring.org"
       "/home/jens/workspace/second-brain/org-roam/20240220111800-dbt_defer.org"
       "/home/jens/workspace/second-brain/org-roam/20240220113010-sql_list_all_tables_in_schema.org"
       "/home/jens/workspace/second-brain/org-roam/20240220122408-tmp.org"
       "/home/jens/workspace/second-brain/org-roam/20240220160531-emacs_zen_mode.org"
       "/home/jens/workspace/second-brain/org-roam/20240220162916-gitlab_cicd.org"
       "/home/jens/workspace/second-brain/org-roam/20240220164012-gitlab_cicd_rules.org"
       "/home/jens/workspace/second-brain/org-roam/20240221102237-workflow_orchestration.org"
       "/home/jens/workspace/second-brain/org-roam/20240221102512-prefect.org"
       "/home/jens/workspace/second-brain/org-roam/20240221102557-dagster.org"
       "/home/jens/workspace/second-brain/org-roam/20240221103054-bash_string_manipulation.org"
       "/home/jens/workspace/second-brain/org-roam/20240221161542-dataiku_get_bundles.org"
       "/home/jens/workspace/second-brain/org-roam/20240222103940-tele2_retrospective.org"
       "/home/jens/workspace/second-brain/org-roam/20240222131332-tele2_dbt_airflow_integration.org"
       "/home/jens/workspace/second-brain/org-roam/20240222141208-gitlab_cicd_run_jobs_sequentially.org"
       "/home/jens/workspace/second-brain/org-roam/20240222144148-gitlab_cicd_artifacts.org"
       "/home/jens/workspace/second-brain/org-roam/20240222151325-gitlab_cicd_git.org"
       "/home/jens/workspace/second-brain/org-roam/20240222155201-gitlab_job_token.org"
       "/home/jens/workspace/second-brain/org-roam/20240222161757-gitlab_cicd_variables.org"
       "/home/jens/workspace/second-brain/org-roam/20240223111933-dataiku_unit_testing.org"
       "/home/jens/workspace/second-brain/org-roam/20240223112011-unit_tests.org"
       "/home/jens/workspace/second-brain/org-roam/20240223145848-dbt_test_check_row_count.org"
       "/home/jens/workspace/second-brain/org-roam/20240224183224-my_games.org"
       "/home/jens/workspace/second-brain/org-roam/20240224183929-godot.org"
       "/home/jens/workspace/second-brain/org-roam/20240224185420-godot_emacs_integration.org"
       "/home/jens/workspace/second-brain/org-roam/20240224234617-emacs_use_package.org"
       "/home/jens/workspace/second-brain/org-roam/Books.org"
       "/home/jens/workspace/second-brain/org-roam/Containers.org"
       "/home/jens/workspace/second-brain/org-roam/DAVG-389-add_write_time_and_dbt_run_started_at_to_lifetime_change_tag.org"
       "/home/jens/workspace/second-brain/org-roam/DBT-backfilling.org"
       "/home/jens/workspace/second-brain/org-roam/Data-structures.org"
       "/home/jens/workspace/second-brain/org-roam/Dbt.org"
       "/home/jens/workspace/second-brain/org-roam/ELT.org"
       "/home/jens/workspace/second-brain/org-roam/Emacs.org"
       "/home/jens/workspace/second-brain/org-roam/Git.org"
       "/home/jens/workspace/second-brain/org-roam/Interview-questions.org"
       "/home/jens/workspace/second-brain/org-roam/LLM-for-production.org"
       "/home/jens/workspace/second-brain/org-roam/Linear-algebra.org"
       "/home/jens/workspace/second-brain/org-roam/MLOps-tools.org"
       "/home/jens/workspace/second-brain/org-roam/MLOps-vocab.org"
       "/home/jens/workspace/second-brain/org-roam/Machine-learning-general.org"
       "/home/jens/workspace/second-brain/org-roam/Machine-learning.org"
       "/home/jens/workspace/second-brain/org-roam/Markdown.org"
       "/home/jens/workspace/second-brain/org-roam/My-devices.org"
       "/home/jens/workspace/second-brain/org-roam/My-hosts.org"
       "/home/jens/workspace/second-brain/org-roam/Open-source-projects.org"
       "/home/jens/workspace/second-brain/org-roam/PiHole.org"
       "/home/jens/workspace/second-brain/org-roam/Portainer.org"
       "/home/jens/workspace/second-brain/org-roam/Raspberry-pi-powered-mirror.org"
       "/home/jens/workspace/second-brain/org-roam/RaspberryPi.org"
       "/home/jens/workspace/second-brain/org-roam/Raspberrypihole.org"
       "/home/jens/workspace/second-brain/org-roam/Sql.org"
       "/home/jens/workspace/second-brain/org-roam/Ssh-key.org"
       "/home/jens/workspace/second-brain/org-roam/Traefik.org"
       "/home/jens/workspace/second-brain/org-roam/Ubuntu-server.org"
       "/home/jens/workspace/second-brain/org-roam/Wishlist.org"
       "/home/jens/workspace/second-brain/org-roam/airflow-backfilling.org"
       "/home/jens/workspace/second-brain/org-roam/airflow-docker.org"
       "/home/jens/workspace/second-brain/org-roam/airflow-open-source.org"
       "/home/jens/workspace/second-brain/org-roam/airflow.org"
       "/home/jens/workspace/second-brain/org-roam/all-posts.org"
       "/home/jens/workspace/second-brain/org-roam/amateur-rocket-building.org"
       "/home/jens/workspace/second-brain/org-roam/bash.org"
       "/home/jens/workspace/second-brain/org-roam/bind9.org"
       "/home/jens/workspace/second-brain/org-roam/blog-dbt.org"
       "/home/jens/workspace/second-brain/org-roam/blog-index.org"
       "/home/jens/workspace/second-brain/org-roam/blog-platforms.org"
       "/home/jens/workspace/second-brain/org-roam/blog-post-org-roam.org"
       "/home/jens/workspace/second-brain/org-roam/blog-post-setting-up-my-home-lab.org"
       "/home/jens/workspace/second-brain/org-roam/blog-posts.org"
       "/home/jens/workspace/second-brain/org-roam/blog-sites.org"
       "/home/jens/workspace/second-brain/org-roam/coffee.org"
       "/home/jens/workspace/second-brain/org-roam/computer-science-vocab.org"
       "/home/jens/workspace/second-brain/org-roam/data-testing.org"
       "/home/jens/workspace/second-brain/org-roam/dataiku-core-designer.org"
       "/home/jens/workspace/second-brain/org-roam/dataiku-docker.org"
       "/home/jens/workspace/second-brain/org-roam/dataiku-mlops-practitoner.org"
       "/home/jens/workspace/second-brain/org-roam/dataiku.org"
       "/home/jens/workspace/second-brain/org-roam/dbt-airflow-integration.org"
       "/home/jens/workspace/second-brain/org-roam/dbt-conversion.org"
       "/home/jens/workspace/second-brain/org-roam/dbt-materializations.org"
       "/home/jens/workspace/second-brain/org-roam/dbt-unit-tests.org"
       "/home/jens/workspace/second-brain/org-roam/dns.org"
       "/home/jens/workspace/second-brain/org-roam/docker-boilerplates.org"
       "/home/jens/workspace/second-brain/org-roam/docker-compose.org"
       "/home/jens/workspace/second-brain/org-roam/docker.org"
       "/home/jens/workspace/second-brain/org-roam/domain-names.org"
       "/home/jens/workspace/second-brain/org-roam/elementary-cli.org"
       "/home/jens/workspace/second-brain/org-roam/elementary-setup.org"
       "/home/jens/workspace/second-brain/org-roam/elementary.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-beacon.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-daemon.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-elfeed.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-evil.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-lisp.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-lsp.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-magit.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-multiple-cursor.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-navigation.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-spell-checking.org"
       "/home/jens/workspace/second-brain/org-roam/emacs-writegood.org"
       "/home/jens/workspace/second-brain/org-roam/export-environment-variables.org"
       "/home/jens/workspace/second-brain/org-roam/font-source-code-pro.org"
       "/home/jens/workspace/second-brain/org-roam/fonts.org"
       "/home/jens/workspace/second-brain/org-roam/gitlab.org"
       "/home/jens/workspace/second-brain/org-roam/go.org"
       "/home/jens/workspace/second-brain/org-roam/google-analytics.org"
       "/home/jens/workspace/second-brain/org-roam/grafana-dashboards.org"
       "/home/jens/workspace/second-brain/org-roam/grep.org"
       "/home/jens/workspace/second-brain/org-roam/home-server.org"
       "/home/jens/workspace/second-brain/org-roam/hugo.org"
       "/home/jens/workspace/second-brain/org-roam/index-to-position.org"
       "/home/jens/workspace/second-brain/org-roam/interview-coding.org"
       "/home/jens/workspace/second-brain/org-roam/javascript.org"
       "/home/jens/workspace/second-brain/org-roam/machine-learning-papers.org"
       "/home/jens/workspace/second-brain/org-roam/mango-sago.org"
       "/home/jens/workspace/second-brain/org-roam/marchine-learning.org"
       "/home/jens/workspace/second-brain/org-roam/matcha-cheescake.org"
       "/home/jens/workspace/second-brain/org-roam/matcha-cheesecake-brownies.org"
       "/home/jens/workspace/second-brain/org-roam/ml-papers.org"
       "/home/jens/workspace/second-brain/org-roam/mlflow.org"
       "/home/jens/workspace/second-brain/org-roam/mu4e.org"
       "/home/jens/workspace/second-brain/org-roam/my-blog-articles.org"
       "/home/jens/workspace/second-brain/org-roam/none-relational-databases.org"
       "/home/jens/workspace/second-brain/org-roam/npm.org"
       "/home/jens/workspace/second-brain/org-roam/org-agenda.org"
       "/home/jens/workspace/second-brain/org-roam/org-image-captions.org"
       "/home/jens/workspace/second-brain/org-roam/org-images.org"
       "/home/jens/workspace/second-brain/org-roam/org-roam-backlinks.org"
       "/home/jens/workspace/second-brain/org-roam/org-roam-node-properties.org"
       "/home/jens/workspace/second-brain/org-roam/org-roam-publish.org"
       "/home/jens/workspace/second-brain/org-roam/org-roam.org"
       "/home/jens/workspace/second-brain/org-roam/org-todo.org"
       "/home/jens/workspace/second-brain/org-roam/org.org"
       "/home/jens/workspace/second-brain/org-roam/ox-hugo.org"
       "/home/jens/workspace/second-brain/org-roam/portfolio-projects.org"
       "/home/jens/workspace/second-brain/org-roam/portfolio.org"
       "/home/jens/workspace/second-brain/org-roam/programming-languages.org"
       "/home/jens/workspace/second-brain/org-roam/projects.org"
       "/home/jens/workspace/second-brain/org-roam/public-dns-servers.org"
       "/home/jens/workspace/second-brain/org-roam/pytorch-open-source.org"
       "/home/jens/workspace/second-brain/org-roam/query-optimization.org"
       "/home/jens/workspace/second-brain/org-roam/reading-list.org"
       "/home/jens/workspace/second-brain/org-roam/recipes.org"
       "/home/jens/workspace/second-brain/org-roam/relational-databases.org"
       "/home/jens/workspace/second-brain/org-roam/residual-attention-network-image-classification.org"
       "/home/jens/workspace/second-brain/org-roam/resturants-stockholm.org"
       "/home/jens/workspace/second-brain/org-roam/rss-feeds.org"
       "/home/jens/workspace/second-brain/org-roam/ruby.org"
       "/home/jens/workspace/second-brain/org-roam/run-bash-script.org"
       "/home/jens/workspace/second-brain/org-roam/rust-analyzer.org"
       "/home/jens/workspace/second-brain/org-roam/rust-arrays.org"
       "/home/jens/workspace/second-brain/org-roam/rust-build-and-run.org"
       "/home/jens/workspace/second-brain/org-roam/rust-cargo.org"
       "/home/jens/workspace/second-brain/org-roam/rust-cast.org"
       "/home/jens/workspace/second-brain/org-roam/rust-classes.org"
       "/home/jens/workspace/second-brain/org-roam/rust-compare-enums.org"
       "/home/jens/workspace/second-brain/org-roam/rust-create-project.org"
       "/home/jens/workspace/second-brain/org-roam/rust-hashmap.org"
       "/home/jens/workspace/second-brain/org-roam/rust-inline-if.org"
       "/home/jens/workspace/second-brain/org-roam/rust-install.org"
       "/home/jens/workspace/second-brain/org-roam/rust-linting.org"
       "/home/jens/workspace/second-brain/org-roam/rust-loops.org"
       "/home/jens/workspace/second-brain/org-roam/rust-null.org"
       "/home/jens/workspace/second-brain/org-roam/rust-parameters.org"
       "/home/jens/workspace/second-brain/org-roam/rust-pointers-and-references.org"
       "/home/jens/workspace/second-brain/org-roam/rust-print.org"
       "/home/jens/workspace/second-brain/org-roam/rust-rustup.org"
       "/home/jens/workspace/second-brain/org-roam/rust-string.org"
       "/home/jens/workspace/second-brain/org-roam/rust-tests.org"
       "/home/jens/workspace/second-brain/org-roam/rust-threads.org"
       "/home/jens/workspace/second-brain/org-roam/rust-tui-popup.org"
       "/home/jens/workspace/second-brain/org-roam/rust.org"
       "/home/jens/workspace/second-brain/org-roam/snowflake-connector.org"
       "/home/jens/workspace/second-brain/org-roam/snowflake.org"
       "/home/jens/workspace/second-brain/org-roam/spacemacs-docker-layer.org"
       "/home/jens/workspace/second-brain/org-roam/spacemacs-packages.org"
       "/home/jens/workspace/second-brain/org-roam/spacemacs-sql-layer.org"
       "/home/jens/workspace/second-brain/org-roam/spacemacs-terminal.org"
       "/home/jens/workspace/second-brain/org-roam/spacemacs.org"
       "/home/jens/workspace/second-brain/org-roam/sql-count.org"
       "/home/jens/workspace/second-brain/org-roam/sql-delete-tables.org"
       "/home/jens/workspace/second-brain/org-roam/sql-exclude-columns.org"
       "/home/jens/workspace/second-brain/org-roam/sql-table.org"
       "/home/jens/workspace/second-brain/org-roam/sql-testing-dataset.org"
       "/home/jens/workspace/second-brain/org-roam/sql-view.org"
       "/home/jens/workspace/second-brain/org-roam/study.org"
       "/home/jens/workspace/second-brain/org-roam/symlink.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-ai-hub-focus-area.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-ai-hub-focus-milestones.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-aws.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-comviq-backfill.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-dbt-conversion.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-dbt-snapshot.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-development-goals.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-docker.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-effective-teams-workshop.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-hackathon-2023-05-31.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-inspire-train-support.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-makefile.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-one-on-one-2023-06-20.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-one-on-one-2023-07-04.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-one-on-one.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-organizational-structure.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-pipeline-failure.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-snowflake-account-admins.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-source-tables.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-time-reporting.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-ubuntu.org"
       "/home/jens/workspace/second-brain/org-roam/tele2-vpn.org"
       "/home/jens/workspace/second-brain/org-roam/tele2.org"
       "/home/jens/workspace/second-brain/org-roam/terraform.org"
       "/home/jens/workspace/second-brain/org-roam/test-dns-server.org"
       "/home/jens/workspace/second-brain/org-roam/the-progmatic-programmer.org"
       "/home/jens/workspace/second-brain/org-roam/the-three-body-problem.org"
       "/home/jens/workspace/second-brain/org-roam/todo.org"
       "/home/jens/workspace/second-brain/org-roam/transformer-ml.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu-cant-start-dns-server.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu-disable-ssh-root-login.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu-domain-name.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu-ftp.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu-static-ip.org"
       "/home/jens/workspace/second-brain/org-roam/ubuntu.org"
       "/home/jens/workspace/second-brain/org-roam/vacation-2023.org"
       "/home/jens/workspace/second-brain/org-roam/validio-tele2-meeting.org"
       "/home/jens/workspace/second-brain/org-roam/validio.org"
       "/home/jens/workspace/second-brain/org-roam/vscode-keybindings.org"
       "/home/jens/workspace/second-brain/org-roam/wget.org"
       "/home/jens/workspace/second-brain/org-roam/windows-terminal.org"
       "/home/jens/workspace/second-brain/org-roam/wsl-systemd.org"
       "/home/jens/workspace/second-brain/org-roam/wsl.org"
       "/home/jens/workspace/second-brain/org-roam/wsl2-exec-format-error.org"
       "/home/jens/workspace/second-brain/org-roam/zettelkasten-method.org"
       "/home/jens/workspace/second-brain/data-innovation-summit-2023.org"))
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
                 fancy-battery flx-ido flycheck-elsa flycheck-package
                 flycheck-pos-tip flycheck-rust font-lock+ fuzzy ggtags gh-md gntp
                 gnuplot golden-ratio google-translate gruvbox-theme helm-ag
                 helm-c-yasnippet helm-company helm-descbinds helm-gtags helm-lsp
                 helm-make helm-mode-manager helm-org helm-org-rifle
                 helm-projectile helm-purpose helm-swoop helm-themes helm-xref
                 help-fns+ hide-comnt highlight-indentation highlight-numbers
                 highlight-parentheses hl-todo holy-mode htmlize hungry-delete
                 hybrid-mode indent-guide info+ inspector ivy link-hint log4e
                 lorem-ipsum lsp-docker lsp-mode lsp-origami lsp-treemacs lsp-ui
                 macrostep map markdown-mode markdown-toc mmm-mode multi-line
                 multi-term multi-vterm mwim nameless neotree nhich-key
                 open-junk-file org org-category-capture org-cliplink org-contrib
                 org-download org-mime org-pomodoro org-present org-projectile
                 org-ql org-rich-yank org-roam org-roam-dailies org-roam-ui
                 org-sidebar org-super-agenda org-superstar orgit orgit-forge
                 origami ov overseer ox-hugo paradox password-generator pcre2el
                 peg pfuture popwin pos-tip project quickrun racer
                 rainbow-delimiters request restart-emacs rich-minority ron-mode
                 rust-mode shell-pop shrink-path smart-mode-line space-doc
                 spaceline-all-the-icons spacemacs-purpose-popwin
                 spacemacs-whitespace-cleanup string-edit-at-point
                 string-inflection swiper symbol-overlay symon term-cursor
                 terminal-here toc-org toml-mode treemacs treemacs-icons-dired
                 treemacs-persp treemacs-projectile ts undo-tree unfill
                 use-package uuidgen valign vi-tilde-fringe vim-powerline vmd-mode
                 volatile-highlights vterm winum writeroom-mode ws-butler xref
                 xterm-color yasnippet yasnippet-snippets))
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
