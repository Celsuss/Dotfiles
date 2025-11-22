# ~/.zshrc

# --- 1. Environment Variables (MUST BE FIRST) ---
export ZSH="$HOME/.oh-my-zsh"
export ZSH_CACHE_DIR="$HOME/.cache/zsh/oh-my-zsh"

# FIX: Create the 'completions' subdirectory manually.
# Plugins like Rust, Helm, and Docker try to write generated completions here.
[[ -d "$ZSH_CACHE_DIR/completions" ]] || mkdir -p "$ZSH_CACHE_DIR/completions"

# FIX: Add this new directory to fpath so Zsh can find the generated completions.
fpath=("$ZSH_CACHE_DIR/completions" $fpath)

# --- 2. Initialize Completion System ---
autoload -Uz compinit
compinit -C -d "$ZSH_CACHE_DIR/zcompdump"

# --- 3. Configuration (Before Plugins Load) ---
zstyle :omz:plugins:keychain agents ssh,gpg
zstyle :omz:plugins:keychain identities id_rsa

# --- 4. Load Plugins (Antidote) ---
source /usr/share/zsh-antidote/antidote.zsh
antidote load ${ZDOTDIR:-$HOME}/.zsh_plugins.txt

# --- 5. Basic Zsh Options ---
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
bindkey -v

# --- 6. Aliases ---
alias fzf="fzf --preview 'bat --color=always {}'"
alias ls="eza --icons"
alias ll='ls -lah'
alias grep='grep --color=auto'

# --- 7. Starship Prompt ---
eval "$(starship init zsh)"
