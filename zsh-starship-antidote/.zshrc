# ~/.zshrc

# --- Environment Variables (MUST BE FIRST) ---
export ZSH="$HOME/.oh-my-zsh"
export ZSH_CACHE_DIR="$HOME/.cache/zsh/oh-my-zsh"

# Plugins like Rust, Helm, and Docker try to write generated completions here.
[[ -d "$ZSH_CACHE_DIR/completions" ]] || mkdir -p "$ZSH_CACHE_DIR/completions"

fpath=("$ZSH_CACHE_DIR/completions" $fpath)

# --- Initialize Completion System ---
autoload -Uz compinit
compinit -C -d "$ZSH_CACHE_DIR/zcompdump"

# --- Configuration (Before Plugins Load) ---
zstyle :omz:plugins:keychain agents ssh,gpg
zstyle :omz:plugins:keychain identities id_rsa

# --- Load Plugins (Antidote) ---
source /usr/share/zsh-antidote/antidote.zsh
antidote load ${ZDOTDIR:-$HOME}/.zsh_plugins.txt

# --- Basic Zsh Options ---
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
bindkey -v

# --- Aliases ---
alias fzf="fzf --preview 'bat --color=always {}'"
alias ls="eza --icons"
alias ll='ls -lah'
alias grep='grep --color=auto'

# --- fzf tab auto complete ---
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false

# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'

# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no

# preview directory's content with eza when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'

# switch group using < and >
zstyle ':fzf-tab:*' switch-group '<' '>'

# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'

# Use your custom FZF colors (Gruvbox)
export FZF_DEFAULT_OPTS="
--color=bg+:#3c3836,bg:#282828,spinner:#8ec07c,hl:#fb4934
--color=fg:#ebdbb2,header:#8ec07c,info:#fabd2f,pointer:#fb4934
--color=marker:#fb4934,fg+:#ebdbb2,prompt:#fb4934,hl+:#fb4934"


# --- Starship Prompt ---
eval "$(starship init zsh)"

# --- Inits ---
eval "$(zoxide init zsh)"
# Apply the same eza preview to 'z' command
zstyle ':fzf-tab:complete:z:*' fzf-preview 'eza -1 --color=always $realpath'

# --- FZF & Vi-Mode Integration ---

# Configure the look of the FZF window (with 'bat' colors)
export FZF_CTRL_R_OPTS="
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-/:toggle-preview'
  --color header:italic
  --header 'Press CTRL-Y to copy command into clipboard'"

function zvm_after_init() {
    # We must source this file here to create the 'fzf-history-widget'
    source /usr/share/fzf/key-bindings.zsh
    # BINDING: Map '/' in Normal Mode to FZF History
    zvm_bindkey vicmd '/' fzf-history-widget
}
