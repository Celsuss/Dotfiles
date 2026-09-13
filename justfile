# Packages linked by the *-all recipes (the active setup on this machine).
# Every package directory is also available as a module, e.g. `just kitty::stow`.
packages := "zsh-starship-antidote git jj spacemacs kitty k9s btop hyprland hyprlock hyprpaper rofi waybar quickshell mpd rmpc opencode qutebrowser"

mod alacritty
mod btop
mod direnv
mod fastfetch
mod git
mod gtk-3-0 'gtk-3.0'
mod hyprland
mod hyprlock
mod hyprpaper
mod i3
mod jj
mod k9s
mod kitty
mod lxappearance
mod mpd
mod oh-my-zsh
mod opencode
mod polybar
mod qutebrowser
mod quickshell
mod rbw
mod rmpc
mod rofi
mod spacemacs
mod waybar
mod zsh-starship-antidote

# List available recipes and modules
default:
    @just --list --list-submodules

# Symlink every active package
stow-all:
    for p in {{packages}}; do just "$p"::stow; done

# Remove the symlinks of every active package
unstow-all:
    for p in {{packages}}; do just "$p"::unstow; done

# Remove and re-create the symlinks of every active package
restow-all:
    for p in {{packages}}; do just "$p"::restow; done

# Preview what stow-all would do without touching anything
dry-run-all:
    for p in {{packages}}; do just "$p"::dry-run; done
