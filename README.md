# Dotfiles

This repo holds a collection of my dotfiles and automation scripts.

Currently, I maintain two different setups in this repository:
1.  **Hyprland (Wayland):** My current, primary driver.
2.  **i3 (X11):** My previous setup, kept for legacy support/backup.

![Hyprland screenshot](https://i.imghippo.com/files/HAGx8223pzI.png)


![Hyprland screenshot wallpaper](https://i.imghippo.com/files/WZ9348xLQ.png)

## Software Stack

I use a consistent **Gruvbox Material Dark** theme across most applications.

### Core Environment
| Category          | Primary (Active)                      | Legacy / Alternative     |
|:------------------|:--------------------------------------|:-------------------------|
| **OS**            | Arch Linux                            | Ubuntu                   |
| **WM/Compositor** | **Hyprland** (w/ Hyprlock, Hyprpaper) | i3 (w/ Polybar, Compton) |
| **Bar**           | Waybar                                | Polybar                  |
| **Launcher**      | Rofi (Wayland fork)                   | Rofi                     |
| **Terminal**      | **Kitty**                             | Alacritty                |
| **Shell**         | **Zsh** + Starship + Antidote         | Oh-My-Zsh                |
| **Editor**        | **Spacemacs** (Emacs)                 |                          |

### CLI Tools
* **VCS:** Git (w/ Delta), Jujutsu (`jj`)
* **Navigation:** `zoxide`, `yazi`
* **Utilities:** `btop`, `eza`, `bat`, `ripgrep`, `posting`
* **Kubernetes:** `k9s`, `kubectx`, `kubens`

## Repository Structure

Each top-level directory is a [GNU Stow](https://www.gnu.org/software/stow/) package
with its own `justfile` that knows where the package should be linked (`~`, `~/.config`,
or `~/.config/<name>`). Not all packages are active: the `packages` variable in the root
`justfile` lists the ones linked by the `*-all` recipes.

* **`justfile`**: Root entry point; exposes every package as a `just` module.
* **`hypr*/`**: Configuration for Hyprland, Hyprlock, and Hyprpaper.
* **`zsh-starship-antidote/`**: My current Zsh setup using Antidote plugin manager and Starship prompt.
* **`spacemacs/`**: Emacs configuration (Org-roam, LSP, etc.).
* **`kitty/`**: Terminal configuration.
* **`i3/`, `polybar/`, `alacritty/`**: Legacy X11 configurations (not part of `packages`, but can be stowed individually).

## Installation

Symlinks are created with **Stow**, driven by **just**.

### Prerequisites
* Git
* [GNU Stow](https://www.gnu.org/software/stow/)
* [just](https://github.com/casey/just)

### Deployment
1.  Clone the repository:
    ```bash
    git clone https://github.com/celsuss/dotfiles.git ~/workspace/dotfiles
    cd ~/workspace/dotfiles
    ```

2.  Stow the dotfiles:
    ```bash
    just                # list all recipes and packages
    just dry-run-all    # preview what would be linked
    just stow-all       # link every active package
    ```

3.  Individual packages can be managed through their module:
    ```bash
    just kitty::stow
    just hyprland::dry-run
    just i3::stow       # legacy packages work too
    just kitty::restow  # re-link after adding/removing files
    just kitty::unstow  # remove the symlinks
    ```

*Note: `stow` ignores the `justfile` and `README` in each package via its `.stow-local-ignore`.*
