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

Not all folders in this repository are currently active. The Ansible playbook (`ansible-playbook/group_vars/all.yml`) determines which configurations are actually stowed to the system.

* **`ansible-playbook/`**: Automation scripts to set up the environment.
* **`hypr*/`**: Configuration for Hyprland, Hyprlock, and Hyprpaper.
* **`zsh-starship-antidote/`**: My current Zsh setup using Antidote plugin manager and Starship prompt.
* **`spacemacs/`**: Emacs configuration (Org-roam, LSP, etc.).
* **`kitty/`**: Terminal configuration.
* **`i3/`, `polybar/`, `alacritty/`**: Legacy X11 configurations (not currently active in the default Ansible install).

## Installation

I use **Ansible** to detect the OS (Arch vs Ubuntu), install the necessary system packages, and Stow the dotfiles.

### Prerequisites
* Ansible
* Git

### Deployment
1.  Clone the repository:
    ```bash
    git clone https://github.com/celsuss/dotfiles.git
    ```

2.  Run the Ansible setup:
    ```bash
    cd dotfiles/ansible-playbook
    # Dry run to check what will happen
    ansible-playbook setup.yml --check --ask-become-pass

    # Install
    ansible-playbook setup.yml --ask-become-pass
    ```

*Note: The playbook automatically handles distribution differences (e.g., installing `base-devel` on Arch vs `build-essential` on Ubuntu).*
