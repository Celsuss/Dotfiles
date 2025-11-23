# Ansible Dotfiles Setup

This Ansible playbook automates the setup and configuration of a personal development environment. It is designed to work locally on **Arch Linux** and **Ubuntu** systems.

## Features

* **Automatic OS Detection**: Identifies the distribution (Arch Linux or Ubuntu) and installs the appropriate system packages.
* **Package Management**: Installs essential tools such as `git`, `stow`, `zsh`, `emacs`, and `ripgrep`.
* **Dotfiles Management**: Uses [GNU Stow](https://www.gnu.org/software/stow/) to symlink dotfiles for `zsh`, `git`, and `spacemacs`.
* **Spacemacs Setup**: Clones the `develop` branch of Spacemacs to `.emacs.d`.
* **Shell Configuration**: Ensures Zsh is set as the default shell.

## Prerequisites

* **Ansible**: Ensure Ansible is installed on your local machine.
* **Git**: Required to clone this repository.

## Usage

Run the following commands from the `ansible-playbook` directory.

### 1. Dry Run (Check Mode)

Use this to simulate the playbook execution without making any actual changes to your system. It helps verify that the playbook can run successfully and shows you what would be changed.

* `--check`: Runs in simulation mode.
* `--ask-become-pass`: Prompts for your sudo password (required for installing packages).

```bash
ansible-playbook setup.yml --check --ask-become-pass
```

## Project Structure

* **`setup.yml`**: The main playbook entry point.
* **`ansible.cfg`**: Local Ansible configuration (sets inventory path, disables retry files, etc.).
* **`inventory/hosts.ini`**: Defines `localhost` as the target with a local connection.
* **`group_vars/`**:
    * `all.yml`: Global variables (user settings, Stow directories).
    * `Archlinux.yml` / `Ubuntu.yml`: OS-specific package lists.
* **`roles/`**:
    * `common`: Installs system packages and configures the shell.
    * `spacemacs`: Handles the cloning of the Spacemacs repository.
    * `dotfiles`: Stows the configuration files from the repository root.

## Configuration

### customizing Packages
You can add or remove system packages by editing the variable files corresponding to your distribution:
* **Arch Linux**: `group_vars/Archlinux.yml`
* **Ubuntu**: `group_vars/Ubuntu.yml`

### Customizing Dotfiles
To change which directories are stowed, edit the `stow_dirs` list in `group_vars/all.yml`:

```yaml
stow_dirs:
  - zsh
  - git
  - spacemacs
```
