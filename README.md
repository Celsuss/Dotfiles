# Dotfiles

## Software summary
Software | Description | Link
--- | --- | ---
i3 | Window Manager | https://github.com/i3/i3
i3lock | lock screen | https://github.com/i3/i3lock
polybar | bar | https://github.com/polybar/polybar
compton | compositor | https://github.com/chjj/compton
rofi | program launcher | https://github.com/davatorium/rofi
feh | desktop wallpaper image viewer | https://github.com/derf/feh
pavucontrol | sound manager | https://github.com/pulseaudio/pavucontrol
playerctl | sound command line controller | https://github.com/altdesktop/playerctl
nm-applet | network manager | https://github.com/pavlix/nm-applet
FontAwesome | Font | https://fontawesome.com/
oh my zsh | terminal config management | https://github.com/ohmyzsh/ohmyzsh

## Setup
### Prerequisites
Before you proceed, make sure you have Ansible installed on your system.

``` bash
sudo apt-get update
sudo apt-get install ansible
```
### i3 Setup
#### Run i3 Playbook
To install i3 dotfiles, use the following command:

``` bash
/playbooks/i3-playbook.yaml
```
This playbook will configure your i3 window manager according to the settings specified in my dotfiles.

### Development Environment Setup
#### Run Development Environment Playbook
To set up your development environment, use the following command:

``` bash
ansible-playbook /playbooks/development-env-playbook.yaml
```
This playbook will install and configure the necessary tools and packages used in my development environment.
