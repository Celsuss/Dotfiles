#!/bin/bash



# Setup systemd file
ln -s ./emacs.service ~/.config/systemd/user/emacs.service
systemctl --user enable emacs.service
systemctl --user start emacs.service
