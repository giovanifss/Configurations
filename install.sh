#!/bin/bash

projects="path/to/projects"

# Xorg configuration
ln -s "$(pwd)/xorg/xsessionrc" "$HOME/.xsessionrc"

# Neovim configuration
sed -i -e 's|<CODE-PROJECTS>|'"$projects"'|g' nvim/plugins/fzf-proj.vimrc
ln -s "$(pwd)/nvim" "$HOME/.config/nvim"

# Xmobar configuration
ln -s "$(pwd)/xmobar/xmobarrc" "$HOME/.xmobarrc"

# Xmonad configuration
sed -i -e 's|<XMOBAR-BIN>|'"$(which xmobar)"'|g' -e "s|<XMOBAR-RC>|$HOME/.xmobarrc|g" xmonad/xmonad.hs
mkdir -p "$HOME/.xmonad"
ln -s "$(pwd)/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"

# Urxvt configuration
ln -s "$(pwd)/urxvt/urxvt.conf" "$HOME/.Xdefaults"

# Zshell configuration
sed -i -e 's|<HOME-DIR>|'"$HOME"'|g' zsh/zshrc
ln -s "$(pwd)/zsh/zshrc" "$HOME/.zshrc"
