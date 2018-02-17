#!/bin/bash

# Neovim configuration
ln -s "$(pwd)/nvim" "$HOME/.config/nvim"

# Xmobar configuration
ln -s "$(pwd)/xmobar/xmobarrc" "$HOME/.xmobarrc"

# Xmonad configuration
cat xmonad/masked-xmonad.hs | sed -e 's|<XMOBAR-BIN>|'"$(which xmobar)"'|g' | sed -e "s|<XMOBAR-RC>|$HOME/.xmobarrc|g" > xmonad/xmonad.hs
mkdir -p "$HOME/.xmonad"
ln -s "$(pwd)/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
