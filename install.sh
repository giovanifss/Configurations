#!/bin/bash

projects="path/to/projects"

function create_symlink () {
  local origin_path="$1"
  local dest_path="$2"

  if [ ! -h "${dest_path}" ]; then
    [ -e "${dest_path}" ] && mv "${dest_path}" "${dest_path}.bkp"    # Backup file if exists
    echo "--> Creating symlink '${dest_path} -> ${origin_path}'"
    ln -s "${origin_path}" "${dest_path}"
  else
    echo ":: Symlink ${dest_path} already exists"
  fi
}

function setup_xorg () {
  local current_dir="$1"
  local from="${current_dir}/xorg/xprofile"
  local to="$HOME/.xprofile"
  create_symlink "${from}" "${to}"
}

function setup_neovim () {
  local current_dir="$1"
  local from="${current_dir}/nvim"
  local to="$HOME/.config/nvim"

  echo "--> Updating fzf-proj.vimrc"
  sed -i -e 's|<CODE-PROJECTS>|'"$projects"'|g' nvim/plugins/fzf-proj.vimrc
  create_symlink "${from}" "${to}"
}

function setup_xmobar () {
  local current_dir="$1"
  local interface="$(ip addr | grep '[0-9]: .*: ' | cut -d ' ' -f2 | cut -d ':' -f1 | grep -v "lo")"
  local from="${current_dir}/xmobar/xmobarrc.hs"
  local to="$HOME/.xmobarrc"

  echo "--> Updating xmobarrc.hs"
  sed -i -e 's|<IF-1>|'"${interface}"'|g' xmobar/xmobarrc.hs
  create_symlink "${from}" "${to}"
}

function setup_xmonad () {
  local current_dir="$1"
  local from="${current_dir}/xmonad/xmonad.hs"
  local to="$HOME/.xmonad/xmonad.hs"

  echo "--> Updating xmonad.hs"
  sed -i -e 's|<XMOBAR-BIN>|'"$(which xmobar)"'|g' -e "s|<XMOBAR-RC>|$HOME/.xmobarrc|g" xmonad/xmonad.hs
  mkdir -p "$HOME/.xmonad"
  create_symlink "${from}" "${to}"
}

function setup_urxvt () {
  local current_dir="$1"
  local from="${current_dir}/urxvt/urxvt.conf"
  local to="$HOME/.Xdefaults"
  create_symlink "${from}" "${to}"
}

function setup_zsh () {
  local current_dir="$1"
  local from="${current_dir}/zsh/zshrc"
  local to="$HOME/.zshrc"

  echo "--> Updating zshrc"
  sed -i -e 's|<HOME-DIR>|'"$HOME"'|g' zsh/zshrc
  sed -i -e 's|<CODE-PROJECTS>|'"$projects"'|g' zsh/zshrc
  create_symlink "${from}" "${to}"
}

function setup_terminator () {
  local current_dir="$1"
  local from="${current_dir}/terminator"
  local to="$HOME/.config/terminator"
  create_symlink "${from}" "${to}"
}

function setup_lightdm () {
  local current_dir="$1"
  local diff="${current_dir}/lightdm/lightdm.diff"
  local to="/usr/share/xgreeters/lightdm-gtk-greeter.desktop"
  echo "--> Updating lightmdm config at ${to}"
  [ -w "${to}" ] && patch "${to}" "${diff}" || sudo patch "${to}" "${diff}"
}

function main () {
  local pwd
  pwd="$(pwd)"

  setup_xorg "${pwd}"
  setup_neovim "${pwd}"
  setup_xmobar "${pwd}"
  setup_xmonad "${pwd}"
  setup_urxvt "${pwd}"
  setup_zsh "${pwd}"
  setup_terminator "${pwd}"
  setup_lightdm "${pwd}"
}

main
