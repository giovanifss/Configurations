#!/bin/bash

projects="path/to/projects"

function create_symlink () {
  local origin_path="$1"
  local dest_path="$2"

  if [ ! -h "${dest_path}" ]; then
    [ -e "${dest_path}" ] && mv "${dest_path}" "${dest_path}.bkp"    # Backup file if exists
    ln -s "${origin_path}" "${dest_path}" \
      && echo "[+] Created symlink '${dest_path} -> ${origin_path}'" \
      || echo "[-] Could not create symlink '${dest_path} -> ${origin_path}'"
  else
    echo ":: Symlink ${dest_path} already exists"
  fi
}

function setup () {
  local current_dir="$1"
  local from="${current_dir}/$2"
  local to="$3"
  create_symlink "${from}" "${to}"
}

function update_file () {
  local file="$1"
  local old="$2"
  local new="$3"
  if grep -q "${old}" "${file}"; then
    sed -i -e 's|'"${old}"'|'"${new}"'|g' "${file}"
    echo -e "\t[+] Updated ${file}"
  else
    echo -e "\t:: Nothing to update in ${file}"
  fi
}

function setup_neovim () {
  local current_dir="$1"
  local from="${current_dir}/nvim"
  local to="$HOME/.config/nvim"
  update_file "nvim/plugins/fzf-proj.vimrc" "<CODE-PROJECTS>" "${projects}"
  create_symlink "${from}" "${to}"
}

function configure_xmobar () {
  local current_dir="$1"
  local interface="$(ip addr | grep '[0-9]: .*: ' | cut -d ' ' -f2 | cut -d ':' -f1 | grep -v "lo")"
  update_file "xmobar/xmobarrc.hs" "<IF-1>" "${interface}"
  update_file "xmobar/xmobarrc.hs" "<XMOBAR-DIR>" "$HOME/.xmobar"
  update_file "xmobar/scripts/battery.sh" "<XMOBAR-DIR>" "$HOME/.xmobar"
  update_file "xmonad/xmonad.hs" "<XMOBAR-BIN>" "$(which xmobar)"
  update_file "xmonad/xmonad.hs" "<XMOBAR-RC>" "$HOME/.xmobar/xmobarrc.hs"
}

function setup_xmonad () {
  local current_dir="$1"
  local from="${current_dir}/xmonad/xmonad.hs"
  local to="$HOME/.xmonad/xmonad.hs"
  mkdir -p "$HOME/.xmonad"
  create_symlink "${from}" "${to}"
}

function setup_zsh () {
  local current_dir="$1"
  local from="${current_dir}/zsh/zshrc"
  local to="$HOME/.zshrc"
  update_file "zsh/zshrc" "<HOME-DIR>" "$HOME"
  update_file "zsh/zshrc" "<CODE-PROJECTS>" "${projects}"
  create_symlink "${from}" "${to}"
}

function setup_lightdm_bg () {
  local current_dir="$1"
  local bgfrom="$(ls $HOME/Pictures/wallpapers/bg-lightdm.*)"
  local bgdest="/usr/share/pixmaps"
  local cfgfrom="${current_dir}/lightdm/lightdm-gtk-greeter.conf"
  local cfgdest="/etc/lightdm/lightdm-gtk-greeter.conf"
  echo "--> Copying lightdm background image"
  [ -w "${bgdest}" ] && cp "${bgfrom}" "${bgdest}" || sudo cp "${bgfrom}" "${bgdest}"
  echo "--> Updating lightdm configuration file"
  [ -w "${cfgdest}" ] && cp "${cfgfrom}" "${cfgdest}" || sudo cp "${cfgfrom}" "${cfgdest}"
}

function setup_lightdm_cfg () {
  local current_dir="$1"
  local diff="${current_dir}/lightdm/lightdm.diff"
  local to="/usr/share/xgreeters/lightdm-gtk-greeter.desktop"
  if ! patch -R -p0 -s -f --dry-run "${to}" "${diff}" &>/dev/null; then
    echo "--> Updating lightmdm config at ${to}"
    [ -w "${to}" ] && patch "${to}" "${diff}" -s &>/dev/null || sudo patch "${to}" "${diff}" -s &>/dev/null
  else
    echo ":: Lightdm already patched"
  fi
}

function setup_lightdm () {
  local current_dir="$1"
  setup_lightdm_bg "${current_dir}"
  setup_lightdm_cfg "${current_dir}"
}

function main () {
  local pwd
  pwd="$(pwd)"

  setup "${pwd}" "xorg/xprofile" "$HOME/.xprofile"        # Setup symlink for xprofile
  setup "${pwd}" "urxvt/urxvt.conf" "$HOME/.Xdefaults"    # Setup symlink for urxvt
  setup "${pwd}" "terminator" "$HOME/.config/terminator"  # Setup symlink for terminator
  setup "${pwd}" "termite" "$HOME/.config/termite"        # Setup symlink for termite
  setup "${pwd}" "wallpapers" "$HOME/.config/wallpapers"  # Setup symlink for wallpapers
  setup "${pwd}" "xmobar" "$HOME/.xmobar"                 # Setup symlink for xmobar

  configure_xmobar "${pwd}" # Configure xmobar
  setup_neovim "${pwd}"
  setup_xmonad "${pwd}"
  setup_zsh "${pwd}"
  setup_lightdm "${pwd}"
}

main
