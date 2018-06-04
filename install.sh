#!/bin/bash

#--------------------------------------------------------------------
# Configuration variables
#--------------------------------------------------------------------
PROJECTS=""
ZSH=true
UDEV=true
XMONAD=true
XMOBAR=true
TERMINATOR=true
TERMITE=true
ALACRITTY=true
URXVT=true
XPROFILE=true
NEOVIM=true
LIGHTDM=true
WALLPAPERS=true

#--------------------------------------------------------------------
# Helper functions
#--------------------------------------------------------------------
function parse_args(){
  while (( "$#" )); do
    case $1 in
      -p|--projects-dir)
        if [ -z $2 ] || [[ $2 == -* ]]; then
          echoerr "Expected argument after output file option" && exit 2
        fi
        PROJECTS=$2
        shift;;

      --no-zsh)
        ZSH=false;;

      --no-udev)
        UDEV=false;;

      --no-xmonad)
        XMONAD=false;;

      --no-xmobar)
        XMOBAR=false;;

      --no-terminator)
        TERMINATOR=false;;

      --no-termite)
        TERMITE=false;;

      --no-alacritty)
        ALACRITTY=false;;

      --no-urxvt)
        URXVT=false;;

      --no-xprofile)
        XPROFILE=false;;

      --no-neovim)
        NEOVIM=false;;

      --no-lightdm)
        LIGHTDM=false;;

      --no-wallpapers)
        WALLPAPERS=false;;

      -h|--help)
        display_help
        exit 0;;

      *)
        echoerr "Unknow argument $1" && exit 2;;
    esac
    shift
  done

  if [ -z $PROJECTS ] && $ZSH && $NEOVIM; then
    echoerr "Error: Projects must be specified to setup neovim and zsh"
    echoerr "Use --projects-dir or disable neovim and zsh setup with --no-zsh and --no-neovim"
    exit 2
  fi
  return 0
}

function display_help(){
    echo
    echo "Usage: ./install.sh [OPTIONS]..."
    echo "Setup all desktop configurations"
    echo
    echo "Options:"
    echo -e "\t-p, --projects-dir\t\tfull path to projects directory"
    echo -e "\t--no-lightdm\t\t\tdo not install lightdm configurations"
    echo -e "\t--no-zsh\t\t\tdo not install zsh configurations"
    echo -e "\t--no-xmonad\t\t\tdo not install xmonad configurations"
    echo -e "\t--no-xmobar\t\t\tdo not install xmobar configurations"
    echo -e "\t--no-terminator\t\t\tdo not install terminator configurations"
    echo -e "\t--no-termite\t\t\tdo not install termite configurations"
    echo -e "\t--no-alacritty\t\t\tdo not install alacritty configurations"
    echo -e "\t--no-neovim\t\t\tdo not install neovim configurations"
    echo -e "\t--no-urxvt\t\t\tdo not create Xdefaults"
    echo -e "\t--no-xprofile\t\t\tdo not create xprofile"
    echo -e "\t--no-wallpapers\t\t\tdo not create wallpapers folder"
    echo -e "\t--no-udev\t\t\tdo not install udev configurations"
    echo
    echo "Exit status:"
    echo -e " 0\tif OK"
    echo -e " 1\tif minor problems"
    echo -e " 2\tif serious trouble"
}

function echoerr {
    cat <<< "$@" 1>&2
}


#--------------------------------------------------------------------
# Base functions
#--------------------------------------------------------------------
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
  [ "$1" == false ] && return 1   # Check if --no-[x] was specified
  local current_dir="$2"
  local from="${current_dir}/$3"
  local to="$4"
  create_symlink "${from}" "${to}"
  return 0
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

function in_file () {
  local content="$1"
  local file="$2"
  grep -q "${content}" "${file}"
}

#--------------------------------------------------------------------
# Configuration functions
#--------------------------------------------------------------------
function configure_neovim () {
  update_file "nvim/plugins/fzf-proj.vimrc" "<CODE-PROJECTS>" "${PROJECTS}"
}

function configure_xmobar () {
  local current_dir="$1"
  local top_xmobar="xmobar/top.hs"
  local interface="$(ip addr | grep '[0-9]: .*: ' | cut -d ' ' -f2 | cut -d ':' -f1 | grep -v -E "lo|docker")"
  update_file "${top_xmobar}" "<IF-1>" "${interface}"
  update_file "${top_xmobar}" "<XMOBAR-DIR>" "$HOME/.xmobar"
  update_file "xmobar/scripts/battery.sh" "<XMOBAR-DIR>" "$HOME/.xmobar"
  update_file "xmonad/xmonad.hs" "<XMOBAR-BIN>" "$(which xmobar)"
  update_file "xmonad/xmonad.hs" "<XMOBAR-TOP>" "$HOME/.xmobar/top.hs"
}

function configure_zsh () {
  local current_dir="$1"
  update_file "zsh/zshrc" "<HOME-DIR>" "$HOME"
  update_file "zsh/zshrc" "<CODE-PROJECTS>" "${PROJECTS}"
}

#--------------------------------------------------------------------
# Specific setup functions
#--------------------------------------------------------------------
function setup_xmonad () {
  local current_dir="$1"
  local from="${current_dir}/xmonad/xmonad.hs"
  local to="$HOME/.xmonad/xmonad.hs"
  mkdir -p "$HOME/.xmonad"
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

function setup_udev_rules () {
  local current_dir="$1"
  local udev_dir="/etc/udev/rules.d"
  local udev_rules="${current_dir}/udev-rules"
  echo "--> Adding udev rules"
  sudo cp -r "${udev_rules}/." "${udev_dir}/"
}

function setup_lightdm () {
  local current_dir="$1"
  setup_lightdm_bg "${current_dir}"
  setup_lightdm_cfg "${current_dir}"
}

#--------------------------------------------------------------------
# Main function and script pre-execution preparation
#--------------------------------------------------------------------
function main () {
  local pwd
  pwd="$(pwd)"

  setup "$XPROFILE" "${pwd}" "xorg/xprofile" "$HOME/.xprofile"          # Setup symlink for xprofile
  setup "$URXVT" "${pwd}" "urxvt/urxvt.conf" "$HOME/.Xdefaults"         # Setup symlink for urxvt
  setup "$TERMINATOR" "${pwd}" "terminator" "$HOME/.config/terminator"  # Setup symlink for terminator
  setup "$TERMITE" "${pwd}" "termite" "$HOME/.config/termite"           # Setup symlink for termite
  setup "$ALACRITTY" "${pwd}" "alacritty" "$HOME/.config/alacritty"     # Setup symlink for termite
  setup "$WALLPAPERS" "${pwd}" "wallpapers" "$HOME/.config/wallpapers"  # Setup symlink for wallpapers
  setup "$ZSH" "${pwd}" "zsh/zshrc" "$HOME/.zshrc" &&                   # Setup symlink for zsh
    configure_zsh "${pwd}"                                              # Configure zsh
  setup "$XMOBAR" "${pwd}" "xmobar" "$HOME/.xmobar" &&                  # Setup symlink for xmobar
    configure_xmobar "${pwd}"                                           # Configure xmobar
  setup "$NEOVIM" "${pwd}" "nvim" "$HOME/.config/nvim" &&               # Setup symlink for neovim
    configure_neovim                                                    # Configure neovim
  [ "$XMONAD" == true ] && setup_xmonad "${pwd}"                        # Setup xmonad
  [ "$LIGHTDM" == true ] && setup_lightdm "${pwd}"                      # Setup lightdm
  [ "$UDEV" == true ] && setup_udev_rules "${pwd}"                      # Setup udev rules
}

parse_args $@
main
