#!/bin/sh

trap bye_bye EXIT

CONFIG_DIR="$HOME/configs"
SUDO_PROGRAM="/usr/bin/doas"

bye_bye () {
	echo "\naborting..."
}

echoerr () {
	echo $@ 1>&2
}

echo_help () {
	echo "usage: install.sh [-c dir] [-s sudo]"
	echo "\t\t-c | --config-dir: Specify the directory containing the configuration files"
	echo "\t\t-s | --sudo-program: Specify the program to elevate priviledges when needed"
}

prompt () {
	if [ -z "$1" ]; then
		echoerr "prompt: No message specified"
		return 11	
	fi
	echo -n "$1"
	read yn
	case "${yn}" in
		[Yy]*) return 0;;
		*) return 1;;
	esac
}

prompt_continuation () {
	echo "No parameters were specified, using default values:"
	echo "\tCONFIG DIR:	${CONFIG_DIR}"
	echo "\tSUDO_PROGRAM:	${SUDO_PROGRAM}"
	prompt "Do you want to procede with these values? [y/N]"
}

parse_args () {
	if [ "$#" -eq 0 ]; then
		prompt_continuation && return 0 || exit 1
	fi
	while [ "$#" -gt 0 ]; do
		case "$1" in
			-h|--help)
				echo_help
				exit 0;;
			-c|--config-dir)
				if [ -z "$2" ] || [[ "$2" == "-*" ]]; then
					echoerr "Expected argument after config-dir option"
					exit 3
				fi
				CONFIG_DIR="$2"
				shift;;
			-s|--sudo-program)
				if [ -z "$2" ] || [[ "$2" == "-*" ]]; then
					echoerr "Expected argument after sudo-program option"
					exit 3
				fi
				SUDO_PROGRAM="$2"
				shift;;
			*)
				echoerr "Unknown argument $1"
				exit 2;;
		esac
		shift
	done
}

write_file () {
	test "$#" -ne 2 && echoerr "write_file: orig and dest must be specified"
	if [ -w "$2" ]; then
		cat "$1" > "$2"
	else
		cat "$1" | "${SUDO_PROGRAM}" tee "$2"
	fi
}

write_config () {
	test "$#" -ne 2 && echoerr "write_config: orig and dest must be specified"
	echo "--> Copying $1 to $2"
	write_file "$1" "$2"
}

setup_ksh () {
	echo "-> Copying ksh configuration"
	write_config "${CONFIG_DIR}/ksh/kshrc"		"$HOME/.kshrc"
	write_config "${CONFIG_DIR}/ksh/profile"	"$HOME/.profile"
	echo "[+] Copied ksh configuration"
}

setup_doas () {
	echo "-> Copying doas configuration"
	write_config "${CONFIG_DIR}/doas/conf" "/etc/doas.conf"
	echo "[+] Copied doas configuration"
}

setup_wsconsctl () {
	echo "-> Copying wsconsctl configuration"
	write_config "${CONFIG_DIR}/wsconsctl/conf" "/etc/wsconsctl.conf"
	echo "[+] Copied wsconsctl configuration"
}

main () {
	setup_ksh
	setup_doas
	setup_wsconsctl
}

set -e
parse_args $@
main
set +e
