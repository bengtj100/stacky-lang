#!/bin/bash
readonly PNAME=$(basename "$0")

readonly SOURCE_DIR=~/src
readonly PROJECT='stacky-lang'
readonly REPO="git@github.com:bengtj100/${PROJECT}.git"

message()
{
    echo
    echo '================================================================================'
    echo "==  $*"
    echo '================================================================================'
    echo
}

run()
{
    echo "Executing: $*"
    "$@"; local ret=$?

    if (( ret > 0 )) ; then
        echo "COMMAND: '$*' failed with return value: $ret. Exiting!"
        exit $ret
    fi

    return 0
}

message "Update packages and update the system"
run sudo apt update
run sudo apt upgrade -y

message "Install needed dependencies for Haskell and documentation"

run sudo apt install -y ghc ghc-prof ghc-doc cabal-install git sudo \
                        zlib1g-dev pkg-config build-essential \
                        pandoc texlive curl vim

message "Clone the Stacky repo"
run mkdir "${SOURCE_DIR}"
run cd "${SOURCE_DIR}"
run git clone "${REPO}"

message "Set up the build system"
run cabal update
run cabal install cabal-install
run ~/.cabal/bin/cabal install hasktags

message "DONE!"

