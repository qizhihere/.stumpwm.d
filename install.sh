#!/usr/bin/env bash

INSTALL_PATH=$HOME/Softs/stumpwm

QUICKLISP_FILE=$HOME/.quicklisp/quicklisp.lisp
INSTALL_LISP=$(dirname $0)/install.lisp
LISP_INTERPRETER=sbcl

build () {
    autoconf && ./configure &&
        make && sudo make install && {
            echo "StumpWM installed! You can now add this to your .xinirc:"
            echo "                      exec stumpwm                      "
        }
}

mkdir -p "$(dirname $QUICKLISP_FILE)"

# install quicklisp
[ -f "$QUICKLISP_FILE" ] || curl -o $QUICKLISP_FILE https://beta.quicklisp.org/quicklisp.lisp

# run install.lisp script
! type -a $LISP_INTERPRETER &>/dev/null && {
    echo "Lisp interpreter $LISP_INTERPRETER not found. Please install it before running this script."
    exit 1
}

if [ -f "$QUICKLISP_FILE" ] && [ -f "$INSTALL_LISP" ]; then
    $LISP_INTERPRETER --load "$QUICKLISP_FILE" --script $INSTALL_LISP
fi

# build stumpwm
! type -a stumpwm &>/dev/null && {
    if [ ! -d "$INSTALL_PATH" ]; then
        mkdir -p "$(dirname $INSTALL_PATH)"
        cd "$(dirname $INSTALL_PATH)" &&
            git clone https://github.com/stumpwm/stumpwm "$INSTALL_PATH" &&
            cd "$INSTALL_PATH" && build
    else
        cd "$INSTALL_PATH" && build
    fi
}
