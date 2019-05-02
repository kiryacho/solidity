#!/usr/bin/env bash

set -e

## GLOBAL VARIABLES

REPO_ROOT=$(cd $(dirname "$0")/.. && pwd)

## FUNCTIONS

if [ "$CIRCLECI" ]
then
    function printTask() { echo "$(tput bold)$(tput setaf 2)$1$(tput setaf 7)"; }
    function printError() { echo "$(tput setaf 1)$1$(tput setaf 7)"; }
else
    function printTask() { echo "$(tput bold)$(tput setaf 2)$1$(tput sgr0)"; }
    function printError() { echo "$(tput setaf 1)$1$(tput sgr0)"; }
fi

printTask "Checking docs examples style…"
(
    set -e
    cd "$SOLTMPDIR"

    if node -v >/dev/null
    then
        if npm list -g | grep solhint >/dev/null
        then
            echo "node is installed, setting up solhint"
            cp "$REPO_ROOT"/test/.solhint.json "$SOLTMPDIR"/.solhint.json
        else
            echo "node is installed, installing solhint"
            cp "$REPO_ROOT"/test/.solhint.json "$SOLTMPDIR"/.solhint.json
        fi
    else
        echo "node not installed, skipping docs style checker"
    fi

    for f in *.sol
    do
        if npm list -g | grep solhint >/dev/null
        then
            # Only report errors
            solhint -q -f unix "$SOLTMPDIR/$f"
        fi
    done
)
rm -rf "$SOLTMPDIR"
echo "Done."
