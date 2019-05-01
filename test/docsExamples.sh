#!/usr/bin/env bash

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
