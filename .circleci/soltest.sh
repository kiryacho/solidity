#! /bin/bash
#------------------------------------------------------------------------------
# Bash script to execute the Solidity tests by CircleCI.
#
# The documentation for solidity is hosted at:
#
#     https://solidity.readthedocs.org
#
# ------------------------------------------------------------------------------
# Configuration Environment Variables:
#
#     EVM=version_string      Specifies EVM version to compile for (such as homestead, etc)
#     OPTIMIZE=1              Enables backend optimizer
#     ABI_ENCODER_V2=1        Enables ABI encoder version 2
#
# ------------------------------------------------------------------------------
# This file is part of solidity.
#
# solidity is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# solidity is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with solidity.  If not, see <http://www.gnu.org/licenses/>
#
# (c) 2016 solidity contributors.
#------------------------------------------------------------------------------
set -ex

OPTIMIZE=${OPTIMIZE:-"0"}
EVM=${EVM:-"invalid"}
WORKDIR=${CIRCLE_WORKING_DIRECTORY:-.}
ALETH_PATH="/usr/bin/aleth"

safe_kill() {
    local PID=${1}
    local NAME=${2:-${1}}
    local n=1

    # only proceed if $PID does exist
    kill -0 $PID 2>/dev/null || return

    echo "Sending SIGTERM to ${NAME} (${PID}) ..."
    kill $PID

    # wait until process terminated gracefully
    while kill -0 $PID 2>/dev/null && [[ $n -le 4 ]]; do
        echo "Waiting ($n) ..."
        sleep 1
        n=$[n + 1]
    done

    # process still alive? then hard-kill
    if kill -0 $PID 2>/dev/null; then
        echo "Sending SIGKILL to ${NAME} (${PID}) ..."
        kill -9 $PID
    fi
}

function run_aleth()
{
    $ALETH_PATH --db memorydb --test -d "${WORKDIR}" &> /dev/null &
    echo $!

    # Wait until the IPC endpoint is available.
    while [ ! -S "${WORKDIR}/geth.ipc" ] ; do sleep 1; done
    sleep 2
}
ALETH_PID=$(run_aleth)

function cleanup() {
	safe_kill $ALETH_PID $ALETH_PATH
}
trap cleanup INT TERM

flags=""
test "${OPTIMIZE}" = "1" && flags="${flags} --optimize"
test "${ABI_ENCODER_V2}" = "1" && flags="${flags} --abiencoderv2 --optimize-yul"

mkdir -p test_results
logger="--logger=JUNIT,error,test_results/$EVM.xml"
./build/test/soltest $logger -- --evm-version=$EVM --ipcpath "${WORKDIR}/geth.ipc" $flags
