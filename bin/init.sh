#!/bin/bash
set -ex

SCRIPT_PATH=$(cd $(dirname $0); pwd)

$SCRIPT_PATH/deploy.sh
$SCRIPT_PATH/install.sh

