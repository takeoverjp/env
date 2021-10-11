#!/bin/bash -eux

function install_go_task() {
    sudo sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin
}

if ! type task >/dev/null; then
    install_go_task
fi

task provision
