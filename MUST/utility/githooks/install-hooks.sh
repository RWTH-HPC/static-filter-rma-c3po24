#!/bin/bash

HOOKS=(commit-msg)

REPO_ROOT=$(git rev-parse --show-toplevel)
GIT_HOOK_DIR="${REPO_ROOT}/.git/hooks"
echo "Installing hooks into: ${GIT_HOOK_DIR}"

for hook in ${HOOKS[@]}
do
    install --verbose --backup --compare --target-directory="${GIT_HOOK_DIR}" "${REPO_ROOT}/utility/githooks/${hook}"
done
