# MUST githooks

git hooks that are supposed to ease development with MUST.

## Install

git hooks must be installed manually.

### Install into default hook directory

One way to use these hooks is to simply install them into the default git hook directory.
Simply execute the install script as show below.
Future updates to the hook scripts have to be installed by hand again.

**BEWARE:** This method might overwrite already existing hooks!

```sh
bash install-hooks.sh
```

### Using git config

Requires git >= v2.9. (CLAIX-2018 currently has git v1.8)

A convenient way is to just point git to the directory with the hook scripts in the repo.
This has the advantage of receiving updates to the hooks automatically.

```sh
git config core.hooksPath utility/githooks
```

## Current hooks

### commit-msg

This hook rejects changes of git submodule references.

It is too easy to accidentally update a git submodule ref.
This hook checks that the change you are about to commit leaves git submodules unchanged.
If you really want to change the submodule you have to put `/update-submodule` in the commit message.
