# Dotfiles

My configuration files.

Quick setup: just run

```sh

perl setup.pl

```

This will create symlinks to all plain files matching `/^./` into the home directory. Next, open Emacs and wait for all the packages to install. (This will take some time.)

## Enabling 24-Bit Color in macOS

Run the following:

    tic -x -o ~/.terminfo terminfo-24bit.src

Then add this line to your `.zshrc`:

    TERM=xterm-24bit; export TERM

