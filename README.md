# Dotfiles

My configuration files.

Quick setup: just run

```sh

perl setup.pl

```

This will create symlinks to all plain files matching `/^./` into the home directory. Next, open Emacs and run `M-x package-list-packages`, then run `install-my-packages`. This should run through everything necessary and install all the missing packages.

## Enabling 24-Bit Color in macOS

Run the following:

    tic -x -o ~/.terminfo terminfo-24bit.src

Then add this line to your `.zshrc`:

    TERM=xterm-24bit; export TERM

