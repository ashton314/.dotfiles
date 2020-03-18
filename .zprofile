if [ $TERM = tramp ]; then
        unset RPROMPT
        unset RPS1
        PS1="$ "
        unsetopt zle
        unsetopt rcs  # Inhibit loading of further config files
fi
