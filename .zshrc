# Bail out of rest of setup if we're coming in from TRAMP
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Add homebrew completions
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

# Load personal configurations
if [[ -r ~/.zsh_personal ]]; then
    . ~/.zsh_personal
fi

# Load local configuration
if [[ -r ~/.zsh_local_conf ]]; then
    . ~/.zsh_local_conf
fi

# Setup for when Emacs native-comp is turned on
if [ -f "/Applications/Emacs.app/Contents/MacOS/Emacs" ]; then
  export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
  alias emacs="$EMACS"
  alias e="$EMACS -nw"
fi

if [ -f "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" ]; then
  alias ec="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
fi

# Completion stuff
zstyle :compinstall filename '/Users/ashton/.zshrc'
autoload -Uz compinit
compinit

# History
setopt HIST_IGNORE_DUPS
SHARE_HISTORY=true
HISTORY_IGNORE='(fg|ll|jobs|cd|j|l|exit)'
bindkey -e

# HSTR configuration - add this to ~/.zshrc
alias hh=hstr                    # hh to be alias for hstr
setopt histignorespace           # skip cmds w/ leading space from history
# export HSTR_CONFIG=hicolor       # get more colors
bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)

fpath=(/usr/local/share/zsh-completions $fpath)

setopt PROMPT_SUBST

git_prompt_info () {
    local ref
    ref=$(git symbolic-ref HEAD 2> /dev/null) || ref=$(git rev-parse --short HEAD 2> /dev/null) || return 0

    local STATUS
    local -a FLAGS

    FLAGS=('--porcelain')

    if [[ "${DISABLE_UNTRACKED_FILES_DIRTY:-}" == "true" ]]
    then
	FLAGS+='--untracked-files=no'
    fi
    case "${GIT_STATUS_IGNORE_SUBMODULES:-}" in
	(git)  ;;
	(*) FLAGS+="--ignore-submodules=${GIT_STATUS_IGNORE_SUBMODULES:-dirty}"  ;;
    esac

    STATUS=$(git status ${FLAGS} 2> /dev/null | tail -n1)

    if [[ -n $STATUS ]]
    then
	echo " (%F{yellow}${ref#refs/heads/}%f)%F{red}!%f"
    else
	echo " (%F{yellow}${ref#refs/heads/}%f)"
    fi

}

# Add "%m" to print the short hostname
PROMPT="%(?:%F{green}⊢:%F{red}!%?)%f %F{cyan}%~%f\$(git_prompt_info) %(!:# :)"
