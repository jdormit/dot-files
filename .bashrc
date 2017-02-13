# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export TERM=xterm

PATH=$PATH:/usr/lib/postgresql/9.4/bin/
PATH=$PATH:~/eclipse/java-neon/eclipse
PATH=$PATH:~/bin

export GOPATH=~/golang
PATH=$PATH:~/golang/bin

source ~/bin/git-completion.bash

if [ -z "$EMACS" ]; then
    alias emacs="em"
    export EDITOR="em"
else
    export EDITOR="emacs -Q"
fi
   
# display git info in prompt
source ~/bin/git-prompt.sh
PS1='$(__git_ps1 " (%s)")'
export GIT_PS1_SHOWDIRTYSTATE="1"
export GIT_PS1_SHOWSTASHSTATE="1"
export GIT_PS1_SHOWUNTRACKEDFILES="1"
export GIT_PS1_SHOWUPSTREAM="auto"

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]'"$PS1"'\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w'"$PS1"'\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

alias say='espeak'

bloomwikissh() {
    ssh -i ~/bloom/BloomWiki.pem ubuntu@ec2-52-32-180-194.us-west-2.compute.amazonaws.com
}

smbuildall() {
    (
    mvnclean ~/sampleminded/clinitraq ~/sampleminded/astellas ~/sampleminded/eyegene ~/sampleminded/exactsciences
    )
}

littleserverssh() {
    ssh jeremy@littleserver.ddns.net
}

alias webstorm="/opt/webstorm/webstorm-latest"

###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#
# for BASH
if type complete &>/dev/null; then
    _npm_completion () {
        local words cword
        if type _get_comp_words_by_ref &>/dev/null; then
            _get_comp_words_by_ref -n = -n @ -w words -i cword
        else
            cword="$COMP_CWORD"
            words=("${COMP_WORDS[@]}")
        fi

        local si="$IFS"


        # if your npm command includes `install` or `i `
        if [[ ${words[@]} =~ 'install' ]] || [[ ${words[@]} =~ 'i ' ]]; then
            local cur=${COMP_WORDS[COMP_CWORD]}

            # supply autocomplete words from `~/.npm`, with $cur being value of current expansion like 'expre'
            COMPREPLY=( $( compgen -W "$(ls ~/.npm )" -- $cur ) )
        else
            IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                COMP_LINE="$COMP_LINE" \
                COMP_POINT="$COMP_POINT" \
                npm completion -- "${words[@]}" \
                2>/dev/null)) || return $?
        fi

        IFS="$si"
    }
    complete -o default -F _npm_completion npm
# for ZSH
elif type compdef &>/dev/null; then
    _npm_completion() {
        local si=$IFS


        # if your npm command includes `install`
        if [[ ${words} =~ 'install' ]] || [[ ${words} =~ 'i ' ]]; then
            compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                COMP_LINE=$BUFFER \
                COMP_POINT=0 \
                ls ~/.npm -- "${words[@]}" \
                2>/dev/null)

        else
            compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                COMP_LINE=$BUFFER \
                COMP_POINT=0 \
                npm completion -- "${words[@]}" \
                2>/dev/null)
        fi

        IFS=$si
    }
    compdef _npm_completion npm
elif type compctl &>/dev/null; then

    _npm_completion () {
        local cword line point words si
        read -Ac words
        read -cn cword
        let cword-=1
        read -l line
        read -ln point
        si="$IFS"
        IFS=$'\n' reply=($(COMP_CWORD="$cword" \
            COMP_LINE="$line" \
            COMP_POINT="$point" \
            npm completion -- "${words[@]}" \
            2>/dev/null)) || return $?
        IFS="$si"
    }
    compctl -K _npm_completion npm
fi
###-end-npm-completion-###

man() {
    LESS_TERMCAP_mb=$'\e'"[1;31m" \
    LESS_TERMCAP_md=$'\e'"[1;31m" \
    LESS_TERMCAP_me=$'\e'"[0m" \
    LESS_TERMCAP_se=$'\e'"[0m" \
    LESS_TERMCAP_so=$'\e'"[1;44;33m" \
    LESS_TERMCAP_ue=$'\e'"[0m" \
    LESS_TERMCAP_us=$'\e'"[1;32m" \
    command man "$@"
}

shopt -s globstar
shopt -s extglob

export AUTO_NTFY_DONE_UNFOCUSED_ONLY=-b
eval "$(ntfy shell-integration)"

export PYTHONSTARTUP=~/.pythonrc
export WORKON_HOME=~/.envs
export PROJECT_HOME=~/python_projects
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/local/bin/virtualenvwrapper.sh

source /home/jeremy/bin/tmux_autocompletion.sh
alias tmux="tmux -2"

eval "$(thefuck --alias)"

ngroksshtunnel() {
    ngrok tcp --remote-addr=1.tcp.ngrok.io:20663 22
}

alias git=hub
source ~/hub/etc/hub.bash_completion.sh
alias sampleminded-log="tail -n500 -f ~/sampleminded/eclipse.log"
alias octave="octave --no-gui"
alias open=xdg-open
alias sudo='sudo '
alias mail-html="mail -a 'Content-Type: text/html'"
alias ngrok-sampleminded="ngrok http --subdomain=sampleminded 8080"
alias google='BROWSER=lynx googler'
alias copy='xclip -selection clipboard'
PYTHONSTARTUP=~/pythonrc/pythonrc.py
