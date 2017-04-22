export LANG=ja_JP.UTF-8;
export EDITOR=$HOME/.bashrc.d/emacs-nw;
export HISTTIMEFORMAT='%Y-%m-%d %T ';
export HISTSIZE=10000;
export HISTFILESIZE=10000;

function share_history {
    history -a
    history -c
    history -r
}
PROMPT_COMMAND='share_history'
shopt -u histappend

if [ ${TERM} = "dumb" ] ; then
    export PAGER="";
else
    export PAGER=less;
fi
                                                                                                                                                                             
#-----------------------------------------------------------------------
# for command line
#-----------------------------------------------------------------------
if [ `uname` = Linux ]; then
    if [ ${TERM} = "dumb" ] ; then
        alias ls="ls -F"
        alias ll="ls -l"
        alias la="ls -Fa"
    else
        alias ls="ls --color -F"
        alias ll="ls --color -l"
        alias la="ls --color -Fa"
    fi 
else
    alias ls="ls -F"
    alias ll="ls -l"
    alias la="ls -Fa"
fi

alias e="emacs -nw"
alias rm="rm -f"
