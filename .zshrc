# .zshrc - For some reason PS1 has to be set here.

# Configure zsh to bash-like behavior for Option+Delete and such.
autoload -U select-word-style
select-word-style bash

# Configure zsh to talk to git (to show the current branch in the prompt).
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%f '
zstyle ':vcs_info:*' enable git

# The prompt. Current date and time, red exit code if previous command failed; prompt
# blue and in bold; incorporate "$vcs_info_msg_0_" from the above code.
export PS1='%0(?.%F{green}.%F{red}[error:%?] )%D{%F %T}%f %F{blue}%B%~ $vcs_info_msg_0_❯%b%f '

# Add ruby stuff to the PATH.
eval "$(rbenv init -)"


# Aliases

function blackbirdctl() {
    (cd "$HOME/src/blackbird" &&
         cargo run -p blackbirdctl --release -- "$@")
}

function blackbird-server() {
    (cd "$HOME/src/blackbird" &&
         cargo run -p blackbird-server --release -- "$@")
}

alias bbctl=blackbirdctl
alias bb=blackbird-client

function ggu() {
    git grep -ho -P "$@" | sort | uniq -c
}

# command-line utility hack: `ok &&` means "if the previous command
# succeded..."  Handy for queueing up more work for the shell to do after the
# current build or whatever.
alias ok='(exit $?)'

# completions. `-C` silences a warning; compinit doesn't like that
# /usr/local/share is group-writable.
fpath+=~/.zfunc
autoload -U compinit; compinit -C
