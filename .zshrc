# .zshrc - For some reason PS1 has to be set here.

# Configure zsh to talk to git (to show the current branch in the prompt).
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%f '
zstyle ':vcs_info:*' enable git

# The prompt. Current date and time, red exit code if previous command failed; prompt
# blue and in bold; incorporate "$vcs_info_msg_0_" from the above code.
export PS1='%0(?.%F{green}.%F{red}%? - )%D{%F %T}%f %F{blue}%B%~ $vcs_info_msg_0_❯%b%f '
