# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Strip RVM directories out of PATH so that the rvm script later on will reinstate them
# (RVM needs to be before /usr/local/bin in the PATH, but /usr/local/bin needs to be
# before most other stuff...)
export PATH=$(echo $PATH | sed 's/\(^\|:\)\([^:]*\/.rvm\/[^:]*:\)*/\1/g')
export PATH=$(echo $PATH | sed 's/\(^\|:\)[^:]*\/.rvm\/[^:]*$//g')

# Add custom builds directory, odds and ends.
# Remove them first so PATH doesn't get longer and longer as we nest interactive shells.
PATH=$(echo $PATH|sed 's/\(^\|:\)usr\/local\/bin:/\1/g')
export PATH=/usr/local/bin:${PATH}
for p in "${HOME}/prefix/bin" \
             "${HOME}/bin" \
             "${HOME}/work/dotfiles/myscripts" \
             "${HOME}/.mozbuild/git-cinnabar" \
             "/usr/local/sbin" \
             "/usr/local/share/dotnet" \
             "$HOME/.cabal/bin" \
             "$HOME/.elan/bin" \
             "$HOME/work/phacility/arcanist/bin"; do
    pr=$(echo "$p" | /usr/bin/sed 's/\//\\\//g')
    PATH=$(echo "$PATH" | /usr/bin/sed 's/\(^\|:\)'"$pr"':/\1/g')
    export PATH="${PATH}:${p}"
done

function bug() {
    open "http://bugzilla.mozilla.org/show_bug.cgi?id=$@"
}

function gg() {
    git log --graph --all --decorate "$@"
}

function grep-c() {
    find . -name '*.c' -o -name '*.cpp' -o -name '*.h' | xargs grep "$@"
}

function grep-make() {
    find . -name '*.mk' -o -name 'Makefile*' | xargs grep "$@"
}

function minefield-debug() {
    dir="`hg root`/obj-ff-debug/dist/MinefieldDebug.app/Contents/MacOS"
    (cd "$dir" && "$dir/firefox" $@)
}

function minefield-release() {
    dir="`hg root`/obj-ff-release/dist/Minefield.app/Contents/MacOS"
    (cd "$dir" && "$dir/firefox" $@)
}

if [ "$TERM" == dumb ]; then
    export PS1='\w\$ '
else
    # Long line of yellow equal signs, and prompt in bold.
    # All this is just to make it easier to find the beginning/end of
    # long command output.
    export PS1='\n\[\e[33;1m\]================================================================================ \D{%F %T}\[\e[39m\]\n\[\e[1m\]\w\$\[\e[0m\] '
fi

function snappy() {
    export PS1='\[\e[1m\]\w\$\[\e[0m\] '
}

function repeat() {
    repeat_count=$1
    shift
    for ((i=0; $i<$repeat_count; i++)); do
        "$@" || return
    done
}

function rfdgrep() {
    (
        cd ~/work/oxidecomputer/rfd/rfd &&
            ls 0*/README.{md,adoc} | xargs grep "$@"
    )
}

. ~/.cargo/env  # enable rustup

alias es6draft='$HOME/work/es6draft/bin/es6draft'

alias dafny='$HOME/play/dafny-lang/dafny/Binaries/dafny'
#alias dafny="mono $HOME/play/dafny/dafny-3.0.0pre1-prebuilt/dafny/Dafny.exe"

# command-line utility hack: `ok &&` means "if the previous command
# succeded..."  Handy for queueing up more work for the shell to do after the
# current build or whatever.
alias ok='(exit $?)'

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/rustlib/x86_64-unknown-linux-gnu/lib
export CCACHE_COMPRESS=""
export PATH="/usr/local/heroku/bin:$PATH"


# version managers!

echo starting nvm...
. ~/.nvm/nvm.sh

if [ -x "$HOME/.rvm" ]
then
    echo starting rvm...

    # Add RVM to PATH for scripting
    export PATH="$PATH:$HOME/.rvm/bin"

    # Load RVM into a shell session *as a function*
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
fi

if [ "x"`which exenv` != "x" ]
then
    echo starting exenv...
    # The following is the output of `exenv init -`; see `~/.exenv/README.md`.
    export PATH="/home/jorendorff/.exenv/shims:${PATH}"
    source "/home/jorendorff/.exenv/libexec/../completions/exenv.bash"
    exenv rehash 2>/dev/null
    exenv() {
      local command="$1"
      if [ "$#" -gt 0 ]; then
        shift
      fi

      case "$command" in
      shell)
        eval `exenv "sh-$command" "$@"`;;
      *)
        command exenv "$command" "$@";;
      esac
    }
fi

export SCHEME=~/src/cell-gc/target/release/lisp
alias scheme='rlwrap $SCHEME'

export EDITOR=vim
export GIT_EDITOR='emacs -nw'

alias msbuild="mono $HOME/src/dafny/Microsoft.Build.Mono.Debug.14.1.0.0-prerelease/lib/MSBuild.exe"

shopt -s histappend
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Created by `pipx` on 2021-09-18 12:36:14
export PATH="$PATH:/home/jorendorff/.local/bin"
