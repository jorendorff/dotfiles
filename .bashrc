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

# CVS settings
export CVS_RSH=ssh

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

function mach() {
    $((hg root 2>/dev/null) || git rev-parse --show-toplevel)/mach "$@"
}
. $HOME/work/mozilla-central/python/mach/bash-completion.sh

export WORDCOUNT_FILE=io.md
export WORDCOUNT_REV=`cd ~/work/rustbook/atlas && git rev-parse HEAD`
function wordcount() {
    echo $((`cat $WORDCOUNT_FILE | wc -w` - `git show $WORDCOUNT_REV:$WORDCOUNT_FILE | wc -w`))
}

function rfdgrep() {
    (
        cd ~/work/oxidecomputer/rfd/rfd &&
            ls 0*/README.{md,adoc} | xargs grep "$@"
    )
}


. ~/.cargo/env  # enable rustup
alias rusti="(cd $HOME/work/rusti && cargo run)"

alias copy-minefield-pid='ps auxww | grep '\''./firefox-bin -P'\'' | grep -v grep | awk '\''{pid = $2; count++} END { if (count == 1) { print "attach " pid; } else { print "ERROR"; } }'\'' | pbcopy'
alias gdb-minefield='`ps auxww | grep Minefield | grep -v grep | awk "{print \"gdb \" \\$11 \" \" \\$2}"`'
export CVS_RSH=ssh
export JAVA_HOME=`/usr/libexec/java_home`

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

echo starting rvm...
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

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

export MOZCONFIG=$HOME/work/mozconfigs/debug

# smoosh-tools!
export PATH="${PATH}:${HOME}/work/smoosh-tools/bin"

# opam configuration
source "$HOME/.opam/opam-init/init.sh"

export SCHEME=~/play/cell-gc/target/release/lisp
alias scheme='rlwrap $SCHEME'

export DJS=~/work/mozilla-central/build_DBG.OBJ/dist/bin/js
export TJS=~/work/mozilla-central/build_TEST.OBJ/dist/bin/js

export EDITOR=vim
export GIT_EDITOR=emacs -nw

alias msbuild="mono $HOME/play/dafny/Microsoft.Build.Mono.Debug.14.1.0.0-prerelease/lib/MSBuild.exe"

shopt -s histappend
