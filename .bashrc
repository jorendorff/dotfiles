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
             "${HOME}/dev/dotfiles/myscripts" \
             "$HOME/dev/node_modules/docco/bin" \
             "$HOME/.cabal/bin"; do
    pr=$(echo "$p" | /usr/bin/sed 's/\//\\\//g')
    PATH=$(echo "$PATH" | /usr/bin/sed 's/\(^\|:\)'"$pr"':/\1/g')
    export PATH="${PATH}:${p}"
done


# CVS settings
export CVS_RSH=ssh

export JS="$HOME/dev/gecko/js/src/od-obj/dist/bin/js"
export DJS="$HOME/dev/gecko/js/src/d-obj/dist/bin/js"

function bug() {
    open "http://bugzilla.mozilla.org/show_bug.cgi?id=$@"
}

function es6draft() {
    bash "$HOME/dev/es6draft/bin/es6draft" "$@"
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
    # Show prompt in bold to make it easier to find the beginning/end of
    # long command output.
    export PS1='\[\e[1m\]\w\$\[\e[0m\] '
fi

function snappy() {
    export PS1='\[\e[1m\]\w\$\[\e[0m\] '
}

function repeat() {
    repeat_count=$1
    shift
    for ((i=0; $i<$repeat_count; i++)); do
        "$@" || break
    done
}

function mach() {
    `hg root`/mach "$@"
}

. ~/.nvm/nvm.sh

alias rusti="(cd $HOME/dev/rusti && cargo run)"

alias copy-minefield-pid='ps auxww | grep '\''./firefox-bin -P'\'' | grep -v grep | awk '\''{pid = $2; count++} END { if (count == 1) { print "attach " pid; } else { print "ERROR"; } }'\'' | pbcopy'
alias gdb-minefield='`ps auxww | grep Minefield | grep -v grep | awk "{print \"gdb \" \\$11 \" \" \\$2}"`'
export CVS_RSH=ssh
##export JAVA_HOME=`/usr/libexec/java_home`

alias ffc='~/apps/firefox/firefox -no-remote -P cal'
alias ffd='firefox -no-remote -P default'  # use shipping ff to avoid okta freeze
alias ffr='~/apps/firefox/firefox -no-remote -P refuge'

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/rustlib/x86_64-unknown-linux-gnu/lib
export CCACHE_COMPRESS=""
export PATH="/usr/local/heroku/bin:$PATH"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
