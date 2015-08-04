# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Add custom builds directory, then MacPorts install directory; also ~/prefix/bin and ~/bin.
# This causes the PATH to get longer and longer as we nest interactive shells, alas.
export PATH=/usr/local/bin:${PATH}
export PATH=${PATH}:${HOME}/prefix/bin:${HOME}/bin:${HOME}/dev/dotfiles/myscripts
export PATH=${PATH}:$HOME/dev/node_modules/docco/bin
export PATH=${PATH}:$HOME/.cabal/bin

# CVS setings
export CVS_RSH=ssh

export JS="$HOME/dev/gecko/js/src/od-obj/dist/bin/js"
export DJS="$HOME/dev/gecko/js/src/d-obj/dist/bin/js"

function bug() {
    open "http://bugzilla.mozilla.org/show_bug.cgi?id=$@"
}

function es6draft() {
    bash "$HOME/dev/es6draft/bin/es6draft" "$@"
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

alias copy-minefield-pid='ps auxww | grep '\''./firefox-bin -P'\'' | grep -v grep | awk '\''{pid = $2; count++} END { if (count == 1) { print "attach " pid; } else { print "ERROR"; } }'\'' | pbcopy'
alias gdb-minefield='`ps auxww | grep Minefield | grep -v grep | awk "{print \"gdb \" \\$11 \" \" \\$2}"`'
alias clojure="java -Xms64m -Xmx1g -cp $HOME/dev/clojure/clojure-1.3.0/clojure-1.3.0.jar clojure.main"
export CVS_RSH=ssh
##export JAVA_HOME=`/usr/libexec/java_home`

alias ffcal='~/apps/firefox/firefox -no-remote -P cal &'
alias ffdefault='firefox -no-remote -P default &'  # use shipping ff to avoid okta freeze
alias ffrefuge='~/apps/firefox/firefox -no-remote -P refuge &'
alias ffall='ffcal; ffdefault; ffrefuge'

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/rustlib/x86_64-unknown-linux-gnu/lib
