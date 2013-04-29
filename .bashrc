# Add custom builds directory, then MacPorts install directory; also ~/prefix/bin and ~/bin.
# This causes the PATH to get longer and longer as we nest interactive shells, alas.
export PATH=/usr/local/bin:${PATH}
export PATH=${PATH}:${HOME}/prefix/bin:${HOME}/bin:${HOME}/dev/dotfiles/myscripts
export PATH=${PATH}:/usr/local/share/python

# CVS setings
export CVS_RSH=ssh

export JS="$HOME/dev/mi/js/src/r-objdir/js -m -n"

# What you say when your tests fail:
function rats() {
    ls "`hg root`/js/tests"/results-*.html | tail -n 1 | xargs open
}

function bug() {
    open "http://bugzilla.mozilla.org/show_bug.cgi?id=$@"
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

function mercurial-qtop() {
    qtop=`hg qtop 2>/dev/null`
    if [ "$qtop" != "" ]; then
        echo " (+$qtop)"
    fi
}

if [ "$TERM" == dumb ]; then
    export PS1='\w`mercurial-qtop`\$ '
else
    # Show prompt in bold to make it easier to find the beginning/end of
    # long command output.  Add qtop too.
    export PS1='\[\e[1m\]\w`mercurial-qtop`\$\[\e[0m\] '
fi

alias hg-try='hg st -mard | grep . && echo abort: uncommitted changes! || hg push -f ssh://hg.mozilla.org/try/'
alias copy-minefield-pid='ps auxww | grep '\''./firefox-bin -P'\'' | grep -v grep | awk '\''{pid = $2; count++} END { if (count == 1) { print "attach " pid; } else { print "ERROR"; } }'\'' | pbcopy'
alias gdb-minefield='`ps auxww | grep Minefield | grep -v grep | awk "{print \"gdb \" \\$11 \" \" \\$2}"`'
alias clojure="java -Xms64m -Xmx1g -cp $HOME/dev/clojure/clojure-1.3.0/clojure-1.3.0.jar clojure.main"
export CVS_RSH=ssh
##export JAVA_HOME=`/usr/libexec/java_home`
