# .zprofile - Setup for login shells
#
# I think a login shell is the initial shell associated with a given tty, also
# the initial process in a session. See `man 7 credentials`.  Sessions exist
# for shell job control. All the processes in a session share a tty, and the
# kernel (unbelievably) sends signals to micromanage what goes on when
# background processes try to read from the tty.
#
# I don't know why setting PS1 here has no effect.  See `.zshrc`.

# strip /usr/local/bin from PATH and add it at the front
export PATH=$(echo "$PATH" | sed 's/^\/usr\/local\/bin://' | sed 's/:\/usr\/local\/bin:/:/')
export PATH="/usr/local/bin:${PATH}"

# This prepends ~/.cargo/bin to the PATH
source "$HOME/.cargo/env"

