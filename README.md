```
 _        _     _       
| | _____| |___(_)_ __  
| |/ / _ \ / __| | '_ \ 
|   <  __/ \__ \ | | | |
|_|\_\___|_|___/_|_| |_|
```

Dotfiles for Christopher Giroir <kelsin@valefor.com>

## Goals

* Able to bootstrap a new Mac machine or Linux server easily with no
  dependencies other than `curl` or `wget`.
* Not having to keep entire home directory in git (as I used to do)
* Keeping external dependencies and custom scripts to a minimum

## Usage

### Bootstrapping

Dependencies: a [posix
sh](http://pubs.opengroup.org/onlinepubs/009695399/utilities/sh.html) and
[curl](https://curl.haxx.se/) or [wget](https://www.gnu.org/software/wget/)

On a mac laptop:

```sh
curl -o- https://raw.githubusercontent.com/kelsin/dotfiles/master/bootstrap | sh mac
```

On a server:
```sh
curl -o- https://raw.githubusercontent.com/kelsin/dotfiles/master/bootstrap | sh server
```

If you need to use wget the commands become:

```sh
wget -O - https://raw.githubusercontent.com/kelsin/dotfiles/master/bootstrap | sh mac
wget -O - https://raw.githubusercontent.com/kelsin/dotfiles/master/bootstrap | sh server
```

### Installing

The bootstrap will run an install initially for you, but at that point you can
easily remove or install packages yourself. There is a script available that
will setup the base mac or server package lists as well. Please note that if you
aren't in a terminal with `$STOW_DIR` set to `~/.dotfiles` (which is done by my
`~/.env` file) then you'll need to provide a `-d ~/.dotfiles` to all stow
commands.

```sh
# Install the bash package
stow bash

# Remove the bash package
stow -D bash

# List packages (only in zsh)
# this is an alias in $DOTFILES/zsh/.zsh/aliases.zsh
packages

# Install mac packages (zsh script)
$DOTFILES/install mac

# Install server packages (zsh script)
$DOTFILES/install server
```

### Updating

Updating the dot files is as easy as running:

```sh
pushd $DOTFILES && git pull && popd
```

Updating all apple / brew / ruby and node packages on your machine can be done
by running:

```sh
update
```

Apple and brew updates are done all the time, ruby and node are only done if
those dotfile packages are currently stowed.

## Alternatives

These are other solutions for storing your dotfiles:

* [rcm](https://github.com/thoughtbot/rcm) rc file management utilities


