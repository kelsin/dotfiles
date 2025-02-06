# Source main environment file
. ~/.env.sh

# Paths
# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Brew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Set the list of directories that Zsh searches for programs.
path=($HOME/.local/bin $HOME/bin /usr/local/{bin,sbin} $path)

# >>> coursier install directory >>>
export PATH="$PATH:/Users/kelsin/Library/Application Support/Coursier/bin"
# <<< coursier install directory <<<

# >>> JVM installed by coursier >>>
export JAVA_HOME="/Users/kelsin/Library/Caches/Coursier/arc/https/github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.25%252B9/OpenJDK11U-jdk_x64_mac_hotspot_11.0.25_9.tar.gz/jdk-11.0.25+9/Contents/Home"
# <<< JVM installed by coursier <<<
