# Sudo
alias _='sudo'
alias please='sudo'

# Conda
alias ca='conda activate'
alias mm='micromamba'
alias mma='micromamba activate'

# No Vi
alias vi='vim'
alias e='emacsclient -a vim -t'

# StarWars
alias sw='telnet towel.blinkenlights.nl'

# Make sure ls has colors and defaults to -h
if ls --color -d . >/dev/null 2>&1; then
    alias ls='ls --color=auto -h'
    alias la='ls --color=auto -lah'
else
    alias ls='ls -h'
    alias la='ls -lah'
fi

# Docker
alias dr='docker run -it --rm'

# Whats My IP
alias wmi='dig +short myip.opendns.com @resolver1.opendns.com'

# Pretty Clear
alias clear='clear && figlet $USER 2>/dev/null | lolcat 2>/dev/null'

# Gource
alias gitvid='gource --user-image-dir ~/src/avatars --key --highlight-all-users --hide filenames --seconds-per-day 0.1 --auto-skip-seconds 1 -1280x720 -o - | ffmpeg -y -r 60 -f image2pipe -vcodec ppm -i - -vcodec libvpx -b:v 10000K'

# Proxy
alias proxy='sudo npx local-ssl-proxy --source 443 --target $PORT'

# Mac
alias o='open'
alias pbc='pbcopy'
alias pbp='pbpaste'

# File Download
alias get='curl --continue-at - --location --progress-bar --remote-name --remote-time'

# Resource Usage
alias df='df -kh'
alias du='du -kh'

# Use HTop
alias top=htop

# Git
alias gitk='gitk --all &'
alias gitx='open -a gitx .'
alias st='open -a SourceTree .'

# QuickMug
alias qm='~/.rbenv/shims/quickmug'

# NetHack
alias nh='DGLAUTH=kelsin:a49Q83A8CrbvEowxTZnn ssh nao'

# Ember
alias em='ember'

# Kubectl
alias k='kubectl'
alias kc='kubectx'
alias kn='kubens'
alias ke='k exec -it'
# alias kdo='k config use-context do-sfo2-kelsin'

# Terraform
alias tf='terraform'

# Pino Pretty
alias pp='pino-pretty --colorize --translateTime "yyyy-mm-dd HH:MM:ss.l"'
alias ppe='pino-pretty --colorize --translateTime "yyyy-mm-dd HH:MM:ss.l" -s "level == \`error\`"'
alias ppw='pino-pretty --colorize --translateTime "yyyy-mm-dd HH:MM:ss.l" -s "level == \`warn\`"'

# Docker
alias d='docker'
alias dc='docker-compose'
alias ecrl='$(aws ecr get-login --no-include-email)'

# Grep Colors
alias grep='grep --color=auto'

# Our Home
alias home='set-title Kelsin;ssh -Y -p 2223 kelsin.is-a-geek.com'

# Giroir Home
alias giroir='set-title Giroir;ssh -Y giroir.homelinux.com'

# Linode
alias zealot='set-title zealot;ssh -Y zealot.kelsin.net'
alias carrier='set-title carrier;ssh -Y carrier.kelsin.net'
alias lish-zealot='set-title lish:zealot;ssh -Y -t kelsin@lish-dallas.linode.com zealot'
alias lish-carrier='set-title lish:carrier;ssh -Y -t kelsin@lish-dallas.linode.com carrier'

# Brew
alias brews='brew list -1'
alias bubo='brew update && brew outdated'
alias bubc='brew upgrade && brew cleanup'
alias bubu='bubo && bubc'
alias brewski='brew update && brew upgrade && brew cleanup; brew doctor'

# Emacs
alias pemacs='emacs -Q -l ~/.emacs.d/profile-dotemacs.el --eval "(setq profile-dotemacs-file (setq load-file-name \"~/.emacs.d/init.el\"))" -f profile-dotemacs'

# Ember
alias em='ember'
alias ems='ember serve'
alias emt='ember test'
alias emb='ember build'

# Poetry
alias po='poetry'
alias poi='poetry install'
alias pois='poetry install --sync'
alias poa='poetry add'
alias poad='poetry add --group dev'

# Python
alias va='source .venv/bin/activate'
alias vd='deactivate'

# Bundler
alias bi='bundle install --without production'
alias bu='bundle update'
alias be='bundle exec'

# NPM and Node.js setup
alias ni='npm install'
alias nt='npm test'
alias nit='ni && nt'
alias nrd='npm run dev'
alias nird='ni && nrd'
alias cov='open coverage/lcov-report/index.html'

# Tmux
alias mux=tmuxinator

# Github CLI
alias ghr="gh pr list --json labels,number | jq '.[] | select(.labels[].name==\"release: pending\") | .number'"
alias ghvr='gh pr view $(ghr) -w'
alias ghcr='gh pr checks $(ghr) --required'
alias ghmr='gh pr merge $(ghr) --auto --squash --delete-branch'
alias ghc='gh pr create --fill'
alias ghcm='git push && gh pr create --fill && gh pr merge --auto --squash --delete-branch'

# 18xx Maker
alias maker='pnpm --silent maker'
