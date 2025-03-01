# 2.1) Safety
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
#alias rm="gio trash"
set -o noclobber

# 2.2) Listing, directories, and motion
alias l="ls --color"
alias ls="ls --color"
alias ll="ls -alrtF --color"
alias la="ls -A"
alias lal="ls -al"
alias l="ls -CF"
alias dir='ls --color=auto --format=vertical'
alias vdir='ls --color=auto --format=long'
alias m='less'
alias c="cd"
alias ..='cd ..'
alias ...='cd ..;cd ..'
alias md='mkdir'
alias cl='clear'
alias du='du -ch --max-depth=1'
alias treeacl='tree -A -C -L 2'

# 2.3) Text and editor commands
alias em='emacs -nw'     # No X11 windows
alias eqq='emacs -nw -Q' # No config and no X11
export EDITOR='emacs -nw'
export VISUAL='emacs -nw'

# 2.4) grep options
export GREP_COLOR='1;31' # green for matches
alias grep="grep -n --color=auto"

# 2.5) sort options
# Ensures cross-platform sorting behavior of GNU sort.
# http://www.gnu.org/software/coreutils/faq/coreutils-faq.html#Sort-does-not-sort-in-normal-order_0021
unset LANG
export LC_ALL=POSIX

# Standard Directories
alias home="cd ~"

# Python Aliases
alias p="python"
alias pip="pip3.11"
alias freeze="pip freeze"
alias freezer="pip freeze >| requirements.txt"

# Moving Around
alias dt="cd ~/dotfiles"
alias dev="cd ~/development"
alias llm="cd ~/development/llm"
alias sd="cd ~/development/stable-diffusion"
alias comfy="cd ~/development/stable-diffusion/ComfyUI"

# Git Aliases
alias gs="git status --ignore-submodules"
alias ga="git add --all"
alias gc="git commit -m"
alias gl="git log --abbrev-commit --pretty=oneline"
alias gd="git diff"
alias ph="git push heroku master"
alias po="git push origin master"

# Virtual Environment Aliases
alias va="source venv/bin/activate"
alias da="deactivate"

# Vim shortcuts
alias vim="nvim"
alias v="nvim"
alias vxb="vim ~/dotfiles/.Xresources"
alias plugin="cd ~/.config/nvim/ftplugin"
alias vswap="cd ~/.local/share/nvim/swap"

# Node Shortcuts
alias npmd="npm install --save-dev"
alias ns="npm run start"
alias nd="npm run develop"
alias ni="npm run inspect"
alias vp="vim package.json"

# Config Aliases
alias slp="sleep 3; xset dpms force off"
alias sbrc="source ~/.bashrc"
alias valias="vim ~/dotfiles/.bash_aliases"
alias salias="source ~/dotfiles/.bash_aliases"
alias vbrc="vim ~/.zshrc_zac"
alias zbrc="source ~/.zshrc_zac"
alias xb="xrdb ~/.Xresources"
alias change_keyboard="sudo dpkg-reconfigure keyboard-configuration"
alias chrome="google-chrome-stable"

# Opening config files
alias ii="vim ~/.config/i3/config"
alias pbar="vim ~/.config/polybar/config"
alias vv="vim ~/.config/nvim/init.vim"
alias ee="vim ~/dotfiles/init.el"

# Directory Aliases
alias anki="cd ~/development/anki"
alias org="cd ~/development/org"


# Shortcuts to edit files
alias vn="vim notes"

# Shortcut for viewing source
alias modules="cd /home/zac/.local/lib/python3.8/site-packages"

# C alias
#alias m="make && ./create"

# Application shortcuts
alias rg="ranger"
alias ex="exit"

# Heroku
alias ph="git push heroku master"
alias ho="heroku open"
alias hl="heroku local"

# Screenplay
alias mkft="screenplain ~/development/screenplay/markov/markov.fountain ~/development/screenplay/markov/markov.pdf"

# Jekyll
alias js="bundle exec jekyll serve"

# Useful
alias smi="nvidia-smi"
alias audio="wpctl status"

# Clojure
alias cljs="shadow-cljs"
alias nb="npm run build"
alias nc="npm run compile"
alias nw="npm run watch"
alias owl="cd ~/development/clojure/owl"
alias ou="cd ~/development/clojure/owl/electron"
alias ob="cd ~/development/clojure/owl/site"
alias thc="cd ~/.config/Thinky"
alias install="npm run dist && cd dist && sudo dpkg -i Thinky_1.0.0_amd64.deb"
alias default="cp -f electron/resources/default-state.edn electron/resources/storage/app-state.edn"
alias lr="lein run"
alias ld="lein ring server-headless 3000"
