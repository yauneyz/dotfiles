# 2.1) Safety
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias tt="gio trash"
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

# 2.8) Django controls
alias pym="python manage.py"
alias shell="python manage.py shell"


# Python Aliases


alias python="python3"
alias p="python"
alias pip="pip3"
alias pt="python -m unittest"
alias db="psql emerald"
alias debug="python -m pdb manage.py runserver"
alias freeze="pip freeze"
alias freezer="pip freeze >| requirements.txt"
alias ph="git push heroku master"
alias po="git push origin master"
alias req="pip install -r requirements.txt"
alias run="python manage.py runserver"

# SASS Aliases

alias css="vim ~/development/emerald/sass/custom.scss"
alias ss="sass sass/custom.scss static/emerald/css/custom.css"
alias sw="sass --watch sass:static/emerald/css/"

# Moving Around

alias dt="cd ~/dotfiles"
alias envs="cd $WORKON_HOME"
alias st="cd ~/setup"
alias dev="cd ~/development"

# Git Aliases

alias gs="git status --ignore-submodules"
alias ga="git add *"
alias gc="git commit -m"
alias gl="git log --abbrev-commit --pretty=oneline"
alias gd="git diff"

# Run that thing that lets you get colors
alias colors="sh ~/.colors.sh"

# Virtual Environment Aliases

alias da="deactivate"

# Vim shortcuts

alias vim="nvim"
alias v="vim"
alias valias="vim ~/.bash_aliases"
alias vbrc="vim ~/.zshrc_zac"
alias vxb="vim ~/dotfiles/.Xresources"
alias vv="vim ~/.config/nvim/init.vim"

# Node Shortcuts
alias npmd="npm install --save-dev"
alias ns="npm run start"
alias ni="npm run inspect"
alias vp="vim package.json"

# Misc Aliases
alias vpn="sudo purevpn-pptp"
alias slp="xset dpms force off"
alias sbrc="source ~/.bashrc"
alias salias="source ~/.bash_aliases"
alias zbrc="source ~/.zshrc"
alias xb="xrdb ~/.Xresources"
alias rng="ranger"
alias change_keyboard="sudo dpkg-reconfigure keyboard-configuration"
alias urxvt="urxvt -pixmap 'find /home/zac/dotfiles/images/Generation II - Johto -name '249.png'"
alias mc="mongodb-compass"
alias pm="postman"

# Opening config files
alias ii="vim ~/.i3/config"

# Directory Aliases
alias vc="cd ~/development/backend/src"
alias vs="cd ~/development/strange/src"
alias vg="cd ~/development/gatsby/packages/gatsby/src"
alias vrs="cd ~/development/react-slack-clone/src"
alias vd="cd ~/development/dragon/src"
alias va="cd ~/development/acme/"
alias v1="cd ~/development/acme/Volume1"
alias vpy="cd ~/development/acme/PythonEssentials"
alias vt="cd ~/development/research/tsunami"

