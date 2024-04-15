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

# 2.8) Django controls
alias pym="python manage.py"
alias shell="python manage.py shell"


# Python Aliases


alias ca="conda activate anaconda"
alias cda="conda deactivate"
alias p="python"
alias pt="python -m unittest"
alias db="psql emerald"
alias debug="python -m pdb manage.py runserver"
alias freeze="pip freeze"
alias freezer="pip freeze >| requirements.txt"
alias ph="git push heroku master"
alias po="git push origin master"
alias req="pip install -r requirements.txt"
alias run="python manage.py runserver"
alias coverage="pytest --cov-report html --cov"

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
alias gz="git push origin Zac"

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
alias ee="vim ~/dotfiles/init.el"
alias plugin="cd ~/.config/nvim/ftplugin"
alias vswap="cd ~/.local/share/nvim/swap"

# Node Shortcuts
alias npmd="npm install --save-dev"
alias ns="npm run start"
alias nd="npm run develop"
alias ni="npm run inspect"
alias vp="vim package.json"

# Misc Aliases
alias vpn="sudo purevpn-pptp"
alias slp="sleep 3; xset dpms force off"
alias sbrc="source ~/.bashrc"
alias salias="source ~/.bash_aliases"
alias zbrc="source ~/.zshrc"
alias xb="xrdb ~/.Xresources"
alias change_keyboard="sudo dpkg-reconfigure keyboard-configuration"
alias mc="mongodb-compass"
alias pm="postman"
alias chrome="google-chrome-stable"
alias jl="jupyter lab"
alias at="alacritty-themes"

# Opening config files
alias ii="vim ~/.i3/config"
alias pbar="vim ~/.config/polybar/config"

# Directory Aliases
alias va="cd ~/development/acme/"
alias v1="cd ~/development/acme/Volume1"
alias vh="cd ~/development/acme/homework"
alias vh1="cd ~/development/acme/homework/1"
alias vh2="cd ~/development/acme/homework/2"
alias l3="cd ~/development/acme/lab_3"
alias l4="cd ~/development/acme/lab_4"
alias q="cd ~/development/Q/"
alias vs="vim ~/development/Q/server.js"
alias qu="cd ~/development/Q/client/src"
alias qb="cd ~/development/Q/server"
alias bio="cd ~/development/research/biocheck/biocheck"
alias net="cd ~/development/netflix-clone/src/"
alias e="cd ~/development/techEd/"
alias eb="cd ~/development/techEd/server"
alias eu="cd ~/development/techEd/client/src"
alias nfl="cd ~/development/nfl-predictor"
alias pr="cd ~/development/acme/637/homework"
alias res="cd ~/development/research"
alias grc="cd ~/development/research/gradientconditioning"
alias thesis="cd ~/development/research/thesis/src"
alias draft="cd ~/development/research/thesis/draft"
alias p3="cd ~/development/acme/CS\ 674/Project\ 3"
alias rp="cd ~/development/acme/522/random_pruning"

# Shortcuts to edit files
alias vA="vim ~/development/Q/client/src/App.js"
alias vS="vim ~/development/Q/server.js"
alias vn="vim notes"

# Shortcut for viewing source
alias modules="cd /home/zac/.local/lib/python3.8/site-packages"

# C alias
#alias m="make && ./create"

# Application shortcuts
alias rg="ranger"
alias t="tmux"
alias ex="exit"

# Heroku
alias ph="git push heroku master"
alias ho="heroku open"
alias hl="heroku local"

# Screenplay
alias mkft="screenplain ~/development/screenplay/markov/markov.fountain ~/development/screenplay/markov/markov.pdf"

# Useful
alias smi="nvidia-smi"
alias chat="./chat -m ggml-alpaca-13b-q4.bin -c 2048 -n 2048"
alias audio="pkill pulseaudio"
alias forget="rm -f models/* results/* diffs/*"
