fpath=(~/vendor/zsh-completions/src $fpath)
fpath=(~/.zsh $fpath)
autoload -U compinit && compinit
setopt completealiases
bindkey -e
stty stop undef
stty start undef

#PROMPT
autoload colors
colors
local GREEN=$'%{\e[1;32m%}'
local BLUE=$'%{\e[1;34m%}'
local DEFAULT=$'%{\e[1;m%}'

autoload -Uz vcs_info
zstyle ':vcs_info:git*' formats "(%{$fg[cyan]%}%b)%{$reset_color%}%m%u%c%{$reset_color%}"
zstyle ':vcs_info:git*' actionformats "%s  %r/%S %b %m%u%c "
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

PROMPT='${BLUE} %(!.#.$) '$DEFAULT
RPROMPT=$GREEN'[%~%{$fg[cyan]%}${vcs_info_msg_0_}$GREEN]'$DEFAULT
setopt PROMPT_SUBST
alias b='bundle exec'
alias diff='colordiff -u'
alias emacs="emacs -nw"
alias grep='grep --color'
alias less='less -R'
alias iptables='sudo iptables'
alias ..='cd ..'
alias gti="git"
alias gnuplot="gnuplot -d"
if [ `uname` = 'Darwin' ];then
  alias ls='ls -G'
  . ~/.profile
  rbenv global 2.3.1
  rbenv rehash
  PASSWORDFILE="${HOME}/.password/.password"
  export NODE_PATH=/opt/boxen/nodenv/versions/v0.10.21/lib/node_modules
  alias mocha-coffee='mocha --compilers coffee:coffee-script/register'
  export BOOST_ROOT=/opt/boxen/homebrew/Cellar/boost/1.55.0/include/boos
  export PATH="/usr/local/bin:${PATH}"
  alias clear_dns_cache="sudo killall -HUP mDNSResponder"
  export ANT_ROOT="/opt/boxen/homebrew/bin"
  alias ldd="otool -L"
  alias refresh_wifi="networksetup -setairportpower en0 off && networksetup -setairportpower en0 on"
else
  alias ls='ls --color=auto'
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  PASSWORDFILE="${HOME}/.password"
  export JAVA_HOME="/usr/lib/jvm/java-6-sun-1.6.0.26"
fi

export EDITOR='vim'
export GIT_PAGER='less -R'
export HREF_DATADIR=/usr/share/href
export WORDCHARS='\._'

#HISTORY
HISTFILE=$HOME/.zsh-history           # 履歴をファイルに保存する
HISTSIZE=100000                       # メモリ内の履歴の数
SAVEHIST=100000                       # 保存される履歴の数
setopt extended_history               # 履歴ファイルに時刻を記録
function history-all { history -E 1 } # 全履歴の一覧を出力する
setopt share_history
setopt hist_ignore_dups
setopt hist_verify
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward

export PATH="$PATH:/home/yalab/bin"
export SCALA_HOME=/opt/scala
export PATH=$PATH:$SCALA_HOME/bin
#export ANDROID_SDK_HOME="$HOME/.android"
#export ANDROID_HOME=$ANDROID_SDK_HOME
#export PATH="$PATH:$ANDROID_SDK_HOME/tools:$ANDROID_SDK_HOME/platform-tools"

export ANDROID_NDK_ROOT=/opt/android-ndk-r10e
#export ANDROID_NDK_ROOT=/opt/android-ndk-r9d
#export ANDROID_NDK_ROOT=/opt/android-ndk-r11

export ANDROID_SDK_ROOT=/opt/android-sdk
export PATH="$PATH:${ANDROID_SDK_ROOT}/tools:${ANDROID_SDK_ROOT}/platform-tools:${ANDROID_NDK_ROOT}"

[ -s "$HOME/.rvm/scripts/rvm" ] && . "$HOME/.rvm/scripts/rvm" && rvm use ruby-2.0.0 > /dev/null

alias s="spring"
alias l="rails"
alias r='rails'
export JRUBY_OPTS=--1.9
setopt nullglob

alias start-apk="sbt android:package-debug android:start-emulator"
alias nexus="emulator -avd nexus -partition-size 1024"

git-commit-subdirs(){
    for d in `ls`;do
        if [ ! -d "$d" ];then
            continue
        fi
        cd $d
        if [ ! -d .git ];then
            git init
            if [ -d .yardoc ];then
                echo ".yardoc" >> .gitignore
            fi
            git add .
            git commit -m "First commit."
        fi
        cd ..
    done
}

in_rails_app(){
    
  if [ ! -f $(git rev-parse --show-toplevel)/Gemfile ];then
      return -1
  else
      grep rails Gemfile > /dev/null
      return $?
  fi
}

rake(){
  in_rails_app
  if [ "$?" = "0" ];then
    spring rake "$@"
  elif [ -d .bundle ];then
    bundle exec rake  "$@"
  else
    /usr/bin/env rake "$@"
  fi
}

# rails(){
#   in_rails_app
#   if [ "$?" = "0" ];then
#     bundle exec spring rails "$@"
#   elif [ -d .bundle ];then
#     bundle exec rails  "$@"
#   else
#     /usr/bin/env rails "$@"
#   fi
# }

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
killzeus(){
  rm ` git rev-parse --show-toplevel`/.zeus.sock
  kill `ps ax | grep zeus | awk '{print $1}'`
}

if [ "$ANDROID_SDK_HOME" = "" ];then
    export ANDROID_SDK_HOME="/opt/android-sdk-linux"
fi
export MYSQL_USERNAME='root'
export MYSQL_USER='yalab'
export POSTGRES_USER='yalab'

copy_password(){
    str="$1"
    grep -i $str $PASSWORDFILE | cut -d ':' -f 3 | pbcopy
}

copy_id(){
    str="$1"
    grep -i $str $PASSWORDFILE | cut -d ':' -f 2 | pbcopy
}

. ~/.oauth.conf
mkpasswd(){
    CHARS=$1
    if [ -z "$CHARS" ];then
        CHARS=12
    fi
    ruby -e "puts ('!'..'~').to_a.sample($CHARS).join"
}

presen_cli()
{
    PROMPT=$BLUE'%(!.#.$) '$DEFAULT
    RPROMPT=""
}


export _JAVA_OPTIONS='-Dfile.encoding=UTF-8'

#alias docker_clean_image='docker rmi \$(docker images | awk '/^<none>/ { print $3 }')"'
#alias docker_clean_container="docker rm `docker ps -a -q`"

alias say="say -v Alex"
alias gi="git"
#say -v Alex $(basename $SHELL)

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=/Users/yalab/Library/Cocos/CocosStore/cocos2d-x-3.8/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=/Applications/Cocos/Cocos2d-x/cocos2d-x-3.10/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_X_ROOT for cocos2d-x
export COCOS_X_ROOT=/Applications/Cocos/Cocos2d-x
export PATH=$COCOS_X_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=/Applications/Cocos/Cocos2d-x/cocos2d-x-3.10/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH
alias "builtin-copy -exclude .DS_Store -exclude CVS -exclude .svn -exclude .git -exclude .hg -resolve-src-symlinks"="rsync --exclude .DS_Store --exclude CVS --exclude .svn --exclude .git --exclude .hg --copy-links"
