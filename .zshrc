fpath=(~/vendor/zsh-completions/src $fpath)
fpath=(~/.zsh $fpath)
autoload -U compinit && compinit
setopt completealiases
bindkey -e
stty stop undef
stty start undef

#PROMPT
local GREEN=$'%{\e[1;32m%}'
local BLUE=$'%{\e[1;34m%}'
local DEFAULT=$'%{\e[1;m%}'

autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}

PROMPT=$BLUE'${USER}@${HOST}: %(!.#.$) '$DEFAULT
RPROMPT=$GREEN'[%~]'$DEFAULT
setopt PROMPT_SUBST
alias b='bundle exec'
alias diff='diff -u'
alias emacs="emacs -nw"
alias grep='grep --color'
alias less='less -R'
alias iptables='sudo iptables'
if [ `uname` = 'Darwin' ];then
  alias ls='ls -G'
  . ~/.profile
  rbenv global 2.1.2
  rbenv rehash
  PASSWORDFILE="${HOME}/.password/.password"
  export NODE_PATH=/opt/boxen/nodenv/versions/v0.10.21/lib/node_modules
  alias mocha-coffee='mocha --compilers coffee:coffee-script'
  export BOOST_ROOT=/opt/boxen/homebrew/Cellar/boost/1.55.0/include/boos
  export PATH="/usr/local/bin:${PATH}"
  alias clear_dns_cache="sudo killall -HUP mDNSResponder"
  export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Home"
  export ANT_ROOT="/opt/boxen/homebrew/bin"
  alias ldd="otool -L"
  
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
export ANDROID_SDK_HOME="/opt/android"
export PATH="$PATH:$ANDROID_SDK_HOME/tools:$ANDROID_SDK_HOME/platform-tools"

export ANDROID_NDK_ROOT=/opt/android-ndk-r9c
export ANDROID_SDK_ROOT=/opt/android-sdk

[ -s "$HOME/.rvm/scripts/rvm" ] && . "$HOME/.rvm/scripts/rvm" && rvm use ruby-2.0.0 > /dev/null

alias s="spring"
alias l="rake"
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
  if [ ! -f Gemfile ];then
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

rails(){
  in_rails_app
  if [ "$?" = "0" ];then
    spring rails "$@"
  elif [ -d .bundle ];then
    bundle exec rails  "$@"
  else
    /usr/bin/env rails "$@"
  fi
}

irb(){
    PRY_PATH=`which pry`
    if [ "$?" = "" ];then
        PRY_PATH=`which irb`
    fi
    $PRY_PATH
}

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
killzeus(){
  rm ` git rev-parse --show-toplevel`/.zeus.sock
  kill `ps ax | grep zeus | awk '{print $1}'`
}

export ANDROID_SDK_HOME="/opt/android-sdk-linux"
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

set_docker_host(){
    DOCKER_STATUS=$(boot2docker status)
    if [ "running" = $DOCKER_STATUS ];then
        export DOCKER_HOST=tcp://$(boot2docker ip 2>/dev/null):2375
        export DOCKER_IP=$(echo $DOCKER_HOST | cut -d '/' -f 3 | cut -d ':' -f 1)
    fi
}
