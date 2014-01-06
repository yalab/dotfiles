fpath=(/home/yalab/vendor/zsh-completions/src $fpath)
autoload -U compinit && compinit
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
  rbenv global 2.0.0-p353
  rbenv rehash
  PASSWORDFILE="${HOME}/.password/.password"
else
  alias ls='ls --color=auto'
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
  PASSWORDFILE="${HOME}/.password"
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

export PATH="$PATH:/home/yalab/bin"

export JAVA_HOME="/usr/lib/jvm/java-6-sun-1.6.0.26"
export SCALA_HOME=/opt/scala
export PATH=$PATH:$SCALA_HOME/bin
export ANDROID_SDK_HOME="/opt/android"
export PATH="$PATH:$ANDROID_SDK_HOME/tools:$ANDROID_SDK_HOME/platform-tools"

export ANDROID_NDK_ROOT=/opt/android-ndk-r6b 
export PATH=$PATH:/opt/android-ndk-r6b

[ -s "$HOME/.rvm/scripts/rvm" ] && . "$HOME/.rvm/scripts/rvm" && rvm use ruby-2.0.0 > /dev/null

rails(){
    RAILS=`which -a rails | tail -1`
    ARGS=($@)
    if [ "$1" = "new" ];then
        ARGS[`expr ${#ARGS} + 1`]="--skip-bundle"
    fi
    $RAILS $ARGS
}
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

rake(){
  if [ -d .bundle ];then
    bundle exec rake  "$@"
  else
    /usr/bin/env rake "$@"
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
export MYSQL_USERNAME='yalab'
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

