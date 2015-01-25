#!/bin/bash
set -e

APP_ROOT=$(realpath $(dirname ${BASH_SOURCE}))
BASE_DIR=`dirname $APP_ROOT`
KERL=/usr/local/bin/kerl
PP_WORKDIR=$APP_ROOT/priv/pp-root
PP_USER=eplaypen
PP_COMMIT=8537989e9df484dcac

make_deps() {
    sudo pacman -S \
        erlang-nox \
        rebar \
        arch-install-scripts

    if [ ! -x $KERL ]; then
        curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
        chmod +x kerl
        sudo mv kerl $KERL
        sudo chown root:root $KERL
    fi
}

make_playpen() {
    d=`pwd`
    cd $BASE_DIR
    if [ ! -d $BASE_DIR/playpen ]; then
        git clone https://github.com/thestinger/playpen
    fi
    cd $BASE_DIR/playpen
    git checkout $PP_COMMIT .
    git apply $APP_ROOT/priv/disable_strict_syscalls_$PP_COMMIT.patch
    make
    # install playpen exe as 'eplaypen/bin/playpen'
    # XXX: maybe install it to /usr/local/bin and chown to root:root for security?
    make install PREFIX=$APP_ROOT
    cd $d
    # XXX: DANGEROUS! Allow to run 'sudo playpen' without password!
    #sudo setcap CAP_SYS_ADMIN+ep bin/playpen
    sudo sh -c "echo '\n$USER   ALL=NOPASSWD:$APP_ROOT/bin/playpen' >> /etc/sudoers"
}

make_erlang_releases() {
    $APP_ROOT/bin/build_erl.sh
    # TODO: automatically add <option>${RELEASE}</option> to priv/htdocs/index.html
}

make_playpen_workdir() {
    # sudo $APP_ROOT/bin/playpen $APP_ROOT/priv/pp-root --user=eplaypen \
    #   --bind $APP_ROOT/priv/erl_installations --bind $APP_ROOT/priv/scripts \
    #   --devices=/dev/urandom:r,/dev/null:w --mount-proc --mount-dev --hostname pp \
    #   --timeout=15 --memory-limit=50 -- <...>

    sudo rm -rf $PP_WORKDIR
    mkdir $PP_WORKDIR

    sudo pacstrap -c -d $PP_WORKDIR \
        bash \
        coreutils \
        grep \
        sed \
        dash \
        filesystem \
        glibc \
        pacman \
        procps-ng \
        shadow \
        util-linux

    sudo mkdir $PP_WORKDIR/dev/shm
    sudo mknod -m 666 $PP_WORKDIR/dev/null c 1 3
    sudo mknod -m 644 $PP_WORKDIR/dev/urandom c 1 9
    sudo arch-chroot $PP_WORKDIR useradd -m $PP_USER
    sudo mkdir -p ${PP_WORKDIR}${APP_ROOT}/priv/erl_installations
    sudo mkdir -p ${PP_WORKDIR}${APP_ROOT}/priv/scripts
}

make_etc() {
    echo "!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "Ensure your /etc/nginx.conf has 'include /etc/nginx/sites-enabled/*;'"
    echo "!!!!!!!!!!!!!!!!!!!!!!!!"
    d=`pwd`
    cd $APP_ROOT/priv
    sudo find etc -type f -print -exec install -o root -g root -m 664 -D {} /{} \;
    cd $d
}

make_all() {
    set -x
    $0 deps
    $0 etc
    $0 playpen
    $0 playpen_workdir
    $0 erlang_releases
    $0 start
}

make_start() {
    cd $APP_ROOT
    rebar get-deps compile
    erl -pa deps/*/ebin -pa ../eplaypen/ebin -sname eplaypen -detached -s playpen start
}

make_remsh() {
    erl -sname eplaypen_console_$RANDOM -remsh eplaypen@eplaypen
}

# usage build.sh {deps|playpen|erlang_releases|playpen_workdir|etc|start|remsh}
make_$1
