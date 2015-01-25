#!/bin/bash
set -e

KERL=kerl
APP_ROOT=$(realpath $(dirname $0)/..)
ERL_RELEASES_PATH=$APP_ROOT/priv/erl_installations
RELEASES=`cat $APP_ROOT/priv/RELEASES.txt | head -n 1`

mkdir -p $ERL_RELEASES_PATH

# clone_extra_libs

for release in $RELEASES; do
    INST_PATH=$ERL_RELEASES_PATH/$release
    if [ ! -d $INST_PATH ]; then
        echo Making ${release}...
        # 'true' to ignore grep's !=0 retcode in set -e
        HAS_BUILD=$($KERL list builds | grep "$release" || true)
        if [ -z "$HAS_BUILD" ]; then
            $KERL build $release $release
        fi
        $KERL install $release $INST_PATH

        # for some reason, inet_gethost behaves badly under playpen,
        # so we just drop it
        gh_path=$(ls $INST_PATH/erts-*/bin/inet_gethost)
        mv $gh_path{,.bak}
        echo \
'#!/bin/bash
cat' > $gh_path
        chmod +x $gh_path
        # $KERL delete build $RELEASE
        # install_extra_libs
    fi
done

#$KERL cleanup all
