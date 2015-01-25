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
        $KERL build $release $release
        $KERL install $release $INST_PATH

        # for some reason, inet_gethost behaves badly under playpen,
        # so we just drop it
        mv $INST_PATH/erts-*/bin/inet_gethost{,.bak}
        echo "#!/bin/bash\ncat" > $INST_PATH/erts-*/bin/inet_gethost
        # $KERL delete build $RELEASE
        # install_extra_libs
    fi
done

$KERL cleanup all
