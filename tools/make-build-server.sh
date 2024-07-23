#!/bin/bash
readonly PNAME=$(basename "$0")

readonly BUILD_USER=bob


function message
{
    echo
    echo '================================================================================'
    echo "==  $*"
    echo '================================================================================'
    echo
}

function run
{
    echo "Executing: $*"
    "$@"; local ret=$?

    if (( ret > 0 )) ; then
        echo "COMMAND: '$*' failed with return value: $ret. Exiting!" 1>&2
        exit $ret
    fi

    return 0
}

function asBob
{
    run sudo -u "${BUILD_USER}" "$@"
}

message "Creating user '${BUILD_USER}'"
run useradd -m -G sudo -s /bin/bash "${BUILD_USER}"

message "Set password for '${BUILD_USER}'"
run passwd "${BUILD_USER}"

message "Adding SSH credentials for user"
asBob ssh-keygen -t ed25519

cat <<EOF


********************************************************************************

DONE!

Don't forget to upload the SSH credentials to the Git repo server so
that the repo can be cloned!

********************************************************************************

EOF

exit 0
