TSRP=${1:-./dist/build/tsrp/tsrp}
REQTOOL=${2:-./dist/build/reqtool/reqtool}

banner () {
    echo
    echo ===========
    echo == "$*"
    echo ===========
    echo
}

fail () {
    echo "  \`- [FAILED] $1"
    exit 1
}

check () {
    [ "$1" = "$2" ] || ( echo "FAIL: $1 != $2" ; exit 1 )
}
