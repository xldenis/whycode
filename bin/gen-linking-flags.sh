#!/bin/sh
set -ue

LINKING_MODE="$1"
OS="$2"
FLAGS=
CCLIB=

case "$LINKING_MODE" in
    dynamic)
        ;; # No extra flags needed
    static)
        case "$OS" in
            linux) # Assuming Alpine here
                FLAGS="-noautolink"
                CCLIB="-Wl,-Bstatic -lbigstringaf_stubs -llwt_unix_stubs -lthreadsnat -lpthread -lcamlzip -lnums -lunix -lcamlstr -lz -Wl,-Bdynamic -lpthread";;
            macosx)
                ;;
            *)
                echo "No known static compilation flags for '$OS'" >&2
                exit 1
        esac;;
    *)
        echo "Invalid linking mode '$LINKING_MODE'" >&2
        exit 2
esac

echo '('
for f in $FLAGS; do echo "  $f"; done
for f in $CCLIB; do echo "  -cclib $f"; done
echo ')'
