#! /bin/sh

CLOAK_BIN=$0
CLOAK_MAIN=rosado.cloak

# Find Cloak
if [ -z "$CLOAK_HOME" ]; then
    if [ -a "$HOME/.cloak" ]; then
        CLOAK_HOME="$HOME/.cloak"
    elif [ -a "/usr/share/cloak" ]; then
        CLOAK_HOME="/usr/share/cloak"
    else
        echo "Error: CLOAK_HOME is not defined correctly."
        exit 1
    fi
fi

# Load global Cloak settings. TODO: Include this in launcher
if [ -f "$CLOAK_HOME/.cloakrc" ]; then
    CLOAK_RC="$CLOAK_HOME/.cloakrc"
fi

# Construct Cloak classpath.
CLOAK_CP=$CLOAK_HOME'/*'

# Call Cloak.
java \
    -Dcloak.bin=$CLOAK_BIN \
    -Dcloak.home=$CLOAK_HOME \
    -cp "$CLOAK_CP" $CLOAK_MAIN \
    $@
