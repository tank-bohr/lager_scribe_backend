#!/usr/bin/env bash

TMP_DIR=".gen_erl_tmp"
OPTS="-v -r --gen erl --out $TMP_DIR"

if [ -n "$FB303_PATH" ]; then
    OPTS="$OPTS -I $FB303_PATH/share"
fi

if [ -n "$SCRIBE_PATH" ]; then
    THRIFT_FILE="$SCRIBE_PATH/if/scribe.thrift"
else
    echo "Environment variable SCRIBE_PATH is not defined"
    exit 1
fi

mkdir -p $TMP_DIR

thrift $OPTS $THRIFT_FILE

mv $TMP_DIR/*.hrl include/
mv $TMP_DIR/*.erl src/

rm -r $TMP_DIR
