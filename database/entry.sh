#!/usr/bin/env bash

pushd /docker-entrypoint-initdb.d/
gunzip calib.sql.gz
gunzip calib_user.sql.gz
popd

/usr/local/bin/docker-entrypoint.sh


