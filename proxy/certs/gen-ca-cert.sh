#!/usr/bin/env bash
set -eu
org=consult-ca

sudo trust anchor --remove consult.crt || true

openssl genpkey -algorithm RSA -out consult.key
openssl req -x509 -key consult.key -out consult.crt \
    -subj "/CN=$org/O=$org"

sudo trust anchor consult.crt
