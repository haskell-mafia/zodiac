#! /bin/sh -eux

#
# Build with static libgmp.
#

CCARGS=$(echo "$@" | sed -e 's/-lgmp//g')

if [ -z ${STATIC_GMP+1} ]; then
  STATIC_GMP="/usr/lib64/libgmp.a"
fi

gcc $CCARGS $STATIC_GMP
