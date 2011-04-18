#!/bin/bash

print_usage() {
  echo "`basename $0` <otp_dir>"
}

main() {
  if [ -z "$1" ]; then
	print_usage;
        exit 1
  fi
  OTP_DIR=`readlink -f $1`
  ECBREAK_DIR=$(readlink -f `dirname $0`)

  sed -e "s#@OTP_DIR@#$OTP_DIR#g" -e "s#@ECBREAK_DIR@#$ECBREAK_DIR#g" build.env.template > build.env	
}

main "$@"
