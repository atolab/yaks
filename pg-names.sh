#!/bin/bash

YAKS_URL="http://localhost:8000"
if [ $# -lt 2 ]; then 
  echo "Usage:"
  echo "          pg-names.sh <access-id> <storage-path> [<yaks-url=$YAKS_URL>]"
  echo ""
  echo " To create an access use create-access.sh <access-paht>"
  exit
fi

AID=$1
STORAGE_PATH=$2

GRAY='\033[0;37m'
RED='\033[1;31m'
BLUE='\033[1;34m'
if [ $# -ge 4 ]; then 
  YAKS_URL=$3
fi

NAMES=`cat /usr/share/dict/propernames`
URL="http://localhost:8000//home/ac/$STORAGE_PATH"
for name in ${NAMES}; do
  echo -e "${RED}curl -X PUT --cookie is.yaks.access=${AID} -d {value:$name} $STORAGE_PATH/${name}"  
  curl -X PUT --cookie is.yaks.access=${AID} -d {value:$name} ${YAKS_URL}${STORAGE_PATH}/${name}
done

for name in ${NAMES}; do
  echo -e "${GRAY}curl -X GET --cookie is.yaks.access=${AID} ${YAKS_URL}${STORAGE_PATH}/${name} ${BLUE}"
  curl -X GET --cookie is.yaks.access=${AID} ${YAKS_URL}${STORAGE_PATH}/${name}
  echo ""
done
