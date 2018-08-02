AID=$1
NAMES=`cat /usr/share/dict/propernames`

for name in ${NAMES}; do
  echo "-"
  echo "curl -X PUT --cookie is.yaks.access=${AID} -d {value:$name} http://localhost:8000/home/ac/${name}"
  echo "-"
  curl -X PUT --cookie is.yaks.access=${AID} -d {value:$name} http://localhost:8000/home/ac/${name}
done

for name in ${NAMES}; do
  echo "curl -X GET --cookie is.yaks.access=${AID} http://localhost:8000/home/ac/${name}"
  curl -X GET --cookie is.yaks.access=${AID} http://localhost:8000/home/ac/${name}
  echo ""
done
