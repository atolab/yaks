AID=$1
NUM_S="cento duecento trecento quattrocento cinquecento seicento settecento ottocento novecento"
echo ${NUM_S}

BASE=100

for num in ${NUM_S}; do
  echo "curl -X PUT --cookie is.yaks.access=${AID} -d {value:$num} http://localhost:8000/home/ac/${num}"
  curl -X PUT --cookie is.yaks.access=${AID} -d {value:$num} http://localhost:8000/home/ac/${num}
done

for num in ${NUM_S}; do
  echo "curl -X GET --cookie is.yaks.access=${AID} http://localhost:8000/home/ac/${num}"
  curl -X GET --cookie is.yaks.access=${AID} http://localhost:8000/home/ac/${num}
  echo ""
done

# curl -X GET --cookie is.yaks.access=${AID}  http://localhost:8000/home/ac/cento
# echo ""
# curl -X GET --cookie is.yaks.access=${AID}  http://localhost:8000/home/ac/duecento
# echo ""
# Create Access with ID
# curl -X PUT http://localhost:8000/yaks/access/c6673415-1c2f-471c-9093-a3d6ba172774?path=/home/ac\&cacheSize=1024
