AID=$1
curl -X PUT --cookie is.yaks.access=${AID} -d {value:100} http://localhost:8000/home/ac/cento
curl -X GET --cookie is.yaks.access=${AID}  http://localhost:8000/home/ac/cento

# Create Access with ID
# curl -X PUT http://localhost:8000/yaks/access/c6673415-1c2f-471c-9093-a3d6ba172774?path=/home/ac\&cacheSize=1024
