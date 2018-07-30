ACP=$1
CSIZE=1024
curl -v -d ''  http://localhost:8000/yaks/access?path=${ACP}\&cacheSize=${CSIZE}
