if [ $# -lt 1 ]; then
    echo "USAGE:"
    echo "         create-access.sh <access-path>"
    exit
fi 
ACP=$1
echo "Creating Access for ${ACP}"
CSIZE=1024
ALINE=`curl -v -d '' http://localhost:8000/yaks/access?path=${ACP}\&cacheSize=${CSIZE} 2>&1 | grep location`
pfix="< location: "
AID=${ALINE:${#pfix}}
echo -e "${BLUE}${AID}"