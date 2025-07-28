mkdir data
pushd data
lynx -dump -listonly http://ratings.food.gov.uk/open-data | grep -e en-GB.xml | awk '{print $2}' > list.txt
while read p; do
    echo "$p"
    curl -O "$p"
done <list.txt
popd
