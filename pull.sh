mkdir pull
cd pull

STACKAGE="http://www.stackage.org/stackage/24e4d1b3bb101a80d3be1b146771c13df0827a82/metadata"
echo ==== fetching stackage snapshot metadata $STACKAGE
wget $STACKAGE

echo ==== pkgs
# haskellXY packages duplicate modules already in base
PKGS=$(cat metadata | grep "^[a-zA-Z0-9\-]\+\-[0-9\.]\+" | grep -v "^haskell\(98|2010\)")

[ -f list.txt ] && rm list.txt
for PKG in $PKGS
do
  LINK=https://hackage.haskell.org/package/$PKG/$PKG.tar.gz
  echo $LINK >> list.txt
done
cat list.txt

echo ==== wget
wget -i list.txt 2> wget.log

echo ==== unpack
ls *tar.gz | xargs -n1 tar xzf

echo ==== list
SRCS=$(find . | grep '\.l\?hs$')
[ -f files.txt ] && rm files.txt
for SRC in $SRCS
do
  FULL_PATH=$(readlink -m $SRC)
  SRC_PKG=$(echo $SRC | cut -d/ -f2)
  echo $SRC_PKG $FULL_PATH >> files.txt
done
