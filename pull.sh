mkdir pull
cd pull

# TODO get a consistent snapshot somewhere, extracting from local cabal sandbox is an approximate hack

echo ==== pkgs
PKGS=$(ls ../.cabal-sandbox/lib/i386-linux-ghc-7.8.3 /usr/local/lib/ghc-7.8.3 | grep -v ':' | grep '\-[0-9\.]\+' | sort)
[ -f list.txt ] && list.txt
for PKG in $PKGS
do
  # haskellXY packages duplicate modules already in base
  if ! ( echo $PKG | grep -q "^haskell\(98\|2010\)" )
  then
    LINK=https://hackage.haskell.org/package/$PKG/$PKG.tar.gz
    echo $LINK >> list.txt
  fi
done
cat list.txt

echo ==== wget
wget -i list.txt 2> wget.log

echo ==== unpack
ls *tar.gz | xargs -n1 tar xzf

echo ==== list
SRCS=$(find . | grep '\.hs$')
[ -f files.txt ] && rm files.txt
for SRC in $SRCS
do
  FULL_PATH=$(readlink -m $SRC)
  SRC_PKG=$(echo $SRC | cut -d/ -f2)
  echo $SRC_PKG $FULL_PATH >> files.txt
done
