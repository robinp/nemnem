mkdir pull
cd pull

PKGS=$(ls ../.cabal-sandbox/lib/i386-linux-ghc-7.8.3)
for PKG in $PKGS
do
  LINK=https://hackage.haskell.org/package/$PKG/$PKG.tar.gz
  wget $LINK
done

ls *tar.gz | xargs -n1 tar xzf

SRCS=$(find . | grep '\.hs$')
rm files.txt
for SRC in $SRCS
do
  FULL_PATH=$(readlink -m $SRC)
  SRC_PKG=$(echo $SRC | cut -d/ -f2)
  echo $SRC_PKG $FULL_PATH >> files.txt
done
