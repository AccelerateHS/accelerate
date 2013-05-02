#!/bin/sh
#
# Update this to point to your GHC installation, then run the script.
# It calls GHC to build the Process.hs module.
#
# It also makes a dir 'hslibs' in this one, and fills it with links to your
# Haskell libraries, so the XCode project can find them.
#
# The version numbers are correct for GHC 7.4.1. For other versions you'll
# need to update the library version numbers, as well as the linker flags
# in XCode in Project/Build Settings/Other Linker Flags.
#
# Run the script thusly:
#   sh ./CONFIGURE.sh
#

GHC_VERSION=7.4.1
INSTALL_GHC=/Library/Frameworks/GHC.framework/Versions/Current/usr
INSTALL_CABAL=`pwd`/cabal-dev

LIB_GHC=$INSTALL_GHC/lib/ghc-$GHC_VERSION
LIB_CABAL=$INSTALL_CABAL/lib

mkdir -p hslibs

# ------------------------------------------------------------------
echo "** Building Process.hs"
# -fllvm -optlo-O3 
ghc -rtsopts -threaded -Odph -fno-liberate-case \
    -funfolding-use-threshold100 -funfolding-keeness-factor100 \
    -package-conf $INSTALL_CABAL/packages-$GHC_VERSION.conf \
    -odir hslibs -hidir hslibs --make -c Process.hs

# -----------------------------------------------------------------

echo "** Adding links to Haskell libs."
cd hslibs

# These libs come with the GHC install.
#
ghc_prim=ghc-prim-0.2.0.0
integer_gmp=integer-gmp-0.4.0.0
base=base-4.5.0.0

array=array-0.4.0.0
binary=binary-0.5.1.0
bytestring=bytestring-0.9.2.1
containers=containers-0.4.2.1
deepseq=deepseq-1.3.0.0
directory=directory-1.1.0.2
extensible_exceptions=extensible-exceptions-0.1.1.4
filepath=filepath-1.3.0.0
old_locale=old-locale-1.0.0.4
old_time=old-time-1.1.0.0
pretty=pretty-1.1.1.0
process=process-1.1.0.1
template_haskell=template-haskell-2.7.0.0
time=time-1.4
unix=unix-2.5.1.0

ln -sf $LIB_GHC/libHSrts_thr.a

for lib in $ghc_prim $integer_gmp $base $array $binary  $bytestring \
  $containers $deepseq $directory $extensible_exceptions $filepath $old_locale \
  $old_time $pretty $template_haskell $time $unix $process ; do
    LIBRARIES="$LIBRARIES $LIB_GHC/$lib/HS$lib.o"
done

# These libs are installed by Cabal
#
accelerate=accelerate-0.13.0.0
accelerate_cuda=accelerate-cuda-0.13.0.0
accelerate_io=accelerate-io-0.13.0.0
blaze_builder=blaze-builder-0.3.1.0
bmp=bmp-1.2.1.1
cereal=cereal-0.3.5.1
cpphs=cpphs-1.13.3
crypto_api=crypto-api-0.10.2
cryptohash=cryptohash-0.7.5
cuda=cuda-0.4.1.1
entropy=entropy-0.2.1
exception_mtl=exception-mtl-0.3.0.2
exception_transformers=exception-transformers-0.3.0.2
fclabels=fclabels-1.1.3
hashable=hashable-1.1.2.3
hashtables=hashtables-1.0.1.6
haskell_src_exts=haskell-src-exts-1.13.3
haskell_src_meta=haskell-src-meta-0.5.1.2
language_c_quote=language-c-quote-0.3.1.2
largeword=largeword-1.0.1
mainland_pretty=mainland-pretty-0.1.2.0
mtl=mtl-2.1.1
primitive=primitive-0.4.1
QuickCheck=QuickCheck-2.4.2
random=random-1.0.1.1
repa=repa-3.2.1.1
srcloc=srcloc-0.1.1.1
stm=stm-2.3
syb=syb-0.3.6.1
symbol=symbol-0.1.1.1
tagged=tagged-0.4.2.1
text=text-0.11.2.1
th_lift=th-lift-0.5.5
transformers=transformers-0.3.0.0
unordered_containers=unordered-containers-0.2.1.0
vector=vector-0.9.1

for lib in $accelerate $accelerate_cuda $accelerate_io $blaze_builder $bmp \
  $cereal $cpphs $crypto_api $cryptohash $cuda $entropy $exception_mtl \
  $exception_transformers $fclabels $hashable $hashtables $haskell_src_exts \
  $haskell_src_meta $language_c_quote $largeword $mainland_pretty $mtl \
  $primitive $QuickCheck $random $repa $srcloc $stm $syb $symbol $tagged $text \
  $th_lift $transformers $unordered_containers $vector ; do
    LIBRARIES="$LIBRARIES $LIB_CABAL/$lib/ghc-$GHC_VERSION/HS$lib.o"
done

ar -rv libHS.a $LIBRARIES

cd ..


