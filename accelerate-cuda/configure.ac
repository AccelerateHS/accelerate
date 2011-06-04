AC_INIT([accelerate-cuda], [0.9.0.0], [accelerate@projects.haskell.org])
AC_CONFIG_SRCDIR([Data/Array/Accelerate/CUDA.hs])
AC_CONFIG_FILES([cubits/accelerate_cuda_shape.h])

# This doesn't pick up a user specified '--with-compiler' flag
#
AC_PROG_CXX(nvcc)
AC_PATH_PROG(GHC, ghc)
AC_MSG_CHECKING(size of Int)

hs_int_size=`$GHC -w -ignore-dot-ghci -e "Foreign.sizeOf (undefined::Int)"`
AC_MSG_RESULT($hs_int_size)

case $hs_int_size in
    4) hs_int_type=int32_t ;;
    8) hs_int_type=int64_t ;;
    *) AC_MSG_ERROR(could not determine size of a Haskell Int)
esac

AC_SUBST([hs_int_type])
AC_OUTPUT

