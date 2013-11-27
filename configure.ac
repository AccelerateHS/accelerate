AC_INIT([accelerate], [0.15.0.0], [accelerate-haskell@googlegroups.com])
AC_CONFIG_SRCDIR([Data/Array/Accelerate.hs])
AC_CONFIG_FILES([accelerate.buildinfo])

AC_ARG_WITH([compiler], [Haskell compiler], [GHC=$withval],  [AC_PATH_PROG(GHC, ghc)])
AC_ARG_WITH([gcc],      [C compiler],       [CC=$withval])

AC_DEFUN([AC_HTYPE], [
    AC_MSG_CHECKING(size of [$1])

    sizeof_hs_[$1]=`$GHC -w -ignore-dot-ghci -e "import Foreign" -e "import Foreign.C" -e "putStr . show $ sizeOf (undefined::$1)"`
    AC_MSG_RESULT($sizeof_hs_[$1])

    case $sizeof_hs_[$1] in
        4) [$4]=[$2]32 ;;
        8) [$4]=[$2]64 ;;
    esac

    # The type name
    def="-D[$3]=$[$4]"
    cpp_flags="$cpp_flags $def"
    ghc_flags="$ghc_flags -optP$def"

    # And size
    def="-DSIZEOF_[$3]=$sizeof_hs_[$1]"
    cpp_flags="$cpp_flags $def"
    ghc_flags="$ghc_flags -optP$def"
])

AC_HTYPE(CLong,  Int,  HTYPE_LONG,          type_hs_long)
AC_HTYPE(CULong, Word, HTYPE_UNSIGNED_LONG, type_hs_unsigned_long)

AC_SUBST([ghc_flags])
AC_SUBST([cpp_flags])
AC_OUTPUT

