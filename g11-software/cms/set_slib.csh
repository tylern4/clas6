
if ( ${?CLAS_SLIB}) then
  setenv OLDCLAS_SLIB $CLAS_SLIB
endif

setenv CLAS_SLIB  $1

if (${?LD_LIBRARY_PATH}) then
  if ( ${?OLDCLAS_SLIB}) then
    if (${?CLAS_CMS}) then
      if (-f ${CLAS_CMS}/replace.csh) then
        echo Replacing occurence of $OLDCLAS_SLIB in LD_LIBRARY_PATH with $CLAS_SLIB
        setenv LD_LIBRARY_PATH `set argv=($OLDCLAS_SLIB $CLAS_SLIB $LD_LIBRARY_PATH); source $CLAS_CMS/replace.csh`
      endif
    endif
    unsetenv OLDCLAS_SLIB
  else
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${CLAS_SLIB}
  endif
else
  setenv LD_LIBRARY_PATH ${CLAS_SLIB}
endif
