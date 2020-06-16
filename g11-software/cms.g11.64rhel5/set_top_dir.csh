#! /bin/csh -f

if ( ${?TOP_DIR}) then
  setenv OLDTOP_DIR $TOP_DIR
endif

setenv TOP_DIR  $1

if (${?LD_LIBRARY_PATH}) then
  if ( ${?OLDTOP_DIR}) then
    if (${?CLAS_CMS}) then
      if (-f ${CLAS_CMS}/replace.csh) then
#        echo Replacing occurence of $OLDTOP_DIR in LD_LIBRARY_PATH with $TOP_DIR
        setenv LD_LIBRARY_PATH `set argv=($OLDTOP_DIR/slib/$OSCLAS $TOP_DIR/slib/$OSCLAS $LD_LIBRARY_PATH); source $CLAS_CMS/replace.csh`
      endif
    endif
    unsetenv OLDTOP_DIR
  else
    setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${TOP_DIR}/slib/${OSCLAS}
  endif
else
  setenv LD_LIBRARY_PATH ${TOP_DIR}/slib/${OSCLAS}
endif
