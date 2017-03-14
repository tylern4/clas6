      SUBROUTINE FSGEN_INI_BOS(ierr,ifile)
c  ---------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Tue Mar 25 14:32:25 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c     
      IMPLICIT NONE
C
#include "bcs.inc"
#include "parm2.inc"
C
      character*5 cunit
      character*80 evin, evout
      character*7 evout1
      integer ierr,ifile,llb,lenocc,llenb
      external lenocc
C
      CALL t2b('celeg.ddl')
      IF(bin.eq.0)THEN
         evin='bosin'
         ierr=0
         Call BosOpen(evin,'r',jin)
      ELSE
         write(LOUT,*)'There are no input data'
      ENDIF
c     
      IF(bout.eq.0)THEN
c         evout=lnam
         llenb=lenocc(lnam)
         ierr=0
         if(ifile.lt.10)then
            write(evout1,101)ifile,'.evt'
 101        format(i1,a4)
            write(evout,*)lnam(1:llenb),'00',evout1
         elseif(ifile.lt.100)then
            write(evout1,102)ifile,'.evt'
 102        format(i2,a4)
            write(evout,*)lnam(1:llenb),'0',evout1
         else
            write(evout1,103)ifile,'.evt'
 103        format(i3,a4)
            write(evout,*)lnam(1:llenb),evout1
         endif
         llb=lenocc(evout)
         print *,evout(1:llb),llb
         Call BosOpen(evout(1:llb),'w',jout)
      ELSE
         write(LOUT,*)'There will be no output data'
      ENDIF
      RETURN
      END
