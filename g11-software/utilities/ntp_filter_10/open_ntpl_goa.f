
      SUBROUTINE open_ntpl_goa(InFile,istep)
      
      IMPLICIT NONE  
      include "ntpl_goa.inc"
      INCLUDE "ntpl_structure.inc"
      integer*4 istat,lrecl,istep
      CHARACTER*(*) InFile
      data lrecl/8192/

c      call hlimit(100)
c ----- OPEN input nt of NEXT file and set -----
          lrecl = 0
      call hropen(12,'ESCA',InFile,' ',lrecl,istat)
        IF(istat.ne.0) print *,' ***Can not open InFile:',InFile
        IF(istat.ne.0) print *,' ***istat,lrecl=',istat,lrecl
        IF(istat.ne.0) print *,' ***trying one more time'
        IF(istat.ne.0) lrecl=0
        IF(istat.ne.0) call hrend('ESCA')
        IF(istat.ne.0) call hropen(12,'ESCA',InFile,' ',lrecl,istat)
c-------------- CEB nt ------------------------------

      if (istep.eq.0.or.istep.eq.1) THEN ! open nt 60 (first time)
       call hrin  (ntupl_number,9999,0)
c+ Getting norm histos
       call hrin  (71,9999,0)
       call hrin  (72,9999,0)
       call hrin  (73,9999,0)
       call hrin  (74,9999,0)
c      call hbname(ntupl_number,' ',0,'$CLEAR')
c       CALL HBNAME(ntupl_number,'PAWCR4',n_ev_run,'$SET')
c       call ntp_structure(ntupl_number)


        call hcdir('//DST',' ')
       if(istep.eq.0) then
        CALL HNTDUP (ntupl_number,93,-1,'DST','A')
c        call hldir ('//PAWC','T')
       else if(istep.eq.1) then
        CALL HNTDUP (ntupl_number,92,-1,'DST','A')
c        call hldir ('//PAWC','T')
       endif

      ELSEif (istep.eq.2) then
       call hrin  (93,9999,0)
c       call hbname(93,' ',0,'$CLEAR')
c       call ntp_structure(93)
       call hcdir('//PART',' ')
       CALL HNTDUP (93,ntupl_number,-1,'PART','A')
      ENDIF


      RETURN
      END
