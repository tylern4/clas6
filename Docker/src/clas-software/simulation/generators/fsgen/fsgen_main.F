      PROGRAM FSGEN_MAIN
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
C
      IMPLICIT NONE
C
#include "parm1.inc"
#include "parm2.inc"
#include "leptou.inc"
#include "ludat.inc"
#include "lujet.inc"
#include "gbd.inc"
#include "bcs.inc"  
      INTEGER I,ierr,icycle,counter,nevent
c      REAL TOTMASS
      REAL T1, T2, T3, T4
C
      INTEGER ijklut, ntotut, nto2ut, nfiles
      LOGICAL fiducial
C
C       force linker to load these routines
C
      EXTERNAL LUDATA
      EXTERNAL LUEDIT
C
      ICYCLE=0
      nfiles=1
C
      CALL TIMEX(T1)
      CALL fsgen_init(ierr)
      if(ierr.ne.0)stop
      nfiles=stfile
 150  continue
      counter=0
      nevent=0
      CALL TIMEX(T2)
C ********************************************************
C	 End of initialisation. 
C	 Now get the photon parameters.
C ********************************************************
 151  CALL fsgen_phot(ierr,nevent)
      IF(ierr.eq.0) THEN
C ********************************************************
C	 Get the masses of final state particles 
C ********************************************************
         DO i = 1,maxiter
            CALL fsgen_evnt(ierr,nevent)
            if(ierr.ne.0)go to 151
            IF (DECAY)CALL LUEXEC
            IF (EDIT) CALL LUEDIT(2)
            IF (LIST) THEN
               icycle=icycle+1
               if(icycle.lt.15)CALL LULIST (3)
            ENDIF
            nevent=nevent+1
            CALL fsgen_output(ierr,nevent,fiducial)
            if(ierr.lt.-1)goto 151
            if(hout.eq.0.and.nfiles.eq.1)call fsgen_hist(2,nevent,
     &          fiducial)
            if(ierr.gt.0)then
               WRITE(lout,*)'Error writing BOS file'
               stop
            endif
         ENDDO
         if(fiducial)counter=counter+1
         if(bin.eq.0.or.(bin.ne.0.and.counter.lt.MAXEVENTS))GOTO 151
C     
      ELSEIF(ierr.lt.0)THEN
         WRITE(lout,*)'End of input file'
      ELSEIF(ierr.gt.0)THEN
         WRITE(lout,*)'Error in generating an event ',nevent
      ENDIF
      CALL TIMEX(T3)
      IF ( hout.EQ.0.and.nfiles.eq.1 ) CALL fsgen_hist(3,0,fiducial)
      IF ( bout.EQ.0.OR.bin.EQ.0) CALL FSGEN_END_BOS(ierr)
      if(ierr.ne.0)WRITE(lout,*)'Error closing BOS file'
      WRITE(lout,502) nevent,T2-T1,T3-T2,
     &     (T3-T2)/MAX(1,counter)
 502  FORMAT(/,' Number of events generated:              ',I12,
     &     /,' Time for initialization (INIT) :         ',F12.3,' s',
     &     /,' Time for event generation,     total:    ',F12.3,' s',
     &     /,'                   per fiducial event:    ',F12.3,' s')
      
      call rmarut(ijklut,ntotut,nto2ut)
      write(lout,503) ijklut,ntotut,nto2ut
 503  FORMAT(/,' Seed for random numbers:                 ',I9,
     $     /,' Number (mod 10^9) of random numbers gen: ',I9,
     $     /,' Billions of random numbers generated:    ',I9)
      if(nfiles.lt.maxfiles.and.bout.eq.0)then
         nfiles=nfiles+1
         CALL fsgen_ini_bos(ierr,nfiles)
         go to 150
      endif
c
      END
c+++++++++++++++++++++++++++++++++++++++++++++


