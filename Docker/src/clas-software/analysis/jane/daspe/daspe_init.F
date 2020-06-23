      SUBROUTINE DASPE_INIT (ierr)
c
c_begin_doc
c  RCS ID string
c  $Id$
c
c  Documentation for subroutine DASPE_INIT
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:         Created:  Wed Dec 11 17:16:27 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "daspe_data.inc"
#include "delta_dat.inc"
#include "inpar.inc"
#include "fit_par.inc"
#include "daspetab.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
	REAL at2,at3,at4,mp33r(144)
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile$')
      PARAMETER (CREVIS = '$Revision$')
      PARAMETER (CSTATE = '$State$')
      PARAMETER (CDATE = '$Date$')
      PARAMETER (CAUTHO = '$Author$')
      DATA CRCSID/   
     1'$Id$'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='DASPE_INIT')
      PARAMETER (CRAUTH='stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      CHARACTER*10 dummych
      CHARACTER*60 M3dat, E3dat, S3dat, OM3dat, OE3dat, OS3dat, HOMdat
      INTEGER i,LINPUT
      data LINPUT/21/
      integer ipow, itest
c_end_var
c
c  executable code for routine DASPE_INIT:
c----6----------------------------------------------------------------72
c  Begin of constants input
      CALL lang_def
      CALL etta
c  End of constants input

      write(6,*)'Reading the input file'
      OPEN (UNIT=LINPUT,FILE='daspe_input',STATUS='OLD')
      READ (LINPUT,51)dummych,fitopt 
      Write(6,*)'Action is    ',fitopt
 51   FORMAT(A10,A10)
      If(fitopt.Eq.'MF'.or.fitopt.Eq.'mf')call read_multables
      READ (LINPUT,*)
      READ (LINPUT,*) Pi_type2fit, obsv2fit
c
      IF((Pi_type2fit.LE.0).OR.(Pi_type2fit.GE.4)) THEN
        WRITE(*,*)'Your entered type of Pion is not correct'
        WRITE(*,*)'Posible entres are [1/2/3] for [Pi0/Pi+/Pi-]'
        RETURN
      ENDIF
c
      do ipow = 1, 4 
         itest = amod( float(obsv2fit), 10.**(ipow) ) / 10**(ipow-1) 
         if ( itest .GT. 9 ) then
            write(*,*)'Your choice of observables is incorrect'
            write(*,*)'Possible choices are 1, 10, 11, 100, 101 etc '
            return
         endif
      enddo
      READ (LINPUT,*) Ebeam
      READ (LINPUT,*) WMIN,WMAX,WSTEP
      READ (LINPUT,*) Q2MIN,Q2MAX,Q2STEP
      READ (LINPUT,*) COSMIN,COSMAX,COSSTEP
      READ (LINPUT,*) PHIMIN,PHIMAX,PHISTEP
      READ (LINPUT,*) IntAccur
      READ (LINPUT,*) tabfile
      READ (LINPUT,*) inffile
      READ (LINPUT,*) M3dat
 52   FORMAT(A10,A60)
      READ (LINPUT,*)
      READ (LINPUT,*) MAXEVENTS
      READ (LINPUT,*) MAXITER
      READ (LINPUT,*) Randomseed
c
         READ (LINPUT,*)
         READ (LINPUT,54)maxcalls
         READ (LINPUT,54)tolorance
         READ (LINPUT,54)errorup
 54      format(f8.2)
         READ (LINPUT,*) parms
         If( parms.Le.MAXpar .And. parms.GT.0 ) Then
            READ (LINPUT,*)
            Do i=1,parms
               read(LINPUT,53)plist(i),pname(i), start_value(i)
     +             ,step_size(i),par_stat(i),low_lim(i),up_lim(i)
 53            FORMAT(i2,1x,A10,2f10.4,i10,2f10.4)
            EndDo
         Else
            Write(6,*)'Wrong Number of parameters    ',parms
            Return
         EndIf
         READ (LINPUT,*)
         READ (LINPUT,52)datform, Datafile
         READ (LINPUT,*)
         READ (LINPUT,*)lmini,lmino,lminf
         READ (LINPUT,*)chimax
         READ (LINPUT,*)Nbad
         If( Nbad.gt.0 )Then
            Do i=1,Nbad
               READ (LINPUT,*)ibad(1,i),ibad(2,i)
            EndDo
         EndIf
c      ENDIF
      open(UNIT=28,file='../work/mp33.dat',status='old')
      do i=1,144
c      read(28,*,end=110)wsa(i),mp33r(i),mp33i(i),at2,at3,at4
      read(28,*,end=110)wsa(i),mp33r(i),at2,mp33i(i),at3
         wsa(i)=wsa(i)/1000.
         mp33r(i)=mp33r(i)/52.437
         mp33i(i)=mp33i(i)/52.437
      enddo
 110  continue
      close(28)
c     
        RETURN
        END
c
c------------------------------------------------------------------------------




