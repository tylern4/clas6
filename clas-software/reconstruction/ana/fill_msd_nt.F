      SUBROUTINE FILL_MSD_NT
c
c_begin_doc
c  RCS ID string
c  $Id: fill_msd_nt.F,v 1.1 2004/04/30 22:00:08 stepanya Exp $
c
c  Documentation for subroutine FILL_MSD_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1  
c                     
c
c  Output Parameters:  Fill's areas in MSD_NT common block.
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Aug Dec 14 00:42:15 EST 1998
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
#include "bcs.inc"
#include "msd_nt.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_msd_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/04/30 22:00:08 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_msd_nt.F,v 1.1 2004/04/30 22:00:08 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_MSD_NT')
      PARAMETER (CRAUTH='stepanya')
c
c  Local User defined variables:
c  -----------------------------
      integer*2 I16(4000)            ! 16 bits
      integer*4 I32(2000)            ! 32 bits
      equivalence (I16(1), I32(1))
      INTEGER NWRITE, pedestal(4,385), spars(4,385)
      INTEGER  loop,pl,id
      INTEGER  ind,ids,bit14,bit13
      INTEGER  MAMIND, jbit
      EXTERNAL  MAMIND, jbit
      INTEGER IMSD
      DATA IMSD, NWRITE/0,0/
      real a,b,c
      CHARACTER*99  call_map
c_end_var
c
c  executable code for routine FILL_MSD_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Ntuple from MSD'
        CALL RECMES(CRNAME,'I',CRMESS)
        call revinm('CLAS_PARMS','plane1.dat',call_map)
        open(77,file=call_map,status="old")
 11     read(77,*,end=12,err=12)id,a,b,c
        pedestal(1,id)=int(a)
        spars(1,id)=int(c)
        goto 11
 12     close(77)
        call revinm('CLAS_PARMS','plane2.dat',call_map)
        open(77,file=call_map,status="old")
c        open(77,file="plane2.dat",status="old")
 21     read(77,*,end=22,err=22)id,a,b,c
        pedestal(2,id)=int(a)
        spars(2,id)=int(c)
        goto 21
 22     close(77)
        call revinm('CLAS_PARMS','plane3.dat',call_map)
        open(77,file=call_map,status="old")
 31     read(77,*,end=32,err=32)id,a,b,c
        pedestal(3,id)=int(a)
        spars(3,id)=int(c)
        goto 31
 32     close(77)
        call revinm('CLAS_PARMS','plane4.dat',call_map)
        open(77,file=call_map,status="old")
 41     read(77,*,end=42,err=42)id,a,b,c
        pedestal(4,id)=int(a)
        spars(4,id)=int(a+6*b)
        goto 41
 42     close(77)
      ENDIF
      IMSD=MAMIND(IW,'MS1 ')
      IF(IMSD.GT.0)ind=IW(IMSD)
      nmsd=0
      IF(IND.GT.0)THEN
        do loop=1,IW(ind)
           i32(loop)=iw(ind+loop)
        enddo
c
        do loop=1,2*IW(ind),2
           ids=i16(loop)
           bit13=jbit(ids,13)
           bit14=jbit(ids,14)
           pl=bit13+bit14*2+1
           id=ids-bit13*2**12-bit14*2**13+1
           if(i16(loop+1).gt.spars(pl,id).and.(pl.lt.4.or.(pl.eq.4.and.id.lt.258)))then
              nmsd=nmsd+1
              msdpl(nmsd)=pl
              msdid(nmsd)=id
              msdadc(nmsd)=i16(loop+1)-pedestal(pl,id)
           endif
        enddo
      ENDIF
99    RETURN
      END
c
c------------------------------------------------------------------------------




