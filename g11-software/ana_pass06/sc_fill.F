      SUBROUTINE SC_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: sc_fill.F,v 1.3 2000/11/10 21:23:02 boiarino Exp $
c
c  Documentation for subroutine SC_FILL
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: sc_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/11/10 21:23:02 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: sc_fill.F,v 1.3 2000/11/10 21:23:02 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SC_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NAMISC1, IND, MAMIND, ND, SC1_COL, sector, indx, in
      EXTERNAL MAMIND
      integer hit, hid
      real scp_st,sc1_l(2,20,6),sc1_r(2,20,6),sc1_pd(20,6)
c_end_var
c
c  executable code for routine SC_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling TOF Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      call hcdir('//PAWC/SC',' ')
      IF(NAMISC1.EQ.0) NAMISC1=MAMIND(IW,'SC1 ')
      IND=NAMISC1+1
  13  IND=IW(IND-1)
      IF(IND.NE.0) THEN
        ND = IW(IND)
        SC1_COL=IW(IND-5)
        sector=IW(ind-2)
        hid=sector*100
        if (sector .gt. 6 .or. sector .lt. 1) then
           call recmes(crname,'w','Bad Sector ID')
           goto 13
        endif
        hit=0
        DO in = 1,ND,SC1_COL
          indx = ind + in -1
          if(hit.lt.20)then
          scp_st=0.
          hit=hit+1
          sc1_pd(hit,sector)=float(iw(indx+1))
          sc1_l(1,hit,sector)=rw(indx+2)
          sc1_l(2,hit,sector)=rw(indx+3)
          sc1_r(1,hit,sector)=rw(indx+4)
          sc1_r(2,hit,sector)=rw(indx+5)
          if(rw(indx+2).gt.0)scp_st=scp_st+10.
          if(rw(indx+4).gt.0)scp_st=scp_st+20.
          if(rw(indx+3).gt.0)scp_st=scp_st+1.
          if(rw(indx+5).gt.0)scp_st=scp_st+2.
          call hf2(hid+1,scp_st,sc1_pd(hit,sector),1.)
          call hf2(hid+11,sc1_l(1,hit,sector)-sc1_r(1,hit,sector),sc1_pd(hit,sector),1.)  ! time diff.
          call hf2(hid+21,sc1_l(2,hit,sector),sc1_pd(hit,sector),1.)  ! left ADC
          call hf2(hid+22,sc1_r(2,hit,sector),sc1_pd(hit,sector),1.)  ! right ADC
          endif
        ENDDO
        call hf1(100,float(sector),float(hit))
        GO TO 13
      ENDIF
c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




