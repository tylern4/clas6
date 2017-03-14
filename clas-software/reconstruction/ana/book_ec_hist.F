      SUBROUTINE BOOK_EC_HIST
c
c_begin_doc
c  RCS ID string
c  $Id: book_ec_hist.F,v 1.5 2007/04/15 21:37:12 lcsmith Exp $
c
c  Documentation for subroutine BOOK_EC_HIST
c
c  Purpose: Booking hisyograms for EC off-line analyses.
c  --------
c
c  Input Parameters: NON
c  ----------------
c
c  Output Parameters: NON
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:     S.Stepanyan    Created:  Sun Jun  8 11:47:04 EDT 1997
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
c#include "bcs.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_ec_hist.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2007/04/15 21:37:12 $')
      PARAMETER (CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/   
     1'$Id: book_ec_hist.F,v 1.5 2007/04/15 21:37:12 lcsmith Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_EC_HIST')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID, HIST1ID, HIST2ID, HIST3ID, HIST4ID
      CHARACTER*3 si(6)
      data si/'S1','S2','S3','S4','S5','S6'/
c_end_var
c
c  executable code for routine BOOK_EC_HIST:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking histograms for forward EC'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      DO id=1,6
c
      hist2id=200+id*1000
c
      call hbook2(hist2id+11,si(id)//' U strip vs E Inner pix',37,0.,37.,50,0.,0.05,0.)
      call hbook2(hist2id+12,si(id)//' V strip vs E Inner pix',37,0.,37.,50,0.,0.05,0.)
      call hbook2(hist2id+13,si(id)//' W strip vs E Inner pix',37,0.,37.,50,0.,0.05,0.)
      call hbook2(hist2id+14,si(id)//' U strip vs E Outer pix',37,0.,37.,50,0.,0.1,0.)
      call hbook2(hist2id+15,si(id)//' V strip vs E Outer pix',37,0.,37.,50,0.,0.1,0.)
      call hbook2(hist2id+16,si(id)//' W strip vs E Outer pix',37,0.,37.,50,0.,0.1,0.)
c
c      call hbook2(hist2id+1,si(id)//' U strip vs E Inner all',37,0.,37,50,0.,1.,0.)
c      call hbook2(hist2id+2,si(id)//' V strip vs E Inner all',37,0.,37,50,0.,1.,0.)
c      call hbook2(hist2id+3,si(id)//' W strip vs E Inner all',37,0.,37,50,0.,1.,0.)
c      call hbook2(hist2id+4,si(id)//' U strip vs E Outer all',37,0.,37,50,0.,1.,0.)
c      call hbook2(hist2id+5,si(id)//' V strip vs E Outer all',37,0.,37,50,0.,1.,0.)
c      call hbook2(hist2id+6,si(id)//' W strip vs E Outer all',37,0.,37,50,0.,1.,0.)
c
      hist3id=300+id*1000
      call hbook1(hist3id+1,si(id)//'  E inner all',200,0.,1.,0.)
      call hbook1(hist3id+2,si(id)//'  E outer all',200,0.,1.,0.)
      call hbook1(hist3id+3,si(id)//'  E whole all',200,0.,1.,0.)
      call hbook1(hist3id+4,si(id)//'  E inner pix',100,0.,0.5,0.)
      call hbook1(hist3id+5,si(id)//'  E outer pix',100,0.,0.5,0.)
      call hbook1(hist3id+6,si(id)//'  E whole pix',100,0.,0.5,0.)
c      call hbook2(hist3id+11,si(id)//'  Ei+Eo vs Ew all',50,0.,1.,50,0.,1.,0.)
c      call hbook2(hist3id+12,si(id)//'  Ei+Eo vs Ew pix',50,0.,1.,50,0.,1.,0.)
      call hbook1(hist3id+13,si(id)//'  Ew-Ei-Eo all',100,-0.5,0.5,0.)
      call hbook1(hist3id+14,si(id)//'  Ew-Ei-Eo pix',100,-0.5,0.5,0.)
      call hbook1(hist3id+15,si(id)//'  Total Ew-Ei-Eo all',100,-0.5,0.5,0.)
      call hbook1(hist3id+16,si(id)//'  Total Ew-Ei-Eo pix',100,-0.5,0.5,0.)
c
      hist4id=400+id*1000
c      call hbook2(hist4id+1,si(id)//'  I vs J Inner all',100,-250.,250.,100,-250.,250.,0.)
c      call hbook2(hist4id+2,si(id)//'  I vs J Outer all',100,-250.,250.,100,-250.,250.,0.)
      call hbook2(hist4id+3,si(id)//'  I vs J Inner pix',100,-250.,250.,100,-250.,250.,0.)
      call hbook2(hist4id+4,si(id)//'  I vs J Outer pix',100,-250.,250.,100,-250.,250.,0.)
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------




