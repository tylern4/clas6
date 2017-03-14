      SUBROUTINE SC_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: sc_book.F,v 1.3 2008/03/16 04:33:10 fklein Exp $
c
c  Documentation for subroutine SC_BOOK
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
      PARAMETER (CFILE = '$RCSfile: sc_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:10 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: sc_book.F,v 1.3 2008/03/16 04:33:10 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SC_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,i
      CHARACTER*3 si(6)
      data si/'S1','S2','S3','S4','S5','S6'/
c_end_var
c
c  executable code for routine SC_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking TOF histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      call hcdir('//PAWC',' ')
      call hmdir('SC','S')

c  Do your HbookS below this line
      do i=1,6
         call hbook2(i*100+1,si(i)//'SC PDid vs paddle status',35,0.,35.,60,0.5,60.5,0.)
         call hbook2(i*100+11,si(i)//'SC PDid vs TDSl-TDCr',80,-40.,40.,60,0.5,60.5,0.)
         call hbook2(i*100+21,si(i)//'SC PDid vs ADSl',50,0.,200.,60,0.5,60.5,0.) 
         call hbook2(i*100+22,si(i)//'SC PDid vs ADSR',50,0.,200.,60,0.5,60.5,0.) 
      enddo
      call hbook1(100,'Number of SC Hits in Sector',7,0.5,7.5,0.) 
c changed back to top directory
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------















