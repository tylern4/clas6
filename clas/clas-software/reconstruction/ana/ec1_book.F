      SUBROUTINE EC1_BOOK
c
c_begin_doc
c  RCS ID string
c  $Id: ec1_book.F,v 1.7 2008/03/16 04:33:09 fklein Exp $
c
c  Documentation for subroutine EC1_BOOK
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
      PARAMETER (CFILE = '$RCSfile: ec1_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:09 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: ec1_book.F,v 1.7 2008/03/16 04:33:09 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      REAL xm, xmin
c_end_var
c
c  executable code for routine EC1_BOOK:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking LAC Histograms'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      call hcdir('//PAWC',' ')
      call hmdir('EC1','S')
      call hcdir('//PAWC/EC1','S')

c  Do your HbookS below this line
cc      CALL hbname(ID,' ',0,'$clear')
       print *,'+++++++++++++++++++++++  EC1 booking ++++++++++++++'
C ADCs:
       xm = 2000.
       xmin = 500.0
c sector 1
      call hbook2(111,'ADC Inner Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(112,'ADC Inner Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(121,'ADC Inner Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(122,'ADC Inner Short right',100,xmin,xm,40,1.,41.,0.)
      call hbook2(131,'ADC Outer Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(132,'ADC Outer Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(141,'ADC Outer Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(142,'ADC Outer Short right',100,xmin,xm,40,1.,41.,0.)
c sector 2
      call hbook2(211,'ADC Inner Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(212,'ADC Inner Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(221,'ADC Inner Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(222,'ADC Inner Short right',100,xmin,xm,40,1.,41.,0.)
      call hbook2(231,'ADC Outer Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(232,'ADC Outer Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(241,'ADC Outer Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(242,'ADC Outer Short right',100,xmin,xm,40,1.,41.,0.)
C TDCs:
       xm = 10000.0
       xmin = 2000.0
c sector 1
      call hbook2(311,'TDC Inner Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(312,'TDC Inner Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(321,'TDC Inner Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(322,'TDC Inner Short right',100,xmin,xm,40,1.,41.,0.)
      call hbook2(331,'TDC Outer Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(332,'TDC Outer Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(341,'TDC Outer Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(342,'TDC Outer Short right',100,xmin,xm,40,1.,41.,0.)
c sector 2
      call hbook2(411,'TDC Inner Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(412,'TDC Inner Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(421,'TDC Inner Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(422,'TDC Inner Short right',100,xmin,xm,40,1.,41.,0.)
      call hbook2(431,'TDC Outer Long   left',100,xmin,xm,24,1.,25.,0.)
      call hbook2(432,'TDC Outer Long  right',100,xmin,xm,24,1.,25.,0.)
      call hbook2(441,'TDC Outer Short  left',100,xmin,xm,40,1.,41.,0.)
      call hbook2(442,'TDC Outer Short right',100,xmin,xm,40,1.,41.,0.)

      call hbook2(1001,'Population Sect.1 In' ,24,1.,25.,40,1.,41.,0.)
      call hbook2(1002,'Population Sect.1 Out',24,1.,25.,40,1.,41.,0.)
      call hbook2(2001,'Population Sect.2 In' ,24,1.,25.,40,1.,41.,0.)
      call hbook2(2002,'Population Sect.2 Out',24,1.,25.,40,1.,41.,0.)

      call hbook2(1101,'DepEnergy Sect.1 In ',24,1.,25.,40,1.,41.,0.)
      call hbook2(1102,'DepEnergy Sect.1 Out',24,1.,25.,40,1.,41.,0.)
      call hbook2(2101,'DepEnergy Sect.2 In ',24,1.,25.,40,1.,41.,0.)
      call hbook2(2102,'DepEnergy Sect.2 Out',24,1.,25.,40,1.,41.,0.)

c changed back to top directory
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------















