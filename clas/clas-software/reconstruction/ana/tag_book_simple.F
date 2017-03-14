      SUBROUTINE TAG_BOOK_SIMPLE
c
c_begin_doc
c  RCS ID string
c  $Id: tag_book_simple.F,v 1.1 1998/10/22 13:49:38 anciant Exp $
c
c  Documentation for subroutine TAG_BOOK
c
c  Purpose: booking of simplified tagger control histograms to be used with
c  -------  recsis on-line and cooking scripts.
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
#include "tag_general.PAR"
#include "clasmdl.inc"
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
      PARAMETER (CFILE = '$RCSfile: tag_book_simple.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/10/22 13:49:38 $')
      PARAMETER (CAUTHO = '$Author: anciant $')
      DATA CRCSID/   
     1'$Id: tag_book_simple.F,v 1.1 1998/10/22 13:49:38 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------

	character*6 names(12)
	data names / 'T_id','TIME_L','TIME_R','E_id','TIME_E','TTAG', 'TRF' ,'GRF','RF1','RF2' ,'Nbhit', 'TNEXT' /
	character*6 namesr(6)
	data namesr / 'ener','time','TIMErf','STAT','T_id','E_id' /
	character*6 namesdsd(12)
	data namesdsd / 'Id', 'Time','ELT','ERT','ELB','ERB','ESUM','ESUM2' ,'ESUM3', 'T_id', 'T_time', 'trigt'/
     
c_end_var
c
c  executable code for routine TAG_BOOK:
c----6----------------------------------------------------------------72
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      call hmdir('TAG','S')
      call hcdir('//PAWC',' ')
      call hmdir('TAG','S')
      call hcdir('//PAWC/TAG','S')

c  Do your HbookS below this line

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c -------------------- Hodoscope stuff                       ----------
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c
c Histograms
c

       call hbook1(2,'Number of good hits/event',10,-.5,9.5,0.) 
       
c -----------   E pattern histo

       call hbook1(100,'Pattern E binned',767,.5,767.5,0.)

c ----------    Energy pattern

       call hbook1(10,'Energy pattern (GeV)',1000,0.,4.,0.)

c ----------   Time pattern

       call hbook1(11,'TAGR time (ns)',640,-32.,32.,0.)

c --------- TDC slope calibration histo

       call hbook2(12,'T TDC slope check L/R balance',160,0.,1600.,160,0.,32.,0.)
       call hbook2(13,'T TDC slope check T/RF balance',160,0.,1600.,160,0.,16.,0.)
       call hbook2(14,'RF calibration check (goodRF-T) vs RF1',200,0.,100.,100,-1.002,1.002,0.)
       call hbook2(15,'RF calibration check (goodRF-T) vs RF2',200,0.,100.,100,-1.002,1.002,0.)

c -----------   T pattern histo
 
       call hbook1(101,'Pattern T binned in time',121,.5,121.5,0.)
       call hbook1(102,'Pattern T binned all hits',121,.5,121.5,0.)
       call hbook1(103,'Pattern T unbinned in time',61,.5,61.5,0.)
       call hbook1(104,'Pattern T unbinned all hits',61,.5,61.5,0.)

c
c  ---- next histogram to check E-T time coincidence
c

       call hbook2(200,'Ttime - Etime (ns) vs E_id',767,.5,767.5,100,-20.,20.,0.)  

c
c---- next histograms to check alignement of T_times with RF as reference time
c

       call hbook2(300,'Ttime - RFtime (ns) vs T id',121,.5,121.5,100,-1.002,1.002,0.)


c-------- this histogram to see if there are aligned, with PC,PS as reference.

       call hbook2(301,'Ttime - PCtime (ns) vs T id',121,.5,121.5,128,-16.,16.,0.)
       call hbook2(302,'Ttime - PStime (ns) vs T id',121,.5,121.5,128,-16.,16.,0.)
       call hbook2(304,'Ttime - STtime (ns) vs T id',121,.5,121.5,128,-16.,16.,0.)

c
c   ---- next histogram to check alignement at the input of the TS using PC,PS,ST
c
       call hbook2(311,'T-Signal arrival time at Trigger supervisor (ns) (using PC as ref) vs T id',121,.5,121.5,200,-20.,20.,0.) 
       call hbook2(312,'T-Signal arrival time at Trigger supervisor (ns) (using PS as ref) vs T id',121,.5,121.5,200,-20.,20.,0.) 
       call hbook2(314,'T-Signal arrival time at Trigger supervisor (ns) (using ST as ref) vs T id',121,.5,121.5,160,-40.,40.,0.) 
        
c
c   ---- This histogram to see the the RF structure
c
       call hbook1(400,'T0 tagger - RF (ns)',100,-1.002,1.002,0.)

       

      RETURN
      END
c
c------------------------------------------------------------------------------















