      SUBROUTINE TAG_BOOK(do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_book.F,v 1.22 2008/03/16 04:33:10 fklein Exp $
c
c  Documentation for subroutine TAG_BOOK
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
#include "tag_general.PAR"
#include "clasmdl.inc"
c#include "../tag/event.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

      logical do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt

c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_book.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.22 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:10 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: tag_book.F,v 1.22 2008/03/16 04:33:10 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_BOOK')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE

      CHARACTER*4 pi(24)
      data pi/'Pd1','Pd2','Pd3','Pd4','Pd5','Pd6','Pd7','Pd8','Pd9','Pd10','Pd11','Pd12'
     &  ,'Pd13','Pd14','Pd15','Pd16','Pd17','Pd18','Pd19','Pd20','Pd21','Pd22','Pd23','Pd24' /
      character*6 names(12)
      data names / 'T_id','TIME_L','TIME_R','E_id','TIME_E','TTAG', 'TRF' ,'GRF','RF1','RF2' ,'Nbhit', 'TNEXT' /
      character*6 namesr(6)
      data namesr / 'ener','time','TIMErf','STAT','T_id','E_id' /
      character*6 namesdsd(12)
      data namesdsd / 'Id', 'Time','ELT','ERT','ELB','ERB','ESUM','ESUM2' ,'ESUM3', 'T_id', 'T_time', 'trigt'/
      character*60 fname
      INTEGER irun,run0,run1,i
     
c_end_var
c
c  executable code for routine TAG_BOOK:
c----6----------------------------------------------------------------72
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        if (do_hist) then
           CRMESS='Booking Tagger Histograms'
           CALL RECMES(CRNAME,'I',CRMESS)
        endif
        if (do_tagr_nt) then
           CRMESS='Booking TAGR bank ntuple'
           CALL RECMES(CRNAME,'I',CRMESS)
        endif
        if (do_tagirf_nt) then
           CRMESS='Booking TAGI & RF banks ntuple'
           CALL RECMES(CRNAME,'I',CRMESS)
        endif
        if (do_dsd_nt) then
           CRMESS='Booking DSD ntuple'
           CALL RECMES(CRNAME,'I',CRMESS)
        endif
      ENDIF

c Do the HMDIR stuff twice to declare the subdirectory in memory and disk
      call hcdir('//PAWC',' ')
      call hmdir('//PAWC/TAG','S')

c  Do your HbookS below this line

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c -------------------- Hodoscope stuff                       ----------
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
c  The Tagger ntuples
c     
       if (do_tagirf_nt) call hbookn(1,'TAGI & RF',12,'//LUN1/TAG',50000,names)
       if (do_tagr_nt)   call hbookn(4,'TAGR',6,'//LUN1/TAG',50000,namesr)

c
c Histograms
c

       if (do_hist) then


c-------- NUMBER OF HITS IN THE TAGGER
          call hbook1(101,'Number of hits/event from TAGR bank'     ,30,-.5,29.5,0.) 
          call hbook1(102,'Number of good hits/event from TAGR bank',30,-.5,29.5,0.) 
          call hbook1(103,'Number of bad hits/event from TAGR bank' ,30,-.5,29.5,0.) 
          call hbook1(104,'Number of hits/event from TGPB bank'     ,30,-.5,29.5,0.) 

c -------- scalers histograms
          call hbook1(105,'Raw T scalers',61,.5,61.5,0.)
       

c -------- ENERGY OF THE PHOTONS
          call hbook1(201,'Energy spectrum for all photons (GeV)'    ,1000,0.,4.,0.)
          call hbook1(202,'Energy spectrum for good photons (GeV)'   ,1000,0.,4.,0.)
          call hbook1(203,'Energy spectrum for bad photons (GeV)'    ,1000,0.,4.,0.)
          call hbook1(204,'Energy spectrum from TGPB bank (GeV)'     ,1000,0.,4.,0.)
          call hbook1(205,'Energy spectrum for the best photon (GeV)',1000,0.,4.,0.)
 

c -------- TIME OF THE PHOTONS
          call hbook1(301,'RF corrected photon time (ns) - all photons',640,-32.,32.,0.)
          call hbook1(302,'RF corrected photon time (ns) - good photon',640,-32.,32.,0.)
          call hbook1(303,'RF corrected photon time (ns) - bad photons',640,-32.,32.,0.)
          call hbook1(304,'RF corrected photon time (ns) - TGPB bank'  ,640,-32.,32.,0.)
          call hbook1(305,'RF corrected photon time (ns) - best photon',640,-32.,32.,0.)


c -------- TDC slope calibration histo

          call hbook2(1001,'T TDC slope check L/R balance',160,0.,1600.,160,0.,32.,0.)
          call hbook2(1002,'T TDC slope check T/RF balance',160,0.,1600.,160,0.,16.,0.)
          call hbook2(1003,'RF calibration check (goodRF-T) vs RF1',200,0.,100.,100,-1.0,1.0,0.)
          call hbook2(1004,'RF calibration check (goodRF-T) vs RF2',320,40.,200.,100,-1.0,1.0,0.)
          call hbook1(1005,'RF calibration check (goodRF-T)',100,-1.0,1.0,0.)
          call hbook2(1006,'Ttime - Etime (ns) vs E_id',767,.5,767.5,100,-20.,20.,0.)  


          call hbook1(1007,'TTag -TPho',100,-1.0,1.0,0.)
          call hbook2(1008,'Ttime - RFtime (ns) vs T id'   ,121,.5,121.5,100,-1.002,1.002,0.)
          call hbook2(1009,'Ttime - RFtime (ns) vs T counter',62,.5,62.5,100,-1.002,1.002,0.)


c -------- E and  T pattern histo 
          call hbook1(2001,'Eid pattern',767,.5,767.5,0.)
          call hbook1(2002,'Tid pattern (all hits)',121,.5,121.5,0.)
          call hbook1(2003,'Tid pattern (good hits)',121,.5,121.5,0.)
          call hbook1(2004,'T counter pattern (all hits)',61,.5,61.5,0.)
          call hbook1(2005,'T counter pattern (good hits)',61,.5,61.5,0.)


c -------- ST plots
          call hbook1(4001,'Number of Hits from ST1 bank', 24,0.5,24.5,0.)
          call hbook1(4011,'ST1 Paddle', 24,0.5,24.5,0.)
          call hbook1(4012,'ST1 Time', 200,-25., 75., 0.)
          call hbook1(4013,'ST1 ADC' , 160,0.,1600., 0.)
          call hbook2(4014,'ST1 Time vs. Paddle',24,0.5,24.5,200,-25., 75.,  0.)
          call hbook2(4015,'ST1 ADC vs. Paddle' ,24,0.5,24.5,160,0., 1600., 0.)

          call hbook1(4020,'Number of hits from STR bank',24,  0.5, 24.5, 0.)
          call hbook1(4021,'STR Paddle (leg,nose)', 48,  0.5, 24.5, 0.)
          call hbook1(4022,'STR Time'            , 200,-25., 75., 0.)
          call hbook1(4023,'STR flight path'     , 200,  0., 75., 0.)
          call hbook1(4024,'STR hit distance from PMT (position)', 200,  0., 75., 0.)
          call hbook1(4025,'STR Status'          ,  10,  0., 10., 0.)
          call hbook2(4026,'STR Time vs. Paddle' ,  24,  0.5, 24.5,200,-25., 75., 0.)
          call hbook2(4027,'STR position vs. Paddle' ,  24,  0.5, 24.5,200,  0., 75., 0.)
          call hbook2(4028,'STR Time vs. position', 100,  0., 75.,100,-20., 40., 0.)
          do i=1,24
             call hbook2(4100+i,'STR Time vs. position, '//pi(i), 100,  0., 75.,100,-20., 40., 0.)
          enddo


c -------- ST - Tagger plots
          call hbook1(4501,'Tagger time - ST time, all Tagger hits',128,-16.,16.,0.)
          call hbook2(4502,'ST time vs.Tagger time'                ,100,-25.,75.,100,-25.,75.,0.)
          call hbook2(4503,'Tagger time - ST time vs Tid'          ,121,.5,121.5,128,-16.,16.,0.)
          call hbook2(4504,'Tagger time - ST time vs ST Pd'        , 24,.5, 24.5,100,-6.0,6.0,0.)
          call hbook2(4505,'ST time vs Tid'                        ,121,.5,121.5, 50,-25.,25.,0.)
          call hbook2(4506,'vtime - ST.vtime vs ST Pd  for pions'      , 24,.5, 24.5,200,-6.0,6.0,0.)
          call hbook2(4507,'vtime - ST.vtime vs ST Pd (leg)  for pions', 24,.5, 24.5,200,-6.0,6.0,0.)
          call hbook2(4508,'vtime - ST.vtime vs ST Pd (nose)  for pions', 24,.5, 24.5,200,-6.0,6.0,0.)

          call hbook1(4511,'Tagger time - ST time,good photon'  ,128,-16.,16.,0.)
          call hbook2(4512,'ST time vs.Tagger time, good photon',100,-25.,75.,100,-25.,75.,0.)



       endif
       
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
c -------------------- DSD stuff                             ----------
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       if (do_dsd_nt) call hbookn(3,'DSD',12,'//LUN1/TAG',50000,namesdsd)

      RETURN
      END
c
c------------------------------------------------------------------------------















