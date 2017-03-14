      SUBROUTINE BOOK_PART_HIST
c
c_begin_doc
c  RCS ID string
c  $Id: book_part_hist.F,v 1.17 2008/04/03 18:07:22 fklein Exp $
c
c  Documentation for subroutine BOOK_PART_HIST
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
c  Author:         Created:  Wed Jun 11 00:13:56 EDT 1997
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
      PARAMETER (CFILE = '$RCSfile: book_part_hist.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.17 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/04/03 18:07:22 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: book_part_hist.F,v 1.17 2008/04/03 18:07:22 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_PART_HIST')
      PARAMETER (CRAUTH='whoistheauthor')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,i
      CHARACTER*3 si(0:6)
      data si/' ','S1','S2','S3','S4','S5','S6'/

      CHARACTER*5 paddle(0:24)
      data paddle/' ','PD1','PD2','PD3','PD4','PD5','PD6','PD7',
     &     'PD8','PD9','PD10','PD11','PD12','PD13','PD14','PD15','PD16',
     &     'PD17','PD18','PD19','PD20','PD21','PD22','PD23','PD24'/

c_end_var
c
c  executable code for routine BOOK_PART_HIST:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      write(6,*)'Booking seb histos'
c
      call hbook1(6100,'EVNT mass',200,0.,4.,0.0)
      call hbook1(6110,'SC time meas.-calc. for proton (p"G#0.8 GeV)',200,-5.,5.,0.0)
      call hbook1(6120,'SC time meas.-calc. for [p]^+!',200,-5.,5.,0.0)
      call hbook1(6130,'SC time meas.-calc. for [p]^-!',200,-5.,5.,0.0)
      call hbook1(6140,'SC time meas.-calc. for [p]^+! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook1(6150,'SC time meas.-calc. for [p]^-! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook2(6310,'dE/dX in SC vs p  for h^+!',50,0.,2.,50,0.,200.,0.0)
      call hbook1(7110,'ST time meas.-calc. for proton (p"G#0.8 GeV)',200,-5.,5.,0.0)
      call hbook1(7120,'ST time meas.-calc. for [p]^+!',200,-5.,5.,0.0)
      call hbook1(7130,'ST time meas.-calc. for [p]^-!',200,-5.,5.,0.0)
      call hbook1(7140,'ST time meas.-calc. for [p]^+! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook1(7150,'ST time meas.-calc. for [p]^-! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook1(7210,'EC time meas.-calc. for proton (p"G#0.8 GeV)',200,-5.,5.,0.0)
      call hbook1(7220,'EC time meas.-calc. for [p]^+!',200,-5.,5.,0.0)
      call hbook1(7230,'EC time meas.-calc. for [p]^-!',200,-5.,5.,0.0)
      call hbook1(7240,'EC time meas.-calc. for [p]^+! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook1(7250,'EC time meas.-calc. for [p]^-! (cut on dedx and mom.)',200,-5.,5.,0.0)
      call hbook2(7111,'ST time meas.-calc.(prot) vs PD no. (p"G#0.8 GeV)',24,0.5,24.5,200,-6.,6.,0.0)
      call hbook2(7112,'ST time meas.-calc.(prot) vs Hit pos. (p"G#0.8 GeV)',100,0.,70.,200,-6.,6.,0.0)
      call hbook2(7121,'ST time meas.-calc.([p]^+!) vs PD no.',24,0.5,24.5,200,-6.,6.,0.0)
      call hbook2(7122,'ST time meas.-calc.([p]^+!) vs Hit pos.',100,0.,70.,200,-6.,6.,0.0)
      call hbook2(7131,'ST time meas.-calc.([p]^-!) vs PD no.'   ,24,0.5,24.5,200,-6.,6.,0.0)
      call hbook2(7132,'ST time meas.-calc.([p]^-!) vs Hit pos.',100,0.,70.,200,-6.,6.,0.0)
      call hbook2(7141,'ST time meas.-calc.([p]^+!) vs PD no. (cut on dedx and mom.)'   ,24,0.5,24.5,200,-6.,6.,0.0)
      call hbook2(7142,'ST time meas.-calc.([p]^+!) vs Hit pos. (cut on dedx and mom.)',100,0.,70.,200,-6.,6.,0.0)
      call hbook2(7151,'ST time meas.-calc.([p]^-!) vs PD no. (cut on dedx and mom.)'   ,24,0.5,24.5,200,-6.,6.,0.0)
      call hbook2(7152,'ST time meas.-calc.([p]^-!) vs Hit pos. (cut on dedx and mom.)',100,0.,70.,200,-6.,6.,0.0)
      do i=1,24
         call hbook1(711100+i,paddle(i)//'dt_st prot',200,-6.,6.,0.0)
         call hbook1(714100+i,paddle(i)//'dt_st [p]^+!',200,-6.,6.,0.0)
         call hbook1(715100+i,paddle(i)//'dt_st [p]^-!',200,-6.,6.,0.0)          
      enddo
      do i=1,6
         call hbook2(i+6100,si(i)//'Mass^2! vs SC paddle',57,0.5,57.5,70,-0.1,1.3,0.0)
         call hbook2(i+6110,si(i)//'SC time meas.-calc.(prot) vs PD prot (p"G#0.8 GeV)',57,0.5,57.5,100,-5.,5.,0.0)
         call hbook2(i+6120,si(i)//'SC time meas.-calc.([p]^+!) vs PD [p]^+!',57,0.5,57.5,100,-5.,5.,0.0)
         call hbook2(i+6130,si(i)//'SC time meas.-calc.([p]^-!) vs PD [p]^-!',57,0.5,57.5,100,-5.,5.,0.0)
         call hbook2(i+6140,si(i)//'SC time meas.-calc.([p]^+!) vs PD (cut on dedx and mom)',57,0.5,57.5,100,-5.,5.,0.0)
         call hbook2(i+6150,si(i)//'SC time meas.-calc.([p]^-!) vs PD (cut on dedx and mom)',57,0.5,57.5,100,-5.,5.,0.0)
         call hbook2(i+6200,si(i)//'[q] vs p, h^+!',50,0.,3.,60,5.,125.,0.0)
         call hbook2(i+6210,si(i)//'[q] vs p, h^-!',50,0.,3.,60,5.,125.,0.0)
         call hbook1(i+6220,si(i)//'SC?PD!, h^+!',60,0.5,60.5,0.0)
         call hbook1(i+6230,si(i)//'SC?PD!, h^-!',60,0.5,60.5,0.0)
         call hbook2(i+6310,si(i)//'dE/dX vs p, h^+!',50,0.,2.,50,0.,200.,0.0)
       enddo
       call hbook2(6041,'H^+! Z vertex',50,-60.,40.,90,-30.,330.,0.)
       call hbook2(6042,'H^-! Z vertex',50,-60.,40.,90,-30.,330.,0.)
       call hbook2(6043,'neutrals Z vertex',50,-60.,40.,90,-30.,330.,0.)
c
       call hbook1(6051,'MVRT X vertex',100,-2.,2.,0.)
       call hbook1(6052,'MVRT Y vertex',100,-2.,2.,0.)
       call hbook2(6053,'MVRT X vs. Y vertex',100,-2.,2.,100,-2.,2.,0.0)
c
       call hbook1(3101,'Corrected RF time',2000,0.,1000.,0.)
       call hbook1(3102,'RF offset (for best matching photon)',150,-1.5,1.5,0.)
       call hbook2(3103,'RF offsets (for best photon) vs RF',2000,0.,1000.,100,-1.,1.,0.)
       call hbook2(3104,'RF offsets (for best photon) vs VZ (all TB tracks)',45,-60.0,30.0,100,-1.,1.,0.)
       call hbook2(3105,'Ttag-Tpho vs Tid (for best photon)',121, 0.5,121.5,100,-1.,1.,0.)
c 3106 filled in seb/get_gt_rf
       call hbook2(3106,'bad RF time vs corr. RF time',100, 0.,1000., 100, 0.,1000.,0.)
       call hbook1(3112,'Start_time-Tag_time (for best photon)',300,-3.,3.,0.)
       call hbook2(3113,'Start_time-Tag_time (for best photon) vs RF',2000,0.,1000.,150,-3.,3.,0.)
       call hbook2(3114,'Start_time-Tag_time (for best photon) vs VZ (all TB tracks)',45,-60.0,30.0,150,-3.,3.,0.)
       call hbook2(3115,'Start_time-Tag_time (for best photon) vs Tbin',121,0.5,121.5,150,-3.,3.,0.)
c
cfk new histogram ids!
       call hbook1(3197,'E?[g]! 1st photon',100,0.8,4.,0.)
       call hbook1(3198,'E?[g]! 2nd photon',100,0.8,4.,0.)
       call hbook1(3199,'E?[g]! 3rd photon',100,0.8,4.,0.)

       call hbook1(6600,'ECt-SCt, pions',200,-5.,5.,0.) 
       call hbook1(6610,'ECt-SCt, protons',200,-5.,5.,0.) 
       call hbook1(6620,'Beta for neutrals',150,0.2,1.5,0.)
       call hbook1(6640,'IM([g][g])'       ,120,-0.2,0.4,0.)
       call hbook1(6650,'Beta for [g] (at least 2[g] in event)' ,70,0.8,1.5,0.)
c       call hbook1(6660,'p?miss!/p?EC! for [p]^+![p]^+![p]^-!n',150,0.2,1.8,0.)
       do i=1,6
          call hbook1(i+6600,si(i)//'ECt-SCt, pions',200,-5.,5.,0.)                
          call hbook1(i+6610,si(i)//'ECt-SCt, protons',200,-5.,5.,0.)                
          call hbook1(i+6620,si(i)//'Beta for neutrals',150,0.2,1.5,0.)
          call hbook2(i+6630,si(i)//'E?EC! vs Beta for neutrals',75,0.,1.5,50,0.2,1.5,0.)
          call hbook1(i+6640,si(i)//'IM([g][g])'  ,120,-0.2,0.4,0.)
          call hbook1(i+6650,si(i)//'Beta for [g]  (at least 2[g] in event)',70,0.8,1.5,0.)
c          call hbook1(i+6660,si(i)//'p?miss!/p?EC! for [p]^+![p]^+![p]^-!n',150,0.2,1.8,0.)
       enddo

c Missing and Invariant Masses
       call hbook1(10001,'MM2(p[p]^+!)',120,-0.1, 0.6,0.)
       call hbook1(10002,'MM(p[p]^+!)' ,100, 0.0, 1.05,0.)
       call hbook1(10003,'MM(p[p]^+![p]^-!)' ,120, -0.1, 0.6,0.)
c       call hbook1(10004,'MM(p) for p[p]^+![p]^-![p]^0!' ,100, 0., 1.2,0.)
c       call hbook1(10005,'IM(p[p]^-!)' ,100, 0.0, 1.13,0.)
       call hbook1(10006,'MM(p) for pX' ,100, 0., 1.5,0.)
       call hbook1(10007,'MM([p]^+!)' ,160, 0.7, 2.3,0.)

       call hbook1(10011,'MM(K^+!)'  ,200, 0.8, 1.8,0.)
       call hbook1(10012,'MM(K^+!) sb'  ,200, 0.8, 1.8,0.)
       call hbook1(10013,'MM(pK^+!)' ,100, 0.3, 0.7,0.)
       call hbook1(10014,'MM(p) for pK^+!K^-!' ,100, 0.9, 1.4,0.)

       call hbook1(10021,'IM([p]^+![p]^!) for K^+![p]^+![p]^-!' ,100, 0.3, 0.7,0.)
       call hbook1(10022,'MM(K^+![p]^+![p]^!)' ,100, 0.7, 1.2,0.)
       call hbook1(10023,'MM(K^+!) for K^+![p]^+![p]^-!'  ,100, 1.4, 2.0,0.)
       call hbook1(10024,'MM(K^+![p]^-!) for K^+![p]^+![p]^-!' ,100, 0.9, 1.3,0.)       
       call hbook1(10025,'MM(K^+![p]^+!) for K^+![p]^+![p]^-!' ,100, 0.9, 1.3,0.)

       call hbook1(10031,'MM(K^+!K^-![p]^+!) for K^+!K^-![p]^+!' ,100, 0.7, 1.2,0.)       
       call hbook1(10032,'IM(K^-![p]^+!) for K^+!K^-![p]^+!' ,100, 0.5, 1.3,0.)      
c
       call hbook1(8970,'number of hits in EVNT (gpart)',21,-0.5,20.5,0.)
       call hbook1(8971,'number of hits in SCPB',21,-0.5,20.5,0.)
       call hbook1(8972,'number of hits in ECPB',21,-0.5,20.5,0.)
       call hbook1(8973,'number of hits in STPB',21,-0.5,20.5,0.)
       call hbook1(8974,'ngood taghits (hits in TGPB)',21,-0.5,20.5,0.)

c-----------------------------------------------------------------------------------
c  these are filled in seb/gtrig_time.F:
c       call hbook1(8981,'NOST Vertex time (1st particle) (HBT Level)',150,-50.,50.,0.)
c       call hbook1(8982,'NOST Vertex time (2nd particle) (HBT Level)',150,-50.,50.,0.)
c       call hbook1(8983,'2 htrk timediff (HBT Level)',200,-20.,20.,0.)
c       call hbook1(8984,'T_photon - Vertex Time (HBT Level)',300,-100.,200.,0.)
c       call hbook1(8985,'Vertex Time from 2 trk - ST time (HBT Level)',100,-15.,15.,0.)
c       call hbook1(8986,'T_photon - ST time (HBT Level)',300,-100.,200.,0.)
c       call hbook2(8987,'T_photon - ST time vs. T_photon - Vertex Time (HBT Level)',150,-50.,50.,150,-50.,50.,0.)
c       call hbook2(8988,'ST Photon ID vs. Vrt Photon ID (HBT Level)',20,0.,20.,20,0.,20.,0.)
c
       call hbook1(8991,'NOST Vertex time (1st particle) (TBT Level)',150,-50.,50.,0.)
       call hbook1(8992,'NOST Vertex time (2nd particle) (TBT Level)',150,-50.,50.,0.)
       call hbook1(8993,'2 ttrk timediff (TBT Level)',200,-20.,20.,0.)
       call hbook1(8994,'T_photon - Vertex Time (TBT Level)',300,-100.,200.,0.)
       call hbook1(8995,'Vertex Time from 2 trk - ST time (TBT Level)',100,-15.,15.,0.)
c       call hbook1(8996,'T_photon - ST time (TBT Level)',300,-100.,200.,0.)
c       call hbook2(8997,'T_photon - ST time vs. T_photon - Vertex Time (TBT Level)',150,-50.,50.,150,-50.,50.,0.)
       call hbook2(8998,'ST Photon ID vs. Vrt Photon ID (TBT Level)',20,0.,20.,20,0.,20.,0.)

c       call hbook1(9981,'T_photon (all photons) - Vertex (HBT Level)',300,-100.,200.,0.)
c       call hbook1(9982,'dt (HBT Level)',200,-15.,15.,0.)
c       call hbook1(9983,'T_photon (all good photons)',200,-50.,50.,0.)
c       call hbook1(9984,'Vertex Time',100,-50.,50.,0.)

       call hbook1(9991,'T_photon (all photons) - Vertex (TBT Level)',300,-100.,200.,0.)
       call hbook1(9992,'dt (TBT Level)',200,-5.,35.,0.)
       call hbook1(9993,'T_photon (all good photons)',200,-50.,50.,0.)
       call hbook1(9994,'Vertex Time',100,-50.,50.,0.)
c--------------------------------------------------------------------------------
       
c
c
      RETURN
      END
c
c------------------------------------------------------------------------------




