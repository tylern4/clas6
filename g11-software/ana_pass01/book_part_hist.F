      SUBROUTINE BOOK_PART_HIST
c
c_begin_doc
c  RCS ID string
c  $Id: book_part_hist.F,v 1.11 2004/07/23 19:08:03 lguo Exp $
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
      PARAMETER (CREVIS = '$Revision: 1.11 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/07/23 19:08:03 $')
      PARAMETER (CAUTHO = '$Author: lguo $')
      DATA CRCSID/   
     1'$Id: book_part_hist.F,v 1.11 2004/07/23 19:08:03 lguo Exp $'   
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
       call hbook1(6100,'TOF mass',200,0.,4.,0.0)
       call hbook1(6110,'dt prot'  ,200,-5.,5.,0.0)
       call hbook1(6120,'dt [p]^+!',200,-5.,5.,0.0)
       call hbook1(6130,'dt [p]^-!',200,-5.,5.,0.0)
       call hbook1(6140,'dt [p]^+!',200,-5.,5.,0.0)
       call hbook1(6150,'dt [p]^-!',200,-5.,5.,0.0)
       call hbook2(6310,'dE/dX vs p, h^+!',50,0.,2.,50,0.,200.,0.0)
       call hbook1(7120,'dt_st [p]^+!',200,-5.,5.,0.0)
       call hbook1(7130,'dt_st [p]^-!',200,-5.,5.,0.0)
       call hbook1(7140,'dt_st [p]^+!',200,-5.,5.,0.0)
       call hbook1(7150,'dt_st [p]^-!',200,-5.,5.,0.0)
       call hbook2(7121,'dt_st [p]^+!',24,0.,24.,200,-5.,5.,0.0)
       call hbook2(7131,'dt_st [p]^-!',24,0.,24.,200,-5.,5.,0.0)
       call hbook2(7141,'dt_st [p]^+!',24,0.,24.,200,-5.,5.,0.0)
       call hbook2(7151,'dt_st [p]^-!',24,0.,24.,200,-5.,5.,0.0)
       do i=6101,6106
         call hbook2(i,si(i-6100)//'M vs PD',50,0.,50.,70,-0.1,1.3,0.0)
         call hbook2(i+10,si(i-6100)//'dt vs PD prot',50,0.,50.,100,-5.,5.,0.0)
         call hbook2(i+20,si(i-6100)//'dt vs PD [p]^+!',50,0.,50.,100,-5.,5.,0.0)
         call hbook2(i+30,si(i-6100)//'dt vs PD [p]^-!',50,0.,50.,100,-5.,5.,0.0)
         call hbook2(i+40,si(i-6100)//'dt vs PD [p]^+!',50,0.,50.,100,-5.,5.,0.0)
         call hbook2(i+50,si(i-6100)//'dt vs PD [p]^-!',50,0.,50.,100,-5.,5.,0.0)
         call hbook2(i+100,si(i-6100)//'[q] vs p, h^+!',50,0.,3.,60,5.,125.,0.0)
         call hbook2(i+110,si(i-6100)//'[q] vs p, h^-!',50,0.,3.,60,5.,125.,0.0)
         call hbook1(i+120,si(i-6100)//'SC?PD!, h^+!',50,0.,50.,0.0)
         call hbook1(i+130,si(i-6100)//'SC?PD!, h^-!',50,0.,50.,0.0)
         call hbook2(i+210,si(i-6100)//'dE/dX vs p, h^+!',50,0.,2.,50,0.,200.,0.0)
       enddo
       call hbook2(6041,'H^+! Z vertex',50,-40.,20.,90,-30.,330.,0.)
       call hbook2(6042,'H^-! Z vertex',50,-40.,20.,90,-30.,330.,0.)
c
       call hbook1(6051,'MVRT X vertex',100,-2.,2.,0.)
       call hbook1(6052,'MVRT Y vertex',100,-2.,2.,0.)
       call hbook2(6053,'MVRT X vs. Y vertex',100,-2.,2.,100,-2.,2.,0.0)
c
       call hbook1(3101,'Corrected RF time',200,0.,100.,0.)
       call hbook1(3102,'RF offsets',150,-1.5,1.5,0.)
       call hbook2(3103,'RF offsets vs RF',100,0.,100.,100,-1.,1.,0.)
       call hbook2(3104,'RF offsets vs Z?v!',50,-40.,20.,100,-1.,1.,0.)
       call hbook2(3105,'Ttag-Tpho vs Tid',121, 0.5,121.5,100,-1.,1.,0.)
       call hbook1(3112,'Start_time-Tag_time',150,-1.5,1.5,0.)
       call hbook2(3113,'Start_time-Tag_tim vs RF',100,0.,100.,100,-1.,1.,0.)
       call hbook2(3114,'Start_time-Tag_tim vs Z?v!',50,-40.,20.,100,-1.,1.,0.)
c
       call hbook1(6600,'Beta for neutrals',150,0.,1.,0.)
       do i=6601,6606
         call hbook2(i,si(i-6600)//'E?EC! vs Beta for neutrals',75,0.,1.5,50,0.,1.,0.)
       enddo

c Missing and Invariant Masses
       call hbook1(10001,'MM2(p[p]^+!)',100,-0.2, 0.2,0.)
       call hbook1(10002,'MM(p[p]^+!)' ,100, 0.0, 0.3,0.)
       call hbook1(10003,'MM([p]^+![p]^!)' ,100, 0.7, 1.2,0.)
       call hbook1(10004,'MM(p) for p[p]^+![p]^-![p]^0!' ,100, 0., 1.2,0.)

       call hbook1(10011,'MM(K^+!)'  ,100, 0.8, 1.8,0.)
       call hbook1(10012,'MM(K^+!) sb'  ,100, 0.8, 1.8,0.)
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
c SEB gtrig
       call hbook1(8970,'gpart',20,0.,20.,0.)
       call hbook1(8971,'sc',20,0.,20.,0.)
       call hbook1(8972,'ec',20,0.,20.,0.)
       call hbook1(8973,'st',20,0.,20.,0.)
       call hbook1(8974,'ngood taghits',20,0.,20.,0.)
       call hbook1(8975,'no st hits',20,0.,20.,0.)
c
       call hbook1(8981,'NOST Vertex time (1st particle) (HBT Level)',150,-50.,50.,0.)
       call hbook1(8982,'NOST Vertex time (2nd particle) (HBT Level)',150,-50.,50.,0.)
       call hbook1(8983,'2 htrk timediff (HBT Level)',200,-20.,20.,0.)
       call hbook1(8984,'T_photon - Vertex Time (HBT Level)',300,-100.,200.,0.)
       call hbook1(8985,'Vertex Time from 2 trk - ST time (HBT Level)',150,-50.,50.,0.)
c
       call hbook1(8991,'NOST Vertex time (1st particle) (TBT Level)',150,-50.,50.,0.)
       call hbook1(8992,'NOST Vertex time (2nd particle) (TBT Level)',150,-50.,50.,0.)
       call hbook1(8993,'2 ttrk timediff (TBT Level)',200,-20.,20.,0.)
       call hbook1(8994,'T_photon - Vertex Time (TBT Level)',300,-100.,200.,0.)
       call hbook1(8995,'Vertex Time from 2 trk - ST time (TBT Level)',150,-50.,50.,0.)

       call hbook1(9981,'T_photon (all photons) - Vertex (HBT Level)',300,-100.,200.,0.)
       call hbook1(9982,'dt (HBT Level)',200,-15.,15.,0.)
       call hbook1(9983,'T_photon (all good photons)',200,-50.,50.,0.)
       call hbook1(9984,'Vertex Time',100,-50.,50.,0.)

       call hbook1(9991,'T_photon (all photons) - Vertex (TBT Level)',300,-100.,200.,0.)
       call hbook1(9992,'dt (TBT Level)',200,-15.,15.,0.)
       call hbook1(9993,'T_photon (all good photons)',200,-50.,50.,0.)
       call hbook1(9994,'Vertex Time',100,-50.,50.,0.)
       
       call hbook1(9997,'E?[g]! 1st photon',100,0.8,4.,0.)
       call hbook1(9998,'E?[g]! 2nd photon',100,0.8,4.,0.)
       call hbook1(9999,'E?[g]! 3rd photon',100,0.8,4.,0.)


c
c
      RETURN
      END
c
c------------------------------------------------------------------------------




