      SUBROUTINE BUILDER_BRUN(irun)
c     
c     _begin_doc
c     RCS ID string
c     $Id: builder_brun.F,v 1.40 2005/09/01 15:41:22 stepanya Exp $
c     
c     Documentation for subroutine BUILDER_BRUN
c     
c     Purpose: Begin run routin for SEB. Reads geometry and control variables. 
c     --------
c     
c     Input Parameters:  Non
c     ----------------
c     
c     Output Parameters:  Non
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   S.Stepanyan      Created:  Thu Apr 10 23:32:47 EDT 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "wcs.inc"
c     #include "clasmdl.inc"
#include "evnt_general.inc"
#include "piddef.inc"
#include "echits.inc"
#include "schits.inc"
#include "ecgmt.inc"
#include "scgeom.inc"
#include "photon_stuff.inc"
#include "dtcl_var.inc"
#include "evnt_par.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: builder_brun.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.40 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2005/09/01 15:41:22 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1     '$Id: builder_brun.F,v 1.40 2005/09/01 15:41:22 stepanya Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BUILDER_BRUN')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Input variables:
c     ----------------
      integer irun
c     
c     Local User defined variables:
c     -----------------------------
      CHARACTER*99  call_map
      REAL value(10),energ
      INTEGER flag, RF_J, i
      INTEGER NWRITE
      INTEGER MBANK, MLINK
      EXTERNAL MBANK
      INTEGER ind,indx,sector,grp,btype
c     _end_var
c     
c     executable code for routine BUILDER_BRUN:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Begin run for event builder'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      DO sector=1,6
         IND=MLINK(jw,'ECP ',sector)
         IF(IND.NE.0) THEN
            n2sect(1,Sector) = rjw(IND+1)
            n2sect(2,Sector) = rjw(IND+2)
            n2sect(3,Sector) = rjw(IND+3)
            ecbsc_r 	   = rjw(IND+4)
            ecbsc_d(whole)   = rjw(IND+5)
            ecbsc_d(inner)   = rjw(IND+6)
            ecbsc_d(outer)   = rjw(IND+7)
         ENDIF
      ENDDO
      Ec_tDepth(inner)=0.
      Ec_tDepth(whole)=0.
      Ec_tDepth(outer)=5.
c     
      DO sector=1,6
         IND=MLINK(jw,'SCP ',sector)
         IF(IND.NE.0) THEN
            do grp = 1,4
               sc_rnormal(grp,sector) = rjw(ind+4)
               ind = ind+4
            enddo
         ENDIF
      ENDDO 
      call revinm('CLAS_PARMS','Maps/CALL_CALIB.map',call_map)
      call map_get_float(call_map,'T1', 'value',10 , value, irun, flag)
      rf1slop=value(6)
      rf2slop=value(7)
c     
c     this should be controlled outside of seb, this can all 
c     come from reading the CL01 bank from the event loop
      call revinm('CLAS_PARMS','Maps/RF_OFFSETS.map',call_map)
      call map_get_float(call_map,'offset','value',1,or_diff, irun, flag)
      call map_get_float(call_map,'offset','delta',1,rf_delta, irun, flag)
      call map_get_float(call_map,'rf_diff','rf1_gt_rf2',1,RF_1G2, irun, flag)
      call map_get_float(call_map,'rf_diff','rf1_lt_rf2',1,RF_1L2, irun, flag)
c     call map_get_float(call_map,'rf_range','high',1,RF_high1, irun, flag)
c     call map_get_float(call_map,'rf_range','low',1,RF_low1, irun, flag)
c     call map_get_float(call_map,'rf_range','high2',1,RF_high2, irun, flag)
c     call map_get_float(call_map,'rf_range','low2',1,RF_low2, irun, flag)
      call map_get_float(call_map,'rf_range','rf1_high1',1,RF1_high1, irun, flag)
      call map_get_float(call_map,'rf_range','rf1_low1',1,RF1_low1, irun, flag)
      call map_get_float(call_map,'rf_range','rf1_high2',1,RF1_high2, irun, flag)
      call map_get_float(call_map,'rf_range','rf1_low2',1,RF1_low2, irun, flag)
      call map_get_float(call_map,'rf_range','rf2_high1',1,RF2_high1, irun, flag)
      call map_get_float(call_map,'rf_range','rf2_low1',1,RF2_low1, irun, flag)
      call map_get_float(call_map,'rf_range','rf2_high2',1,RF2_high2, irun, flag)
      call map_get_float(call_map,'rf_range','rf2_low2',1,RF2_low2, irun, flag)
      call map_get_float(call_map,'dead_range','rf1_dead_low',1,RF1dl, irun, flag)
      call map_get_float(call_map,'dead_range','rf1_dead_high',1,RF1dh, irun, flag)
      call map_get_float(call_map,'dead_range','rf2_dead_low',1,RF2dl, irun, flag)
      call map_get_float(call_map,'dead_range','rf2_dead_high',1,RF2dh, irun, flag)
c     call revinm('CLAS_PARMS','Maps/RF_COR_F.map',call_map)
      call map_get_float(call_map,'F1','high',1,RF_high1, irun, flag)
      call map_get_float(call_map,'F1','low',1,RF_low1, irun, flag)
      call map_get_float(call_map,'F2','high',1,RF_high2, irun, flag)
      call map_get_float(call_map,'F2','low',1,RF_low2, irun, flag)
      call map_get_float(call_map,'F1','p0',1,f10, irun, flag)
      call map_get_float(call_map,'F1','p1',1,f11, irun, flag)
      call map_get_float(call_map,'F1','p2',1,f12, irun, flag)
      call map_get_float(call_map,'F2','p0',1,f20, irun, flag)
      call map_get_float(call_map,'F2','p1',1,f21, irun, flag)
      call map_get_float(call_map,'F2','p2',1,f22, irun, flag)
C///////////////////////////////////////////////////////////////////////////
C---- Getting Parameters For function 1 of RF corrections -----------------
C     
      CALL map_get_float(call_map,'F1','low',1,RFG_low(1),irun,flag)
      CALL map_get_float(call_map,'F1','high',1,RFG_high(1),irun,flag)
      call map_get_float(call_map,'F1','p0',1,RFG_PARMS(1,1), irun, flag)
      call map_get_float(call_map,'F1','p1',1,RFG_PARMS(1,2), irun, flag)
      call map_get_float(call_map,'F1','p2',1,RFG_PARMS(1,3), irun, flag)
      call map_get_float(call_map,'F1','p3',1,RFG_PARMS(1,4), irun, flag)
C///////////////////////////////////////////////////////////////////////////
C---- Getting Parameters For function 2 of RF corrections -----------------
C     
      CALL map_get_float(call_map,'F2','low',1,RFG_low(2),irun,flag)
      CALL map_get_float(call_map,'F2','high',1,RFG_high(2),irun,flag)
      call map_get_float(call_map,'F2','p0',1,RFG_PARMS(2,1), irun, flag)
      call map_get_float(call_map,'F2','p1',1,RFG_PARMS(2,2), irun, flag)
      call map_get_float(call_map,'F2','p2',1,RFG_PARMS(2,3), irun, flag)
      call map_get_float(call_map,'F2','p3',1,RFG_PARMS(2,4), irun, flag)

C///////////////////////////////////////////////////////////////////////////
C---- Getting Parameters For function 3 of RF corrections -----------------
C     
      CALL map_get_float(call_map,'F3','low',1,RFG_low(3),irun,flag)
      CALL map_get_float(call_map,'F3','high',1,RFG_high(3),irun,flag)
      call map_get_float(call_map,'F3','p0',1,RFG_PARMS(3,1), irun, flag)
      call map_get_float(call_map,'F3','p1',1,RFG_PARMS(3,2), irun, flag)
      call map_get_float(call_map,'F3','p2',1,RFG_PARMS(3,3), irun, flag)
      call map_get_float(call_map,'F3','p3',1,RFG_PARMS(3,4), irun, flag)

C///////////////////////////////////////////////////////////////////////////
C---- Getting Parameters For function 4 of RF corrections -----------------
C     
      CALL map_get_float(call_map,'F4','low' ,1,RFG_low(4)  ,irun,flag)
      CALL map_get_float(call_map,'F4','high',1,RFG_high(4) ,irun,flag)
      call map_get_float(call_map,'F4','p0'  ,1,RFG_PARMS(4,1) ,irun, flag)
      call map_get_float(call_map,'F4','p1'  ,1,RFG_PARMS(4,2) ,irun, flag)
      call map_get_float(call_map,'F4','p2'  ,1,RFG_PARMS(4,3) ,irun, flag)
      call map_get_float(call_map,'F4','p3'  ,1,RFG_PARMS(4,4) ,irun, flag)
C////////////////////////////////////////////////////////////////////////////
C---- 6-- Checking Printout -------------------------------------------------
      CALL map_get_int(call_map,'status', 'value', 1, RF_status, irun,flag)
c     New RF pulse correction, using 100 numbers, means of a Gaussian fits to 
c     the RF offset distributions in each 1ns bin of the RF time
      if(iRFpcorr.eq.1)then
         CALL map_get_float(call_map,'correction', 'value', 100, RFpcorr,irun,flag)
      endif
c     get beam position
      call revinm('CLAS_PARMS','Maps/GEOMETRY.map',call_map)
      CALL map_get_float(call_map,'beam','position',3,beam_position,irun,flag)
      CALL map_get_float(call_map,'target','position',3,target_position,irun,flag)
      beam_position(3)=target_position(3)
      if(dTargetPos(3).ne.-9999.)beam_position(3)=sngl(dTargetPos(3))
      write(6,*)'Beam position for Run ',irun, ' is ',beam_position,'  From run ',flag
      write(6,*)'TRG position ',sngl(dTargetPos(3)),beam_position(3)
      bt_Z_center = beam_position(3)
c     All DB parameters are initialized.
      if(RF_STATUS.EQ.1.OR.RF_STATUS.EQ.2)THEN
         WRITE(*,*)'SEB_BUILDER: Using RF',RF_STATUS,' (old TDCs)'
         DO RF_J = 1,4
            Write(*,*)'SEB_RF_Func ==> [',RFG_low(RF_J),':',RFG_high(RF_J),']. PARMS :',
     &           RFG_PARMS(RF_J,1),RFG_PARMS(RF_J,2),RFG_PARMS(RF_J,3), RFG_PARMS(RF_J,4)
         ENDDO
      elseif(RF_STATUS.EQ.3.OR.RF_STATUS.EQ.4)THEN
         WRITE(*,*)'SEB_BUILDER: Using RF',RF_STATUS,' (new TDC)'
      ELSE
         WRITE(*,*)'ERROR: RF_STATUS IS NOT DEFINED..., No RF corrections will be done'
         RF_STATUS = 0
      ENDIF

c     get beam type from the RUNC bank (0:electron, 1:photon)  
      YES_PHOTON = .FALSE.
c     located in c_bos_io makeRUNC.c	    
      call get_beam_type(btype)
      if(btype.eq.1.or.btype.eq.3.or.btype.eq.4) YES_PHOTON = .TRUE.
      write(6,*)'Beam type is ',btype,YES_PHOTON
c           
      call vertex_brun(irun)
      RETURN
      END
c
c------------------------------------------------------------------------------




