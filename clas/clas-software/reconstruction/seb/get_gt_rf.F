      real function get_gt_rf()

      implicit none
c     
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "piddef.inc"
c     
      INTEGER NWRITE
      LOGICAL rf1dead,rf2dead
      INTEGER RF_J,RF_REGION, RF_N_REGIONS
      REAL gt_rf_cl01,gt_rf_seb,delta,fdelta
c     
c     - Get results from CL01. 
      gt_rf_cl01=1000000.
c      print *,rftest(1),rftest(2),rftest(3)
      IF(rftest(1).gt.10.OR.rftest(2).gt.10.OR.RF_STATUS.gt.2)THEN
         gt_rf_cl01=rftest(3)
         call hcdir('//PAWC/SEB',' ')
         call hf2(3105,rftest(1),rftest(3),1.)
      ENDIF
c     - See if RF signal is OK at all
      gt_rf_seb=0.
      IF(RF_STATUS.EQ.1.OR.RF_STATUS.EQ.2)THEN
         gt_rf_seb = rf_time(RF_STATUS)
c     
         If(iRFpcorr.eq.0)then
            RF_REGION    = 0
            RF_N_REGIONS = 0
c     
            DO RF_J = 1, 4
               IF(gt_rf_seb.GE.RFG_LOW(RF_J).and.gt_rf_seb.LT.RFG_HIGH(RF_J))THEN
                  RF_REGION = RF_J
                  RF_N_REGIONS = RF_N_REGIONS + 1
               ENDIF
            ENDDO
C     
            FDelta = 0.
            IF(RF_N_REGIONS.EQ.1)THEN
               Fdelta = RFG_PARMS(RF_REGION,1) + RFG_PARMS(RF_REGION,2)*gt_rf_seb + 
     &              RFG_PARMS(RF_REGION,3)*(gt_rf_seb**2) + RFG_PARMS(RF_REGION,4)*(gt_rf_seb**3)
c     &        RFG_PARMS(RF_REGION,3)*(rf_time(1)**2) + RFG_PARMS(RF_REGION,4)*(rf_time(1)**3)
c     
            ELSEIF(RF_N_REGIONS.GT.1.and.NWRITE.lt.7)THEN
               WRITE(*,*)'SEB_TRIGTIME:(WARNING) Overlap in RF regions..'
               NWRITE = NWRITE + 1
            ELSEIF(RF_N_REGIONS.EQ.0.and.NWRITE.lt.7)THEN
               WRITE(*,*)'SEB_TRIGTIME:(WARNING) RF time is out of correction regions..'
               NWRITE = NWRITE + 1         
            ENDIF
         ElseIf(iRFpcorr.eq.1)then
            fdelta=RFpcorr(int(gt_rf_seb))
         endif
c     Corrected RF time 
         gt_rf_seb=gt_rf_seb + or_diff + fdelta
c     - Check with CL01 results.
c     if(NWRITE.lt.7)then
         if(abs(gt_rf_cl01-gt_rf_seb).gt.0.0001.and.NWRITE.lt.7)then
            write(6,*)'Different RF times from CL01 and CALL directly'
            write(6,*)rf_time(1),rf_Time(2),gt_rf_seb,rftest(1),rftest(2),rftest(3)
            NWRITE = NWRITE + 1
         endif
         get_gt_rf=gt_rf_seb
      ELSEIF((RF_STATUS.EQ.3.OR.RF_STATUS.EQ.4).and.abs(gt_rf_cl01).lt.99999.)THEN
c      ELSEIF(RF_STATUS.EQ.3.OR.RF_STATUS.EQ.4)THEN
         gt_rf_cl01=rftest(3)
         gt_rf_seb=gt_rf_cl01
         get_gt_rf=gt_rf_seb
      ELSE 
         if(NWRITE.lt.1)then
            NWRITE = NWRITE + 1
            WRITE(*,*)'SEB_TRIGTIME: (ERROR) RF status = ',RF_STATUS,'( should be 1 through 4)'
         endif
      ENDIF
      Return
      End
      
