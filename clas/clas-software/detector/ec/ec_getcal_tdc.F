      SUBROUTINE EC_GETCAL_TDC(IRUN)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_getcal_tdc.F,v 1.14 2008/03/16 04:33:14 fklein Exp $
c
c  Documentation for subroutine EC_GETCAL_TDC.F
c
c  Purpose: This routine is used to calibration arrays.
c  --------
c
c  Input Parameters:  IRUN
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
c  Author:   Lee Smith      Created:  Thu Aug 22 13:46:25 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     Ky Kim, May 1999, modified to read a new calibration map
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "run_control.inc"
#include "Ec_general.PAR"
#include "EcCal.CMN"
#include "ec_control.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_getcal_tdc.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.14 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:14 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: ec_getcal_tdc.F,v 1.14 2008/03/16 04:33:14 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_GETCAL_TDC.F')
      PARAMETER (CRAUTH='Lee Smith')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer isec,irun,ilow_run_tdc,ihi_run_tdc
      integer i,j,k,l,m
      logical ok_sect(6)
      logical qfirst/.true./

      integer ilun,iret
      real EcCal_1(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_2(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_3(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_4(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_5(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_6(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
      real EcCal_7(MaxEcstrips, Ec_u: Ec_w, inner:outer, MAXsectors) 
c_end_var
c
c  executable code for routine EC_GETCAL_TDC:
c  -------------------------------------------
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        write(CRMESS,99)
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
*
      IF (def_TDC) THEN

        do i = 1, MAXsectors
           do j = inner, cover
              do k = Ec_u, Ec_w
                 do l = 1, MAXstrips
                    EcCal_Tch(l,k,j,i)=default_Tch
                    EcCal_To(l,k,j,i)=0.0
                    EcCal_Tadc(l,k,j,i)=0.0
                    EcCal_dT1(l,k,j,i)=0.0
                    EcCal_dT2(l,k,j,i)=0.0
                    EcCal_Trms(l,k,j,i)=default_Trms
                    EcCal_TDCstat(l,k,j,i)=0.0
                enddo
              enddo
           enddo
        enddo

      ELSE

        call ec_read_tdc_map(irun, EcCal_1, 'EC_Tch',default_Tch)
        call ec_read_tdc_map(irun, EcCal_2, 'EC_To',0.0)
        call ec_read_tdc_map(irun, EcCal_3, 'EC_Tadc',0.0)
        call ec_read_tdc_map(irun, EcCal_4, 'EC_dT1',0.0)
        call ec_read_tdc_map(irun, EcCal_5, 'EC_dT2',0.0)
        call ec_read_tdc_map(irun, EcCal_6, 'EC_Trms',default_Trms)
        call ec_read_tdc_map(irun, EcCal_7, 'TDCstat',0.0)
        do i = 1, MAXsectors
           do j = inner, outer
              do k = Ec_u, Ec_w
                 do l = 1, MaxEcstrips
                    EcCal_Tch(l,k,j,i)=EcCal_1(l,k,j,i)
                    EcCal_To(l,k,j,i)=EcCal_2(l,k,j,i)
                    EcCal_Tadc(l,k,j,i)=EcCal_3(l,k,j,i)
                    EcCal_dT1(l,k,j,i)=EcCal_4(l,k,j,i)
                    EcCal_dT2(l,k,j,i)=EcCal_5(l,k,j,i)
                    EcCal_Trms(l,k,j,i)=EcCal_6(l,k,j,i)
                    EcCal_TDCstat(l,k,j,i)=EcCal_7(l,k,j,i)
                 enddo
              enddo
           enddo
        enddo
        write(crmess, 32) irun
        call recmes(crname,'I',crmess)
c
      ENDIF

10    FORMAT('Calibration data in sector',1x,6(i1,':',L1,',',1x))
32    FORMAT('Loading EC Timing Calibration Database Values for Run:',1x,i4,1x)
99    format('This routine gets TDC calibration parameters')
100   FORMAT('Using default values for TDC constants T/ch=0.05ns, To=0.0')

      RETURN
      END
