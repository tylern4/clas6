      SUBROUTINE EC_GETCAL_ADC(IRUN)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_getcal_adc.F,v 1.13 2008/03/16 04:33:14 fklein Exp $
c
c  Documentation for subroutine EC_GETCAL_ADC.F
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
c     
c
c_end_doc
c
      IMPLICIT NONE
C      SAVE
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
      integer irun
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_getcal_adc.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.13 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:14 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: ec_getcal_adc.F,v 1.13 2008/03/16 04:33:14 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_GETCAL_ADC.F')
      PARAMETER (CRAUTH='Lee Smith')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer i,j,k,l
      logical ok_sect(6)
      real EcCal_1(MaxEcstrips, Ec_u: Ec_w, whole:outer, MAXsectors) 
      real EcCal_2(MaxEcstrips, Ec_u: Ec_w, whole:outer, MAXsectors) 
c_end_var
c
c  executable code for routine DC_GETCAL_TDLY:
c  -------------------------------------------
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        write(CRMESS,99)
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
*
      IF ( def_ADC ) THEN      
        write(CRMESS,100)	
        CALL RECMES(CRNAME,'I',CRMESS)
*
        do i = 1, MAXsectors
          do j = inner, cover
            do k = Ec_u, Ec_w
              do l = 1, MAXstrips
                EcCal_Eo(l,k,j,i) = 0.0
                EcCal_Ech(l,k,j,i) = default_Ech
              enddo
            enddo
          enddo
        enddo
*
      ELSE
*
c     read the map
c
         call ec_read_map(irun, EcCal_1, 'EC_GAIN',default_Ech)
         call ec_read_map(irun, EcCal_2, 'EC_PEDESTALS',400.0)
         do i = 1, MAXsectors
          do j = whole, outer
            do k = Ec_u, Ec_w
              do l = 1, MaxEcstrips
                EcCal_Ech(l,k,j,i) = EcCal_1(l,k,j,i)
                EcCal_Eo(l,k,j,i) = EcCal_2(l,k,j,i)
              enddo
            enddo
          enddo
        enddo
        write(crmess, 32) irun
        call recmes(crname,'I',crmess)
      ENDIF

32    FORMAT('Loading Calibration Database Values for Run:',1x,i4,1x)  
10    FORMAT('Calibration data in sector',1x,6(i1,':',L1,',',1x))
99    format('This routine gets ADC calibration parameters')
100   FORMAT('Using default values for ADC constants E/ch=0.0001MeV, Eo=0.0')

      RETURN
      END
c
c------------------------------------------------------------------------------




