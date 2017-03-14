      SUBROUTINE FILL_EPICS_NT(ierr)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_epics_nt.F,v 1.4 2008/03/16 04:33:09 fklein Exp $
c
c  Documentation for subroutine FILL_EPICS_NT.F
c
c  Purpose:
c  --------
c
c  Input Parameters: Non (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:Non  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  2004
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
#include "bcs.inc"
#include "clasmdl.inc"
#include "epics_nt.inc"
#include "epics.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_epics_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:09 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: fill_epics_nt.F,v 1.4 2008/03/16 04:33:09 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_EPICS_NT.F')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      integer gtime(9)
      INTEGER NWRITE, ierr,i, ITGBI,ind,MAMIND
      DATA ITGBI/0/
c_end_var
c
c  executable code for routine FILL_EPICS_NT.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling a N-tuple for EPICS hits'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      ierr=0
      run=NCCRUN
      event=nevent
      call gmtimef(evtime,gtime)
      evntime=((gtime(4)-17)*24+gtime(3))*3600+gtime(2)*60+gtime(1)
c beam energy
c      ebsya=epics_value(1)
c      ebsyb=epics_value(2)
c      etiefa=epics_value(111)
c magnet settings
c      if(epics_value(146).ne.0.)ierr=1
c      pscurrent=epics_value(146)
c      psprobe=epics_value(150)
c      taggerprobe=epics_value(149)
c      taggercurrent=epics_value(28)
c BPMs
c      x2c21a=epics_value(67)
c      y2c21a=epics_value(68)
c      c2c21a=epics_value(69)
c      x2c24a=epics_value(58)
c      y2c24a=epics_value(59)
c      c2c24a=epics_value(60)
      do i=1,tot_epics
         if(epics_name(i)(1:13).eq.'MBSY1C_energy') then
            ebsya=epics_value(i)
         elseif(epics_name(i)(1:13).eq.'MBSY2C_energy') then
            ebsyb=epics_value(i)
         elseif(epics_name(i)(1:9).eq.'halla_MeV') then
            etiefa=epics_value(i)
         elseif(epics_name(i)(1:7).eq.'TMIRBCK') then
            taggercurrent=epics_value(i)
         elseif(epics_name(i)(1:10).eq.'PSPECIRBCK') then
            pscurrent=epics_value(i)
            ierr=1
         elseif(epics_name(i)(1:22).eq.'Hac_TGR_LS450:FLD_DATA') then
            taggerprobe=epics_value(i)
         elseif(epics_name(i)(1:21).eq.'Hac_PS_LS450:FLD_DATA') then
            psprobe=epics_value(i)
         elseif(epics_name(i)(1:13).eq.'IPM2C21A.XPOS') then
            x2c21a=epics_value(i)
         elseif(epics_name(i)(1:13).eq.'IPM2C21A.YPOS') then
            y2c21a=epics_value(i)
         elseif(epics_name(i)(1:8).eq.'IPM2C21A') then
            c2c21a=epics_value(i)
         elseif(epics_name(i)(1:13).eq.'IPM2C24A.XPOS') then
            x2c24a=epics_value(i)
         elseif(epics_name(i)(1:13).eq.'IPM2C24A.YPOS') then
            y2c24a=epics_value(i)
         elseif(epics_name(i)(1:8).eq.'IPM2C24A') then
            c2c24a=epics_value(i)
         endif
      enddo

      IF(ITGBI.eq.0)ITGBI=MAMIND(IW,'TGBI')      
      ind=IW(ITGBI)
      if(itgbi.gt.0 .and. ind.gt.0) intt=rw(ind+3)

      RETURN
      END
c
c------------------------------------------------------------------------------
