      SUBROUTINE EC_READ_ECCT(irun,ok,jlow_run,jhigh_run)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_read_ecct.F,v 1.3 1999/05/12 00:35:55 kykim Exp $
c
c  Documentation for subroutine EC_READ_ECCT
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
c  Author:   Lee Cole Smith      Created:  Thu Aug 22 14:00:59 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c     Ky Kim, May 1999, Updated for the new EC timing
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "wcs.inc"
#include "Ec_general.PAR"
#include "EcCal.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      logical ok(6)
      integer irun,jlow_run,jhigh_run	
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_read_ecct.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/05/12 00:35:55 $')
      PARAMETER (CAUTHO = '$Author: kykim $')
      DATA CRCSID/   
     1'$Id: ec_read_ecct.F,v 1.3 1999/05/12 00:35:55 kykim Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_READ_ECCT')
      PARAMETER (CRAUTH='Lee Cole Smith')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer i,j,layer,strip,axis,isec,view
      integer jcal_version,jcal_time,jcal_type
      integer irec,iret,indc,indh
      character*30 crtim 
      character*4 name
      Integer  Ilun, Iretb
      Character*7 Bopen
      CHARACTER*50 fname
c      
c  External functions:
c  -------------------
      integer MLINK
      character*4 NLIST
c_end_var
c
c  executable code for routine EC_READ_ECCT.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine reads the TDC calibration BOS banks'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
     
      call revinm('CLAS_PARMS','ECCT.bfp',fname)
      call ResLun(crname,Ilun,Iretb)
     
      If ( Ilun .lt. 10) Write(Bopen,50)Ilun
      If ( Ilun .ge. 10) Write(Bopen,51)Ilun
             
      CALL FPARM(' OPEN CALINPUT ' // Bopen // ' FILE="' // fname // '" ')
      
      write(crmess,52) fname
      call recmes(CRNAME,'L',CRMESS)
      
      irec = 0
      call blist(jw,'C=','HCALECCT')
18    call frbos(jw,ilun,'C',iret)
      do i = 1,2
        name = nlist(jw,i,'C')
        if (name.ne.'    ') then
          WRITE(CRMESS,100) i,name
          CALL RECMES(CRNAME,'I',CRMESS)
        endif	
      enddo
      isec = 0
      if (iret.eq.0) then
        irec = irec + 1
        indh  = mlink(jw,'HCAL',0)
        if (indh.eq.0) then
          WRITE(CRMESS,10) irec
          CALL RECMES(CRNAME,'I',CRMESS)
          goto 18
        else	
          jcal_version  = jw(indh+1)
          jlow_run      = jw(indh+2)
          jhigh_run     = jw(indh+3)
          jcal_time     = jw(indh+4)
          jcal_type     = jw(indh+5)
          if (jcal_type.eq.4) then
            WRITE(CRMESS,11) irec
            CALL RECMES(CRNAME,'I',CRMESS)
            jcal_time = jcal_time + 4*3600
            CALL getasciitime(jcal_time,CRTIM)
            WRITE(CRMESS,12) CRTIM(12:20),
     1                       CRTIM(1:3),CRTIM(9:10),
     2                       CRTIM(5:7),CRTIM(21:24)
            CALL RECMES(CRNAME,'I',CRMESS)
            WRITE(CRMESS,13) jlow_run,jhigh_run
            CALL RECMES(CRNAME,'I',CRMESS)
          else
            WRITE(CRMESS,14) jcal_type
            CALL RECMES(CRNAME,'I',CRMESS)
            goto 18
          endif	
          if (irun.ge.jlow_run.and.irun.lt.jhigh_run) then
            goto 17
          else
            goto 18
          endif		
        endif	
17      isec = isec + 1
        indc = mlink(jw,'ECCT',isec)
        if (indc.eq.0) then
          WRITE(CRMESS,20) irec,isec
          CALL RECMES(CRNAME,'I',CRMESS)
          if(isec.le.5) goto 17
        else
          if(jw(indc).eq.0) goto 17
            do j = 1, jw(indc), 12 
              view   = jw(indc+j)/256
              strip  = mod(jw(indc+j),256)
              layer  = inner + (view-1)/3
              axis   = Ec_u + mod(view-1,3)
CTime              EcCal_To(strip,axis,layer,isec)  = rjw(indc+j+1)
CTime              EcCal_Tch(strip,axis,layer,isec) = rjw(indc+j+3)
            enddo
          ok(isec)=.true.
          WRITE(CRMESS,21) irec,isec
          CALL RECMES(CRNAME,'I',CRMESS)
c go to next sector
          if(isec.le.5) goto 17
c go to next record
          goto 99 	
        endif
      else
        WRITE(CRMESS,30) 
        CALL RECMES(CRNAME,'I',CRMESS)
        goto 99	
      endif 	
99    CONTINUE
      CALL FPARM('CLOSE CALINPUT')
      Call ResLun(crname,-Ilun,Iretb)

100   FORMAT('Contents',1x,i4,1x,'of C LIST is',1x,a4,1x,'bank')
10    FORMAT('Record',1x,i4,1x,': no data in HCAL bank !')
11    FORMAT('Record',1x,i4,1x,': HCAL bank contains TDC ',1x,
     1         'Calibration data')
12    FORMAT('Calibration BOS Bank was created at: ',A9,'on ',A3,
     1         ' the ',A2,'th of ',A3,' ',A5)
13    FORMAT('Valid from Run #',i4,1x,'to Run #',i6)  
14    FORMAT('Data are from calibration type',1x,i4)
20    FORMAT('Record',1x,i4,1x,'Sector',1x,i1,1x,
     1         ': no data in ECCT bank !')
21    FORMAT('Record',1x,i4,1x,'Sector',1x,i1,1x,
     1         ': ECCT bank contains data')
30    FORMAT('END of BOS file')

50    Format(' UNIT=',I1)
51    Format('UNIT=',I2)
52    Format('Open the calibration file :',a)
            
      RETURN
      END
c
c------------------------------------------------------------------------------

