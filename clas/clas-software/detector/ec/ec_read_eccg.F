      SUBROUTINE EC_READ_ECCG(irun,ok,jlow_run,jhigh_run)
c
c_begin_doc
c  RCS ID string
c  $Id: ec_read_eccg.F,v 1.2 1997/05/01 19:48:54 freyberg Exp $
c
c  Documentation for subroutine EC_READ_ECCG
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
#include "wcs.inc"
#include "Ec_general.PAR"
#include "ecgeom.inc"
#include "ecgeompar.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_read_eccg.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/01 19:48:54 $')
      PARAMETER (CAUTHO = '$Author: freyberg $')
      DATA CRCSID/   
     1'$Id: ec_read_eccg.F,v 1.2 1997/05/01 19:48:54 freyberg Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_READ_ECCG')
      PARAMETER (CRAUTH='Lee Cole Smith')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer i,j,k,isec
      integer jcal_version,jcal_time,jcal_type
      integer irec,iret,indc,indh,inds
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
c  executable code for routine EC_READ_ECCL.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This routine reads the GEOMETRY BOS banks'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
     
      call revinm('CLAS_PARMS','eccg.bfp',fname)
      call ResLun(crname,Ilun,Iretb)

      If ( Ilun .lt. 10) Write(Bopen,50)Ilun
      If ( Ilun .ge. 10) Write(Bopen,51)Ilun
             
      CALL FPARM(' OPEN CALINPUT ' // Bopen // ' FILE="' // fname // '" ')
      
      write(crmess,52) fname
      call recmes(CRNAME,'L',CRMESS)
      
      irec = 0
      call blist(jw,'C=','HCALECCG')
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
          if (jcal_type.eq.6) then
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
   17   indc = mlink(jw,'ECCG',0)
*
        if (indc.eq.0) then
          WRITE(CRMESS,20) irec
          CALL RECMES(CRNAME,'I',CRMESS)
          GOTO 999
        else
          if(jw(indc).eq.0) goto 999
          EcBsc_r=rjw(indc+1)
          EcBsc_a=rjw(indc+2)    !degrees for sector one 
          ylow=rjw(indc+3)
          yhi=rjw(indc+4)
          dylow=rjw(indc+5)
          dyhi=rjw(indc+6)
          thickness=rjw(indc+7)
          tgrho=rjw(indc+8)
          inds=indc+8
          do k=1,6
            inds=inds+1
            isec=jw(inds)
            EcBsc_phi(isec)=rjw(inds+1)
            xoff(isec)=rjw(inds+2)
            yoff(isec)=rjw(inds+3)
            zoff(isec)=rjw(inds+4)
            inds=inds+4
            ok(isec) =.true.
            do i=1,3
              do j=1,3
                inds=inds+1
                rotm(isec,i,j)=rjw(inds)
              enddo
            enddo
          enddo
          WRITE(CRMESS,21) irec
          CALL RECMES(CRNAME,'I',CRMESS)
          goto 999 	
        endif
      else
        WRITE(CRMESS,30) 
        CALL RECMES(CRNAME,'I',CRMESS)
        goto 999	
      endif 	
  999 continue
      CALL FPARM('CLOSE CALINPUT')
      Call ResLun(crname,-Ilun,Iretb)

100   FORMAT('Contents',1x,i4,1x,'of C LIST is',1x,a4,1x,'bank')
10    FORMAT('Record',1x,i4,1x,': no data in HCAL bank !')
11    FORMAT('Record',1x,i4,1x,': HCAL bank contains GEOMETRY data')
12    FORMAT('Geometry BOS Bank was created at: ',A9,'on ',A3,
     1         ' the ',A2,'th of ',A3,' ',A5)
13    FORMAT('Valid from Run #',i4,1x,'to Run #',i6)  
14    FORMAT('Data are from geometry type',1x,i4)
20    FORMAT('Record',1x,i4,1x,
     1         ': no data in ECCG bank !')
21    FORMAT('Record',1x,i4,1x,
     1         ': ECCG bank contains data')
30    FORMAT('END of BOS file')

50    Format(' UNIT=',I1)
51    Format('UNIT=',I2)
52    Format('Open the geometry file :',a)
            
99    RETURN
      END
c
c------------------------------------------------------------------------------

