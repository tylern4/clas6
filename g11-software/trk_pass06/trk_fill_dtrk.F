      SUBROUTINE trk_fill_dtrk(ntrk,ok_trk)
*_begin_doc
*  RCS ID string
*  $Id: trk_fill_dtrk.F,v 1.6 1999/02/16 01:58:31 boiarino Exp $
*
*  Documentation for subroutine TRK_FILL_DTRK
*
*  Purpose: This routine fills the DTRK bos banks.
*  --------                    
*                         
*  Inputs Parameters: NTRK total number of track
*  -----------------
*
*  Outputs Parameters: None
*  -------------------
*
*  Other routines: ana_prfit
*  ---------------
*
*  Notes: Bank for CED
*  -----
*
*  Author:         Francois H. Roudot 
*  ------         
*
*  Creation Date:  15 November 1996
*  -------------
*
*  Major revisions:
*  --------------- 
*
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "anapatt.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      integer ntrk,ok_trk(MAX_TRACKS)
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_fill_dtrk.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/02/16 01:58:31 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: trk_fill_dtrk.F,v 1.6 1999/02/16 01:58:31 boiarino Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_FILL_DTRK')
      PARAMETER (CRAUTH='Francois H. Roudot')
*
*  Local User defined variables:
*  -----------------------------
      integer*4 MBANK
      integer*4 ind
      integer*4 rec
      real test(4),sc_good
      integer*4 j
      INTEGER ip,ipsc,il,i,k,pid,npln,nst_max,wr_sc
      character*2 DET(46)

      data pid /0/
      data det /'VT',2*'ST',36*'DC','CC',4*'SC','EC','LA'/
      data npln /46/ 
      data nst_max /3/        

      do i = 1, ntrk
        if(ok_trk(i).eq.0) then
          rec = itr_sect(i) + 16 * i + 512 * pid
	  ind = MBANK(iw,'DTRK',rec,3,37)
	  if (ind.eq.0) then
		write (6,'('' Error in creating the bank "DTRK"'')')
		go to 1000
          endif
          j = -3
          wr_sc = 0
          do 30 ip = 1,npln
            il = ip - nst_max
            IF((DET(ip).eq.'DC').and.(il.ne.5).and.(il.ne.6)) then
              j= j + 3
	      rw(ind+j+1)  = trkp(2,ip,i)
	      rw(ind+j+2)  = trkp(3,ip,i)
	      rw(ind+j+3)  = trkp(1,ip,i)
            endif
            IF(DET(ip).eq.'CC') then
              j= j + 3
	      rw(ind+j+1)  = trkp(2,ip,i)
	      rw(ind+j+2)  = trkp(3,ip,i)
	      rw(ind+j+3)  = trkp(1,ip,i)
            ENDIF
            IF(DET(ip).eq.'SC'.and.wr_sc.eq.0) then
              do k = 0, 3
                test(k+1) = sqrt(trkp(1,ip+k,i)**2+trkp(2,ip+k,i)**2+trkp(3,ip+k,i)**2)
              enddo
              sc_good = min(test(1),test(2),test(3),test(4))
              if(sc_good.eq.test(1)) ipsc = ip
              if(sc_good.eq.test(2)) ipsc = ip + 1
              if(sc_good.eq.test(3)) ipsc = ip + 2
              if(sc_good.eq.test(4)) ipsc = ip + 3
              j= j + 3
              rw(ind+j+1)  = trkp(2,ipsc,i)
              rw(ind+j+2)  = trkp(3,ipsc,i)
              rw(ind+j+3)  = trkp(1,ipsc,i)
              wr_sc = 1
            ENDIF
            IF(DET(ip).eq.'EC'.and.trkp(1,ip,i).ne.1000) then
              j= j + 3
              rw(ind+j+1)  = trkp(2,ip,i)
              rw(ind+j+2)  = trkp(3,ip,i)
              rw(ind+j+3)  = trkp(1,ip,i)
            ENDIF
            IF(DET(ip).eq.'LA'.and.trkp(1,ip,i).ne.1000) then
              j= j + 3
              rw(ind+j+1)  = trkp(2,ip,i)
              rw(ind+j+2)  = trkp(3,ip,i)
              rw(ind+j+3)  = trkp(1,ip,i)
            ENDIF
 30       CONTINUE
        endif
      enddo

1000  RETURN
      END





