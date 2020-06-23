      SUBROUTINE USER_EVNT
c     
c     _begin_doc
c     RCS ID string
c     $Id: user_evnt.F,v 1.19 2009/03/31 20:56:55 clasg9 Exp $
c     
c     Documentation for subroutine USER_EVNT
c     
c     Purpose:  USER supplied EVENT processing routine, called once per event
c     --------
c     
c     Input Parameters:  NONE
c     ----------------
c     
c     Output Parameters: NONE
c     -----------------
c     
c     Called from:
c     ------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:23 EST 1995
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
#include "bnkfmt.inc"
#include "run_control.inc"
#include "histtcl.inc"
#include "user_control.inc"
#include "seb_nt.inc"
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
      PARAMETER ( CFILE = '$RCSfile: user_evnt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.19 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2009/03/31 20:56:55 $')
      PARAMETER ( CAUTHO = '$Author: clasg9 $')
      DATA CRCSID/
     1     '$Id: user_evnt.F,v 1.19 2009/03/31 20:56:55 clasg9 Exp $
     3     '/
c     Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='USER_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE,ICYCLE,iret
      INTEGER ncol, nrow, nami, ntid
      data nami/0/
      integer mamind
      external mamind
      integer ind, ns, err(10), i, ierr, nraw, nmess, nmon
      INTEGER LENOCC, LNBLNK
c     _end_var
c     
c     executable code for routine USER_EVNT:
c     -------------------------------------
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         nraw=0
         nmon=0
         nmess=0
         do i=1,10
            if(outbanknames(i).ne.'junk')write(6,*)outbanknames(i)
         enddo
      ENDIF
c     The following is just an example of how to fill histograms, including
c     changing PAWC directories.
c     
c     Perform fill of all BOS bank assoccated N tuples
      IF (LALL_NT_DO) then
         CALL ALL_NT_FILL(ntid)
         call hcdir('//PAWC',' ')
         call hcdir('//LUN1',' ')
         If(ntid.ge.10) CALL hfnt(ntid)
c     Perform fill of N tuples from MCTK
      ELSEIF (LMCTK_NT_DO) THEN
         call hcdir('//PAWC',' ')
c------------------------------------------------------------72
         call hcdir('//LUN1',' ')
         CALL FILL_MC_NT(ierr)
         If(ierr.eq.0) CALL hfnt(19)
      ENDIF
c     change back to top directory       
c     perform users fill histograms
      nmon=nmon+1
      IF(.NOT.LMON_HIST .OR.nmon.lt.3000000)THEN
         IF ((LUSR0_DO.OR.LUSR1_DO).AND.(LUSR0_H_DO.or.LUSR1_H_DO)) CALL USER_FILL

c     perform tracking fill histograms
         IF (LTRK_H_DO) CALL DCTRK_FILL

c     Perform Calorimeter fill histograms
         IF (LEGN_H_DO) CALL EC_FILL

c     Perform Large Angle Calorimeter fill histograms
         IF (LEC1_H_DO) CALL EC1_FILL

c     Perform Time of Flight fill histograms
         IF (LTOF_H_DO) CALL SC_FILL

c     Perform Cerenkov fill histograms
         IF (LCC_H_DO) CALL CC_FILL

c     Perform TAGGER fill histograms
         IF (LTAGGER_H_DO) CALL TAG_FILL(.true.,.true.,.true.,.true.)

c     Perform histogram and N tupl fill for event builder
         IF (LSEB_H_DO) CALL SEB_FILL
c     
      ELSEIF(nmon.eq.3000001)THEN
         call hcdir('//PAWC',' ')
         call hcdir('//LUN2',' ')
         CALL HRPUT(0,' ','T')
         CALL HREND('LUN2')
         if(let.gt.0) write(let,*)'Closing Monhist file'
         close(LHM)
         CALL RESLUN(CRNAME,-LHM,IRET)
         LTRK_H_DO    = .FALSE.
         LEGN_H_DO    = .FALSE.
         LCC_H_DO     = .FALSE.
         LTOF_H_DO    = .FALSE.
         LEC1_H_DO    = .FALSE.
         LSEB_H_DO    = .FALSE.
         LMON_HIST    = .FALSE.
      ELSE
c     Nothing left to do at this point.
      ENDIF
      call hcdir('//PAWC',' ')

c     Calling FSEQW tells FPACK to write this event out
c     comment it out or place in an if-then if you wish to be
c     a bit more sophisticated then merely writing every event out
c     
      do i=1,10
         err(i)=1
      enddo
      call user_xtra(err)
      nmess=nmess+1
c
      nraw=nraw+1
      if(LMySQL.and.NMySQL.gt.0)Then
         if(nraw.ge.NMySQL)then
            nraw=0
            nami=mamind(jw,'CSQL')
            ind=0
            if(nami.gt.0)ind=JW(nami)
            if(ind.gt.0)then
               call set_group('CSQL',JW(ind+1))
               call write_mon_table('SYSTCSQL')
            endif
         endif
      endif
c
      nami=mamind(iw,'HEAD')
      ind=0
      if(nami.gt.0)ind=IW(nami)
      if(ind.gt.0)then
         if(let.GT.0) then
            if(iw(ind+5).eq.3.and.iw(ind+6).ne.0)then
               write(let,*)iw(ind+3)
            endif
         endif
      endif
c      call bprnt(iw,'HEADEVNTECPBECHB')
      DO i=1,10
         IF(outbanknames(i).ne.'junk')THEN
            ns=i+1
            If(outbanknames(i).eq.'all')Then
               call fwbos(iw,ns,CBANK_WRITE(1:LENOCC(CBANK_WRITE)),ierr)
            Elseif(lnblnk(outbanknames(i)).gt.0) then
               if(err(i).eq.0)then
                  call fwbos(iw,ns,outbanknames(i),ierr)
               else
c                  nami=mamind(iw,'HLS ')
c                  if(iw(nami).gt.0)
c     +                 call fwbos(iw,ns,'HEADHLS ',ierr)
               endif
            Endif
         ENDIF
      ENDDO
      RETURN
      END
c
c----------------------------------------------------------------------




