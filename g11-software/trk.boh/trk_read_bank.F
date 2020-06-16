
      SUBROUTINE trk_read_bank(ntracks)
*
*_begin_doc
*  RCS ID string
*  $Id: trk_read_bank.F,v 1.6 1999/02/24 02:50:23 boiarino Exp $
*
*  Documentation for subroutine TRK_READ_BANK
*
*  Purpose:  Reads the reconstructed track banks and fills 
*  -------	   the arrays which are used in time-based tracking
*
*            (allows Separation of Hit and Time Based Tracking)
*
*  Inputs Parameters: None
*  ------------------
*
*  Outputs Parameters: ntracks  - number of tracks read in
*  -------------------
*
*  calling routines: trk_evnt_tbt
*  -----------------
*
*  Notes:
*  ------
*
*  Author:        Franz Klein  (Nov.1997)
*  -------         
*
*  Major revisions: 
*  ---------------- 
*
*_end_doc
*
      Implicit None
      Save
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "trktcl_var.inc"
#include "anapatt.inc"
#include "sdaswim.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      integer  ntracks
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_read_bank.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/02/24 02:50:23 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: trk_read_bank.F,v 1.6 1999/02/24 02:50:23 boiarino Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_READ_BANK')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local User defined variables:
*  -----------------------------
      INTEGER  MAMIND
      INTEGER  ind1, ind2, ind3, ind, ncol

      INTEGER irun, ievnt, irec
      INTEGER my_level, id
      INTEGER it, its, trkno2(MAX_TRACKS)
      INTEGER i, j, nd, isec, il, ipl
      INTEGER icl, is, isg
      INTEGER*4  my_id, my_tdc, my_ind
      INTEGER*2  my_bid(2)
      EQUIVALENCE (my_bid(1),my_id)

      INTEGER ind_hbla,ind_hdpl,ind_hbtr
      INTEGER ind_dc00, ind_dc01, dc01_flag
      INTEGER ndat_dc01(6), idsec_dc01(6)
      REAL vect(3), vecnorm

      INTEGER my_errcnt
      DATA my_errcnt/0/
      DATA ind_hbla/0/,ind_hdpl/0/,ind_hbtr/0/,ind_dc00/0/


***************

      If (ind_dc00 .le. 0) ind_dc00 = MAMIND(iw,'DC0 ')	
      If (ind_dc01 .le. 0) ind_dc01 = MAMIND(iw,'DC1 ')	
      If (ind_hbla .le. 0) ind_hbla = MAMIND(iw,'HBLA')
      If (ind_hdpl .le. 0) ind_hdpl = MAMIND(iw,'HDPL')
      If (ind_hbtr .le. 0) ind_hbtr = MAMIND(iw,'HBTR')
*
      ntracks = 0
      CALL RERNEV(irun,ievnt,irec)

      If ( (IW(ind_hbtr) .LE. 0) .OR. 
     &           (IW(ind_hbla) .LE. 0) .OR. 
     &           (IW(ind_hdpl) .LE. 0) ) my_level = 0
*
*
*        Abort on ERROR_CONDITION (Log the first 30).
*
      If ( my_level .LE. 0 ) Then
*
         If ( my_errcnt .LT. 30 ) Then
*
            Write(CRMESS,'(A,I8,A)')' Event:',ievnt,
     &                      ': no TRACKING Banks found'
            Call RecMes(CRNAME,'W',CRMESS)
            my_errcnt = my_errcnt +1
*
         Endif
*
         Goto 999
*
      Endif
*

      do it=1,MAX_TRACKS
        do i=1,3
          do j=1,nplane
            trkp(i,j,it) = 1000.		!position not known
          enddo
        enddo
        trk(12,5,it) = 11.
        trk(12,6,it) = 11.
      enddo

*
*        Bank switching based on who we are using:
c-
      if(IW(ind_dc01).gt.0)then
        dc01_flag=1
        ind = ind_dc01 + 1
      elseif(IW(ind_dc00).gt.0)then
        dc01_flag=0
        ind = ind_dc00 + 1
      else
        dc01_flag=-1
      endif
      if(dc01_flag.ge.0)then
         do while ( IW(ind-1).GT.0 )
            ind = IW(ind-1)
            isec = IW(ind-2)
            if(dc01_flag.EQ.1) then
               ndat_dc01(isec) = IW(ind)/2
            else
               ndat_dc01(isec) = IW(ind)
            endif
            idsec_dc01(isec) = ind
         enddo
      else
         do isec=1,6
            ndat_dc01(isec)=0
            idsec_dc01(isec)=0
         enddo
      endif
*
      ind1 = IW(ind_hbtr)
      ind2 = ind_hbla +1
      ind3 = ind_hdpl +1

      it = 0
      call vzero(trkno2,MAX_TRACKS)
c- Now read first HBTR - 

      ncol = IW(ind1-5)
      nd = IW(ind1)-1
      if(nd.GT.0) then
        do j = 0,nd,ncol
            id = IW(ind1+9+j)
            its = mod(id,100)
            isec = id/100
            it = it +1
            itr_sect(it)  = isec
            trkno2(it) = its
c  Momentum and charge
          trkp(7,1,it)  = SQRT(RW(ind1+4+j)**2+RW(ind1+5+j)**2+RW(ind1+6+j)**2)
          trkp(9,1,it)  = RW(ind1+7+j)
          trkp(11,1,it) = my_level + itr_sect(it)*10
          trkp(12,1,it) = RW(ind1+8+j)         ! Chi2
        enddo
      endif
      ntr_link=it
      ntracks = ntr_link
c- fill standard information for each DC plane -  
      do it = 1,ntr_link
        do ipl = 2,nplane
          do i = 7,12
              trkp(i,ipl,it) = trkp(i,1,it)
          enddo
        enddo
      enddo
*  Read HBLA bank

      do while ( IW(ind2-1).GT.0 )
        ind2 = IW(ind2-1)
        isec = IW(ind2-2)
        ncol = IW(ind2-5)
        nd   = IW(ind2)-1
        its=0
        if(nd.GT.0) then
          do j=0,nd,ncol
            it = IW(ind2+1+j)/100
            its=its+1
            if(it.LE.0 .OR. it.GT.ntr_link.OR.itr_sect(it).ne.isec) then
              write(CRMESS,'(A,I8,2(A,I3))') ' Event',ievnt,
     &          ' HBLA/TBLA: Sector#',isec,' Track# not defined',it
              call RecMes(CRNAME,'W',CRMESS)
            else
              ipl= mod(IW(ind2+1+j),100)
              il = ipl - nst_max
              trkp(2,ipl,it) = RW(ind2+2+j)		!position
              trkp(3,ipl,it) = RW(ind2+3+j)
              trkp(1,ipl,it) = RW(ind2+4+j)
              trkp(14,ipl,it) = RW(ind2+5+j)		!direction
              trkp(15,ipl,it) = RW(ind2+6+j)
              trkp(13,ipl,it) = RW(ind2+7+j)
              if(il.GT.1) then
                 vecnorm=0
                 do i=1,3
                    vect(i)=trkp(i,ipl,it)-trkp(i,ipl-1,it)
                    vecnorm=vecnorm+vect(i)*vect(i)
                 enddo
                 vecnorm=SQRT(vecnorm)
                 do i=1,3
                    trkp(i+3,ipl,it)=vect(i)/vecnorm
                 enddo
              endif
              if(il.EQ.36) then
                 do i=4,6
                    trkp(i,ipl,it)=trkp(i,ipl-1,it)
                 enddo
              endif
              trkp(10,ipl,it)= RW(ind2+8+j)		!track length
              trk(1,il,it)  = IW(ind2+11+j)		!wire#
              trk_pt(il,it) = IW(ind2+9+j)		!ptr to DC1
              if(trk_pt(il,it).GT.0 .AND. trk_pt(il,it).LE.ndat_dc01(isec))then
c                if(dc01_flag.eq.1) then
c                 trk(2,il,it)  = RW(idsec_dc01(isec)+trk_pt(il,it)*2)	!tdc
c                else
                 my_id = IW(idsec_dc01(isec)+trk_pt(il,it))	        !tdc
                 my_tdc = my_bid(2)
                 trk(2,il,it)  = my_tdc
c                endif
              endif
              trk(3,il,it)  = RW(ind2+12+j)		!drift time
              trk(5,il,it)  = RW(ind2+15+j)		!Sigma doca
              trk(7,il,it)  = RW(ind2+16+j)		!fitted doca
              trk(8,il,it)  = RW(ind2+8+j)		!track length
              trk(9,il,it)  = RW(ind2+14+j)		!wire length to preamp.
              trk(10,il,it) = RW(ind2+13+j)		!track angle in Layer
              trk(11,il,it) = my_level + itr_sect(it)*10
              trk(12,il,it) = IW(ind2+10+j)		!Status
            endif
          enddo
        endif
      enddo

*  Read HDPL bank

      do while ( IW(ind3-1).GT.0 )
        ind3 = IW(ind3-1)
        isec = IW(ind3-2)
        ncol = IW(ind3-5)
        nd   = IW(ind3)-1
        if(nd.GT.0) then
          do j=0,nd,ncol
            id = IW(ind3+1+j)
            it = id/100
c            it = trkno2(it)				!wrt. TRKS, HBTB banks
            ipl= mod(id,100)
            if(it.LE.0.OR.it.GT.ntr_link.OR.ipl.GT.nplane.OR.itr_sect(it).ne.isec)then
              write(CRMESS,'(A,I8,3(A,I3))') ' Event',ievnt,
     &      ' HBLA/TBLA: Sector#',isec,' Track# not defined',it,'  plane',ipl
              call RecMes(CRNAME,'W',CRMESS)
            else
              trkp(1,ipl,it) = RW(ind3+4+j)
              trkp(2,ipl,it) = RW(ind3+2+j)		!position
              trkp(3,ipl,it) = RW(ind3+3+j)
              trkp(4,ipl,it) = RW(ind3+7+j)
              trkp(5,ipl,it) = RW(ind3+5+j)		!direction
              trkp(6,ipl,it) = RW(ind3+6+j)
              trkp(10,ipl,it) = RW(ind3+8+j)		!track length
            endif
          enddo
        endif
      enddo
c
      do it=1,ntr_link
c        if(itr_level(it).GT.0) then
c          if(trkp(10,11,it)+trkp(10,17,it)+trkp(10,23,it).EQ.0.) then
c            itr_level(it)=0
c            ntracks=ntracks-1
c          endif
          trk(1,5,it) = -ABS(trk(1,4,it))
          trk(1,6,it) = trk(1,5,it)
c        endif
      enddo
*
 999  continue
      end






