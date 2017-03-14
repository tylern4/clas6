
      SUBROUTINE trk_read_banks(ntracks)
*
*_begin_doc
*  RCS ID string
*  $Id: trk_read_banks.F,v 1.10 1999/02/24 02:50:24 boiarino Exp $
*
*  Documentation for subroutine TRK_READ_BANKS
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
*  Minor revisions: Curtis A. Meyer (December 1997)
*        MAMIND was not being used correctly in this routine. The
*        result would have been a "slow down" because of a lot
*        of expensive BOS operations. In fixing this, I noticed
*        another incorrect usage of MAMIND which required the
*        introduction of a pointer for both the DC1 and DC0
*        banks, and some subsequent code to correctly switch
*        between these.
* 
*  ----------------
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
      PARAMETER ( CFILE = '$RCSfile: trk_read_banks.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.10 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/02/24 02:50:24 $')
      PARAMETER ( CAUTHO = '$Author: boiarino $')
      DATA CRCSID/
     1'$Id: trk_read_banks.F,v 1.10 1999/02/24 02:50:24 boiarino Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_READ_BANKS')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local User defined variables:
*  -----------------------------
      INTEGER  MAMIND
      INTEGER  ind1, ind2, ind3, ind, ncol
c      INTEGER  ncol_hbla/16/
c      INTEGER  ncol_tbla/18/
c      INTEGER  ncol_dpl/8/
c      INTEGER  ncol_hbtb/10/
c      INTEGER  ncol_hbtr/9/      
c      INTEGER  ncol_tbtr/10/
c      INTEGER  ncol_trks/7/

      INTEGER irun, ievnt, irec
      INTEGER my_level, id
      INTEGER it, it4, trkno2(MAX_TRACKS)
      INTEGER i, j, nd, isec, il, ipl
      INTEGER icl, is, isg
      INTEGER*4  my_id, my_tdc, my_ind
      INTEGER*2  my_bid(2)
      EQUIVALENCE (my_bid(1),my_id)

      INTEGER ind_hbtb,ind_tbla,ind_tdpl,ind_tbtr
      INTEGER ind_trks,ind_hbla,ind_hdpl,ind_hbtr
      INTEGER ind_dc01,ind_dc00, dc01_flag
      INTEGER ndat_dc01(6), idsec_dc01(6)

      INTEGER my_errcnt
      DATA my_errcnt/0/
      DATA ind_hbtb/0/,ind_tbla/0/,ind_tdpl/0/,ind_tbtr/0/
      DATA ind_trks/0/,ind_hbla/0/,ind_hdpl/0/,ind_hbtr/0/
      DATA ind_dc01/0/,ind_dc00/0/


***************
*
*        It is only desireable to call MAMIND once, then
*        let BOS deal with this stuff. There is a fairly
*        large overhead on this call. There was also a
*        fairly serious misunderstanding on how MAMIND 
*        was used in selecting either the DC0 or DC1
*        banks. What was done was likely to "fail".
*        Note that these "indicies" will have non-zero
*        values EVEN when the bank does not exist. In
*        the case of a nonexistent bank, IW(IND) will
*        be <= 0 .
*
      If (ind_trks .le. 0) ind_trks = MAMIND(iw,'TRKS')
      If (ind_tbla .le. 0) ind_tbla = MAMIND(iw,'TBLA')
      If (ind_tdpl .le. 0) ind_tdpl = MAMIND(iw,'TDPL')
      If (ind_tbtr .le. 0) ind_tbtr = MAMIND(iw,'TBTR')
      If (ind_hbtb .le. 0) ind_hbtb = MAMIND(iw,'HBTB')
      If (ind_hbla .le. 0) ind_hbla = MAMIND(iw,'HBLA')
      If (ind_hdpl .le. 0) ind_hdpl = MAMIND(iw,'HDPL')
      If (ind_hbtr .le. 0) ind_hbtr = MAMIND(iw,'HBTR')
      If (ind_dc00 .le. 0) ind_dc00 = MAMIND(iw,'DC0 ')	
      If (ind_dc01 .le. 0) ind_dc01 = MAMIND(iw,'DC1 ')
*
*        Here the code is trying to decide if the DC1
*        banks or the DC0 banks should be taken as the
*        starting point of analysis. The default is to
*        take the DC1, and if they do not exist, drop 
*        back to the DC0 banks: 
*       
*        The following status code pattern should come
*        out of the following logic:
*
*        dc01_flag=1 : DC1 exists and being used.
*        dc01_flag=0 : DC1 does not exist, using DC0
*        dc01_flag=-1: DC1 and DC0 both do not exist.
*
*
*        Check for DC1 by asking if there is a bank in
*        the BOS memory. If not then check for a DC0
*        bank.
*
      If ( IW(ind_dc01) .GT. 0 ) Then
*
         dc01_flag = 1
*
      Else
*
         If ( IW(ind_dc00) .GT. 0) Then
*
            dc01_flag = 0
*
         Else
*
            dc01_flag=-1
*
         Endif
*
      Endif
*
      ntracks = 0
      CALL RERNEV(irun,ievnt,irec)
*
*        The following section of code decides which level of
*        tracking output to use. The default will be time-based-
*        tracking. The flag "my_level" is set to the following
*        masks:
*
*           TRK_LEVEL_tbt: take things from time-based. In order
*                          for this to be set, the following
*                          banks must exist:
*                          "HBTB","TRKS","TBLA","TBTR","TDPL".
*
*           TRK_LEVEL_hbt: Time based does not exist, take
*                          things from hit based. The following
*                          banks must exist for this to happen:
*                          "HBTB","TRKS","HBLA","HBTR","HDPL"
*
*           0            : Neither HBT nor TBT exist condition
*                          satisfied, or if the dc01_flag is 
*                          negative, i.e. neither DC0 nor DC1. 
*
      my_level = TRK_LEVEL_tbt
*
*        Check to make sure that both the "HBTB" and "TRKS" banks
*        exist and that we have some DCx banks:
*
      If ( (dc01_flag    .LT. 0) .OR. 
     &     (IW(ind_hbtb) .LE. 0) .OR.
     &     (IW(ind_trks) .EQ. 0) ) Then
*
         my_level = -1
*
      Else
*
*           Check to see if we are forced to drop back to HBT because
*           of missing TBT banks??
*
         If ( (IW(ind_tbla) .LE. 0) .OR. 
     &        (IW(ind_tbtr) .LE. 0) .OR. 
     &        (IW(ind_tdpl) .LE. 0)) Then
*
            my_level = TRK_LEVEL_hbt
*
*              Now make sure that all of the needed HBT banks are here??
*
            If ( (IW(ind_hbla) .LE. 0) .OR. 
     &           (IW(ind_hbtr) .LE. 0) .OR. 
     &           (IW(ind_hdpl) .LE. 0) ) my_level = 0
*
         Endif
*
      Endif
*
*        Abort on ERROR_CONDITION (Log the first 30).
*
      If ( my_level .LE. 0 ) Then
*
         If ( my_errcnt .LT. 30 ) Then
*
            IF ( my_level .LT. 0 ) Then
               Write(CRMESS,'(A,I8,A)')' Event:',ievnt,
     &                      ': no HBTB or TRKS bank found'
*
            Else
               Write(CRMESS,'(A,I8,A)')' Event:',ievnt,
     &                      ': no TRACKING Banks found'
            Endif
*
            Call RecMes(CRNAME,'W',CRMESS)
            my_errcnt = my_errcnt +1
*
         Endif
*
         my_level = 0
         Goto 999
*
      Endif
*
c  initialize tracking arrays
cc  (is done in  trk_evnt_hbt)
cc      ntr_link = 0
cc      call vzero(nsegmc,36*nclmx)
cc      call vzero(segm,432*nsgmx)
cc      call vzero(nsegm,36)
cc      call vzero(nclust,36)
cc      call vzero(itr_sect,MAX_TRACKS)
cc      call vzero(itr_level,MAX_TRACKS)
cc      call vzero(lnk_clust,6*MAX_TRACKS)
cc      call vzero(lnk_segm,6*MAX_TRACKS)
cc      call vzero(trk,12*npl_dc*MAX_TRACKS)
cc      call vzero(trkp,12*nplane*MAX_TRACKS)

      do it=1,MAX_TRACKS
        do i=1,3
          do j=1,nplane
            trkp(i,j,it) = 1000.		!position not known
          enddo
        enddo
        trk(12,5,it) = 11.
        trk(12,6,it) = 11.
      enddo

      ind = ind_trks +1
      do while ( IW(ind-1).GT.0 )
        ind  = IW(ind-1)
        isec = IW(ind-2)
        ncol = IW(ind-5)
        nd   = IW(ind)-1
        if(nd.GT.0) then
          do j=0,nd,ncol
            id = IW(ind+1+j)
            it = mod(id,100)
            if(id.LE.200 .OR. it.EQ.0 .OR. it.GT.MAX_TRACKS) then
              write(CRMESS,'(A,I8,3(A,I3),I3)') ' Event',ievnt,
     & ' Sector',isec,': TrackId in TRKS not valid :',it,id/100
              call RecMes(CRNAME,'W',CRMESS)
            else
              if(it.GT.ntr_link) ntr_link = it
              itr_sect(it)  = isec
              itr_level(it) = id/100
              trkp(8,1,it)  = RW(ind+2+j)
            endif
          enddo
        endif
      enddo

      call vzero(ndat_dc01,6)
      call vzero(idsec_dc01,6)
*
*        Bank switching based on who we are using:
*
      If ( dc01_flag .eq. 1) Then
         ind = ind_dc01 + 1
      Else
         ind = ind_dc00 + 1
      Endif
*
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

      ind = ind_hbtb +1
      do while ( IW(ind-1).GT.0 )
        ind  = IW(ind-1)
        isec = IW(ind-2)
        ncol = IW(ind-5)
        nd   = IW(ind)-1
        if(nd.GT.0) then
          do j=0,nd,ncol
            it = IW(ind+j+1)
            if(itr_sect(it).NE.isec) then
              write(CRMESS,'(A,I8,2(A,I3),I3)') ' Event',ievnt,
     & ' for track',it,' Sector# in HBTB doesn"t match:',itr_sect(it),isec
              call RecMes(CRNAME,'W',CRMESS)
            endif
            is = IW(ind+j+2)
            icl= IW(ind+j+3)
            isg= IW(ind+j+4)
            if(isg.LT.0) then
              lnk_clust(is,it) = -icl
              lnk_segm(is,it)=IW(ind+j+5)
            else
              if(icl.GT.nclust(is,isec)) nclust(is,isec) = icl
              lnk_clust(is,it) = icl
              if(nsegmc(icl,is,isec).LT.nclmx) then
                nsegmc(icl,is,isec) = nsegmc(icl,is,isec) +1
                clust(nsegmc(icl,is,isec),icl,is,isec) = isg
                nsegm(is,isec) = nsegm(is,isec) +1
                do i=1,6
                  if(is.EQ.1 .AND. i.GT.4) then     !bug in ANA_SEGM ??
                    segm(i*2-1,isg,is,isec)=-IABS(segm(7,isg,is,isec))
                  else
                    id = IW(ind+j+i+4)
                    if(id.GT.0 .AND. id.LE.ndat_dc01(isec)) then
                      if(dc01_flag.EQ.1) then
                        segm(i*2-1,isg,is,isec)= mod(IW(idsec_dc01(isec)+id*2-1),256)
                        segm(i*2,isg,is,isec)  = RW(idsec_dc01(isec)+id*2)
                      else                              ! read DC0 bank:
                        my_id = IW(idsec_dc01(isec)+id)
                        my_ind = my_bid(1)
                        segm(i*2-1,isg,is,isec)= mod(my_ind,256)
                        segm(i*2,isg,is,isec)  = my_bid(2)
                      endif
                    else
                      if(IABS(id).GT.192) then        !bug in ANA_SEGM ??
                        if(i.EQ.1) then
                          id=-1
                        else
                          id=segm(i*2-3,isg,is,isec)
                        endif
                      endif
                      segm(i*2-1,isg,is,isec)= -IABS(id)
                    endif
                  endif
                  if((segm(i*2,isg,is,isec).EQ.0.AND.segm(i*2-1,isg,is,isec).GE.0)
     &     .OR.      IABS(segm(i*2-1,isg,is,isec)).GT.192) then
                    write(CRMESS,'(A,2I6)') ' wire no. out of range {#,tdc}:',
     &                    segm(i*2-1,isg,is,isec),segm(i*2,isg,is,isec)
                    call recmes(crname,'I',crmess)
                    segm(i*2-1,isg,is,isec)=-1
                  endif
                enddo
c                do i=1,12
c                  segm(i,isg,is,isec) = IW(ind+j+i+4)
c                enddo
              endif
            endif
          enddo
        endif
      enddo

      if(my_level.EQ.TRK_LEVEL_tbt) then
        ind1 = IW(ind_tbtr)
        ind2 = ind_tbla +1
        ind3 = ind_tdpl +1
      else
        ind1 = IW(ind_hbtr)
        ind2 = ind_hbla +1
        ind3 = ind_hdpl +1
      endif

      it = 0
      it4= 0
      call vzero(trkno2,MAX_TRACKS)

      ncol = IW(ind1-5)
      nd = IW(ind1)-1
      if(nd.GT.0) then
        do j = 0,nd,ncol
          if(my_level.EQ.TRK_LEVEL_tbt) then
            it = IW(ind1+10+j)
            it4 = it4 +1
            trkno2(it4) = it
          else
            it = it +1
            trkno2(it) = it
          endif
c  Momentum and charge
          trkp(7,1,it)  = SQRT(RW(ind1+4+j)**2+RW(ind1+5+j)**2+RW(ind1+6+j)**2)
          trkp(9,1,it)  = RW(ind1+7+j)
          trkp(11,1,it) = my_level + itr_sect(it)*10
          trkp(12,1,it) = RW(ind1+8+j)         ! Chi2
        enddo
      endif
      if(my_level.EQ.TRK_LEVEL_tbt) then
        ntracks = it4
      else
        ntracks = ntr_link
      endif

      do it = 1,ntr_link
        if(itr_level(it).GE.TRK_LEVEL_hbt) then
          do ipl = 2,nplane
            do i = 7,12
              trkp(i,ipl,it) = trkp(i,1,it)
            enddo
          enddo
        endif
      enddo

*  read TBLA or HBLA bank

      do while ( IW(ind2-1).GT.0 )
        ind2 = IW(ind2-1)
        isec = IW(ind2-2)
        ncol = IW(ind2-5)
        nd   = IW(ind2)-1
        if(nd.GT.0) then
          do j=0,nd,ncol
            it = IW(ind2+1+j)/100
            if(it.LE.0 .OR. it.GT.ntr_link) then
              write(CRMESS,'(A,I8,2(A,I3))') ' Event',ievnt,
     &          ' HBLA/TBLA: Sector#',isec,' Track# not defined',it
              call RecMes(CRNAME,'W',CRMESS)
            else
              it = trkno2(it)				!wrt. TRKS, HBTB banks
              ipl= mod(IW(ind2+1+j),100)
              il = ipl - nst_max
              trkp(2,ipl,it) = RW(ind2+2+j)		!position
              trkp(3,ipl,it) = RW(ind2+3+j)
              trkp(1,ipl,it) = RW(ind2+4+j)
              trkp(5,ipl,it) = RW(ind2+5+j)		!direction
              trkp(6,ipl,it) = RW(ind2+6+j)
              trkp(4,ipl,it) = RW(ind2+7+j)
              trkp(10,ipl,it)= RW(ind2+8+j)		!track length
              trk(1,il,it)  = IW(ind2+11+j)		!wire#
              trk_pt(il,it) = IW(ind2+9+j)		!ptr to DC1
              if(trk_pt(il,it).GT.0 .AND. trk_pt(il,it).LE.ndat_dc01(isec))then
                if(dc01_flag.eq.1) then
                 trk(2,il,it)  = RW(idsec_dc01(isec)+trk_pt(il,it)*2)	!tdc
                else
                 my_id = IW(idsec_dc01(isec)+trk_pt(il,it))	        !tdc
                 my_tdc = my_bid(2)
                 trk(2,il,it)  = my_tdc
                endif
              endif
              trk(3,il,it)  = RW(ind2+12+j)		!drift time
              if(my_level.EQ.TRK_LEVEL_tbt) then
                trk(4,il,it) = RW(ind2+17+j)		!calculated doca
              endif
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

*  read TDPL or HDPL bank

      do while ( IW(ind3-1).GT.0 )
        ind3 = IW(ind3-1)
        isec = IW(ind3-2)
        ncol = IW(ind3-5)
        nd   = IW(ind3)-1
        if(nd.GT.0) then
          do j=0,nd,ncol
            id = IW(ind3+1+j)
            it = id/100
            ipl= mod(id,100)
            if(it.LE.0 .OR. it.GT.ntr_link .OR. ipl.GT.nplane) then
              write(CRMESS,'(A,I8,3(A,I3))') ' Event',ievnt,
     &      ' HBLA/TBLA: Sector#',isec,' Track# not defined',it,'  plane',ipl
              call RecMes(CRNAME,'W',CRMESS)
            else
              it = trkno2(it)				!wrt. TRKS, HBTB banks
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

c   Decreases the number of found tracks by the number of bad links that
c   don't intersect with various DC planes.
c   since the links come from HB, we don't want to decrease the number of
c   final tracks found since they have already been eliminated by the
c   swimming. Is this area still necessary?
c   Also, synthesize hits in the non-existent layers 5 and 6 
      do it=1,ntr_link
        if(itr_level(it).GT.0) then
          if (my_level.NE.TRK_LEVEL_tbt) then
            if(trkp(10,11,it)+trkp(10,17,it)+trkp(10,23,it).EQ.0.) then
              itr_level(it)=0
              ntracks=ntracks-1
            endif
          endif
          trk(1,5,it) = -ABS(trk(1,4,it))
          trk(1,6,it) = trk(1,5,it)
        endif
      enddo

      if(ipar_trk_print(6).GT.0) then
        if(ipar_trk_print(6).GT.1) then
          do it=1,ntr_link
            if(itr_level(it).GT.1) then
              isec=itr_sect(it)
              write(6,*)'lnk_clust for sec/it,',isec,it,(lnk_clust(is,it),is=1,6)
            endif
          enddo
          do isec=1,6
            write(6,*)'nclust in sec',isec,(nclust(is,isec),is=1,6)
            do is = 1,6
              do i=1,nsegm(is,isec)
                write(6,*)'segm',is,isec,(segm(j,i,is,isec),j=1,12)
              enddo
            enddo
          enddo
        endif
        if(ipar_trk_print(6).LT.3) then
          do it=1,ntr_link 
            if(itr_level(it).GT.1) then
              call sda_ptrk(it,itr_sect(it))
            endif
          enddo
        endif
      endif
 
 999  continue
      end






