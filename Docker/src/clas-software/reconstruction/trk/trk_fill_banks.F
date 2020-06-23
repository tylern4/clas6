      SUBROUTINE trk_fill_banks(Level,ntrk,trk_in_sect)
*     -------------------------
*_begin_doc
*  RCS ID string
*  $Id: trk_fill_banks.F,v 1.16 2004/06/28 12:40:44 fklein Exp $
*
*  Documentation for subroutine TRK_FILL_BANKS
*
*  Purpose:  fill the result BOS banks of Hit Based, Time Based Tracking
*  --------                    
*                         
*  Inputs Parameters: - LEVEL   tracking analysis level:
*                             =2: Hit Based Tracking
*                             =4: Time Based Tracking
*                     - NTRK    total number of track
*                     - TRK_IN_SECT  track# in the Sector based Output banks
*                       (cf.  TRK_FILL_BANKS_PER_SECT )
*
*  Outputs Parameters: None
*  -------------------
*
*  calling routines: ana_prfit, ana_fit
*  -----------------
*
*  Notes:  This routine is valid until tracking is working with BOS workbanks
*  ------
*
*  Author:        Franz Klein  (Nov.1997)
*  -------         
*
*  Major revisions: 
*  ---------------- 
*     fill HBTR & TBTR banks from HBER and TBER respectively.
*  Minor revisions: 
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
#include "anapatt.inc"
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      integer  Level, ntrk, trk_in_sect(MAX_TRACKS)
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_fill_banks.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.16 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/06/28 12:40:44 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_fill_banks.F,v 1.16 2004/06/28 12:40:44 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_FILL_BANKS')
      PARAMETER (CRAUTH='Franz Klein')
*
*  Local User defined variables:
*  -----------------------------
      INTEGER  MBANK, MAMIND
      REAL  evout(9)
      INTEGER  ind1, ind2, ind3, ind, inde, nrow, ncol
      INTEGER  it, i, j, k, isum, nhits(0:5), ip
      INTEGER  nhbt, nof_trk, trkno(MAX_TRACKS)
      INTEGER  iflg_hbt(MAX_TRACKS), iflg_tbt(MAX_TRACKS),iflg1,iflg2
      INTEGER  iflg_fit(MAX_TRACKS)
      INTEGER  layer, LAYINFO1, LAYINFO2, hbtrn
      INTEGER  ncol_hbtr, ncol_tbtr, ncol_err, ncol_trl1
      DATA  ncol_hbtr /9/, ncol_tbtr /10/, ncol_err /23/, ncol_trl1/10/
      INTEGER  indHBER, indTRL1
      DATA  indHBER/0/, indTRL1/0/


c Count the number of tracks that will be stored
      
      nof_trk = 0
      do it = 1, ntrk
        if(itr_level(it).GE.Level) then
          nof_trk = nof_trk +1
          trkno(nof_trk) = it
        endif
      enddo

      nhbt=0
      if(nof_trk.EQ.0) goto 999

c  hbt/tbt info for TRL1:
        do it=1,ntrk
           iflg_hbt(it)=0
           iflg_tbt(it)=0
        enddo

*  Create the banks wrt. to Tracking Level 
*                   'name',rec_no,ncol,nrow

      if(Level.EQ.TRK_LEVEL_hbt) then

        ind1 = MBANK(IW,'HBTR',    0,ncol_hbtr, nof_trk)
        ind2 = MBANK(IW,'HBER',    0,ncol_err, nof_trk)
        ind3 = MBANK(IW,'TRL1',    0,ncol_trl1, nof_trk)
        if(ind1.EQ.0 .OR. ind2.EQ.0 .OR. ind3.EQ.0) then
          write(CRMESS,'(A)')'Error Creating HBTR & HBER banks'
          call RecMes(CRNAME,'W',CRMESS)
          goto 999
        endif

      elseif(Level.EQ.TRK_LEVEL_tbt) then

c  get hbt info for TRL1:

c  first skip double tracks of remaining HBT tracks 
c   that didn't make it through TBT (output=nhbt)
        call trk_delete(nhbt,ntrk,TRK_LEVEL_HBT)

c  get layerinfo from HBER:
        ind=0
        if(indTRL1.EQ.0) indTRL1=MAMIND(iw,'TRL1')
        if(indHBER.EQ.0) indHBER=MAMIND(iw,'HBER')
        if(indTRL1.GT.0) then
           ind=iw(indTRL1)
           if(ind.GT.0) then
c  was TRL1 built from hit based tracks (hits_tbt<=0)? otherwise drop old stuff
              if(iw(ind+3).GT.0) then
                 call bdrop(iw,'TRL1')
                 ind=0
              else
                 ncol=iw(ind-5)
                 nrow=iw(ind-4)
c  get hbt info from TRL1 bank
                 do it=1,ntrk
                    if(it.le.nrow) then
                       iflg_fit(it)=iw(ind+ncol*(it-1)+1)
                       iflg_hbt(it)=iw(ind+ncol*(it-1)+2)
                    endif
                 enddo
                 call bdrop(iw,'TRL1')
              endif
           endif
        endif

c  else get the info from HBER
        if(ind.EQ.0 .AND. indHBER.GT.0) then
          ind =iw(indHBER)
          if(ind.GT.0) then
            ncol=iw(ind-5)
            nrow=iw(ind-4)
            do it=1,ntrk
              if(it.le.nrow .and. itr_level(it).GE.TRK_LEVEL_HBT) then
                 iflg1=iw(ind+ncol*(it-1)+22)
                 iflg2=iw(ind+ncol*(it-1)+23)
                 do j=0,4,1
                    nhits(j)=0
                    do i=j*6,j*6+5,1
                       if(BTEST(iflg1,i+j)) nhits(j)=nhits(j)+1
                    enddo
                 enddo
                 nhits(5)=0
                 do i=0,5,1
                    if(BTEST(iflg2,i)) nhits(5)=nhits(5)+1
                 enddo
                 isum=0
                 do j=0,5,1
                    iflg_hbt(it)=iflg_hbt(it)+nhits(j)*10**j
                    if(nhits(j).gt.0) isum=isum+1
                 enddo
                 if(isum.eq.6) iflg_hbt(it)=iflg_hbt(it)+10**6
              endif
            enddo
          endif
        endif

        ind1 = MBANK(IW,'TBTR',    0,ncol_tbtr,nof_trk)
        ind2 = MBANK(IW,'TBER',    0,ncol_err, nof_trk)
        ind3 = MBANK(IW,'TRL1',    0,ncol_trl1, nof_trk)
        if(ind1.EQ.0 .OR. ind2.EQ.0 .OR. ind3.EQ.0) then
          write(CRMESS,'(A)')'Error Creating TBTR & TBER & TRL1 banks'
          call RecMes(CRNAME,'W',CRMESS)
          goto 999
        endif

      else
        goto 999
      endif

*  Save Covariance Matrix of Hit Based or Time Based tracking
*   {c11,c12,c13,c14,c15,c22,c23,c24,c25,c33,c34,c35,c44,c45,c55}

      do it = 1,nof_trk
        ind = ind2 + (it-1)*ncol_err
        do j = 1,ncol_err-3
          RW(ind+j) = trk_err(j,trkno(it))
        enddo
      enddo

      do it = 1,nof_trk
        inde = ind2 + (it-1)*ncol_err	  
	RW(inde+21) = trkp(12,1,trkno(it))      ! Chi2
        LAYINFO1=0
        LAYINFO2=0
        do j=0,5,1
           nhits(j)=0
        enddo
        do layer = 0 , 29
          if(trk(12,layer+1,trkno(it)).EQ.0) then
             LAYINFO1 = LAYINFO1+2**layer
             nhits(layer/6)=nhits(layer/6)+1
          endif
        enddo
        do layer = 31 , 36
          if(trk(12,layer,trkno(it)).EQ.0) then
             LAYINFO2 = LAYINFO2+2**(layer-31)
             nhits(5)=nhits(5)+1
          endif
        enddo
        hbtrn=0
        isum=0
        if(Level.EQ.TRK_LEVEL_tbt) then
          hbtrn = trkno(it)  ! This is a track number in HB.
          do j=0,5,1
             iflg_tbt(it)=iflg_tbt(it)+nhits(j)*10**j
             if(nhits(j).gt.0) isum=isum+1
          enddo
          if(isum.eq.6) iflg_tbt(it)=iflg_tbt(it)+10**6
       else
          do j=0,5,1
             iflg_hbt(trkno(it))=iflg_hbt(trkno(it))+nhits(j)*10**j
             if(nhits(j).gt.0) isum=isum+1
          enddo
          if(isum.eq.6) iflg_hbt(trkno(it))=iflg_hbt(trkno(it))+10**6
        endif
        LAYINFO2 = LAYINFO2+256*trk_in_sect(trkno(it))+256**2*hbtrn
        LAYINFO2 = LAYINFO2+256**3*itr_sect(trkno(it))
        IW(inde+22) = LAYINFO1
        IW(inde+23) = LAYINFO2
      enddo

c  we fill HBTR and TBTR here
c      if(Level.EQ.TRK_LEVEL_hbt) call make_hbtr() 
cStep
c In case starting point for the fit is not a true vertex **ER to **BR is not working.

      do it = 1,nof_trk
        if(Level.EQ.TRK_LEVEL_hbt) then
          ind = ind1 + (it-1)*ncol_hbtr		! output of Time Based Tracking
        else
          ind = ind1 + (it-1)*ncol_tbtr		! output of Time Based Tracking
        endif
* As the Coordinate System used in the Tracking
* is not oriented with beamline as z axis,
* we have to rotate : x_b --> z
*                     y_b --> x
*                     z_b --> y
        CALL sda_rotsd(evout,trkp(1,1,trkno(it)),itr_sect(trkno(it)))
c
        do j = 1,3
          RW(ind+j) = evout(j)			!Startposition {X,Y,Z}
        enddo
        do j = 4,6
          RW(ind+j) = evout(j)*evout(7) 	! Momentum
        enddo
        RW(ind+7) = evout(9)			! Charge
        RW(ind+8) = trkp(12,1,trkno(it))        ! Chi2
        IW(ind+9) = trk_in_sect(trkno(it))+itr_sect(trkno(it))*100
        if(Level.EQ.TRK_LEVEL_tbt) then
          IW(ind+10) = trkno(it)  ! This is a track number in HB.
        endif

c  fill TRL1 bank: flags, position/direction on DC Layer 1
        ind = ind3 + (it-1)*ncol_trl1
c  keep simply the tcl parameters for the moment (fk 3.99)
        ip=1 +3
        if(BTEST(ipar_trk_fitregion,2)) ip=24 +3
        if(BTEST(ipar_trk_fitregion,1)) ip=12 +3
        if(BTEST(ipar_trk_fitregion,0)) ip= 1 +3
        if(BTEST(ipar_trk_fitregion,3)) ip= 7 +3
        IW(ind+1) = iabs(ipar_trk_vxconstr) + 256*(ipar_trk_fitregion+256*(ip-4))
        IW(ind+2) = iflg_hbt(trkno(it))
        IW(ind+3) = iflg_tbt(it)
        CALL sda_rotsd(evout,trkp(1,ip,trkno(it)),itr_sect(trkno(it)))
        do j = 1,6
          RW(ind+j+3) = evout(j)			!pos/dir in DC layer 1
        enddo
        RW(ind+10) = trkp(10,ip,trkno(it))
      enddo

c  add TRL1 bank for tracks that didn't make it through TBT:
c      flags, position/direction on DC Layer 1
      if(Level.EQ.TRK_LEVEL_tbt .AND. nhbt.GT.0) then
      ind3 = MBANK(IW,'TRL1',    1,ncol_trl1, nhbt)
        do it=1,ntrk
          if(itr_level(it).EQ.TRK_LEVEL_hbt) then
            ind = ind3 + (it-1)*ncol_trl1
            IW(ind+1) = iflg_fit(it)
            IW(ind+2) = iflg_hbt(it)
            IW(ind+3) = -it
            ip=iflg_fit(it)/2**16 +3
            CALL sda_rotsd(evout,trkp(1,ip,it),itr_sect(it))
            do j = 1,6
               RW(ind+j+3) = evout(j)       !pos/dir in DC layer 1
            enddo
            RW(ind+10) = trkp(10,ip,it)
          endif
        enddo
      endif

 999  continue
      end
