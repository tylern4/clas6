      SUBROUTINE TAG_FILL(do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fill.F,v 1.27 1999/03/09 17:40:41 boiarino Exp $
c
c  Documentation for subroutine TAG_FILL
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "tag_general.PAR"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------

      logical do_hist,do_tagr_nt,do_tagirf_nt,do_dsd_nt
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.27 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:41 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: tag_fill.F,v 1.27 1999/03/09 17:40:41 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer MLINK,ind
      integer i,j,Nb_good_Hits,size,Number_a_goodh,k
      integer nb_ps_hits,nb_sti_hit,nb_str_hit
      real unbinnedT
      real Tag_Hit(12),PC_hits(12),PS_hits(12,8),TAC_hits(12)
      real tagr_hit(6)
      real T_mean,xoff,yoff
      real sti_id(24),sti_status(24),sti_tdc1(24),sti_tdc2(24),sti_adc1(24),sti_adc2(24)
      real str_sec(10),str_trkNum(10),str_time(10),str_fitlength(10)
      real besttagtime(10)
      logical pc_evnt,ps_evnt,tac_evnt,str_event
      real pc_time,ps_time,tac_time
	integer indRF
	real goodrf,rf1,rf2

      integer tag_unbin,tag_nbc_bin
      integer sc_id(6,20),nb_hbid_hit(6),sector
      integer tid,column,row
       real  sc_time(6,20),sc_qual(6,20),sc_vtime(6,20),sc_beta(6,20)       

c -------- external functions

	real rf_corr_time


c_end_var
c
c  executable code for routine TAG_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling Tagger Histograms & ntuples, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c  First change PAWC directories
      call hcdir('//PAWC/TAG',' ')

c  Do your HFILLS below this line
     

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- get the RF for this event:
    
	indRF = MLINK(iw,'CL01',0)

	if (indRF.ne.0) then
	 size = IW(indRF)
	 if (size.ne.0) then 
	  goodrf = RW(indRF + 10) 
	  rf1    = RW(indRF + 11) 
	  rf2    = RW(indRF + 12)
         endif
	endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  --- get Start counter stuff for this event:

c - - intermediate

       ind = MLINK(iw,'ST1 ',1)
	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/6
	else
	  Nb_good_Hits = 0
	endif

	nb_sti_hit = Nb_good_Hits
        do i=1,Nb_good_Hits
	  j = (i - 1)*6
          sti_id(i)     = IW(ind + j + 1)
          sti_status(i) = IW(ind + j + 2)
          sti_tdc1(i)   = IW(ind + j + 3)
          sti_adc1(i)   = IW(ind + j + 4)
          sti_tdc2(i)   = IW(ind + j + 5)
          sti_adc2(i)   = IW(ind + j + 6)
        enddo

c - - Final

       ind = MLINK(iw,'STR ',1)
	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/4
	else
	  Nb_good_Hits = 0
	endif

	nb_str_hit = Nb_good_Hits
        str_event = (nb_str_hit.gt.0)
        do i=1,Nb_good_Hits
	  j = (i - 1)*4
          str_sec(i)       = IW(ind + j + 1)
          str_trkNum(i)    = IW(ind + j + 2)
          str_time(i)      = RW(ind + j + 3)
          str_fitlength(i) = RW(ind + j + 4) 
        enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  get TGPB bank

       ind = MLINK(iw,'TGPB',0)
	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/4
	else
	  Nb_good_Hits = 0
	endif

        do i=1,Nb_good_Hits
	  j = (i - 1)*4
          call hf1(520,RW(ind + j + 4),1.)
        enddo

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c ---- READ DSD stuff for this event
c

        pc_evnt = .false.
        ps_evnt = .false.
        tac_evnt = .false.

c
c --- PC ---------------
c
	ind = MLINK(iw,PCOData_BOS_name,1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/PCOData_BOS_size
	else
	  Nb_good_Hits = 0
	endif

	if (Nb_good_Hits.ne.0) then ! --- filling DSD ntuple
        pc_evnt = .true.
	do i=1,Nb_good_Hits
	  j = (i - 1)*PCOData_BOS_size
	  PC_hits(1) = 10                ! PC ID = 10
	  PC_hits(2) = RW(ind+j+1)	  ! time
	  PC_hits(3) = RW(ind+j+2)	  ! left top
 	  PC_hits(4) = RW(ind+j+5)	  ! right top!!!!!!!
	  PC_hits(5) = RW(ind+j+4)	  ! left bottom
	  PC_hits(6) = RW(ind+j+3)	  ! right bottom!!!!
	  PC_hits(7) = RW(ind+j+6)	  ! main
	  PC_hits(8) = RW(ind+j+7)	  ! veto
	  PC_hits(9) = 0		  ! dummy
	  PC_hits(10) = float(IW(ind+j+8)) ! T id
          PC_hits(11) = 0
          PC_hits(12) = -999              ! dummy value in case there is no MOR TDC
	enddo
	endif
        
c
c --- TAC ---------------
c
	ind = MLINK(iw,TACOData_BOS_name,1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/TACOData_BOS_size
	else
	  Nb_good_Hits = 0
	endif

	if (Nb_good_Hits.ne.0) then ! --- filling DSD ntuple
        tac_evnt = .true.
	do i=1,Nb_good_Hits
	  j = (i-1)*TACOData_BOS_size
          if (IW(ind+j+1).eq.1) then
             TAC_hits(1) = 9                 ! TAC ID = 9
          else
             TAC_hits(1) = 11                ! USLG ID = 11
          endif
	  TAC_hits(2) = RW(ind+j+2)	  ! time
	  TAC_hits(3) = RW(ind+j+3)	  ! left top
 	  TAC_hits(4) = RW(ind+j+4)	  ! right top
	  TAC_hits(5) = RW(ind+j+5)	  ! left bottom
	  TAC_hits(6) = RW(ind+j+6)	  ! right bottom
	  TAC_hits(7) = RW(ind+j+7)	  ! sum
	  TAC_hits(8) = RW(ind+j+8)	  ! sum1
	  TAC_hits(9) = RW(ind+j+9)	  ! sum2
	  TAC_hits(10) = float(IW(ind+j+10)) ! T id
          TAC_hits(11) = 0
          TAC_hits(12) = -999             ! dummy value in case there is no MOR TDC
	enddo
	endif

c
c --- PS
c
 	ind = MLINK(iw,PSOData_BOS_name,1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_ps_Hits = size/PSOData_BOS_size
	else
	  Nb_ps_Hits = 0
	endif

	if (Nb_ps_Hits.ne.0) then ! --- filling DSD ntuple
	ps_evnt = .true.
        do i=1,Nb_ps_Hits
	  j = (i-1)*PSOData_BOS_size
	  PS_hits(1,i) = float(IW(ind+j+1)) ! ID
	  PS_hits(2,i) = RW(ind+j+2)	  ! time
	  PS_hits(3,i) = RW(ind+j+3)	  ! adc
 	  PS_hits(4,i) = 0		  ! right top
	  PS_hits(5,i) = 0		  ! left bottom
	  PS_hits(6,i) = 0		  ! right bottom
	  PS_hits(7,i) = 0		  ! sum
	  PS_hits(8,i) = 0		  ! sum1
	  PS_hits(9,i) = 0		  ! sum2
	  PS_hits(10,i) = float(IW(ind+j+4)) ! T id
          PS_hits(11,i) = 0
          PS_hits(12,i) = -999              ! dummy value in case there is no MOR TDC
	enddo
        endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  HODOSCOPE analysis :
c       
c

c ----- Connect to the TAGR BOS bank---------------------------------------
c
	ind = MLINK(iw,'TAGR',1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/TagR_BOS_size
	else
	  Nb_good_Hits = 0
	endif

	if (Nb_good_Hits.ne.0) then ! --- begin filling tagger event histo

c
c ----- Number of good hits.
c
	do i=1,Nb_good_Hits
	  j = (i-1)*TagR_BOS_size
	  Tagr_hit(1) = RW(ind+j+1)       ! energy
	  Tagr_hit(2) = RW(ind+j+2)	  ! time
	  Tagr_hit(3) = RW(ind+j+3)	  ! time rf
	  Tagr_hit(4) = float(IW(ind+j+4)) ! status
	  Tagr_hit(5) = float(IW(ind+j+5)) ! T_id
	  Tagr_hit(6) = float(IW(ind+j+6)) ! E_id
	  

          if (do_tagr_nt) call hfn(4,Tagr_hit)

          if (do_hist) then
c
c ----- T alignement to RF
c

             call hf2(300,Tagr_hit(5),Tagr_hit(2)-Tagr_hit(3),1.)
	  	  
c
c ----- RF structure 
c
             call hf1(400,Tagr_hit(2)-Tagr_hit(3),1.)
c
c  ------- Energy pattern
c
             call hf1(10,tagr_hit(1),1.)
c
c  -------- Time pattern
c

             call hf1(11,tagr_hit(3),1.)

c
c  --------- ST - tagger coincidence
c

             do k=1,nb_str_hit
                besttagtime(k) = RW(ind + 3)
                call hf1(500,tagr_hit(3)-str_time(k),1.)
                call hf2(501,tagr_hit(3),str_time(k),1.)
                call hf2(304,Tagr_hit(5),tagr_hit(3)-str_time(k),1.)
                call hf2(305,str_sec(k),tagr_hit(3)-str_time(k),1.)
                call hf2(314,Tagr_hit(5),-str_time(k),1.)
                if (abs(tagr_hit(3)-str_time(k)).lt.abs(besttagtime(k)-str_time(k)))
     +                                                  besttagtime(k) = tagr_hit(3)
             enddo
              
c
c ---------  ST - TOF histograms
c

c            do sector=1,6
c              do j=1,nb_hbid_hit(sector)
c              call hf2(600+sector,sc_id(sector,j),tagr_hit(3) - sc_vtime(sector,j),1.)
c              enddo
c            enddo
           

c
c
c ----- E counter pattern histo fills
c
          call hf1(100,Tagr_hit(6),1.)
c
c ----- T counter patterns  histo fills
c
          call hf1(102,Tagr_hit(5),1.)
          unbinnedT = float(tag_unbin(IW(ind+j+5)))
          call hf1(104,unbinnedT,1.)
          if (tag_nbc_bin(IW(ind+j+5)).eq.2) call hf1(104,unbinnedT+1.,1.)
          if (abs(Tagr_hit(3)).lt.20) then
             call hf1(101,Tagr_hit(5),1.)
             call hf1(103,unbinnedT,1.)
             if (tag_nbc_bin(IW(ind+j+5)).eq.2) call hf1(103,unbinnedT+1.,1.)
          endif
         endif ! (do_hist)
	enddo

c
c--------  Tagger - st histograms for the best tagger hit guess:
c

           do k=1,nb_str_hit
             call hf1(510,besttagtime(k)-str_time(1),1.)
             call hf2(511,besttagtime(k),str_time(1),1.)
           enddo

        endif ! (Nb_good_Hits.ne.0)



c ----- Connect to the TAGI BOS bank ----------------------------------------
c
	ind = MLINK(iw,'TAGI',1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/TagIData_BOS_size
	else
	  Nb_good_Hits = 0
	endif

	if (Nb_good_Hits.ne.0) then ! --- begin filling tagger event histo

c
c ----- Number of good hits.
c
         Number_a_goodh = Nb_good_Hits


	do i=1,Nb_good_Hits
	  j = (i-1)*TagIData_BOS_size
	  Tag_hit(1) = float(IW(ind+j+1)) ! T ID
	  Tag_hit(2) = RW(ind+j+2)	  ! T left
	  Tag_hit(3) = RW(ind+j+3)	  ! T right
	  Tag_hit(4) = float(IW(ind+j+4)) ! E Id
	  Tag_hit(5) = RW(ind+j+5) 	  ! E time
	  Tag_hit(6) = RW(ind+j+6) 	  ! Mean time aligned 
	  Tag_hit(7) = RW(ind+j+7) 	  ! RF correction
	  Tag_hit(8) = goodrf             ! good RF
	  Tag_hit(9) = rf1          	  ! rf1
          Tag_hit(10) = rf2               ! rf2
	  Tag_hit(11) = Nb_good_Hits	  ! Nb_good_Hits
          Tag_hit(12) = RW(ind+j+8)       ! time difference with next T
	  
	  if (IW(ind+j+1).ne.0) then ! this is a real T being hit

            if (do_tagirf_nt) call hfn(1,Tag_hit)
            if (do_hist) then
c
c ----- E-T time coincidence histo fills
c
	      T_mean = (Tag_hit(2)+Tag_hit(3))/2.

              call hf2(200,Tag_hit(4),T_mean - Tag_hit(5),1.)
	  	
c
c  --- T TDC slopes CHECK:
c
              tid = tag_unbin(IW(ind + j + 1))
              row = (tid-1)/8
              column = mod(tid-1,8)
              xoff = 50. + column * 200.

              yoff = 2. + row * 4.
              call hf2(12,xoff + T_mean,yoff + Tag_hit(2)-Tag_hit(3),1.)

              yoff = 1. + row * 2.
              call hf2(13,xoff + T_mean,yoff + Tag_hit(7),1.)

              call hf2(14,rf1,Tag_hit(7),1.)
              call hf2(15,rf2,Tag_hit(7),1.)

            endif
c
c ----- get the T time for the DSD ntuple:
c
            if (pc_evnt.and.PC_HITS(10).eq.TAG_HIT(1)) PC_HITS(11) = TAG_HIT(6)
            if (ps_evnt) then
              do j=1,nb_ps_hits
                 if (PS_HITS(10,j).eq.TAG_HIT(1)) PS_HITS(11,j) = TAG_HIT(6)
              enddo
            endif
            if (tac_evnt.and.TAC_HITS(10).eq.TAG_HIT(1)) TAC_HITS(11) = TAG_HIT(6)


          else ! this is the entry for the MOR TDC
            PC_hits(12) = Tag_hit(6)
            TAC_hits(12) = Tag_hit(6)
            do j=1,nb_ps_hits
              PS_hits(12,j) = Tag_hit(6)
            enddo
            Number_a_goodh = Number_a_goodh - 1
          endif
	enddo

c
c finally how many good hits ?
c
	if (do_hist) call hf1(2,float(Number_a_goodh),1.)

	endif ! ----------- end filling hodoscope histograms

c
c----------------- fill the dsd stuff
c

        if (pc_evnt) then
          if (do_dsd_nt) call hfn(3,PC_hits)
          if (do_hist.and.PC_HITS(10).ne.0) then
            call hf2(301,PC_hits(10),PC_HITS(11) - PC_HITS(2),1.)
            call hf2(311,PC_hits(10),-PC_HITS(2),1.)
          endif
        endif

        if (ps_evnt) then
          do i=1,nb_ps_hits
            if (do_dsd_nt) call hfn(3,PS_hits(1,i))
            if (do_hist.and.PS_HITS(10,i).ne.0) then
              call hf2(302,PS_hits(10,i),PS_HITS(11,i) - PS_HITS(2,i),1.)
              call hf2(312,PS_hits(10,i),-PS_HITS(2,i),1.)
            endif
          enddo
        endif

        if (tac_evnt) then
          if (do_dsd_nt) call hfn(3,TAC_hits)
          if (do_hist.and.TAC_HITS(10).ne.0) then
            call hf2(303,TAC_hits(10),TAC_HITS(11) - TAC_HITS(2),1.)
            call hf2(313,TAC_hits(10),-TAC_HITS(2),1.)
          endif
        endif

c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




