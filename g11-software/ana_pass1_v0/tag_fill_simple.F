      SUBROUTINE TAG_FILL_SIMPLE
c
c_begin_doc
c  RCS ID string
c  $Id: tag_fill_simple.F,v 1.2 1999/03/09 17:40:41 boiarino Exp $
c
c  Documentation for subroutine TAG_FILL_SIMPLE
c
c  Purpose: simplified Tagger control histograms routine to be used with
c  --------  recsis on-line and cooking scripts.
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
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: tag_fill_simple.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:41 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: tag_fill_simple.F,v 1.2 1999/03/09 17:40:41 boiarino Exp $'   
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
      integer i,j,Nb_good_Hits,size,k
      integer nb_ps_hits,nb_str_hit
      real unbinnedT
      real Tag_Hit(12),PC_hits(12),PS_hits(12,8)
      real tagr_hit(6)
      real T_mean,xoff,yoff
      real str_sec(10),str_trkNum(10),str_time(10),str_fitlength(10)
      logical pc_evnt,ps_evnt,str_event
	integer indRF
	real goodrf,rf1,rf2

      integer tag_unbin,tag_nbc_bin
      integer tid,column,row

c -------- external functions

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
    
	indRF = MLINK(iw,'RF  ',0)

	if (indRF.ne.0) then
	 size = IW(indRF)
	 if (size.ne.0) then 
	  goodrf = RW(indRF + 1)
	  rf1    = RW(indRF + 2)
	  rf2    = RW(indRF + 3)
         endif
	endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  --- get Start counter stuff for this event:

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
c ---- READ DSD stuff for this event
c

        pc_evnt = .false.
        ps_evnt = .false.

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
	  PC_hits(2) = RW(ind+j+1)	  ! time
	  PC_hits(10) = float(IW(ind+j+8)) ! T id
          PC_hits(11) = 0     ! will be T id from TAGI
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
	  PS_hits(2,i) = RW(ind+j+2)	  ! time
	  PS_hits(10,i) = float(IW(ind+j+4)) ! T id
          PS_hits(11,i) = 0 ! willl be T time from TAGI
	enddo
        endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  HODOSCOPE analysis :

c ----- Connect to the TAGR BOS bank---------------------------------------
c
	ind = MLINK(iw,'TAGR',1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/TagR_BOS_size
	else
	  Nb_good_Hits = 0
	endif

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
	  
c ----- T alignement to RF

	  call hf2(300,Tagr_hit(5),Tagr_hit(2)-Tagr_hit(3),1.)
	  	  
c ----- RF structure 

	  call hf1(400,Tagr_hit(2)-Tagr_hit(3),1.)

c  ------- Energy pattern

          call hf1(10,tagr_hit(1),1.)

c  -------- Time pattern

          call hf1(11,tagr_hit(3),1.)

c  --------- ST - tagger alignement

          do k=1,nb_str_hit
             call hf2(304,Tagr_hit(5),tagr_hit(3)-str_time(k),1.)
             call hf2(314,Tagr_hit(5),-str_time(k),1.)
          enddo

c ----- E counter pattern 

          call hf1(100,Tagr_hit(6),1.)
c
c ----- T counter patterns 
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
	enddo

	call hf1(2,float(Nb_good_Hits),1.)

c ----- Connect to the TAGI BOS bank ----------------------------------------

	ind = MLINK(iw,'TAGI',1)

	if (ind.ne.0) then
	  size = IW(ind)
	  Nb_good_Hits = size/TagIData_BOS_size
	else
	  Nb_good_Hits = 0
	endif

	if (Nb_good_Hits.ne.0) then ! --- begin filling tagger event histo

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


c ----- E-T time coincidence histo fills

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
c
c ----- get the T time for the DSD ntuple:
c
            if (pc_evnt.and.PC_HITS(10).eq.TAG_HIT(1)) PC_HITS(11) = TAG_HIT(6)
            if (ps_evnt) then
              do j=1,nb_ps_hits
                 if (PS_HITS(10,j).eq.TAG_HIT(1)) PS_HITS(11,j) = TAG_HIT(6)
              enddo
            endif

          endif
	enddo


	endif ! ----------- end filling hodoscope histograms

c----------------- fill the dsd stuff


        if (pc_evnt) then
          if (PC_HITS(10).ne.0) then
            call hf2(301,PC_hits(10),PC_HITS(11) - PC_HITS(2),1.)
            call hf2(311,PC_hits(10),-PC_HITS(2),1.)
          endif
        endif

        if (ps_evnt) then
          do i=1,nb_ps_hits
            if (PS_HITS(10,i).ne.0) then
              call hf2(302,PS_hits(10,i),PS_HITS(11,i) - PS_HITS(2,i),1.)
              call hf2(312,PS_hits(10,i),-PS_HITS(2,i),1.)
            endif
          enddo
        endif

c  return to the top directory and exit

      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




