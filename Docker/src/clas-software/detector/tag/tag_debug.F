      SUBROUTINE TAG_DEBUG(levelmin,levelmax)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_debug.F,v 1.13 1998/10/22 21:47:39 fklein Exp $
c
c  Documentation for subroutine TAG_BRUN
c
c  Purpose: TAGGER supplied routined that reads the EVENT.CMN common
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois ROUDOT      Created:  Thu Jun 19 12:00:00 EST 1997
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
c#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: tag_debug.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.13 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 1998/10/22 21:47:39 $')
      PARAMETER (CAUTHO= '$Author: fklein $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TAG_BRUN')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local tag defined variables:
c  ----------------------------
      integer levelmin,levelmax,level,i,j
      real tag_deltae
      character*32 status
c_end_var
c
c  executable code for routine TAG_DEBUG:
c  -------------------------------------
c
        
	level = -1
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'=================================================='
	print *,'=   processing parameters'
	print *,'=================================================='
        print *,'Adjacent T time coincidence window :',ADJ_T_COINC
        print *,'Adjacent E time coincidence window :',ADJ_E_COINC
        print *,'E/ T time coincidence window       :',ET_window
        print *,'E TDC minimum accepted value       :',E_TDC_MIN
        print *,'E TDC maximum accepted value       :',E_TDC_MAX
        print *,'T- DSD time coincidence window     :',TAG_DSD_window
        print *,'PS paddles time coincidence window :',PS_COINC_WINDOW
	endif


	level = level + 1	! 1
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'=================================================='
	print *,'=   T OUTPUT OF BOS BANKS'
	print *,'=================================================='
	print *,'TAGT : ',Tag_T_NbHits,' entries'
	do i=1,Tag_T_NbHits
	print *,'T',tc_id(i),' : left=',tc_val_left(i),' / right = ',tc_val_right(i)
	enddo
	print *,'MORTDC : ',MOR_tdc


	print *,'=================================================='
	print *,'=   SORTED E OUTPUT OF BOS BANKS '
	print *,'=================================================='
        print *,Tag_E_NbHits,' entries'
	do i=1,Tag_E_NbHits
	print *,'E',ec_id(i),' : ',ec_val(i)
	enddo
	endif
	
c
c -------------------------
c
	level = level + 1	! 1
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==============================================='
	print *,'= FILTERED DATA ; ============================='
	print *,'==============================================='
	print *,'TC : ',NbGoodT,' entries'
	do i=1,NbGoodT
	print *,'T',Good_T_Id(i),' : ',Good_T_L_val(i),' - ',Good_T_R_val(i)
	enddo
	print *,'..........'
	print *,'EC : ',NbGoodE,' entries'
	do i=1,NbGoodE
	print *,'E',Good_E_Id(i),' : ',Good_E_val(i)
	enddo
	endif

c
c -------------------------
c
	level = level + 1	! 2
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==================================================='
	print *,'=   Matched hits    ==============================='
	print *,Nb_matched_Hits,' matched hits involving ',Nb_matched_Ts,' T counters'
	do i=1,Nb_matched_Ts
	  print *,'T',Matched_T_Id(i),'(',Matched_T_L_val(i),'/',Matched_T_R_Val(i),')'
	do j=1,NB_matched_Es(i)
	  print *,'     <- E',Matched_E_Id(i,j),' (',Matched_E_val(i,j),')'
	enddo
	enddo
	endif


c
c -------------------------
c
	level = level + 1	! 3
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==================================================='
	print *,'=   Time coincidence match   ======================'
	print *,Nb_coinc_Hits,' coincidences involving ',Nb_coinc_Ts,' T counters'
	do i=1,Nb_coinc_Ts
	  print *,'T',coinc_T_Id(i),'(',coinc_T_L_val(i),'/',coinc_T_R_Val(i),')'
	do j=1,NB_coinc_Es(i)
	  print *,'     <- E',coinc_E_Id(i,j),' (',coinc_E_val(i,j),')'
	enddo
	enddo
	endif
	
c
c -------------------------
c
	level = level + 1	! 4
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==================================================='
	print *,'=   binned hits    ==============================='
	print *,Nb_bin_Hits,' possible hits involving ',Nb_bin_Ts,' T bins'
	do i=1,Nb_bin_Ts
	if ((bin_T_Id(i)/2)*2.ne.bin_T_Id(i)) then
	  print *,'T bin',bin_T_Id(i),'(',bin_T_L_val(i,1),'/',bin_T_R_Val(i,1),')'
	else
	  print *,'T bin',bin_T_Id(i),'(',bin_T_L_val(i,1),'/',bin_T_R_Val(i,1),')',
     +		'(',bin_T_L_val(i,2),'/',bin_T_R_Val(i,2),')'
	endif
	do j=1,NB_bin_Es(i)
	if ((bin_E_Id(i,j)/2)*2.ne.bin_E_Id(i,j)) then
	  print *,'     <- E bin',bin_E_Id(i,j),' (',bin_E_val(i,j,1),')'
	else
	  print *,'     <- E bin',bin_E_Id(i,j),' (',bin_E_val(i,j,1),')',' (',bin_E_val(i,j,2),')'
	endif
	enddo
	enddo
	endif

c
c -------------------------
c
	level = level + 1	! 5
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==================================================='
	print *,'=   RF correction            ======================'
	print *,Nb_bin_Hits,' binned hits (E/T) involving ',Nb_bin_Ts,' T bins'
	do i=1,Nb_bin_Ts
	  print *,'T bin',bin_T_Id(i),'(L/R mean time : ',mean_T_val(i,1),'/L/R diff time :',Diff_T_Val(i,1),
     +                  '/ RF - T diff :',RF_T_diff(i,1),'/ Corrected time:',T_time(i,1)
	if ((bin_T_Id(i)/2)*2.eq.bin_T_Id(i)) then
	  print *,'second T of bin',bin_T_Id(i),'(L/R mean time : ',mean_T_val(i,2),'/L/R diff time :',Diff_T_Val(i,2),
     +                  '/ RF - T diff :',RF_T_diff(i,2),'/ Corrected time:',T_time(i,2),')'
	endif
	enddo
	endif

c
c -------------------------
c
	level = level + 1	! 6
	if (level.ge.levelmin.and.level.le.levelmax) then
	print *,'==================================================='
	print *,'=   TAGR                     ======================'
	print *,Nb_TAGR_hits,' hits '
	do i=1,Nb_TAGR_hits
	print *,'Photon Energy            :',photonEnergy(i)
	if (TAGR_EID(i).ne.0) print *,'Delta Photon Energy      :',tag_deltaE(TAGR_EID(i))
	print *,'Time before correction   :',TimeEvent(i)
	print *,'Time after RF correction :',TimeEventCor(i)
        print *,'E bin                    :',TAGR_EID(i)
        print *,'T bin                    :',TAGR_TID(i)

        status = '....    ....    ....    ....    '
        if (IAND(HitStatus(i),1).ne.0) 	status(1:4) = 'LEFT'
        if (IAND(HitStatus(i),2).ne.0)	status(5:8) = 'RIGH'
        if (IAND(HitStatus(i),4).ne.0) 	status(9:12) = ' EC '
        if (IAND(HitStatus(i),8).ne.0) status(13:16) = 'MULH'
        if (IAND(HitStatus(i),16).ne.0) status(17:20) = 'AdjT '
        if (IAND(HitStatus(i),32).ne.0) status(21:24) = 'AdjE '
        if (IAND(HitStatus(i),64).ne.0) status(25:28) = 'MulE'
        if (IAND(HitStatus(i),256).ne.0) status(29:32) = 'OVFL'
	print *,' hit status : ',HitStatus(i),' - ',status,'<'
	enddo
	endif
	
	RETURN
	END
c
c------------------------------------------------------------------------------




