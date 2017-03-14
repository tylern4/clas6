        subroutine tag_read_from_BOS(OK,
     +     TAGT_bank,
     +     TAGE_bank,
     +     TAGT_event,
     +     TAGE_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_from_BOS.F,v 1.30 2006/03/30 02:41:23 pasyuk Exp $
c
c  Documentation for subroutine Stread_from_bos
c
c  Purpose: 
c  --------
c
c       Reads raw data from TAGE & TAGT BOS bank and fills the StFastbus_data
c       common.
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
c  Author:          Created:  Sat jun 11 14:07:15 EST 1997
c  -------
c
c  Major revisions:
c  ----------------    
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
* 
c BOS common block  uncomment the next line for BOS include file
*      
#include "bcs.inc"
#include "tag_general.PAR"
#include "event.CMN"
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
#include "tagRunControl.CMN"
#include "tagcal.CMN"
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
*
*-OUTPUT
      LOGICAL OK                !success or failure
      LOGICAL TAGT_bank,TAGE_bank ! are the bank existing ?
      LOGICAL TAGT_event,TAGE_event ! Is there at least one event there ?

      LOGICAL TGTL_bank, TGTR_bank
      LOGICAL TGTL_event, TGTR_event
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
c      DATA CRCSID/   
c     1'$Id: tag_read_from_BOS.F,v 1.30 2006/03/30 02:41:23 pasyuk Exp $'   
c     2/   

c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_read_from_BOS')
      PARAMETER (CRAUTH='Thierry Auger ')
c
c  Local User defined variables:
c  -----------------------------
c        
      integer NWRITE
      integer NGETTAGEMODE,TAGEMODE ! TAGE double edges crap
      integer size,size4
        
      integer idE,idT 
      integer idTL, idTR
      integer i,j,k,TAGTbank_nbhits
      integer doubleedges,bascule,ec_idpred ! TAGE double edges crap
      integer ec_idtest
      integer MLINK,MAMIND

      integer idtest
      integer idtest1
      integer irun

      integer dt_corr

      common/tagrun/irun
c_end_var
c
c
c  executable code for routine tag_read_from_BOS:
c----6----------------------------------------------------------------72
c
     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         crmess = 
     +        'Reading for the first time Tagger BOS bank.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
           
      
c
c     -------- initialisation
      
      Tag_t_nbhits = 0
      Tag_E_NbHits = 0
      Tgtl_NBHits = 0
      Tgtr_NBHits = 0
      
      TAGT_bank = .false.
      TAGT_event = .false.
      TAGE_event = .false.
      TAGE_bank = .false.
      TGTL_bank = .false.
      TGTR_bank = .false.

      TGTL_event = .false.
      TGTR_event = .false.
    
      MOR_tdc = -1              ! by convention MOR_TDC<0 <=> no hit in the MOR TDC.
      
      if(use_tgtlr .eq. 0) then ! old TDCs in TAGT
      
c     
c     -------- deal with TAGT bank
      

      idT=MAMIND(iw,TagTData_BOS_name)
      idT = IW(idT)

      OK = (idT.gt.0)
      if (.not.OK) then
c
c  The data have a wrong format this is not OK at all !!
c
         wtagta = wtagta + 1
         if (mod(wtagta-1,Tagger_warning).eq.0) then
            write(crmess,1) TagTData_BOS_name,wtagta
 1          format (a4,'bos Bank corrupted or absent, occured ',
     +           I5,' times')
            CALL RECMES(CRNAME,'I',CRMESS)
         endif
      else
         TAGT_bank=.true.
c     
c     Deal with TAGT bank
c     
         size4=IW(idT)          !size in [I*4] words
         size= 2*size4          !size in [I*2] words
*     
*     -BOS only "knows" fullwords; odd number of half words were rounded up
*     -(the remaining halfword is wasted)
*     
         IF(size.eq.0) then
            wtagte = wtagte + 1
            if (mod(wtagte-1,Tagger_warning).eq.0) then
               write(crmess,2) TagTData_BOS_name,wtagte
 2             format ('empty bank : ',a4,' occured ',I5,' times')
               CALL RECMES(CRNAME,'I',CRMESS)
            endif
            TAGT_event=.false.
         ELSE  !<---- if(size)
            IF( MOD(size-1,TagTData_BOS_size).EQ.0 ) THEN
               size= size-1     !BOS was forced to round up
            ENDIF
            OK= size.GE.TagTData_BOS_size .and. MOD(size,TagTData_BOS_size).EQ.0
            IF(.NOT.OK) THEN
               wtagtm = wtagtm + 1
               if (mod(wtagtm-1,Tagger_warning).eq.0) then
                  write(crmess,3) TagTData_BOS_name,wtagtm
 3                format ('malformed bank : ',a4,' occured ',I5,' times')
                  CALL RECMES(CRNAME,'I',CRMESS)
               endif
            ELSE
               TAGT_event=.true. ! YES we are happy !!
            ENDIF
            
            IF (TAGT_event) then	
c     
c     fill the common TCevent
c     
               TAGTbank_nbhits = size/TagTData_BOS_size
               Tag_t_nbhits = 0 ! <----- in the common TCevent
               do i=1,TAGTbank_nbhits
                  j=(i-1)*TagTData_BOS_size
                  if (iw16(2*idT+j+1).le.0.or.iw16(2*idT+j+1).gt.62) then
                     wtid = wtid + 1
                     if (mod(wtid-1,Tagger_warning).eq.0) then
                        write(crmess,4) iw16(2*idT+j+1),wtid
 4                      format ('T counter ID out of range ',I4,
     +                       ': hit bypassed - occured ',I5,' times')
                        CALL RECMES(CRNAME,'W',CRMESS)
                     endif
                  else          ! good T id
                     if (iw16(2*idT+j+1).eq.62) then
                        MOR_tdc = iw16(2*idT+j+2)
                     else       ! the value is between 1 and 61
                        Tag_t_nbhits = Tag_t_nbhits + 1 ! <----- in the common TCeven
                        tc_id(i)=iw16(2*idT+j+1) ! <----- in the common TCevent
                        tc_val_left(i)=iw16(2*idT+j+2) ! <----- in the common TCevent
                        tc_val_right(i)=iw16(2*idT+j+3) ! <----- in the common TCevent
                     endif
                  endif
               enddo
            ENDIF               ! (TAGT_event)
         ENDIF                  ! (size.ne.0)
      endif                     ! (ok)
c
c-------------------------------------------------------------------------------------------
c      
      else                      ! new pipeline TDCs in TGTL/TGTR banks

         call tc_shift          ! prepare correction if required

c------------ begin TGTL ----------------         
         idTL=MAMIND(iw,TGTL_BOS_name)
         idTL = IW(idTL)
         OK = (idTL.gt.0)
         if(.not.OK) then
c     
c     -- The format is wrong, this is not good
c     
            wtgtla = wtgtla + 1
            if (mod(wtgtla-1,Tagger_warning).eq.0) then
               write(crmess,15) TGTL_BOS_name,wtgtla
 15            format (a4,'bos bank corrupted or absent occured ',
     +              I5,' times')
               CALL RECMES(CRNAME,'W',CRMESS)
            endif
         else                   !OK
            TGTL_bank=.true.    ! we have TGTL bank
            
            size4=IW(idTL)      !size in [I*4] words
            size= 2*size4       !size in [I*2] words
            
            if(size.eq.0) then
               wtgtle = wtgtle + 1
               if (mod(wtgtle-1,Tagger_warning).eq.0) then
                  write(crmess,16) TGTL_BOS_name,wtgtle
 16               format ('empty bank : ',a4,' occured ',I5,' times')
                  CALL RECMES(CRNAME,'W',CRMESS)
               endif
            ELSE                !<--- size.ne.0 	
               IF( MOD(size-1,TGTL_BOS_size).EQ.0 ) THEN
                  size= size-1  !BOS was forced to round up
               ENDIF
               
               OK= size.GE.TGTL_BOS_size .and. MOD(size,TGTL_BOS_size).EQ.0
               IF(.NOT.OK) THEN
                  wtgtlm = wtgtlm + 1
                  if (mod(wtgtlm-1,Tagger_warning).eq.0) then
                     write(crmess,17) TGTL_BOS_name,wtgtlm
 17                  format ('malformed bank : ',a4,
     +                    ' occured ',I5,' times')
                     CALL RECMES(CRNAME,'W',CRMESS)
                  endif
               ELSE
c     
c     Now we know we have hits in TGTL:
c     
                  TGTL_event=.true.
                  Tgtl_NbHits= size/TGTL_BOS_size
                  
                  if (Tgtl_Nbhits.gt.(MAXTC)) then
                     wtgtlo = wtgtlo + 1
                     if (mod(wtgtlo-1,Tagger_warning).eq.0) then
                        write(crmess,18) Tgtl_NbHits,MAXTC,wtgtlo
 18                     format (I4,
     +                       ' hits in TGTL, more than we can eat! ',
     +                       I4,' occured ',I5,' times')
                        CALL RECMES(CRNAME,'W',CRMESS)
                     endif
                     Tgtl_NbHits = MAXTC
                  endif
                  
c     --- get hits from TGTL bank
                  
                  k = 0
                  do i=1,Tgtl_NbHits
                     j=(i-1)*TGTL_BOS_size
                     idtest = iw16(2*idTL+j+1)
                     if (idtest .le. 0 .or. idtest .gt. 63) then
                        wtgtlid = wtgtlid + 1
                     else if(idtest .lt.62) then
                        k = k + 1
                        itc_id_left(k) = idtest ! <----- in the common TgtLraw
                        itc_val_left(k) = iw16(2*idTL+j+2) ! <----- in the common TgtLraw
                        dt_corr = 0
                        if(Tagger_correct.ne.0) dt_corr=tl_shift(idtest)
                        itc_val_left(k) = itc_val_left(k) - dt_corr ! shift correction
                     endif
                  enddo
               ENDIF            !<---- enf if(size)             
               Tgtl_NbHits = k     
               call tag_sort_array(itc_id_left,itc_val_left,Tgtl_NbHits)
            endif               !<----------- if(.not.OK)
         endif                  !<--- if(.not.OK)
c-------------end TGTL -------------------         
c-------------begin TGTR ----------------
         idTR=MAMIND(iw,TGTR_BOS_name)
         idTR = IW(idTR)
         OK = (idTR.gt.0)
         if(.not.OK) then
c     
c     -- The format is wrong, this is not good
c     
            wtgtra = wtgtra + 1
            if (mod(wtgtra-1,Tagger_warning).eq.0) then
               write(crmess,25) TGTR_BOS_name,wtgtra
 25            format (a4,'bos bank corrupted or absent occured ',
     +              I5,' times')
               CALL RECMES(CRNAME,'W',CRMESS)
            endif
         else                   !OK
            TGTR_bank=.true.    ! we have TGTR bank
            
            size4=IW(idTR)      !size in [I*4] words
            size= 2*size4       !size in [I*2] words
            
            if(size.eq.0) then
               wtgtre = wtgtre + 1
               if (mod(wtgtre-1,Tagger_warning).eq.0) then
                  write(crmess,26) TGTR_BOS_name,wtgtre
 26               format ('empty bank : ',a4,' occured ',I5,' times')
                  CALL RECMES(CRNAME,'W',CRMESS)
               endif
            ELSE                !<--- size.ne.0 	
               IF( MOD(size-1,TGTR_BOS_size).EQ.0 ) THEN
                  size= size-1  !BOS was forced to round up
               ENDIF
               
               OK= size.GE.TGTR_BOS_size .and. MOD(size,TGTR_BOS_size).EQ.0
               IF(.NOT.OK) THEN
                  wtgtrm = wtgtrm + 1
                  if (mod(wtgtrm-1,Tagger_warning).eq.0) then
                     write(crmess,27) TGTR_BOS_name,wtgtrm
 27                  format ('malformed bank : ',a4,
     +                    ' occured ',I5,' times')
                     CALL RECMES(CRNAME,'W',CRMESS)
                  endif
               ELSE
c     
c     Now we know we have hits in TGTR:
c     
                  TGTR_event=.true.
                  Tgtr_NbHits= size/TGTR_BOS_size
                  
                  if (Tgtr_Nbhits.gt.(MAXTC)) then
                     wtgtro = wtgtro + 1
                     if (mod(wtgtro-1,Tagger_warning).eq.0) then
                        write(crmess,28) Tgtr_NbHits,MAXTC,wtgtro
 28                     format (I4,
     +                       ' hits in TGTR, more than we can eat! ',
     +                       I4,' occured ',I5,' times')
                        CALL RECMES(CRNAME,'W',CRMESS)
                     endif
                     Tgtr_NbHits = MAXTC
                  endif
                  
c     --- get hits from TGTR bank
                  
                  k = 0
                  do i=1,Tgtr_NbHits
                     j=(i-1)*TGTR_BOS_size
                     idtest = iw16(2*idTR+j+1)
                     idtest1 = idtest
                     if (idtest .le. 0 .or. idtest .gt. 63) then
                        wtgtrid = wtgtrid + 1
                     else if(idtest .lt.62) then
c --  kludge for eg3a,g8b T37R<->T38R cable swap 
                        if(irun .ge. 45129 .and. irun .le. 48330) then
                           if(idtest .eq. 38) then
                              idtest=37
                           else if(idtest.eq.37) then
                              idtest=38
                           endif
                        endif
c -- end kludge -----------------------
                        k = k + 1
                        itc_id_right(k) = idtest ! <----- in the common TgtRraw
                        itc_val_right(k) = iw16(2*idTR+j+2) ! <----- in the common TgtLraw
                        dt_corr = 0
                        if(Tagger_correct.ne.0) dt_corr = tr_shift(idtest1)
                        itc_val_right(k) = itc_val_right(k) - dt_corr ! shift correction
                     endif
                  enddo
               ENDIF            !<---- enf if(size)             
               Tgtr_NbHits = k     
               call tag_sort_array(itc_id_right,itc_val_right,Tgtr_NbHits)
            endif               !<----------- if(.not.OK)
         endif                  !<--- if(.not.OK)
c-------------end TGTR -------------------         
      endif                     ! <--- end TGTL/TGTR

      if(TGTL_event .or. TGTR_event) TAGT_event = .true.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
c
c     Deal with E bank
c     
      
      idE=MAMIND(iw,TagEData_BOS_name)
      idE = IW(idE)
      
      OK = (idE.gt.0)
      if (.not.OK) then
c     
c     -- The format is wrong, this is not good
c     
         wtagea = wtagea + 1
         if (mod(wtagea-1,Tagger_warning).eq.0) then
            write(crmess,5) TagEData_BOS_name,wtagea
 5          format (a4,'bos bank corrupted or absent occured ',
     +           I5,' times')
            CALL RECMES(CRNAME,'W',CRMESS)
         endif
      else
         TAGE_bank=.true.
c     
c     deal with E bank
c     
         
         size4=IW(idE)          !size in [I*4] words
         size= 2*size4          !size in [I*2] words
*     
*     -BOS only "knows" fullwords; odd number of half words were rounded up
*     -(the remaining halfword is wasted)
*     
c     OK = size.ne.0 ! well that 's OK it can happens , E.A.
         
         if(size.eq.0) then
            wtagee = wtagee + 1
            if (mod(wtagee-1,Tagger_warning).eq.0) then
               write(crmess,6) TagEData_BOS_name,wtagee
 6             format ('empty bank : ',a4,' occured ',I5,' times')
               CALL RECMES(CRNAME,'W',CRMESS)
            endif
         ELSE 	
            
            IF( MOD(size-1,TagEData_BOS_size).EQ.0 ) THEN
               size= size-1     !BOS was forced to round up
            ENDIF
            
            OK= size.GE.TagEData_BOS_size .and. MOD(size,TagEData_BOS_size).EQ.0
            IF(.NOT.OK) THEN
               wtagem = wtagem + 1
               if (mod(wtagem-1,Tagger_warning).eq.0) then
                  write(crmess,7) TagEData_BOS_name,wtagem
 7                format ('malformed bank : ',a4,' occured ',I5,' times')
                  CALL RECMES(CRNAME,'W',CRMESS)
               endif
               TAGE_event=.false.
               RETURN           ! there is nothin else to do later on...
            ENDIF
c     
c     Now we know we have events in TAGE:
c     
            TAGE_event=.true.
            Tag_E_NbHits= size/TagEData_BOS_size
c     
c  try to find out if they are in double edges mode or not
c
c!!!!!!!!!!!!!!!!!!!!! 02-JUN-2004 EAP commented it out
c       IF (TAGEMODE.lt.1) THEN
c         NGETTAGEMODE = NGETTAGEMODE + 1
c         CALL tag_get_TAGE_mode(ok,doubleedges,Tag_E_NbHits,idE,TagEData_BOS_size)
c         if (ok) then
c	  TAGEMODE = TAGEMODE + 1
c           write(crmess,10) NGETTAGEMODE,doubleedges
c10         format ('Succesfully found TAGE mode at ',I5,' pass: E counter TDC are ',I1,' edge(s) mode')
c           CALL RECMES(CRNAME,'I',CRMESS)
c         else
c           write(crmess,11) NGETTAGEMODE
c11         format ('Could not determine TAGE mode at ',I5,' pass')
c           CALL RECMES(CRNAME,'W',CRMESS)
c         endif
c       ENDIF
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
c	fill the common ECevent - first check for overflow:
c
            doubleedges = 1     ! asume it is always single edge mode as it is supposed to be
            
            if (Tag_E_Nbhits.gt.(doubleedges*MAXEC)) then
               wtageo = wtageo + 1
               if (mod(wtageo-1,Tagger_warning).eq.0) then
                  write(crmess,8) (Tag_E_NbHits/doubleedges),MAXEC,wtageo
 8                format (I4,
     +                 ' hits in TAGE, more than we can eat! ',
     +                 I4,' occured ',I5,' times')
                  CALL RECMES(CRNAME,'W',CRMESS)
               endif
               Tag_E_NbHits = doubleedges*MAXEC
            endif
            
            IF (doubleedges.eq.2) then
c     
c     We have to deal with leading and falling edges:
c     
               bascule = 0
               k = 0
               ec_idpred = -1
               
               do i=1,Tag_E_NbHits
                  j=(i-1)*TagEData_BOS_size
                  ec_idtest = iw16(2*idE+j+1)
                  if (ec_idtest.le.0.or.ec_idtest.gt.384) then
                     wecid = wecid + 1
                     if (mod(wecid-1,Tagger_warning).eq.0) then
                        write(crmess,9) ec_idtest,wecid
 9                      format ('E counter ID out of range ',I4,
     +                       ' occured ',I5,' times')
                        CALL RECMES(CRNAME,'W',CRMESS)
                     endif
                  else
                     if (bascule.eq.1.and.ec_idtest.eq.ec_idpred) then
                        bascule = 0
                     else
                        k = k + 1
                        ec_id(k) = ec_idtest ! <----- in the common ECevent
                        ec_val(k) = iw16(2*idE+j+2) ! <----- in the common ECevent
                        ec_idpred = ec_idtest
                        bascule = 1
                     endif      ! (bascule.eq.1.and.ec_idtest.eq.ec_idpred) 
                  endif         ! (ec_idtest.le.0.or.ec_idtest.gt.384)
               enddo
            ELSE                ! doubleedges
c     
c     ---------- The TDC is in leading edge mode
c     
               k = 0
               do i=1,Tag_E_NbHits
                  j=(i-1)*TagEData_BOS_size
                  ec_idtest = iw16(2*idE+j+1)
                  if (ec_idtest .le. 0 .or. ec_idtest .gt. 384) then
                     wecid = wecid + 1
c     Turned off error message for new TAGE format --- July-1-2001 (EAP)
c     if (mod(wecid-1,Tagger_warning).eq.0) then
c     write(crmess,9) ec_idtest,wecid
c     CALL RECMES(CRNAME,'W',CRMESS)
c     endif
                  else
                     k = k + 1
c     -- kludge to fix swapped cables for g10-g11 
c     --     E42<->E44, E61<->E63 E223<->E225
                     
                     if(irun .ge. 42271 .and. irun .le.44190) then
                        if (ec_idtest .eq. 61) then
                           ec_idtest = 63
                        else if (ec_idtest .eq. 63) then
                           ec_idtest = 61
                        else if (ec_idtest .eq. 223) then
                           ec_idtest = 225
                        else if (ec_idtest .eq. 225) then
                           ec_idtest = 223
                        endif
                     endif
c     -- end kludge
                     ec_id(k) = ec_idtest ! <----- in the common ECevent
                     ec_val(k) = iw16(2*idE+j+2) ! <----- in the common ECevent
                  endif
               enddo
            ENDIF               ! doubleedges
            
            
c     
c     keep on dealing with double edeges
c     
            
            Tag_E_NbHits = k
            
c     
c     ----- sort the array by increasing order in id
c     
            call tag_sort_array(ec_id,ec_val,Tag_E_NbHits)
            
         ENDIF                  ! (size.gt.0)
      ENDIF                     ! (ok)  
      
      return
      end		
      
