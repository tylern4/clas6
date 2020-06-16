        subroutine tag_read_from_BOS(OK,TAGT_bank,TAGE_bank,TAGT_event,TAGE_event)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_read_from_BOS.F,v 1.20 2004/06/02 23:13:21 pasyuk Exp $
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
      DATA CRCSID/   
     1'$Id: tag_read_from_BOS.F,v 1.20 2004/06/02 23:13:21 pasyuk Exp $'   
     2/   
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
      integer i,j,k,TAGTbank_nbhits
      integer doubleedges,bascule,ec_idpred ! TAGE double edges crap
      integer ec_idtest
      integer MLINK,MAMIND

c_end_var
c
c
c  executable code for routine tag_read_from_BOS:
c----6----------------------------------------------------------------72
c
     
       IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         crmess = 'Reading for the first time Tagger BOS bank. Message written once '
         CALL RECMES(CRNAME,'I',CRMESS)
       ENDIF

           

c
c -------- initialisation

      Tag_t_nbhits = 0
      Tag_E_NbHits = 0
      MOR_tdc = -1 ! by convention MOR_TDC<0 <=> no hit in the MOR TDC.

c
c -------- deal with T bank


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
1            format (a4,'bos Bank corrupted or absent, occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
       TAGT_bank=.false.
       TAGT_event=.false.
      else
       TAGT_bank=.true.
c
c	Deal with T bank
c
        size4=IW(idT)                        !size in [I*4] words
        size= 2*size4                       !size in [I*2] words
*
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
*
c         OK = size.ne.0  ! well that's OK, it happens sometime.. E.A.

         IF(size.eq.0) then
            wtagte = wtagte + 1
            if (mod(wtagte-1,Tagger_warning).eq.0) then
             write(crmess,2) TagTData_BOS_name,wtagte
2            format ('empty bank : ',a4,' occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
           TAGT_event=.false.
         ELSE

           IF( MOD(size-1,TagTData_BOS_size).EQ.0 ) THEN
            size= size-1                      !BOS was forced to round up
           ENDIF
      
           OK= size.GE.TagTData_BOS_size .and. MOD(size,TagTData_BOS_size).EQ.0
           IF(.NOT.OK) THEN
            wtagtm = wtagtm + 1
            if (mod(wtagtm-1,Tagger_warning).eq.0) then
             write(crmess,3) TagTData_BOS_name,wtagtm
3            format ('malformed bank : ',a4,' occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
            endif
             TAGT_event=.false.
           ELSE
             TAGT_event=.true. ! YES we are happy !!
           ENDIF

           IF (TAGT_event) then	
c
c	fill the common TCevent
c
            TAGTbank_nbhits = size/TagTData_BOS_size
            Tag_t_nbhits = 0  ! <----- in the common TCevent
            do i=1,TAGTbank_nbhits
		j=(i-1)*TagTData_BOS_size
                if (iw16(2*idT+j+1).le.0.or.iw16(2*idT+j+1).gt.62) then
                  wtid = wtid + 1
                  if (mod(wtid-1,Tagger_warning).eq.0) then
                    write(crmess,4) iw16(2*idT+j+1),wtid
4                   format ('T counter ID out of range ',I4,': hit bypassed - occured ',I5,' times')
                    CALL RECMES(CRNAME,'W',CRMESS)
                  endif
                else ! good T id
                  if (iw16(2*idT+j+1).eq.62) then
                    MOR_tdc = iw16(2*idT+j+2)
                  else ! the value is between 1 and 61
                    Tag_t_nbhits = Tag_t_nbhits + 1 ! <----- in the common TCeven
                    tc_id(i)=iw16(2*idT+j+1)        ! <----- in the common TCevent
		    tc_val_left(i)=iw16(2*idT+j+2)  ! <----- in the common TCevent
		    tc_val_right(i)=iw16(2*idT+j+3) ! <----- in the common TCevent
                  endif
                endif
            enddo
           ENDIF ! (TAGT_event)
         ENDIF ! (size.ne.0)
      endif ! (ok)

c
c	Deal with E bank
c

      idE=MAMIND(iw,TagEData_BOS_name)
      idE = IW(idE)

      OK = (idE.gt.0)
      if (.not.OK) then
c
c  -- The format is wrong, this is not good
c
       wtagea = wtagea + 1
       if (mod(wtagea-1,Tagger_warning).eq.0) then
         write(crmess,5) TagEData_BOS_name,wtagea
5        format (a4,'bos bank corrupted or absent occured ',I5,' times')
         CALL RECMES(CRNAME,'W',CRMESS)
       endif
       TAGE_bank=.false.
       TAGE_event=.false.
      else
       TAGE_bank=.true.
c
c  deal with E bank
c

        size4=IW(idE)                       !size in [I*4] words
        size= 2*size4                       !size in [I*2] words
*
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
*
c       OK = size.ne.0 ! well that 's OK it can happens , E.A.

       if(size.eq.0) then
         wtagee = wtagee + 1
         if (mod(wtagee-1,Tagger_warning).eq.0) then
           write(crmess,6) TagEData_BOS_name,wtagee
6          format ('empty bank : ',a4,' occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
         endif
          TAGE_event=.false.
       ELSE 	

        IF( MOD(size-1,TagEData_BOS_size).EQ.0 ) THEN
          size= size-1                      !BOS was forced to round up
        ENDIF
      
        OK= size.GE.TagEData_BOS_size .and. MOD(size,TagEData_BOS_size).EQ.0
        IF(.NOT.OK) THEN
         wtagem = wtagem + 1
         if (mod(wtagem-1,Tagger_warning).eq.0) then
           write(crmess,7) TagEData_BOS_name,wtagem
7          format ('malformed bank : ',a4,' occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
         endif
          TAGE_event=.false.
          RETURN ! there is nothin else to do later on...
        ENDIF
c
c    Now we know we have events in TAGE:
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
       doubleedges = 1 ! asume it is always single edge mode as it is supposed to be
       
       if (Tag_E_Nbhits.gt.(doubleedges*MAXEC)) then
         wtageo = wtageo + 1
         if (mod(wtageo-1,Tagger_warning).eq.0) then
           write(crmess,8) (Tag_E_NbHits/doubleedges),MAXEC,wtageo
8          format (I4,' hits in TAGE bank overflowing the array maximum value of ',I4,' occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
         endif
         Tag_E_NbHits = doubleedges*MAXEC
       endif

       IF (doubleedges.eq.2) then
c
c  We have to deal with leading and falling edges:
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
9             format ('E counter ID out of range ',I4,' occured ',I5,' times')
              CALL RECMES(CRNAME,'W',CRMESS)
            endif
          else
            if (bascule.eq.1.and.ec_idtest.eq.ec_idpred) then
              bascule = 0
            else
              k = k + 1
              ec_id(k) = ec_idtest             ! <----- in the common ECevent
              ec_val(k) = iw16(2*idE+j+2)      ! <----- in the common ECevent
              ec_idpred = ec_idtest
              bascule = 1
            endif ! (bascule.eq.1.and.ec_idtest.eq.ec_idpred) 
          endif ! (ec_idtest.le.0.or.ec_idtest.gt.384)
        enddo
       ELSE ! doubleedges
c
c ---------- The TDC is in leading edge mode
c
        k = 0
        do i=1,Tag_E_NbHits
          j=(i-1)*TagEData_BOS_size
          ec_idtest = iw16(2*idE+j+1)
          if (ec_idtest.le.0.or.ec_idtest.gt.384) then
            wecid = wecid + 1
c  Turned off error message for new TAGE format --- July-1-2001 (EAP)
c            if (mod(wecid-1,Tagger_warning).eq.0) then
c              write(crmess,9) ec_idtest,wecid
c              CALL RECMES(CRNAME,'W',CRMESS)
c            endif
          else
            k = k + 1
            ec_id(k) = ec_idtest             ! <----- in the common ECevent
            ec_val(k) = iw16(2*idE+j+2)      ! <----- in the common ECevent
          endif
        enddo
       ENDIF ! doubleedges


c
c  keep on dealing with double edeges
c

        Tag_E_NbHits = k

c
c ----- sort the array by increasing order in id
c
        call tag_sort_array(ec_id,ec_val,Tag_E_NbHits)
       
       ENDIF ! (size.gt.0)
      ENDIF ! (ok)  

      return
      end		
