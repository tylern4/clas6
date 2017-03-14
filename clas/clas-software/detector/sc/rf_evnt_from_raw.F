	subroutine rf_evnt_from_raw(OK,rf1,rf2)
c
c_begin_doc
c  RCS ID string
c  $Id: rf_evnt_from_raw.F,v 1.4 1999/03/09 17:56:05 boiarino Exp $
c
c  Documentation for subroutine rf_evnt_from_raw
c
c  Purpose:    In case the call bank does not exists,
c  --------   try to build the rf from the raw baks (RC13)
c
c
c  Input Parameters: OK
c  ----------------
c
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------    OK     logical  true = everything went right
c                       rf1 (integer) rf1
c                       rf2 (integer) rf2
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
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
      logical ok    ! conversion worked or not
      integer rf1,rf2
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
     1'$Id: rf_evnt_from_raw.F,v 1.4 1999/03/09 17:56:05 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='rf_evnt_from_raw')
      PARAMETER (CRAUTH='Eric Anciant ')
c
c  Local User defined variables:
c  -----------------------------
c        
c_end_var
c
c
c  executable code for routine rf_evnt_from_raw
c----6----------------------------------------------------------------72
*       
        integer NWRITE
 
	integer mamind,ndrop
	external mamind,ndrop

	integer nami,ind,i
		
	integer Raw_Data(2000),Nb_Word
	
	integer channel,Slot_Id,value
c
       
c----------------------------------------------------
c
c	 deal with RC13.

       call blist(iw,'E+','RC13')
       nami = mamind(IW,'RC13')
       ind = IW(nami)

       OK = ind.ne.0
       if (OK) then
c
c	Copying raw data to temporary array so that data can be reformatted locally
c
          Nb_Word = iw(ind)
          do i=1,Nb_Word
            Raw_Data(i) =  iw(ind+i)
          enddo
       else
	 CRMESS = 'no data from CALL bank or ROC 13 available - no RF for this event.'
c	 call RECMES(CRNAME,'W',CRMESS)
	 return
       endif

       do i=1,Nb_Word
          Slot_Id=ishft(Raw_Data(i),-27)
          channel=ISHFT(IAND(Raw_Data(i),'007F0000'x),-16)
          value=IAND(Raw_Data(i),'00000FFF'x) !TDC 12 bit
          
          if (Slot_Id.eq.13) then
         	if (channel.eq.48) then
                  rf1 = value
         	else if (channel.eq.49) then
                  rf2 = value
           	endif
          endif
       enddo	
   

       return
       end
