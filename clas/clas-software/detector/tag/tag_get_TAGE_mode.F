        subroutine tag_get_TAGE_mode(OK,NB_edges,Tag_E_NbHits,idE,TagEData_BOS_size)
c
c_begin_doc
c  RCS ID string
c  $Id: tag_get_TAGE_mode.F,v 1.1 1998/05/05 10:29:24 anciant Exp $
c
c  Documentation for subroutine tag_get_TAGE_mode
c
c  Purpose: 
c  --------
c
c       Reads raw data from TAGE and determine if the E counters
c        TDC are in simple or doble edges mode.
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  NB_edges : 1 simple mode
c             2 doubles edges mode
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  I look at 2 consecutive hits in the TAGE bank, if they don't have have the same ID
c  then TDC is in single mode, if I see several times couples of hits with the same
c  Id I assume they are in double edge mode.
c
c
c
c
c  Author:          E. Anciant, may, 4th 1998
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
*
*-OUTPUT
      LOGICAL OK                !success or failure
      INTEGER NB_edges        ! how many edges ?
*-INPUT
      integer Tag_E_NbHits      ! NB hits in the TAGE bank
      integer idE               ! TAGE bank index in IW
      integer TagEData_BOS_size ! TagEData_BOS_size
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
     1'$Id: tag_get_TAGE_mode.F,v 1.1 1998/05/05 10:29:24 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_get_TAGE_mode')
      PARAMETER (CRAUTH='Eric Anciant ')
c
c  Local User defined variables:
c  -----------------------------
c        
      integer bascule,i,val1,val2,j
      logical doubleedge

c_end_var
c
c
c  executable code for routine tag_get_TAGE_mode:
c----6----------------------------------------------------------------72
c
     
        NB_edges = 1 ! default routine ouput value
        
c
c   algorithm starting values.

        bascule = 0
        doubleedge = .true.
        val1 = -1
        val2 = -1
        i = 0

        do while (i.lt.Tag_E_NbHits.and.val1.ne.0.and.val2.ne.0.and.i.lt.200)
         i = i + 1
         j=(i-1)*TagEData_BOS_size

         if (bascule.eq.0) then 
           val1=iw16(2*idE+j+1)  
           bascule = 1
         else
           val2=iw16(2*idE+j+1)      
           bascule = 0
           if (val2.ne.val1.and.val2.ne.0.and.val1.ne.0) doubleedge = .false.
         endif ! bascule
        enddo


        ok = .true.
        if (doubleedge) then
          if (i.gt.20) then
             NB_edges = 2
          else
             ok = .false.
          endif
        endif 
              
      return
      end		
