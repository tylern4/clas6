      SUBROUTINE GET_ALL_INDEX(Iarray)
c
c_begin_doc
c  RCS ID string
c  $Id: get_all_index.F,v 1.4 2000/11/10 21:42:12 boiarino Exp $
c
c  Documentation for subroutine GET_INDEX
c
c  Purpose:  To make life easier for the users, loads ALL the bank indices into a common block so that
c  --------  the niave user doesn't have to make BOS calls.  Note the common block is filled with the
c            index of the first bank in the IW array if there are multiple occurances the user must
c            flesh them out.
c
c  Input Parameters:  IARRAY: I: 1= IW array, 2=jw array
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
c  Author:   arne       Created:  Sat Oct 25 12:11:51 EDT 1997
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
#include "wcs.inc"
#include "clas_index.inc"
#include "clas_offsets.inc"
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
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: get_all_index.F,v 1.4 2000/11/10 21:42:12 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='GET_INDEX')
      PARAMETER (CRAUTH='arne ')
c
c  Local User defined variables:
c  -----------------------------
      integer mamind
      external mamind

      INTEGER ibank, nami, iarray
c_end_var
c
c  executable code for routine GET_INDEX:
c----6----------------------------------------------------------------72
c

           do 100 ibank = 1, N_DDL_BANKS
              nami = mamind(iw,ddl_bank_names(ibank))
              index_to_banks(ibank) = iw(nami)
              index16_to_banks(ibank) = 2*iw(nami)
              which_array(ibank) = 1
              if (index_to_banks(ibank) .eq. 0) then
                nami = mamind(jw,ddl_bank_names(ibank))
                index_to_banks(ibank) = jw(nami)
                index16_to_banks(ibank) = 2*jw(nami)
                which_array(ibank) = 2
              endif
 100       continue

      RETURN
      END
c
c------------------------------------------------------------------------------




