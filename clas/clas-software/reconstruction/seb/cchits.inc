c      include file CCHITS
c
c_begin_doc
c  RCS ID string
c  $Id: cchits.inc,v 1.2 1997/05/01 19:59:45 freyberg Exp $
c
c  Documentation for include file CCHITS
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:    S.Stepanyan     Created:  Thu Apr 10 21:40:17 EDT 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file CCHITS:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCHITS
      DATA CICCHITS/
     1'$Id: cchits.inc,v 1.2 1997/05/01 19:59:45 freyberg Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCHITS common block
c  --------------------------------------------
      INTEGER maxcchits
      PARAMETER (maxcchits=20)
      INTEGER nrclust
      INTEGER nrsect(maxcchits)
      INTEGER nrphe(maxcchits)
      INTEGER nrtime(maxcchits)
      INTEGER nrthet(maxcchits)
      INTEGER nrdthet(maxcchits)
      INTEGER nrphi(maxcchits)
      INTEGER nrsegm(maxcchits)
c
c_end_var
c
      COMMON/CCHITS/nrclust,nrsect,nrsegm,nrphe,nrtime,nrthet,nrdthet,
     +nrphi
c----6----------------------------------------------------------------72




