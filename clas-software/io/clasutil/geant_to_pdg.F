      integer function geant_to_pdg(geantid)

c_begin_doc

c  Documentation for integer function geant_to_pdg(geantid)

c  $Id: geant_to_pdg.F,v 1.1.1.1 1997/03/28 20:50:00 marki Exp $

c  Purpose:
c  --------
c     translates geant to pdg id
c     if no pdg translation, then uses 1000000 + geant id

c  Input:
c  ------
c     geantid          geant id

c  Output:
c  -------
c     returns translation from geant to pdg id, 0 if none found

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 19-sep-95
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: geant_to_pdg.F,v $')
      parameter (crevis= '$Revision: 1.1.1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1997/03/28 20:50:00 $')
      parameter (cautho= '$Author: marki $')
      parameter (crname= 'GEANT_TO_PDG')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: geant_to_pdg.F,v 1.1.1.1 1997/03/28 20:50:00 marki Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer geantid

c  local variables:
c  ----------------
      integer nsel
      parameter (nsel=44)

      integer idpdg(nsel)
      data idpdg/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/
c_end_var


c  executable code:
c  ----------------

      if(geantid.le.0)then
         geant_to_pdg=0
      elseif(geantid.le.nsel)then
         geant_to_pdg=idpdg(geantid)
      else
         geant_to_pdg=1000000+geantid
      endif
      
      return
      end

c--------------------------------------------------------------------
