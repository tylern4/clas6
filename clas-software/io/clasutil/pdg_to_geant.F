      integer function pdg_to_geant(idpdg)

c_begin_doc

c  Documentation for integer function pdg_to_geant(idpdg)

c  $Id: pdg_to_geant.F,v 1.2 2001/09/20 21:33:40 vlassov Exp $

c  Purpose:
c  --------
c     translates pdg (Particle Data Group) to geant id
c     uses 1000000+geantid if no pdg id exists

c  Input:
c  ------
c     idpdg       pdg id

c  Output:
c  -------
c     returns translation from pdg to geant id, 0 if none found
c  FOR CLAS :    if pdg = 45 to 50, then is is unchanged
c                ( d,t,He4,Geantino,He3,Cerenkov)

c  Notes:
c  ------
c     Mostly stolen from geant321.car

c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 14-jul-95
c     Last correction : A. Vlassov, Sept, 2001
c_end_doc
c
c

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: pdg_to_geant.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/09/20 21:33:40 $')
      parameter (cautho= '$Author: vlassov $')
      parameter (crname= 'PDG_TO_GEANT')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: pdg_to_geant.F,v 1.2 2001/09/20 21:33:40 vlassov Exp $
     1 '/
      save


c  include files:
c  --------------
c_end_inc

c  input/output variables:
c  -----------------------
      integer idpdg

c  local variables:
c  ----------------
      integer nsel,id
      parameter (nsel=44)

      integer ippdg(nsel)
      data ippdg/
     +    22,   -11,    11,    12,   -13,    13,   111,   211,
     +  -211,   130,   321,  -321,  2112,  2212, -2212,   310,
     +   221,  3122,  3222,  3212,  3112,  3322,  3312,  3334,
     + -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +   -15,    15,   411,  -411,   421,  -421,   431,  -431,
     +  4122,    24,   -24,    23/
c_end_var


c  executable code:
c  ----------------

c  is id legal
      if(idpdg.gt.1000000)then
         pdg_to_geant=mod(idpdg,1000000)
         return
      endif

c  search in table
      do id = 1,nsel
         if(idpdg.eq.ippdg(id)) then
           pdg_to_geant = id 
           return
         end if
      enddo
c  special case for CLAS
      if(idpdg.ge.45 .and.idpdg.le.50) then
        pdg_to_geant = idpdg
      else
c  no translation
c        print *,' Unknown ID PDG :',idpdg
        pdg_to_geant = 0 
      end if
      return
      end

c--------------------------------------------------------------------
