c  stsets.inc

c  $Id: stsets.inc,v 1.4 2004/09/07 16:49:28 devita Exp $

      character*132 cistsets
      data cistsets/
     1'$Id: stsets.inc,v 1.4 2004/09/07 16:49:28 devita Exp $'
     1/

      integer nhits_sth
      parameter (nhits_sth=10)
      
      integer nvol_st,nhits_st,ndigi_st
      parameter (nvol_st=2)
      parameter (nhits_st=6)
      parameter (ndigi_st=4)

      integer idtype_st,set_st
      integer det_st1,det_st2,det_sth1,det_sth2
      integer
     1    det_stp11,det_stp12
     1   ,det_stp21,det_stp22,det_stp23
     1   ,det_stp31,det_stp32,det_stp33
     1   ,det_stp41,det_stp42
     1   ,det_stph11,det_stph12
     1   ,det_stph21,det_stph22,det_stph23
     1   ,det_stph31,det_stph32,det_stph33
     1   ,det_stph41,det_stph42


    
      common/stsets/idtype_st,set_st,det_st1,det_st2,det_sth1,det_sth2
     1   ,det_stp11,det_stp12
     1   ,det_stp21,det_stp22,det_stp23
     1   ,det_stp31,det_stp32,det_stp33
     1   ,det_stp41,det_stp42
     1   ,det_stph11,det_stph12
     1   ,det_stph21,det_stph22,det_stph23
     1   ,det_stph31,det_stph32,det_stph33
     1   ,det_stph41,det_stph42




