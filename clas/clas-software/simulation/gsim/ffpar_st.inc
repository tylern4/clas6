c  ffpar_st.inc

c  $Id: ffpar_st.inc,v 1.4 2004/09/07 16:46:49 devita Exp $

      character*132 ciffpar_st
      data ciffpar_st/
     1'$Id: ffpar_st.inc,v 1.4 2004/09/07 16:46:49 devita Exp $'
     1/

        real st_cuts(5)       !cut parameter for physics reactions
        real st_zoff_ff       !offset wrt. nominal position of start counter
	real st_type	      ! 0 = old, 1 = new
	common/ffpar_st/st_cuts,st_zoff_ff,st_type

