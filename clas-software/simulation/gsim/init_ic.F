      subroutine init_ic
      
      implicit none
      save
      
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "ffpar_mg.inc"
#include "ffpar_ic.inc"
#include "icmate.inc"
#include "ictmed.inc"
      
*  PbWO4 compound parameters
      REAL A_pbwo4(3), Z_pbwo4(3), W_pbwo4(3), dens_pbwo4
      DATA A_pbwo4 / 207.19, 183.85, 16.0/
      DATA Z_pbwo4 / 82., 74., 8./
      DATA W_pbwo4 /1., 1., 4./
      DATA dens_pbwo4 /8.28/

c  stainless steel
      integer mxstst
      parameter (mxstst=3)
      real astst(mxstst),zstst(mxstst),wstst(mxstst),dnstst
c                  Fe    Cr    Ni
c                  --    --    --
      data astst / 56. , 52. , 59. /
      data zstst / 26. , 24. , 28. /
      data wstst / 7.4 , 1.8 , 0.8 /
      data dnstst / 7.75 /
      
c  Mylar compound parameters
      REAL  A_mylar(3), Z_mylar(3), W_mylar(3), dens_mylar
      DATA  A_mylar /12.01, 1.01, 16.00/
      DATA  Z_mylar / 6.0,  1.0,   8.0/
      DATA  W_mylar / 5.,   4.,    2./
      DATA  dens_mylar / 1.387 /

      integer get_mate,get_tmed,mate

      
c  define IC1 pbwo4 cristals  

      ic_mat_pbwo4 = get_mate()
      
      call gsmixt(ic_mat_pbwo4,'IC PBWO4$',
     &            A_pbwo4,Z_pbwo4,dens_pbwo4,-3,W_pbwo4)
     
      ic_med_pbwo4 = get_tmed()
      
      if(nomate(ic))then
         mate=16
      else
         mate=ic_mat_pbwo4
      endif
      
      call gstmed(ic_med_pbwo4,'IC PBWO4$', mate,
     +     1,init_ifield,init_fieldm,
c     1     5., 0.5, 0.05, 0.1, 0.1, 0, 0)
     1     0.,0.,0.05, 0.005, 0.003, 0, 0)
      call set_tpar(ic_med_pbwo4,ic_cuts)
      

c  define cristal wrapping
 
      ic_mat_mylar = get_mate()
      call gsmixt(ic_mat_mylar,'IC MYLAR$',
     &            A_mylar,Z_mylar,dens_mylar,-3,W_mylar)

      ic_med_mylar = get_tmed()
      
      if(nomate(ic))then
         mate=16
      else
         mate=ic_mat_mylar
      endif
      
      call gstmed(ic_med_mylar,'IC MYLAR$', mate,
     +     1,init_ifield,init_fieldm,
     1     0.,0.,0.05, 0.005, 0.003, 0, 0)
      call set_tpar(ic_med_mylar,ic_cuts)


c  define stainless steel support foils

      ic_mat_steel = get_mate()
      call gsmixt(ic_mat_steel,'IC St. steel$',
     +             astst, zstst,dnstst,-mxstst,wstst)
      ic_med_steel = get_tmed()
      if(nomate(ic))then
         mate=16
      else
         mate=ic_mat_steel
      endif
      call gstmed(ic_med_steel,'IC St. Steel$',mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .03,   .1,   0.06,  0.03, 0,0)
      call set_tpar(ic_med_steel,ic_cuts)
      
      
c  define air foils which role is to finalise the shapes of the
c  stainless steel foils 

      ic_med_air = get_tmed()
      if(nomate(ic))then
         mate=16
      else
         mate=15
      endif
      call gstmed(ic_med_air,'IC Air$',mate,
     1     1,init_ifield,init_fieldm,
     1     5.,   0.5,   .1,   0.1,  0.1, 0, 0)
      call set_tpar(ic_med_air,ic_cuts)      

c  IC lead shielding

      ic_med_lead = get_tmed()
      if(nomate(ic))then
         mate=16
      else
         mate=13
      endif
      call gstmed(ic_med_lead,'IC Lead$', mate,
     1     0,init_ifield,init_fieldm,
     1     5.,   .006,   .5,   0.006,  0.003, 0, 0)
      call set_tpar(ic_med_lead,ic_cuts)

      return
      end
