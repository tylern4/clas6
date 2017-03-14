c----6----------------------------------------------------------------72
c  
c  Variables contained in SCRW_NT common block
c  --------------------------------------------
      INTEGER MAXsc1hit
      PARAMETER (MAXsc1hit = 20)
      INTEGER nsc1
      INTEGER sc1sectpd(MAXsc1hit)
      REAL sctl(MAXsc1hit),sctr(MAXsc1hit)
      REAL scel(MAXsc1hit),scer(MAXsc1hit)
c
c_end_var
c
       COMMON/SC1_NT/nsc1,sc1sectpd,sctl,scel,sctr,scer

