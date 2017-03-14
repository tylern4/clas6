*
      REAL elee,elet,elef,elex,eley,elez,xb,q2,w
      INTEGER nele,eleevtclass,eleh,elerun,eletype,eleevent
      COMMON /ELEREC/ nele,eleevtclass,eleh,elerun,eletype,eleevent,elee
     + ,elet,elef,elex,eley,elez,xb,q2,w
*
      REAL pi0e,pi0t,pi0f,pipe,pipt,pipf,pime,pimt,pimf,proe,prot,prof
     + ,game,gamt,gamf,gam2t,gam2e,gam2f,neue,neut,neuf,pim,pipx,pipy
     + ,pipz,pimx,pimy,pimz,prox,proy,proz
      INTEGER npi0,npip,npim,npro,ngam,nneu
      COMMON /HADREC/ npi0,npip,npim,npro,ngam,nneu,pi0e,pi0t,pi0f,pipe
     + ,pipt,pipf,pime,pimt,pimf,proe,prot,prof,game,gamt,gamf,gam2t
     + ,gam2e,gam2f,neue,neut,neuf,pim,pipx,pipy,pipz,pimx,pimy,pimz
     + ,prox,proy,proz
*
      REAL gelee,gelet,gelef,gelex,geley,gelez,gelewgt,gxb,gq2,gw
      INTEGER gnele,gelebits,ghpar,gproh,geleh
      COMMON /ELEGEN/ gnele,gelebits,ghpar,gproh,geleh,gelee,gelet
     +,gelef,gelex,geley,gelez,gelewgt,gxb,gq2,gw
*
      REAL gpi0e,gpi0t,gpi0f,gpipe,gpipt,gpipf,gpime,gpimt,gpimf,gproe
     + ,gprot,gprof,ggame,ggamt,ggamf,ggam2t,ggam2e,ggam2f,gneue,gneut
     + ,gneuf,gpipx,gpipy,gpipz,gpimx,gpimy,gpimz,gprox,gproy,gproz
      INTEGER gnpi0,gnpip,gnpim,gnpro,gngam,gnneu
      COMMON /HADGEN/ gnpi0,gnpip,gnpim,gnpro,gngam,gnneu,gpi0e,gpi0t
     + ,gpi0f,gpipe,gpipt,gpipf,gpime,gpimt,gpimf,gproe,gprot,gprof
     + ,ggame,ggamt,ggamf,ggam2t,ggam2e,ggam2f,gneue,gneut,gneuf,gpipx
     + ,gpipy,gpipz,gpimx,gpimy,gpimz,gprox,gproy,gproz
*
      INTEGER iproctpe
      REAL radq2,radnu,radweight,grade,gradt,gradf
      COMMON /RADGN/iproctpe,radq2,radnu,radweight,grade,gradt,gradf
