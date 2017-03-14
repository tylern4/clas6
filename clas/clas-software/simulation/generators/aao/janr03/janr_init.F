      SUBROUTINE JANR_INIT

      IMPLICIT NONE
      SAVE

#include "janr_data.inc"
#include "delta_dat.inc"
#include "inpar.inc"
#include "fit_par.inc"
#include "janrtab.inc"

      CHARACTER*10 dummych
      character*60 parifile,infile,resifile
      INTEGER i,LINPUT
      REAL qavg 
      data LINPUT/21/
      
      read (5,*) infile
      write(6,*)'JANR_INIT: Reading the input file ',infile
      
      OPEN (UNIT=LINPUT,FILE=infile,STATUS='OLD')
      
      READ (LINPUT,*) fitopt 
      READ (LINPUT,*) grfopt 
      READ (LINPUT,*) n1,n2,nebac
      READ (LINPUT,*) obsv2fit     
      READ (LINPUT,*) Ebeam
      READ (LINPUT,*) WMIN,WMAX,WSTEP
      READ (LINPUT,*) Q2MIN,Q2MAX,Q2STEP
      READ (LINPUT,*) COSMIN,COSMAX,COSSTEP
      READ (LINPUT,*) PHIMIN,PHIMAX,PHISTEP
      READ (LINPUT,*) IntAccur
      READ (LINPUT,*) inffile
      READ (LINPUT,*) maxcalls
      READ (LINPUT,*) tolorance 
      READ (LINPUT,*) errorup
      READ (LINPUT,*) parms
      READ (LINPUT,*) resifile
      READ (LINPUT,*) parifile      
      READ (LINPUT,*) parofile      
      READ (LINPUT,*) datform 
      READ (LINPUT,*) lmini,lmino,lminf
      READ (LINPUT,*) chimax
      READ (LINPUT,*) fname(1)
      READ (LINPUT,*) fname(2)
      READ (LINPUT,*) fname(3)
      READ (LINPUT,*) fname(4)
      close(LINPUT)
      
      call janr_input_res(resifile)
      
      OPEN (UNIT=LINPUT,FILE=parifile,STATUS='OLD')
      
      If(parms.gt.0.and.parms.le.maxpar) then  
         Do i=1,parms
           read(LINPUT,*)plist(i),pname(i),start_value(i)
     +         ,step_size(i),par_stat(i),low_lim(i),up_lim(i)
         EndDo
      Else
         Write(6,*)'JANR_INIT: Wrong Number of parameters=',parms
         Return
      EndIf
      
53    format(i2,1x,A10,2f12.2,i10,2f10.4)
      
      close(LINPUT)
           
      call janr_input_said()
      call janr_input_ebac()
      
      if (grfopt.ne.'N') call hjanr_init(fname)
      
      qavg = 0.5*(q2min+q2max)      
      call janr_ini_point(qavg)
      
      call mult_ana(qavg)
      
      RETURN
      END





