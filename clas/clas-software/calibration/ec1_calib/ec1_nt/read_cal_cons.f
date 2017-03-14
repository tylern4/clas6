 
      subroutine read_cal_cons(runno,pedest)
 
      include "ntpl_com.inc"
      include "ec1_calibr.inc"
      include "ec1_pedstl.inc"
      character*(*) clas_parms
c      character*120 filename
      parameter (clas_parms  = 'CLAS_PARMS')
      integer *4 i_id,ilay,isec,ID_i
c      real    *4 a1L,a1R,a2L,a2R,t1L,t1R,t2L,t2R     
      INTEGER *4 pedest(2,2,1088)
      
      INTEGER *4 runno
      
c      call revinm(clas_parms,'ec1calib.dat',filename)
c Call LAC.lib to get calib data and pedestals from the maps

      call ec1calibmap(runno)    

      DO isec=1,2
         DO ilay=1,4
            idmax=24
            IF(ilay.eq.2.or.ilay.eq.4) idmax=40
            DO i_id=1,idmax
               A_l (isec,ilay,i_id) = ec1_cala1l(i_id,ilay,isec)
               A_r (isec,ilay,i_id) = ec1_cala1r(i_id,ilay,isec)
               dA_l(isec,ilay,i_id) = ec1_cala2l(i_id,ilay,isec)
               dA_r(isec,ilay,i_id) = ec1_cala2r(i_id,ilay,isec)
               cT_l(isec,ilay,i_id) = ec1_calt1l(i_id,ilay,isec)
               cT_r(isec,ilay,i_id) = ec1_calt1r(i_id,ilay,isec)
               dT_l(isec,ilay,i_id) = ec1_calt2l(i_id,ilay,isec)
               dT_r(isec,ilay,i_id) = ec1_calt2r(i_id,ilay,isec)
               ID_i=256*ilay+i_id
               pedest(isec,1,ID_i)=ec1_pedl(i_id,ilay,isec)
               pedest(isec,2,ID_i)=ec1_pedr(i_id,ilay,isec)
            END DO
         END DO
      END DO
c read from current dir (temporary!!!)
      
c      filename='ec1calib.dat'
c      open (unit=22,file=filename,status='old')
c      do iii=1,256
c        read (22,*) isec,ilay,i_id,pL,pR,a1L,a1R,a2L,a2R,t1L,t1R,t2L,t2R
c        A_l (isec,ilay,i_id) = a1L
c        A_r (isec,ilay,i_id) = a1R
c        dA_l(isec,ilay,i_id) = a2L
c        dA_r(isec,ilay,i_id) = a2R
c        cT_l(isec,ilay,i_id) = t1L
c        cT_r(isec,ilay,i_id) = t1R
c        dT_l(isec,ilay,i_id) = t2L
c        dT_r(isec,ilay,i_id) = t2R
c      end do
c      close (22)

      return
      end
