      subroutine read_att_len

c  calculate attenuation length for each stack (from fit)      
c   constant att.length = 3.5m
      include "ntpl_com.inc"
      
      integer *4 i_id,ilay,ilam,isec

      character*(*) clas_parms
      character*120 filename
      parameter (clas_parms  = 'CLAS_PARMS')
      
      call revinm(clas_parms,'ec1atten.dat',filename)
      
      open (unit=22,file=filename,status='old')
      do iii=1,256
          read (22,*) isec,ilay,i_id,ilam
        if (attlen .gt. 0) then   ! put constant att.len.
          ilam = attlen
        endif
        if (ilay.eq.1) lambda(isec,2,i_id) = ilam
        if (ilay.eq.2) lambda(isec,1,i_id) = ilam
        if (ilay.eq.3) lambda(isec,4,i_id) = ilam
        if (ilay.eq.4) lambda(isec,3,i_id) = ilam
      end do
      close (22)

c read calibration constant

c      filename = '/home/users/vvsap/PARMS/timing.dat'
c      open (unit=22,file=filename,status='old')
c      
c      do iii=1,256
c        read (22,*) isec,ilay,i_id,t1,t2
c        dT_l(isec,ilay,i_id) = t1
c        dT_r(isec,ilay,i_id) = t2
c      end do
c      close (22)
      return
      end
