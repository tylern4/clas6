      subroutine ec_bor
      include 'eccal.inc'

      character*132 file_in,dumc
      integer str,lay,ped,id,sec,s,i,dumi
      real rms

      file_in='/usr/local/clas/parms/pedman/Tfiles/ec.tped'

      open(unit=12,file=file_in,status='old')

      read(12,*) dumc
      read(12,*) dumc
      read(12,*) dumi
      do s=1,6
        read(12,*) dumc
        do i=1,216
          read(12,*) id,sec,ped,rms
          str = mod(id,256)
          lay = id/256
          ecped(str,lay,sec+1) = ped
        enddo
      enddo

      close(12)

      return
      end
