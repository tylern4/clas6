      subroutine sc_bor

      include "sccal.inc"
      
      
      character*132 file_in,dumc
      integer pedl,pedr,id,sec,s,i,dumi
      real rms
      
      file_in='/usr/local/clas/parms/pedman/Tfiles/sc.tped'

      open(unit=12,file=file_in,status='old')

      read(12,*) dumc
      read(12,*) dumc
      read(12,*) dumi
      do s=1,6
        read(12,*) dumc
        do i=1,48
          read(12,*) id,sec,pedl,rms,pedr,rms
          scped(id,1,sec+1) = pedl
          scped(id,2,sec+1) = pedr
        enddo
      enddo

      close(12)
      
      return
      end

