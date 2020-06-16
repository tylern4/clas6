*
* ec.inc - definitions for EC package
*
      integer  MAXsectors
      parameter ( MAXsectors= 6)
*
      integer  MAXlayers
      parameter ( MAXlayers= 6)
*
      integer  MAXstrips
      parameter ( MAXstrips= 36)
*
      integer  MAXpeaks, MAXhits
      parameter ( MAXpeaks= 30)
      parameter ( MAXhits= 30)
*
      structure /ecstrip/
        real energy
        real time
      end structure
*
      structure /ecpeak/
        real energy
        real time
        real coord
        real width
        integer strip1
        integer stripn
        real tmp
      end structure
*
      structure /echit/
        integer*2 sector
        integer*2 layer
        real energy
        real denergy
        real time
        real dtime
        real i
        real j
        real di
        real dj
        real x
        real y
        real z
        real dx
        real dy
        real dz
*
        real uvw2(3)
        real uvw3(3)
        real i2
        real j2
        real i3
        real j3
*
        real width
        integer peak1(3)
        integer peakn(3)

        real k

      end structure
*



