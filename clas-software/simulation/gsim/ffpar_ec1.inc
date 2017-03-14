c  ffpar_ec1.inc

c  $Id: ffpar_ec1.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $

      character*132 ciffpar_ec1
      data ciffpar_ec1/
     1'$Id: ffpar_ec1.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $'
     1/

      integer iatten,ithreshold
      real ec1_cuts,photoel,delay,reflect,charge_to_adc
      common/ffpar_ec1/ec1_cuts(5)
     1,iatten,ithreshold,photoel,delay,reflect,charge_to_adc
