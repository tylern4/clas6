c  ffpar_ec.inc

c  $Id: ffpar_ec.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $

      character*132 ciffpar_ec
      data ciffpar_ec/
     1'$Id: ffpar_ec.inc,v 1.2 1997/04/24 15:38:28 holtrop Exp $'
     1/

      integer ec_tdc_to_channel
      real atlen,poiss,ec_cuts
      common/ffpar_ec/ec_cuts(5),atlen,poiss,ec_tdc_to_channel
