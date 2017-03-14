c  gsimcontrol.inc

c  $Id: gsimcontrol.inc,v 1.6 2001/03/24 17:40:04 natasha Exp $

      character*132 cigsimcontrol
      data cigsimcontrol/
     1'$Id: gsimcontrol.inc,v 1.6 2001/03/24 17:40:04 natasha Exp $'
     1/

c  general purpose control flags
      logical noffread,nofield,helium_only,rawbanks_in
      logical nogeom,nomate,nosec,nohits,nodigi,nobosout
      logical nodata,nomcdata,fast_code
c
      common/gsimcontrol/
     1     noffread,nofield,rawbanks_in,
     1     nogeom(max_detector),nomate(max_detector),
     1     nosec(max_detector),nohits(max_detector),
     1     nodigi(max_detector),nobosout(max_detector),
     1     nodata(max_detector),nomcdata(max_detector),
     1     helium_only,fast_code(max_detector)

	
