c  gsimout.inc

c  $Id: gsimout.inc,v 1.2 1997/04/24 15:38:36 holtrop Exp $

      character*132 cigsimout
      data cigsimout/
     1'$Id: gsimout.inc,v 1.2 1997/04/24 15:38:36 holtrop Exp $'
     1/

	integer gsimout_unit
	character*120 gsimout_filename,gsimout_daname

      	common/gsimout/gsimout_unit,gsimout_filename,
     1      gsimout_daname


