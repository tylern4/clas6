c  ectmed.inc

c  $Id: ectmed.inc,v 1.3 1998/01/22 19:15:52 lcsmith Exp $

      character*132 ciectmed
      data ciectmed/
     1'$Id: ectmed.inc,v 1.3 1998/01/22 19:15:52 lcsmith Exp $'
     1/

	integer ec_med_air,ec_med_helium,ec_med_vac
	integer ec_med_scint, ec_med_lead, ec_med_steel
        integer ec_med_cvp_air,ec_med_foam
	common/ectmed/ec_med_air,ec_med_helium,ec_med_vac,
     1       ec_med_scint,ec_med_lead,ec_med_steel,
     1       ec_med_cvp_air,ec_med_foam
     
