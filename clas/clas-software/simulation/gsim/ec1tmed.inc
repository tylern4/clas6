c  ec1tmed.inc

c  $Id: ec1tmed.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $

      character*132 ciec1tmed
      data ciec1tmed/
     1'$Id: ec1tmed.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $'
     1/

	integer ec1_med_air,ec1_med_vac
	integer ec1_med_scint, ec1_med_lead, ec1_med_steel
        integer ec1_med_cvp_air
	common/ec1tmed/ec1_med_air,ec1_med_vac,
     1       ec1_med_scint,ec1_med_lead,ec1_med_steel,
     1       ec1_med_cvp_air
     
