c
c       map_manager.inc user include file for use with FORTRAN map routines
c         Created           :  9-Nov-1993 by Thom Sulanke
c
 
c map_manager error codes

	integer MAP_OK
	parameter (MAP_OK = 0)		! no errors

	integer MAP_USER_ERROR_ARGUMENT
	parameter (MAP_USER_ERROR_ARGUMENT = -1) 
					! user supplied arguement incorrect 

	integer MAP_USER_WARN_NOREPEAT
	parameter (MAP_USER_WARN_NOREPEAT = -2) 
			! previously performed operation is not repeated

	integer MAP_USER_WARN_NOFIND
	parameter (MAP_USER_WARN_NOFIND = -3) ! time not found for item

	integer MAP_USER_WARN_NOMATCH
	parameter (MAP_USER_WARN_NOMATCH = -4) 
				! subsystem or item name no found

	integer MAP_USER_WARN_NOREPLACE
	parameter (MAP_USER_WARN_NOREPLACE = -5)
			! values not replaced for previously filled time 
 
	integer MAP_SYSTEM_ERROR_OPEN
	parameter (MAP_SYSTEM_ERROR_OPEN = -101) 
				! system error opening map file

	integer MAP_SYSTEM_ERROR_IO
	parameter (MAP_SYSTEM_ERROR_IO = -102) 
				! system file i/o error after opening 
	
	integer MAP_SYSTEM_ERROR_MEMORY
	parameter (MAP_SYSTEM_ERROR_MEMORY = -103) 
				! system error allocating/deallocating memory

c	fuction names

	integer map_add_item, map_put_int, map_put_float, map_get_int,
     &	 map_get_float, map_fnd_int, map_fnd_flo, map_stat_item,
     &   map_rem_item, map_rem_sub, map_rem_arr, map_dump, map_log_mess
