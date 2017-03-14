#include <stdio.h>

#include <st.h>

extern stgeom_t stgeom_;

stgeom_print_()
{
  fprintf(stderr,"ST Z LIMITS: %f %f\n",stgeom_.nose_z_min,stgeom_.nose_z_max);
}
