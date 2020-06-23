/* 
 *        Map_wrap.c: Map like wrapers for use with MySQL C API routines
 *        Created   :         25-MAY-2000 by Riad Suleiman
 */

#include <stdio.h>
#include <libgen.h>
#include <string.h>
#include <stdlib.h>


char* MapFile2Table(char filename[])
{
  static char fname[256];
  char *return_ptr, *final_ptr, *path_ptr;

  strcpy(fname, filename);

  final_ptr = strstr(fname,".map");
  if (final_ptr) *final_ptr=0; /* chop off .map file extension */
  path_ptr = strrchr(fname, '/');
  if (path_ptr) {
    return_ptr = path_ptr + 1; /* if path is present, return first character
				  after the path */
  } else {
    return_ptr = fname; /* otherwise simply return the first character of
			   fname */
  }
  return return_ptr;
}

/*   */

int map_stat_item(char filename[], const char subsystemname[],
		    const char itemname[], int *arraylength, int *arraytype)
{
  int status;

  status = calib_stat_item(MapFile2Table(filename), subsystemname, itemname, 
			   arraylength, arraytype);
  return(status);
}

/*   */

int map_get_char(char filename[], const char subsystemname[], 
		 const char itemname[], int arraylength, char carray[], 
		 int atime, int *firsttime, int *olength)
{
  int status;
  
  status = calib_get_char(MapFile2Table(filename), subsystemname, itemname, arraylength, carray, 
			  atime, firsttime, olength);
  return(status);
}

/*   */

int map_get_float(char filename[], const char subsystemname[], 
		    const char itemname[], int arraylength, float farray[], 
		    int atime, int *firsttime)
{
  int status;
  char carray[200000],*ptr;
  int olength;
  int k;
  
  if (arraylength<=3398) {
	  status = calib_get_float(MapFile2Table(filename), subsystemname, itemname, 
				   arraylength, farray, atime, firsttime);
  } else {    
    status = calib_get_char(MapFile2Table(filename), subsystemname, itemname, 
			    arraylength, carray, atime, firsttime, &olength);   
    ptr=carray;
    while(isspace(*ptr))ptr++; /* drop leading spaces */
    for (k=0; k<arraylength; k++) {
      farray[k] = atof(ptr);
      while(!isspace(*ptr))ptr++;
      while(isspace(*ptr))ptr++;
    }
  }
  return(status);
}

/*   */

int map_get_int(char filename[], const char subsystemname[], 
		      const char itemname[], int arraylength, int array[], 
		      int atime, int *firsttime)
{
  int status;
  char carray[50000],*ptr;
  int olength;
  int k;
  
  if (arraylength<=3398) {
    status = calib_get_int(MapFile2Table(filename), subsystemname, itemname, 
			   arraylength, array, atime, firsttime);
  } else {    
    status = calib_get_char(MapFile2Table(filename), subsystemname, itemname, 
			    arraylength, carray, atime, firsttime, &olength);   
    ptr=carray;
    while(isspace(*ptr))ptr++;
    for (k=0; k<arraylength; k++) {
    	array[k] = atoi(ptr);
	while(!isspace(*ptr))ptr++;
	while(isspace(*ptr))ptr++;
    }
  }
  
  return(status);
}

/*   */

int map_put_char(char filename[], const char subsystemname[], 
		   const char itemname[], int arraylength, 
		   const char carray[], int firsttime)
{
  int status;
  
  status = calib_put_char(MapFile2Table(filename), subsystemname, itemname, arraylength, carray, 
			  firsttime);
  return(status);
}

/*   */

int map_put_float(char filename[], const char subsystemname[], 
		    const char itemname[], int arraylength, 
		    const float farray[], int firsttime)
{
  int status;
  char carray[200000];
  char floatstr[20];
  int k;
  
  if (arraylength<=3398) {
    status = calib_put_float(MapFile2Table(filename), subsystemname, itemname, 
			     arraylength, farray, firsttime);
  } else {
    sprintf(carray, "%f", farray[0]);
    for (k = 1; k < arraylength; k++) {
      sprintf(floatstr, "%f", farray[k]);
      strcat(carray, " ");
      strcat(carray, floatstr);
    }
    status = calib_put_char(MapFile2Table(filename), subsystemname, itemname, 
			    arraylength, carray, firsttime);
  }
  return(status);
}
 
/*   */
 
int map_put_int(char filename[], const char subsystemname[], 
		const char itemname[], int arraylength, const int array[], 
		int firsttime)
{
  int status;
  char carray[200000];
  char intstr[10];
  int k;

  if (arraylength<=3398) {
    status = calib_put_int(MapFile2Table(filename), subsystemname, itemname, 
			   arraylength, array, firsttime);
  } else {
    sprintf(carray, "%i", array[0]);
    for (k = 1; k < arraylength; k++) {
      sprintf(intstr, "%i", array[k]);
      strcat(carray, " ");
      strcat(carray, intstr);
    }
    status = calib_put_char(MapFile2Table(filename), subsystemname, itemname, 
			    arraylength, carray, firsttime);
  }
  
  return(status);   
}

/*===================
   Fortran wrappers
  ===================*/
int map_get_float_(char *filename, char *subsystemname,
                  char *itemname, int *arraylength, float *array, 
                  int *atime, int *firsttime,
                  int filename_len, int subsystemname_len, int itemname_len)
{
	char fname[256],subsys[256],item[256];

	bzero(fname,256); bzero(subsys,256); bzero(item,256);
	strncpy(fname ,filename     ,filename_len);
	strncpy(subsys,subsystemname,subsystemname_len);
	strncpy(item  ,itemname     ,itemname_len);
	
	return map_get_float(fname, subsys, 
		item, *arraylength, array, *atime,firsttime);
}

int map_get_int_(char *filename, char *subsystemname,
                  char *itemname, int *arraylength, int *array, 
                  int *atime, int *firsttime,
                  int filename_len, int subsystemname_len, int itemname_len)
{
	char fname[256],subsys[256],item[256];

	bzero(fname,256); bzero(subsys,256); bzero(item,256);
	strncpy(fname ,filename     ,filename_len);
	strncpy(subsys,subsystemname,subsystemname_len);
	strncpy(item  ,itemname     ,itemname_len);

	return map_get_int(fname, subsys, 
		item, *arraylength, array, *atime, firsttime);
}

/*==========================================================================
   The following are some dummy routines that allow linking in a1/recsis
  ==========================================================================*/


void Exit_Without_Prejudice(char *routine_name)
{
  /*

	fprintf(stderr,"\nIllegal attempt to use map manager function (%s) not in\n"
		,routine_name);
	fprintf(stderr,"attempting to continue...\n\n");
  */
}

void Exit_With_Prejudice(char *routine_name)
{
	fprintf(stderr,"\nIllegal attempt to use map manager function (%s) not in\n"
		,routine_name);
	fprintf(stderr,"libcaldb.a! Exiting.\n\n");
	exit(-1);
}


int map_add_item(char filename[], const char subsystemname[], 
            const char itemname[], int arraylength, int arraytype)
{Exit_Without_Prejudice("map_add_item");}


int map_rem_arr(char filename[], const char subsystemname[], 
            const char itemname[], int firsttime)
{Exit_Without_Prejudice("map_rem_arr");}


int     map_create(char filename[])
{Exit_With_Prejudice("map_create");}


int map_close_map( int fd )
{Exit_With_Prejudice("map_close_map");}


int map_log_mess(const int  error_code,  const  int  log)
{Exit_Without_Prejudice("map_log_mess");}
