/* 
 *  converting utilities
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "calib_envir.h"


/*   time test  */
#include <time.h>
#include <sys/types.h>
time_t t1,t2;
/*   time test  */


int calib_int2value(int itemlength, int array[], char **valuestring)
{
	int k, status=0;
	int nc;
	char value_one[100];
	char value[LEN_VALUE];
	/*  time(&t1); */
	sprintf(value, "%i", array[0]);
	/*  time(&t2); printf("next time= %d seconds\n", (int) t2-t1); */
	for (k = 1; k < itemlength; k++) {
		sprintf(value_one, "%i", array[k]);
		strcat(value, ", ");
		strcat(value, value_one);
	}
	*valuestring=strdup(value);
	return(status);
}

int calib_value2float(int *itemlength, float array[], valuestruc tlvalue )
{
	int k, status=0;
	*itemlength=tlvalue.length;
	for (k = 0; k < *itemlength; k++) {
		*(array + k)=strtod(tlvalue.item[k],(char **)NULL);
	}
	return(status);
}

int calib_value2int(int *itemlength, int array[], valuestruc tlvalue )
{
	int k, status=0;
	*itemlength=tlvalue.length;
	for (k = 0; k < *itemlength; k++) {
		*(array + k)=strtod(tlvalue.item[k],(char **)NULL);
	}
	return(status);
}

int calib_char2value( int itemlength, char *carray, char **valuestring)

{
	int status=0;
	char *value;
	value = (char *) malloc(strlen(carray) + 3);
	sprintf(value, "\'%s\'", carray);
	printf("calib_char2value: value = %s\n", value);
	*valuestring=strdup(value);
	free(value);
	return(status);
}

int calib_float2value(int itemlength, float array[], char **valuestring)

{
	int k, status=0;
	char value_one[100];
	char value[LEN_VALUE];
	sprintf(value, "%f", array[0]);
	for (k = 1; k < itemlength; k++) {
		sprintf(value_one, "%f", array[k]);
		strcat(value, ", ");
		strcat(value, value_one);
	}
	*valuestring=strdup(value);
	return(status);
}

