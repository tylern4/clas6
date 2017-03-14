/*
 * map_readonly_stub.c
*/

static const char sccsid[] = "@(#)"__FILE__"\t4.1\tCreated 12/10/95 17:48:31, \tcompiled "__DATE__;

#include <stdio.h>

int map_create(void)
{
  fprintf(stderr,"map_create: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int map_put_float(void)
{
  fprintf(stderr,"map_put_float: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int mapf_put_float_(void)
{
  fprintf(stderr,"map_put_float: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int map_put_int(void)
{
  fprintf(stderr,"map_put_int: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int mapf_put_int_(void)
{
  fprintf(stderr,"map_put_int: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int map_add_item(void)
{
  fprintf(stderr,"map_add_item: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int mapf_add_item_(void)
{
  fprintf(stderr,"map_add_item: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

int mapf_rem_item_(void)
{
  fprintf(stderr,"map_rem_item: Stub: This function is not supported by the READ-ONLY version of the map manager library.\n");
  return -1;
}

/* end file */
