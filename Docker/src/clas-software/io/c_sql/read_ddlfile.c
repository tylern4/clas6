/********************************************************
 *   This subroutin reads DDL file given in arguments
 *   and returns array of names and types of columns
 *   
 *   Author: G.Gavalian
 ********************************************************/

#include <stdio.h>
#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include "csql.h"

/******************************************************************
 *   This returns position of character "sym" in string "src".
 *   If "src" does not contain character "sym" -1 will be returned
 ******************************************************************/

int read_ddlfile2(const char *filename)
{
  FILE *fp;
  char  line[256];
  int   n_tokens;
  char  token[32];
  char  type[12];
  char  col_type_str[32];
  int   col_type;
  int   group_created_flag = 0;
  char  group_name[5];
  

  printf("Openning file : %s\n",filename);

  if(access(filename,R_OK)!=0){
    printf("ERROR: error opening file %s.check the path\n",filename);
    return;
  }

  fp = fopen(filename,"r");
    while(fgets(line,256,fp) != NULL)
    {
      string_space_clean(line,256);
      n_tokens = string_get_ntokens(line);
      if(n_tokens>2&&line[0]!='!'){
	if(group_created_flag == 0)
	  {
	    string_get_token(line,0,token);
	    if(strcmp(token,"TABLE")==0){
	      string_get_token(line,1,token);
	      strncpy(group_name,token,4); group_name[4] = '\0';
	      printf("READ_DDLFILE: Found a group %s\n",group_name);
	      add_group(group_name);
	      group_created_flag++;
	    }
	  }
	if(group_created_flag == 1)
	  {
	    if(string_get_int_token(line,0)>0)
	      {
		string_get_token(line,1,token);
		string_get_token(line,2,type);
		if(strncmp(type,"I",1)==0||strncmp(type,"B",1)==0){ col_type = COL_TYPE_INT; strcpy(col_type_str,"INT");} 
		else { col_type = COL_TYPE_FLOAT; strcpy(col_type_str,"FLOAT");};
		add_column(group_name,token);
		init_column(group_name,token,col_type_str,col_type,COL_CW_TBL);
		/*		init_column(); */
		if(col_type==COL_TYPE_INT)
		set_column_int(group_name,token,0); else set_column_float(group_name,token,0.);
		/*		printf("Adding line into group: %s\n",line); */
	      }
	  }
	/*	printf("%s",line); */
      }
      
    }

    fclose(fp);
    return 42;
}


int  getcharpos(char *src, char sym){
int indx = 0;
 while((src[indx] != sym)&&(indx < strlen(src))) indx++;
 if(src[indx] == sym) return indx;
 return -1;
}
