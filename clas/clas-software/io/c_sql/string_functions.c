#include "csql.h"

void string_shift(char *line, int npos,int max)
{
  int i = npos;
  while(line[i]!='\0'&&i<max)
    {
      line[i] = line[i+1];
      i++;
    }
}


void string_space_clean(char *line,int maxlen)
{
  int i;
  for(i=0;i<strlen(line);i++) if(line[i]=='\t') line[i] = ' ';
  while(line[0]==' ') string_shift(line,0,maxlen);
  
  
  for(i = 0; i<strlen(line);i++)
    {
      if(line[i]==' ')
	{
	  while(line[i+1]==' ') string_shift(line,i+1,maxlen);
	}
    }
  if(line[strlen(line)-1]==' ') line[strlen(line)-1] = '\0';
}

void string_get_token(char *line,int ntoken,char *dst)
{
  int indx = 0;
  int cicl;
  int n_spaces;
  if(ntoken>0){
      n_spaces = 0;
      while(n_spaces<ntoken&&indx<strlen(line)){
	  if(line[indx++]==' ') n_spaces++;}
  }
  if(indx==strlen(line)){
    /*    printf("There is no tocken # %d\n",ntoken); */
    dst[0] = '\0';
    return;
  }

  /*  printf("Token #%d starts at  %d FIRST CHAR IS [%c]\n",ntoken,indx,line[indx]); */
  cicl = 0;
  while(line[indx]!=' '&&indx<=strlen(line)) dst[cicl++] = line[indx++];
  dst[cicl] = '\0';
}

int  string_get_int_token(char *line,int ntoken)
{
  int tvalue;
  char token[32];
  string_get_token(line,ntoken,token);
  tvalue = atoi(token);
  /*  printf("For %s got value %d\n",token,tvalue); */
  return tvalue;
}


int  string_cmp_token(char *line, int ntoken,const char *exp)
{
  int  exp_len;
  char token[64];
  
  exp_len = strlen(exp);
  string_get_token(line,ntoken,token);
  return strcmp(token,exp);
}

int  string_get_ntokens(char *line)
{
  int n_tokens = 0;
  int i;
  for(i=0;i<strlen(line);i++)
    {
      if(line[i]==' ') n_tokens++;
    }
  return n_tokens+1;
}

void string_append(char *line, const char *str)
{
   strcat(line,str);
}
