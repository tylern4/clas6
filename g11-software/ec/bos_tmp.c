
/* temporary interface from bosio to bos */

int bosNNlink(int *jw, char *name, int nr)
{
  int status;

  status = mlink_(jw, name, &nr, 4);
  return(status);  
}

int bosNNcreate(int *jw, char *name, int nr, int ncol, int nrow)
{
  int status;

  status = mbank_(jw, name, &nr, &ncol, &nrow,4);
  return(status);  
}

