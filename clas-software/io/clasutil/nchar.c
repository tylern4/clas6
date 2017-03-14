int nchar(char *str,char c) 
{
  /* count the number of times c appears in str */
  int ret = 0;
  while (*str) {
    if (*str++ == c)
      ret++;
  }
  return (ret);
}
