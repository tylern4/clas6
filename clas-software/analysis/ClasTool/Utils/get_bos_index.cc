#include <string.h>

extern "C" {
  void make_bos_call_(int *array_key, char *bankname_fortran, int *bank_index, int *bank_address);
}

int get_bos_index(int array_key, char *bankname, int bank_index) {
  char bankname_fortran[4];
  const char blank = ' ';
  int i, length, ind;
  length = strlen(bankname);
  for (i = 0; i < length; i++) bankname_fortran[i] = bankname[i];
  for (i = length; i < 4; i++) bankname_fortran[i] = blank;
  make_bos_call_(&array_key, bankname_fortran, &bank_index, &ind);
  return (ind);
}
