#define ROUTINE_LENGTH 64
#define LEVEL_LENGTH 1
#define MESSAGE_LENGTH 256

#include <string.h>

extern "C" void recmes_wrap_(char routine[], char level[], char message[]);

void fortranize(char *string, int length, char char_array[]) {
  int ichar, string_length;
  string_length = strlen(string);
  for (ichar = 0; ichar < string_length; ichar++) {
    char_array[ichar] = string[ichar];
  }
  for (ichar = string_length; ichar < length; ichar++) {
    char_array[ichar] = ' ';
  }
}

void recmes(char *routine, char *level, char *message) {
  char routine_out[ROUTINE_LENGTH], level_out[LEVEL_LENGTH],
    message_out[MESSAGE_LENGTH];
  fortranize(routine, ROUTINE_LENGTH, routine_out);
  fortranize(level, LEVEL_LENGTH, level_out);
  fortranize(message, MESSAGE_LENGTH, message_out);
  recmes_wrap_(routine_out, level_out, message_out);
}
