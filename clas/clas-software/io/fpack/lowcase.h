/* on APOLLO and ALLIANT 'tolower' is a macro, not a function,
   and its behaviour is undefined if the argument is not in
   upper case, hence...                                       */
#ifndef TOLOWER
#define TOLOWER(x) tolower(x)
#endif
