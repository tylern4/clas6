
typedef struct PFrag {
  int     nr;
  int     nrows ;
  int     ncols;
  int     d_size;
  int     type ;
  char  * format;
  char  * name;
  int   * in;
  short * sn;
  struct PFrag * next;
} PFrag;

typedef struct CtlBlk {
  PFrag * head;
  PFrag * tail;
  PFrag * cursor;
  int     nItems;
} CtlBlk;

void lst_error () ;
int lst_init () ;
PFrag * get_pfrag () ;
int lst_add () ;
void lst_drop () ;
int lst_count () ;
PFrag * lst_nxt () ;
