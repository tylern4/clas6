
union  bdata switch (int type) {
case 0:
	int   iraw<>;
case 1:
	short sraw<>;
default:
        void ;
};

struct aBank {
	string name<> ;
	string format<>;
	int    nr ;
	int    nrows;
	int    ncols;
	int    d_size;
	bdata  bd<> ;
};	

typedef string mstr<> ;

struct Event {
	int    errcode;
        mstr   s;
        aBank  b<>;
} ;

program EVTSVC_INP {
        version IVER {
        mstr        SVCNAME     (void)  = 1 ;
        int         MsgNoReply  (Event) = 2 ;
        Event       MsgWReply   (Event) = 3 ;

        int         BrowCreate  (Event)  = 4 ;
        int         BrowDestroy (Event)  = 5 ;
        Event       BrowFilter  (Event)  = 6 ;
        } = 1;
} = 0x20000033 ;



