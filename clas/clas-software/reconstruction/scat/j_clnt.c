#include <rpc/rpc.h>
#include <sys/time.h>
#include "j.h"

#ifdef HPUX

#ifndef NULL
#define NULL  0
#endif /* NULL */

#endif /* hpux */

static struct timeval TIMEOUT = { 25, 0 };

mstr *
svcname_1(argp, clnt)
	void *argp;
	CLIENT *clnt;
{
	static mstr res;
	static struct timeval SpecialTimeOut = { 5, 0 };
	memset(&res, 0, sizeof(res));

	if (clnt_call(clnt, SVCNAME, xdr_void, argp, xdr_mstr, &res, SpecialTimeOut) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
msgnoreply_1(argp, clnt)
	Event *argp;
	CLIENT *clnt;
{
	static int res;

	memset(&res, 0, sizeof(res));

	if (clnt_call(clnt, MsgNoReply, xdr_Event, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


Event *
msgwreply_1(argp, clnt)
	Event *argp;
	CLIENT *clnt;
{
	static Event res;

	memset(&res, 0, sizeof(res));

	if (clnt_call(clnt, MsgWReply, xdr_Event, argp, xdr_Event, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
browcreate_1(argp, clnt)
	Event *argp;
	CLIENT *clnt;
{
	static int res;

	memset(&res, 0, sizeof(res));

	if (clnt_call(clnt, BrowCreate, xdr_Event, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
browdestroy_1(argp, clnt)
	Event *argp;
	CLIENT *clnt;
{
	static int res;

	memset(&res, 0, sizeof(res));
	if (clnt_call(clnt, BrowDestroy, xdr_Event, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


Event *
browfilter_1(argp, clnt)
	Event *argp;
	CLIENT *clnt;
{
	static Event res;

	memset(&res, 0, sizeof(res));
	if (clnt_call(clnt, BrowFilter, xdr_Event, argp, xdr_Event, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

