#include <rpc/rpc.h>
#include "j.h"


bool_t
xdr_bdata(xdrs, objp)
	XDR *xdrs;
	bdata *objp;
{
	if (!xdr_int(xdrs, &objp->type)) {
		return (FALSE);
	}
	switch (objp->type) {
	case 0:
		if (!xdr_array(xdrs, (char **)&objp->bdata_u.iraw.iraw_val, (u_int *)&objp->bdata_u.iraw.iraw_len, ~0, sizeof(int), xdr_int)) {
			return (FALSE);
		}
		break;
	case 1:
		if (!xdr_array(xdrs, (char **)&objp->bdata_u.sraw.sraw_val, (u_int *)&objp->bdata_u.sraw.sraw_len, ~0, sizeof(short), xdr_short)) {
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}




bool_t
xdr_aBank(xdrs, objp)
	XDR *xdrs;
	aBank *objp;
{
	if (!xdr_string(xdrs, &objp->name, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->format, ~0)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nrows)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->ncols)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->d_size)) {
		return (FALSE);
	}
	if (!xdr_array(xdrs, (char **)&objp->bd.bd_val, (u_int *)&objp->bd.bd_len, ~0, sizeof(bdata), xdr_bdata)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_mstr(xdrs, objp)
	XDR *xdrs;
	mstr *objp;
{
	if (!xdr_string(xdrs, objp, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_Event(xdrs, objp)
	XDR *xdrs;
	Event *objp;
{
	if (!xdr_int(xdrs, &objp->errcode)) {
		return (FALSE);
	}
	if (!xdr_mstr(xdrs, &objp->s)) {
		return (FALSE);
	}
	if (!xdr_array(xdrs, (char **)&objp->b.b_val, (u_int *)&objp->b.b_len, ~0, sizeof(aBank), xdr_aBank)) {
		return (FALSE);
	}
	return (TRUE);
}


