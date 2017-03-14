/*
 * data.c
 *
*/


#include <stdio.h>
#ifdef Darwin
#include <sys/malloc.h>
#else
#include <malloc.h>
#endif
#include <string.h>
#include <time.h>
#include <limits.h>

#include <itape.h>
#include <alarm.h>

int data_check_crc = 0;

int tmask(unsigned long mask,unsigned long m)
{
  int trigger = (int)m;
  if (trigger > 8) trigger = (-1);  /* just in case we lost the sign somewhere... */
  return (int)mask & (1<<(trigger+1)); 
}

int data_newItape(void* ptr)
{
  itape_header_t *ih = ptr;
  ih->length = sizeof(itape_header_t);
  ih->type = TYPE_ITAPE;
  ih->transComputerCode = TRANSCOMPUTERCODE;
  ih->ngroups = 0;
  ih->runNo = 0;
  ih->spillNo = 0;
  ih->eventNo = 0;
  ih->trigger   = 0;
  ih->eventType = 0;
  ih->time = time(NULL);
  ih->latch = 0;
  return 0;
}

group_header_t* data_getGroupHeader(const void*buffer,uint32 group)
{
  const itape_header_t *ih = buffer;
  const group_header_t *gh;
  int sz;
  int i,n;

  if (ih==NULL) return NULL;

  sz = ih->length;
  n  = ih->ngroups;
  gh = (group_header_t*)(((char*)ih) + sizeof(itape_header_t));
  sz -= sizeof(itape_header_t);
  for (i=0; (sz>0) && (i<n); i++)
    {
      unsigned int length = gh->length;

      if (length == 0)
	{
	  fprintf(stderr,"data_getGroup: Itape broken: group id(%d),size(%d) has length zero\n",gh->type,gh->length);
	  esend(ALARM_ERROR,"data_getGroup: Itape broken","group id(%d),size(%d) has length zero\n",gh->type,gh->length);
	  return NULL;
	}

      if (length > sz)
	{
	  fprintf(stderr,"data_getGroup: Itape broken: group id(%d),size(%d) bigger than itape (size=%d)\n",gh->type,gh->length,sz);
	  esend(ALARM_ERROR,"data_getGroup: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",gh->type,gh->length,sz);
	  return NULL;
	}

      if (length&3) /* check that we stay aligned at 4 bytes */
	{
	  fprintf(stderr,"data_getGroup: Itape broken: group id(%d),size(%d) length is not multiple of 4 bytes.\n",gh->type,gh->length);
	  esend(ALARM_ERROR,"data_getGroup: Itape broken","group id(%d),size(%d) length is not multiple of 4 bytes.\n",gh->type,gh->length);
	  return NULL;
	}

      if (gh->type == group) return (group_header_t*)gh;

      gh = (group_header_t*)(((char*)gh) + length);
      sz -= length;
    }

  if ((i != n) || (sz != 0))
    {
      fprintf(stderr,"data_getGroup: Itape broken: group id(%d),size(%d) bigger than itape (size=%d)\n",gh->type,gh->length,sz);
      esend(ALARM_ERROR,"data_getGroup: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",gh->type,gh->length,sz);
    }

  return NULL;
}

void* data_getGroup(const void*buffer,uint32 group)
{
  char*p = (char*) data_getGroupHeader(buffer,group);
  if (p==NULL) return NULL;
  p += sizeof(group_header_t);
  return p;
}

unsigned long data_getGroupSize(const void*buffer,uint32 group)
{
  group_header_t *p = data_getGroupHeader(buffer,group);
  if (p==NULL) return ULONG_MAX;
  return p->length - sizeof(group_header_t);
}

group_header_t *data_addGroup_header(void *buffer,int bufsize,uint32 type,uint32 length)
{
  itape_header_t *ih = buffer;
  group_header_t *gh;
  int i;

  if (ih==NULL) return NULL;

  data_removeGroup(ih,type);

  if (ih->length + sizeof(group_header_t) + length>=bufsize)
    {
      fprintf(stderr,"data_addGroup: Buffer overflow: buffer length is %d, used %d, create group type %d length %d\n",bufsize,ih->length,type,length);
      esend(ALARM_ERROR,"data_addGroup: Buffer overflow","buffer length is %d, used %d, create group type %d length %d\n",bufsize,ih->length,type,length);
      return NULL;
    }

  gh = (void*)((char*)ih + sizeof(*ih));
  for (i=0; i<ih->ngroups; i++)
    {
      gh = (void*)((char*)gh + gh->length);
    }
/*
  gh = (group_header_t*) (p + ih->length);
*/
  gh->length = (unsigned int)length;
  gh->type = (unsigned int)type;

  ih->length += (uint32)sizeof(group_header_t) + length;
  ih->ngroups ++;

  return gh;
}

void *data_addGroup(void *buffer,int bufsize,uint32 type,uint32 length)
{
  itape_header_t *ih = buffer;
  char *p = buffer;
  group_header_t *gh;

  if (ih==NULL) return NULL;

  data_removeGroup(buffer,type);

  if ((length%4)!=0)
    {
      /* align everything to a 4 byte boundary */

      length = ((length/4) + 1)*4;
    }

  if (ih->length + sizeof(group_header_t) + length>=bufsize)
    {
      fprintf(stderr,"data_addGroup: Buffer overflow: buffer length is %d, used %d, create group type %d length %d\n",bufsize,ih->length,type,length);
      esend(ALARM_ERROR,"data_addGroup: Buffer overflow","buffer length is %d, used %d, create group type %d length %d\n",bufsize,ih->length,type,length);
      return NULL;
    }

  gh = (group_header_t*) (p + ih->length);
  gh->length = (unsigned int)sizeof(group_header_t) + (unsigned int)length;
  gh->type = (unsigned int)type;

  ih->length += (uint32)sizeof(group_header_t) + length;
  ih->ngroups ++;

  return ((char*)gh)+sizeof(group_header_t);
}

void *data_getAdr(const void *buffer)
{
  const itape_header_t *ih = buffer;
  return ((char*)buffer + ih->length + sizeof(group_header_t));
}

int data_addCRC(void *buffer)
{
  unsigned char *p = buffer;
  itape_header_t *ih = buffer;
  uint32 crc = data_crc(ih,ih->length);
  p[ih->length] = (unsigned char)(crc & 0xff);
  p[ih->length+1] = (unsigned char)((crc>>8) & 0xff);
  p[ih->length+2] = (unsigned char)((crc>>16) & 0xff);
  p[ih->length+3] = (unsigned char)((crc>>24) & 0xff);
  return 0;
}     

int data_checkCRC1(const void *buffer,int bufferLength)
{
  if (data_check_crc)
    {
      const unsigned char *p = buffer;
      uint32 crc = data_crc(buffer,bufferLength);
      uint32 oldcrc = p[bufferLength] | p[bufferLength+1]<<8 |
	p[bufferLength+2]<<16 | p[bufferLength+3]<<24;
/*
fprintf(stderr,"crc: new: 0x%x data: 0x%x\n",crc,oldcrc);
*/
      return crc!=oldcrc;
    }
  else return 0;
}     

int data_checkCRC(const void *buffer)
{
  if (data_check_crc)
    {
      const itape_header_t *ih = buffer;

      return data_checkCRC1(ih,ih->length);
    }
  else return 0;
}     

int data_clean(void* buffer)
{
  char* wptr = buffer;
  const char* rptr = buffer;
  itape_header_t* ih = buffer;
  group_header_t* gh = buffer;
  uint32 sz;
  uint32 count;
  uint32 size;

  count = 0;
  size = ih->length;

  if (size < sizeof(itape_header_t))
    {
      fprintf(stderr,"data_clean: Itape broken (0): Itape is too short (size=%d)\n",size);
      esend(ALARM_ERROR,"data_clean: Itape broken","Itape is too short (size=%d)\n",size);
      return -1;
    }

  rptr += sizeof(itape_header_t);
  size -= sizeof(itape_header_t);

  gh = (void*) rptr;
  while (gh->type!=0)
    {
      uint32 length = gh->length;

      if ((length == 0)||(length > size))
	{
	  fprintf(stderr,"data_clean: Itape broken (1): group id(%d),size(%d) bigger than itape (size=%d)\n",
		  gh->type,length,size);
	  esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",
		gh->type,length,size);
	  return -1;
	}

      if (length&3)
	{
	  fprintf(stderr,"data_clean: Itape broken (1): group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		  gh->type,length,size);
	  esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		gh->type,length,size);
	  return -1;
	}

      rptr += length;
      size -= length;
/*
printf("1: type %d len %d, size %d\n",gh->type,gh->length,size);
*/
      gh = (void*) rptr;
      count ++;

      if (size==0)
	return 0;
    }

  if (size==0)
    return 0;

  wptr = (void*)rptr;

  while (size>0)
    {
      while (gh->type == 0)
	{
	  uint32 length = gh->length;

	  if ((length == 0)||(length > size))
	    {
	      fprintf(stderr,"data_clean: Itape broken (2): group id(%d),size(%d) bigger than itape (size=%d)\n",
		      gh->type,gh->length,size);

	      esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",
		    gh->type,gh->length,size);
	      return -1;
	    }

	  if (length&3)
	    {
	      fprintf(stderr,"data_clean: Itape broken (2): group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		      gh->type,length,size);
	      esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		    gh->type,length,size);
	      return -1;
	    }

	  rptr += length;
	  size -= length;
/*
printf("2: type %d len %d, size %d\n",gh->type,gh->length,size);
*/
	  gh = (void*) rptr;
	  if (size==0)
	    break;
	}

      if (size==0) break;
/*
printf("3: type %d len %d, size %d\n",gh->type,gh->length,size);
*/
      sz = gh->length;

      if ((sz == 0)||(sz > size))
	{
	  fprintf(stderr,"data_clean: Itape broken (3): group id(%d),size(%d) bigger than itape (size=%d)\n",
		  gh->type,gh->length,size);
	  
	  esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",
		gh->type,gh->length,size);
	  return -1;
	}

      if (sz&3)
	{
	  fprintf(stderr,"data_clean: Itape broken (3): group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		  gh->type,gh->length,size);
	  esend(ALARM_ERROR,"data_clean: Itape broken","group id(%d),size(%d) length is not a multiple of 4 (size=%d)\n",
		gh->type,gh->length,size);
	  return -1;
	}

#ifdef UNDEF /* this was a crashing memmove() scare... */

      if ((rptr >= wptr)&&(rptr <= wptr + sz))
	memmove(wptr,rptr,sz);
      else
	memcpy(wptr,rptr,sz);
#endif

      memmove(wptr,rptr,sz);

      wptr += sz;
      rptr += sz;
      size -= sz;
      count ++;
      gh = (void*) rptr;
    }

  ih->ngroups = count;
  ih->length = (unsigned int)((char*)wptr - (char*)buffer);

  if (ih->length&3)
    {
      fprintf(stderr,"data_clean: Itape broken (4): Itape length is not a multiple of 4 (size=%d)\n",
	      ih->length);
      esend(ALARM_ERROR,"data_clean: Itape broken","Itape length is not a multiple of 4 (size=%d)\n",
	    ih->length);
      return -1;
    }

  return 0;
}

int data_removeGroup(void* buffer,uint32 group)
{
  while (1)
    {
      group_header_t* gh = data_getGroupHeader(buffer,group);
      if (gh==NULL) return 1;
      gh->type = 0;
    }
}

int data_saveGroups(void *buffer,int nsave,int *isave)
{
  char*p = buffer;
  itape_header_t *h = buffer;
  group_header_t *g;
  int sz;
  int i,n;
  int isv,save;

  if (h==NULL) return 0;

  sz = h->length;
  n = h->ngroups;
  p += sizeof(itape_header_t);
  sz -= sizeof(itape_header_t);
  for (i=0; i<n; i++) {
      g = (group_header_t*) p;
      /* if g is in the list to save - don't remove it */
      for (save = 0,isv = 0; isv < nsave; ++isv) {
	if (g->type == isave[isv])
	  save = 1;
      }
      if (!save) 
	g->type = 0;
      if ((g->length>sz)||(g->length==0)) {
	  fprintf(stderr,"data_saveGroups: Itape broken: group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  esend(ALARM_ERROR,"data_saveGroups: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  return -1;
	}
    
      p += g->length;
      sz -= g->length;
    }

  return 0;
}

int data_removeAllgroups(void *buffer)
{
  char*p = buffer;
  itape_header_t *h = buffer;
  group_header_t *g;
  int sz;
  int i,n;

  if (h==NULL) return 0;

  sz = h->length;
  n = h->ngroups;
  p += sizeof(itape_header_t);
  sz -= sizeof(itape_header_t);
  for (i=0; i<n; i++) {
      g = (group_header_t*) p;
      g->type = 0;
      if ((g->length>sz)||(g->length==0)) {
	  fprintf(stderr,"data_removeAllgroups: Itape broken: group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  esend(ALARM_ERROR,"data_removeAllgroups: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  return -1;
	}
      p += g->length;
      sz -= g->length;
    }
  return 0;
}

void* data_getItape(const void*buffer,const void**nextItape)
{
  const itape_header_t *ih = buffer;
  int esize = ih->length;
  const char*next;

  next = (char*)ih + esize + 4; /* four bytes of CRC */

  if (data_check_crc)
    if (data_checkCRC(buffer)!=0)
      {
	fprintf(stderr,"data_getItape: Warning: Itape CRC check failed, data might be corrupted or not in itape format\n");
	esend(ALARM_WARNING,"data_getItape: Itape CRC check failed","data might be corrupted or not in itape format\n");
      }
  
  if (nextItape) *nextItape = next;
  return (void*)ih;
}

int data_listGroups(const void*event,int *ngroups_ret,int **list_ret,int **size_ret)
{
  const char*p = event;
  const itape_header_t *h = event;
  const group_header_t *g;
  int sz;
  int i,n;
  int ngroups = 0;

  if (ngroups_ret) (*ngroups_ret) = 0;

  if (h==NULL)
	return -1;

  if (h->type != TYPE_ITAPE)
	return -1;

  sz = h->length;
  n = h->ngroups;

  if (ngroups_ret) (*ngroups_ret) = 0;
  if (list_ret) (*list_ret) = (int*)malloc(sizeof(list_ret[0]) * n);
  if (size_ret) (*size_ret) = (int*)malloc(sizeof(size_ret[0]) * n);

  p += sizeof(itape_header_t);
  sz -= sizeof(itape_header_t);
  for (i=0; i<n; i++)
    {
      g = (group_header_t*) p;
      if (g->length > sz) {
	  fprintf(stderr,"data_listGroups: Itape broken: group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  esend(ALARM_ERROR,"data_listGroups: Itape broken","group id(%d),size(%d) bigger than itape (size=%d)\n",g->type,g->length,sz);
	  if (ngroups_ret) (*ngroups_ret) = ngroups;
	  return -1;
      }
      if (g->length <= 0) {
	  fprintf(stderr,"data_listGroups: Itape broken: group id(%d),size(%d) zero or negative.\n",g->type,g->length);
	  esend(ALARM_ERROR,"data_listGroups: Itape broken","group id(%d),size(%d) zero or negative.\n",g->type,g->length);
	  if (ngroups_ret) (*ngroups_ret) = ngroups;
	  return -1;
      }
      if (g->length&3) {
	  fprintf(stderr,"data_listGroups: Itape broken: group id(%d),size(%d) is not a multiple of 4.\n",g->type,g->length);
	  esend(ALARM_ERROR,"data_listGroups: Itape broken","group id(%d),size(%d) is not a multiple of 4.\n",g->type,g->length);
	  if (ngroups_ret) (*ngroups_ret) = ngroups;
	  return -1;
      }

      if (list_ret) (*list_ret)[ngroups] = g->type;
      if (size_ret) (*size_ret)[ngroups] = g->length;
      ngroups++;

      p += g->length;
      sz -= g->length;
    }

  if (ngroups_ret) (*ngroups_ret) = ngroups;

  return 0;
}

int data_renameGroup(void* event,int oldName,int newName)
{
  group_header_t* ptr;  

  ptr = data_getGroupHeader(event,oldName);

  if (ptr == NULL)
    return 1;

  if (ptr->type != oldName)
    return 2;

  ptr->type = newName;

  return 0;
}

/* end file */
