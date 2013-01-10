#include "myops.h"

int64_t fetch_add_int64 (int64_t *ptr, int64_t val)
{
    return __sync_fetch_and_add(ptr,val);
}

int64_t add_fetch_int64 (int64_t *ptr, int64_t val)
{
    return __sync_add_and_fetch(ptr,val);
}
