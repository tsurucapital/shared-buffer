#ifndef MYOPS_H

#define MYOPS_H
#include <stdint.h>

int64_t fetch_add_int64 (int64_t *ptr,int64_t val);
int64_t add_fetch_int64 (int64_t *ptr,int64_t val);

#endif
