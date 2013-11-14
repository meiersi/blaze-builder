#include <string.h>

/* This wrapper is here so that we can copy a sub-range of a ByteArray#.
   We cannot construct a pointer to the interior of an unpinned ByteArray#,
   except by doing an unsafe ffi call, and adjusting the pointer C-side. */
void * fps_memcpy_offsets(void       *dst, unsigned long dst_off,
                          const void *src, unsigned long src_off, size_t n) {
    return memcpy(dst + dst_off, src + src_off, n);
}
