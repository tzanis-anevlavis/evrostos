/* ---------------------------------------------------------------------------


  This file is part of the ``utils'' package of NuSMV version 2.
  Copyright (C) 2014 by FBK.

  NuSMV version 2 is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  NuSMV version 2 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA.

  For more information on NuSMV see <http://nusmv.fbk.eu>
  or email to <nusmv-users@fbk.eu>.
  Please report bugs to <nusmv-users@fbk.eu>.

  To contact the NuSMV development board, email to <nusmv@fbk.eu>. 

-----------------------------------------------------------------------------*/

/*!
 *  \author Alberto Griggio
 *  \brief Public interface for a generic vector class
 *
 * The macro DECLARE_VECTOR(type, name, prefix) declares a new vector class of
 * elements of type "type", of name "name". All the methods of the class will
 * begin with "prefix". For example:
 *
 *   DECLARE_VECTOR(void *, Vector_ptr, Vector)
 *
 * will declare a Vector_ptr class of "void *" elements, with methods like
 * Vector_push(), Vector_pop(), and so on.
 *
 * The macro DEFINE_VECTOR(type, name, prefix) expands to the implementation
 * of the methods, and so must be placed in a .c file
 *
 * The macro DEFINE_STATIC_VECTOR(type, name, prefix) declares and defines a
 * vector that is local to the current translation unit (i.e. its methods are
 * static).
 *
 * There is one predefined vector class, corresponding to the declaration:
 *
 *   DECLARE_VECTOR(void *, Vector_ptr, Vector)
 *
 * The methods of the class are:
 *
 * - name prefix_create()
 *      create a new empty vector
 *
 * - void prefix_destroy(name v)
 *      destroy the vector
 *      
 * - void prefix_push(name v, type elem)
 *      append elem at the end of v
 *
 * - void prefix_pop(name v)
 *      remove the last element
 *
 * - void prefix_append(name v1, name v2)
 *      append "v2" at the end of "v1"
 *
 * - void prefix_resize(name v, size_t newsz)
 *      resize the vector, allocating memory if needed, but not deallocating
 *      in case of shrinking
 *
 * - void prefix_shrink(name v, size_t newsz)
 *      shrink the vector reducing also the memory occupied
 *      
 * - void prefix_reserve(name v, size_t cap)
 *      reserve space for "cap" elements, but do not resize the vector
 *
 * - void prefix_clear(name v)
 *      empty the vector and free memory
 *
 * - void prefix_copy(name v1, name v2)
 *      copy v1 into v2
 *
 * - void prefix_swap(name v1, name v2)
 *      efficiently swap the contents of v1 and v2
 *
 * - void prefix_sort(name v, int (*cmp)(type a, type b, void *p), void *p)
 *      sort the vector in-place, using the given comparison function
 *      "cmp". "p" is a user-defined parameter that is simply passed to "cmp"
 *
 * - void prefix_acquire(name v, type *arr, size_t sz)
 *      take ownership of the array "arr"
 *
 * - type *prefix_release(name v)
 *      release ownership of the underlying array
 *
 * The following macros are defined:
 *
 * - VECTOR_SIZE(v) return the size of v
 * - VECTOR_CAPACITY(v) return the capacity of v
 * - VECTOR_ARRAY(v) return the underlying array of elements
 * - VECTOR_FIRST(v) return the first element in the array
 * - VECTOR_LAST(v) return the last element in the array
 * - VECTOR_AT(v, i) return the element at position i in the array
 */

#ifndef __NUSMV_CORE_UTILS_VECTOR_H__
#define __NUSMV_CORE_UTILS_VECTOR_H__

#include "nusmv/core/utils/defs.h"
#include <string.h>


#define DECLARE_VECTOR__impl_(qual, type, name, prefix)                 \
  struct name {                                                         \
    type *data_;                                                        \
    size_t size_;                                                       \
    size_t capacity_;                                                   \
  };                                                                    \
  typedef struct name *name;                                            \
  qual name prefix ## _create(void);                                    \
  qual void prefix ## _destroy(name v);                                 \
  qual void prefix ## _push(name v, type elem);                         \
  qual void prefix ## _pop(name v);                                     \
  qual void prefix ## _append(name v1, name v2);                        \
  qual void prefix ## _resize(name v, size_t newsz);                    \
  qual void prefix ## _shrink(name v, size_t newsz);                    \
  qual void prefix ## _reserve(name v, size_t cap);                     \
  qual void prefix ## _clear(name v);                                   \
  qual void prefix ## _copy(name src, name dest);                       \
  qual void prefix ## _swap(name v1, name v2);                          \
  qual void prefix ## _sort(name v, int (*cmp)(type a, type b, void *p), \
                            void *p);                                   \
  qual void prefix ## _acquire(name v, type *arr, size_t sz);           \
  qual type *prefix ## _release(name v);

#define VECTOR_SIZE(v) ((v)->size_)
#define VECTOR_CAPACITY(v) ((v)->capacity_)
#define VECTOR_ARRAY(v) ((v)->data_)
#define VECTOR_FIRST(v) (VECTOR_ARRAY(v)[0])
#define VECTOR_AT(v,i) (VECTOR_ARRAY(v)[i])
#define VECTOR_LAST(v) (VECTOR_ARRAY(v)[VECTOR_SIZE(v)-1])

#define EMPTY__nusmv_utils_vector__

#define DECLARE_VECTOR(type, name, prefix)              \
  DECLARE_VECTOR__impl_(EMPTY__nusmv_utils_vector__, type, name, prefix)

#define DEFINE_VECTOR__impl_(qual, type, name, prefix)                  \
  qual name prefix ## _create(void)                                     \
  {                                                                     \
    name ret = ALLOC(struct name, 1);                                   \
    ret->data_ = NULL;                                                  \
    ret->size_ = 0;                                                     \
    ret->capacity_ = 0;                                                 \
                                                                        \
    return ret;                                                         \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _destroy(name v)                                  \
  {                                                                     \
    if (v->data_) {                                                     \
      FREE(v->data_);                                                   \
    }                                                                   \
    FREE(v);                                                            \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _push(name v, type elem)                          \
  {                                                                     \
    while (v->size_ >= v->capacity_) {                                  \
      prefix ## _reserve(v, v->capacity_ ? v->capacity_ * 2 : 2);       \
    }                                                                   \
    v->data_[v->size_++] = elem;                                        \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _pop(name v)                                      \
  {                                                                     \
    nusmv_assert(v->size_ > 0);                                         \
    --v->size_;                                                         \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _append(name v1, name v2)                         \
  {                                                                     \
    prefix ## _reserve(v1, v1->size_ + v2->size_);                      \
    memcpy(v1->data_ + v1->size_, v2->data_, sizeof(type) * v2->size_); \
    v1->size_ += v2->size_;                                             \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _resize(name v, size_t newsz)                     \
  {                                                                     \
    if (v->size_ >= newsz) {                                            \
      v->size_ = newsz;                                                 \
    } else {                                                            \
      size_t i;                                                         \
      prefix ## _reserve(v, newsz);                                     \
      for (i = v->size_; i < newsz; ++i) {                              \
        v->data_[i] = 0;                                                \
      }                                                                 \
      v->size_ = newsz;                                                 \
    }                                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _shrink(name v, size_t newsz)                     \
  {                                                                     \
    if (newsz == 0) {                                                   \
      prefix ## _clear(v);                                              \
    } else if (newsz < v->size_) {                                      \
      v->data_ = REALLOC(type, v->data_, newsz);                        \
      v->size_ = newsz;                                                 \
      v->capacity_ = newsz;                                             \
    }                                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _reserve(name v, size_t cap)                      \
  {                                                                     \
    if (cap > 0 && v->capacity_ < cap) {                                \
      v->data_ = REALLOC(type, v->data_, cap);                          \
      v->capacity_ = cap;                                               \
    }                                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _clear(name v)                                    \
  {                                                                     \
    v->size_ = 0;                                                       \
    v->capacity_ = 0;                                                   \
    if (v->data_) {                                                     \
      FREE(v->data_);                                                   \
      v->data_ = NULL;                                                  \
    }                                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _copy(name src, name dest)                        \
  {                                                                     \
    prefix ## _resize(dest, src->size_);                                \
    dest->size_ = src->size_;                                           \
    if (src->data_ != NULL) {                                           \
      memcpy(dest->data_, src->data_, sizeof(type) * src->size_);       \
    }                                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _swap(name v1, name v2)                           \
  {                                                                     \
    size_t tmp1;                                                        \
    type *tmp2;                                                         \
                                                                        \
    tmp1 = v1->size_;                                                   \
    v1->size_ = v2->size_;                                              \
    v2->size_ = tmp1;                                                   \
                                                                        \
    tmp1 = v1->capacity_;                                               \
    v1->capacity_ = v2->capacity_;                                      \
    v2->capacity_ = tmp1;                                               \
                                                                        \
    tmp2 = v1->data_;                                                   \
    v1->data_ = v2->data_;                                              \
    v2->data_ = tmp2;                                                   \
  }                                                                     \
                                                                        \
                                                                        \
  static void prefix ## _sort_impl(                                     \
    type *arr, size_t size,                                             \
    int (*cmp)(type a, type b, void *p), void *p)                       \
  {                                                                     \
    /* sorting algorithm taken from minisat */                          \
    if (size <= 15) {                                                   \
      /* selection sort */                                              \
      size_t i, j, best_i;                                              \
      type tmp;                                                         \
                                                                        \
      for (i = 1; i < size; ++i){                                       \
        best_i = i-1;                                                   \
        for (j = i; j < size; ++j) {                                    \
          if (cmp(arr[j], arr[best_i], p) < 0) {                        \
            best_i = j;                                                 \
          }                                                             \
        }                                                               \
        tmp = arr[i-1];                                                 \
        arr[i-1] = arr[best_i];                                         \
        arr[best_i] = tmp;                                              \
      }                                                                 \
    } else {                                                            \
      size_t i, j;                                                      \
      type pivot;                                                       \
      type tmp;                                                         \
      pivot = arr[size / 2];                                            \
      i = (size_t)-1;                                                   \
      j = size;                                                         \
                                                                        \
      for (;;) {                                                        \
        do i++; while(cmp(arr[i], pivot, p) < 0);                       \
        do j--; while(cmp(pivot, arr[j], p) < 0);                       \
                                                                        \
        if (i >= j) break;                                              \
                                                                        \
        tmp = arr[i];                                                   \
        arr[i] = arr[j];                                                \
        arr[j] = tmp;                                                   \
      }                                                                 \
                                                                        \
      prefix ## _sort_impl(arr, i, cmp, p);                             \
      prefix ## _sort_impl(&arr[i], size-i, cmp, p);                    \
    }                                                                   \
  }                                                                     \
                                                                        \
  qual void prefix ## _sort(name v, int (*cmp)(type a, type b, void *p), \
                            void *p)                                    \
  {                                                                     \
    prefix ## _sort_impl(v->data_, v->size_, cmp, p);                   \
  }                                                                     \
                                                                        \
                                                                        \
  qual void prefix ## _acquire(name v, type *arr, size_t sz)            \
  {                                                                     \
    if (v->data_) {                                                     \
      FREE(v->data_);                                                   \
    }                                                                   \
    v->data_ = arr;                                                     \
    v->size_ = sz;                                                      \
    v->capacity_ = sz;                                                  \
  }                                                                     \
                                                                        \
                                                                        \
  qual type *prefix ## _release(name v)                                 \
  {                                                                     \
    type *ret = v->data_;                                               \
    v->data_ = NULL;                                                    \
    v->size_ = 0;                                                       \
    v->capacity_ = 0;                                                   \
    return ret;                                                         \
  }


#define DEFINE_VECTOR(type, name, prefix)       \
  DEFINE_VECTOR__impl_(EMPTY__nusmv_utils_vector__, type, name, prefix)

#define DEFINE_STATIC_VECTOR(type, name, prefix)        \
  DECLARE_VECTOR__impl_(static, type, name, prefix)     \
  DEFINE_VECTOR__impl_(static, type, name, prefix)

/* predefined vectors */
DECLARE_VECTOR(void *, Vector_ptr, Vector) 
DECLARE_VECTOR(int, IntVector_ptr, IntVector)
DECLARE_VECTOR(double, DoubleVector_ptr, DoubleVector)


#endif /* __NUSMV_CORE_UTILS_VECTOR_H__ */
