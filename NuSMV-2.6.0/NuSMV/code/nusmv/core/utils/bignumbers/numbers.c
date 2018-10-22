/* ---------------------------------------------------------------------------



  This file is part of the ``utils/bignumbers'' package of NuSMV version 2.
  Copyright (C) 2012 by FBK-irst.

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
  \author \todo: Missing author
  \brief QNumber class implementation

  \todo: Missing description

*/


#include "nusmv/core/utils/bignumbers/bvnumbersInt.h"
#include "nusmv/core/utils/bignumbers/numbersInt.h"
#include "cudd/util.h"

/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/

/*!
  \brief returns the greatest common divisor of a & b

  returns the greatest common divisor of a & b
*/

long QNumber_gcd_long(long a, long b)
{
  long t;

  if (a < 0) {
    a = -a;
  }
  if (b < 0) {
    b = -b;
  }

  while (b != 0) {
    t = b;
    b = a % b;
    a = t;
  }
  return a;
}

/*!
  \brief allocates heapspace for a GMP object

  allocates heapspace for a GMP object
*/

Gmp* Gmp_alloc(void)
{
  Gmp* v = (Gmp*) ALLOC(Gmp, 1);
  return v;
}

/*!
  \brief frees GMP object

  frees GMP object
*/

void Gmp_free(Gmp* p)
{
  FREE(p);
}

/*!
  \brief return a QNumber representing 0

  return a QNumber representing 0
*/

QNumber QNumber_from_nothing()
{
  QNumber v;
  v.data.num = 0;
  v.den = 1;
  return v;
}

/*!
  \brief Creates a copy of other and pushes it onto the heap

  Creates a copy of other and pushes it onto the heap
*/

QNumber* QNumber_copy_to_heap(QNumber* other)
{
  QNumber* v = (QNumber*) ALLOC(QNumber, 1);
  if (QNumber_big(other)) {
    v->den = 0;
    v->data.gmp = Gmp_alloc();
    mpz_init_set(v->data.gmp->num, other->data.gmp->num);
    mpz_init_set(v->data.gmp->den, other->data.gmp->den);
  }
  else {
    v->data.num = other->data.num;
    v->den = other->den;
  }
  return v;
}

/*!
  \brief Frees heap space allocated to p

  Frees heap space allocated to p
*/

void QNumber_clean_gmp(QNumber* p)
{
  if (QNumber_big(p)) {
    nusmv_assert(p->data.gmp);
    mpz_clear(p->data.gmp->num);
    mpz_clear(p->data.gmp->den);
    Gmp_free(p->data.gmp);
  }
}

/*!
  \brief Creates QNumber n / 1

  Creates QNumber n / 1
*/

QNumber QNumber_from_long(long n)
{
  QNumber v;
  v.data.num = n;
  v.den = 1;
  QNumber_fix_int_min(&v);
  return v;
}

/*!
  \brief Creates QNumber n / d

  Creates QNumber n / d
*/

QNumber QNumber_from_two_longs(long n, long d)
{
  QNumber v;
  v.data.num = n;
  v.den = d;
  QNumber_fix_int_min(&v);
  QNumber_normalize(&v);
  return v;
}

/*!
  \brief Creates a QNumber from a GMP rational number

  Creates a QNumber from a GMP rational number
*/

QNumber QNumber_from_mpq(mpq_t* q_number)
{
  QNumber v;
  mpz_t tmp, tmp2;
  mpq_get_num(tmp, *q_number), mpq_get_den(tmp2, *q_number);
  if (mpz_fits_sint_p(tmp) && mpz_fits_sint_p(tmp)) {
    v.data.num = mpz_get_si(tmp);
    v.den = mpz_get_si(tmp2);
    return v;
  }
  else {
    v.den = 0;
    v.data.gmp = Gmp_alloc();
    mpz_init_set(v.data.gmp->num, tmp);
    mpz_init_set(v.data.gmp->den, tmp2);
  }
  QNumber_normalize(&v);
  return v;
}

/*!
  \brief Creates a new QNumber from two GMP whole numbers

  Creates a new QNumber from two GMP whole numbers
*/

QNumber QNumber_from_two_mpzs(mpz_t *n, mpz_t *d)
{
  QNumber v;
  if (mpz_fits_sint_p(*n) && mpz_fits_sint_p(*d)) {
    v.data.num = mpz_get_si(*n);
    v.den = mpz_get_si(*d);
  }
  else {
    v.den = 0;
    v.data.gmp = Gmp_alloc();
    mpz_init_set(v.data.gmp->num, *n);
    mpz_init_set(v.data.gmp->den, *d);
  }
  QNumber_normalize(&v);
  return v;
}

/*!
  \brief Creates a new QNumber from another number

  Creates a new QNumber from another number
*/

QNumber QNumber_from_other(const QNumber *other)
{
  QNumber v;
  if (QNumber_big(other)) {
    v.den = 0;
    v.data.gmp = Gmp_alloc();
    mpz_init_set(v.data.gmp->num, other->data.gmp->num);
    mpz_init_set(v.data.gmp->den, other->data.gmp->den);
  }
  else {
    v.data.num = other->data.num;
    v.den = other->den;
  }
  return v;
}

/*!
  \brief returns true if self uses GMP, false otherwise

  returns true if self uses GMP, false otherwise
*/

boolean QNumber_big(const QNumber *self)
{
  return (self->den == 0);
}


/*!
  \brief Creates a new QNumber from two GMP whole numbers

  Creates a new QNumber from two GMP whole numbers
*/

QNumber QNumber_assign(mpz_t n, mpz_t d, int b)
{
  QNumber v;

  nusmv_assert(b == 0 || b == 1);

  nusmv_assert(b);
  v.den = 0;
  v.data.gmp = Gmp_alloc();
  mpz_init_set(v.data.gmp->num, n);
  mpz_init_set(v.data.gmp->den, d);
  QNumber_normalize(&v);
  return v;
}

/*!
  \brief Fixes off by one situation which can occur, during number
                     creation

  Fixes off by one situation which can occur, during number
                     creation
*/

boolean QNumber_fix_int_min(QNumber *self)
{
  nusmv_assert(!QNumber_big(self));
  /* TODO: Explicitly test this code! */
  if (self->data.num == LONG_MIN || self->den == LONG_MIN) {
    long n = self->data.num, d = self->den;
    self->den = 0;
    self->data.gmp = Gmp_alloc();
    mpz_init_set_si(self->data.gmp->num, n);
    mpz_init_set_si(self->data.gmp->den, d);
    return true;
  }
  return false;
}

/*!
  \brief returns 1 if self uses GMP, and is a natural number,
                     0 otherwise

  returns 1 if self uses GMP, and is a natural number,
                     0 otherwise
*/

boolean QNumber_is_int_big(const QNumber *self)
{
  if (QNumber_big(self)) {
    return mpz_cmp_si(self->data.gmp->den, 1) == 0;
  }
  else {
    return false;
  }
}

/*!
  \brief returns 1 if does not use GMP, and is a natural number,
                     0 otherwise

  returns 1 if does not use GMP, and is a natural number,
                     0 otherwise
*/

boolean QNumber_is_int_normal(const QNumber *self)
{
  if (!QNumber_big(self)) {
    return (self->den == 1);
  }
  else {
    return false;
  }
}

/*!
  \brief Stop using longs for number representation and start
                      using GMP numbers

  Stop using longs for number representation and start
                      using GMP numbers
*/

void QNumber_make_big(QNumber *self)
{
  long n = 0, d = 0;

  nusmv_assert(!QNumber_big(self));
  n = self->data.num, d = self->den;
  self->den = 0;
  self->data.gmp = Gmp_alloc();
  mpz_init_set_si(self->data.gmp->num, n);
  mpz_init_set_si(self->data.gmp->den, d);
}


/*!
  \brief returns -r

  returns -r
*/

QNumber QNumber_operator_unaray_minus(const QNumber *r)
{
  return QNumber_neg(r);
}

/*!
  \brief A new qnumber with value n

  A new qnumber with value n
*/

QNumber QNumber_make_number_from_unsigned_long_long(unsigned long long n)
{
  if (n > LONG_MAX) {
    QNumber return_value;
    const int mpz_pool_size = 2;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* value = &mpz_pool[0];
    mpz_t* one = &mpz_pool[1];

    char buff[manual_set_buf_size]; /* TODO UGLY hack */

    init_mpz_pool(mpz_pool, mpz_pool_size);

    sprintf(buff, "%llu", n);
    mpz_set_str(*value, buff, 10);
    mpz_set_ui(*one, 1);
    return_value = QNumber_from_two_mpzs(value, one);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return return_value;
  }
  else {
    signed long int signed_long_value = (signed long int) n;

    nusmv_assert(signed_long_value >= 0);

    return QNumber_from_long(signed_long_value);
  }
}

/*!
  \brief self += r, used if self +=r would overflow when
                      longs would be used

  self += r
*/

void QNumber_operator_self_plus_big (QNumber* self, const QNumber *r)
{
  /* fallback case - convert to GMP and do the addition there ... */
  if (!QNumber_big(self)) {
    QNumber_make_big(self);
  }

  if (!QNumber_big(r)) {
    if (QNumber_is_int_normal(r) && QNumber_is_int_big(r)) {
      if (r->data.num >= 0) {
        mpz_add_ui(self->data.gmp->num,
                   self->data.gmp->num,
                   ((unsigned long) (r->data.num)));
      }
      else {
        mpz_add_ui(self->data.gmp->num,
                   self->data.gmp->num,
                   ((unsigned long) (-r->data.num)));
      }
      return;
    }
    else {
      const int mpz_pool_size = 2;
      mpz_t mpz_pool[mpz_pool_size];

      long r_num = r->data.num;
      long r_den = r->den;
      mpz_t*  bg = &mpz_pool[0];
      mpz_t*  tmp = &mpz_pool[1];
      unsigned long g = 0;

      init_mpz_pool(mpz_pool, mpz_pool_size);

      g = mpz_gcd_ui(*bg, self->data.gmp->den, labs(r_den));
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *bg);
      mpz_mul_si(self->data.gmp->num,
                 self->data.gmp->num,
                 r_den/ ((long) (g)));
      mpz_mul_si(*tmp, self->data.gmp->den, r_num);
      mpz_add(self->data.gmp->num, self->data.gmp->num, *tmp);
      g = mpz_gcd_ui(*bg, self->data.gmp->num, g);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *bg);
      mpz_mul_si(self->data.gmp->den,
                 self->data.gmp->den,
                 r_den/((long) (g)));

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
  else {
    if (QNumber_is_int_big(self) && QNumber_is_int_big(r)) {
      mpz_add(self->data.gmp->num, self->data.gmp->num, r->data.gmp->num);
      return;
    }
    else {
      const int mpz_pool_size = 4;
      mpz_t mpz_pool[mpz_pool_size];

      mpz_t* r_num = &mpz_pool[0];
      mpz_t* r_den = &mpz_pool[1];
      mpz_t* g = &mpz_pool[2];
      mpz_t* tmp = &mpz_pool[3];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set(*r_num, r->data.gmp->num);
      mpz_set(*r_den, r->data.gmp->den);
      mpz_gcd(*g, self->data.gmp->den, *r_den);
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *g);
      mpz_divexact(*tmp, *r_den, *g);
      mpz_mul(self->data.gmp->num, self->data.gmp->num, *tmp);
      mpz_mul(*r_num, *r_num, self->data.gmp->den);
      mpz_add(self->data.gmp->num, self->data.gmp->num, *r_num);
      mpz_gcd(*g, self->data.gmp->num, *g);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *g);
      mpz_divexact(*r_den, *r_den, *g);
      mpz_mul(self->data.gmp->den, self->data.gmp->den, *r_den);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
}

/*!
  \brief self += r

  
     This calculation avoids overflow, and minimises the number
     of expensives calculations. Thanks to Nickolay Mladenov for this algorithm.

     Proof:
     We have to compute a/b + c/d, where gcd(a,b)=1 and gcd(b,c)=1.
     Let g = gcd(b,d), and b = b1*g, d=d1*g. Then gcd(b1,d1)=1

     The result is (a*d1 + c*b1) / (b1*d1*g).
     Now we have to normalize this ratio.
     Let's assume h | gcd((a*d1 + c*b1), (b1*d1*g)), and h > 1
     If h | b1 then gcd(h,d1)=1 and hence h|(a*d1+c*b1) => h|a.
     But since gcd(a,b1)=1 we have h=1.
     Similarly h|d1 leads to h=1.
     So we have that h | gcd((a*d1 + c*b1) , (b1*d1*g)) => h|g
     Finally we have gcd((a*d1 + c*b1), (b1*d1*g)) = gcd((a*d1 + c*b1), g)
     Which proves that instead of normalizing the result, it is better to
     divide num and den by gcd((a*d1 + c*b1), g)

     ORIGINAL ALGORITHM:
      Protect against self-modification
     IntType r_num = r.num;
     IntType r_den = r.den;

     IntType g = gcd(den, r_den);
     den /= g;   = b1 from the calculations above
     num = num * (r_den / g) + r_num * den;
     g = gcd(num, g);
     num /= g;
     den *= r_den/g;

     return *this;
*/

void QNumber_operator_self_plus (QNumber* self, const QNumber *r)
{
  long r_num = 0;
  long r_den = 0;
  long s_num = 0;
  long s_den = 0;
  long g = 0;

  if (!QNumber_big(self) && !QNumber_big(r)) {
    /* should be pretty fast if both are integers...*/
    if (QNumber_is_int_normal(self) && QNumber_is_int_normal(r)) {
      if (QNumber_add_overflow(&(self->data.num),
                                 self->data.num,
                                 r->data.num)) {
        QNumber_operator_self_plus_big(self, r);
      }
      return;
    }

    /* Protect against self-modification */
    r_num = r->data.num;
    r_den = r->den;

    s_num = self->data.num;
    s_den = self->den;

    g = QNumber_gcd_long(s_den, r_den);
    if (!QNumber_div_overflow(&s_den, s_den, g)) {
      /* = b1 from the calculations above */
      long tmp1, tmp2;
      if (QNumber_div_overflow(&tmp1, r_den, g)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&tmp2, r_num, s_den)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&tmp1, tmp1, s_num)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      if (QNumber_add_overflow(&s_num, tmp1, tmp2)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      /* now: num = num * (r_den / g) + r_num * den; */
      g = QNumber_gcd_long(s_num, g);
      if (QNumber_div_overflow(&s_num, s_num, g)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      /* now: num /= g; */
      if (QNumber_div_overflow(&tmp1, r_den, g)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&s_den, s_den, tmp1)) {
        QNumber_operator_self_plus_big(self, r);
        return;
      }
      /* now: den *= r_den/g; */
      self->data.num = s_num;
      self->den = s_den;
    }
    else {
      QNumber_operator_self_plus_big(self, r);
      return;
    }
    return;
  }
  QNumber_operator_self_plus_big(self, r);
}

/*!
  \brief self -= r, used if self -=r would overflow when
                      longs would be used

  self -= r
*/

void QNumber_operator_self_minus_big(QNumber* self, const QNumber *r)
{
  if (!QNumber_big(self)) {
    QNumber_make_big(self);
  }

  if (!QNumber_big(r)) {
    if (QNumber_is_int_normal(r) && QNumber_is_int_big(self)) {
      if (r->data.num >= 0) {
        mpz_sub_ui(self->data.gmp->num,
                   self->data.gmp->num,
                   ((unsigned long) (r->data.num)));
      }
      else {
        mpz_sub_ui(self->data.gmp->num,
                   self->data.gmp->num,
                   ((unsigned long) (-(r->data.num))));
      }
      return;
    }
    else {
      const int mpz_pool_size = 2;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* bg  = &mpz_pool[0];
      mpz_t* tmp = &mpz_pool[1];
      long r_num = r->data.num;
      long r_den = r->den;
      unsigned long g = 0;

      init_mpz_pool(mpz_pool, mpz_pool_size);
      g = mpz_gcd_ui(*bg, self->data.gmp->den, labs(r_den));

      /* = b1 from the calculations  above */
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *bg);
      mpz_mul_si(self->data.gmp->num,
                 self->data.gmp->num,
                 r_den / ((long) (g)));
      mpz_mul_si(*tmp, self->data.gmp->den, r_num);
      mpz_sub(self->data.gmp->num, self->data.gmp->num, *tmp);
      g = mpz_gcd_ui(*bg, self->data.gmp->num, g);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *bg);
      mpz_mul_si(self->data.gmp->den,
                 self->data.gmp->den,
                 r_den / ((long) (g)));

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }

  }
  else {
    if (QNumber_is_int_big(self) && QNumber_is_int_big(r)) {
      mpz_sub(self->data.gmp->num, self->data.gmp->num, r->data.gmp->num);
      return;
    } else {
      const int mpz_pool_size = 4;
      mpz_t mpz_pool[mpz_pool_size];

      mpz_t* r_num = &mpz_pool[0];
      mpz_t* r_den = &mpz_pool[1];
      mpz_t* g = &mpz_pool[2];
      mpz_t* tmp = &mpz_pool[3];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set(*r_num, r->data.gmp->num);
      mpz_set(*r_den, r->data.gmp->den);

      mpz_gcd(*g, self->data.gmp->den, *r_den);
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *g);

      mpz_divexact(*tmp, *r_den, *g);
      mpz_mul(self->data.gmp->num, self->data.gmp->num, *tmp);
      mpz_mul(*r_num, *r_num, self->data.gmp->den);
      mpz_sub(self->data.gmp->num, self->data.gmp->num, *r_num);
      mpz_gcd(*g, self->data.gmp->num, *g);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *g);
      mpz_divexact(*r_den, *r_den, *g);
      mpz_mul(self->data.gmp->den, self->data.gmp->den, *r_den);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
}

/*!
  \brief self -= r

  
                      self -= r
                      ORIGINAL ALGORITHM - this is the same as
                      += (with - where appropriate...)
                      Protect against self-modification
                      IntType r_num = r.num;
                      IntType r_den = r.den;

                      This calculation avoids overflow, and minimises
                      the number of expensive calculations. It
                      corresponds exactly to the += case above
                      IntType g = gcd(den, r_den);
                      den /= g;
                      num = num * (r_den / g) - r_num * den;
                      g = gcd(num, g);
                      num /= g;
                      den *= r_den/g;

                      return *this;
                     
*/

void QNumber_operator_self_minus(QNumber* self, const QNumber *r)
{
  long r_num = 0;
  long r_den = 0;
  long s_num = 0;
  long s_den = 0;
  long g = 0;

  if (!QNumber_big(self) && !QNumber_big(r)) {
    /* should be pretty fast if both are integers ... */
    if (QNumber_is_int_normal(self) && QNumber_is_int_normal(r)) {
      if (QNumber_sub_overflow(&self->data.num, self->data.num, r->data.num)) {
        QNumber_operator_self_minus_big(self, r);
      }
      return;
    }

    /* Protect against self-modification */
    r_num = r->data.num;
    r_den = r->den;

    s_num = self->data.num;
    s_den = self->den;

    g = QNumber_gcd_long(s_den, r_den);
    if (!QNumber_div_overflow(&s_den, s_den, g)) {
      /* = b1 from the calculations above */
      long tmp1, tmp2;
      if (QNumber_div_overflow(&tmp1, r_den, g)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&tmp2, r_num, s_den)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&tmp1, tmp1, s_num)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      if (QNumber_sub_overflow(&s_num, tmp1, tmp2)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      /* now: num = num * (r_den / g) - r_num * den; */
      g = QNumber_gcd_long(s_num, g);
      if (QNumber_div_overflow(&s_num, s_num, g)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      /* now: num /= g; */
      if (QNumber_div_overflow(&tmp1, r_den, g)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&s_den, s_den, tmp1)) {
        QNumber_operator_self_minus_big(self, r);
        return;
      }
      /* now: den *= r_den/g; */
      self->data.num = s_num;
      self->den = s_den;
    }
    else {
      QNumber_operator_self_minus_big(self, r);
      return;
    }
    return;
  }

  /* fallback case - convert to GMP and do the addition there ... */
  QNumber_operator_self_minus_big(self, r);
}

/*!
  \brief self *= r, used if self *=r would overflow when
                      longs would be used

  self *= r
*/

void QNumber_operator_self_mul_big (QNumber* self, const QNumber *r)
{
  if (!QNumber_big(self)) {
    QNumber_make_big(self);
  }

  if (!QNumber_big(r)) {
    if (QNumber_is_int_normal(r) && QNumber_is_int_big(self)) {
      mpz_mul_si(self->data.gmp->num, self->data.gmp->num, r->data.num);
      return;
    }
    else {
      long r_num = r->data.num;
      long r_den = r->den;
      if (!r_num) {
        /* multiplication by zero, easy (we can't use the general case
         because of this: if labs(r_num) is zero and den.big is indeed
         big, mpz_gcd_ui will return zero. If we let this go, we will
         get a division by zero later on... */
        mpz_clear(self->data.gmp->num);
        mpz_clear(self->data.gmp->den);
        Gmp_free(self->data.gmp);
        self->data.num = 0;
        self->den = 1;
        return;
      }
      else {
        unsigned long gcd1 = 0;
        unsigned long gcd2 = 0;
        const int mpz_pool_size = 2;
        mpz_t mpz_pool[mpz_pool_size];
        mpz_t* g1 = &mpz_pool[0];
        mpz_t* g2 = &mpz_pool[1];

        init_mpz_pool(mpz_pool, mpz_pool_size);

        gcd1 = mpz_gcd_ui(*g1, self->data.gmp->num, labs(r_den));
        gcd2 = mpz_gcd_ui(*g2, self->data.gmp->den, labs(r_num));
        mpz_divexact_ui(self->data.gmp->num, self->data.gmp->num, gcd1);
        mpz_mul_si(self->data.gmp->num,
                   self->data.gmp->num,
                   r_num/((long) (gcd2)));
        mpz_divexact_ui(self->data.gmp->den, self->data.gmp->den, gcd2);
        mpz_mul_si(self->data.gmp->den,
                   self->data.gmp->den,
                   r_den/((long) (gcd1)));

        clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
      }
    }
  }
  else {
    if (QNumber_is_int_big(self) && QNumber_is_int_big(r)) {
        mpz_mul(self->data.gmp->num, self->data.gmp->num, r->data.gmp->num);
        return;
    } else {
      const int mpz_pool_size = 4;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* r_num = &mpz_pool[0];
      mpz_t* r_den = &mpz_pool[1];
      mpz_t* gcd1 = &mpz_pool[2];
      mpz_t* gcd2 = &mpz_pool[3];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set(*r_num, r->data.gmp->num);
      mpz_set(*r_den, r->data.gmp->den);
      mpz_gcd(*gcd1, self->data.gmp->num, *r_den);
      mpz_gcd(*gcd2, *r_num, self->data.gmp->den);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *gcd1);
      mpz_divexact(*r_num, *r_num, *gcd2);
      mpz_mul(self->data.gmp->num, self->data.gmp->num, *r_num);
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *gcd2);
      mpz_divexact(*r_den, *r_den, *gcd1);
      mpz_mul(self->data.gmp->den, self->data.gmp->den, *r_den);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
}

/*!
  \brief self *= r

  self *= r

  \se 
                       ORIGINAL ALGORITHM:
                       Protect against self-modification
                       IntType r_num = r.num;
                       IntType r_den = r.den;

                       Avoid overflow and preserve normalization
                       IntType gcd1 = gcd<IntType>(num, r_den);
                       IntType gcd2 = gcd<IntType>(r_num, den);
                       num = (num/gcd1) * (r_num/gcd2);
                       den = (den/gcd2) * (r_den/gcd1);

                       return *this;
                     
*/

void QNumber_operator_self_mul(QNumber* self, const QNumber *r)
{
  if (!QNumber_big(self) && !QNumber_big(r)) {
    if (QNumber_is_int_normal(self) && QNumber_is_int_normal(r)) {
      if (QNumber_mul_overflow(&(self->data.num),
                               self->data.num,
                               r->data.num)) {
        QNumber_operator_self_mul_big(self, r);
      }
      return;
    } else {
      unsigned long gcd1 = 0;
      unsigned long gcd2 = 0;
      long tmp = 0;
      /* Protect against self-modification */
      long r_num = r->data.num;
      long r_den = r->den;
      long s_num = self->data.num;
      long s_den = self->den;

      /* Avoid overflow and preserve normalization */
      gcd1 = QNumber_gcd_long(s_num, r_den);
      gcd2 = QNumber_gcd_long(r_num, s_den);
      if (QNumber_div_overflow(&tmp, s_num, gcd1)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      if (QNumber_div_overflow(&r_num, r_num, gcd2)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&s_num, tmp, r_num)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      /* num = (num/gcd1) * (r_num/gcd2); */
      if (QNumber_div_overflow(&tmp, s_den, gcd2)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      if (QNumber_div_overflow(&r_den, r_den, gcd1)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      if (QNumber_mul_overflow(&s_den, tmp, r_den)) {
        QNumber_operator_self_mul_big(self, r);
        return;
      }
      /* den = (den/gcd2) * (r_den/gcd1); */

      self->data.num = s_num;
      self->den = s_den;
      return;
    }
  }
  QNumber_operator_self_mul_big(self, r);
}

/*!
  \brief self /= r

  self /= r
*/

void QNumber_operator_self_div(QNumber* self, const QNumber * r)
{
  if (!QNumber_big(r)) {
    /* return (*this *= QNumber(r.den, r.num)); */
    QNumber v = QNumber_from_two_longs(r->den, r->data.num);
    QNumber_operator_self_mul(self, &v);
    QNumber_clean_gmp(&v);
    return;
  }
  else {
    QNumber v;
    v = QNumber_assign(r->data.gmp->den, r->data.gmp->num, 1);
    QNumber_operator_self_mul(self, &v);
    return;
  }
}

/*!
  \brief return 1 if n < r, return 0 otherwise, if n is big and
                      r is small

  return 1 if n < r, return 0 otherwise
*/

boolean QNumber_operator_less_than_r_small(const QNumber *n,
                                           const QNumber *r) {
  unsigned long gcd1 = 0;
  unsigned long gcd2 = 0;
  int ret =0;
  int num_sgn = mpz_sgn(n->data.gmp->num);

  if (num_sgn < 0 && r->data.num >= 0) {
    return true;
  }
  if (num_sgn >= 0 && r->data.num <= 0) {
    return false;
  }

  if (QNumber_is_int_normal(r) && QNumber_is_int_big(n)) {
    return (mpz_cmp_si(n->data.gmp->num, r->data.num) < 0);
  }
  else {
   const int mpz_pool_size = 2;
   mpz_t mpz_pool[mpz_pool_size];
   mpz_t* tmp  = &mpz_pool[0];
   mpz_t* tmp2 = &mpz_pool[1];

   init_mpz_pool(mpz_pool, mpz_pool_size);

   gcd1 = mpz_gcd_ui(*tmp, n->data.gmp->num, labs(r->data.num));
   gcd2 = mpz_gcd_ui(*tmp, n->data.gmp->den, labs(r->den));
   mpz_divexact_ui(*tmp, n->data.gmp->num, gcd1);
   mpz_mul_si(*tmp, *tmp, r->den/((long) gcd2));
   mpz_divexact_ui(*tmp2, n->data.gmp->den, gcd2);
   mpz_mul_si(*tmp2, *tmp2, r->data.num/((long) gcd1));
   ret = mpz_cmp(*tmp, *tmp2);

   clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
   return (ret < 0);
 }
}

/*!
  \brief 
                      return 1 if n < r, return 0 otherwise, if n & r are small
                     

  return 1 if n < r, return 0 otherwise
*/

boolean QNumber_operator_less_than_both_small(const QNumber *n,
                                              const QNumber *r) {
  int ret = 0;
  long gcd1 = 0;
  long gcd2 = 0;

  /* If the two values have different signs, we don't need to do the
  expensive calculations below. We take advantage here of the fact
  that the denominator is always positive. */
  if (n->data.num < 0 && r->data.num >= 0) { /* -ve < +ve */
    return true;
  }
  if (n->data.num >= 0 && r->data.num <= 0) {
    /* +ve or zero is not < -ve or zero */
    return false;
  }

  if (QNumber_is_int_normal(n) && QNumber_is_int_normal(r)) {
    return n->data.num < r->data.num;
  }

  /* Try to avoid overflow */
  gcd1 = QNumber_gcd_long(n->data.num, r->data.num);
  gcd2 = QNumber_gcd_long(r->den, n->den);

  if (sizeof(long long) == sizeof(long) * 2) {
    /* we *have to* cast to long long here, because the multiplication
    might overflow... */
    ret = ((long long)(n->data.num/gcd1) * (long long)(r->den/gcd2) <
           (long long)(n->den/gcd2) * (long long)(r->data.num/gcd1));
  }
  else {
    /* on 64-bits, we use the safe multiplication */
    long tmp1, tmp2;
    long d1 = n->data.num/gcd1, d2 = r->den/gcd2;
    long d3 = n->den/gcd2, d4 = r->data.num/gcd1;
    if (!QNumber_mul_overflow(&tmp1, d1, d2) &&
        !QNumber_mul_overflow(&tmp2, d3, d4)) {
      ret = tmp1 < tmp2;
    }
    else {
      /* convert to mpz and do the comparison there */
      const int mpz_pool_size = 2;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* mp_tmp1 = &mpz_pool[0];
      mpz_t* mp_tmp2 = &mpz_pool[1];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set_si(*mp_tmp1, d1);
      mpz_mul_si(*mp_tmp1, *mp_tmp1, d2);
      mpz_set_si(*mp_tmp2, d3);
      mpz_mul_si(*mp_tmp2, *mp_tmp2, d4);

      ret = (mpz_cmp(*mp_tmp1, *mp_tmp2) < 0);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
  return ret;
}

/*!
  \brief return 1 if n < r, return 0 otherwise, if n is small and
                      r is big

  return 1 if n < r, return 0 otherwise
*/

boolean QNumber_operator_less_than_n_small(const QNumber *n,
                                           const QNumber *r) {
  int r_num_sgn = mpz_sgn(r->data.gmp->num);
  int ret = 0;

  if (n->data.num < 0 && r_num_sgn >= 0) {
    return true;
  }
  if (n->data.num >= 0 && r_num_sgn <= 0) {
    return false;
  }

  if (QNumber_is_int_normal(n) && QNumber_is_int_big(r)) {
    return mpz_cmp_si(r->data.gmp->num, n->data.num) > 0;
  }

  if (!n->data.num) {
     /* comparison with zero, easy (we can't use the general case
     because of this: if labs(r.num.normal) is zero and n.num.big is
     indeed big, mpz_gcd_ui will return zero. If we let this go, we
     will get a division by zero later on... */
     return mpz_sgn(r->data.gmp->num) > 0;
  } else {
    unsigned long gcd1 = 0;
    unsigned long gcd2 = 0;
    const int mpz_pool_size = 2;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp = &mpz_pool[0];
    mpz_t* tmp2 = &mpz_pool[1];

    init_mpz_pool(mpz_pool, mpz_pool_size);

    gcd1 = mpz_gcd_ui(*tmp, r->data.gmp->num, labs(n->data.num));
    gcd2 = mpz_gcd_ui(*tmp, r->data.gmp->den, labs(n->den));

    mpz_divexact_ui(*tmp, r->data.gmp->den, gcd2);
    mpz_mul_si(*tmp, *tmp, n->data.num/((long) gcd1));
    mpz_divexact_ui(*tmp2, r->data.gmp->num, gcd1);
    mpz_mul_si(*tmp2, *tmp2, n->den/((long) gcd2));
    ret = mpz_cmp(*tmp, *tmp2);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return ret < 0;
  }
}


/*!
  \brief return 1 if n < r, return 0 otherwise

  return 1 if n < r, return 0 otherwise
*/

boolean QNumber_operator_less_than_both_big(const QNumber *n,
                                            const QNumber *r) {
  int n_num_sgn = mpz_sgn(n->data.gmp->num);
  int r_num_sgn = mpz_sgn(r->data.gmp->num);
  int ret = 0;

  if (n_num_sgn < 0 && r_num_sgn >= 0) {
      return true;
  }
  if (n_num_sgn >= 0 && r_num_sgn <= 0) {
      return false;
  }
  if (QNumber_is_int_big(n) && QNumber_is_int_big(r)) {
      return mpz_cmp(n->data.gmp->num, r->data.gmp->num) < 0;
  } else {
    const int mpz_pool_size = 4;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* gcd1 = &mpz_pool[0];
    mpz_t* gcd2 = &mpz_pool[1];
    mpz_t* tmp1 = &mpz_pool[2];
    mpz_t* tmp2 = &mpz_pool[3];

    init_mpz_pool(mpz_pool, mpz_pool_size);

    mpz_gcd(*gcd1, n->data.gmp->num, r->data.gmp->num);
    mpz_gcd(*gcd2, r->data.gmp->den, n->data.gmp->den);
    mpz_divexact(*tmp1, n->data.gmp->num, *gcd1);
    mpz_divexact(*tmp2, r->data.gmp->den, *gcd2);
    mpz_mul(*tmp1, *tmp1, *tmp2);
    mpz_divexact(*tmp2, n->data.gmp->den, *gcd2);
    mpz_divexact(*gcd2, r->data.gmp->num, *gcd1);
    mpz_mul(*tmp2, *tmp2, *gcd2);

    ret = mpz_cmp(*tmp1, *tmp2);
    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return ret < 0;
  }
}


/*!
  \brief return 1 if n < r, return 0 otherwise

  return 1 if n < r, return 0 otherwise
*/

boolean QNumber_operator_less_than(const QNumber *n, const QNumber *r)
{
  if (!QNumber_big(n) && !QNumber_big(r)) {
    return QNumber_operator_less_than_both_small(n, r);
  }
  else if (!QNumber_big(r)) {
    return QNumber_operator_less_than_r_small(n, r);
  }
  else if (!QNumber_big(n)) {
    return QNumber_operator_less_than_n_small(n, r);
  }
  else {
    return QNumber_operator_less_than_both_big(n, r);
  }
}

/*!
  \brief return 1 if n == r, return 0 otherwise

  return 1 if n == r, return 0 otherwise
*/

boolean QNumber_operator_equals(const QNumber *n, const QNumber *r)
{
  if (!QNumber_big(n) && !QNumber_big(r)) {
    return ((n->data.num == r->data.num) && (n->den == r->den));
  }
  else if (!QNumber_big(n)) {
    return (!mpz_cmp_si(r->data.gmp->num, n->data.num) &&
            !mpz_cmp_si(r->data.gmp->den, n->den));
  }
  else if (!QNumber_big(r)) {
    return (!mpz_cmp_si(n->data.gmp->num, r->data.num) &&
            !mpz_cmp_si(n->data.gmp->den, r->den));
  }
  return (!mpz_cmp(n->data.gmp->num, r->data.gmp->num) &&
          !mpz_cmp(n->data.gmp->den, r->data.gmp->den));

}


/*!
  \brief Returns the inverse rational number of self i.e. n / d
                      returns d / n

  Returns the inverse rational number of self i.e. n / d
                      returns d / n
*/

QNumber QNumber_inv(const QNumber *self)
{
  QNumber v;
  if (!QNumber_big(self)) {
    v = QNumber_from_two_longs(self->den, self->data.num);
  }
  else {
    v = QNumber_assign(self->data.gmp->den, self->data.gmp->num, 1);
  }
  return v;
}


/*!
  \brief self = -self

  self = -self
*/

void QNumber_self_neg(QNumber *self)
{
  if (!QNumber_big(self)) {
    if (self->data.num == LONG_MIN) {
      /* this overflows! */
      QNumber_make_big(self);
      mpz_neg(self->data.gmp->num, self->data.gmp->num);
    }
    else {
      self->data.num = -(self->data.num);
    }
  }
  else {
    mpz_neg(self->data.gmp->num, self->data.gmp->num);
  }
}

/*!
  \brief Returns a new number which is the negative of self

  Returns a new number which is the negative of self
*/

QNumber QNumber_neg(const QNumber *self)
{
  QNumber ret = QNumber_from_other(self);
  QNumber_self_neg(&ret);
  return ret;
}


/*!
  \brief Returns a new number equal to a + b

  Returns a new number equal to a + b
*/

QNumber QNumber_operator_plus(const QNumber *a, const QNumber *b)
{
  QNumber ret = QNumber_from_other(a);
  QNumber_operator_self_plus(&ret, b);
  return ret;
}

/*!
  \brief Returns a new number equal to a - b

  Returns a new number equal to a - b
*/

QNumber QNumber_operator_minus(const QNumber *a, const QNumber *b)
{
  QNumber ret = QNumber_from_other(a);
  QNumber_operator_self_minus(&ret, b);
  return ret;
}

/*!
  \brief Returns a new number equal to a * b

  Returns a new number equal to a * b
*/

QNumber QNumber_operator_mul(const QNumber *a, const QNumber *b)
{
  QNumber ret = QNumber_from_other(a);
  QNumber_operator_self_mul(&ret, b);
  return ret;
}

/*!
  \brief Returns a new number equal to a / b

  Returns a new number equal to a / b
*/

QNumber QNumber_operator_div(const QNumber *a, const QNumber *b)
{
  QNumber ret = QNumber_from_other(a);
  QNumber_operator_self_div(&ret, b);
  return ret;
}

/*!
  \brief returns 1 if a > b, otherwise returns 0

  returns 1 if a > b, otherwise returns 0
*/

boolean QNumber_operator_more_than(const QNumber *a, const QNumber *b)
{
  return QNumber_operator_less_than(b, a);
}

/*!
  \brief 

  returns 1 if a <= b, otherwise returns 0
*/

boolean QNumber_operator_less_than_or_equals(const QNumber *a,
                                             const QNumber *b)
{
  return (QNumber_operator_equals(a, b) || QNumber_operator_less_than(a, b));
}

/*!
  \brief returns 1 if a >= b, otherwise returns 0

  returns 1 if a >= b, otherwise returns 0
*/

boolean QNumber_operator_more_than_or_equals(const QNumber *a,
                                             const QNumber *b)
{
  return (QNumber_operator_equals(a, b) || QNumber_operator_more_than(a, b));
}

/*!
  \brief returns 1 if a != b, otherwise returns 0

  returns 1 if a != b, otherwise returns 0
*/

boolean QNumber_operator_not_equal(const QNumber *a, const QNumber *b)
{
  return !(QNumber_operator_equals(a, b));
}

/*!
  \brief returns a new QNumber which is equal to the absolute
                      value of n

  returns a new QNumber which is equal to the absolute
                      value of n
*/

QNumber QNumber_abs(const QNumber *n)
{
  if (!QNumber_big(n)) {
    QNumber ret = QNumber_from_two_longs(labs(n->data.num), n->den);
    return ret;
  }
  else {
    QNumber ret = QNumber_assign(n->data.gmp->num, n->data.gmp->den, 1);
    mpz_abs(ret.data.gmp->num, ret.data.gmp->num);
    return ret;
  }
}

/*!
  \brief returns 1 if a > 0; 0 if a == 0, and -1 otherwise

  returns 1 if a > 0; 0 if a == 0, and -1 otherwise
*/

int QNumber_sgn(const QNumber *n)
{
  if (!QNumber_big(n)) {
    int ret = (n->data.num < 0) ? -1 : (n->data.num > 0);
    return ret;
  }
  else {
    int ret = mpz_sgn(n->data.gmp->num);
    return ret;
  }
}

/*!
  \brief returns -1 if a < b; 0 if a == 0, and 1 otherwise

  returns -1 if a < b; 0 if a == 0, and 1 otherwise
*/

int QNumber_cmp(const QNumber *a, const QNumber *b)
{
  int lt = QNumber_operator_less_than(a, b);
  int ret = lt ? -1 : QNumber_operator_not_equal(a, b);
  return ret;
}

/*!
  \brief returns a new QNumber which is the greatest common
                      divider of a and b

  returns a new QNumber which is the greatest common
                      divider of a and b
*/

QNumber QNumber_gcd(const QNumber *a, const QNumber *b)
{
  nusmv_assert(QNumber_is_integer(a) && QNumber_is_integer(b));

  if (!QNumber_big(a)) {
    if (!QNumber_big(b)) {
      return QNumber_from_long(QNumber_gcd_long(a->data.num, b->data.num));
    }
    else {
      if (!a->data.num) {
        QNumber ret = QNumber_from_other(b);
        return QNumber_sgn(b) < 0 ? QNumber_operator_unaray_minus(&ret) : ret;
      }
      else {
        long n = mpz_gcd_ui(NULL, b->data.gmp->num, labs(a->data.num));
        return QNumber_from_long(n);
      }
    }
  }
  else {
    if (!QNumber_big(b)) {
      if (!b->data.num) {
        QNumber ret = QNumber_from_other(a);
        return QNumber_sgn(b) < 0 ? QNumber_operator_unaray_minus(&ret) : ret;
      }
      else {
        long n = mpz_gcd_ui(NULL, a->data.gmp->num, labs(b->data.num));
        return QNumber_from_long(n);
      }
    }
    else {
      const int mpz_pool_size = 2;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* n = &mpz_pool[0];
      mpz_t* d = &mpz_pool[1];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set_si(*d, 1);
      mpz_gcd(*n, a->data.gmp->num, b->data.gmp->num);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
      return QNumber_assign(*n, *d, 1);
    }
  }
}

/*!
  \brief self += (a * b)

  self += (a * b)
*/

void QNumber_self_addmul(QNumber *self, const QNumber *a, const QNumber *b)
{
  if (!QNumber_big(a) && !QNumber_big(b)) {
    QNumber v = QNumber_operator_mul(a, b);
    QNumber_operator_self_plus(self, &v);
  }
  else {
    QNumber v;
    const int mpz_pool_size = 2;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* n = &mpz_pool[0];
    mpz_t* d = &mpz_pool[1];

    init_mpz_pool(mpz_pool, mpz_pool_size);

    if (QNumber_big(a) && QNumber_big(b)) {
      mpz_mul(*n, a->data.gmp->num, b->data.gmp->num);
      mpz_mul(*d, a->data.gmp->den, b->data.gmp->den);
    }
    else if (QNumber_big(a)) {
      mpz_mul_si(*n, a->data.gmp->num, b->data.num);
      mpz_mul_si(*d, a->data.gmp->den, b->den);
    }
    else {
      mpz_mul_si(*n, b->data.gmp->num, a->data.num);
      mpz_mul_si(*d, b->data.gmp->den, a->den);
    }
    /* return (*this += QNumber(n, d, true)); */
    v = QNumber_assign(*n, *d, 1);
    QNumber_operator_self_plus(self, &v);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
  }
}

/*!
  \brief returns 1, if self fits into an int, returns 0 otherwise,
                     fills out_value if 1 is returned

  returns 1, if self fits into an int, returns 0 otherwise

  \se if 1 is returned &out_value equals the value of self
*/

boolean QNumber_self_to_int(const QNumber *self, int* out_value)
{
  if (!QNumber_big(self)
      && QNumber_is_int_normal(self)
      && self->data.num <= LONG_MAX && self->data.num >= LONG_MIN) {
    *out_value = self->data.num;
    return true;
  }
  else {
    return false;
  }
}

/*!
  \brief returns a new QNumber equal to floor(self)

  returns a new QNumber equal to floor(self)
*/

QNumber QNumber_floor(const QNumber *self)
{
  if (QNumber_is_integer(self)) {
    return QNumber_from_other(self);
  }
  if (!QNumber_big(self)) {
    long r = self->data.num / self->den;
    if (self->data.num < 0) {
      --r;
    }
    return QNumber_from_long(r);
  }
  else {
    QNumber rvalue;
    const int mpz_pool_size = 2;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    mpz_t* tmp2 = &mpz_pool[1];

    init_mpz_pool(mpz_pool, mpz_pool_size);

    mpz_fdiv_q(*tmp1, self->data.gmp->num, self->data.gmp->den);
    mpz_set_ui(*tmp2, 1);
    rvalue = QNumber_assign(*tmp1, *tmp2, 1);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}

/*!
  \brief Returns a new qnumber equal to the numerator of self

  Returns a new qnumber equal to the numerator of self
*/

QNumber QNumber_get_num(const QNumber *self)
{
  if (QNumber_big(self)) {
    mpz_t one;
    mpz_set_ui(one, 1);
    return QNumber_from_two_mpzs(&(self->data.gmp->num), &one);
  }
  else {
    return QNumber_from_two_longs(self->data.num, 1);
  }
}


/*!
  \brief Returns a new qnumber equal to the denominator of self

  Returns a new qnumber equal to the denominator of self
*/

QNumber QNumber_get_den(const QNumber *self)
{
  if (QNumber_big(self)) {
    mpz_t one;
    mpz_set_ui(one, 1);
    return QNumber_from_two_mpzs(&(self->data.gmp->den), &one);
  }
  else {
    return QNumber_from_two_longs(self->den, 1);
  }
}

/*!
  \brief return 1 if self % other == 0, 1 otherwise

  return 1 if self % other == 0, 1 otherwise
*/

boolean QNumber_divides(const QNumber *self, const QNumber *other)
{
  nusmv_assert(QNumber_is_integer(self) && QNumber_is_integer(other));

  if (!QNumber_big(self)) {
    if (!QNumber_big(other)) {
      return other->data.num % self->data.num == 0;
    }
    else {
      const int mpz_pool_size = 1;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* r = &mpz_pool[0];
      boolean rvalue = false;

      init_mpz_pool(mpz_pool, mpz_pool_size);

      rvalue = mpz_mod_ui(*r, other->data.gmp->num, self->data.num) == 0;

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
      return rvalue;
    }
  }
  else {
    if (!QNumber_big(other)) {
      const int mpz_pool_size = 2;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* a = &mpz_pool[0];
      mpz_t* r = &mpz_pool[1];
      boolean rvalue = false;

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set_si(*a, other->data.num);
      mpz_mod(*r, *a, self->data.gmp->num);
      rvalue = (mpz_sgn(*r) == 0);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
      return rvalue;
    }
    else {
      const int mpz_pool_size = 1;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* r = &mpz_pool[0];
      int rvalue = false;

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_mod(*r, other->data.gmp->num, self->data.gmp->num);
      rvalue = (mpz_sgn(*r) == 0);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
      return rvalue;
    }
  }
  return 0;
}

/*!
  \brief divides self by other and fill q & r with the results

  

  \se q is set to left /(int) right, r is set to left % right
*/

void QNumber_divmod(const QNumber *self,
                    const QNumber *other,
                    QNumber *q,
                    QNumber *r)
{
  nusmv_assert(QNumber_is_integer(self) && QNumber_is_integer(other));
  if (!QNumber_big(self)) {
    if (!QNumber_big(other)) {
      ldiv_t res = ldiv(self->data.num, other->data.num);
      *q = QNumber_from_long(res.quot);
      *r = QNumber_from_long(res.rem);
    }
    else {
      const int mpz_pool_size = 1;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* a = &mpz_pool[0];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set_si(*a, self->data.num);

      /* clear/initialize the bignums for q and r */
      if (QNumber_big(q)) {
        mpz_set_si(q->data.gmp->den, 1);
      }
      else {
        q->den = 0;
        q->data.gmp = Gmp_alloc();
        mpz_init_set_si(q->data.gmp->den, 1);
        mpz_init(q->data.gmp->num);
      }
      if (QNumber_big(r)) {
        mpz_set_si(r->data.gmp->den, 1);
      }
      else {
        r->den = 0;
        r->data.gmp = Gmp_alloc();
        mpz_init_set_si(r->data.gmp->den, 1);
        mpz_init(r->data.gmp->num);
      }

      mpz_fdiv_qr(q->data.gmp->num,
                  r->data.gmp->num,
                  *a,
                  other->data.gmp->num);
      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
  }
  else {
    /* clear/initialize the bignums for q and r */
    if (QNumber_big(q)) {
      mpz_set_si(q->data.gmp->den, 1);
    }
    else {
      q->den = 0;
      q->data.gmp = Gmp_alloc();
      mpz_init_set_si(q->data.gmp->den, 1);
      mpz_init(q->data.gmp->num);
    }
    if (QNumber_big(r)) {
      mpz_set_si(r->data.gmp->den, 1);
    }
    else {
      r->den = 0;
      r->data.gmp = Gmp_alloc();
      mpz_init_set_si(r->data.gmp->den, 1);
      mpz_init(r->data.gmp->num);
    }
    if (!QNumber_big(other)) {
      const int mpz_pool_size = 1;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* b = &mpz_pool[0];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_set_si(*b, other->data.num);

      mpz_fdiv_qr(q->data.gmp->num, r->data.gmp->num, self->data.gmp->num, *b);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
    else {
      mpz_fdiv_qr(q->data.gmp->num,
                  r->data.gmp->num,
                  self->data.gmp->num,
                  other->data.gmp->num);
    }
  }
}

/*!
  \brief returns 1 if self has a denominator of 1

  returns 1 if self has a denominator of 1
*/

boolean QNumber_is_integer(const QNumber * self)
{
  int b = QNumber_big(self);
  return b ? QNumber_is_int_big(self) : QNumber_is_int_normal(self);
}

/*!
  \brief Normalizes self

  Normalizes self
*/

void QNumber_normalize(QNumber *self)
{
  /* Handle the case of zero separately, to avoid division by zero */
  if (!QNumber_big(self)) {
    if (self->data.num == 0) {
      self->den = 1;
      return;
    }
    else {
      long g = QNumber_gcd_long(self->data.num, self->den);
      self->data.num /= g;
      self->den /= g;

      /* Ensure that the denominator is positive */
      if (self->den < 0) {
        self->data.num = -(self->data.num);
        self->den = -(self->den);
      }
    }
  }
  else {
    nusmv_assert(self->data.gmp);
    if (!mpz_sgn(self->data.gmp->num)) {
      mpz_set_si(self->data.gmp->den, 1);
      return;
    }
    if (mpz_cmpabs_ui(self->data.gmp->den, 1) != 0) {
      const int mpz_pool_size = 1;
      mpz_t mpz_pool[mpz_pool_size];
      mpz_t* g = &mpz_pool[0];

      init_mpz_pool(mpz_pool, mpz_pool_size);

      mpz_gcd(*g, self->data.gmp->num, self->data.gmp->den);
      mpz_divexact(self->data.gmp->num, self->data.gmp->num, *g);
      mpz_divexact(self->data.gmp->den, self->data.gmp->den, *g);

      clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    }
    if (mpz_sgn(self->data.gmp->den) < 0) {
      mpz_neg(self->data.gmp->num, self->data.gmp->num);
      mpz_neg(self->data.gmp->den, self->data.gmp->den);
    }
  }
}

/*!
  \brief returns 0 if no overflow in lhs + rhs,
                      returns 1 otherwise, fill res with the result
                      of lhs + rhs

  returns 0 if no overflow in lhs + rhs,
                      returns 1 otherwise

  \se if 0 is returned *res = lhs + rhs
*/

boolean QNumber_add_overflow(long* res, long lhs, long rhs)
{
  if (!((rhs ^ lhs) < 0)) { /* test for +/- combo */
    /* either two negatives, or 2 positives */
    if (rhs < 0) {
      /* two negatives
       we use <= instead of < to fix the corner case of INT_MIN
       (see QNumber_fix_int_min()) */
      if (lhs <= LONG_MIN - rhs) {
        /* remember rhs < 0 */
        return true;
      }
      /* ok */
    }
    else {
      /* two positives */
      if (LONG_MAX - lhs < rhs) {
        return true;
      }
      /* OK */
    }
  }
  /* else overflow not possible */
  *res = lhs + rhs;
  return false;
}

/*!
  \brief returns 0 if no overflow in lhs - rhs,
                      returns 1 otherwise, fill out with result

  returns 0 if no overflow in lhs - rhs,
                      returns 1 otherwise

  \se if 0 is returned *res = lhs - rhs
*/

boolean QNumber_sub_overflow(long* res, long lhs, long rhs)
{
  if ((rhs ^ lhs) < 0) {
    /* test for +/- combo
       mixed positive and negative
       two cases - +X - -Y => X + Y - check for overflow against MaxInt()
       -X - +Y - check for overflow against MinInt() */
    if (lhs >= 0) { /* first case*/
      /* test is X - -Y > MaxInt()
         equivalent to X > MaxInt() - +Y
         Y == MinInt() creates special case
         Even 0 - MinInt() can't be done
         note that the special case collapses into the general
         case, due to the fact
         MaxInt() - MinInt() == -1, and lhs is non-negative */
      if (lhs > LONG_MAX + rhs) {
        /* remember that rhs is negative */
        return true;
      }
      /* fall through to return value */
    }
    else {
      /* second case
       test is -X - Y < MinInt()
       or      -X < MinInt() + Y
       we do not have the same issues because
       abs(MinInt()) > MaxInt()
       we use <= instead of < to fix the corner case of INT_MIN
       (see QNumber::fix_int_min()) */
      if (lhs <= LONG_MIN + rhs) {
        return true;
      }
      /* fall through to return value */
    }
  }
  /* else
   both negative, or both positive
   no possible overflow */
  *res = lhs - rhs;
  return false;
}

/*!
  \brief returns 0 if no overflow in lhs * rhs,
                      returns 1 otherwise, fills res with the result

  returns 0 if no overflow in lhs * rhs,
                      returns 1 otherwise

  \se if 0 is returned *res = lhs * rhs
*/

boolean QNumber_mul_overflow(long *res, long lhs, long rhs)
{
  if (sizeof(long long) == sizeof(long) * 2) {
    /*
     fast path: on 32-bit systems we can cast to a 64-bit integer and
     perform multiplication there
     */
    long long lhs_ = (long long) (lhs);
    long long rhs_ = (long long) (rhs);
    long long tmp = lhs_ * rhs_;
    /* use <= instead of < to fix the corner case of INT_MIN
     (see QNumber::fix_int_min()) */
    if (tmp > LONG_MAX || tmp <= LONG_MIN) {
      return true;
    }
    *res = (long) (tmp); /* TODO */
    return false;
  }
  else {
    /* on 64-bit machines, we check whether (|lhs| > LONG_MAX/|rhs|)
     since |LONG_MAX| < |LONG_MIN|, this should be enough...
     moreover, since both arguments to division are positive, division
     can't overflow */
    if (rhs && labs(lhs) > LONG_MAX / labs(rhs)) {
      return true;
    }
    *res = lhs * rhs;
    return false;
  }
}


/*!
  \brief returns 0 if no overflow in lhs / rhs,
                      returns 1 otherwise, fill res with the result

  returns 0 if no overflow in lhs / rhs,
                      returns 1 otherwise

  \se if 0 is returned *res = lhs / rhs
*/

boolean QNumber_div_overflow(long* res, long lhs, long rhs)
{
  if (lhs == LONG_MIN && rhs == -1) {
    return true;
  }
  *res = lhs / rhs;
  return false;
}

/*!
  \brief returns 0 if string represents a number in base base
                      otherwise assigns target with represented number.

  returns 0 if string represents a number in base base

  \se if 0 is returned, numb is set to the value of string
*/

int QNumber_integer_from_string(char* str,
                                char* error,
                                int base,
                                QNumber* target)
{
  UNUSED_PARAM(error);
  if (str == (char*) NULL) {
    return -1;
  }
  else {
    const int mpz_pool_size = 2;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t*  value = &mpz_pool[0];
    mpz_t* den = &mpz_pool[1];
    int succes = 0;

    init_mpz_pool(mpz_pool, mpz_pool_size);

    succes = mpz_set_str(*value, str, base);

    if (succes == 0) {
      QNumber v;

      mpz_set_ui(*den, 1);
      v = QNumber_from_two_mpzs(value, den);
      *target = v;
    }

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return succes;
  }
}

/*!
  \brief Returns a string repr of number in base base, must
                     be freed by caller!

  Returns a string repr of number in base base
*/

char* QNumber_print_integer(const QNumber* n, int base)
{
  const int mpz_pool_size = 1;
  mpz_t mpz_pool[mpz_pool_size];
  mpz_t* target = &mpz_pool[0];
  unsigned int i = 0;
  int needed_size =0;
  char* buffer = (char*) NULL;

  init_mpz_pool(mpz_pool, mpz_pool_size);

  nusmv_assert(QNumber_is_int_big(n) || QNumber_is_int_normal(n));

  if (QNumber_big(n)) {
    mpz_set(*target, n->data.gmp->num);
  }
  else {
    mpz_set_ui(*target, n->data.num);
  }

  needed_size = mpz_sizeinbase(*target, base) + 2;
  buffer = (char*) ALLOC(char, needed_size);

  mpz_get_str(buffer, base, *target);
  for (i = 0; i < strlen(buffer); i++) {
    int dif = 'a' - 'A';
    if (buffer[i] >= 'a' && buffer[i] <= 'z') {
      buffer[i] = buffer[i] - dif;
    }
  }

  clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
  return buffer;
}

/*---------------------------------------------------------------------------*/
/* BitVector functions                                                       */
/*---------------------------------------------------------------------------*/

/*!
  \brief checks that n is indeed a natural number >= 0

  checks that n is indeed a natural number >= 0
*/

void BVQNumber_check_bv(const QNumber* n)
{
  nusmv_assert(QNumber_is_int_big(n) || QNumber_is_int_normal(n));
  nusmv_assert(QNumber_sgn(n) >= 0);
}

/*!
  \brief returns 1 if n can be represented with the given number
                     of bits

  returns 1 if n can be represented with the given number
                     of bits.
*/

boolean BVQNumber_fits(const QNumber* n, size_t num_bits)
{
  BVQNumber_check_bv(n);
  if (!QNumber_big(n)) {
    if (num_bits >= QNumber_NUM_BITS_NORMAL) {
      return true;
    }
    else {
      return (((unsigned long) n->data.num)
              < ((unsigned long) (1ul << num_bits)));
    }
  }
  else {
    return mpz_sizeinbase(n->data.gmp->num, 2) <= num_bits;
  }
}

/*!
  \brief return 1 if n is a power of two.

  return 1 if n is a power of two.

  \se 1 is returned and  out_n is not NULL,
                      the exponent will be stored in *out_n
*/

boolean BVQNumber_is_pow2(const QNumber* n, size_t* out_n)
{
  BVQNumber_check_bv(n);

  if (!QNumber_big(n)) {
    if (n->data.num && (n->data.num & (n->data.num - 1)) == 0) {
      if (out_n) {
        unsigned long m = n->data.num;
        *out_n = 0;
        m >>= 1;
        while (m) {
          ++(*out_n);
          m >>= 1;
        }
      }
      return true;
    }
    else {
      return false;
    }
  }
  else {
    if (mpz_popcount(n->data.gmp->num) == 1) {
      if (out_n) {
        *out_n = mpz_scan1(n->data.gmp->num, 0);
      }
      return true;
    }
    else {
      return false;
    }
  }
}

/*!
  \brief returns 2^n

  returns 2^n
*/

QNumber BVQNumber_pow2(size_t n)
{
  QNumber rvalue;
  const int mpz_pool_size = 2;
  mpz_t mpz_pool[mpz_pool_size];
  mpz_t* tmpnum = &mpz_pool[0];
  mpz_t* one = &mpz_pool[1];

  init_mpz_pool(&mpz_pool[0], mpz_pool_size);

  mpz_set_si(*tmpnum, 1);
  mpz_mul_2exp(*tmpnum, *tmpnum, n);
  if (mpz_fits_slong_p(*tmpnum)) {
     rvalue = QNumber_from_two_longs(mpz_get_si(*tmpnum), 1);
  }
  else {
    mpz_set_ui(*one, 1);
    rvalue = QNumber_from_two_mpzs(tmpnum, one);
  }

  clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
  return rvalue;
}

/*!
  \brief returns the 2's complement of the given number,
                      using the given number of bits

  returns the 2's complement of the given number,
                      using the given number of bits
*/

QNumber BVQNumber_twos_complement(const QNumber* n, size_t width)
{
  if (QNumber_sgn(n) < 0) {
    QNumber ret = BVQNumber_pow2(width);
    QNumber_operator_self_plus(&ret, n);
    return ret;
  }
  return *n;
}

/*!
  \brief returns 1 if the given bit of the binary representation
                      of the number is set, 0 otherwise

  returns 1 if the given bit of the binary representation
                      of the number is set, 0 otherwise
*/

boolean BVQNumber_test_bit(const QNumber* n, size_t index)
{
  BVQNumber_check_bv(n);

  if (QNumber_big(n)) {
    return mpz_tstbit(n->data.gmp->num, index);
  }
  else {
    if (index >= QNumber_NUM_BITS_NORMAL) {
      return false;
    }
    else {
      return (n->data.num & (1ul << index)) != 0;
    }
  }
}

/*!
  \brief sets the given bit of the binary representation of the
                      number to the given value.

  sets the given bit of the binary representation of the
                      number to the given value.
*/

inline void BVQNumber_set_bit(QNumber* n, size_t index, int value)
{
  BVQNumber_check_bv(n);
  nusmv_assert(value >= 0 && value <= 1);
  if (QNumber_big(n)) {
    if (value) {
      mpz_setbit(n->data.gmp->num, index);
    }
    else {
      mpz_clrbit(n->data.gmp->num, index);
    }
  }
  /* we must avoid making the number signed! */
  else if (index < QNumber_NUM_BITS_NORMAL - 1) {
    if (value) {
      n->data.num |= (1ul << index);
    }
    else {
      n->data.num &= ~(1ul << index);
    }
  }
  else if (value) {
    QNumber_make_big(n);
    mpz_setbit(n->data.gmp->num, index);
  }
}

/*!
  \brief returns the bitwise and of n1 and n2

  returns the bitwise and of n1 and n2
*/

QNumber BVQNumber_bit_and(const QNumber* n1, const QNumber* n2)
{
  BVQNumber_check_bv(n1);
  BVQNumber_check_bv(n2);

  if (!QNumber_big(n1) && !QNumber_big(n2)) {
    return QNumber_from_two_longs(BVQNumber_bit_and_l(n1->data.num,
                                                      n2->data.num), 1);
  }
  else {
    const int mpz_pool_size = 1;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    QNumber rvalue;

    init_mpz_pool(&mpz_pool[0], mpz_pool_size);

    if (!QNumber_big(n1)) {
      mpz_set_si(*tmp1, n1->data.num);
      mpz_and(*tmp1, *tmp1, n2->data.gmp->num);
    }
    else if (!QNumber_big(n2)) {
      mpz_set_si(*tmp1, n2->data.num);
      mpz_and(*tmp1, *tmp1, n1->data.gmp->num);
    }
    else {
      mpz_and(*tmp1, n1->data.gmp->num, n2->data.gmp->num);
    }

    if (QNumber_big(n1)) {
      rvalue = QNumber_assign(*tmp1, n1->data.gmp->den, 1);
    }
    else {
      rvalue = QNumber_assign(*tmp1, n2->data.gmp->den, 1);
    }

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}


/*!
  \brief returns the bitwise or of n1 and n2

  returns the bitwise or of n1 and n2
*/

inline QNumber BVQNumber_bit_or(const QNumber* n1, const QNumber* n2)
{
  BVQNumber_check_bv(n1);
  BVQNumber_check_bv(n2);

  if (!QNumber_big(n1) && !QNumber_big(n2)) {
    /* fast path */
    return QNumber_from_two_longs(BVQNumber_bit_or_l(n1->data.num,
                                                     n2->data.num), 1);
  }
  else {
    const int mpz_pool_size = 1;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    QNumber rvalue;

    init_mpz_pool(&mpz_pool[0], mpz_pool_size);

    if (!QNumber_big(n1)) {
      mpz_set_si(*tmp1, n1->data.num);
      mpz_ior(*tmp1, *tmp1, n2->data.gmp->num);
    }
    else if (!QNumber_big(n2)) {
      mpz_set_si(*tmp1, n2->data.num);
      mpz_ior(*tmp1, *tmp1, n1->data.gmp->num);
    }
    else {
      mpz_ior(*tmp1, n1->data.gmp->num, n2->data.gmp->num);
    }

    if (QNumber_big(n1)) {
      rvalue = QNumber_assign(*tmp1, n1->data.gmp->den, 1);
    }
    else {
      rvalue = QNumber_assign(*tmp1, n2->data.gmp->den, 1);
    }

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}


/*!
  \brief returns the bitwise xor of n1 and n2

  returns the bitwise xor of n1 and n2
*/

inline QNumber BVQNumber_bit_xor(const QNumber* n1, const QNumber* n2)
{
  BVQNumber_check_bv(n1);
  BVQNumber_check_bv(n2);

  if (!QNumber_big(n1) && !QNumber_big(n2)) {
    /* fast path */
    return QNumber_from_two_longs(BVQNumber_bit_xor_l(n1->data.num,
                                                      n2->data.num), 1);
  }
  else {
    const int mpz_pool_size = 1;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    QNumber rvalue;

    init_mpz_pool(&mpz_pool[0], mpz_pool_size);

    if (!QNumber_big(n1)) {
      mpz_set_si(*tmp1, n1->data.num);
      mpz_xor(*tmp1, *tmp1, n2->data.gmp->num);
    }
    else if (!QNumber_big(n2)) {
      mpz_set_si(*tmp1, n2->data.num);
      mpz_xor(*tmp1, *tmp1, n1->data.gmp->num);
    }
    else {
      mpz_xor(*tmp1, n1->data.gmp->num, n2->data.gmp->num);
    }

    if (QNumber_big(n1)) {
      rvalue = QNumber_assign(*tmp1, n1->data.gmp->den, 1);
    }
    else {
      rvalue = QNumber_assign(*tmp1, n2->data.gmp->den, 1);
    }

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}

/*!
  \brief returns the 1's complement of n

  returns the 1's complement of n
*/

inline QNumber BVQNumber_bit_complement(const QNumber* n)
{
  BVQNumber_check_bv(n);
  if (!QNumber_big(n)) {
    return QNumber_from_two_longs(~(n->data.num), 1);
  }
  else {
    const int mpz_pool_size = 1;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    QNumber rvalue;

    init_mpz_pool(&mpz_pool[0], mpz_pool_size);

    mpz_com(*tmp1, n->data.gmp->num);
    rvalue = QNumber_assign(*tmp1, n->data.gmp->den, 1);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}

/*!
  \brief returns (n << i)

  returns (n << i)
*/

inline QNumber BVQNumber_bit_left_shift(const QNumber* n, size_t i)
{
  const int mpz_pool_size = 2;
  mpz_t mpz_pool[mpz_pool_size];
  mpz_t* tmp1 = &mpz_pool[0];
  mpz_t* tmp2 = &mpz_pool[1];
  QNumber rvalue;

  BVQNumber_check_bv(n);

  init_mpz_pool(&mpz_pool[0], mpz_pool_size);

  if (!QNumber_big(n)) {
    mpz_set_si(*tmp1, n->data.num);
  } else {
    mpz_set(*tmp1, n->data.gmp->num);
  }

  mpz_mul_2exp(*tmp1, *tmp1, i);
  if (mpz_fits_slong_p(*tmp1)) {
    rvalue = QNumber_from_two_longs(mpz_get_si(*tmp1), 1);
  }
  else {
    mpz_set_si(*tmp2, 1);
    rvalue =  QNumber_assign(*tmp1, *tmp2, 1);
  }

  clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
  return rvalue;
}

/*!
  \brief returns (n >> i)

  returns (n >> i)
*/

inline QNumber BVQNumber_bit_right_shift(const QNumber* n, size_t i)
{
  BVQNumber_check_bv(n);
  if (!QNumber_big(n)) {
    if (i >= QNumber_NUM_BITS_NORMAL) {
      return QNumber_from_two_longs(0, 1);
    }
    else {
      return QNumber_from_two_longs(n->data.num >> i, 1);
    }
  }
  else {
    const int mpz_pool_size = 1;
    mpz_t mpz_pool[mpz_pool_size];
    mpz_t* tmp1 = &mpz_pool[0];
    QNumber rvalue;

    init_mpz_pool(&mpz_pool[0], mpz_pool_size);

    mpz_fdiv_q_2exp(*tmp1, n->data.gmp->num, i);
    rvalue = QNumber_assign(*tmp1, n->data.gmp->den, 1);

    clean_mpz_pool(&mpz_pool[0], mpz_pool_size);
    return rvalue;
  }
}


/*!
  \brief returns the index of the first bit 1, starting from
                      start_index will return that max value of size_t if no
                      bit 1 is found

  returns the index of the first bit 1, starting from
                      start_index will return that max value of size_t if no
                      bit 1 is found
*/

inline size_t BVQNumber_scan_bit_1(const QNumber* n, size_t start_index)
{
  BVQNumber_check_bv(n);
  if (!QNumber_big(n)) {
      /* naive algorithm, hardly a bottleneck for us :-) */
    if (start_index >= QNumber_NUM_BITS_NORMAL) {
      return get_max_size_t_value();
    }
    else {
      long r = (n->data.num >> start_index);
      while (r) {
        if (r & 1) return start_index;
          ++start_index;
          r = r >> 1;
      }
      return get_max_size_t_value();
    }
  }
  else {
    size_t ret = mpz_scan1(n->data.gmp->num, start_index);
    if (ret == ULONG_MAX) {
        ret = get_max_size_t_value();
    }
    return ret;
  }
}

/*!
  \brief Tries to store n int a positive signed long.
                      returns 1 if this is possible, false otherwise

  Tries to store n int a positive signed long.
                      returns 1 if this is possible, false otherwise

  \se if 1 is returned out is set to the value of n
*/

boolean BVQNumber_to_long(const QNumber* n, long* out)
{
  BVQNumber_check_bv(n);
  if (!QNumber_big(n)) {
      *out = n->data.num;
      return true;
  } else if (mpz_fits_slong_p(n->data.gmp->num)) {
      *out = mpz_get_si(n->data.gmp->num);
      return true;
  }
  return false;
}

/*!
  \brief Return bitwise and of two longs

  Return bitwise and of two longs
*/

long BVQNumber_bit_and_l(long a, long b) {
    return a & b;
}

/*!
  \brief Return bitwise or of two longs

  Return bitwise or of two longs
*/

long BVQNumber_bit_or_l(long a, long b) {
    return a | b;
}

/*!
  \brief 

  Return bitwise xor of two longs
*/

long BVQNumber_bit_xor_l(long a, long b) {
    return a ^ b;
}

/*!
  \brief Return maximum value of size_t, the impl of this function
                      is NOT optimal if a more formal method can be found
                      please change it!

  Return maximum value of size_t, the impl of this function
                      is NOT optimal if a more formal method can be found
                      please change it!
*/

size_t get_max_size_t_value() {
  return (size_t) -1;
}


/*!
  \brief cleans a pool of mpz structs, of size nr_to_clean

  
*/


void clean_mpz_pool(mpz_t* mpz_pool, size_t nr_to_clean) {
  size_t i;

  for (i = 0; i < nr_to_clean; i++) {
    mpz_clear((mpz_pool[i]));
  }
}

/*!
  \brief inits a pool of mpz structs, of size nr_to_init

  
*/


void init_mpz_pool(mpz_t* mpz_pool, size_t nr_to_init) {
  size_t i;

  for (i = 0; i < nr_to_init; i++) {
    mpz_init((mpz_pool[i]));
  }
}
