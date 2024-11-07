/* mpfrtoa.c -- MPFR -> ASCII

Copyright 2024 Doug Torrance <dtorrance@piedmont.edu>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
“AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

/* Based on Burger & Dybvig's improvements to Steel & White's Dragon4
algorithm.

Burger, Robert G., and R. Kent Dybvig. "Printing floating-point
  numbers quickly and accurately." ACM SIGPLAN Notices 31, no. 5 (1996):
  108-116.

Steele Jr, Guy L., and Jon L. White. "How to print floating-point
  numbers accurately." In Proceedings of the ACM SIGPLAN 1990 conference
  on Programming language design and implementation, pp. 112-126. 1990.
*/

#include <gmp.h>
#include <mpfr.h>
#include <math.h>

#include <stdio.h> /* REMOVE ME */

static const char num_to_text36[] = "0123456789abcdefghijklmnopqrstuvwxyz";
static const char num_to_text62[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz";

char *mpfrtoa(char *str, mpfr_exp_t *expptr, int base, size_t num_digits,
	      mpfr_t op)
{
  const char *num_to_text;
  int is_negative, i, condition;
  mpfr_exp_t e, k;
  mpfr_prec_t p;
  mpz_t f, r, s, m_plus, m_minus, tmp;
  mpfr_t v;

  /* if NaN, infinity, or zero, fall back to mpfr_get_str */
  if (!mpfr_number_p(op) || mpfr_zero_p(op))
    return mpfr_get_str(str, expptr, base, num_digits, op, MPFR_RNDN);

  is_negative = mpfr_sgn(op) < 0;
  p = mpfr_get_prec(op);

  num_to_text = (2 <= base && base <= 36) ? num_to_text36 : num_to_text62;
  base = (base > 0) ? base : -base;

  if (num_digits == 0)
    num_digits = mpfr_get_str_ndigits(base, p);

  if (str == NULL) {
    void *(*alloc_function)(size_t);

    mp_get_memory_functions(&alloc_function, NULL, NULL);
    str = alloc_function(is_negative + num_digits + 1);
  }

  mpz_inits(f, r, s, m_plus, m_minus, tmp, NULL);

  /* deal with negatives */
  i = 0;
  if (is_negative)
    str[i++] = '-';
  mpfr_init2(v, p);
  mpfr_abs(v, op, MPFR_RNDN); /* rounding mode ignored since same precision */

  e = mpfr_get_z_2exp(f, v);
  mpz_ui_pow_ui(tmp, 2, p - 1);

  /* burger/dybvig table 1 */
  if (e >= 0) {
    if (mpz_cmp(f, tmp) == 0) {
      mpz_mul_2exp(r, f, e + 2);
      mpz_set_ui(s, 4);
      mpz_ui_pow_ui(m_plus, 2, e + 1);
      mpz_ui_pow_ui(m_minus, 2, e);
    } else {
      mpz_mul_2exp(r, f, e + 1);
      mpz_set_ui(s, 2);
      mpz_ui_pow_ui(m_plus, 2, e);
      mpz_set(m_minus, m_plus);
    }
  } else {
    if ((e > mpfr_get_emin()) && (mpz_cmp(f, tmp) == 0)) {
      mpz_mul_2exp(r, f, 2);
      mpz_ui_pow_ui(s, 2, 2 - e);
      mpz_set_ui(m_plus, 2);
      mpz_set_ui(m_minus, 1);
    } else {
      mpz_mul_2exp(r, f, 1);
      mpz_ui_pow_ui(s, 2, 1 - e);
      mpz_set_ui(m_minus, 1);
      mpz_set_ui(m_plus, 1);
    }
  }

  /* TODO: simplify this? see burger/dybvig section 3.2 */
  /* only works if e + p - 1< 10^14 */
  k = ceil((e + p - 1)*(1/log2(base)));
  *expptr = k;
  printf("k = %ld\n", k);

  if (k >= 0) {
    mpz_ui_pow_ui(tmp, 10, k);
    mpz_mul(s, s, tmp);
  } else {
    mpz_ui_pow_ui(tmp, 10, -k);
    mpz_mul(r, r, tmp);
    mpz_mul(m_plus, m_plus, tmp);
    mpz_mul(m_minus, m_minus, tmp);
  }

  for(;; i++) {
    mpz_mul_ui(r, r, 10);
    mpz_fdiv_qr(tmp, r, r, s);
    str[i] = num_to_text[mpz_get_ui(tmp)];
    mpz_mul_ui(m_plus, m_plus, 10);
    mpz_mul_ui(m_minus, m_minus, 10);

    condition = 0;

    /* condition (1) */
    if (mpz_cmp(r, m_minus) < 0)
      condition |= 1;

    /* condition (2) */
    mpz_add(tmp, r, m_plus);
    if (mpz_cmp(tmp, s) > 0) /* typo in burger/dybvig -- they use "<" */
      condition |= 2;

    if (condition == 1) { /* (1) true, (2) false */
      str[i + 1] = '\0';
      break;
    } else if (condition == 2) { /* (1) false, (2) true */
      str[i]++;
      str[i + 1] = '\0';
      break;
    } else if (condition == 3) { /* (1) & (2) both true */
      /* TODO */
      str[i + 1] = '\0';
      break;
    }
  }

  mpz_clears(f, r, s, m_plus, m_minus, tmp, NULL);

  return str;
}
