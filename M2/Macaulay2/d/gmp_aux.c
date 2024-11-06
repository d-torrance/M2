/* some routines to augment the gmp library */
#include <M2/config.h>
#include "M2/math-include.h"
#include <string.h>

int mpz_hash(mpz_srcptr x) {
  int h = 0;
  int n = x->_mp_size;
  int i;
  if (n < 0) n = -n;
  for (i = 0; i<n; i++, h*=3737) h += x->_mp_d[i];
  if (x->_mp_size < 0) h = -h;
  return h;
}

int mpfr_hash(mpfr_srcptr x) {
  int h = 0;
  int n = (x->_mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
  int i;
  if (0 != mpfr_sgn(x))
    for (i = 0; i<n; i++, h*=3737) h += x->_mpfr_d[i];
  return 777 + h * 3737 + x->_mpfr_exp + 11 * x->_mpfr_sign;
}


int mpfi_hash(mpfi_srcptr x) { // Really not sure if this is doing the right thing.
    int h = 0;
    int n_left = (x->left._mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
    int n_right = (x->right._mpfr_prec+mp_bits_per_limb-1)/mp_bits_per_limb;
    int i;
    if (0 != mpfr_sgn(&x->left))
    for (i = 0; i<n_left; i++, h*=3737) h += x->left._mpfr_d[i];
    if (0 != mpfr_sgn(&x->right))
    for (i = 0; i<n_right; i++, h*=3737) h += x->right._mpfr_d[i];
    return 777 + h * 3737 + x->left._mpfr_exp + x->right._mpfr_exp + 11 * x->left._mpfr_sign + 11 * x->right._mpfr_sign;
}

void mp_free_str(char *str){
    void (*free_function) (void *ptr, size_t size);
    mp_get_memory_functions(NULL,NULL,&free_function);
    free_function(str,strlen(str)+1);

}

char *mpfr_dragon4(mpfr_exp_t *expptr, mpfr_srcptr x)
{
  int i, condition;
  mpfr_exp_t e, k;
  mpfr_prec_t p;
  mpz_t f, r, s, m_plus, m_minus, one, tmp_zz;
  mpfr_t tmp_rr;
  char *str;
  void *(*alloc_function)(size_t);

  mp_get_memory_functions(&alloc_function, NULL, NULL);
  str = alloc_function(mpfr_get_str_ndigits(10, mpfr_get_prec(x)) + 1);

  mpz_inits(f, r, s, m_plus, m_minus, one, tmp_zz, NULL);
  mpfr_init(tmp_rr);

  p = mpfr_get_prec(x);
  e = mpfr_get_z_2exp(f, x);
  mpz_set_ui(one, 1);
  mpz_mul_2exp(tmp_zz, one, p - 1);

  /* burger/dybvig table 1 */
  if (e >= 0) {
    if (mpz_cmp(f, tmp_zz) == 0) {
      mpz_mul_2exp(r, f, e + 2);
      mpz_set_ui(s, 4);
      mpz_mul_2exp(m_plus, one, e + 1);
      mpz_mul_2exp(m_minus, one, e);
    } else {
      mpz_mul_2exp(r, f, e + 1);
      mpz_set_ui(s, 2);
      mpz_mul_2exp(m_plus, one, e);
      mpz_set(m_minus, m_plus);
    }
  } else {
    if ((e > mpfr_get_emin()) && (mpz_cmp(f, tmp_zz) == 0)) {
      mpz_mul_2exp(r, f, 2);
      mpz_mul_2exp(s, one, 2 - e);
      mpz_set_ui(m_plus, 2);
      mpz_set(m_minus, one);
    } else {
      mpz_mul_2exp(r, f, 1);
      mpz_mul_2exp(s, one, 1 - e);
      mpz_set(m_minus, one);
      mpz_set(m_plus, one);
    }
  }

  /* TODO: simplify this? see burger/dybvig section 3.2 */
  mpfr_set_z(tmp_rr, r, MPFR_RNDN);
  mpfr_add_z(tmp_rr, tmp_rr, m_plus, MPFR_RNDN);
  mpfr_div_z(tmp_rr, tmp_rr, s, MPFR_RNDN);
  mpfr_log(tmp_rr, tmp_rr, MPFR_RNDN);
  mpfr_div_d(tmp_rr, tmp_rr, 2.3025850929940459 /* log 10 */, MPFR_RNDN);
  mpfr_ceil(tmp_rr, tmp_rr);
  k = mpfr_get_si(tmp_rr, MPFR_RNDN);
  *expptr = k;

  if (k >= 0) {
    mpz_ui_pow_ui(tmp_zz, 10, k);
    mpz_mul(s, s, tmp_zz);
  } else {
    mpz_ui_pow_ui(tmp_zz, 10, -k);
    mpz_mul(r, r, tmp_zz);
    mpz_mul(m_plus, m_plus, tmp_zz);
    mpz_mul(m_minus, m_minus, tmp_zz);
  }

  for(i = 0; ; i++) {
    mpz_mul_ui(r, r, 10);
    mpz_fdiv_qr(tmp_zz, r, r, s);
    str[i] = '0' + mpz_get_ui(tmp_zz);
    mpz_mul_ui(m_plus, m_plus, 10);
    mpz_mul_ui(m_minus, m_minus, 10);

    condition = 0;

    /* condition (1) */
    if (mpz_cmp(r, m_minus) < 0)
      condition |= 1;

    /* condition (2) */
    mpz_add(tmp_zz, r, m_plus);
    if (mpz_cmp(tmp_zz, s) > 0) /* typo in burger/dybvig -- they use "<" */
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

  mpz_clears(f, r, s, m_plus, m_minus, tmp_zz, NULL);
  mpfr_clear(tmp_rr);

  return str;
}

/*
 Local Variables:
 compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
