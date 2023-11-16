
#include <quadmath.h>
#include <stdio.h>

typedef _Float128 f128;

union ieee754_quad {
  f128 as_float128;
  struct {
#if WORDS_BIGENDIAN
    uint64_t negative:1;
    uint64_t exponent:15;
    uint64_t mantissa0:48;
    uint64_t mantissa1;
#else
    uint64_t mantissa1;
    uint64_t mantissa0:48;
    uint64_t exponent:15;
    uint64_t negative:1;
#endif
  } as_uint128;
};

/* Operations from Read and Show
 */
void _readq(f128* r, const char* str) { *r = strtoflt128(str, NULL); }
void _showq(char* buf, size_t n, f128 *a) { quadmath_snprintf(buf, n, "%Qf", *a); }

/* Operations from Num
 */
void _addq(f128* r, const f128* a, const f128* b) { *r = *a + *b; }
void _subq(f128* r, const f128* a, const f128* b) { *r = *a - *b; }
void _mulq(f128* r, const f128* a, const f128* b) { *r = *a * *b; }
void _negateq(f128* r, const f128* a) { *r = - *a; }
void _absq(f128* r, const f128* a) { *r = fabsq(*a); }
void _signumq(f128* r, const f128* a) { *r = (*a > 0.0q) - (*a < 0.0q); }

/* Operations from Fractional
 */
void _divq(f128* r, const f128* a, const f128* b) { *r = *a / *b; }
void _recipq(f128* r, const f128* a) { *r = 1.0q / *a; }

/* Operations from Floating
 */
void _piq(f128* r) { *r = M_PIq; }
void _expq(f128* r, const f128* a) { *r = expq(*a); }
void _logq(f128* r, const f128* a) { *r = logq(*a); }
void _sqrtq(f128* r, const f128* a) { *r = sqrtq(*a); }
void _powq(f128* r, const f128* a, const f128* b) { *r = powq(*a, *b); }
void _sinq(f128* r, const f128* a) { *r = sinq(*a); }
void _cosq(f128* r, const f128* a) { *r = cosq(*a); }
void _tanq(f128* r, const f128* a) { *r = tanq(*a); }
void _asinq(f128* r, const f128* a) { *r = asinq(*a); }
void _acosq(f128* r, const f128* a) { *r = acosq(*a); }
void _atanq(f128* r, const f128* a) { *r = atanq(*a); }
void _sinhq(f128* r, const f128* a) { *r = sinhq(*a); }
void _coshq(f128* r, const f128* a) { *r = coshq(*a); }
void _tanhq(f128* r, const f128* a) { *r = tanhq(*a); }
void _asinhq(f128* r, const f128* a) { *r = asinhq(*a); }
void _acoshq(f128* r, const f128* a) { *r = acoshq(*a); }
void _atanhq(f128* r, const f128* a) { *r = atanhq(*a); }
void _log1pq(f128* r, const f128* a) { *r = log1pq(*a); }
void _expm1q(f128* r, const f128* a) { *r = expm1q(*a); }

/* Operations from RealFrac
 */
void _roundq(f128* r, const f128* a) { *r = roundq(*a); }
void _truncq(f128* r, const f128* a) { *r = truncq(*a); }
void _floorq(f128* r, const f128* a) { *r = floorq(*a); }
void _ceilq(f128* r, const f128* a) { *r = ceilq(*a); }

/* Operations from RealFloat
 */
uint32_t _isnanq(const f128* a) { return isnanq(*a); }
uint32_t _isinfq(const f128* a) { return isinfq(*a); }
void _frexpq(f128* r, const f128* a, int32_t* b) { *r = frexpq(*a, b); }
void _ldexpq(f128* r, const f128* a, int32_t b) { *r = ldexpq(*a, b); }
void _atan2q(f128* r, const f128* a, const f128* b) { *r = atan2q(*a, *b); }

/* A (single/double/quad) precision floating point number is denormalized iff:
 *   - exponent is zero
 *   - mantissa is non-zero
 *   - (don't care about the sign bit)
 */
uint32_t _isdenormq(const f128* a)
{
  union ieee754_quad u;
  u.as_float128 = *a;

  return (u.as_uint128.exponent == 0
      && (u.as_uint128.mantissa0 != 0 || u.as_uint128.mantissa1 != 0));
}

/* A (single/double/quad) precision floating point number is negative zero iff:
 *   - sign bit is set
 *   - all other bits are zero
 */
uint32_t _isnegzeroq(const f128* a)
{
  union ieee754_quad u;
  u.as_float128 = *a;

  return (
      u.as_uint128.negative &&
      u.as_uint128.exponent  == 0 &&
      u.as_uint128.mantissa0 == 0 &&
      u.as_uint128.mantissa1 == 0
  );
}

/* Operations from Ord
 */
uint32_t _ltq(const f128* a, const f128* b) { return *a < *b; }
uint32_t _leq(const f128* a, const f128* b) { return *a <= *b; }
uint32_t _gtq(const f128* a, const f128* b) { return *a > *b; }
uint32_t _geq(const f128* a, const f128* b) { return *a <= *b; }
void _fminq(f128* r, const f128* a, const f128* b) { *r = fminq(*a, *b); }
void _fmaxq(f128* r, const f128* a, const f128* b) { *r = fmaxq(*a, *b); }

