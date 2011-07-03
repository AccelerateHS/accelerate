
#ifndef ACCELERATE_H
#define ACCELERATE_H
import qualified Data.Array.Accelerate.Internal.Check as Ck
import qualified Data.Array.Accelerate.Internal.Trace as T

/*
 * Tracing
 */
#define INFO  (T.message T.Loud   __FILE__ __LINE__)
#define DEBUG (T.message T.Normal __FILE__ __LINE__)
#define TRACE (T.trace   T.Normal __FILE__ __LINE__)


/*
 * Internal checks
 */
#define ERROR(f)  (Ck.f __FILE__ __LINE__)
#define ASSERT (Ck.assert __FILE__ __LINE__)
#define ENSURE (Ck.f __FILE__ __LINE__)
#define CHECK(f) (Ck.f __FILE__ __LINE__)

#define BOUNDS_ERROR(f) (ERROR(f) Ck.Bounds)
#define BOUNDS_ASSERT (ASSERT Ck.Bounds)
#define BOUNDS_ENSURE (ENSURE Ck.Bounds)
#define BOUNDS_CHECK(f) (CHECK(f) Ck.Bounds)

#define UNSAFE_ERROR(f) (ERROR(f) Ck.Unsafe)
#define UNSAFE_ASSERT (ASSERT Ck.Unsafe)
#define UNSAFE_ENSURE (ENSURE Ck.Unsafe)
#define UNSAFE_CHECK(f) (CHECK(f) Ck.Unsafe)

#define INTERNAL_ERROR(f) (ERROR(f) Ck.Internal)
#define INTERNAL_ASSERT (ASSERT Ck.Internal)
#define INTERNAL_ENSURE (ENSURE Ck.Internal)
#define INTERNAL_CHECK(f) (CHECK(f) Ck.Internal)

#endif

