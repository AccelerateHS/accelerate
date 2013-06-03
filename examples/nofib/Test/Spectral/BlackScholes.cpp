
#include <math.h>


/*
 * Polynomial approximation of cumulative normal distribution function
 */
template <typename T>
static T CND(T d)
{
    const T       a1 = 0.31938153;
    const T       a2 = -0.356563782;
    const T       a3 = 1.781477937;
    const T       a4 = -1.821255978;
    const T       a5 = 1.330274429;
    const T rsqrt2pi = 0.39894228040143267793994605993438;

    T k         = 1.0 / (1.0 + 0.2316419 * fabs(d));

    T cnd       = rsqrt2pi * exp(- 0.5 * d * d)
                * (k * (a1 + k * (a2 + k * (a3 + k * (a4 + k * a5)))));

    if (d > 0)
        cnd = 1.0 - cnd;

    return cnd;
}


/*
 * Black-Scholes formula for both call and put
 */
template <typename T>
static void BlackScholesBody
(
    T &callResult,
    T &putResult,
    T sf,       // Stock price
    T xf,       // Option strike
    T tf,       // Option years
    T rf,       // Riskless rate
    T vf        // Volatility rate
)
{
    T s = sf, x = xf, t = tf, r = rf, v = vf;

    T sqrtT = sqrt(t);
    T    d1 = (log(s / x) + (r + 0.5 * v * v) * t) / (v * sqrtT);
    T    d2 = d1 - v * sqrtT;
    T CNDD1 = CND(d1);
    T CNDD2 = CND(d2);

    //Calculate Call and Put simultaneously
    T expRT     = exp(- r * t);
    callResult  = (s * CNDD1 - x * expRT * CNDD2);
    putResult   = (x * expRT * (1.0 - CNDD2) - s * (1.0 - CNDD1));
}


/*
 * Process an array of optN options
 */
extern "C"
void BlackScholes_f
(
    float *CallResult,
    float *PutResult,
    float *StockPrice,
    float *OptionStrike,
    float *OptionYears,
    float Riskfree,
    float Volatility,
    int optN
)
{
    for (int opt = 0; opt < optN; opt++) {
        BlackScholesBody<float>
        (
            CallResult[opt],
            PutResult[opt],
            StockPrice[opt],
            OptionStrike[opt],
            OptionYears[opt],
            Riskfree,
            Volatility
        );
    }
}

extern "C"
void BlackScholes_d
(
    double *CallResult,
    double *PutResult,
    double *StockPrice,
    double *OptionStrike,
    double *OptionYears,
    double Riskfree,
    double Volatility,
    int optN
)
{
    for (int opt = 0; opt < optN; opt++) {
        BlackScholesBody<double>
        (
            CallResult[opt],
            PutResult[opt],
            StockPrice[opt],
            OptionStrike[opt],
            OptionYears[opt],
            Riskfree,
            Volatility
        );
    }
}

