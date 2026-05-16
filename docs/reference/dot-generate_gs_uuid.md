# Generate a unique identifier for GraphSpace objects

This helper function creates a unique ID without relying on the R Random
Number Generator (RNG), making it immune to
[`set.seed()`](https://rdrr.io/r/base/Random.html). It combines the
Process ID (PID), high-precision system time, and a system-level
temporary identifier to ensure uniqueness across parallel processes and
rapid sequential calls.

## Usage

``` r
.generate_gs_uuid()
```

## Value

A character string containing a unique alphanumeric ID.
