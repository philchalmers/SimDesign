# Set RNG sub-stream for Pierre L'Ecuyer's RngStreams

Sets the sub-stream RNG state within for Pierre L'Ecuyer's (1999)
algorithm. Should be used within distributed array jobs after suitable
L'Ecuyer's (1999) have been distributed to each array, and each array is
further defined to use multi-core processing. See
[`clusterSetRNGStream`](https://rdrr.io/r/parallel/RngStream.html) for
further information.

## Usage

``` r
clusterSetRNGSubStream(cl, seed)
```

## Arguments

- cl:

  A cluster from the `parallel` package, or (if `NULL`) the registered
  cluster

- seed:

  An integer vector of length 7 as given by `.Random.seed` when the
  L'Ecuyer-CMR RNG is in use.
  See[`RNG`](https://rdrr.io/r/base/Random.html) for the valid values

## Value

invisible NULL
