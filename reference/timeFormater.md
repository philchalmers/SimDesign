# Format time string to suitable numeric output

Format time input string into suitable numeric output metric (e.g.,
seconds). Input follows the `SBATCH` utility specifications. Accepted
time formats include `"minutes"`, `"minutes:seconds"`,
`"hours:minutes:seconds"`, `"days-hours"`, `"days-hours:minutes"` and
`"days-hours:minutes:seconds"`. Alternatively, function can be used to
convert numeric input to SBATCH format.

## Usage

``` r
timeFormater(time, output = "sec", input = "min", sround = floor)
```

## Arguments

- time:

  a character string to be formatted. If a numeric vector is supplied
  then this will be interpreted as minutes due to character coercion.

- output:

  type of numeric output to convert time into. Currently supported are
  `'sec'` for seconds (default), `'min'` for minutes, `'hour'`, and
  `'day'`.

  Alternatively, if `time` were numeric then setting `output` to
  `'SBATCH'` will return a suitable SBATCH format.

- input:

  if supplied `time` is a numeric, indicates what the value represents.
  Default assumes the input is in minutes (see `output` for supported
  values)

- sround:

  function used to round last seconds computation

## Details

For example, `time = "60"` indicates a maximum time of 60 minutes,
`time = "03:00:00"` a maximum time of 3 hours, `time = "4-12"` a maximum
of 4 days and 12 hours, and `time = "2-02:30:00"` a maximum of 2 days, 2
hours and 30 minutes.

## Examples

``` r
# Test cases (outputs in seconds)
timeFormater("4-12")        # day-hours
#> [1] 388800
timeFormater("4-12:15")     # day-hours:minutes
#> [1] 389700
timeFormater("4-12:15:30")  # day-hours:minutes:seconds
#> [1] 389730

timeFormater("30")          # minutes
#> [1] 1800
timeFormater("30:30")       # minutes:seconds
#> [1] 1830
timeFormater("4:30:30")     # hours:minutes:seconds
#> [1] 16230

# output in hours
timeFormater("4-12", output = 'hour')
#> [1] 108
timeFormater("4-12:15", output = 'hour')
#> [1] 108.25
timeFormater("4-12:15:30", output = 'hour')
#> [1] 108.2583

timeFormater("30", output = 'hour')
#> [1] 0.5
timeFormater("30:30", output = 'hour')
#> [1] 0.5083333
timeFormater("4:30:30", output = 'hour')
#> [1] 4.508333

# numeric input is understood as minutes
timeFormater(42)               # seconds
#> [1] 2520
timeFormater(42, output='min') # minutes
#> [1] 42

# convert numeric inputs to SBATCH format
timeFormater(60, output='SBATCH')
#> [1] "1:0:0"
timeFormater(3, output='SBATCH', input='day')
#> [1] "3-0:0:0"
timeFormater(7000, output='SBATCH', input='sec')
#> [1] "1:56:40"
timeFormater(100000, output='SBATCH', input='sec')
#> [1] "1-3:46:40"

# rounding seconds
timeFormater(1.55555, output='SBATCH', input='sec') # floor default
#> [1] "0:0:1"
timeFormater(1.55555, output='SBATCH', input='sec', sround=ceiling)
#> [1] "0:0:2"
timeFormater(1.55555, output='SBATCH', input='sec', sround=\(x) round(x, 3))
#> [1] "0:0:1.556"

```
