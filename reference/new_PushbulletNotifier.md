# Create a Pushbullet Notifier

Constructs a notifier object for sending messages via Pushbullet. This
requires a Pushbullet account, the Pushbullet application installed on
both a mobile device and computer, and a properly configured JSON file
(typically `~/.rpushbullet.json`, using
[`RPushbullet::pbSetup()`](https://rdrr.io/pkg/RPushbullet/man/pbSetup.html)).

## Usage

``` r
new_PushbulletNotifier(
  config_path = "~/.rpushbullet.json",
  verbose_issues = FALSE
)
```

## Arguments

- config_path:

  A character string specifying the path to the Pushbullet configuration
  file. Defaults to `"~/.rpushbullet.json"`.

- verbose_issues:

  Logical. If `TRUE`, includes detailed information about warnings and
  errors in notifications. Default is `FALSE`.

## Value

An S3 object of class `"PushbulletNotifier"` and `"Notifier"`.

## Details

To use `RPushbullet` in `SimDesign`, create a `PushbulletNotifier`
object using `new_PushbulletNotifier()` and pass it to the `notifier`
argument in
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md).

## Examples

``` r
if (FALSE) { # interactive()
# Create a Pushbullet notifier (requires a valid configuration file)
pushbullet_notifier <- new_PushbulletNotifier(verbose_issues = TRUE)
}
```
