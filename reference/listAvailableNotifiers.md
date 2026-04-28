# List All Available Notifiers

Automatically detects all S3 classes that have a specialized
[`notify()`](http://philchalmers.github.io/SimDesign/reference/notify.md)
method (like `notify.MyNotifier`) and prints them as a character vector
of class names (e.g., `"PushbulletNotifier"`, `"TelegramNotifier"`).

Note that only classes defined and loaded at the time you call this
function will appear. If you just created a new notifier in another file
or package, ensure it's sourced/loaded first.

## Usage

``` r
listAvailableNotifiers()
```

## Value

A character vector of class names that have `notify.<ClassName>`
methods.

## Examples

``` r
if (FALSE) { # \dontrun{
listAvailableNotifiers()
# [1] "PushbulletNotifier" "TelegramNotifier"
} # }
```
