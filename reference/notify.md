# Send a simulation notification

Package extensions can implement custom notifiers by creating S3 methods
for this generic.

## Usage

``` r
notify(notifier, event, event_data)
```

## Arguments

- notifier:

  The notifier object

- event:

  Character string indicating the notification trigger ("condition" or
  "complete")

- event_data:

  List containing context information for the notification
