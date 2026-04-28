# S3 method to send notifications via Pushbullet

S3 method to send notifications via Pushbullet

## Usage

``` r
# S3 method for class 'PushbulletNotifier'
notify(notifier, event, event_data)
```

## Arguments

- notifier:

  A TelegramNotifier object created with new_TelegramNotifier()

- event:

  Character string indicating the notification trigger ("condition" or
  "complete")

- event_data:

  List containing context information for the notification

## Value

Invisibly returns NULL
