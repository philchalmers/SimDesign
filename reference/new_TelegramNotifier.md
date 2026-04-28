# Create a Telegram Notifier

Constructs a notifier object for sending messages via Telegram. Requires
a valid Telegram bot token and chat ID.

## Usage

``` r
new_TelegramNotifier(bot_token, chat_id, verbose_issues = FALSE)
```

## Arguments

- bot_token:

  A character string representing your Telegram bot token, typically
  something like `"123456:ABC-xxxx"`.

- chat_id:

  A character string or numeric representing the chat/group to send
  messages to.

- verbose_issues:

  Logical. If TRUE, provides detailed information about warnings and
  errors in the notifications.

## Value

An S3 object of class `"TelegramNotifier"`.

## Details

To use send notifications over Telegram with `httr` in `SimDesign`,
install `httr`, set set up a Telegram bot, and obtain a bot token and
chat ID. For more information, see the [Telegram Bots
API](https://core.telegram.org/bots). Then use the
`new_TelegramNotifier()` function to create a `TelegramNotifier` object
and pass it to the `notifier` argument in
[`runSimulation()`](http://philchalmers.github.io/SimDesign/reference/runSimulation.md).

## Examples

``` r
if (FALSE) { # interactive()
# Create a Telegram notifier (requires setting up a Telegram Bot)
telegram_notifier <- new_TelegramNotifier(bot_token = "123456:ABC-xyz", chat_id = "987654321")
}
```
