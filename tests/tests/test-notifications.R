library(testthat)
library(mockery)

timeFormater_internal <- function(time) {
  return(sprintf("%02d:%02d:%02d", time %/% 3600, (time %% 3600) %/% 60, time %% 60))
}

test_that("generate_condition_notification generates correct condition notification", {
    fake_result <- dplyr::tibble(
        ID = 1,
        N = 100,
        bias = 0.1,
        REPLICATIONS = 100,
        SIM_TIME = 20.123,
        COMPLETED = "Tue Feb 25 19:36:05 2025",
        "ERROR: Test" = 6,
        "ERROR: Test 2" = 3,
        "WARNING: Test" = 5,
        "WARNING: Test 2" = 2
    )
    fake_event_data <- list(
        result = fake_result,
        condition = list(ID = 1),
        total = 3
    )

    dummy_notifier <- list(verbose_issues = TRUE)
    dummy_notifier$class <- "DummyNotifier"  # dummy class assignment

    notification <- generate_condition_notification(dummy_notifier, fake_event_data)

    expect_match(notification$title, "Condition 1/3 completed")
    expect_match(notification$body, "Execution time:")
    expect_match(notification$body, "Errors: 9")
    expect_match(notification$body, "Warnings: 7")
    expect_true(!is.null(notification$issue_details))
})

test_that("generate_complete_notification generates correct complete notification", {
    fake_final <- data.frame(
        SIM_TIME = c(3, 7),
        ERRORS = c(0, 1),
        WARNINGS = c(1, 0),
        stringsAsFactors = FALSE
    )

    attr(fake_final, "ERROR_msg") <- matrix(c(0,1), ncol=1, dimnames = list(NULL, "ERROR:Final"))
    attr(fake_final, "WARNING_msg") <- matrix(c(1,0), ncol=1, dimnames = list(NULL, "WARNING:Final"))

    event_data <- list(
        final = fake_final
    )

    dummy_notifier <- list(verbose_issues = FALSE)
    dummy_notifier$class <- c("DummyNotifier", "Notifier")

    notification <- generate_complete_notification(dummy_notifier, event_data)

    expect_equal(notification$title, "Simulation completed")
    expect_match(notification$body, "Execution time:")
    expect_match(notification$body, "Errors: 1")
    expect_match(notification$body, "Warnings: 1")
})

test_that("generate_notification delegates to correct function", {
    fake_result <- dplyr::tibble(
        ID = 1,
        N = 100,
        bias = 0.1,
        REPLICATIONS = 100,
        SIM_TIME = 20.123,
        COMPLETED = "Tue Feb 25 19:36:05 2025",
        "ERROR: Test" = 6,
        "ERROR: Test 2" = 3,
        "WARNING: Test" = 5,
        "WARNING: Test 2" = 2
    )
    fake_event_data <- list(
        result = fake_result,
        condition = list(ID = 1),
        total = 3
    )

    dummy_notifier <- list(verbose_issues = TRUE)
    dummy_notifier$class <- c("DummyNotifier", "Notifier")

    notification <- generate_notification(dummy_notifier, "condition", fake_event_data)

    expect_match(notification$title, "Condition 1/3 completed")
    expect_match(notification$body, "Execution time:")
    expect_match(notification$body, "Errors: 9")
    expect_match(notification$body, "Warnings: 7")
    expect_true(!is.null(notification$issue_details))

    fake_final <- data.frame(
        SIM_TIME = c(3, 7),
        ERRORS = c(0, 1),
        WARNINGS = c(1, 0),
        stringsAsFactors = FALSE
    )
    attr(fake_final, "ERROR_msg") <- matrix(c(0,1), ncol=1, dimnames = list(NULL, "ERROR:Final"))
    attr(fake_final, "WARNING_msg") <- matrix(c(1,0), ncol=1, dimnames = list(NULL, "WARNING:Final"))

    event_data <- list(
        final = fake_final
    )

    dummy_notifier <- list(verbose_issues = FALSE)
    dummy_notifier$class <- c("DummyNotifier", "Notifier")

    notification <- generate_notification(dummy_notifier, "complete", event_data)

    expect_equal(notification$title, "Simulation completed")
    expect_match(notification$body, "Execution time:")
    expect_match(notification$body, "Errors: 1")
    expect_match(notification$body, "Warnings: 1")
})

test_that("notify_single_or_list calls notify for a single notifier", {
    mock_notify <- mock()
    stub(notify_single_or_list, "notify.DummyNotifier", mock_notify)

    dummy_notifier <- list(verbose_issues = FALSE)
    class(dummy_notifier) <- c("DummyNotifier", "Notifier")

    notify_single_or_list(dummy_notifier, "condition", list(result = data.frame(SIM_TIME = 1)))
    expect_called(mock_notify, 1)
})

test_that("notify_single_or_list calls notify for a list of notifiers", {
    mock_notify <- mock()
    stub(notify_single_or_list, "notify.DummyNotifier", mock_notify)

    dummy_notifier1 <- list(verbose_issues = FALSE)
    dummy_notifier2 <- list(verbose_issues = FALSE)
    class(dummy_notifier1) <- c("DummyNotifier", "Notifier")
    class(dummy_notifier2) <- c("DummyNotifier", "Notifier")

    notify_single_or_list(
      list(dummy_notifier1, dummy_notifier2),
      "complete",
      list(final = data.frame(SIM_TIME = 2))
    )
    expect_called(mock_notify, 2)
})

test_that("notify.PushbulletNotifier calls RPushbullet pbPost", {
    mock_pbpost <- mock()
    stub(notify.PushbulletNotifier, "get", function(name, ns) {
        if (name == "pbPost") mock_pbpost else stop("unexpected get call")
    })

    dummy_notifier <- list(verbose_issues = FALSE, config_path = "dummy_path")
    class(dummy_notifier) <- c("PushbulletNotifier", "Notifier")

    fake_result <- data.frame(SIM_TIME = 5)
    fake_event_data <- list(
        result = fake_result,
        condition = list(ID = 2),
        total = 5
    )

    notify.PushbulletNotifier(dummy_notifier, "condition", fake_event_data)
    expect_called(mock_pbpost, 1)

    call_args <- mock_args(mock_pbpost)[[1]]
    expect_equal(call_args$type, "note")
    expect_match(call_args$title, "Condition")
    expect_match(call_args$body, "Execution time:")
})

test_that("notify.TelegramNotifier calls httr::POST", {
    mock_POST <- mock(return_value = list(status_code = 200))
    stub(notify.TelegramNotifier, "get", function(name, ns) {
        if (name == "POST") mock_POST else stop("unexpected get call")
    })

    dummy_notifier <- list(bot_token = "dummy_token", chat_id = "dummy_chat_id", verbose_issues = FALSE)
    class(dummy_notifier) <- c("TelegramNotifier", "Notifier")

    fake_result <- data.frame(SIM_TIME = 6)
    fake_event_data <- list(
        result = fake_result,
        condition = list(ID = 3),
        total = 10
    )

    notify.TelegramNotifier(dummy_notifier, "condition", fake_event_data)
    expect_called(mock_POST, 1)

    call_args <- mock_args(mock_POST)[[1]]
    expect_match(call_args$url, "https://api.telegram.org/botdummy_token/sendMessage")
    expect_equal(call_args$encode, "form")
    expect_true("chat_id" %in% names(call_args$body))
    expect_true("text" %in% names(call_args$body))
})

test_that("listAvailableNotifiers returns PushbulletNotifier and TelegramNotifier", {
    notifiers <- listAvailableNotifiers()
    expect_true("PushbulletNotifier" %in% notifiers)
    expect_true("TelegramNotifier" %in% notifiers)
})