#' Send a simulation notification
#'
#' Package extensions can implement custom notifiers by creating S3 methods
#' for this generic.
#'
#' @param notifier The notifier object
#' @param event Character string indicating the notification trigger ("condition" or "complete")
#' @param event_data List containing context information for the notification
#'
#' @keywords internal
notify <- function(notifier, event, event_data) {
    UseMethod("notify")
}

notify_single_or_list <- function(notifier, event, event_data) {
    if (inherits(notifier, "Notifier")) {
        notify(notifier, event, event_data)
    } else if (is.list(notifier)) {
        lapply(notifier, function(n) notify(n, event, event_data))
    }
}

generate_notification <- function(notifier, event, event_data) {
    if (event == "condition") {
        return(generate_condition_notification(notifier, event_data))
    } else if (event == "complete") {
        return(generate_complete_notification(notifier, event_data))
    }
}

generate_condition_notification <- function(notifier, event_data) {
    issue_details <- NULL
    total_time <- timeFormater_internal(event_data$result$SIM_TIME)
    error_cols <- grep("^ERROR:", names(event_data$result), value = TRUE)
    warning_cols <- grep("^WARNING:", names(event_data$result), value = TRUE)
    error_warning_cols <- c(error_cols, warning_cols)

    total_errors <- if (length(error_cols) > 0) {
        sum(as.matrix(event_data$result[, error_cols, drop = FALSE]), na.rm = TRUE)
    } else {
        0
    }

    total_warnings <- if (length(warning_cols) > 0) {
        sum(as.matrix(event_data$result[, warning_cols, drop = FALSE]), na.rm = TRUE)
    } else {
        0
    }

    notification_title <- sprintf("Condition %i/%i completed", event_data$condition$ID, event_data$total)
    notification_body <- sprintf(
        "Execution time: %s\nErrors: %i\nWarnings: %i",
        total_time,
        total_errors,
        total_warnings
    )

    if (notifier$verbose_issues && length(error_warning_cols) > 0) {
        issue_counts <- colSums(
            as.data.frame(
                lapply(event_data$result[, error_warning_cols, drop = FALSE], as.numeric),
                check.names = FALSE
            ),
            na.rm = TRUE
        )
        issue_details <- paste(names(issue_counts), issue_counts, sep = " = ", collapse = "\n")
    }

    return(list(
        title = notification_title,
        body = notification_body,
        issue_details = issue_details
    ))
}

generate_complete_notification <- function(notifier, event_data) {
    issue_details <- NULL
    total_time <- timeFormater_internal(sum(event_data$final$SIM_TIME))

    total_errors <- if ("ERRORS" %in% names(event_data$final)) {
        sum(event_data$final$ERRORS, na.rm = TRUE)
    } else {
        0
    }

    total_warnings <- if ("WARNINGS" %in% names(event_data$final)) {
        sum(event_data$final$WARNINGS, na.rm = TRUE)
    } else {
        0
    }

    notification_title <- "Simulation completed"
    notification_body <- sprintf(
        "Execution time: %s\nErrors: %i\nWarnings: %i",
        total_time,
        total_errors,
        total_warnings
    )

    if (notifier$verbose_issues) {
        error_msg <- attr(event_data$final, "ERROR_msg")
        warning_msg <- attr(event_data$final, "WARNING_msg")

        error_string <- NULL
        warning_string <- NULL

        if (!is.null(error_msg)) {
            error_counts <- colSums(error_msg, na.rm = TRUE)
            error_string <- paste(sprintf("%s = %d", names(error_counts), error_counts), collapse = "\n")
        }
        if (!is.null(warning_msg)) {
            warning_counts <- colSums(warning_msg, na.rm = TRUE)
            warning_string <- paste(sprintf("%s = %d", names(warning_counts), warning_counts), collapse = "\n")
        }

        issue_details <- paste(
            c(Details:\n\n rerror_string, warning_string),
            collapse = "\n"
        )
    }

    return(list(
        title = notification_title,
        body = notification_body,
        issue_details = issue_details
    ))
}

#' Create a Pushbullet Notifier
#'
#' Constructs a notifier object for sending messages via Pushbullet. This requires a
#' Pushbullet account, the Pushbullet application installed on both a mobile device
#' and computer, and a properly configured JSON file (typically \code{~/.rpushbullet.json},
#' using \code{RPushbullet::pbSetup()}).
#'
#' To use \code{RPushbullet} in \code{SimDesign}, create a \code{PushbulletNotifier}
#' object using \code{new_PushbulletNotifier()} and pass it to the \code{notifier}
#' argument in \code{runSimulation()}.
#'
#' @param config_path A character string specifying the path to the Pushbullet configuration file.
#'   Defaults to \code{"~/.rpushbullet.json"}.
#' @param verbose_issues Logical. If \code{TRUE}, includes detailed information about warnings
#'   and errors in notifications. Default is \code{FALSE}.
#'
#' @return An S3 object of class \code{"PushbulletNotifier"} and \code{"Notifier"}.
#'
#' @examplesIf interactive()
#' # Create a Pushbullet notifier (requires a valid configuration file)
#' pushbullet_notifier <- new_PushbulletNotifier(verbose_issues = TRUE)
#'
#' @export
new_PushbulletNotifier <- function(config_path = "~/.rpushbullet.json", verbose_issues = FALSE) {

    if(!requireNamespace("RPushbullet", quietly = TRUE)) {
        stop("RPushbullet has to be installed to send notifications via Pushbullet.")
    }

    expanded_config_path <- path.expand(config_path)
    if (!file.exists(expanded_config_path)) {
        stop(sprintf(
            "Pushbullet configuration file not found at %s. Ensure you have configured RPushbullet correctly.",
            expanded_config_path
        ))
    }
    structure(
        list(
            config_path = expanded_config_path,
            verbose_issues = verbose_issues),
        class = c("PushbulletNotifier", "Notifier")
    )
}

#' S3 method to send notifications via Pushbullet
#'
#' @param notifier A TelegramNotifier object created with new_TelegramNotifier()
#' @param event Character string indicating the notification trigger ("condition" or "complete")
#' @param event_data List containing context information for the notification
#'
#' @return Invisibly returns NULL
#' @exportS3Method
#' @keywords internal
notify.PushbulletNotifier <- function(notifier, event, event_data) {
    notification <- generate_notification(notifier, event, event_data)
    rpushbullet_pbpost <- get("pbPost", asNamespace("RPushbullet"))
    rpushbullet_pbpost(
        type = 'note',
        title = notification$title,
        body = notification$body
    )
    return(invisible(NULL))
}

#' Create a Telegram Notifier
#'
#' Constructs a notifier object for sending messages via Telegram.
#' Requires a valid Telegram bot token and chat ID.
#'
#' To use send notifications over Telegram with \code{httr} in \code{SimDesign},
#' install \code{httr}, set set up a Telegram bot, and obtain a bot token and chat ID.
#' For more information, see the \href{https://core.telegram.org/bots}{Telegram Bots API}.
#' Then use the \code{new_TelegramNotifier()} function to create a \code{TelegramNotifier}
#' object and pass it to the \code{notifier} argument in \code{runSimulation()}.
#'
#' @param bot_token A character string representing your Telegram bot token, typically
#'   something like \code{"123456:ABC-xxxx"}.
#' @param chat_id A character string or numeric representing the chat/group to send
#'   messages to.
#' @param verbose_issues Logical. If TRUE, provides detailed information about warnings and errors in the notifications.
#'
#' @return An S3 object of class \code{"TelegramNotifier"}.
#'
#' @examplesIf interactive()
#' # Create a Telegram notifier (requires setting up a Telegram Bot)
#' telegram_notifier <- new_TelegramNotifier(bot_token = "123456:ABC-xyz", chat_id = "987654321")
#'
#' @export
new_TelegramNotifier <- function(bot_token, chat_id, verbose_issues = FALSE) {

    if(!requireNamespace("httr", quietly = TRUE)) {
        stop("httr has to be installed to send notifications via Telegram.")
    }

    structure(
        list(
            bot_token = bot_token,
            chat_id = chat_id,
            verbose_issues = verbose_issues
        ),
        class = c("TelegramNotifier", "Notifier")
    )
}

#' S3 method to send notifications through the Telegram API.
#'
#' @param notifier A TelegramNotifier object created with new_TelegramNotifier()
#' @param event Character string indicating the notification trigger ("condition" or "complete")
#' @param event_data List containing context information for the notification
#'
#' @return Invisibly returns NULL
#' @exportS3Method
#' @keywords internal
notify.TelegramNotifier <- function(notifier, event, event_data) {

    bot_url <- sprintf("https://api.telegram.org/bot%s/sendMessage", notifier$bot_token)
    notification <- generate_notification(notifier, event, event_data)

    formatted_notification <- paste0(
        sprintf("*%s*\n%s", notification$title, notification$body),
        if (!is.null(notification$issue_details)) sprintf("\n\n*Details:* \n_%s_", notification$issue_details) else ""
    )

    httr_post <- get("POST", asNamespace("httr"))
    httr_post(
        url = bot_url,
        body = list(
            chat_id = notifier$chat_id,
            text = formatted_notification,
            parse_mode = "Markdown"
        ),
        encode = "form"
    )
    return(invisible(NULL))
}

#' @title List All Available Notifiers
#' @description
#'   Automatically detects all S3 classes that have a specialized \code{notify()} method
#'   (like \code{notify.MyNotifier}) and prints them as a character vector of class names
#'   (e.g., \code{"PushbulletNotifier"}, \code{"TelegramNotifier"}).
#'
#'   Note that only classes defined and loaded at the time you call this function will
#'   appear. If you just created a new notifier in another file or package, ensure it's
#'   sourced/loaded first.
#'
#' @return A character vector of class names that have \code{notify.<ClassName>} methods.
#' @examples
#' \dontrun{
#' listAvailableNotifiers()
#' # [1] "PushbulletNotifier" "TelegramNotifier"
#' }
#' @importFrom utils methods
#' @export
listAvailableNotifiers <- function() {
    s3_methods <- methods("notify")
    classes <- sub("^notify\\.", "", s3_methods)
    classes <- classes[classes != "Notifier"]
    if (interactive()) {
        message("To use a notifier, create an instance with new_<NotifierName>() and pass it to runSimulation().")
        message("For custom notifiers, see R/notifications.R.")
    }
    return(classes)
}
