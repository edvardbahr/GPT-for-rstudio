# Conversation state
.init_explain_history <- function() {
  list(list(role = "system",
            content = "You are a clear, pragmatic R assistant. Be concise but helpful."))
}

#' Explain/Chat about the selection; continue in Console; ENTER to reset
#' If called with NO selection, it resets & exits immediately.
#' @export
explain_code <- function(model = getOption("gptstudio.model", "gpt-4.1"),
                         temperature = getOption("gptstudio.temperature", 0.2),
                         max_tokens = getOption("gptstudio.max_tokens", 600)) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) stop("install.packages('rstudioapi')")
  if (!requireNamespace("httr2", quietly = TRUE))     stop("install.packages('httr2')")

  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) stop("Missing OPENAI_API_KEY in ~/.Renviron (no quotes). Restart R after setting it.")

  # Ensure history exists in the session
  if (!exists(".explain_code_history", envir = .GlobalEnv)) {
    assign(".explain_code_history", .init_explain_history(), envir = .GlobalEnv)
  }
  history <- get(".explain_code_history", envir = .GlobalEnv)

  # helper to send request using current history and append reply
  .send <- function() {
    req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(Authorization = paste("Bearer", key),
                         "Content-Type" = "application/json") |>
      httr2::req_body_json(list(
        model = model,
        temperature = temperature,
        max_tokens = max_tokens,
        messages = history
      ))
    resp <- httr2::req_perform(req)
    out  <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (is.null(out$choices) || !length(out$choices)) stop("No response from API.")
    reply <- out$choices[[1]]$message$content
    history <<- append(history, list(list(role = "assistant", content = reply)))
    assign(".explain_code_history", history, envir = .GlobalEnv)
    reply
  }

  # Selection handling
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]$text

  # If NO selection: use the shortcut as a "reset & exit"
  if (!nzchar(sel)) {
    assign(".explain_code_history", .init_explain_history(), envir = .GlobalEnv)
    cat("Conversation reset.\n")
    return(invisible(NULL))
  }

  # If there IS a selection: send it and show immediate reply
  bar  <- paste(rep("=", 72), collapse = "")
  dash <- paste(rep("-", 72), collapse = "")
  cat("\n", bar, "\nMarked text:\n", sep = "")
  cat(sel, "\n", sep = "")

  history <- append(history, list(list(role = "user", content = sel)))
  assign(".explain_code_history", history, envir = .GlobalEnv)
  reply <- .send()

  cat("\n", dash, "\nGPT response:\n", sep = "")
  cat(reply, "\n", sep = "")
  cat(bar, "\n", sep = "")

  # Console continuation loop ??? ENTER to reset & exit
  repeat {
    msg <- readline("Continue (press ENTER to reset & exit): ")
    if (!nzchar(trimws(msg))) {
      assign(".explain_code_history", .init_explain_history(), envir = .GlobalEnv)
      cat("Conversation reset. Bye!\n")
      break
    }
    history <- get(".explain_code_history", envir = .GlobalEnv)
    history <- append(history, list(list(role = "user", content = msg)))
    assign(".explain_code_history", history, envir = .GlobalEnv)
    reply <- .send()
    cat("\n--- GPT ---\n", reply, "\n", sep = "")
  }

  invisible(NULL)
}
