capture_calls <- function(e) {
  e$calls <- sys.calls()
  signalCondition(e)
}

fn <- function(x) {
  call_another_function(x + 1)
}

call_another_function <- function(y) {
  call_yet_other_function(y + 2)
}

call_yet_other_function <- function(z) {
  tryCatch(
    withCallingHandlers(stop("Omg"), error = capture_calls),
    error = identity
  )
}

fn(1)
