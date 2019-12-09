# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(call. = FALSE, ...)
}

# .message_availableTables -----------------------------------------------------
.message_availableTables <- function(tableNames)
{
  paste("Available tables:\n ", paste(tableNames, collapse = "\n  "))
}
