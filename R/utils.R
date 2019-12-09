# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(call. = FALSE, ...)
}

# get_odbc_function ------------------------------------------------------------
get_odbc_function <- function(function_name)
{
  package <- if (is64BitR()) "odbc32" else "RODBC"
  
  getExportedValue(package, function_name)
}

# .message_availableTables -----------------------------------------------------
.message_availableTables <- function(tableNames)
{
  paste("Available tables:\n ", paste(tableNames, collapse = "\n  "))
}
