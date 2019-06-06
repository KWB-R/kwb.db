# renamesToQueries -------------------------------------------------------------

#' List of Renamings to SQL Queries
#' 
#' Convert a list of renamings to a list of SQL queries each of which can be
#' used to select and rename the given fields from a database. 
#' 
#' @param renamesList list of renaming lists. A renaming list is a list of 
#'   \code{key = value} pairs defining renamings from the keys to the values.
#' 
#' @return list of character each of which represents an SQL query
#' 
renamesToQueries <- function(renamesList)
{
  stopifnot(is.list(renamesList))
  
  lapply(seq_along(renamesList), function(i) {
    
    structure(alias = LETTERS[i], sqlForSelect(
      tablename = names(renamesList)[i],
      fields = renamesToFieldList(renamesList[[i]])
    ))
  })
}

# renamesToFieldList -----------------------------------------------------------

#' List of Renamings to Field Selection String
#' 
#' Convert a list of renamings to a field selection string that can be used as
#' \code{fields} argument in a call to \code{\link[kwb.db]{hsGetTable}}
#' 
#' @param renames list of key = value pairs defining renamings from the keys to
#'   the values
#' @param source.in.brackets if \code{TRUE} (default) the source expressions
#'   will be enclosed in square brackets: \code{[source] AS <target>}
#' @param target.in.brackets if \code{TRUE} (default) the target names will be
#'   enclosed in square brackets: \code{<source> AS [target]}
#' @export
#' 
renamesToFieldList <- function(
  renames, source.in.brackets = TRUE, target.in.brackets = TRUE
)
{
  if (is.null(renames)) {
    
    "*"
  } else {
    
    sources <- names(renames)
    targets <- as.character(renames)
    
    parts <- sprintf(
      "%s AS %s", 
      if (source.in.brackets) paste0("[", sources, "]") else sources,
      if (target.in.brackets) paste0("[", targets, "]") else targets
    )
    
    paste(parts, collapse = ", ")
  }
}
