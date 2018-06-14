# renamesToFieldList -----------------------------------------------------------

#' List of Renamings to Field Selection String
#' 
#' Convert a list of renamings to a field selection string that can be used as
#' \code{fields} argument in a call to \code{kwb.db::hsGetTable}
#' 
#' @param renames list of key = value pairs defining renamings from the keys to
#'   the values
#' @param source.in.brackets if \code{TRUE} (default) the source expressions
#'   will be enclosed in square brackets: \code{[source] AS <target>}
#' @param target.in.brackets if \code{TRUE} (default) the target names will be
#'   enclosed in square brackets: \code{<source> AS [target]}
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
