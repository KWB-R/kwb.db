# hsSetForeignKey --------------------------------------------------------------

#' Set Foreign Key for Table Field
#' 
#' Set foreign key constraint for a table field
#' 
#' @param mdb full path to MS Access database file (*.mdb)
#' @param tbl name of table containing the field for which the foreign key
#'   constraint is to be defined
#' @param field name of table field for which the foreign key constraint is to
#'   be defined
#' @param ref.tbl name of table containing the referenced foreign key field
#' @param ref.field name of foreign key field in ref.tbl
#' @param key.name optional. Name to be given to the foreign key
#' @param dbg passed to \code{\link{hsSqlQuery}}
#' @export
#' 
hsSetForeignKey <- function(
  mdb, tbl, field, ref.tbl, ref.field, 
  key.name = .getForeignKeyName(tbl, field, ref.tbl, ref.field), dbg = FALSE
)
{
  hsSqlQuery(
    mdb = mdb,
    sql = .sqlForeignKey(tbl, key.name, field, ref.tbl, ref.field),
    dbg = dbg
  )
}

# .getForeignKeyName -----------------------------------------------------------
.getForeignKeyName <- function(tbl, field, ref.tbl, ref.field)
{
  paste("fk", tbl, field, ref.tbl, ref.field, sep = "_")
}

# .sqlForeignKey ---------------------------------------------------------------
.sqlForeignKey <- function(tbl, constraint.name, field, ref.tbl, ref.field)
{
  sql <- "ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s(%s)"
  
  sprintf(sql, tbl, constraint.name, field, ref.tbl, ref.field)
}
