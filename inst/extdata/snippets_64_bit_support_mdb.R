file_accdb <- paste0(
  "C:/Users/hsonne/Documents/Projekte2/sema-berlin/DatenCOd_G_ds20180620.accdb"
)

file_mdb <- paste0(
  "C:/Users/hsonne/Documents/Projekte2/SEMA/1a_Environment_mdb/", 
  "StatAna_be_SEMA_versions/StatAna_be_SEMA_v09.mdb"
)

stopifnot(file.exists(file_accdb))
stopifnot(file.exists(file_mdb))

kwb.db::hsTables(file_accdb)
kwb.db::hsOpenDb(file_accdb)

con <- kwb.db:::openAdequateConnectionOrStop(file_accdb)
con <- kwb.db:::odbcConnectionAccess(file_accdb)
con <- kwb.db:::odbcConnectionAccess64(file_accdb, TRUE)
con <- kwb.db:::odbcConnectionAccess64(file_mdb, FALSE)

con_accdb <- odbc32::odbcConnectAccess2007(
  file_accdb, 
  socket = kwb.utils::defaultIfNULL(
    x = .GlobalEnv$.r2r_socket, 
    default = odbc32::start_server(invisible = TRUE)
  )
)

con_mdb <- odbc32:::odbcConnectAccess(
  file_mdb, 
  socket = kwb.utils::defaultIfNULL(
    x = .GlobalEnv$.r2r_socket, 
    default = odbc32::start_server(invisible = TRUE)
  )
)

odbc32::sqlTables(con_accdb)
odbc32::sqlTables(con_mdb)

kwb.db:::hsCloseDb(con_accdb)
kwb.db:::hsCloseDb(con_mdb)

key <- "SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers"

utils::readRegistry(key, view = "64-bit")
utils::readRegistry(key, view = "32-bit")
