

# kwb.db::hsTables
# function (mdb, excludeSystemTables = grepl("\\.(mdb|accdb)$", 
#                                            mdb), namesOnly = TRUE, use2007Driver = NULL, dbg = FALSE) 
# {

  dbg <- TRUE
  use2007Driver <- NULL
  mdb <- db_path
  
  kwb.utils::catIf(dbg, "in hsTables: use2007Driver =", use2007Driver, 
                   "\n")
  sqlDialect = kwb.db::getCurrentSqlDialect(warn = FALSE)
  
  
  #con <- kwb.db::hsOpenDb(mdb, use2007Driver = use2007Driver)
  con <- odbc32::odbcConnectAccess2007(db_path)
  
  on.exit({
    #kwb.db::hsCloseDb(con)
    odbc32::odbcClose(con)
    setCurrentSqlDialect(sqlDialect)
  })
  
  is_64_bit <- (.Machine$sizeof.pointer == 8)
  
  tblList <- RODBC::sqlTables(con)
  tblList <- odbc32::sqlTables(con)
  
  if (excludeSystemTables) 
    tblList <- tblList[tblList$TABLE_TYPE != "SYSTEM TABLE", 
                       ]
  if (namesOnly) {
    return(tblList$TABLE_NAME)
  }
  else {
    return(tblList)
  }
#}