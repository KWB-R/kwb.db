if (FALSE)
{
  if (FALSE)
  {
    cat(createRInterfaceToDatabase(dbSchema_ODM()))
  }
  
  createRInterfaceToDatabase <- function(dbSchema)
  {
    RCode <- pasteWithNewLine(
      c(codeForFunctionsNew(dbSchema),
        codeForFunctionsGet(dbSchema)))
    
    RScriptPath <- tempfile(fileext = ".R")
    writeLines(RCode, RScriptPath)
    
    source(RScriptPath)
    RCode
  }
  
  codeForFunctionsNew <- function(dbSchema)
  { 
    codeLines <- c()
    
    for (tableName in names(dbSchema$tables)) {
      cat("Creating new-functions for table", tableName, "...\n")
      codeLines <- c(codeLines, codeForFunctionNew(dbSchema, tableName))
    }
    
    pasteWithNewLine(codeLines)
  }
  
  codeForFunctionNew <- function(dbSchema, tableName)
  {
    sprintf("odmNew_%s <- function\n(\n%s\n)\n{\n%s\n}\n", 
            tableName, 
            pasteWithNewLine(indent(argumentListNew(dbSchema$tables[[tableName]]), 2)),
            indent(functionBodyNew(), 2))
  }
  
  pasteWithNewLine <- function(x)
  {
    paste(x, collapse = "\n")
  }
  
  indent <- function(text, nchar)
  {
    FUN <- function(x) { 
      paste(c(rep(" ", nchar), x), collapse = "") 
    }
    
    sapply(text, FUN, USE.NAMES=FALSE)
  }
  
  argumentListNew <- function(tableSchema)
  {
    parameterLines <- sprintf("%s = NULL,\n  ### %s", 
                              names(tableSchema$fields),
                              sapply(tableSchema$fields, "[[", "Description"))
    
    c(parameterLines, "db = currentDb()")
  }
  
  functionBodyNew <- function()
  {
    "# function body..."
  }
  
  codeForFunctionsGet <- function(dbSchema)
  {
    "# Get-functions..."
  }  
}
