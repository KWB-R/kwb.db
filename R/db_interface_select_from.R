# argumentsToSqlParts ----------------------------------------------------------

argumentsToSqlParts <- function(
  select, selects, where, wheres, orderBy, orderBys
)
{
  list(
    select = .argumentsToSqlExpressions(
      mainArgument = select,
      arguments = selects,
      argumentPrefix = "select",
      FUN = .argumentsToFieldNames,
      default = "*",
      collapse = ", "
    ),

    where = .argumentsToSqlExpressions(
      mainArgument = where,
      arguments = wheres,
      argumentPrefix = "where",
      FUN = .argumentsToConditions,
      default = "TRUE",
      collapse = " AND "
    ),

    orderBy = .argumentsToSqlExpressions(
      mainArgument = orderBy,
      arguments = orderBys,
      argumentPrefix = "orderBy",
      FUN = .argumentsToFieldNames,
      default = "",
      collapse = ", "
    )
  )
}

# .argumentsToSqlExpressions ---------------------------------------------------

.argumentsToSqlExpressions <- function
(
  mainArgument, arguments, argumentPrefix, FUN, default, collapse
)
{
  expressions <- FUN(mainArgument, arguments, argumentPrefix)

  if (length(expressions) == 0) {
    
    default
    
  } else {
    
    paste(expressions, collapse = collapse)
  }
}

# .argumentsToFieldNames -------------------------------------------------------

#' @importFrom kwb.utils isNullOrEmpty hsTrim
#' 
.argumentsToFieldNames <- function(mainArgument, arguments, argumentPrefix)
{
  #arguments <- list(...)

  fieldNames <- .stripPrefix(names(arguments), argumentPrefix)

  if (is.null(mainArgument)) {

    argumentValues <- unlist(arguments)

    stopifnot(!is.null(argumentValues))

    # non-NA arguments > 0, ordered by their value
    indices <- which(!is.na(argumentValues) & argumentValues > 0)

    if (! kwb.utils::isNullOrEmpty(indices)) {
      
      indices <- indices[order(argumentValues[indices])]
    }

    fieldNames <- fieldNames[indices]
    
  } else {
    
    argumentMode <- .stopIfModeNotCharacterOrNumeric(mainArgument, argumentPrefix)

    if (argumentMode == "character") {
      
      mainArgument <- kwb.utils::hsTrim(mainArgument)
      fieldNames <- mainArgument[mainArgument != ""]
      
    } else if (argumentMode == "numeric") {
      
      indices <- .toSafeIndices(mainArgument, argumentPrefix, length(fieldNames))
      fieldNames <- fieldNames[indices]
    }
  }

  fieldNames
}

# .argumentsToConditions -------------------------------------------------------

#' @importFrom kwb.utils hsTrim
#' 
.argumentsToConditions <- function(mainArgument, arguments, argumentPrefix)
{
  fieldNames <- .stripPrefix(names(arguments), argumentPrefix)

  if (is.null(mainArgument)) {

    if (FALSE) {
      
      argumentValues <- unlist(arguments)
      stopifnot(!is.null(argumentValues))
      indices <- which(!is.na(argumentValues) & argumentValues != "")
      fieldNames <- fieldNames[indices]
      values <- argumentValues[indices]
      conditions <- .namesAndValuesToConditions(fieldNames, values)
      
    } else {
      
      conditions <- character()
      
      for (i in seq_len(length(arguments))) {
        
        argument <- arguments[[i]]
        
        if (!all(is.na(argument))) {
          
          condition <- .toCondition(fieldName = fieldNames[i], value = argument)
          
          conditions <- c(conditions, condition)
        }
      }
    }
    
  } else {
    
    argumentMode <- .stopIfModeNotCharacterOrNumeric(mainArgument, argumentPrefix)

    if (argumentMode == "character") {
      
      mainArgument <- kwb.utils::hsTrim(mainArgument)
      conditions <- mainArgument[mainArgument != ""]
      
    } else if (argumentMode == "numeric") {
      
      indices <- .toSafeIndices(mainArgument, argumentPrefix, length(fieldNames))
      fieldNames <- fieldNames[abs(indices)]
      values <- rep("IsNull", length(indices))
      positive <- indices > 0
      values[positive] <- paste("NOT", values[positive])
      conditions <- .namesAndValuesToConditions(fieldNames, values)
    }
  }

  conditions
}

# .namesAndValuesToConditions --------------------------------------------------

.namesAndValuesToConditions <- function(fieldNames, values)
{
  conditions <- character()

  for (i in seq_len(length(values))) {
    
    conditions <- c(conditions, .toCondition(fieldNames[i], values[i]))
  }

  conditions
}

# .toCondition -----------------------------------------------------------------

#' @importFrom kwb.utils commaCollapsed hsQuoteChr
#' 
.toCondition <- function(fieldName, value)
{
  if (mode(value) == "numeric") {
    
    if (length(value) > 1) {
      
      sprintf("%s IN (%s)", fieldName, kwb.utils::commaCollapsed(value))
      
    } else {
      
      paste(fieldName, "=", value)
    }
    
  } else {
    
    if (all(grepl("IsNull", value, ignore.case = TRUE))) {
      
      isNullExpression <- sprintf("IsNull(%s)", fieldName)
      
      gsub("IsNull", isNullExpression, value, ignore.case = TRUE)
      
    } else {
      
      if (all(.looksLikeFullExpression(value))) {
        
        sprintf("%s %s", fieldName, value)
        
      } else {
        
        if (length(value) > 1) {
          
          sprintf(
            "%s IN (%s)",
            fieldName,
            kwb.utils::commaCollapsed(kwb.utils::hsQuoteChr(value, qchar = "'"))
          )
          
        } else {
          
          sprintf("%s = '%s'", fieldName, value)
        }
      }
    }
  }
}

# .looksLikeFullExpression -----------------------------------------------------

#' @importFrom kwb.utils hsTrim
#' 
.looksLikeFullExpression <- function(x)
{
  grepl(
    pattern = "^((=|LIKE)\\s*['\"])|BETWEEN|[<=>]",
    x = kwb.utils::hsTrim(x),
    ignore.case = TRUE
  )
}

# .stopIfModeNotCharacterOrNumeric ---------------------------------------------

.stopIfModeNotCharacterOrNumeric <- function(x, variableName)
{
  argumentMode <- mode(x)

  if (! (argumentMode %in% c("character", "numeric"))) {
    
    clean_stop(variableName, " must be of mode character or numeric")
  }

  argumentMode
}

# .toSafeIndices ---------------------------------------------------------------

#' @importFrom kwb.utils inRange
#' 
.toSafeIndices <- function(mainArgument, argumentPrefix, maxIndex)
{
  indices <- round(mainArgument)

  if (! all(kwb.utils::inRange(abs(indices), 1, maxIndex))) {
    
    clean_stop(
      "All elements of ", argumentPrefix, " must be numbers with ",
      "absolute values between 1 and ", maxIndex, " when used in numeric mode"
    )
  }

  indices
}

# .stripPrefix -----------------------------------------------------------------

.stripPrefix <- function(x, prefix)
{
  substr(x, nchar(prefix) + 2, nchar(x))
}
