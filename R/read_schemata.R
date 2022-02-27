# read_schemata ----------------------------------------------------------------

#' Read Table Schemata from XML Files
#' 
#' This function reads the *_schema.xml files exported to folder TBL by KWB tool 
#' mdbToTxt.exe
#' 
#' @param path path to folder containing xml files
#' @param reduce if \code{TRUE} (the default) the returned tables are reduced to 
#'   relevant fields
read_schemata <- function(path, reduce = TRUE)
{
  files <- dir(path, "\\.xml$", full.names = TRUE)
  
  schemata <- lapply(files, read_schema)
  
  names(schemata) <- sapply(schemata, function(x) x$table)
  
  if (! reduce) {
    return(schemata)
  }
  
  lapply(schemata, reduce_to_relevant)
}

# read_schema ------------------------------------------------------------------
read_schema <- function(file)
{
  xpaths <- list(
    fields = "/xsd:schema/xsd:element/xsd:complexType/xsd:sequence/xsd:element",
    indices = "/xsd:schema/xsd:element/xsd:annotation/xsd:appinfo/od:index"
  )
  
  check_xml2()

  schema <- xml2::read_xml(file)
  
  nodes <- xml2::xml_find_all(schema, xpaths$fields)
  indices <- xml2::xml_find_all(schema, xpaths$indices)
  
  list(
    table = xml2::xml_attr(nodes[[1L]], "ref"),
    fields = get_field_info(nodes = nodes[-1L]),
    indices = get_index_info(indices)
  )
}

# check_xml2 -------------------------------------------------------------------
check_xml2 <- function()
{
  if (! requireNamespace("xml2")) {
    clean_stop("Please install R package 'xml2' to use this function.")
  }
}

# get_field_info ---------------------------------------------------------------
get_field_info <- function(nodes)
{
  attr_names <- list(
    field = "name",
    minOccurs = "minOccurs",
    jetType = "jetType",
    sqlType = "sqlSType",
    autoUnique = "autoUnique",
    nonNullable = "nonNullable",
    type = "type"
  )
  
  field_info <- get_attributes(nodes, attr_names)
  
  max_lengths <- character(nrow(field_info))
  is_text <- field_info$jetType == "text"
  
  if (any(is_text)) {
    max_lengths[is_text] <- sapply(nodes[is_text], get_max_length)    
  }
  
  field_info[["maxLength"]] <- max_lengths
  
  field_info
}

# get_attributes ---------------------------------------------------------------
get_attributes <- function(x, attr_names)
{
  check_xml2()
  
  do.call(rbind, lapply(x, function(node) {
    do.call(kwb.utils::noFactorDataFrame, lapply(
      attr_names, 
      function(attr) kwb.utils::defaultIfNA(xml2::xml_attr(node, attr), "")
    ))
  }))
}

# get_max_length ---------------------------------------------------------------
get_max_length <- function(node)
{
  check_xml2()
  
  xpath <- ".//xsd:simpleType/xsd:restriction/xsd:maxLength"
  xml2::xml_attr(xml2::xml_find_all(node, xpath), "value")
}

# get_index_info ---------------------------------------------------------------
get_index_info <- function(indices)
{
  attr_names <- list(
    index = "index-name",
    key = "index-key",
    primary = "primary",
    unique = "unique",
    clustered = "clustered",
    order = "order"
  )
  
  result <- get_attributes(indices, attr_names)
  
  # Trim leading/trailing whitespace in key
  result$key <- gsub("^\\s+|\\s+$", "", result$key)
  
  result
}

# reduce_to_relevant -----------------------------------------------------------
reduce_to_relevant <- function(x)
{
  is_relevant <- x$indices$primary == "yes" | x$indices$unique == "yes"
  
  x$fields <- kwb.utils::removeColumns(
    x$fields, c("minOccurs", "sqlType", "type")
  )
  
  x$indices <- kwb.utils::removeColumns(
    x$indices[is_relevant, ], c("index", "clustered", "order")
  )
  
  x$table <- NULL
  
  x
}
