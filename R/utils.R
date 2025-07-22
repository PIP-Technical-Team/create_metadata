get_study_desc <- function(mtd) {
  
  mtd <- purrr::flatten(mtd)
  
  # Title and id ------------------------------------------------------------
  
  id     <- mtd$id
  svy_id <- mtd$study_desc$title_statement$idno
  title  <- mtd$study_desc$title_statement$title
  year_start <- mtd$year_start
  year_end   <- mtd$year_end
  data_access <- mtd$data_access_type
  
  
  # Authoring entity --------------------------------------------------------
  
  authoring_entity_name <- collapse_list(mtd$study_desc$authoring_entity,
                                         "name")
  authoring_entity_affiliation <- collapse_list(mtd$study_desc$authoring_entity,
                                                "affiliation")
  
  
  # Contact --------------------------------------------------
  
  contact_name <- collapse_list(mtd$study_desc$distribution_statement$contact,
                                "name")
  contact_affiliation <- collapse_list(mtd$study_desc$distribution_statement$contact,
                                       "affiliation")
  contact_email <- collapse_list(mtd$study_desc$distribution_statement$contact,
                                 "email")
  contact_uri <- collapse_list(mtd$study_desc$distribution_statement$contact,
                               "uri")
  
  # Abstract ----------------------------------------------------------------
  
  abstract <- mtd$study_desc$study_info$abstract
  
  # extract collection dates information ------------------------------------
  
  # coll_dates_elements <- c("start", "end", "cycle")
  collection_dates_cycle <- collapse_list(mtd$study_desc$study_info$coll_dates,
                                          "cycle")
  collection_dates_start <- collapse_list(mtd$study_desc$study_info$coll_dates,
                                          "start")
  collection_dates_end <- collapse_list(mtd$study_desc$study_info$coll_dates,
                                        "end")
  
  
  # Geographical coverage ---------------------------------------------------
  
  coverage <- mtd$study_desc$study_info$geog_coverage 
  
  
  # Method ------------------------------------------------------------------
  
  sampling_procedure  <- mtd$study_desc$method$data_collection$sampling_procedure  
  collection_mode     <- unlist(mtd$study_desc$method$data_collection$coll_mode) 
  coll_situation      <- mtd$study_desc$method$data_collection$coll_situation 
  weight              <- mtd$study_desc$method$data_collection$weight 
  cleaning_operations <- mtd$study_desc$method$data_collection$cleaning_operations
  
  
  # Combine results ---------------------------------------------------------
  
  out <- list(
    id = id,
    svy_id = svy_id,
    title  = title,
    data_access = data_access,
    year_start = year_start,
    year_end = year_end,
    authoring_entity_name = authoring_entity_name,
    authoring_entity_affiliation = authoring_entity_affiliation,
    contact_name = contact_name,
    contact_affiliation = contact_affiliation,
    contact_email = contact_email,
    contact_uri = contact_uri,
    abstract = abstract,
    collection_dates_cycle = collection_dates_cycle,
    collection_dates_start = collection_dates_start,
    collection_dates_end  = collection_dates_end,
    coverage = coverage,
    sampling_procedure = sampling_procedure,
    collection_mode = collection_mode,
    coll_situation = coll_situation,
    weight  = weight ,
    cleaning_operations = cleaning_operations
  )  

# Fix NULL value to allow row binding -------------------------------------

  to_be_fixed <-  purrr::map_lgl(out, is_null)
  out[to_be_fixed] <- ""


# Fix strings containing only | -------------------------------------------

  to_be_fixed <-  purrr::map_lgl(out, `==`, "|")
  out[to_be_fixed] <- ""
  
  return(
    out
  )
}

#' Collapse list into single string
#'
#' @param x list
#' @param element character: element to extract from the list
#'
#' @return character

collapse_list <- function(x, element) {
  out <- unlist(purrr::map(x, element))
  out <- paste(out, collapse = "|")
  
  return(out)
}
