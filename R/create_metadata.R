#devtools::install_github("tonyfujs/mdlibtoddh")
#devtools::install_github("tonyfujs/ddhconnect")
library(mdlibconnect)
library(pipload)
library(tidyverse)
library(mdlibtoddh)

source("R/utils.R")


# Constant ----------------------------------------------------------------

json_path <- "output/metadata_json.rds"

# Get PIP surveys ---------------------------------------------------------

pfw <- pipload::pip_load_aux(measure = "pfw")
pfw$surveyid_year <- NULL
ids <- pfw %>%
  filter(!is.na(wbint_link)) %>%
  pull(wbint_link)



# Extract metadata --------------------------------------------------------

my_token <- Sys.getenv("mdlib_token")

metadata <- vector(mode = "list", length = length(ids))

# for (i in seq_along(ids)) {
for (i in 1197:1251) {
  result <- try({
    m1 <- get_survey_metadata(id = ids[[i]], 
                              token = my_token)
    m2 <- get_survey(id = ids[[i]],
                     token = my_token)
    out <- list(c(m1, m2))
  }, silent = TRUE)
  
  if (inherits(result, "try-error")) {
    # optional: message about the failure
    message("Skipping i = ", i, " because of error: ", attr(result, "condition")$message)
    next  # go to next iteration
  }
  
  metadata[[i]] <- out
  
  print(i)
}


readr::write_rds(metadata, json_path)

# Flatten metadata --------------------------------------------------------

metadata <- readr::read_rds(json_path)

metadata <- purrr::keep(metadata, ~ !is.null(.x))

out <- purrr::map(metadata, get_study_desc)

out <- dplyr::bind_rows(out)


# Save results as .csv ----------------------------------------------------

readr::write_csv(out, "output/metadata.csv", na = "")
# haven::write_dta(out, "output/metadata.dta", version = 15)
