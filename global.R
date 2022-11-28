#########################
# Function: Load data
#########################
load_file <- function(name, path){
  ext <- tools::file_ext(name)
  switch (ext,
          xls = readxl::read_xls(paste(path, sep = ""), 1),
          xlsx = readxl::read_xlsx(paste(path, sep = ""), 1),
          validate("Invalid file; Please upload a .xls or .xlsx file")
  )
}

############################
# Grouping by level factors
############################
groupSummary <- function(data, summaryFunction, factors){
  summaryFunction <- enquo(summaryFunction)
  data %>% 
    group_by(., .[,factors]) %>% 
    summarise_each(funs(!!summaryFunction))
}
