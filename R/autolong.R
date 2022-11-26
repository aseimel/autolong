#' Automatically aggregate, combine and harmonize multiple survey data sources.
#'
#' This function combines, aggregates, and harmonizes multiple datasets into a
#' single long format dataset. The user must provide a standardized csv table
#' containing all necessary coding information, which can be found in the
#' github repository.
#'
#' @param info.csv a csv file that contains the required codinf information
#' @return A matrix of the infile
#' @export
autolong <- function(info.csv){
x <-  read.csv(info.csv)
myCluster <- makeCluster(detectCores(), # number of cores to use
                         type = "PSOCK") # type of cluster
registerDoParallel(myCluster)

output <- foreach(i=1:nrow(x), .combine=rbind, .packages = c("tidyverse","naniar", "countrycode", "scales", "haven")) %dopar% {

  format <- str_extract(tolower(x$filename[i]), "([^\\.]*)$")

  #decting file format and loading data
  if (format == "dta") {
    c <-  read_dta(x$filename[i])
  } else if (format == "rds") {
    c <-  readRDS(info$filename[i])
  } else if (format == "sav") {
    c <-  read_sav(info$filename[i])
  } else if (format == "por") {
    c <-  read_spss(info$filename[i])
  } else if (format == "sas7bdat") {
    c <-  read_sas(info$filename[i])
  } else {
    paste("Unknown file format in row", i)
  }

  #defining grouping based on whether country is supplied by the table or by the dataset
  if (x$year_constant[i] == "no" & x$country_constant[i] == "no") {
    groups <- c(x$year_var[i], x$country_var[i])
  } else if (x$year_constant[i] == "no" & x$country_constant[i] == "yes") {
    groups <- x$year_var[i]
  } else if (x$year_constant[i] == "yes" & x$country_constant[i] == "no") {
    groups <- x$country_var[i]
  } else if (x$year_constant[i] == "yes" & x$country_constant[i] == "yes") {
    groups <- ""
  } else {print(paste("Error: one of the constant dummies is not correctly set in row", i))}


  c <- c %>%
    mutate(response = as.numeric(as.numeric(eval(parse(text = x$item_var[i])))))
  #code missings
  if (x$item_na[i] == "none"){
  }else{
    nas <- as.numeric(unlist(strsplit(x$item_na[i], ",")))
    c <- replace_with_na(c,
                         replace = list(response = nas))

  }



  c <- c %>%
    mutate(response = rescale(response, from = c(as.numeric(x$item_scale_min[i]), as.numeric(x$item_scale_max[i])), to = c(0,1)))

  #reverts scale (if necessary)
  if (x$reverse_scale[i] == "no"){
  }else{
    c <- mutate(c, response = -(response-1))
  }


  c <- c %>%
    group_by_at(groups) %>%
    summarise(response = weighted.mean(response, eval(parse(text = x$weight[i])), na.rm = T),
              sample = n()) %>%
    mutate(project = x$project_label[i],
           item = x$item_label[i])


  #code country and year variables
  if (x$year_constant[i] == "no" & x$country_constant[i] == "no") {
    c <- c %>%
      mutate(country = eval(parse(text = x$country_var[i])),
             year = eval(parse(text = x$year_var[i])))
  } else if (x$year_constant[i] == "no" & x$country_constant[i] == "yes") {
    c <- c %>%

      mutate(country = x$country_var[i],
             year = eval(parse(text = x$year_var[i])))

  } else if (x$year_constant[i] == "yes" & x$country_constant[i] == "no") {
    c <- c %>%
      mutate(year = x$year_var[i],
             country =  eval(parse(text = x$country_var[i])))

  } else if (x$year_constant[i] == "yes" & x$country_constant[i] == "yes") {
    c <- c %>%
      mutate(year = x$year_var[i],
             country = x$country_var[i])
  }

  if (x$country_format[i] == "custom") {
    link.tab <- read.csv(x$country_link[i]) %>%
      mutate(link = as.character(link))
    c <- c %>%
      rename(link = country) %>%
      mutate(link = as.character(link)) %>%
      left_join(link.tab) %>%
      select(!link)

  } else {
    c <-  mutate(c, country = countrycode(country, origin = x$country_format[i], destination = "country.name", nomatch = NULL),
                 year = as.numeric(year))
  }


  c <- c %>%
    select(response, sample, project, item, year, country) %>%
    filter(!is.nan(response))

  c

}
return(output)
}








