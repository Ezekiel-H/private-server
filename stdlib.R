libs <- c('tidyverse', 'rvest', 'stringi', 'httr', 'curl', 'jsonlite', 'getPass', 'lubridate')
lapply(libs, require, character.only = TRUE)

thats_all_folks <- function() {
  
  cat("
                           , 
                          /,\\ 
                         //|\\\\   _ 
                     ,-.//||_\\`,'_`. 
                    /,-.,-.o,-. ( `\\\\__ 
                  _,/_ //_) ,---'`-'   `-. 
                ;'      `--'              `-. 
              ,'                             `. 
            ,'         ___   _,-.__            `. 
           ,'      ,--'   `-'      `-.          _\\_ 
          /       /                   \\       ,',- `. 
        _/       /           ::'##`--. \\     ( (   _/ 
      ,',`.     |  _,-:::'   ,::' ___ `.`;   /  _,'  \\ 
     ((_  ``.   |,' ___      :' ,'(O)`.  |  \\,-'      \\ 
      `--.__/   |  /(O)`.    :  `----'    \\            ) 
       ;       ;  `----','    `.           \\           ; 
       :       |       (       _)           ;         / 
       |       |      , `##--#'     `.      |        / 
       ;       |     '   _.._...___.  `     |       ; 
        `.     :    ' -::_____.--';         ;     _.' 
          `.    \\       `______.-'        ;; _.-' 
            `-._ \\                      .'(-'-. 
              '-`.'`.                _.' 
                     `--._      _.--' 
                           `__'
  ")
  return()
}

get_places_api_key <- function() {
  return('')
}

static_var <- function(var_name, val = NA) {
  if(is.na(val)) {
    # caller did not pass a value, so they want the value (read operation)
    return(attr(static_var, var_name))
  } else {
    # caller passed a value, so they want to write the value
    attr(static_var, var_name) <<- val
    return()
  }
}



lazy_load_df <- function(fn, sql_fn=NA, dsn="", num_expiry_days=7, FUN=NULL) {
  
  odbcChannel   <- odbcConnect(dsn)  # ZEKE for you dsn needs to be "SQLRDB",
  fn_current    <- FALSE
  message(paste("Checking for", fn, "RDS file")) 
  fn_exists     <- file.exists(fn)
  skip_db_step  <- is.na(sql_fn)
  if(skip_db_step) {
    sql_fn_exists <- FALSE
  } else {
    message(paste("Checking for", fn, "SQL file")) 
    sql_fn_exists <- file.exists(sql_fn)
  }
  
  if(!skip_db_step & !sql_fn_exists) {
    warning(paste("SQL file", sql_fn, "could not be found - lazy_load_df() failed"))
    odbcClose(odbcChannel)
    return(NA)
  }
  
  if(fn_exists) {
    info <- file.info(fn)
    # check that file is no older than "num_expiry_days" days.
    fn_current <- days(ymd(format(Sys.time(), "%Y-%m-%d")) - ymd(format(info$mtime, "%Y-%m-%d"))) < days(num_expiry_days)
    if( !fn_current ) {
      message("RDS file found, but is too old - so removing")
      file.remove(fn)  
    } else {
      message("RDS file found, and is current. Loading into dataframe")
      df <- readRDS(fn)
    }
  } else {
    message("RDS file not found - generating dataframe from SQL")
  }
  
  if(!(fn_exists & fn_current)) {
    if(!skip_db_step) {
      # SQL query needs to run
      message(paste("Running SQL file", sql_fn))
      # run query into df and save
      sql_txt <- readChar(sql_fn, file.info(sql_fn)$size)
      df <- as_tibble(sqlQuery(odbcChannel, sql_txt)) 
      message("SQL Query run and loaded into dataframe.")
    }
    if(!is_null(FUN)) {
      # a post processing function was passed
      # this is a closure so even although we are in a separate function we will still be able to access
      # the local variables of the callign function as if they were our local variables
      # be ware of variables with the same names declared here and in the caller.
      message(paste("Beginning", if_else(skip_db_step, 'first pass', 'post'), "processing of dataframe"))
      df <- FUN(df) 
      message(paste(if_else(skip_db_step, 'First pass', 'Post'), "processing completed"))
    }
    saveRDS(df, fn)
    message(paste("Dataframe saved into RDS file", fn))
  }
  
  odbcClose(odbcChannel)
  return(df)
}


set_ups <- function(running_dir = NULL) {
  cat("\014")                                       ## Clear the console
  message('set_ups()')
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")  ## Needed for writing Excel files
  options(digits=14)                                ## Make nice wide doubles to hold lat_northings and lon_eastings
  options(timeout=5*60)
  options(stringsAsFactors = FALSE)
  if(!is.na(running_dir)) {
    setwd(running_dir)
  }
}

set_proxy <- function(v_password = NA) {
  message('set_proxy()')
  if(is.na(v_password)) {
    v_password <- getPass(msg = " Enter your password:  ", forcemask = FALSE)
  }
  v_username <- Sys.info()[["user"]]
#  v_url      <- "pxychc"
  v_url      <- ""
  v_port     <- 8080
  
  # this next line is an attempt to make Google places API work, so if other apps are not working try removing this line.
  options(internet.info = 0)
  
  http_proxy_str <- paste("http://", v_username, ":", v_password, "@", v_url, ":", v_port, sep='')
  https_proxy_str <- paste("http://", v_username, ":", v_password, "@", v_url, ":", v_port, sep='')
  
  # Welcome to the wild wild west of Windows networking.
  #options(RCurlOptions = list(proxy = http_proxy_str))
  
  Sys.setenv('http_proxy' = http_proxy_str)
  Sys.setenv('https_proxy' = https_proxy_str)
  
  
  static_var('proxy_has_been_set', val = TRUE)
  return()
}


get_directors <- function(directors_url) {
  # the mechanics of accesing the HTML can be a major pain in the posterior
  # it does not seem to be uniform across different platforms
  # so what is here works on MAC OS X but under (deliberate lowercase) windoze
  # you may be able to work with an HTML stream or memory object.
  # ..... I might have to rewrite this bit, perhaps making it OS conditional.
  
  if(static_var('proxy_has_been_set') == FALSE) set_proxy()
  destfilestr <- './output/directors.html'
  download.file(directors_url, destfile = destfilestr)
  directors <- read_html(destfilestr) %>%
    html_nodes("td.director") %>%
    html_text()
  
  directors <- data.frame(raw_text = directors, stringsAsFactors = FALSE)
  
  directors <- directors %>%
    mutate(raw_text = gsub("\r|\n", "", raw_text)) %>%
    mutate(full_legal_name = trimws(gsub("Full legal name:(.+) Residential Address:.*$", "\\1", raw_text))) %>%
    mutate(residential_address = trimws(gsub("^.*Residential Address:(.+) Appointment Date:.*$", "\\1", raw_text))) %>%
    mutate(appointment_date = trimws(gsub("^.*Appointment Date:(.+) Consent:.*$", "\\1", raw_text))) %>%
    select(-raw_text)
  
  return(directors)
}
  
  
get_shareholders <- function(shareholdings_url) {
  
  if(static_var('proxy_has_been_set') == FALSE) set_proxy()
  destfilestr <- './output/shareholders.html'
  download.file(shareholdings_url, destfile = destfilestr)
  shareholders <- read_html(destfilestr) %>%
    html_nodes("div#allocations div.allocationDetail ") %>%
    html_text()
  
  shareholders <- data.frame(raw_text = shareholders, stringsAsFactors = FALSE)
  mac_window_pattern = ifelse(.Platform$OS.type == "windows", '(\\s+\\r+\\s+\\,")', '(\\s|\")+')
  
  
  pattern <- "c\\(\\s+\\d+:(\\d+)\\|\\((\\d+\\.\\d+)%\\)\\|([^|]+)\\|([^|]+)\\|([^|]+)\\|.*$"
  shareholders <- shareholders %>%
    #mutate(raw_text = strsplit(shareholders[[1]], "\\r\\n")) %>%
    mutate(raw_text = gsub("(\\s|\")+"," ",strsplit(shareholders[[1]], "\r\n"))) %>%
    mutate(raw_text = as.character(raw_text)) %>%
    mutate(raw_text = gsub("\\s+,", "|", raw_text)) %>%
    mutate(raw_text = gsub('", "', '|', raw_text)) %>%
    mutate(raw_text = gsub("\\s*..\\|\\s*", '|', raw_text)) %>%
    mutate(raw_text = gsub("\\|+", "|", raw_text)) %>%
    mutate(num_shares  = gsub(pattern,"\\1", raw_text),
           pcnt_shares = gsub(pattern,"\\2", raw_text),
           entity_name = gsub(pattern,"\\3", raw_text),
           addr_1      = gsub(pattern,"\\4", raw_text),
           addr_2      = gsub(pattern,"\\5", raw_text)) %>%
    select(-raw_text)
  
  return(shareholders)
}
  
get_companies_and_shareholdings <- function(person_name) {
  
  if(static_var('proxy_has_been_set') == FALSE) set_proxy()
  if(static_var('using_windows') == FALSE) {
    person_search_url <- "https://www.companiesoffice.govt.nz/companies/app/ui/pages/individual/search?q=PERSON_NAME_HERE&start=0&limit=200&entitySearch=&addressKeyword=&postalCode=&country=&addressType=&advancedPanel=false&roleType=ALL&indEntityTypes=ALL&indEntityStatusGroups=ALL&indDirStatus=ALL&sf=&sd="
    person_name = stri_trans_totitle(gsub(" ","+", person_name))
    person_search_url = gsub("PERSON_NAME_HERE", person_name, person_search_url)
    interests <- html(x = person_search_url) %>% 
      #  html_nodes("div#allocations div.allocationDetail div.row:nth-child(2) div.labelValue") %>%
      html_nodes("div.indResult") %>%
      html_text()
  } else {
    destfilestr <- './output/person.html'
    person_search_url <- "https://www.companiesoffice.govt.nz/companies/app/ui/pages/individual/search?q=PERSON_NAME_HERE&start=0&limit=200&entitySearch=&addressKeyword=&postalCode=&country=&addressType=&advancedPanel=false&roleType=ALL&indEntityTypes=ALL&indEntityStatusGroups=ALL&indDirStatus=ALL&sf=&sd="
    person_name = stri_trans_totitle(gsub(" ","+", person_name))
    person_search_url = gsub("PERSON_NAME_HERE", person_name, person_search_url)  
    download.file(person_search_url, destfile = destfilestr) 
    interests <- read_html(destfilestr) %>%
      #  html_nodes("div#allocations div.allocationDetail div.row:nth-child(2) div.labelValue") %>%
      html_nodes("div.indResult") %>%
      html_text()
  }
  
  pattern <- "\\|([^|]+)\\|([^|(]+)\\s+\\((\\d+)\\)\\|(.+)"
  interests <- data.frame(raw_text = interests, stringsAsFactors = FALSE)
  interests <- interests %>%
    mutate(raw_text = gsub("\\r|\\n|\\s+,", "|", raw_text)) %>%
    mutate(raw_text = gsub("\\|\\s+","|", raw_text)) %>%
    mutate(raw_text = gsub("\\s*\\|+", "|", raw_text)) %>%
    mutate(individual = gsub(pattern,"\\1",raw_text)) %>%
    mutate(company = gsub(pattern,"\\2",raw_text)) %>%
    mutate(company_number = gsub(pattern,"\\3",raw_text)) %>%
    mutate(details = gsub(pattern,"\\4",raw_text)) %>%
    mutate(details = gsub("\\|"," ",details)) %>%
    select(-raw_text)
  
  return(interests)
}
  
memorials_involved <- function(input_df) {
  # we expect input_df to have 1 field name entity_name
  
  if(static_var('proxy_has_been_set') == FALSE) set_proxy()
  cat("memorials_involved.\n")
  input_df$linz_url <- paste(
    'https://data.linz.govt.nz/services;key=1ecc95205f304c9bac2cc3cf41a2e57e/wfs?service=WFS&VERSION=2.0.0&REQUEST=GetFeature&typeNames=table-1695&cql_filter=memorial_text%20ILIKE%20%27%25',
    stri_trans_toupper(gsub(' ', '%20', input_df$entity_name)),
    '%25%27&sortBy=instrument_lodged_datetime&PropertyName=title_no&outputformat=json',
    sep = '')
  input_df$idx <- 1:nrow(input_df)
  json_df <- data.frame(json = 1:nrow(input_df), titles = 1:nrow(input_df))
  
  by(input_df, 1:nrow(input_df), function(row) {
    cat(paste("trying", row$linz_url, "\n"))
    json_df[[1]][row$idx] <<- NA  # set default value
    json_df[[2]][row$idx] <<- NA
    if(row$idx == 9) {
      try({
        xx <- GET(row$linz_url)
        if (status_code(xx) == 200) {
          json <- as.character(httr::content(xx, 'text'))
          json_as_structure <- fromJSON(json)
          json_as_structure <- json_as_structure$features$properties$title_no
          if (length(json_as_structure) == 0) {
            json_as_structure <- NA
          } else {
            json_as_structure <- paste(json_as_structure, collapse = ',')
          }
          cat(paste("'json' is: ", json, "\n"))
          json_df[[1]][row$idx] <<- json  # override default
          json_df[[2]][row$idx] <<- json_as_structure
          cat("got one\n")
        } else {
          cat("missed one\n")
        }
      })
    }
    json_df$entity_name <- input_df$entity_name
    return(json_df)
  })
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
  
static_var('proxy_has_been_set', val = FALSE)
static_var('using_windows', val = ("Windows" %in% Sys.info()[1])) 
