if(!require(dplyr)){install.packages('dplyr')}
library(dplyr)
if(!require(stringr)){install.packages('stringr')}
library(stringr)
if(!require(httr)){install.packages('httr')}
library(httr)

# Get a list of articles to go through
dois <- list.files('apa_articles') 

# Extract the results from each paper
for(doi in dois) {
  if(!file.exists(sprintf('apa_articles/%s/results.csv', doi))) {  
    
    filename <- sprintf('apa_articles/%s/fulltext.txt', doi)  
    txt <- readChar(filename, file.info(filename)$size)
    
    txt <- gsub(pattern = '_', replacement = '', x = txt)
    txt <- gsub(pattern = '&lt;', replacement = '<', x = txt)
    txt <- gsub(pattern = '&gt;', replacement = '>', x = txt)
    
    total_length <- nchar(txt)
    
    # search for p values
    ## their locations
    locations_p <- str_locate_all(pattern = "\\sp\\s*[=<>]\\s*\\d?\\.\\d*", txt)[[1]]
    
    ## the p values
    result <- str_match_all(pattern = "\\sp\\s*[=<>]\\s*\\d?\\.\\d*", txt)[[1]][,1] %>%
      gsub(patter = '[_ ]', replacement = '')
    
    comparison <- unlist(str_match_all(result, pattern = "[<>=]"))
    
    value <- unlist(str_match_all(result, pattern = "\\d?\\.\\d*"))
    
    
    # search for references to hypothes*
    ## their locations
    locations_hypothesi <- str_locate_all(pattern = "hypothesi", txt)[[1]]
    ## strings before and after it
    pre_hypothesi <- str_sub(txt,
                             start = locations_hypothesi[, 1] - 100,
                             end = locations_hypothesi[, 1])
    
    post_hypothesi <- str_sub(txt,
                              end = locations_hypothesi[, 2] + 100,
                              start = locations_hypothesi[, 2])
    
    # search for references to theor*
    ## their locations
    locations_theor <- str_locate_all(pattern = "theor", txt)[[1]]
    ## strings before and after it
    pre_theor <- str_sub(txt,
                         start = locations_theor[, 1] - 100,
                         end = locations_theor[, 1])
    
    post_theor <- str_sub(txt,
                          end = locations_theor[, 2] + 100,
                          start = locations_theor[, 2])
    
    # search for references to account*
    ## their locations
    locations_account <- str_locate_all(pattern = "account", txt)[[1]]
    ## strings before and after it
    pre_account <- str_sub(txt,
                           start = locations_account[, 1] - 100,
                           end = locations_account[, 1])
    
    post_account <- str_sub(txt,
                            end = locations_account[, 2] + 100,
                            start = locations_account[, 2])
    
    # search for references to model*
    ## their locations
    locations_model <- str_locate_all(pattern = "model", txt)[[1]]
    ## strings before and after it
    pre_model <- str_sub(txt,
                         start = locations_model[, 1] - 100,
                         end = locations_model[, 1])
    
    post_model <- str_sub(txt,
                          end = locations_model[, 2] + 100,
                          start = locations_model[, 2])
    
    # search for references to data*
    ## their locations
    locations_data <- str_locate_all(pattern = "data", txt)[[1]]
    ## strings before and after it
    pre_data <- str_sub(txt,
                        start = locations_data[, 1] - 100,
                        end = locations_data[, 1])
    
    post_data <- str_sub(txt,
                         end = locations_data[, 2] + 100,
                         start = locations_data[, 2])
    
    # search for references to finding*
    ## their locations
    locations_finding <- str_locate_all(pattern = "finding", txt)[[1]]
    ## strings before and after it
    pre_finding <- str_sub(txt,
                           start = locations_finding[, 1] - 100,
                           end = locations_finding[, 1])
    
    post_finding <- str_sub(txt,
                            end = locations_finding[, 2] + 100,
                            start = locations_finding[, 2])
    
    # search for references to result*
    ## their locations
    locations_result <- str_locate_all(pattern = "result", txt)[[1]]
    ## strings before and after it
    pre_result <- str_sub(txt,
                          start = locations_result[, 1] - 100,
                          end = locations_result[, 1])
    
    post_result <- str_sub(txt,
                           end = locations_result[, 2] + 100,
                           start = locations_result[, 2])
    
    
    if(!is.null(comparison)) {
      
      # combine results into long data frames
      data_total_length <- data.frame(total_length = total_length) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "length",
               doi = doi,
               value = as.character(value))
      
      p_values <- data.frame(locations_p = locations_p,
                             result = result,
                             comparison = comparison,
                             value = value) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "p_values",
               doi = doi)
      
      string_hypothesi <- data.frame(locations_hypothesi = locations_hypothesi,
                                     pre_hypothesi = pre_hypothesi,
                                     post_hypothesi = post_hypothesi) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_hypothesi",
               doi = doi)
      
      string_theor <- data.frame(locations_theor = locations_theor,
                                 pre_theor = pre_theor,
                                 post_theor = post_theor) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_theor",
               doi = doi)
      
      string_account <- data.frame(locations_account = locations_account,
                                   pre_account = pre_account,
                                   post_account = post_account) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_account",
               doi = doi)
      
      string_model <- data.frame(locations_model = locations_model,
                                 pre_model = pre_model,
                                 post_model = post_model) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_model",
               doi = doi)
      
      string_data <- data.frame(locations_data = locations_data,
                                pre_data = pre_data,
                                post_data = post_data) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_data",
               doi = doi)
      
      string_finding <- data.frame(locations_finding = locations_finding,
                                   pre_finding = pre_finding,
                                   post_finding = post_finding) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_finding",
               doi = doi)
      
      string_result <- data.frame(locations_result = locations_result,
                                   pre_result = pre_result,
                                   post_result = post_result) %>%
        rownames_to_column(var = "exemplar") %>%
        gather(key, value, -exemplar) %>%
        mutate(type = "string_result",
               doi = doi)
      
      # combine dfs
      df <- bind_rows(data_total_length,
                      p_values, 
                      string_hypothesi, 
                      string_theor,
                      string_account,
                      string_model,
                      string_data,
                      string_finding,
                      string_result) %>%
        dplyr::select(doi, exemplar, type, key, value)
      
      # write to disk
      write.csv(df, 
                sprintf('apa_articles/%s/results.csv', doi), 
                row.names = FALSE)
    }
    cat(sprintf('%s\n', doi)) 
  }
}