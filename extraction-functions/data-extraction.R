if(!require(stringr)){install.packages('stringr')}
library(stringr)
if(!require(httr)){install.packages('httr')}
library(httr)

# Get a list of articles to go through
dois <- list.files('apa_articles')

# Extract the results from each paper
for(doi in dois)
{
  if(!file.exists(sprintf('apa_articles/%s/results.csv', doi)))
  {
    filename <- sprintf('apa_articles/%s/fulltext.txt', doi)
    txt <- readChar(filename, file.info(filename)$size)
    
    txt <- gsub(pattern = '_', replacement = '', x = txt)
    txt <- gsub(pattern = '&lt;', replacement = '<', x = txt)
    txt <- gsub(pattern = '&gt;', replacement = '>', x = txt)
    
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
    
    # search for references to idea*
    ## their locations
    locations_idea <- str_locate_all(pattern = "idea", txt)[[1]]
    ## strings before and after it
    pre_idea <- str_sub(txt,
                        start = locations_idea[, 1] - 100,
                        end = locations_idea[, 1])
    
    post_idea <- str_sub(txt,
                         end = locations_idea[, 2] + 100,
                         start = locations_idea[, 2])
    
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
    
    # Combine with previously collected metadata
    sel <- grepl(list.files('data/metadata'), pattern = doi)
    x <- tryCatch(read.csv(sprintf('data/metadata/%s',
                                   list.files('data/metadata')[sel]),
                           header = FALSE),
                  error = function (e) data.frame(V1 = NA, V2 = NA))
    
    if(!is.null(comparison))
    {
      df <- data.frame(doi,
                       journal = as.character(x$V1)[1],
                       year = as.numeric(x$V2)[1],
                       
                       locations_p,
                       result,
                       comparison,
                       value,
                       
                       locations_hypothesi,
                       pre_hypothesi,
                       post_hypothesi,
                       
                       locations_theor,
                       pre_theor,
                       post_theor,
                       
                       locations_account,
                       pre_account,
                       post_account,
                       
                       locations_model,
                       pre_model,
                       post_model,
                       
                       locations_data,
                       pre_data,
                       post_data,
                       
                       locations_idea,
                       pre_idea,
                       post_idea,
                       
                       locations_finding,
                       pre_finding,
                       post_finding)
      
      write.csv(df, 
                sprintf('apa_articles/%s/results.csv', doi),
                row.names = FALSE)
    }
    cat(sprintf('%s\n', doi)) 
  }
}