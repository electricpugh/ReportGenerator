#Scripts for the ReportGenerator App

groom <- function(x, y = FALSE) {

  x$NST_DATI <- floor_date(x$NST_DATI, unit = "minute") # round date to nearest minute
  x <- x[!duplicated(x$NST_DATI), ] # Eliminate any duplicate rows to avoid problems with sequence
  
  lst <- list() # init empty list to hold subsets
  lst_index <- 1 # lst_index
  second.lst <- list() # init empty list to hold final copy of lst
  second.lst_index <- 1
  
  for (i in unique(format(x$NST_DATI, "%M"))) { # Subset each unique minute value into lst according to index
    lst[[lst_index]] <- x %>%
      filter(format(x$NST_DATI, "%M") == i)
    lst_index <- lst_index + 1
  }
  
  if (y == TRUE) { # Removes extra Hydrometric data from each subsetted minute in lst and places remainging data into second.lst
    for (i in 1:length(lst)) {
      if (any(!is.na(lst[[i]][, !(colnames(x) %in% c("STAT_NUM", "WSC_NUM", "NST_DATI", "STAGE", "FLOW"))]))) {
         second.lst[[second.lst_index]] <- lst[[i]]
         second.lst_index <- second.lst_index + 1
      }
    }
  } else {
    second.lst <- lst
  }
  
  for (i in 1:length(second.lst)) { # If each unique minute > length 2, fill blanks
    y <- second.lst[[i]]
    if (length(y$NST_DATI) > 2) {
      min <- y$NST_DATI[1]
      max <- y$NST_DATI[length(y$NST_DATI)]
      sequence <- seq(min, max, by = max - min)
      times <- data.frame(list(NST_DATI = sequence))
      y <- merge(times, y, all = TRUE)
      y$STAT_NUM <- y$STAT_NUM[1]
      second.lst[[i]] <- y
    } else {
      second.lst[[i]] <- y
    }
  }
  
  second.lst <- second.lst %>% # Collapses second.lst into a flat dataframe and arranges all by datetime
    bind_rows %>%
    arrange(NST_DATI)
  return(second.lst)
}