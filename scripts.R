#Scripts for the ReportGenerator App

groom <- function(x, y = FALSE) {
  x$NST_DATI <- floor_date(x$NST_DATI, unit = "minute") # round date to nearest minute
  x <- x[!duplicated(x$NST_DATI), ] # Eliminate any duplicate rows to avoid problems with sequence
  
  # if (y == TRUE) { #If Y is TRUE, then remove extra hydrometric data
  #   # Old Function #x <- x[complete.cases(x[, !(colnames(x) %in% c("STAT_NAME", "NST_DATI", "Stage", "Flow"))]),]
  #   #New function
  #   a <- as.logical(rowSums(!is.na(x[, !(colnames(x) %in% c("STAT_NAME", "NST_DATI", "STAGE", "FLOW"))])))
  #   b <- as.vector (!is.na(x[, 'STAGE']))
  #   x[!(a == FALSE & b == TRUE), ]
  # }
  
  if (y == TRUE) {
    browser()
    if ("STAGE" %in% names(x)) {
      a <- as.logical(rowSums(!is.na(x[, !(colnames(x) %in% c("STAT_NUM", "NST_DATI", "STAGE", "FLOW"))])))
      b <- as.vector (!is.na(x[, 'STAGE']))
      x[!(a == FALSE & b == TRUE), ]
    }
  }
  
  lst <- list() # init empty lst to hold subsets
  lst_index <- 1 # lst_index
  
  for (i in unique(format(x$NST_DATI, "%M"))) { # Subset each unique minute value into lst according to index
    lst[[lst_index]] <- x %>%
      filter(format(x$NST_DATI, "%M") == i)
    x <- x %>%
      filter(!format(x$NST_DATI, "%M") == i)
    lst_index <- lst_index + 1
  }
  
  for (i in 1:length(lst)) { # If each unique minute > length 2, fill blanks
    y <- lst[[i]]
    if (length(y$NST_DATI > 2)) {
      min <- y$NST_DATI[1]
      max <- y$NST_DATI[length(y$NST_DATI)]
      sequence <- seq(min, max, by = y$NST_DATI[2] - y$NST_DATI[1])
      times <- data.frame(list(NST_DATI = sequence))
      y <- merge(times, y, all = TRUE)
      y$STAT_NUM <- y$STAT_NUM[1]
      y$STAT_NAME <- y$STAT_NAME[1]
      y$variable <- y$variable[1]
      lst[[i]] <- y
    } else {
      lst[[i]] <- y
    }
  }
  return(bind_rows(lst) %>%
           arrange(NST_DATI))
}