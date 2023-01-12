#' similar to a select but by a given time frequency
#'
#' @param d a data.frame
#' @param ... 'what' arguments: a collection of column definitions. They often are aggregations of the original columns. See examples below
#' @param where boolean condition on columns, under the form cond1 if only one condition, and c(cond1, cond2, ...) if more than one condition
#' @param by the name of the 'timestamp' column in the data.frame from which time is sampled. Will raise an error if NULL or not one of d's column names
#' @param freq sampling frequency. Format is, e.g, 5m (5 minutes), 10s (10 seconds), 2d (two days), etc. See examples below
#' @param start start time in the frequency. If NULL (default value), start time is taken as the first timestamp encountered. Everything before the start time is dropped.
#'
#' @return the sampled data.frame
#' @export
#'
#' @examples
#'
#' Note: it is assumed that rows are ordered by the timestamp column provided in by.
#' Time format is any natural number followed by Y/M/d/h/m/s/mil/mic where mil and mic are respectively millisecond and microsecond.
#'
#' d <- data.frame(ret1 = 1:10, ret2 = rnorm(10), time = Sys.time() + 60*c(1.1,2.2,3.3,4.4,4.5,6.6,7.7,8.8,9.9,10.1))
#'
#'d %>% sampleby(ret1 = mean(ret1), ret2= mean(ret2), by = time, freq = "2m" )
#'
#'d %>% sampleby(ret1 = mean(ret1), ret2= mean(ret2), by = time, freq = "2m", start = d[1,"time"] - 60)
#'
sampleby <- function(d, ..., where = NULL, by = NULL, freq = NULL, start = NULL){

  if (is.null(freq)){
    return(d)
  }

  freq_spl <- strsplit(freq,split="")[[1]]
  freq_num <- freq_spl[(freq_spl %in% 0:9)] %>% paste0(collapse="") %>% as.numeric
  freq_char <- freq_spl[!(freq_spl %in% 0:9)] %>% paste0(collapse="")

  if (!all(freq_spl %in% c(0:9,"Y", "M", "d", "h", "m", "s", "i", "l", "c"))) stop("mispecified frequency. Check ?sampleby.")


  freq_names <- c("year", "month", "day", "hour", "minute", "second", "milli", "micro")
  renaming <- T
  while(renaming){
    if (any(freq_names %in% colnames(d))){
      freq_names %<>% paste0("0")
    } else {
      renaming <- F
    }
  }

  freq_types <- c("Y", "M", "d", "h", "m", "s", "mil", "mic")

  dico_freq <- 1:length(freq_types) %>% each(function(k) freq_names[1:k])
  names(dico_freq) <- freq_types


  if ((unparse(substitute(by))!="NULL") & (unparse(substitute(by)) %in% colnames(d))){
    time_col <- unparse(substitute(by))

    idx_freq <- length(dico_freq[[freq_char]])
    att_list <- c("year", "mon", "mday", "hour", "min", "sec")
    round_level <- c(rep(0,6),3,6)
    coeff_diff_time <- c(0,0,86400,3600,60,1,0.001,0.000001)

    if (is.null(start)) {

      if (idx_freq <= 2){
        r <- as.POSIXlt(d[1,time_col])[1,att_list[idx_freq]]%%freq_num
        start <- as.POSIXlt(d[1,time_col])
        start[1,att_list[idx_freq]] <- start[1,att_list[idx_freq]] - r

        if (idx_freq == 1){
          start[1,att_list[2]] <- 0
        }

        offset <- (start[1,att_list[3]]-1)*coeff_diff_time[3] + start[1,att_list[4]]*coeff_diff_time[4] + start[1,att_list[5]]*coeff_diff_time[5] +start[1,att_list[6]]*coeff_diff_time[6]

        start <- start - offset
      }

      if (idx_freq == 3){
        r <- as.POSIXlt(d[1,time_col])[1,att_list[idx_freq]]%%freq_num
        start <- as.POSIXlt(d[1,time_col])

        offset <- r*coeff_diff_time[3] + start[1,att_list[4]]*coeff_diff_time[4] + start[1,att_list[5]]*coeff_diff_time[5] +start[1,att_list[6]]*coeff_diff_time[6]

        start <- start - offset

      }

      if (idx_freq == 4){
        r <- as.POSIXlt(d[1,time_col])[1,att_list[idx_freq]]%%freq_num
        start <- as.POSIXlt(d[1,time_col])

        offset <- r*coeff_diff_time[4] + start[1,att_list[5]]*coeff_diff_time[5] +start[1,att_list[6]]*coeff_diff_time[6]

        start <- start - offset

      }

      if (idx_freq == 5){
        r <- as.POSIXlt(d[1,time_col])[1,att_list[idx_freq]]%%freq_num
        start <- as.POSIXlt(d[1,time_col])

        offset <- r*coeff_diff_time[5] +start[1,att_list[6]]*coeff_diff_time[6]

        start <- start - offset

      }


      if (idx_freq >= 6){
        r <- floor(as.POSIXlt(d[1,time_col])[1,att_list[idx_freq]]*10^(round_level[idx_freq]))%%freq_num
        start <- as.POSIXlt(d[1,time_col])
        start[1,att_list[idx_freq]] <- 10^(-round_level[idx_freq])*(floor(start[1,att_list[idx_freq]]*10^(round_level[idx_freq])) - r)
      }

    }

    start <- as.POSIXlt(start)

    d <- d[as.POSIXlt(d[,time_col]) >= start,]



    start_in_freq <- 0


    if (idx_freq == 1){
      start_in_freq <- start[1,att_list[1]]
      d[,dico_freq[[freq_char]][1]] <- d[,time_col] %>% each(function(x) floor(as.POSIXlt(x)[1,att_list[1]]) - start_in_freq) %>% unlist
    }

    if (idx_freq == 2){
      start_in_freq <- start[1,att_list[2]] + 12*start[1,att_list[1]]
      d[,dico_freq[[freq_char]][2]] <- d[,time_col] %>% each(function(x) floor(as.POSIXlt(x)[1,att_list[2]]+12*as.POSIXlt(x)[1,att_list[1]]) - start_in_freq) %>% unlist
    }



    if (idx_freq >= 3){

        d[,dico_freq[[freq_char]][idx_freq]] <- floor(difftime(as.POSIXlt(d[,time_col]), start, units= "secs")/coeff_diff_time[idx_freq])

    }

    d[,"bkt"] <- as.numeric(d[,dico_freq[[freq_char]][idx_freq]])%/%freq_num

    env <- parent.frame()

    d <- eval(substitute(select(d, ..., where = where2, by = bkt),
                    list(where2= substitute(where),
                         env= env)),
         enclos = env)

    att_list_posix <- c("year", "mon", "yday", "hour", "min", "sec")
    d[,time_col] <- d[,"bkt"] %>%
      each(
        function(k) {
          u <- start
          u[1,att_list_posix[min(idx_freq,6)]] <- u[1,att_list_posix[min(idx_freq,6)]] + k*freq_num*10^(-round_level[idx_freq])
          u %>% as.character
        }
      ) %>% unlist

    d$bkt <- NULL
    d[,dico_freq[[freq_char]][idx_freq]] <- NULL
    rownames(d) <- NULL
    d

  } else {
    stop("'by' argument must point to a valid column")
  }

}
