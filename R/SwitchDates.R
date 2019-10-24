switchDates = function(price, specificDate = NULL, WeekDay = NULL, Appearance = 1) {
  if (class(price)[1] != "xts") {
    stop("The data for 'price' have to be in the format 'xts'. Please check the R library 'xts' to convert the data.")
  }
  if (is.null(specificDate) & is.null(WeekDay)) {
    stop("Please chose either a 'specificDate' to search for or a 'WeekDay'.")
  }
    language = Sys.getenv("LANG")
    Sys.setenv(LANG = "en")
    all_days  = index(price)
    if (!is.null(specificDate)) {
        days_line = which(specificDate == day(all_days)) - 1
        dates = all_days[days_line]
    } else if (!is.null(WeekDay)) {
        MonthYear = cbind(month(all_days), year(all_days))
        SelectDay = c()
        for (i in 1:dim(MonthYear)[1]) {
            SelectDay[i] = getNthDayOfWeek(Appearance, WeekDay, MonthYear[i,1], MonthYear[i,2])
        }
        dates = as.Date(unique(SelectDay), origin = "1970-01-01")
    }
    Sys.setenv(LANG = language)
    return(dates)
}
