cleanPersonalityData_tidy = function(dat)
{
  test = dat %>%
    mutate(date_test = as.Date(dat$date_test, "%m/%d/%Y %H:%M")) %>% # create as a date
    arrange(desc(date_test)) %>%  # arrange by date
    mutate(V00 = lubridate::week(date_test)) %>%  # date to week
    mutate(date_test = lubridate:: year(date_test)) %>% # date to year
    rename(., year = date_test) %>% # rename year
    rename(. , week = V00) %>%  # rename week
    distinct(md5_email, .keep_all = T ) # find unique values by date
  return(test)
  
}

