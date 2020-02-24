# get required packages
check_package <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

# usage
# check_package(c("package1", "package2"))

# return last or nth to last value of a list
get_last <- function(x, n){
    x %>% map(str_split, ' ') %>% # split the string
    map(1) %>% # unnest the string
    map_dfr(~ tibble(last = .x[[length(.x) - n + 1]])) # grab the last nth value
}

# usage
# my_list <- list(one = "1 sep 2014", two = "1-2 sep 2015", three = "1 - 2 sept 2016")
# get_last(my_list, 1) # gets the year
# get_last(my_list, 2) # gets the month
