okko <- read.csv("~/Downloads/GuestCenter_guestexport_102043_2021-04-02_1252.csv")

okko_cust_phone <- okko[which(nchar(okko$Phone.Number) == 10 | nchar(okko$Phone.Number) == 12), ]

clean_phone <- function(x) {
  x <- ifelse(stringr::str_detect(x, "\\+"), stringr::str_sub(x, 3,12),x) ## remove any +1 country code
  return(x)
}

okko_cust_phone$Phone.Number <- clean_phone(okko_cust_phone$Phone.Number)


area_by_state <- read.csv("~/Downloads/area_state.csv", header = FALSE)



colnames(area_by_state) <- c("Country", "State", "Area Code", "N1", "N2","N3","N4")

area_by_state <- area_by_state[c(1:3)] ## keep only country and state, area code

area_by_state_us <- area_by_state[which(area_by_state$Country == "USA"), ] ## keeping only states within usa


