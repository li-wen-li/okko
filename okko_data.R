okko <- read.csv("~/Downloads/GuestCenter_guestexport_102043_2021-04-02_1252.csv")   ## from open table

okko_cust_phone <- okko[which(nchar(okko$Phone.Number) == 10 | nchar(okko$Phone.Number) == 12), ]  

clean_phone <- function(x) {
  x <- ifelse(stringr::str_detect(x, "\\+"), stringr::str_sub(x, 3,12),x) ## remove any +1 country code
  return(x)
}

okko_cust_phone$Phone.Number <- clean_phone(okko_cust_phone$Phone.Number)

okko_cust <- okko_cust_phone[c(1,8,14,15,17)]  ## keeping only customer name, phone number, first visit date, recent visit date and number of visits

okko_cust$area_code <- stringr::str_sub(okko_cust$Phone.Number, 1,3)  ## extra area code


area_by_state <- read.csv("~/Downloads/area_state.csv", header = FALSE) ## area code data for merging data



colnames(area_by_state) <- c("Country", "State", "Area Code", "N1", "N2","N3","N4")

area_by_state <- area_by_state[c(1:3)] ## keep only country and state, area code

area_by_state_us <- area_by_state[which(area_by_state$Country == "USA"), ] ## keeping only states within usa

area_by_state_us_nonservice <- area_by_state_us[which(nchar(area_by_state_us$State) > 0 ), ]  ## getting rid of special service phone number area code
colnames(area_by_state_us_nonservice) <- c("country","state","area_code")

final <- merge(okko_cust, area_by_state_us_nonservice,by = "area_code")  ##merging two dataframes


data_for_map <- data.frame(plyr::count(final,'state'))
data_for_map$rating <- -log(data_for_map$freq/sum(data_for_map$freq))


okko_map <- plot_usmap(data = data_for_map, values = 'rating', labels = TRUE)+
              scale_fill_continuous(name = "Visit", 
                        low = "red", high = "white",
                        label = scales::comma)+
              labs(title = "Origins of OKKO's Visitors", 
              caption = "Source: Liwen Li")
