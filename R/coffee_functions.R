fix_date <- . %>% 
  mutate(Date=ymd(Date))

check_info <- function(inf, consump){
  inf %>% 
    extract2("Date") %>% 
    max %>% 
    difftime(max(consump$Date),units="days") %>% 
    equals(0) %>% 
    not %>% 
    if(.) stop(message("did you update info?"))
}

calc_money_owed <- function(consump, inf){
  consump %>%
    left_join(inf) %>%
    mutate(owing=CostBlack * Coffee + Milk * CostMilk) %>%
    group_by(ID) %>% 
    summarise(owing_total = sum(owing, na.rm=TRUE))
}

calc_money_paid <- . %>%
  group_by(ID) %>%
  summarize(paid_total=sum(Payment))

calc_goods_bought <- . %>%
  group_by(ID) %>%
  summarize(GoodsCredit=sum(Cost))

do_accounts <- function(.money_owed, .money_paid, .people, .goods_bought){
  left_join(.money_owed, .money_paid) %>%
    rbind_list(anti_join(.money_paid, .money_owed)) %>%
    left_join(.people) %>% 
    filter(!Gone) %>%
    ## some of the remaining (not gone people) have not paid  
    mutate(paid_total_0 = ifelse(is.na(paid_total), 0, paid_total)) %>%
    left_join(.goods_bought) %>% 
    mutate(GoodsCredit_0=ifelse(is.na(GoodsCredit),0,GoodsCredit),
           owing_total_0=ifelse(is.na(owing_total),0,owing_total),
           balance=GoodsCredit_0+paid_total_0-owing_total_0,
           balance=ifelse(ID%in%c(18,214),0,balance)) %>%
    select(ID,Printed.Name,balance) %>% 
    mutate(Name=Printed.Name) %>% 
    arrange(Name)
}

## When did people last use the coop?

consumption_active_drink <- . %>%
  select(ID, Date) %>%
  group_by(ID) %>%
  summarise(lastday = max(Date)) %>%
  filter(lastday>(now()-dweeks(8)))

payments_active_money <- . %>%
  mutate(Date = ymd(data_date)) %>%
  select(ID, Date, data_date) %>% #filter(is.na(Date))
  group_by(ID) %>%
  summarise(lastday = max(Date)) %>%
  filter(lastday > (now() %>% subtract(dweeks(8))))

whois_active <- function(.active_drink, .active_money){
  rbind(.active_drink, .active_money) %>%
    select(ID) %>%
    unique
}

find_accounts_active <- function(.accounts, .active){
  semi_join(.accounts, .active) %>%
    arrange(Name)
}
