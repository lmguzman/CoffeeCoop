library(magrittr)

add_donation <- function(consump, donation, multi){
  donation %>%
    mutate(Coffee = multi*Coffee, Milk = multi*Milk) %>%
    bind_rows(consump)
}

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

calc_money_owed <- function(consump, inf, extra){
  consump %>%
    left_join(inf) %>%
    mutate(owing=CostBlack * Coffee + Milk * CostMilk) %>%
    group_by(ID) %>% 
    summarise(owing_total = sum(owing, na.rm=TRUE)) %>%
    left_join(extra) %>%
    mutate(final_owing_0 = ifelse(is.na(extra_donated), 0, extra_donated)) %>%
    mutate(final_owing = owing_total + final_owing_0)  
}

calc_money_paid <- . %>%
  group_by(ID) %>%
  summarize(paid_total=sum(Payment))

calc_goods_bought <- . %>%
  group_by(ID) %>%
  summarize(GoodsCredit=sum(Cost))

do_accounts <- function(.money_owed, .money_paid, .people, .goods_bought){
  left_join(.money_owed, .money_paid) %>%
    bind_rows(anti_join(.money_paid, .money_owed)) %>%
    left_join(.people) %>% 
    filter(!Gone) %>%
    ## some of the remaining (not gone people) have not paid  
    mutate(paid_total_0 = ifelse(is.na(paid_total), 0, paid_total)) %>%
    left_join(.goods_bought) %>% 
    mutate(GoodsCredit_0 = ifelse(is.na(GoodsCredit), 0, GoodsCredit),
           owing_total_0 = ifelse(is.na(final_owing), 0, final_owing),
           balance = GoodsCredit_0 + paid_total_0 - owing_total_0,
           balance = ifelse(ID %in% c(18, 214,297), 0, balance),
           balance = round(balance, 2)) %>%
    select(ID, Printed.Name, balance) %>% 
    mutate(Name = Printed.Name) %>% 
    arrange(Name)
}

calculate_donations <- function(donation, inf){
  donation %>%
    left_join(inf) %>%
    mutate(collected=CostBlack * Coffee + Milk * CostMilk) %>%
    group_by(Date) %>%
    summarise(collected_total = sum(collected, na.rm=TRUE), coffees_drank = sum(Coffee))
}

# Identifying active users ------------------------------------------------

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


# formatting Signup Sheet -------------------------------------------------

format_accounts <- . %>%
  mutate(balance_text = sprintf("%.2f", balance),
         balance_text_format = ifelse(balance < 0,
                                      paste0("\\textbf{", balance_text, "}"),
                                      balance_text),
         Name = ifelse(balance < 0,
                       paste0("\\textbf{", Name, "}"),
                       Name)) %>%
  mutate(Coffee="", Milk="") %>%
  select(Name, balance_text_format, Coffee, Milk, ID) %>%
  set_names(c("Name","balance","\\textbf{Coffee}","\\textbf{Milk}","ID"))

print_signup_sheet <- function(.accounts_formatted, output){
  sink()
  sink(output)
  xtable(.accounts_formatted,
         align = c("|","l","|","p{5cm}","|","r","|","p{9cm}","|","p{6cm}","|","l","|")
  ) %>%
    print(type = 'latex', sanitize.text.function = identity,
          tabular.environment = "longtable", 
          hline.after = 1:nrow(.accounts_formatted),  # note we count the lines here.
          floating = FALSE, include.rownames=FALSE,
          add.to.row = list(pos = list(0),
                            command = c("\\hline \\endhead ")),
          comment = FALSE) %>%
    cat
  sink()
}
  