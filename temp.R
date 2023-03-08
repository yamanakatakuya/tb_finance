csv_datestamp <- '2023-02-28'

# Function to read in timestamped CSV file and to undo Philippe's conversion of underscores to dots
get_timestamped_csv <- function(csv_filename_stub, timestamp) {
  df <- read.csv(here(paste0('csv/', csv_filename_stub, '_', timestamp,'.csv')))
  names(df) <- gsub('[.]', '_', names(df))
  return(df)
}

# Load data

budget_prev <- get_timestamped_csv('TB_budget', csv_datestamp)
expend_prev <- get_timestamped_csv('TB_expenditure_utilisation', csv_datestamp)

budget_temp <- get_timestamped_csv('latest_budgets', csv_datestamp) %>% mutate(year=2023) # dummy
expend_temp <- get_timestamped_csv('latest_expenditures_services', csv_datestamp) %>% mutate(year=2022) # dummy

notif_prev  <- get_timestamped_csv('TB_notifications', csv_datestamp) %>%
  filter(year>2017) %>%
  select(country,iso3,year,
         ret_nrel, c_newinc,
         conf_rrmdr_tx,unconf_mdr_tx,conf_mdr_tx,conf_xdr_tx,unconf_rr_nfqr_tx,conf_rr_nfqr_tx,conf_rr_fqr_tx
  ) %>%
  rowwise() %>% 
  mutate(c_notified = sum (ret_nrel, c_newinc, na.rm=T),
         c_rrmdr_tx = sum (conf_rrmdr_tx,unconf_mdr_tx,conf_mdr_tx, unconf_rr_nfqr_tx,conf_rr_nfqr_tx,na.rm=T),
         c_xdr_tx = sum (conf_xdr_tx,conf_rr_fqr_tx,na.rm=T))

notif_temp  <- get_timestamped_csv('latest_notifications', csv_datestamp) %>%
  mutate(year=2022) %>% # dummy
  select(country,iso2,year,
         c_notified,
         c_rrmdr_tx = conf_rr_nfqr_tx,
         c_xdr_tx = conf_rr_fqr_tx
  )

budget <- plyr::rbind.fill(budget_prev,budget_temp) %>% arrange(country)
expend <- plyr::rbind.fill(expend_prev,expend_temp) %>% arrange(country)
notif  <- plyr::rbind.fill(notif_prev,notif_temp) %>%
  select(country,year,c_notified,c_rrmdr_tx,c_xdr_tx) %>%
  arrange(country)


# Plot data
#-- tab 1
## budget plot1: budget by line
df_b1 <- budget %>%
  select(country,iso3,year,
         budget_fld,
         budget_staff,
         budget_prog,
         budget_lab,
         budget_tbhiv,
         budget_sld,
         budget_mdrmgt,
         budget_orsrvy,
         budget_patsup,
         budget_tpt,
         budget_oth
  ) %>%
  mutate(year=as.factor(year)) %>%
  rename('1st-line drugs'=4,Staff=5,'Programme management'=6,Lab=7,'TB/HIV'=8,'2nd-line drugs'=9,'MDR management'=10,
         'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14) %>%
  filter(country=="Afghanistan")

plot_b1 <- df_b1 %>% 
  e_charts(year) %>%
  e_bar(`1st-line drugs`, stack = "stack") %>%
  e_bar(`Staff`, stack = "stack") %>%
  e_bar(`Programme management`, stack = "stack") %>%
  e_bar(`Lab`, stack = "stack") %>%
  e_bar(`TB/HIV`, stack = "stack") %>%
  e_bar(`2nd-line drugs`, stack = "stack") %>%
  e_bar(`MDR management`, stack = "stack") %>%
  e_bar(`Research/surveys`, stack = "stack") %>%
  e_bar(`Patient support`, stack = "stack") %>%
  e_bar(`Preventive drugs`, stack = "stack") %>%
  e_bar(`Other`, stack = "stack") %>%
  e_grid(containLabel = T) %>%
  e_legend(show = T, position = 'insideTop') %>% 
  e_title(text = paste0(": budget by line"))   %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))


## budget plot2: budget by source
df_b2 <- budget %>%
  select(country,iso3,year,
         `Domestic` = cf_tot_domestic ,
         `Global Fund` = cf_tot_gf,
         `USAID` =  cf_tot_usaid,
         `Other external` =  cf_tot_grnt,
         cf_tot,
         `Total budget required` = budget_tot
  ) %>%
  mutate(year=as.factor(year),
         Gap = `Total budget required` - cf_tot) %>%
  filter(country=="Afghanistan")

plot_b2 <- df_b2 %>% 
  e_charts(year) %>%
  e_bar(`Domestic`, stack = "stack") %>%
  e_bar(`Global Fund`, stack = "stack") %>%
  e_bar(`USAID`, stack = "stack") %>%
  e_bar(`Other external`, stack = "stack") %>%
  e_bar(`Gap`, stack = "stack") %>%
  e_grid(containLabel = T) %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))

## budget plot3: total budget
plot_b3 <- df_b2 %>% 
  e_charts(year) %>%
  e_bar(`Total budget required`) %>%
  e_grid(containLabel = T) %>%
    e_tooltip(
      trigger = "item",
      axisPointer = list(
        type = "shadow"
      )
    )


## budget plot4: Notifications and expected cases
df_b4 <- notif %>%
  full_join(select(budget, country, year, tx_dstb, tx_mdr, tx_xdr), by = c("country","year")) %>%
  mutate(year=as.factor(year)) %>%
  arrange(country) %>%
  filter(country=="Afghanistan")


plot_b4 <- df_b4 %>% 
  e_charts(year) %>%
  e_line(c_notified, name = "All forms of TB Notified") %>%
  e_line(tx_dstb, name = "Expected # of DS-TB starting treatment") %>%
  e_line(c_rrmdr_tx, name = "Patients started on treatment - MDR/RR-TB") %>%
  e_line(tx_mdr, name = "Expected # of MDR/RR-TB starting treatment") %>%
  e_line(c_xdr_tx, name = "Patients started on treatment - pre-XDR-TB or XDR-TB") %>%
  e_line(tx_xdr, name = "Expected # of pre-XDR-TB or XDR-TB starting treatment") %>%
  e_grid(containLabel = T) %>%
  e_y_axis(type = 'log') %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))


#-- tab 2
## expenditure plot1: expenditure by line
df_e1 <- expend %>%
  select(country,iso3,year,
         exp_fld,
         exp_staff,
         exp_prog,
         exp_lab,
         exp_tbhiv,
         exp_sld,
         exp_mdrmgt,
         exp_orsrvy,
         exp_patsup,
         exp_tpt,
         exp_oth
  ) %>%
  mutate(year=as.factor(year)) %>%
  rename('1st-line drugs'=4,Staff=5,'Programme management'=6,Lab=7,'TB/HIV'=8,'2nd-line drugs'=9,'MDR management'=10,
         'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14) %>%
  filter(country=="Afghanistan")

plot_e1 <- df_e1 %>% 
  e_charts(year) %>%
  e_bar(`1st-line drugs`, stack = "stack") %>%
  e_bar(`Staff`, stack = "stack") %>%
  e_bar(`Programme management`, stack = "stack") %>%
  e_bar(`Lab`, stack = "stack") %>%
  e_bar(`TB/HIV`, stack = "stack") %>%
  e_bar(`2nd-line drugs`, stack = "stack") %>%
  e_bar(`MDR management`, stack = "stack") %>%
  e_bar(`Research/surveys`, stack = "stack") %>%
  e_bar(`Patient support`, stack = "stack") %>%
  e_bar(`Preventive drugs`, stack = "stack") %>%
  e_bar(`Other`, stack = "stack") %>%
  e_grid(containLabel = T) %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))


## expenditure plot2: received by line
df_e2 <- expend %>%
  select(country,iso3,year,
         rcvd_fld,
         rcvd_staff,
         rcvd_prog,
         rcvd_lab,
         rcvd_tbhiv,
         rcvd_sld,
         rcvd_mdrmgt,
         rcvd_orsrvy,
         rcvd_patsup,
         rcvd_tpt,
         rcvd_oth
  ) %>%
  mutate(year=as.factor(year)) %>%
  rename('1st-line drugs'=4,Staff=5,'Programme management'=6,Lab=7,'TB/HIV'=8,'2nd-line drugs'=9,'MDR management'=10,
         'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14) %>%
  filter(country=="Afghanistan")

plot_e2 <- df_e2 %>% 
  e_charts(year) %>%
  e_bar(`1st-line drugs`, stack = "stack") %>%
  e_bar(`Staff`, stack = "stack") %>%
  e_bar(`Programme management`, stack = "stack") %>%
  e_bar(`Lab`, stack = "stack") %>%
  e_bar(`TB/HIV`, stack = "stack") %>%
  e_bar(`2nd-line drugs`, stack = "stack") %>%
  e_bar(`MDR management`, stack = "stack") %>%
  e_bar(`Research/surveys`, stack = "stack") %>%
  e_bar(`Patient support`, stack = "stack") %>%
  e_bar(`Preventive drugs`, stack = "stack") %>%
  e_bar(`Other`, stack = "stack") %>%
  e_grid(containLabel = T) %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))


## expenditure plot3: received by source
df_e3 <- expend %>%
  select(country,iso3,year,
         `Domestic` = rcvd_tot_domestic ,
         `Global Fund` = rcvd_tot_gf,
         `USAID` =  rcvd_tot_usaid,
         `Other external` =  rcvd_tot_grnt,
         `Received total` =  rcvd_tot,
         `Expended total` =  exp_tot
  ) %>%
  mutate(year=as.factor(year),
         Percentage = `Expended total`/`Received total`) %>%
  filter(country=="Afghanistan")

plot_e3 <- df_e3 %>% 
  e_charts(year) %>%
  e_bar(`Domestic`, stack = "stack") %>%
  e_bar(`Global Fund`, stack = "stack") %>%
  e_bar(`USAID`, stack = "stack") %>%
  e_bar(`Other external`, stack = "stack") %>%
  e_grid(containLabel = T) %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))

## expenditure plot4: Total received and expended

plot_e4 <- df_e3 %>% 
  e_charts(year) %>%
  e_bar(`Received total`) %>%
  e_bar(`Expended total`) %>%
  e_line(Percentage, y_index = 1) %>%
  e_y_axis(
    index = 1,
    axisLabel = list(formatter = e_axis_formatter(style = "percent", locale = "ru"))
  ) %>%
  e_grid(containLabel = T) %>%
  e_tooltip(trigger="item",
            textStyle=list(fontFamily="arial", fontSize=12))





df_g2 <- budget %>%
  rowwise() %>%
  mutate(gap_fld = sum (budget_fld, cf_fld*(-1), na.rm = T),
         gap_staff = sum (budget_staff, cf_staff*(-1), na.rm = T),
         gap_prog = sum (budget_prog, cf_prog*(-1), na.rm = T),
         gap_lab = sum (budget_lab, cf_lab*(-1), na.rm = T),
         gap_tbhiv = sum (budget_tbhiv, cf_tbhiv*(-1), na.rm = T),
         gap_sld = sum (budget_sld, cf_sld*(-1), na.rm = T),
         gap_mdrmgt = sum (budget_mdrmgt, cf_mdrmgt*(-1), na.rm = T),
         gap_orsrvy = sum (budget_orsrvy, cf_orsrvy*(-1), na.rm = T),
         gap_patsup = sum (budget_patsup, cf_patsup*(-1), na.rm = T),
         gap_tpt = sum (budget_tpt, cf_tpt*(-1), na.rm = T),
         gap_oth = sum (budget_oth, cf_oth*(-1), na.rm = T)
         ) %>%
  select(country,iso3,year,
         gap_fld,
         gap_staff,
         gap_prog,
         gap_lab,
         gap_tbhiv,
         gap_sld,
         gap_mdrmgt,
         gap_orsrvy,
         gap_patsup,
         gap_tpt,
         gap_oth
  ) %>%
  mutate(year=as.factor(year)) %>%
  rename('1st-line drugs'=4,Staff=5,'Programme management'=6,Lab=7,'TB/HIV'=8,'2nd-line drugs'=9,'MDR management'=10,
         'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14) %>%
  filter(country=="Afghanistan")

output$gap_plot2 <- renderEcharts4r({
  
  plot_g2 <- df_g2() %>% 
    
    e_charts(year) %>%
    e_bar(`1st-line drugs`, stack = "stack") %>%
    e_bar(`Staff`, stack = "stack") %>%
    e_bar(`Programme management`, stack = "stack") %>%
    e_bar(`Lab`, stack = "stack") %>%
    e_bar(`TB/HIV`, stack = "stack") %>%
    e_bar(`2nd-line drugs`, stack = "stack") %>%
    e_bar(`MDR management`, stack = "stack") %>%
    e_bar(`Research/surveys`, stack = "stack") %>%
    e_bar(`Patient support`, stack = "stack") %>%
    e_bar(`Preventive drugs`, stack = "stack") %>%
    e_bar(`Other`, stack = "stack") %>%
    e_grid(containLabel = T) %>%
    e_tooltip(trigger="item",
              textStyle=list(fontFamily="arial", fontSize=12))
  
  plot_g2
