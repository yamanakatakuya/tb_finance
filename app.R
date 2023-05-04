# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic cascade, treatment success rate, and number
# of labs for a group of countries and for a country using JSON data retrieved 
# from the WHO global tuberculosis database.
# Takuya Yamanaka, June 2022
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.1"
library("shiny")
library("glue")
library("rlang")
library("shiny")
library("sourcetools")
library("dplyr")
library("ggplot2")
library("readxl")
library(shinythemes)
library("cowplot")
library(jsonlite)
library(RColorBrewer)
library(gtbreport)
library(highcharter)
library(tidyr)
library(ggpubr)
library(writexl)
library(ggrepel)
library(echarts4r)
library(here)

report_year <- 2022
csv_datestamp <- '2023-02-28'
csv_datestamp2 <- '2023-05-04' # change the date everytime 
col <-  RColorBrewer::brewer.pal(12,"Paired")

# Function to read in timestamped CSV file and to undo Philippe's conversion of underscores to dots
get_timestamped_csv <- function(csv_filename_stub, timestamp) {
  df <- read.csv(here(paste0('csv/', csv_filename_stub, '_', timestamp,'.csv')))
  names(df) <- gsub('[.]', '_', names(df))
  return(df)
}
NZ <- function(x) {
  ifelse(is.na(x),
         0,
         x)
}

# Load data

finance_prev <- haven::read_stata("./csv/finance2022_13MAR2023.dta") %>%
  mutate(cf_tpt=NA)

# budget_prev <- get_timestamped_csv('TB_budget', csv_datestamp)
# expend_prev <- get_timestamped_csv('TB_expenditure_utilisation', csv_datestamp)

budget_temp <- get_timestamped_csv('latest_budgets', csv_datestamp2) 
expend_temp <- get_timestamped_csv('latest_expenditures_services', csv_datestamp2) 

notif_prev  <- get_timestamped_csv('TB_notifications', csv_datestamp) %>%
  filter(year>2005) %>%
  select(country,iso2,year,
         ret_nrel, c_newinc,
         conf_rrmdr_tx,unconf_mdr_tx,conf_mdr_tx,conf_xdr_tx,unconf_rr_nfqr_tx,conf_rr_nfqr_tx,conf_rr_fqr_tx
  ) %>%
  rowwise() %>% 
  mutate(c_notified = sum (ret_nrel, c_newinc, na.rm=T),
         c_rrmdr_tx = sum (conf_rrmdr_tx,unconf_mdr_tx,conf_mdr_tx, unconf_rr_nfqr_tx,conf_rr_nfqr_tx,na.rm=T),
         c_dstb_tx = c_notified - c_rrmdr_tx,
         c_xdr_tx = sum (conf_xdr_tx,conf_rr_fqr_tx,na.rm=T))

notif_temp  <- get_timestamped_csv('latest_notifications', csv_datestamp2) %>%
  select(country,iso2,year,
         c_notified,
         c_rrmdr_tx = conf_rr_nfqr_tx,
         c_xdr_tx = conf_rr_fqr_tx
  ) %>%
  mutate(c_dstb_tx = c_notified - c_rrmdr_tx)

tpt_prev <- get_timestamped_csv('TB_contact_tpt', csv_datestamp)
tpt_temp <- get_timestamped_csv('latest_strategy', csv_datestamp2) 


budget <- plyr::rbind.fill(finance_prev,budget_temp) %>% arrange(country) %>% filter(year>2005)
expend <- plyr::rbind.fill(filter(finance_prev,year<report_year),expend_temp) %>% arrange(country) %>% filter(year>2005)
notif  <- plyr::rbind.fill(notif_prev,notif_temp) %>%
  select(country,year,c_notified,c_dstb_tx,c_rrmdr_tx,c_xdr_tx) %>%
  arrange(country)
tpt <- plyr::rbind.fill(tpt_prev,tpt_temp) %>% arrange(country)

# common_cols <- intersect(colnames(budget), colnames(finance_prev))
# 
# budget <- rbind(
#   subset(budget, select = common_cols), 
#   subset(finance_prev, year>2009, select = common_cols)
# ) %>% arrange(country, year)
# 
# common_cols <- intersect(colnames(expend), colnames(finance_prev))
# 
# expend <- rbind(
#   subset(expend, select = common_cols), 
#   subset(finance_prev, year>2009, select = common_cols)
# ) %>% arrange(country, year)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- 
  navbarPage(
    "TB Finance Data Review - Cleaned data!",
    tabPanel(
      "Budget",
      # --------------------- Global, WHO regions, HBCs, GF etc ---------------------#
      fluidPage(theme = shinytheme("sandstone"),
        # fluidRow(
        #   column(width = 6, 
        #          tags$div(style = "padding-left: 20px;"),
        #          downloadButton('dl_fig_bud', 'Download (figures)')
        #          ),      
        #   column(width = 6,
        #          tags$div(style = "padding-left: 20px;"),
        #          downloadButton('dl_dat_bud', 'Download (data)')
        #   )),      
        # 
        # br(),
        

        
        fluidRow(tags$div(id = "page_header",
                          HTML("Select a country"),
                          uiOutput(outputId = "country"))
        ),
        
        br(),
        
        fluidRow(tags$div(id = "date_notice",
                          HTML(paste("Data are provisional as of",csv_datestamp2)))
        ),     
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("b1_heading", container = h3),
                 echarts4rOutput("budget_plot1",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("b2_heading", container = h3),
                 echarts4rOutput("budget_plot2",height=600)
          )),
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("b3_heading", container = h3),
                 echarts4rOutput("budget_plot3",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("b4_heading", container = h3),
                 echarts4rOutput("budget_plot4",height=600))
        

      )
    )
    ),
    
    tabPanel(
      "Funding gap",
      # --------------------- gap tab ---------------------#
      fluidPage(
 
        fluidRow(tags$div(id = "date_notice2",
                          HTML(paste("Data are provisional as of",csv_datestamp2)))
        ),    
        
        br(),
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("g1_heading", container = h3),
                 echarts4rOutput("gap_plot1",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("g2_heading", container = h3),
                 echarts4rOutput("gap_plot2",height=600)
          )),
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("g3_heading", container = h3),
                 echarts4rOutput("gap_plot3",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("g4_heading", container = h3),
                 echarts4rOutput("gap_plot4",height=600))
          
          
        )
        
        
      )),
    
    
    tabPanel(
      "Expenditure",
      # --------------------- Expenditure tab ---------------------#
      fluidPage(


        fluidRow(tags$div(id = "date_notice3",
                          HTML(paste("Data are provisional as of",csv_datestamp2)))
        ),     
        br(),
        
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("e1_heading", container = h3),
                 echarts4rOutput("expend_plot1",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("e2_heading", container = h3),
                 echarts4rOutput("expend_plot2",height=600)
          )),
        
        fluidRow(
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("e3_heading", container = h3),
                 echarts4rOutput("expend_plot3",height=600)
          ),
          column(width = 6,
                 tags$div(style = "padding-left: 20px;"),
                 textOutput("e4_heading", container = h3),
                 echarts4rOutput("expend_plot4",height=600))
          
          
        )
        
        
        )),
    
    tabPanel(
      "Drug cost per patient",
      # --------------------- Drug cost tab ---------------------#
      fluidPage(     
        fluidRow(tags$div(id = "date_notice4",
                        HTML(paste("Data are provisional as of",csv_datestamp2)))
      ),     
      br(),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d1_heading", container = h3),
               textOutput("d1_subheading", container = h3),
               echarts4rOutput("drug_plot1",height=600)
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d2_heading", container = h3),
               textOutput("d2_subheading", container = h3),
               echarts4rOutput("drug_plot2",height=600)
        )),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d3_heading", container = h3),
               textOutput("d3_subheading", container = h3),
               echarts4rOutput("drug_plot3",height=600)
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d4_heading", container = h3),
               textOutput("d4_subheading", container = h3),
               echarts4rOutput("drug_plot4",height=600)
               )
        ),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d5_heading", container = h3),
               textOutput("d5_subheading", container = h3),
               echarts4rOutput("drug_plot5",height=600)
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("d6_heading", container = h3),
               textOutput("d6_subheading", container = h3),
               echarts4rOutput("drug_plot6",height=600)
        )
      )
      
      )),
    
    tabPanel(
      "Utilization",
      # --------------------- Service utilization tab ---------------------#
      fluidPage(         
      fluidRow(tags$div(id = "date_notice5",
                        HTML(paste("Data are provisional as of",csv_datestamp2)))
      ),     
      br(),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("u1_heading", container = h3),
               echarts4rOutput("util_plot1",height=600)
        ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("u2_heading", container = h3),
               echarts4rOutput("util_plot2",height=600)
        )),
      
      fluidRow(
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput("u3_heading", container = h3),
               echarts4rOutput("util_plot3",height=600)
        )
      ),
      
    )),
    

  )



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {
  
  json_url <- "https://extranet.who.int/tme/generateJSON.asp"

# --------------------- Country ---------------------#
  
  # Get the latest list of countries with provisional data to use in country dropdown
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  country_list_json <- reactive({
    
    url <- paste0(json_url, "?ds=countries")
    
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
  })
  
  # Build the select country control
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$country <- renderUI({
    
    already_selected <- input$iso2
    
    # Create a named list for selectInput
    country_list <- country_list_json()$countries %>%
      select(iso2, country) %>%
      arrange(country)
    
    country_list <- setNames(country_list[,"iso2"], country_list[,"country"])
    
    selectInput(inputId = "iso2",
                label = "",
                choices = country_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })
  
  selected_country <- reactive({
    selected_country <- country_list_json()$countries %>%
      filter(iso2 == input$iso2)
  })

  #-- tab 1
  ## budget plot1: budget by line
  output$b1_heading <- renderText({ 
    paste("Fig.1: Budget by line")
  })
  
  
  df_b1 <- reactive({
    
  df_b1 <- budget %>%
    select(country,iso2,year,
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
    filter(country==selected_country()$country)
  })
  
  output$budget_plot1 <- renderEcharts4r({
  
    plot_b1 <- df_b1() %>% 
    
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
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
      )
    
    plot_b1
    
  })
  
  ## budget plot2: budget by source
  output$b2_heading <- renderText({ 
    paste("Fig.2: Budget by source")
  })
  
  df_b2 <- reactive({
    
    df_b2 <- budget %>%
      select(country,iso2,year,
           `Domestic` = cf_tot_domestic ,
           `Global Fund` = cf_tot_gf,
           `USAID` =  cf_tot_usaid,
           `Other external` =  cf_tot_grnt,
           cf_tot,
           `Total budget required` = budget_tot
           ) %>%
      mutate(year=as.factor(year),
           Gap = `Total budget required` - cf_tot)  %>%
      filter(country==selected_country()$country)
  })
  
  output$budget_plot2 <- renderEcharts4r({
    
  plot_b2 <- df_b2() %>% 
    e_charts(year) %>%
    e_bar(`Domestic`, stack = "stack") %>%
    e_bar(`Global Fund`, stack = "stack") %>%
    e_bar(`USAID`, stack = "stack") %>%
    e_bar(`Other external`, stack = "stack") %>%
    e_bar(`Gap`, stack = "stack") %>%
    e_grid(containLabel = T) %>%
    e_tooltip(trigger="item",
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col) %>%
    e_y_axis(
      name = "US$",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    )
  
  
  plot_b2
  
  })
  
  ## budget plot3: total budget
  output$b3_heading <- renderText({ 
    paste("Fig.3: Total budget required")
  })
  
  output$budget_plot3 <- renderEcharts4r({
    
    plot_b3 <- df_b2() %>% 
    e_charts(year) %>%
    e_bar(`Total budget required`) %>%
    e_grid(containLabel = T) %>%
    e_tooltip(
      trigger = "item",
      axisPointer = list(
        type = "shadow"
      )
    ) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
      )
  
    plot_b3
    
  })
  
  ## budget plot4: Notifications and expected cases
  output$b4_heading <- renderText({ 
    paste("Fig.4: Notifications (solid line) and expected cases (dashed line)")
  })
  
  df_b4 <- reactive({
    
    df_b4 <- notif %>%
    full_join(select(budget, country, year, tx_dstb, tx_mdr, tx_xdr), by = c("country","year")) %>%
    mutate(year=as.factor(year)) %>%
    arrange(country)  %>% 
      mutate(across(everything(), ~replace(., . ==  0 , NA))) %>%
    filter(country==selected_country()$country)
    
    })

  output$budget_plot4 <- renderEcharts4r({
    
  plot_b4 <- df_b4() %>% 
    e_charts(year) %>%
    e_line(c_dstb_tx, name = "DS-TB notified") %>%
    e_line(tx_dstb, name = "DS-TB starting tx (expected)", lineStyle = list(type = "dashed")) %>%
    e_line(c_rrmdr_tx, name = "MDR/RR-TB started tx") %>%
    e_line(tx_mdr, name = "MDR/RR-TB starting tx (expected)", lineStyle = list(type = "dashed")) %>%
    e_line(c_xdr_tx, name = "(Pre-)XDR-TB started tx") %>%
    e_line(tx_xdr, name = "(pre-)XDR-TB starting tx (expected)", lineStyle = list(type = "dashed")) %>%
    e_grid(containLabel = T) %>%
    e_y_axis(type = 'log') %>%
    e_tooltip(trigger="item",
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col) %>%
    e_y_axis(
      name = "Cases per year (log scale)",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    )
  
  plot_b4
  
  })
  
  #-- tab 2
  ## expenditure plot1: expenditure by line
  output$e1_heading <- renderText({ 
    paste("Fig.1: Expenditure by line")
  })
  
  df_e1 <- reactive({
    
  df_e1 <- expend %>%
    select(country,iso2,year,
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
           'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14)%>%
    filter(country==selected_country()$country)
  
  })
  
  output$expend_plot1 <- renderEcharts4r({
  
  plot_e1 <- df_e1() %>% 
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
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col) %>%
    e_y_axis(
      name = "US$",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    )
  
  })
  
  ## expenditure plot2: received by line
  output$e2_heading <- renderText({ 
    paste("Fig.2: Received by line")
  })
  
  df_e2 <- reactive({

    df_e2 <- expend %>%
    select(country,iso2,year,
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
    filter(country==selected_country()$country)
  })  
  
  output$expend_plot2 <- renderEcharts4r({
    
  plot_e2 <- df_e2() %>% 
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
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col) %>%
    e_y_axis(
      name = "US$",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    )
  
  }) 
  
  ## expenditure plot3: received by source
  output$e3_heading <- renderText({ 
    paste("Fig.3: Received by source")
  })
  
  df_e3 <- reactive({
    
    df_e3 <- expend %>%
    select(country,iso2,year,
           `Domestic` = rcvd_tot_domestic ,
           `Global Fund` = rcvd_tot_gf,
           `USAID` =  rcvd_tot_usaid,
           `Other external` =  rcvd_tot_grnt,
           `Received total` =  rcvd_tot,
           `Expended total` =  exp_tot
    ) %>%
    mutate(year=as.factor(year),
           `Percentage of expended to received (right axis)` = round(`Expended total`/`Received total`*100),0) %>%
    filter(country==selected_country()$country)
    
  }) 
  
  col3 <- c("#A6CEE3","#33A02C","limegreen","#B2DF8A") 
  
  output$expend_plot3 <- renderEcharts4r({
  plot_e3 <- df_e3() %>% 
    e_charts(year) %>%
    e_bar(`Domestic`, stack = "stack") %>%
    e_bar(`Global Fund`, stack = "stack") %>%
    e_bar(`USAID`, stack = "stack") %>%
    e_bar(`Other external`, stack = "stack") %>%
    e_grid(containLabel = T) %>%
    e_tooltip(trigger="item",
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col3) %>%
    e_y_axis(
      name = "US$",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    )
  
  })
  
  ## expenditure plot4: Total received and expended
  output$e4_heading <- renderText({ 
    paste("Fig.4: Total received and expended")
  })
  
  col2 <- c("#A6CEE3","#1F78B4","#E31A1C") 
  
  output$expend_plot4 <- renderEcharts4r({
  plot_e4 <- df_e3() %>% 
    e_charts(year) %>%
    e_bar(`Received total`) %>%
    e_bar(`Expended total`) %>%
    e_line(`Percentage of expended to received (right axis)`, y_index = 1)  %>%
    
    e_y_axis(
      name = "US$",
      nameLocation = "middle",
      nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
    ) %>%
    
    e_y_axis(
      index = 1,
      name = "Percentage",
      nameLocation = "middle",
      axisLabel = list(formatter = e_axis_formatter(style = "decimal", locale = "ru")),
      nameTextStyle = list(fontSize = 18, padding = c(40, 0, 60, 0))
    ) %>%
    e_grid(containLabel = T) %>%
    e_tooltip(trigger="item",
              textStyle=list(fontFamily="arial", fontSize=12)) %>%
    e_color(col2)
  
  })

  #-- tab 3: Gap
  ## gap plot1: expected funding by line
  output$g1_heading <- renderText({ 
    paste("Fig.1: Expected funding by line")
  })
  
  
  df_g1 <- reactive({
    
    df_g1 <- budget %>%
      select(country,iso2,year,
             cf_fld,
             cf_staff,
             cf_prog,
             cf_lab,
             cf_tbhiv,
             cf_sld,
             cf_mdrmgt,
             cf_orsrvy,
             cf_patsup,
             cf_tpt,
             cf_oth
      ) %>%
      mutate(year=as.factor(year)) %>%
      rename('1st-line drugs'=4,Staff=5,'Programme management'=6,Lab=7,'TB/HIV'=8,'2nd-line drugs'=9,'MDR management'=10,
             'Research/surveys'=11,'Patient support'=12,'Preventive drugs'=13,'Other'=14) %>%
      filter(country==selected_country()$country)
  })
  
  output$gap_plot1 <- renderEcharts4r({
    
    plot_g1 <- df_g1() %>% 
      
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
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
      )
    
    plot_g1
    
  })

  ## gap plot2: Gap by line
  output$g2_heading <- renderText({ 
    paste("Fig.2: Funding gap by line")
  })
  
  
  df_g2 <- reactive({
    
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
      select(country,iso2,year,
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
      filter(country==selected_country()$country)
  })
  
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
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
      )
    
    plot_g2
    
  })
  
  ## expenditure plot3: Total budget and expected funding
  output$g3_heading <- renderText({ 
    paste("Fig.3: Total budget and expected funding")
  })
  
  df_g3 <- reactive({
    
    df_g3 <- budget %>%
      select(country,iso2,year,
             `Total budget` = budget_tot ,
             `Total expected funding` = cf_tot
      ) %>%
      mutate(year=as.factor(year),
             `Funding gap` = `Total budget`-`Total expected funding`) %>%
      filter(country==selected_country()$country)
    
  }) 
  
  col2 <- c("#A6CEE3","#1F78B4","#E31A1C") 
  
  output$gap_plot3 <- renderEcharts4r({
    plot_g3 <- df_g3() %>% 
      e_charts(year) %>%
      e_bar(`Total budget`) %>%
      e_bar(`Total expected funding`) %>%
      e_line(`Funding gap`) %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col2) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 60, 0))
      )
    
  })
  
  #-- tab 4: drug cost per patient
  ## drug plot1: 1st line drug cost 
  output$d2_heading <- renderText({ 
    paste("Fig.1b: Cost per patient: 1st line drugs")
  })
  
  output$d2_subheading <- renderText({ 
    paste("(average reported by country)")
  })
  
  df_d1 <- reactive({
    
    df_d1 <- budget %>%
      select(country,iso2,year,
             budget_cpp_dstb,
             budget_cpp_mdr,
             budget_cpp_tpt,
             budget_fld,
             budget_sld,
             budget_tpt,
             tx_dstb,
             tx_mdr,
             tx_tpt
      ) %>%
      full_join(select(expend, country, year, 
                       exp_cpp_dstb, exp_cpp_mdr, exp_cpp_tpt,
                       exp_fld, exp_sld, exp_tpt
      ), by = c("country","year")) %>%
      full_join(select(tpt, country, year, 
                     newinc_con_prevtx
    ), by = c("country","year")) %>%
      full_join(select(notif, country, year, 
                       c_notified,
                       c_rrmdr_tx,
                       c_dstb_tx,
                       c_xdr_tx
      ), by = c("country","year")) %>%
      mutate(year=as.factor(year)) %>%
      mutate(est_budget_cpp_dstb = round(budget_fld/tx_dstb,0),
             est_budget_cpp_mdr  = round(budget_sld/tx_mdr,0),
             est_budget_cpp_tpt  = round(budget_tpt/tx_tpt,1),
             est_expend_cpp_dstb = round(exp_fld/c_dstb_tx,0),
             est_expend_cpp_mdr  = round(exp_sld/c_rrmdr_tx,0),
             est_expend_cpp_tpt  = round(exp_tpt/newinc_con_prevtx,1),
               ) %>%
      arrange(country,year)  %>%
      filter(country==selected_country()$country)
    
  })
  
  col4 <- c("#1F78B4","#FF7F00")  
  # "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C"
  # "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
  # "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"

  output$drug_plot2 <- renderEcharts4r({
    
    plot_d1 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(budget_cpp_dstb, name = "Budget reported") %>%
      e_line(exp_cpp_dstb, name = "Expenditure reported") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d1
    
  })
  
  
  output$d1_heading <- renderText({ 
    paste("Fig.1a: Cost per patient: 1st line drugs")
  })
  
  
  output$d1_subheading <- renderText({ 
    paste("(estimation from budget/expenditure for 1st-line drugs and # of patients treated)")
  })
  
  output$drug_plot1 <- renderEcharts4r({
    
    plot_d2 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(est_budget_cpp_dstb, name = "Estimation from budget") %>%
      e_line(est_expend_cpp_dstb, name = "Estimation from expenditure") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d2
    
  })
  
  
  ## drug plot2: 2nd line drug cost 
  output$d4_heading <- renderText({ 
    paste("Fig.2b: Cost per patient: 2nd line drugs")
  })
  
  output$d4_subheading <- renderText({ 
    paste("(average reported by country)")
  })

  output$drug_plot4 <- renderEcharts4r({
    
    plot_d3 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(budget_cpp_mdr, name = "Budget reported") %>%
      e_line(exp_cpp_mdr, name = "Expenditure reported") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d3
    
  })
  
  
  output$d3_heading <- renderText({ 
    paste("Fig.2a: Cost per patient: 2nd line drugs")
  })
  
  output$d3_subheading <- renderText({ 
    paste("(estimation from budget/expenditure for 2nd-line drugs and # of patients treated)")
  })
  
  output$drug_plot3 <- renderEcharts4r({
    
    plot_d4 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(est_budget_cpp_mdr, name = "Estimation from budget") %>%
      e_line(est_expend_cpp_mdr, name = "Estimation from expenditure") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d4
    
  })

  ## drug plot3: TPT drug cost 
  output$d6_heading <- renderText({ 
    paste("Fig.3b: Cost per patient: TPT drugs")
  })
  
  output$d6_subheading <- renderText({ 
    paste("(average reported by country)")
  })
  
  output$drug_plot6 <- renderEcharts4r({
    
    plot_d5 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(budget_cpp_tpt, name = "Budget reported") %>%
      e_line(exp_cpp_tpt, name = "Expenditure reported") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d5
    
  })
  
  
  output$d5_heading <- renderText({ 
    paste("Fig.3a: Cost per patient: TPT drugs")
  })
  
  output$d5_subheading <- renderText({ 
    paste("(estimation from budget/expenditure for 2nd-line drugs and # of patients treated)")
  })
  
  output$drug_plot5 <- renderEcharts4r({
    
    plot_d6 <- df_d1() %>% 
      e_charts(year) %>%
      e_line(est_budget_cpp_tpt, name = "Estimation from budget") %>%
      e_line(est_expend_cpp_tpt, name = "Estimation from expenditure") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "US$",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_d6
    
  })
  
  
  #-- tab 5: Service utilization
  ## utilization plot1: Number of facility visits
  output$u1_heading <- renderText({ 
    paste("Fig.1: Number of facility visits")
  })
  
  df_u1 <- reactive({
    
    df_u1 <- expend %>%
      filter(year>2008) %>%
      select(country, iso2, year, 
                       hcfvisit_dstb:hosp_type_mdr) %>%
      mutate(year=as.factor(year)) %>%
      arrange(country,year)  %>%
      filter(country==selected_country()$country)
    
  })
  
  output$util_plot1 <- renderEcharts4r({
    
    plot_u1 <- df_u1() %>% 
      e_charts(year) %>%
      e_line(hcfvisit_dstb, name = "DS-TB") %>%
      e_line(hcfvisit_mdr, name = "MDR/RR-TB") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "Typical number of visits",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_u1
    
  })

  ## utilization plot2: Hospitalization rate
  output$u2_heading <- renderText({ 
    paste("Fig.2: Hospitalization rate")
  })
  
  output$util_plot2 <- renderEcharts4r({
    
    plot_u2 <- df_u1() %>% 
      e_charts(year) %>%
      e_line(hospd_dstb_prct, name = "DS-TB") %>%
      e_line(hospd_mdr_prct, name = "MDR/RR-TB") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "Rate (%)",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_u2
    
  })

  
  ## utilization plot3: Hospitalization rate
  output$u3_heading <- renderText({ 
    paste("Fig.3: Duration of hospitalization (days)")
  })
  
  output$util_plot3 <- renderEcharts4r({
    
    plot_u3 <- df_u1() %>% 
      e_charts(year) %>%
      e_line(hospd_dstb_dur, name = "DS-TB") %>%
      e_line(hospd_mdr_dur, name = "MDR/RR-TB") %>%
      e_grid(containLabel = T) %>%
      e_tooltip(trigger="item",
                textStyle=list(fontFamily="arial", fontSize=12)) %>%
      e_color(col4) %>%
      e_y_axis(
        name = "Days",
        nameLocation = "middle",
        nameTextStyle = list(fontSize = 18, padding = c(0, 0, 30, 0))
      )
    
    plot_u3
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

  
