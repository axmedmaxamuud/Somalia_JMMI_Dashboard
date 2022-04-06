### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
###
### PROJECT:  Somalia JMMI Dashboard VERSION 2 <----------!
### PURPOSE:  Analyse JMMI data
### INPUT:    jmmi_data.csv (data),
###           coverage.csv (location coordinates),
###           items.csv (translation of item names),
###           translation_so.csv (lang file),
###           style.css
### OUTPUT:   Shiny application
### AUTHOR:   Tie Franco Brotto
### LAST UPDATED: 16 December, 2020
###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# clean memory
rm(list = ls())

# Make the timestamp more precise
op <- options(digits.secs = 6)
Sys.time()

# load required packages
library(shiny)
library(shiny.i18n)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)
library(waiter)

# Set REACH colors
cc_red <- "#ee5859"
cc_lightred <- "#fbdada"
cc_grey <- "#58595a"
cc_midgrey <- "#acacad"
cc_lightgrey <- "#d1d3d4"
cc_bg <- "#e4e6e8"
cc_gr1 <- "#e6ced0"
cc_gr2 <- "#e7b7b8"
cc_gr3 <- "#e99fa1"
cc_gr4 <- "#eb8789"
cc_green <- "#37a76f"
cc_lightgreen <- "#a5c9a1"

### Set Directory (ONLY FOR R STUDIO TESTING, comment before publishing to server)
#ssetwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### LOAD LANGUAGE SUPPORT

# Load file with translations
i18n <-
  Translator$new(translation_csvs_path = "www/lang/") # This should be a directory with only .csv lang files
i18n$set_translation_language("en") # This is the auto language

# Set language options for selectInput
ll_lang_keys <- c("English", "Somali")
ll_lang <- i18n$get_languages()
names(ll_lang) <- ll_lang_keys

# Load item translations
items0 <-
  read.csv(
    "www/items.csv",
    stringsAsFactors = F,
    header = T,
    na.strings = c("", " ", "NA")
  )
# Cound the number of rows
items_n <- nrow(items0)

# Load JMMI data
df_base <-
  read.csv(
    "www/jmmi_data.csv",
    stringsAsFactors = F,
    header = T,
    na.strings = c("", " ", "NA")
  )
# Make tibble copy
df <- as_tibble(df_base)
# List of select multiple questions
sm <- as_tibble(rownames(t(df))) %>%
  mutate(value = gsub('[.].*', "", value)) %>%
  mutate(dup = duplicated(value)) %>%
  filter(dup == TRUE) %>%
  distinct() %>%
  pull(value)

# Load JMMI locations map
map_points0 <-
  read.csv("www/coverage.csv",
           stringsAsFactors = F,
           header = T)
# Pass all points to a secondary var, which can be further modified
map_points <- map_points0


# List of locations
ll_locations0 <- df %>%
  distinct(call_location) %>%
  pull(call_location) %>%
  sort()
ll_locations <- c("all", ll_locations0)
ll_locations_name <- str_to_title(ll_locations)
ll_locations_list <- ll_locations
# Set location options for selectInput
names(ll_locations_list) <- ll_locations_name

# List of rounds
ll_round <- df %>%
  distinct(round_id) %>%
  pull(round_id)
ll_round_name <- df %>%
  distinct(round) %>%
  pull(round)
# Get latest round
latest_round <- max(ll_round)
# Set round options for selectInput
names(ll_round) <- ll_round_name

# Boxplot styling
f <- list(family = '"Arial Narrow", sans-serif',
          size = 18,
          color = cc_grey)
x <- list(titlefont = f,
          tickangle = 90)

# UI ---------------------------------------------
ui <- fluidPage(
  # Start waiter engine and define colors
  use_waitress(color = cc_bg, percent_color = cc_red),
  
  # Start translation engine
  shiny.i18n::usei18n(i18n),
  
  # Include main CSS sheet
  tags$head(includeCSS("www/style.css")),
  
  wellPanel(
    style = "background: #FFFFFF;",
    titlePanel(title = fluidRow(column(
      12,
      HTML(
        "<h1 align = 'left'>SOMALIA &#149; JOINT MARKET MONITORING INITIATIVE</h1>"
      )
    )),
    
    windowTitle = "SOMALIA JMMI"),
    fluidRow(
      column(
        4,
        h2(i18n$t("ABOUT")),
        p(
          i18n$t(
            "The Joint Market Monitoring Initiative (JMMI) is a joint initiative from the Somalia Water, Sanitation and Hygiene (WASH), Shelter, and Education clusters and REACH. It aims to address an information gap in Somalia in terms of regular and updated monitoring of market functionality and a broad range of non-food items (NFIs), while contributing to existing supply chain and price monitoring of the main minimum expenditure basket (MEB) items."
          )
        ),
        p(
          i18n$t(
            "Refer to the bottom of this page for more information about the methodology, coordination framework, target item specifications, and other resources."
          )
        ),
        h3(i18n$t("Co-leads")),
        div(class = "img_left",
            imageOutput(
              "lg_reach", height = "auto", width = "100%"
            )),
        div(class = "img_left",
            imageOutput(
              "lg_wash", height = "auto", width = "100%"
            )),
        div(class = "img_left",
            imageOutput(
              "lg_shel", height = "auto", width = "100%"
            )),
        div(class = "img_left",
            imageOutput(
              "lg_educ", height = "auto", width = "100%"
            ))
      ),
      column(4,
             h3(
               i18n$t("Locations covered in the selected round")
             ),
             leafletOutput("Map", height = 400),
             p(
               i18n$t(
                 "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the JMMI coordination framework"
               )
             )),
      column(
        4,
        # Select location
        selectInput(
          "select_location",
          label = h3(i18n$t("Select location")),
          choices = ll_locations_list,
          multiple = FALSE,
          selected = "all"
        ),
        # Select language
        selectInput(
          "select_lang",
          h3(i18n$t("Select language")),
          choices = ll_lang,
          selected = i18n$get_key_translation(),
          multiple = FALSE
        ),
        # Select round
        selectInput(
          "select_round",
          h3(i18n$t("Select round")),
          choices = ll_round,
          selected = latest_round,
          multiple = FALSE
        )
      )
    ),
    
    fluidRow(column(12,
                    # Horizontal line
                    hr())),
    
    fluidRow(column(
      3,
      h2(textOutput("txt_location")),
      textOutput("txt_summary"),
    ),
    column(
      9,
      h2(i18n$t("PRICES")),
      p(
        i18n$t(
          "Median reported prices for items assessed and number of vendors reporting this per key item."
        )
      ),
      # Price table
      DTOutput("table_price")
    )),
    
    fluidRow(column(
      12,
      h2(i18n$t("DISTRIBUTION OF PRICES")),
      p(
        i18n$t(
          "Refer to 'How to read a boxplot' (below) for an explanation about how to read this graph. The targetted item specifications can also be found below. This graph is interactive, you can zoom in and mouse over to get more information, among other tools available at the upper right corner of this graph."
        )
      ),
      # Price boxplot
      plotlyOutput("boxplot")
    )),
    
    fluidRow(column(12,
                    # Horizontal line
                    hr())),
    
    fluidRow(column(
      9,
      h2(i18n$t("STOCK")),
      p(
        i18n$t(
          "Median reported stock levels and expected restocking time (in days), and median stocking difficulty."
        )
      ),
      # Stock table
      DTOutput("table_stock")
    ),
    column(
      3,
      h3(i18n$t("Key")),
      p(
        i18n$t(
          "Stock duration: Median number of days reported by vendors, that current stock of each assessed item is expected to last, assuming that the rate of purchase remains consistent."
        ),
        em(i18n$t(
          "In the graph below it is represented by the left bar."
        ))
      ),
      p(
        i18n$t(
          "Restocking time: Median number of days reported by vendors, that it would take to restock each item assessed, from ordering to delivery in the shop."
        ),
        em(i18n$t(
          "In the graph below it is represented by the right bar."
        ))
      ),
      p(
        i18n$t(
          "Stocking difficulty: Percentage of vendors reporting having experienced difficulty to restock each item assessed, in the 3 months prior to data collection."
        ),
        em(i18n$t(
          "In the graph below it is represented by the color red."
        ))
      )
    )),
    
    fluidRow(column(
      12,
      h2(i18n$t("STOCK GRAPH")),
      p(
        i18n$t(
          "Refer to the key (above) for an explanation about how to read this graph. The targetted item specifications can be found below. For stock conditions, items with more than one specification are combined. This graph is interactive, you can zoom in and mouse over to get more information, among other tools available at the upper right corner of this graph."
        )
      ),
      # Stock graph
      plotlyOutput("graph_stock")
    )),
    
    fluidRow(column(
      8,
      
      fluidRow(
        column(
          6,
          h2(i18n$t("METHODOLOGY")),
          p(
            i18n$t(
              "Primary data is collected quarterly through interviews with market vendors purposively selected from the targeted markets. The clusters' partners are responsible for data collection. In this first round, 491 interviews were collected from 12 locations between August 9-12, 2020. As vendors were purposively selected, findings are not statistically representative. To prevent spread and contraction of COVID-19, data was collected remotely. This situation limited the capacity of enumerators to target specific vendors. In addition, it limited possibilities of follow up with vendors. All findings are indicative only, and only apply to the period within which data was collected. Moreover, item specifications may vary slightly between locations according to different brands available, and comparability between the locations assessed is limited."
            )
          ),
          h2(i18n$t("FEEDBACK")),
          p(
            i18n$t(
              "If you have questions about the JMMI, suggestions for improving this dashboard, and/or would like to report an error, please write to us using the link below"
            )
          ),
          HTML(
            "<a href='mailto:tie.franco-brotto@reach-initiative.org'>E-mail</a>"
          ),
          h2(i18n$t("RESOURCES")),
          HTML(
            "REACH, <a target='_blank' href='https://bit.ly/som-jmmi-20q3'>Joint Market Monitoring Initiative (factsheets)</a><br/>
          REACH, <a target='_blank' href='https://www.reachresourcecentre.info/country/somalia/cycle/29743/#cycle-29743'>Joint Market Monitoring Initiative</a><br/>
          REACH, <a target='_blank' href='https://bit.ly/3eBwBX6'>Market Feasibility Study - Mogadishu</a><br/>
REACH, <a target='_blank' href='https://www.reachresourcecentre.info/country/somalia/cycle/705#cycle-705'>Market Feasibility Studies</a><br/>
REACH, <a target='_blank' href='https://www.reachresourcecentre.info/country/somalia/'>Somalia</a><br/>
CWG, <a target='_blank' href='https://data.humdata.org/visualization/somalia-cash-programing-v3/'>Cash Based Programming in Somalia</a><br/>
CWG, <a target='_blank' href='https://www.humanitarianresponse.info/en/operations/somalia/cash-activities'>Website</a><br/>
FSNAU, <a target='_blank' href='https://www.fsnau.org/sectors/markets'>Markets</a><br/>
WFP VAM, <a target='_blank' href='https://dataviz.vam.wfp.org/Reports_Explorer'>Reports Explorer</a>
"
          )
        ),
        column(
          6,
          h2(i18n$t("COORDINATION FRAMEWORK")),
          p(
            i18n$t(
              "The WASH, Shelter, and Education clusters are responsible for the identification of partners, among cluster members, willing to contribute to the JMMI. The clusters also lead external coordination with the Humanitarian Country Team (HCT) stakeholders and government actors. Cluster members identified as partners provide data collection capacity according to their access and availability, and support the study with sector-specific expertise. REACH is responsible for leading the tools and analysis framework design, training of partners and technical support for data collection, supporting focal points in managing the field data collection, leads on technical data management and data cleaning, data analysis, and output production. The geographic coverage area is determined by the access and capacity of partners. In order to maximize efficacy, certain markets are prioritized to reflect the areas in which cash transfer programs, particularly focused on NFIs, are planned or ongoing, as well as key supply chains for the main NFIs assessed."
            )
          ),
          h3(i18n$t("Partners")),
          uiOutput("lg_partners"),
          div(class = "clear_float"),
          h3(i18n$t("Donor")),
          div(class = "img_left33",
              imageOutput(
                "lg_donor", height = "auto", width = "100%"
              ))
        )
      ),
      
      fluidRow(column(
        12,
        h2(i18n$t("HOW TO READ A BOXPLOT")),
        imageOutput("img_boxplot", height = "auto")
      ))
      
    ),
    
    column(
      4,
      h2(i18n$t("TARGET ITEM SPECIFICATIONS")),
      tableOutput('table_specs')
    ))
    
  )
  
) #

# Server -----------------------------------------
server <- function(input, output, session) {
  # Call the waitress
  waitress <- Waitress$new(theme = "overlay-percent")
  
  # REACTIVE LANG
  observeEvent(input$select_lang, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$select_lang)
    
    # Waitress progress
    waitress$start(h2("Cusbooneysiinta luqadda ..."))
  })
  
  # REACTIVE DT TRANSLATION
  lang_dt <- reactive({
    # Translate DT output tags
    lang_dt <- list(
      sEmptyTable =     i18n$t("No data available in table"),
      sInfo =           i18n$t("Showing _START_ to _END_ of _TOTAL_ entries"),
      sInfoEmpty =      i18n$t("Showing 0 to 0 of 0 entries"),
      sInfoFiltered =   i18n$t("(filtered from _MAX_ total entries)"),
      sInfoPostFix =    "",
      sInfoThousands =  ",",
      sLengthMenu =     i18n$t("Show _MENU_ entries"),
      sLoadingRecords = i18n$t("Loading..."),
      sProcessing =     i18n$t("Processing..."),
      sSearch =         i18n$t("Search:"),
      sZeroRecords =    i18n$t("No matching records found"),
      oPaginate = list(
        sFirst =    i18n$t("First"),
        sLast =     i18n$t("Last"),
        sNext =     i18n$t("Next"),
        sPrevious = i18n$t("Previous")
      ),
      oAria = list(
        sSortAscending =   i18n$t(": activate to sort column ascending"),
        sSortDescending =  i18n$t(": activate to sort column descending")
      )
    )
  })
  
  # REACTIVE ROUND FILTER
  rex_round <- reactive({
    # Kickoff WAITRESS
    waitress$start()
    print(paste0("waitress started at" , Sys.time()))
    
    temp <- df %>%
      filter(str_detect(round_id, input$select_round))
    return(temp)
  })
  
  # REACTIVE PARTNER LIST
  rex_partners <- reactive({
    temp0 <- rex_round() %>%
      distinct(enumerator_org) %>%
      pull(enumerator_org)
    temp <- sort(c(temp0, "norcap"), FALSE)
    return(temp)
  })
  
  # Update list of location, according to the round
  observeEvent(input$select_round, {

    if (!is.null(input$select_round)) {
      # List of locations
      ll_locations0 <- rex_round() %>%
        distinct(call_location) %>%
        pull(call_location) %>%
        sort()
      ll_locations <- c("all", ll_locations0)
      ll_locations_name <- str_to_title(ll_locations)
      
      # Set location options for selectInput
      ll_locations_list <- ll_locations
      names(ll_locations_list) <- ll_locations_name
      
      # Check if location selected exists in the new location list
      if (input$select_location %in% ll_locations) {
        # Update list of locations, WITH same location
        updateSelectInput(
          session,
          "select_location",
          choices = ll_locations_list,
          selected = input$select_location
        )
      } else {
        # Update list of locations
        updateSelectInput(session,
                          "select_location",
                          choices = ll_locations_list,
                          selected = "all")
        
      }
      # Update map points
      map_points <- map_points0 %>%
        filter(id %in% ll_locations)
      leafletProxy(mapId = "Map",
                   data = map_points) %>%
        clearMarkers() %>%   ## clear previous markers
        addCircleMarkers(
          data = map_points,
          lng = ~ Lon,
          lat = ~ Lat,
          layerId = ~ id,
          color = cc_red,
          label = ~ as.character(Location),
          radius = 6
        )
    }
  })
  
  # REACTIVE LOCATION FILTER
  rex_location <- reactive({
    
    if (input$select_location != "all") {
      temp <- rex_round() %>%
        filter(str_detect(call_location, input$select_location)) %>%
        group_by(call_location)
    } else {
      temp <- rex_round() %>%
        group_by(call_location)
    }
    return(temp)
  })
  
  # Location name for section title
  output$txt_location <- renderText({
    if (input$select_lang == "so")  {
      if (input$select_location == "all") {
        return("GUUD AHAAN")
      }
    } else {
      return(str_to_upper(input$select_location))
    }
  })
  
  # Interactive Map
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(43, 5.15, zoom = 5) %>%
      addCircleMarkers(
        data = map_points,
        lng = ~ Lon,
        lat = ~ Lat,
        layerId = ~ id,
        color = cc_red,
        label = ~ as.character(Location),
        radius = 6
      )  
  })
  
  # Update selected location in the map
  observeEvent(input$Map_marker_click, {
    p <- input$Map_marker_click
    if (!is.null(p$id)) {
      if (is.null(input$select_location))
        updateSelectInput(session,
                          "select_location",
                          choices = ll_locations_list,
                          selected = p$id)
      if (!is.null(input$select_location) &&
          input$select_location != p$id)
        updateSelectInput(session,
                          "select_location",
                          choices = ll_locations_list,
                          selected = p$id)
    }
  })
  
  # REACTIVE PRICE DATA
  rex_price <- reactive({
    # Only select columns with final USD price
    temp <- rex_location() %>%
      # Adds all columns starting with price_usd_ and
      # Excludes the _other prices (which should have been manually moved to price_usd_ during checks)
      select(starts_with("price_usd_"), vendor_type) %>%
      select(!ends_with("_other"))
    return(temp)
  })
  
  # REACTIVE ITEM LIST WITH VENDOR TYPES
  rex_items <- reactive({
    # This is used below, so that we can calculate prices without having to deal with vendor types in the meantime
    temp <- rex_price() %>%
      # Pivot so that we can create an items column
      pivot_longer(starts_with("price_usd_"),
                   names_to = "item",
                   values_to = "value") %>%
      filter(!is.na(value)) %>%
      ungroup(call_location) %>%
      select(vendor_type, item) %>%
      distinct()
    return(temp)
  })
  
  # REACTIVE PRICE TABLE - pt 1
  rex_table_price1 <- reactive({
    
    temp <- rex_price()  %>%
      select(!vendor_type) %>%
      # Get number of vendors reporting each item (nv), median, and quartiles
      summarise(across(everything(),  list(
        nv = ~ sum(!is.na(.)),
        median = ~ median(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
      ))) %>%
      # Pivot so that we can create an items column
      pivot_longer(!any_of("call_location"),
                   names_to = "key",
                   values_to = "value") %>%
      # obs. the regex "_(?!.*_)" means the last _ in the string
      separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
      pivot_wider(names_from = "info", values_from = "value") %>%
      # Add a check column for all items with less than three prices (OR two prices for water suppliers)
      mutate(check = case_when(str_detect(item, "truck") ~ nv > 1,
                               str_detect(item, "communal") ~ nv > 1,
                               str_detect(item, "piped") ~ nv > 1,
                               TRUE ~ nv > 2))
  })
  
  # CREATE PRICE BOXPLOTS
  output$boxplot <- renderPlotly({
    # Calculate aggregate prices
    
    
    
    
    
    
    
    
    
    
    
    
    # List items with SUFFICIENT quotations
    ll_sufficient_items <- rex_table_price1() %>%
      # Delete items with less than three prices
      filter(check == TRUE) %>%
      distinct(item) %>%
      pull(item)
    
    #># Item prices (boxplot)
    bx_g <- rex_price() %>%
      select(!vendor_type) %>%
      pivot_longer(contains("price_usd"),
                   names_to = "item",
                   values_to = "price") %>%
      # Only keep items with sufficient quotations
      filter(item %in% ll_sufficient_items) %>%
      # Remove empty prices
      filter(!is.na(price))
    
    # If Somali, do DB translations
    if (input$select_lang == "so") {
      for (b in 1:items_n) {
        bx_g <- bx_g %>%
          # Translate items to Somali
          mutate(item = gsub(
            pattern = items0$english[b],
            replacement = items0$somali[b],
            item
          ))
      }
    }
    
    bx_g <- bx_g %>%
      mutate(item = str_to_title(gsub("_", " ", gsub(
        "price_usd_", "", item
      ))))
    
    temp <- plot_ly(
      bx_g,
      y = ~ price,
      color = ~ item,
      colors = cc_red,
      type = "box"
    ) %>%
      layout(
        showlegend = FALSE,
        xaxis = x,
        yaxis = list(
          title = i18n$t("Reported price (USD)"),
          titlefont = f,
          type = "log"
        )
      )
    return(temp)
  })
  
  
  # REACTIVE PRICE TABLE - pt 2
  rex_table_price <- reactive({

    temp <- rex_table_price1() %>%
      # Delete items with less than three prices
      filter(check == TRUE) %>%
      select(!check) %>%
      # Add vendor type
      left_join(rex_items(), by="item")
    
    # Draw median of medians
    if (input$select_location == "all") {
      temp <- temp %>%
        group_by(item) %>%
        summarise(
          call_location = "all",
          item = item, 
          nv = sum(nv),
          median = median(median),
          q1 = median(q1),
          q3 = median(q3),
          vendor_type = vendor_type
        ) %>%
        ungroup(item) %>%
        distinct()
    }
    
    # If Somali, do DB translations
    if (input$select_lang == "so") {
      for (b in 1:items_n) {
        temp <- temp %>%
          # Translate items to Somali
          mutate(item = gsub(
            pattern = items0$english[b],
            replacement = items0$somali[b],
            item
          )) %>%
          # Translate item types to Somali
          mutate(
            vendor_type = gsub(
              pattern = items0$english[b],
              replacement = items0$somali[b],
              vendor_type
            )
          )
      }
    }
    
    print(temp)
    
    temp <- temp %>%
      # Rename items
      mutate(item = str_to_title(gsub('_', " ", gsub(
        'price_usd_', "", item
      )))) %>%
      # Order by vendor type and then item
      arrange(vendor_type, item) %>%
      # Finalize table
      select(item, nv, price = median, q1, q3, vendor_type) # Change number of vendors to integer
    
    print("POTATO")
    
    names(temp) <- c(
      i18n$t("Item"),
      i18n$t("# Vendors"),
      i18n$t("Price (USD)"),
      i18n$t("1st Quartile"),
      i18n$t("3rd Quartile"),
      i18n$t("Vendor Type")
    )
    return(temp)
  })
  
  # CREATE TABLE PRICE
  output$table_price <- renderDT({
    temp <- datatable(rex_table_price(),
                      options = list(language = lang_dt())) %>%
      formatStyle(c(i18n$t("1st Quartile"), i18n$t("3rd Quartile")),
                  color = cc_grey,
                  backgroundColor = cc_midgrey) %>%
      formatCurrency(
        c(
          i18n$t("Price (USD)"),
          i18n$t("1st Quartile"),
          i18n$t("3rd Quartile")
        ),
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 2,
        dec.mark = getOption("OutDec"),
        before = TRUE
      )
    return(temp)
  })
  
  
  # REACTIVE MAIN CURRENCY TABLE
  rex_table_locationcounts <- reactive({
    # Create a table with counts of interviews per location
    nn <- rex_round() %>%
      select(call_location) %>%
      group_by(call_location) %>%
      summarise(nn = n())
    # Summarize so there's a line with all
    nn0 <- nn %>%
      summarise(call_location = "all",
                nn = sum(nn))
    # Combine both
    nn <- nn %>%
      bind_rows(nn0)
    return(nn)
  })
  
  # REACTIVE MAIN CURRENCY TABLE
  rex_table_currencymain <- reactive({
    # Get the location counts
    nn <- rex_table_locationcounts()
    # Start the calculation for main_currency
    pa_x <- rex_round() %>%
      # Adds all columns related to payment
      select(call_location, currency_main) %>%
      group_by(call_location) %>%
      count(currency_main)
    # Create a global line
    pa_x0 <- pa_x %>%
      select(currency_main, n) %>%
      group_by(currency_main) %>%
      summarise(call_location = "all",
                n = sum(n))
    # Combine both
    pa_x <- pa_x %>%
      bind_rows(pa_x0) %>%
      # Attach the location counts
      left_join(nn, by = "call_location") %>%
      # Calculate percentages
      mutate(pc = n / nn)
    
    return(pa_x)
  })
  
  output$txt_summary <- renderText({
    # Get location count
    location_count <- rex_table_locationcounts() %>%
      filter(str_detect(call_location, input$select_location)) %>%
      pull(nn)
    currency <- rex_table_currencymain() %>%
      filter(str_detect(call_location, input$select_location)) %>%
      filter(!is.na(currency_main)) %>%
      arrange(desc(pc))
    location_name <- str_to_title(input$select_location)
    
    
    # Write text (A BIT MESSY, NEEDS TO BE FIXED IN THE FUTURE)
    if (input$select_lang == "en") {
      if (location_name == "All") {
        location_name <- "total"
      }
      
      temp <- paste0(
        "In " ,
        location_name ,
        ", " ,
        location_count ,
        " vendors were interviewed in the selected round. The main currency reportedly used by these vendors in their shops was "
      )
      
      for (a in 1:nrow(currency)) {
        if (a == 2) {
          temp <- paste0(temp, ", followed by ")
        }
        if (a > 2) {
          temp <- paste0(temp, ", and ")
        }
        temp <- paste0(temp, paste0(
          " ",
          str_to_title(gsub("_", " ", currency$currency_main[a])),
          " (",
          paste(round(100 * currency$pc[a], 2), "%", sep = ""),
          ")"
        ))
        if (a == nrow(currency)) {
          temp <- paste0(temp, ".")
        }
      }
      
    } else {
      if (location_name == "All") {
        temp <- paste0(
          "Guud ahaan, " ,
          location_count ,
          " ganacsato ayaa lagu wareystay wareega la soo xulay. Lacagta ugu badan ee la sheegay in ganacsatadan dukaankooda ku isticmaaleen ayaa ahayd "
        )
      } else {
        temp <- paste0(
          "Magaalada " ,
          location_name ,
          ", " ,
          location_count ,
          " ganacsato ayaa lagu wareystay wareega la soo xulay. Lacagta ugu badan ee la sheegay in ganacsatadan dukaankooda ku isticmaaleen ayaa ahayd "
        )
      }
      
      for (a in 1:nrow(currency)) {
        if (a == 2) {
          temp <- paste0(temp, ", waxaa ku xiga ")
        }
        if (a > 2) {
          temp <- paste0(temp, ", iyo ")
        }
        temp <- paste0(temp, paste0(
          " ",
          str_to_title(gsub("_", " ", currency$currency_main[a])),
          " (",
          paste(round(100 * currency$pc[a], 2), "%", sep = ""),
          ")"
        ))
        if (a == nrow(currency)) {
          temp <- paste0(temp, ".")
        }
      }
    }
    
    
    # " the Somali Shilling (89%) and the United States Dollar (USD, 11%). All vendors reported accepting mobile payment (100%) and nearly all reportedly accepted cash (98%). The majority (64%) reported not charging more for a specific payment method.")
    return(temp)
  })
  
  
  # REACTIVE START OF STOCK TABLE
  rex_table_stock <- reactive({
    
    # Create a reference of number of interviews per location
    rex_nn <- rex_location() %>%
      select(call_location) %>%
      summarise(nn = n())
    
    # Simple assign, can be integrated below later on, once everything is working
    temp <- rex_location() 
    
    # Waitress progress
    waitress$set(20)
    print(paste0("20% at" , Sys.time()))
    
    ### Stock analysis
    st_i <- temp %>%
      # Adds all columns related to stock
      select(starts_with(c("stock_len_", "stock_time_"))) %>%
      # Get median and quartiles
      summarise(across(
        everything(),
        list(
          nv = ~ sum(!is.na(.)),
          median = ~ median(.x, na.rm = TRUE),
          q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
          q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
        )
      )) %>%
      pivot_longer(!any_of("call_location"),
                   names_to = "key",
                   values_to = "value") %>%
      # obs. the regex "_(?!.*_)" means the last _ in the string
      separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
      pivot_wider(names_from = "info", values_from = "value")
    
    # Stock length table
    st_l <- st_i %>%
      rename(len_m = median,
             len_q1 = q1,
             len_q3 = q3) %>%
      filter(str_detect(item, "stock_len_")) %>%
      mutate(item = gsub('stock_len_', "", item)) %>%
      unite("name2", call_location, item, sep = "__")
    
    # Stock time table
    st_t <- st_i %>%
      select(!nv) %>%
      rename(tim_m = median,
             tim_q1 = q1,
             tim_q3 = q3) %>%
      filter(str_detect(item, "stock_time_")) %>%
      mutate(item = gsub('stock_time_', "", item)) %>%
      unite("name2", call_location, item, sep = "__")
    
    #># Stock difficult table
    st_d <- temp %>%
      # Adds all columns related to stock
      select(starts_with("stock_dif_")) %>%
      mutate(across(everything(), ~ gsub("dk", 'no', .))) %>%
      mutate(across(everything(), ~ gsub("pnta", 'no', .))) %>%
      pivot_longer(!any_of("call_location"),
                   names_to = "name",
                   values_to = "value") %>%
      unite("key", call_location, name, sep = "_1_") %>%
      unite("key", key, value, sep = "_2_") %>%
      count(key) %>%
      separate(key, c("key", "value"), sep = "_2_") %>%
      separate(key, c("call_location", "name"), sep = "_1_") %>%
      na_if("NA") %>%
      mutate(name1 = gsub('stock_dif_', "", name)) %>%
      full_join(rex_nn, by = "call_location") %>%
      unite("name2", call_location, name1, sep = "__") %>%
      unite("key", name, value, sep = "__")
    
    # This will calculate the number of responses, by excluding the NAs
    st_dx <- st_d %>%
      filter(str_detect(key, "__NA")) %>%
      mutate(nn = nn - n) %>%
      select(name2, nn)
    
    st_d <- st_d %>%
      filter(!str_detect(key, "__NA")) %>%
      left_join(st_dx, by = "name2") %>%
      mutate(nn = case_when(is.na(nn.y) ~ nn.x,!is.na(nn.y) ~ nn.y)) %>%
      mutate(pc = n / nn) %>%
      select(name2,
             item = key,
             dif = n,
             dif_pc = pc)
    
    # Create a temporary table to convert 100% no in 0% yes
    st_d0 <- st_d %>%
      filter(str_detect(item, "__no")) %>%
      filter(dif_pc == 1) %>% # To avoid an error, eg NO 90% DK 10%, must convert all DK and PNTA to NO (see above)
      mutate(item = gsub("__no", '__yes', item)) %>%
      mutate(dif_pc = 0) %>%
      mutate(dif = 0)
    
    # Merge back
    st_d <- st_d %>%
      filter(str_detect(item, "__yes")) %>%
      bind_rows(st_d0) %>%
      mutate(item = gsub('stock_dif_', "", gsub('__yes', "", item)))
    
    # Fix item names for the non-price indicators
    ve_t <- rex_items() %>%
      # obs. the regex "_(?!.*_)" means the last _ in the string
      separate(item, c("item", "extra"), sep = "_(?!.*_)") %>%
      mutate(item = gsub('price_usd_', "", item)) %>%
      select(!extra) %>%
      distinct()
    
    #># Finalize main stock table
    temp <- st_l %>%
      full_join(st_t, by = "name2") %>%
      full_join(st_d, by = "name2") %>%
      # obs. the regex "_{1}" means the first _ in the string
      separate(name2, c("call_location", "item"), sep = "__") %>%
      # Add vendor type
      left_join(ve_t, by = "item") %>%
      # Clear NAs
      filter(!is.na(len_m)) %>%
      # Order by vendor type and then item
      arrange(vendor_type, item) %>%
      # Add colours that will be used in the graph
      mutate(
        color = case_when(
          is.na(dif_pc) ~ cc_grey,
          len_m < tim_m ~ cc_red,
          # This is for imminent shortage, rest is reported difficulty to stock
          dif_pc < 0.2 ~ cc_midgrey,
          dif_pc < 0.4 ~ cc_gr1,
          dif_pc < 0.6 ~ cc_gr2,
          dif_pc < 0.8 ~ cc_gr3,
          TRUE ~ cc_gr4
        )
      ) %>%
      mutate(check = case_when(
        str_detect(item, "truck") ~ nv > 1,
        str_detect(item, "communal") ~ nv > 1,
        str_detect(item, "piped") ~ nv > 1,
        TRUE ~ nv > 2
      ))
    
    # Delete items with less than three prices
    #filter(check == TRUE)
    
    if (input$select_location == "all") {
      # Calculate aggregate
      temp <- temp %>%
        # Delete items with less than three prices
        filter(check == TRUE) %>%
        group_by(item) %>%
        summarise(
          call_location = "all",
          nv = sum(nv, na.rm = TRUE),
          len_m = median(len_m, na.rm = TRUE),
          tim_m = median(tim_m, na.rm = TRUE),
          dif_pc = median(dif_pc, na.rm = TRUE),
          vendor_type = vendor_type
          ) %>%
        mutate(
          color = case_when(
            is.na(dif_pc) ~ cc_grey,
            len_m < tim_m ~ cc_red,
            # This is for imminent shortage, rest is reported difficulty to stock
            dif_pc < 0.2 ~ cc_midgrey,
            dif_pc < 0.4 ~ cc_gr1,
            dif_pc < 0.6 ~ cc_gr2,
            dif_pc < 0.8 ~ cc_gr3,
            TRUE ~ cc_gr4
          )
        ) %>%
        ungroup(item) %>%
        distinct()
    }
    
    # If Somali, do DB translations
    if (input$select_lang == "so") {
      for (b in 1:items_n) {
        temp <- temp %>%
          # Translate items to Somali
          mutate(item = gsub(
            pattern = items0$english[b],
            replacement = items0$somali[b],
            item
          )) %>%
          # Translate item types to Somali
          mutate(
            vendor_type = gsub(
              pattern = items0$english[b],
              replacement = items0$somali[b],
              vendor_type
            )
          )
      }
    }
    
    temp <- temp %>%
      mutate(item = str_to_title(gsub('_', " ", gsub(
        'price_usd_', "", item
      ))))
  })
  
  ### Stock analysis
  output$table_stock <- renderDT({
    temp <- rex_table_stock() %>%
      select(item, nv, len_m, tim_m, dif_pc, vendor_type)
    
    names(temp) <- c(
      i18n$t("Item"),
      i18n$t("# Vendors"),
      i18n$t("Stock duration (days)"),
      i18n$t("Restocking time (days)"),
      i18n$t("Stocking difficulty"),
      i18n$t("Vendor Type")
    )
    temp <- datatable(temp,
                      options = list(language = lang_dt())) %>%
      formatStyle(i18n$t("Stocking difficulty"),
                  backgroundColor = styleInterval(
                    c(0.2, 0.4, 0.6, 0.8),
                    c(cc_bg, cc_gr1, cc_gr2, cc_gr3, cc_gr4)
                  )) %>%
      formatPercentage(
        i18n$t("Stocking difficulty"),
        digits = 0,
        interval = 3,
        mark = ",",
        dec.mark = getOption("OutDec")
      )
    return(temp)
  })
  
  
  ### Stock analysis
  output$graph_stock <- renderPlotly({
    
    # List of select multiple questions
    st_color <- rex_table_stock() %>%
      pull(color)
    
    st_g <-
      plot_ly(
        rex_table_stock(),
        x = ~ item,
        y = ~ len_m,
        type = 'bar',
        name = i18n$t('Reported length of current stock'),
        marker = list(color = st_color),
        hovertemplate = paste('(%{x}, %{y})')
      ) %>%
      add_trace(
        y = ~ tim_m,
        name = i18n$t('Reported time to restock, from order to item in shop')
      ) %>%
      layout(
        yaxis = list(title = i18n$t('Days'),
                     titlefont = f),
        xaxis = list(title = ''),
        barmode = 'group',
        showlegend = FALSE
      )
    
    # Waitress progress
    waitress$close()
    print(paste0("100% at" , Sys.time()))
    return(st_g)
    
  })
  
  # Make specs table, filtering according to the language
  output$table_specs <- renderTable({
    if (input$select_lang == "en") {
      temp <- items0 %>%
        select(fullen, specen) %>%
        filter(!is.na(fullen))
    } else {
      temp <- items0 %>%
        select(fullso, specso) %>%
        filter(!is.na(fullso))
    }
    #head(temp)
    return(temp)
  },
  hover = TRUE,
  width = '100%',
  align = 'l',
  colnames = FALSE)
  
  
  ### Logo REACH
  output$lg_reach <- renderImage({
    list(
      src = file.path("www", paste0("reach.png")),
      contentType = "image/png",
      width = "100%",
      className = "img_left"
    )
  }, deleteFile = FALSE)
  ### Logo WASH
  output$lg_wash <- renderImage({
    list(
      src = file.path("www", paste0("wash.png")),
      contentType = "image/png",
      width = "100%",
      className = "img_left"
    )
  }, deleteFile = FALSE)
  ### Logo SHELTER
  output$lg_shel <- renderImage({
    list(
      src = file.path("www", paste0("shel.png")),
      contentType = "image/png",
      width = "100%",
      className = "img_left"
    )
  }, deleteFile = FALSE)
  ### Logo EDUCATION
  output$lg_educ <- renderImage({
    list(
      src = file.path("www", paste0("educ.png")),
      contentType = "image/png",
      width = "100%",
      className = "img_left"
    )
  }, deleteFile = FALSE)
  ### Logo donor
  output$lg_donor <- renderImage({
    list(
      src = file.path("www", paste0("usaid.png")),
      contentType = "image/png",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  
  ### PARTNERS LOGOS
  output$lg_partners <- renderUI({
    image_names <- rex_partners()
    image_output_list <-
      lapply(1:length(image_names),
             function(i)
             {
               imagename = paste0(image_names[i], "_image")
               div(class = "img_left33",
                   imageOutput(imagename, height = "auto", width = "100%"))
             })
    
    do.call(tagList, image_output_list)
  })
  
  observe({
    image_names <- rex_partners()
    for (i in 1:length(image_names))
    {
      local({
        ii <- i
        imagename <- paste0(image_names[ii], "_image")
        output[[imagename]] <-
          renderImage({
            list(
              src = normalizePath(paste0(
                'www/partners/', image_names[ii], '.png'
              )),
              contentType = "image/png",
              width = "100%",
              className = "img_left"
            )
          }, deleteFile = FALSE)
      })
    }
  })
  
  ### How to read a boxplot
  output$img_boxplot <- renderImage({
    if (input$select_lang == "en") {
      list(src = file.path("www", paste0("how_boxplot.svg")),
           contentType = "image/svg+xml")
    } else {
      list(src = file.path("www", paste0("how_boxplot_som.svg")),
           contentType = "image/svg+xml")
    }
  }, deleteFile = FALSE)
  
} #

# Run the application
shinyApp(ui = ui, server = server)