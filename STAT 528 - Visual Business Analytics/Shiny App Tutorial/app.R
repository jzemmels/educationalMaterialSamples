# This document contains code to create an interactive shiny application using
# the shinydashboard package. The code here extends past the application created
# in the lecture videos to include changes and additions.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#FFA500"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

liquor <- read_csv("liquorCleaned.csv") %>%
  mutate(County = ifelse(County == "BUENA VIST","BUENA VISTA",
                         ifelse(County == "CERRO GORD","CERRO GORDO",
                                ifelse(str_detect(toupper(County),pattern = "POTTAWATTA"),
                                       "POTTAWATTAMIE",toupper(County)))),
         saleID = str_sub(invoice,5,10))

iowaCounty <- ggplot2::map_data(map = "county",region = "iowa") %>%
  mutate(subregion = toupper(subregion))

iowaCountyPop <- read_csv("iowaCountyPop.csv")


##################

# these two objects are used in the association rules tab

allSaleID <- unique(liquor$saleID)

liquorYFrequency <- liquor %>%
  select(saleID,description) %>%
  distinct() %>%
  group_by(description) %>%
  tally(name = "freq")

# the shinybusy package adds a loading spinner

ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(title = "Iowa Liquor Sales"),
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem(text = "Popularity",tabName = "popularity",icon = icon("fire")),
      menuItem(text = "Profitability",tabName = "profitability",icon = icon('dollar-sign')),
      menuItem(text = "Sales by County",tabName = "salesMap",icon = icon('map')),
      menuItem(text = "Sales by County Multi-Select",tabName = "salesMapMultiSelect",icon = icon('layer-group')),
      menuItem(text = "Association Rules",tabName = "associationRules",icon = icon("exchange"))
    )
  ),
  body = shinydashboard::dashboardBody(
    shinybusy::add_busy_spinner(height = "75px",width = "75px"),
    use_theme(mytheme),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "popularity",
                              sidebarPanel(
                                selectInput(inputId = "popularityVariable",
                                            label = "Select a variable",
                                            choices = c("vendor","description","category")),
                                selectInput(inputId = "popularityMeasure",
                                            label = "Order variable by...",
                                            choices = c('Total Sales in $',"Total Number of Sales")),
                                numericInput(inputId = "popularityNumber",
                                             label = "Number of items shown",
                                             value = 15,min = 1,step = 1)
                              ),
                              plotOutput(outputId = "popularityPlot")),
      shinydashboard::tabItem(tabName = "profitability",
                              h4("This tab implements the multiple selectInput option discussed in the lecture videos."),
                              sidebarPanel(selectInput(inputId = "profitabilityVariable",
                                                       label = "Select categories from variable...",
                                                       choices = c("vendor","description","category")),
                                           # this select input will be updated based on the
                                           # value of the popularityVariable input:
                                           selectInput(inputId = "profitabilityCategorySelection",
                                                       label = "Select one or more vendors:",
                                                       choices = unique(liquor$vendor),
                                                       multiple = TRUE,
                                                       selected = c('BACARDI USA INC','Brown Forman Corp.',
                                                                    'DIAGEO AMERICAS','E & J Gallo Winery',
                                                                    'FIFTH GENERATION INC','Heaven Hill Brands',
                                                                    'Jim Beam Brands','Laird & Company',
                                                                    'LUXCO INC','McCormick Distilling Co.',
                                                                    'PERNOD RICARD USA','Phillips Beverage',
                                                                    'PROXIMO','SAZERAC COMPANY  INC',
                                                                    'SAZERAC NORTH AMERICA')),
                                           selectInput(inputId = "profitabilityMeasure",
                                                       label = "Measure profit in terms of...",
                                                       choices = c("Total Profit in $",
                                                                   "Profit Margin"))),
                              column(width = 8,plotOutput(outputId = "profitabilityPlot"))
      ),
      tabItem(tabName = "salesMap",
              sidebarPanel(
                selectInput(inputId = "salesMapLiquor",
                            label = "Select a spirit",
                            choices = unique(liquor$description)),
                selectInput(inputId = "salesMapMeasure",
                            label = "Measure sales by...",
                            choices = c("Total Sales in $","Per Capita Sales in $"))
              ),
              column(width = 8,plotly::plotlyOutput(outputId = "salesMapPlot"))
      ),
      tabItem(tabName = "salesMapMultiSelect",
              h4("This tab is basically a hybrid between the Profitability
                 and Sales by County tabs"),
              sidebarPanel(
                selectInput(inputId = "salesMapMultiSelectVariable",
                            label = "Select categories from variable...",
                            choices = c("vendor","description","category")),
                # this select input will be updated based on the
                # value of the popularityVariable input:
                selectInput(inputId = "salesMapMultiSelectCategorySelection",
                            label = "Select one or more vendors:",
                            choices = unique(liquor$vendor),
                            multiple = TRUE,
                            selected = c('BACARDI USA INC','Brown Forman Corp.',
                                         'DIAGEO AMERICAS','E & J Gallo Winery',
                                         'FIFTH GENERATION INC','Heaven Hill Brands',
                                         'Jim Beam Brands','Laird & Company',
                                         'LUXCO INC','McCormick Distilling Co.',
                                         'PERNOD RICARD USA','Phillips Beverage',
                                         'PROXIMO','SAZERAC COMPANY  INC',
                                         'SAZERAC NORTH AMERICA')),
                selectInput(inputId = "salesMapMultiSelectMeasure",
                            label = "Measure sales by...",
                            choices = c("Total Sales in $",
                                        "Per Capita Sales in $"))
              ),
              column(width = 8,plotly::plotlyOutput(outputId = "salesMapMultiSelectPlot"))
      ),
      tabItem(tabName = "associationRules",
              h4("This tab shows basic functionality of a Market Basket Analysis.
                 We'll just print a table arranged by the highest-lift liquors.
                 Another popular visualization is a network graph."),
              sidebarPanel(
                selectInput(inputId = "associationRulesLiquor",
                            label = "Select a spirit",
                            choices = unique(liquor$description))
              ),
              dataTableOutput(outputId = "associationRulesTable"))
    )
  )
)

server <- function(input, output,session) {

  #######################################################################

  # Code for Popularity tab

  output$popularityPlot <- renderPlot({

    if(input$popularityVariable == "vendor"){

      if(input$popularityMeasure == 'Total Sales in $'){

        plt <- liquor %>%
          group_by(vendor) %>%
          summarise(saleTotal = sum(saleTotal)) %>%
          top_n(n = input$popularityNumber,wt = saleTotal) %>%
          ggplot(aes(x=reorder(vendor,saleTotal),y=saleTotal)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Sales in $",
               x = "Vendor") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$popularityMeasure == "Total Number of Sales"){

        plt <- liquor %>%
          group_by(vendor) %>%
          summarise(n = n()) %>%
          top_n(n = input$popularityNumber,wt = n) %>%
          ggplot(aes(x=reorder(vendor,n),y=n)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Number of Sales",
               x = "Vendor") +
          scale_y_continuous(labels = scales::comma)

      }

    }
    if(input$popularityVariable == "description"){

      if(input$popularityMeasure == 'Total Sales in $'){

        plt <- liquor %>%
          group_by(description) %>%
          summarise(saleTotal = sum(saleTotal)) %>%
          top_n(n = input$popularityNumber,wt = saleTotal) %>%
          ggplot(aes(x=reorder(description,saleTotal),y=saleTotal)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Sales in $",
               x = "Description") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$popularityMeasure == "Total Number of Sales"){

        plt <- liquor %>%
          group_by(description) %>%
          summarise(n = n()) %>%
          top_n(n = input$popularityNumber,wt = n) %>%
          ggplot(aes(x=reorder(description,n),y=n)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Number of Sales",
               x = "Description") +
          scale_y_continuous(labels = scales::comma)

      }

    }
    if(input$popularityVariable == "category"){

      if(input$popularityMeasure == 'Total Sales in $'){

        plt <- liquor %>%
          group_by(category) %>%
          summarise(saleTotal = sum(saleTotal)) %>%
          top_n(n = input$popularityNumber,wt = saleTotal) %>%
          ggplot(aes(x=reorder(category,saleTotal),y=saleTotal)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Sales in $",
               x = "Category") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$popularityMeasure == "Total Number of Sales"){

        plt <- liquor %>%
          group_by(category) %>%
          summarise(n = n()) %>%
          top_n(n = input$popularityNumber,wt = n) %>%
          ggplot(aes(x=reorder(category,n),y=n)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Number of Sales",
               x = "Category") +
          scale_y_continuous(labels = scales::comma)

      }

    }

    return(plt)

  })


  ##########################################################

  # Code for Profitability tab

  # A quick way to get a character vector of the top 15 categories in a variable:
  # liquor %>%
  #   group_by(description) %>%
  #   tally() %>%
  #   top_n(n = 15,wt = n) %>%
  #   pull(description) %>%
  #   paste0(collapse = '","')


  # observeEvent will re-execute the handlerExpr whenever the
  # eventExpr changes
  observeEvent(eventExpr = input$profitabilityVariable,
               handlerExpr = {

                 if(input$profitabilityVariable == "vendor"){

                   updateSelectInput(session = session,
                                     inputId = "profitabilityCategorySelection",
                                     label = "Select one or more vendors:",
                                     choices = unique(liquor$vendor),
                                     selected = c('BACARDI USA INC','Brown Forman Corp.',
                                                  'DIAGEO AMERICAS','E & J Gallo Winery',
                                                  'FIFTH GENERATION INC','Heaven Hill Brands',
                                                  'Jim Beam Brands','Laird & Company',
                                                  'LUXCO INC','McCormick Distilling Co.',
                                                  'PERNOD RICARD USA','Phillips Beverage',
                                                  'PROXIMO','SAZERAC COMPANY  INC',
                                                  'SAZERAC NORTH AMERICA'))

                 }

                 if(input$profitabilityVariable == "description"){

                   updateSelectInput(session = session,
                                     inputId = "profitabilityCategorySelection",
                                     label = "Select one or more spirits:",
                                     choices = unique(liquor$description),
                                     selected = c("Admiral Nelson Spiced","Black Velvet",
                                                  "Captain Morgan Original Spiced","Crown Royal",
                                                  "Crown Royal Regal Apple","Fireball Cinnamon Whiskey",
                                                  "Five O'Clock Vodka","Hawkeye Vodka",
                                                  "Hennessy VS","Jack Daniels Old #7 Black Label",
                                                  "Jim Beam","McCormick 80prf Vodka PET",
                                                  "Seagrams 7 Crown","Smirnoff 80prf",
                                                  "Titos Handmade Vodka"))

                 }

                 if(input$profitabilityVariable == "category"){

                   updateSelectInput(session = session,
                                     inputId = "profitabilityCategorySelection",
                                     label = "Select one or more categories:",
                                     choices = unique(liquor$category),
                                     selected = c('100% Agave Tequila','American Brandies',
                                                  'American Cordials & Liqueur','American Flavored Vodka',
                                                  'American Schnapps','American Vodkas',
                                                  'Blended Whiskies','Canadian Whiskies',
                                                  'Cocktails /RTD','Flavored Rum',
                                                  'Imported Vodkas','Spiced Rum',
                                                  'Straight Bourbon Whiskies','Tennessee Whiskies',
                                                  'Whiskey Liqueur'))

                 }

               })

  # the renderPlot code below was copied + pasted from the popularityPlot
  # created above and changed to reflect profitability

  output$profitabilityPlot <- renderPlot({

    if(input$profitabilityVariable == "vendor"){

      if(input$profitabilityMeasure == 'Total Profit in $'){

        plt <- liquor %>%
          filter(vendor %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 totalProfit = profit*numSold) %>%
          group_by(vendor) %>%
          summarise(totalProfit = sum(totalProfit)) %>%
          ggplot(aes(x=reorder(vendor,totalProfit),y=totalProfit)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Profit in $",
               x = "Vendor") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$profitabilityMeasure == "Profit Margin"){

        plt <- liquor %>%
          filter(vendor %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 profitMargin = profit/retail) %>%
          group_by(vendor) %>%
          summarise(profitMargin = mean(profitMargin)) %>%
          ggplot(aes(x=reorder(vendor,profitMargin),y=profitMargin)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Profit Margin",
               x = "Vendor") +
          scale_y_continuous(labels = scales::percent)


      }

    }
    if(input$profitabilityVariable == "description"){

      if(input$profitabilityMeasure == 'Total Profit in $'){

        plt <- liquor %>%
          filter(description %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 totalProfit = profit*numSold) %>%
          group_by(description) %>%
          summarise(totalProfit = sum(totalProfit)) %>%
          ggplot(aes(x=reorder(description,totalProfit),y=totalProfit)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Profit in $",
               x = "Description") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$profitabilityMeasure == "Profit Margin"){

        plt <- liquor %>%
          filter(description %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 profitMargin = profit/retail) %>%
          group_by(description) %>%
          summarise(profitMargin = mean(profitMargin)) %>%
          ggplot(aes(x=reorder(description,profitMargin),y=profitMargin)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Profit Margin",
               x = "Description") +
          scale_y_continuous(labels = scales::percent)

      }

    }
    if(input$profitabilityVariable == "category"){

      if(input$profitabilityMeasure == 'Total Profit in $'){

        plt <- liquor %>%
          filter(category %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 totalProfit = profit*numSold) %>%
          group_by(category) %>%
          summarise(totalProfit = sum(totalProfit)) %>%
          ggplot(aes(x=reorder(category,totalProfit),y=totalProfit)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Total Profit in $",
               x = "Category") +
          scale_y_continuous(labels = scales::dollar)

      }
      if(input$profitabilityMeasure == "Profit Margin"){

        plt <- liquor %>%
          filter(category %in% input$profitabilityCategorySelection) %>%
          mutate(profit = retail - cost,
                 profitMargin = profit/retail) %>%
          group_by(category) %>%
          summarise(profitMargin = mean(profitMargin)) %>%
          ggplot(aes(x=reorder(category,profitMargin),y=profitMargin)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme_bw() +
          labs(y = "Profit Margin",
               x = "Category") +
          scale_y_continuous(labels = scales::percent)


      }

    }

    return(plt)

  })



  ##########################################################

  # Code for Sales by County tab

  output$salesMapPlot <- plotly::renderPlotly({

    liquorFiltered <- liquor %>%
      filter(description == input$salesMapLiquor) %>%
      group_by(description,County) %>%
      summarise(saleTotal = sum(saleTotal))

    if(input$salesMapMeasure == "Total Sales in $"){

      plt <- iowaCounty %>%
        left_join(y = liquorFiltered,
                  by = c("subregion" = "County")) %>%
        mutate(text = paste0(subregion,"\n$",saleTotal)) %>%
        ggplot(aes(x = long,y = lat,text = text)) +
        geom_polygon(aes(group = group,fill = saleTotal),
                     colour = "gray50") +
        theme_void() +
        labs(fill = "Total Sale in $") +
        scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
        coord_fixed()

    }
    if(input$salesMapMeasure == "Per Capita Sales in $"){

      plt <- iowaCounty %>%
        mutate(subregion = toupper(subregion)) %>%
        left_join(y = liquorFiltered,
                  by = c("subregion" = "County")) %>%
        left_join(iowaCountyPop,
                  by = c("subregion" = "County" )) %>%
        mutate(perCapSales = saleTotal/Population,
               text = paste0(subregion,"\n$",round(perCapSales,2))) %>%
        ggplot(aes(x = long,y = lat,text = text)) +
        geom_polygon(aes(group = group,fill = perCapSales),
                     colour = "gray50") +
        theme_void() +
        labs(fill = "Per Capita Sales in $") +
        scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
        coord_fixed()

    }

    return(plotly::ggplotly(plt,tooltip = "text"))

  })


  #######################################################

  # Code for salesMapMultiSelect tab

  # Similar to the profitability tab, we'll allow the user to select multiple
  # descriptions/vendors/categories. We use the updateSelectInput function again

  observeEvent(eventExpr = input$salesMapMultiSelectVariable,
               handlerExpr = {

                 if(input$salesMapMultiSelectVariable == "vendor"){

                   updateSelectInput(session = session,
                                     inputId = "salesMapMultiSelectCategorySelection",
                                     label = "Select one or more vendors:",
                                     choices = unique(liquor$vendor),
                                     selected = c('DIAGEO AMERICAS','Heaven Hill Brands',
                                                  'Jim Beam Brands','SAZERAC COMPANY  INC'))

                 }

                 if(input$salesMapMultiSelectVariable == "description"){

                   updateSelectInput(session = session,
                                     inputId = "salesMapMultiSelectCategorySelection",
                                     label = "Select one or more spirits:",
                                     choices = unique(liquor$description),
                                     selected = c('Black Velvet','Fireball Cinnamon Whiskey',
                                                  'Hawkeye Vodka','Titos Handmade Vodka'))

                 }

                 if(input$salesMapMultiSelectVariable== "category"){

                   updateSelectInput(session = session,
                                     inputId = "salesMapMultiSelectCategorySelection",
                                     label = "Select one or more categories:",
                                     choices = unique(liquor$category),
                                     selected = c('American Vodkas','Canadian Whiskies',
                                                  'Straight Bourbon Whiskies','Whiskey Liqueur'))

                 }

               })


  output$salesMapMultiSelectPlot <-
    plotly::renderPlotly({

      if(input$salesMapMultiSelectVariable == "vendor"){

        liquorFiltered <- liquor %>%
          filter(vendor %in% input$salesMapMultiSelectCategorySelection) %>%
          group_by(vendor,County) %>%
          summarise(saleTotal = sum(saleTotal))

        if(input$salesMapMultiSelectMeasure == "Total Sales in $"){

          plt <- iowaCounty %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            mutate(text = paste0(subregion,"\n$",saleTotal)) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = saleTotal),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Total Sale in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ vendor)

        }
        if(input$salesMapMultiSelectMeasure == "Per Capita Sales in $"){

          plt <- iowaCounty %>%
            mutate(subregion = toupper(subregion)) %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            left_join(iowaCountyPop,
                      by = c("subregion" = "County" )) %>%
            mutate(perCapSales = saleTotal/Population,
                   text = paste0(subregion,"\n$",round(perCapSales,2))) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = perCapSales),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Per Capita Sales in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ vendor)

        }

      }

      if(input$salesMapMultiSelectVariable == "description"){

        liquorFiltered <- liquor %>%
          filter(description %in% input$salesMapMultiSelectCategorySelection) %>%
          group_by(description,County) %>%
          summarise(saleTotal = sum(saleTotal))

        if(input$salesMapMultiSelectMeasure == "Total Sales in $"){

          plt <- iowaCounty %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            mutate(text = paste0(subregion,"\n$",saleTotal)) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = saleTotal),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Total Sale in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ description)

        }
        if(input$salesMapMultiSelectMeasure == "Per Capita Sales in $"){

          plt <- iowaCounty %>%
            mutate(subregion = toupper(subregion)) %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            left_join(iowaCountyPop,
                      by = c("subregion" = "County" )) %>%
            mutate(perCapSales = saleTotal/Population,
                   text = paste0(subregion,"\n$",round(perCapSales,2))) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = perCapSales),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Per Capita Sales in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ description)

        }

      }

      if(input$salesMapMultiSelectVariable == "category"){

        liquorFiltered <- liquor %>%
          filter(category %in% input$salesMapMultiSelectCategorySelection) %>%
          group_by(category,County) %>%
          summarise(saleTotal = sum(saleTotal))

        if(input$salesMapMultiSelectMeasure == "Total Sales in $"){

          plt <- iowaCounty %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            mutate(text = paste0(subregion,"\n$",saleTotal)) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = saleTotal),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Total Sale in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ category)

        }
        if(input$salesMapMultiSelectMeasure == "Per Capita Sales in $"){

          plt <- iowaCounty %>%
            mutate(subregion = toupper(subregion)) %>%
            left_join(y = liquorFiltered,
                      by = c("subregion" = "County")) %>%
            left_join(iowaCountyPop,
                      by = c("subregion" = "County" )) %>%
            mutate(perCapSales = saleTotal/Population,
                   text = paste0(subregion,"\n$",round(perCapSales,2))) %>%
            ggplot(aes(x = long,y = lat,text = text)) +
            geom_polygon(aes(group = group,fill = perCapSales),
                         colour = "gray50") +
            theme_void() +
            labs(fill = "Per Capita Sales in $") +
            scale_fill_gradient(low = "white",high = "red",labels = scales::dollar) +
            coord_fixed() +
            facet_wrap(~ category)

        }

      }

      return(plotly::ggplotly(plt,tooltip = "text"))

    })


  #########################################################

  # Code for associationRules tab

  output$associationRulesTable <- renderDataTable({

    selectedLiquorSaleID <- liquor %>%
      select(saleID,description) %>%
      filter(description == input$associationRulesLiquor) %>%
      distinct() %>%
      pull(saleID) %>%
      unique()

    salesIncludingSelectedLiquor <- liquor %>%
      filter(saleID %in% selectedLiquorSaleID) %>%
      select(saleID,description) %>%
      arrange(saleID) %>%
      distinct()

    ret <- salesIncludingSelectedLiquor %>%
      group_by(description) %>%
      summarise(n = n()) %>%
      mutate(support = n/length(allSaleID),
             confidence = support/(length(selectedLiquorSaleID)/length(allSaleID))) %>%
      left_join(liquorYFrequency,
                by = "description") %>%
      mutate(lift = confidence/(freq/length(allSaleID))) %>%
      select(-c(support,n,freq)) %>%
      arrange(desc(lift),desc(confidence)) %>%
      filter(description != input$associationRulesLiquor)

    return(ret)

  })

}

# Run the application
shinyApp(ui = ui, server = server)
