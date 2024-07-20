## AAS Projects Database
## Created by Sara Zhu (saraz2069@gmail.com), last updated July 2024

# load required packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinyalert)
library(shinyjs)
library(reactable)
library(htmltools)
library(openxlsx)
library(googlesheets4)
library(tidyverse)

gs4_deauth()

# read data from google sheet
project_df <- as_tibble(
  read_sheet("https://docs.google.com/spreadsheets/d/1hiluBkMV81ZDxZzl449kp1qAh7Wux6jqrewHACRJaXs")
)


############
#    ui    #
############

ui <- fluidPage(
  # set page theme based on 'Lux': https://bootswatch.com/lux/
  theme = bslib::bs_theme(
    base_font = bslib::font_google("Jost"),
    heading_font = bslib::font_google("Jost"),
    version = 4,
    bootswatch = "lux"
  ),
  
  # set page title that appears on tab
  title = "AAS Projects",

  tabPanel(
    title = "AAS Projects",
    
    tags$head(
      # add tags for google analytics
      includeHTML(("google-analytics.html")),
      includeHTML(("google-tag-manager.html")),
      # import css style sheet
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tags$noscript(
      # add iframe for google analytics tags
      tags$iframe(
        src = 'https://www.googletagmanager.com/ns.html?id=GTM-PJPKFSDB',
        height = '0', width = '0',
        style = 'display:none;visibility:hidden'
      )
    ),
    
    # add line to enable shinyjs functions
    useShinyjs(),
    
    mainPanel(
      # set panel format
      width = "100%",
      align = "center",
      
      div(
        class = "navbar",
        div(
          style = "display: inline-block; text-align:center; margin: 0px auto;",
          # AAS logo, positioned to the left of header
          img(id = "logo", src = "header.png", height = 100),
          # horizontal line
          div(style = "margin-bottom: -20px;", hr()),
          # about button, shows pop-up when clicked
          div(
            class = "btn",
            id = 'about_btn',
            actionBttn(
              inputId = "about",
              label = "about",
              style = "stretch",
              color = "danger"
            )
          ),
          # section with drop-down inputs
          div(
            style = "text-align:center; margin-top:-20px;",
            ## drop-down for project category
            div(
              class = "bar",
              id = "all_projects",
              style = "",
              pickerInput(
                inputId = "category_select",
                label = "",
                choices = c(
                  "All Categories",
                  "Sustainability",
                  "Arts",
                  "Community Engagement",
                  "Academics",
                  "Wellbeing",
                  "Equity & Inclusion",
                  "Dining",
                  "Transportation"
                ),
                options = list(`style` = "btn-info")
              )
            ),
            ## drop-down for project status
            div(
              class = "bar",
              id = "all_categories",
              style = "",
              pickerInput(
                inputId = "status_select",
                label = "",
                choices = c(
                  "All Projects", 
                  "In Progress", 
                  "Completed"
                  ),
                options = list(`style` = "btn-info")
              )
            )
          )
        )
      ),
      # main table
      div(class = "main", reactableOutput("master_table")),
      # mammoth logo, positioned on bottom right of page
      div(id = "mammoth", img(src = "mammoth.png", height = 50))
    )
    
    
  )
  
  
)

############
# server   #
############

server <- function(input, output, session){
  
  # create 'about' pop-up introducing the page
  aboutModal <- function(failed = FALSE) {
    modalDialog(
      div(style = 'padding:15px 10px 15px 30px; font-size:15px; font-family:Jost; line-height:1.7; font-weight:500',
          div(style = "text-align:center; margin-bottom: 12px; color:black; text-transform:uppercase; font-size:20px; font-weight:600;", p("Welcome to the AAS Projects Database!")),
          paste("This searchable database is intended to provide ideas for future AAS funded projects (Idea), share a list of current and ongoing AAS funded projects (In Progress), and highlight past AAS funded projects (Completed). Click on the Project to see a short project summary. Contact AAS for more information and how to get involved - add AAS email.")
      ),
      footer = modalButton("dismiss"),
      easyClose = TRUE
    )
  }
  
  # create 'sustainability' pop-up to redirect users to the Office of Sustainability App
  susModal <- function(failed = FALSE) {
    modalDialog(
      div(
        style = 'padding:30px 10px 30px 20px; font-size:17px; font-family:Jost; line-height:1.7; text-align:center; font-weight:500',
        paste("To learn more about sustainability-related projects on campus, check out the "), span(style = "font-weight:700; color:#529c60; font-size:19px", a("Sustainability Projects Database", href = "https://sazhu24.shinyapps.io/ACOS-project-dashboard/", target = '_blank')), paste(" hosted by The Office of Sustainability.")
      ),
      footer = modalButton("dismiss"),
      easyClose = TRUE
    )
  }
  
  # show 'about' pop-up when page loads
  showModal(aboutModal())
  
  # show 'about' pop-up when 'about' button is clicked
  onclick("about", showModal(aboutModal()))
  
  # create table
  table <- project_df %>%
    select(project,
           senator,
           status,
           total_cost,
           year,
           category,
           description) %>%
    
    mutate(
      # set NA values equal to blank
      across(1:5, ~ ifelse(is.na(.x)|.x == 'NA', "", .x)),
      # make year column characters 
      year = as.character(year),
      # clean description column
      description = iconv(description, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
      # if column is not empty, add dollar sign prefix and reformat
      total_cost = ifelse(total_cost == '', '',
                          paste0("$", formatC(
                            as.numeric(total_cost),
                            format = "d",
                            big.mark = ","
                          )))
    )
  
  
  observe({
    
    ## filter table based on selected category (from drop down)
    if (input$category_select == "All Categories") {
      # show all categories
    } else if (input$category_select == "Sustainability") {
      # show pop-up redirecting to office of sustainability app 
      showModal(susModal())
      
      # filter table by category
      table <- table %>%
        filter(grepl(input$category_select, category))
      
    } else {
      # filter table by category
      table <- table %>%
        filter(grepl(input$category_select, category))
    }
    
    ## filter table based on selection (from drop down)
    if (input$status_select == "All Projects") {
      # show all projects
    } else {
      # filter table by status
      table <- table %>%
        filter(grepl(input$status_select, status))
    }
    
    
    ## create table output
    output$master_table <- renderReactable({
      reactable(
        table,
        class = "tbl",
        # set class
        showPageInfo = FALSE,
        searchable = TRUE,
        # make table searchable
        
        theme = reactableTheme(
          # make columns flexible and center content
          cellStyle = list(
            display = "flex",
            flexDirection = "column",
            justifyContent = "center"
          ),
          # add yellow accent line to selected rows
          rowSelectedStyle = list(
            boxShadow = "inset 2px 0 0 0 #ffa62d"
          )
        ),
        # show expanded row with description when a row is clicked
        details = function(index) {
          # create expand, a data frame containing the 'description' column for the selected row
          expand <- table[index, "description"]
          # if expand contains content, expand row and display 'description'
          if (!is.na(expand[, 1])) {
            htmltools::div(style = "padding-bottom:15px; padding-top:10px",
                           reactable(
                             expand,
                             class = "expand",
                             columns = list(description = colDef(name = ""))
                           ))
          }
        },
        
        defaultColDef = colDef(headerClass = "my-header"),
        columns = list(
          description = colDef(show = FALSE), # hide column
          category = colDef(show = FALSE), # hide column
          project = colDef(class = "col1", minWidth = 200), # col1, set minimum width
          senator = colDef(class = "col2", minWidth = 150), # col2, set minimum width
          status = colDef(class = "col3"), # col3
          total_cost = colDef(name = "funding", class = "col3"), # col3
          year = colDef(class = "col3", maxWidth = 100) # col3, set minimum width
        )
      )
    })
    
    
  })
  
  
  
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

