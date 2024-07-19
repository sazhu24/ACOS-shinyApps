## Amherst College SDG Course Mapping Tool
## Created by Sara Zhu (saraz2069@gmail.com), last updated July 2024

## includes code adapted from the following sources:
# https://cran.r-project.org/web/packages/text2sdg/vignettes/text2sdg.html
# https://github.com/USC-Office-of-Sustainability/SustainabilityCourseFinder

# load required packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(stringr)
library(purrr)
library(htmltools)
library(reactable)
library(dipsaus)
library(openxlsx)

## import data
courses_table <- read.xlsx('data.xlsx') %>% 
  # nest sdg scores and sdg keywords to tidy data
  nest(sdg_score = c(score1:score16),
       sdg_keywords = c(keywords1:keywords16))

## set options for drop-down inputs

# 1. for drop-down input: SDGs 1-16
sdg_values <-
  c(
    "SDG 1: No Poverty" = 1,
    "SDG 2: Zero Hunger" = 2,
    "SDG 3: Good Health and Well-being" = 3,
    "SDG 4: Quality Education" = 4,
    "SDG 5: Gender Equality" = 5,
    "SDG 6: Clean Water and Sanitation" = 6,
    "SDG 7: Affordable and Clean Energy" = 7,
    "SDG 8: Decent Work and Economic Growth" = 8,
    "SDG 9: Industry, Innovation and Infrastructure" = 9,
    "SDG 10: Reduced Inequality" = 10,
    "SDG 11: Sustainable Cities and Communities" = 11,
    "SDG 12: Responsible Consumption and Production" = 12,
    "SDG 13: Climate Action" = 13,
    "SDG 14: Life Below Water" = 14,
    "SDG 15: Life on Land" = 15,
    "SDG 16: Peace and Justice Strong Institutions" = 16
  )

# 2. for drop-down input: departments and abbreviations
depts <-
  c(
    "All Departments" = "All Departments",
    "American Studies" = "AMST",
    "Anthropology" = "ANTH",
    "Architectural Studies" = "ARCH",
    "Art and the History of Art" = "ARHA",
    "Asian Languages and Civilizations" = "ASLC",
    "Astronomy" = "ASTR",
    "Biochemistry and Biophysics" = "BCBP",
    "Biology" = "BIOL",
    "Black Studies" = "BLST",
    "Chemistry" = "CHEM",
    "Classics" = "CLAS",
    "Colloquia" = "COLQ",
    "Computer Science" = "COSC",
    "Economics" = "ECON",
    "Educational Studies" = "EDST",
    "English" = "ENGL",
    "Environmental Studies" = "ENST",
    "European Studies" = "EUST",
    "Film and Media Studies" = "FAMS",
    "French" = "FREN",
    "Geology" = "GEOL",
    "German" = "GERM",
    "History" = "HIST",
    "Latinx and Latin American Studies" = "LLAS",
    "Law, Jurisprudence, and Social Thought" = "LJST",
    "Mathematics" = "MATH",
    "Music" = "MUSI",
    "Neuroscience" = "NEUR",
    "Philosophy" = "PHIL",
    "Physics" = "PHYS",
    "Political Science" = "POSC",
    "Psychology" = "PSYC",
    "Religion" = "RELI",
    "Russian" = "RUSS",
    "Sexuality Women's & Gender Studies" = "SWAG",
    "Sociology" = "SOCI",
    "Spanish" = "SPAN",
    "Statistics" = "STAT",
    "Theater and Dance" = "THDA"
  )

# 3. for drop-down input: semesters
offered <- c("ALL SEMESTERS", "SPRING 2024", "FALL 2023", "SPRING 2023", "FALL 2022")


############
#    ui    #
############

ui <- navbarPage(
  
  # set page theme based on 'Lux': https://bootswatch.com/lux/
  theme = bs_theme(
    base_font = font_google("Jost"),
    heading_font = font_google("Jost"),
    version = 4,
    bootswatch = "lux"
  ),

  # add mammoth SDG logo to page title
  # this appears as an image in the top left corner
  title = div(style = "margin-top: - 6px;",
              img(src = "mammothSDG_logo.png", height = 70)),
  
  ### TAB 1: MAIN TABLE
  tabPanel(
  
    # app title that appears on browser tab
    title = span("Course Mapping", style = "font-size:15px; width: 200px"),
    
    tags$head(
      # add google analytics tags
      includeHTML(("google-analytics.html")),
      includeHTML(("google-tag-manager.html")),
      
      # import CSS stylesheet
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # include line for google analytics tag
    tags$noscript(
      tags$iframe(
        src = 'https://www.googletagmanager.com/ns.html?id=GTM-M8F65B4S',
        height = '0', width = '0',
        style = 'display:none; visibility:hidden'
      )
    ),
    
    # use javascript to add a google analytics event listener 
    # this event listener tracks tab clicks 
    tags$script(
      HTML(
        "document.querySelector('#methodology_tab').addEventListener('click', () => {
          gtag('event', 'view_tab', { 'section_name': 'tab 3' });
        })"
        )
      ),

    # main panel with outputs
    mainPanel(
      # format page: center content
      width = "100%",
      align = "center",
      
      # add main logo: mammoth in SDG wheel 
      div(
        class = 'box',
        style = "padding: 20px 0 10px 0; width: 87%; style: inline-block; border-radius: 5px; text-align: center",
        img(
          id = 'logo-mobile',
          src = "mammothSDG_logo.png",
          height = 85,
          style = ''
        ),
        # render SDG logo + text based on selected SDG
        uiOutput(outputId = "banner_logo", style = "margin-right:20px; display: inline-block"),
        uiOutput(outputId = "banner_text", style = "margin-top:-12px; display: inline-block")
      ),
      
      # control bar with drop-down inputs for customizing table output
      div(
        id = 'control-bar', style = "margin: 35px 0 0 0; width: 90%",
        
        div(
          id = 'selectInputs', style = 'display: flex;',
          
          ## drop-down input for filtering by SDG
          pickerInput(
            inputId = "sdg",
            label = div("", style = 'text-decoration: underline; font-size:14px; font-weight: bold'),
            choices = sdg_values,
            options = list(`style` = "btn-info"),
            # set default SDG as "SDG 13: Climate Action"
            selected = sdg_values[13],
            width = "90%"
          ),
          
          ## drop-down input for filtering by department
          pickerInput(
            inputId = "dept_picker",
            label = tags$div("", style = 'font-size:10.5px; font-style: bold'),
            choices = depts,
            options = list(`style` = "btn-danger"),
            width = "90%"
          ),
          
          ## drop-down input for filtering by semester
          pickerInput(
            inputId = "offered_button",
            label = div("", style = 'text-decoration: underline; font-size:14px; font-weight: bold'),
            choices = offered,
            options = list(`style` = "btn-info"),
            selected = "ALL SEMESTERS",
            width = "90%"
          )
          
        )
      ),
      
      # new div section positioned below the control bar
      div(
        id = 'table_div', style = "margin: 10px 6% 50px 6%;",
      
        ## table output
        reactableOutput("courses_table"),
        
      ),
      
    )
    
  ),

  
  ### TAB 2: METHODOLOGY
  tabPanel(
    
    # place title in span to customize CSS
    title = span(
      "Methodology",
      id = 'methodology_tab', 
      style = "font-size:15px; width: 200px",
      ),
    
    mainPanel(
      width = "100%", 
      align = "center",
      
      ## text box with paragraphs describing the methodology and tool
      div(
        class = "methodology",
        style = "display: inline-block; margin-top: 0px; width:80%; margin-left:2%; font-size: 15px; line-height: 1.5; text-align:left; font-family:Jost",
          
        # ABOUT
        h5("About", class = "m-header"),
        paste("The Office of Sustainability at Amherst College created this app to promote sustainability in the college's curriculum by mapping Amherst course descriptions to the "),
        a("United Nations Sustainable Development Goals (SDGs).", href="https://sdgs.un.org/goals"), paste("NOTE: This tool is intended to be used as an exploratory tool only."),
        br(),
        
        # METHODOLOGY
        h5("Methodology", class = "m-header"),
        paste("Course descriptions from the 22-23 school year were scraped from Amherst's online course catalog. Each course description was then run through an algorithm that searches for key words and phrases specific to each SDG (for this part, we used a modified version of the "),
        a("keyword list published by USC's Office of Sustainability).", href="https://github.com/USC-Office-of-Sustainability/USC-SDGmap/blob/main/Data/cmu_usc_pwg_SDG_Keywords_corrected_10_11_22.csv"), paste("The top scoring courses for each SDG are presented in the course mapping section of this tool."),
        p(style = "margin-top:15px", "As noted earlier, this tool is designed to be used as an exploratory tool only. The results were generated by a predictive algorithm and have not undergone a systematic review process."),
        
        # INTERPRETING RESULTS
        h5("Interpreting Results", class = "m-header"),
        p(style = "margin-top: 0px;", "Courses that score high in an SDG category have course descriptions that include many of the keywords that are frequently associated with that SDG. For example, the highest scoring courses for SDG 2: Zero Hunger most likely have course descriptions that contain several of the following key words: agriculture, hunger, global, food, systems, political, development, and malnutrition. 
                  Our model predicts that these courses are most likely to cover content related to SDG 2 based on their course descriptions. However, just because a course does not score highly for a particular SDG does not necessarily mean that the course does not align with that SDG. It is important to remember that this is a predictive model, so any predictions generated by the model are limited by the data it takes as input.  
                  Bearing these shortcomings in mind, we believe this platform can still serve as a valuable starting point for students who are interested in exploring courses that relate to particular SDGs or the sustainable development goals as a whole."),
        br(), 
        br(),
        
        # FOOTER
        div(
          style = "margin-bottom:25px; color:#757575; font-weight:bold",
          paste("If you have any further questions, comments, or feedback, please contact the Office of Sustainability at"),
          span(style = "font-weight: normal; color: #0e67ed; display: inline", "sustainability@amherst.edu.")
          )
        
        )
    )
    
  ),
  
  
)

##########
# server #
##########

server <- function(input, output, session){
  
  ## pop-ups
  
  # about modal
  aboutModal <- function(failed = FALSE) {
    
    modalDialog(
      div(class = "modal-header", style = "font-size:18px; font-family:Jost; font-weight:600", p("Welcome!")),
      div(style = "width:90%; margin: 10px auto -3px auto", hr()),
      
      div(id = 'modal1', 
          
          paste("Created by Amherst's Office of Sustainability, this app uses a keyword detection algorithm to map course description text from Amherst's course catalog to the "),
          a("United Nations Sustainable Development Goals (SDGs).", href="https://sdgs.un.org/goals"),
          #h6("How To Use the Tool", class = "m-header"),
          
          #paste("The Office of Sustainability at Amherst College created this app to help students visualize interdisciplinary pathways through the college's curriculum that align with the"),
          #a("United Nation’s Sustainable Development Goals.", href="https://sdgs.un.org/goals"),
          div(style = 'margin-top:15px', 
              h6("Instructions", style = "text-align:center"),
              paste("Use the search inputs to view course offerings mapped to a particular SDG, or to filter results by department or semester. Feel free to click on any course to view more details."),
              p(style = "margin-top:14px", "NOTE: This tool is intended to be used as an exploratory tool only. Please see the methodology section to learn more about the course mapping process.")
          )
      ),
      div(style = "text-align:center; margin: 15px 13px 5px 13px; font-family:Cabin Sketch; color: grey; font-family:16px; font-weight:550",
          img(id = "logo", src="mammoth.png", width=25, style = 'margin-bottom:8px')
      ),
      footer = NULL,
      easyClose = TRUE
    )
  }
  
  showModal(aboutModal())
  
  ## reactive content
  
  # selected_sdg is a reactive variable 
  # referencing the requested SDG number 
  selected_sdg <- reactive({
    paste(gsub("weight", "", input$sdg))
  })
  
  # selected_sdg is a reactive variable 
  # referencing the requested semester
  selected_semester <- reactive({
    if(input$offered_button != 'ALL SEMESTERS'){
      if(grepl('SPRING', input$offered_button)){
        season <- 'S'
      } else {
        season <- 'F'
      }
      
      year <- str_extract(input$offered_button, "([0-9]{2})$")
      paste0(season, year)
      
    } else {
      "F22 - S24"
    }
  })
  
  ## generate header image for requested SDG
  # this output spans the entire width of the page
  
  # render SDG logo image based on selected_sdg
  output$banner_logo <- renderUI({ 
    tags$img(src = paste0("headers/E_WEB_INVERTED_", selected_sdg(), ".png"), class = "banner_logo", width = "85px")
  })
  
  # render SDG text image based on selected_sdg
  output$banner_text <- renderUI({ 
    tags$img(src = paste0("headers/BA", selected_sdg(), ".png"), class = "banner_text", height = "83px", style = "margin-top:-12px")
  })
  
  ## return the requested dataset ----
  # note that we use reactive() here, which depends on:
  # input$offered_button, input$dept_picker, selected_sdg(), selected_semester()
  DF <- reactive({
    
    # filter and restructure dataset based on 'selected_sdg'
    DF <- courses_table %>%
      hoist(sdg_score, 'weight' = paste0('score', selected_sdg())) %>%
      hoist(sdg_keywords, 'keywords' = paste0('keywords', selected_sdg())) %>%
      filter(weight != '') %>%
      mutate(weight = as.integer(weight)) %>% 
      arrange(-weight) %>% 
      add_column(w1 = NA) %>% 
      relocate(w1, .before = keywords) 
    
    # filter dataset by semester based on 'selected_semester'
    # default table shows all semesters
    if(input$offered_button != "ALL SEMESTERS"){
      DF <- DF %>% 
        filter(grepl(selected_semester(), semesters))
    }
    
    # filter dataset by department based on 'input$dept_picker'
    # default table shows all departments
    if(input$dept_picker != "All Departments"){
      DF <- DF %>% 
        filter(grepl(input$dept_picker, depts))
    }
    
    # reformat as new dataframe
    ex_t <- data.frame(
      # transpose dataset with selected columns
      t(DF[, c('course', 'semesters', 'professors', 'keywords', 'description', 'goals')])
      )
    
    # set column and row names
    rownames(ex_t) <- NULL
    colnames(ex_t) <- DF$course
    
    # bind 'ex_t' to new dataframe with labeled rows
    tbl <- data.frame(
      rows = c('Course', 
               'Offered \n During', 
               'Instructor(s)',
               paste0('SDG ', selected_sdg(), ' \n Keywords'), 
               'Course \n Description',
               'All Mapped \n SDGs')) %>% 
      # bind 'ex_t' as new columns
      cbind(ex_t)
    
    # divide into two data frames: 
    # 1. main - information displayed on main table
    # 2. expanded - information displayed when a row is clicked
    
    main <- DF %>% # 1
      select(course, depts, goals, w1, keywords)

    expanded <- tbl # 2
    
    # place both data frames in a list
    # to retrieve when DF is called in output$courses_table
    table_list <- list(
      main = main, 
      expanded = tbl
      )
    
    return(table_list)
    
  })
  
  
  # generate table from reactive dataset returned by DF ----
  output$courses_table <- renderReactable({
    
    # extract 'main' dataset (displayed on main table)
    main <- DF()$main
    
    # extract 'expanded' dataset (displayed when a row is clicked)
    expanded <- DF()$expanded
    
    # create table with 
    reactable(
      main,
      class = "tbl",
      defaultPageSize = 20,
      searchable = TRUE,
      
      defaultColDef = colDef(
        # customize table header
        headerClass = "header",
        header = function(value) {
          # only apply function to first column
          if (!grepl('Keywords', value) & value != '') {
            # customize header output based on selected_sdg()
            units <-
              div(style = "color: #999; font-size:13px", paste("Mapped to SDG ", selected_sdg(), sep = ""))
            div(title = value, value, units)
          }
        }
      ),
      
      theme = reactableTheme(
        # set cell style
        cellStyle = list(
          # flex table along columns page resizes
          display = "flex", 
          flexDirection = "column",
          # center content in cells
          justifyContent = "center"
        )
      ),
      
      # customize settings for table columns
      columns = list(
        
        # 1. course name and information
        course = colDef(
          # add semester label based on selected_semester()
          name = paste0("Courses (", selected_semester(), ")"),
          class = "col1",
          # create label with course name and course department
          cell = function(value, index) {
            # get department from table using row index
            tag <- main$depts[index]
            tag <- if (!is.na(tag)) tag else "Unknown"
            # format label
            div(
              # course name on top
              div(
                style = list(fontWeight = 600, fontSize = 15, color = "black"), 
                value
                ),
              # course department on bottom
              div(
                style = list(fontSize = 12, color = "#828282"), 
                tag
                )
              )
          },
          # expand table with more course details when a row is selected
          details = function(index) {
            # get information from 'expanded' using selected row index
            tbl <- expanded[-1, c(1, (index + 1))]
            # format table for display
            colnames(tbl) <- c('rows', 'values') 
            tbl$values_mobile <- map2_chr(toupper(tbl$rows), tbl$values, ~ str_glue("{.x}: {.y}"))
            
            htmltools::div(style = "padding-bottom:15px",
                           reactable(
                             tbl,
                             class = "expand",
                             columns = list(
                               rows = colDef(
                                 name = "",
                                 width = 130,
                                 cell = function(value) {
                                   div(style = "font-weight: bold", value)
                                 }
                               ),
                               values = colDef(name = ""),
                               values_mobile = colDef(name = "")
                             )
                           ))
          }
        ),
        
        # 2. SDG logo
        w1 = colDef(
          # set name as blank so this column has no header
          name = "",
          class = "col2",
          maxWidth = 76,
          cell = function(value) {
            htmltools::tags$img(
              # generate source url for image based on selected_sdg()
              src = paste('sdgs/', selected_sdg(), '.png', sep = ''),
              # make image a square 76 x 76 
              height = 76,
              width = 76
            )
          }
        ),
        
        # 3. keywords
        keywords = colDef(
          name = "Detected Keywords",
          class = 'col3',
          minWidth = 300
        ),
        
        # hide these columns
        depts = colDef(show = FALSE),
        goals = colDef(show = FALSE)
        
      )
    )
  })
  
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)