library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(bslib)
library(shinyWidgets)
library(stringr)
library(shinyBS)
library(reactable)
library(htmltools)
library(dipsaus)
library(shinyalert)
library(shinyjs)
library(openxlsx)
library(googlesheets4)
library(remotes)
library(gargle)
library(conflicted)

# prefer filter from dplyr package
conflicts_prefer(dplyr::filter)

# authenticate with gargle using read-only token
options(gargle_oauth_cache = ".secrets")

gs4_auth(
  email = "saraz2069@gmail.com",
  cache = ".secrets", 
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
  )

# read projects from google sheet
project_df <- tibble::as_tibble(
  read_sheet("https://docs.google.com/spreadsheets/d/1EzIMb-RsjX4bvsZ6rt-GTxEzjzRCW9x9HxrFdS0LnZY")
)

############
#    ui    #
############

ui <- fluidPage(
  # set page theme based on 'Lux': https://bootswatch.com/lux/
  theme = bs_theme(
    base_font = font_google("Jost"),
    heading_font = font_google("Jost"),
    version = 4,
    bootswatch = "lux"
  ),
  
  # add page title that appears on tab
  title = "Sustainability Project Ideas",
  
  tabPanel(
    # tab title
    title = "Projects",
    
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
        src = 'https://www.googletagmanager.com/ns.html?id=GTM-KMKSCLPK',
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
      
      # new row - office of sustainability logo
      div(
        style = "text-align:center; margin-top:15px; margin-bottom:5px", 
        img(src = "sus_herd.png", height = 90)
      ),
      
      # new row - page title
      div(
        style = "color:black; font-weight:500; font-family: 'Bebas Neue', cursive; font-size:40px; margin: 10px 0 -15px 0", 
        p("Sustainability Project Ideas")
        ),
      
      # new row - horizontal line 
      # top border of the control bar
      div(
        style = "width:60%; margin-bottom:-10px", 
        hr()
        ),
      
      # make all text in this div uppercase
      div(
        style = "font-size: 12px; text-transform:uppercase;",
        
        # new row - action buttons for generating pop-ups
        div(
          style = "display:inline",
          
          # 'about' button
          # opens ABOUT pop-up with instructions for navigating the tool
          actionBttn(
            inputId = "about",
            label = div(class = "menu", "about"),
            style = "simple",
            color = "primary"
          ),
          # 'get_involved' button
          # opens PROJECT INTEREST form
          actionBttn(
            inputId = "get_involved",
            label = div(id = "highlight", class = "menu", "interest form"),
            style = "simple",
            color = "primary"
          ),
          # 'propose' button
          # opens PROPOSAL form
          actionBttn(
            inputId = "propose",
            label = div(class = "menu", "propose a new project"),
            style = "simple",
            color = "primary"
          )
          
        ),
        
        # new row - horizontal line 
        # bottom border of the control bar
        div(
          style = "width:60%; margin-top:-8px; ", 
          hr()
          ),
        
        # new row - drop-down input for category selection
        div(
          style = "margin-top:15px",
          # drop-down input for filtering the table by project category
          pickerInput(
            inputId = "category_select",
            label = "",
            choices = c("All Categories", unique(project_df$category)),
            width = "50%",
            options = list(style = "btn-secondary"),
            # add icon for each choice
            choicesOpt = list(
              content = c(
                "<img style='display:inline; margin-right:8px' src='earth.png' height=18</img><p style='display:inline'>All Categories</p>",
                "<img style='display:inline; margin-right:10px' src='climate-energy.png' height=20</img><p style='display:inline'>Climate & Energy</p>",
                "<img style='display:inline; margin-right:10px' src='engagement.png' height=20</img><p style='display:inline'>Engagement</p>",
                "<img style='display:inline; margin-right:10px' src='transportation.png' height=20</img><p style='display:inline'>Transportation</p>",
                "<img style='display:inline; margin-right:10px' src='procurement-waste.png' height=20</img><p style='display:inline'>Procurement & Waste</p>",
                "<img style='display:inline; margin-right:10px' src='equity-inclusion.png' height=20</img><p style='display:inline'>Equity & Inclusion</p>",
                "<img style='display:inline; margin-right:10px' src='academics.png' height=20</img><p style='display:inline'>Academics</p>",
                "<img style='display:inline; margin-right:10px' src='grounds.png' height=20</img><p style='display:inline'>Grounds</p>",
                "<img style='display:inline; margin-right:10px' src='food-dining.png' height=20</img><p style='display:inline'>Food & Dining</p>",
                "<img style='display:inline; margin-right:10px' src='buildings.png' height=20</img><p style='display:inline'>Buildings</p>",
                "<img style='display:inline; margin-right:10px' src='wellbeing.png' height=20</img><p style='display:inline'>Wellbeing</p>"
              )
            )
          )
        ),
        # new row - radio buttons for status selection
        div(
          style = "margin-bottom:-5px; margin-top:-15px; font-size:13px; margin-left:1%; font-family:Jost",
          # radio buttons for filtering the table by project status
          prettyRadioButtons(
            inputId = "type_select",
            label = "",
            choices = c("All Projects", unique(project_df$status)),
            status = "warning",
            animation = "pulse",
            width = "90%",
            inline = TRUE
          )
        ),
      ),
      
      # line break
      br(),
      
      # table output
      div(
        style = "text-align:center; margin-bottom:10px",
        reactableOutput("master_table")
        )
      
    )
    
  )
  
)

############
# server   #
############

server <- function(input, output, session){
  
  ### ABOUT POP-UP ###
  aboutModal <- function(failed = FALSE) {
    modalDialog(
      div(style = 'font-size:15px; font-family:Jost; line-height:1.7; font-weight:500; padding:15px 25px;',
          div(style = "text-align:center; margin-bottom: 12px; color:black; text-transform:uppercase; font-size:16.2px; font-weight:800;", 
              p("Welcome to the Sustainability Projects Database!")),
          paste("Hosted by the Office of Sustainability, this searchable database is intended to provide ideas for potential sustainability projects (Available), share a list of current and ongoing sustainability projects (In Progress), and highlight some past campus sustainability efforts (Completed). If you would like to get involved with or learn more about any of these projects, simply check the box next to the project, click on the “Get Involved” button, complete the interest form, and the Office of Sustainability will follow up with you!")
      ),
      footer = modalButton("ok"),
      easyClose = TRUE
    )
  }
  
  # show ABOUT pop-up when page loads
  showModal(aboutModal())
  
  # show ABOUT pop-up when 'about' button is clicked
  observeEvent(input$about, {
    showModal(aboutModal())
  })
  
  
  ### GENERATE MAIN TABLE ###
  master_table <- project_df
  
  ## render table based on inputs ----
  # depends on: input$category_select, input$type_select
  observe({
    
    # filter dataset by category based on 'input$category_select'
    # default table shows all projects
    if(input$category_select != "All Categories"){
      master_table <- master_table %>% 
        filter(category == input$category_select)
    }
    
    # filter dataset by status based on 'input$type_select'
    # default table shows all projects
    if(input$type_select != "All Projects"){
      master_table <- master_table %>% 
        filter(status == input$type_select)
    }
    
    # select and reformat columns
    table <- master_table[,c("project", "category", "description", "summary_description", "status")] %>% 
      mutate(images = paste0(tolower(gsub(" & ", "-", category)), ".png", sep = "")) %>% 
      relocate(images, .after = "project") %>% 
      select(-category) %>% 
      mutate(project = gsub("And", "and", project),
             more = description,
             description = iconv(description, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
             summary_description = iconv(summary_description, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
             # use first sentence if summary description is empty
             summary_description = ifelse(is.na(summary_description), gsub("\\..*", "", description), summary_description)) 
    
    # create 'expanded', a data frame containing additional info on projects
    # this information is only displayed when a row is clicked 
    expanded <- table %>% select(more)
    
    # create table output
    output$master_table <- renderReactable({
      reactable(table, 
                class = "tbl", 
                selection = "multiple", 
                onClick = "select", 
                showPageInfo = FALSE, 
                searchable = TRUE,
                
                theme = reactableTheme(
                  # make columns flexible with content centered
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
                
                defaultColDef = colDef(
                  # customize table header based on column
                  headerClass = "my-header",
                  header = function(value) {
                    if (value == "Project") {
                      div(title = value, value, style = "padding-left:50px")
                    } else if (value == "Description") {
                      div(title = value, value, style = "")
                    } else if (value == "Status") {
                      div(title = value, value, style = "")
                    } else if (value == "") {
                      div(title = value, value, style = "")
                    }
                  }
                ),
                
                # customize settings for table columns
                columns = list(
                  
                  .selection = colDef(class = "box", width = 50),
                  
                  # 1. project name
                  project = colDef(
                    name = "Project",
                    class = "col1",
                    minWidth = 290,
                    maxWidth = 470
                  ),
                  
                  # 2. project category logo
                  images = colDef(
                    name = "",
                    class = "col2",
                    maxWidth = 130,
                    cell = function(value) {
                      htmltools::tags$img(src = value,
                                          height = 70,
                                          width = 72)
                    }
                  ),

                  # 3. project description summary
                  summary_description = colDef(
                    name = "Description",
                    class = "col3",
                    minWidth = 280,
                    details = function(index) {
                      expand <- expanded[expanded$more == table$more[index],]
                      htmltools::div(style = "padding-bottom:15px",
                                     reactable(expand, class = "expand",
                                               columns = list(more = colDef(name = ""))))
                    }
                  ),
                  
                  # 4. project status
                  status = colDef(
                    name = "Status",
                    class = "col4",
                    minWidth = 100
                  ),
                  
                  # hide these columns
                  description = colDef(show = FALSE),
                  more = colDef(show = FALSE)
                  
                )
      )
    })
    
    
    ### FUNCTIONS ###
    
    # this function returns the current time in EST
    currentTime <- function() {
      as.POSIXct(format(Sys.time()), tz = "America/New_York")
    }

    # this function uses modalDialog() to create a template for forms
    formTemplate <- function(form_id, title, instructions, text_label, text_input){
      modalDialog(
        div(style = "text-align:center", h5(title)),
        div(style = "width:85%; margin: 0 auto", hr()),
        div(style = 'font-size:15px; font-family:Jost; font-weight:500; line-height:1.5; width: 85%; margin: 0 auto 1rem auto;', p(instructions)),
        textInput(str_glue('name{form_id}'), span("Name *", style = 'font-size:13px;')),
        textInput(str_glue('email{form_id}'), div("Email *", style = 'font-size:13px;')),
        textAreaInput(
          str_glue('message{form_id}'),
          div(text_label, style = 'font-size:13px;'),
          value = text_input,
          rows = 3
        ), 
        div(
          style = "text-align:center; margin: 20px 0", 
          actionBttn(
            inputId =  str_glue('submit_btn{form_id}'), 
            label = div("submit", style = ""),
            style = "pill",
            color = "success"
          )),
        footer = NULL,
        easyClose = TRUE
      )
    }
    
    # this function toggles the submit button on forms
    # the submit button is enabled when all required fields are filled
    toggleButton <- function(button_name, fieldsMandatory) {
      
      # check if all required fields are filled
      # return 1/0 for each condition checked
      mandatoryFilled <- vapply(
        fieldsMandatory,
        function(x) { !is.null(input[[x]]) && input[[x]] != "" }, 
        logical(1)
      )
      
      # enable button if all conditions are true
      if(all(mandatoryFilled)){
        enable(button_name)
      } else{
        disable(button_name)
      }
      
    }
    
    # this function reads from the csv with all collected form responses
    # then adds the submitted response as a new row
    submitForm <- function(file, fieldsAll) {
      
      # read sheet with all form responses
      form <- read_csv(file) 
      
      # reformat form data
      data <- sapply(fieldsAll, function(x) input[[x]])
      df <- as.data.frame(t(data)) |> 
        cbind(time = currentTime()) # add time stamp
      names(df) <- c('Name', 'Email', 'Message', 'Time')
      new_form <- form %>% rbind(df)
      
      # write sheet with latest response as a new row
      write_csv(new_form, file)
    }
    
    # this function closes pop-ups and prints a thank you message
    # called when a form is submitted
    closeForm <- function(){
      removeModal()
      shinyalert(
        "Thank You!",
        "",
        className = "thanks",
        showConfirmButton = F,
        closeOnEsc = T,
        closeOnClickOutside	= T
      )
    }
      
    ### FORMS ###
    
    ## PROPOSAL (form)
    proposal_form <- function(failed = FALSE) {
      formTemplate(
        form_id = 3,
        title = "New Project Proposal",
        instructions = "Have an idea for a project that you don't see listed in our database? Describe what you have in mind below and we'll be in touch with you shortly!",
        text_label = "Project Description",
        text_input = ""
      )
    }
    
    ## PROJECT INTEREST (form)
    project_interest_form <- function(failed = FALSE) {
      formTemplate(
        form_id = 2,
        title = "Project Interest Form",
        instructions = "Interested in a project? Fill out the interest form below and we'll be in touch with you shortly!",
        text_label = "Message",
        # include text output generated by projData()
        text_input = as.character(projData()) 
      )
    }
    
    ## pre-fill PROJECT INTEREST form 
    ## if at least one project is selected:
    
    # get row number of the selected project
    selected_row_num <- reactive(getReactableState("master_table", "selected"))
    # get name of the selected project
    selected_project <- reactive(table[selected_row_num(), 'project'])
    
    ## generate form text based on selected project(s)
    # returns nothing if no projects are selected
    projData <- reactive({
      # include the name of the selected project
      if(length(selected_row_num()) == 1){
        listed <- str_glue("I'm interested in the following project: {as.character(selected_project())}.")
        # make 'projects' plural if multiple are selected
      } else if(length(selected_row_num()) > 1){
        listed <- str_glue("I'm interested in the following projects: {as.character(selected_project())}.")
        listed <- gsub("c[()]", "", listed)
        listed <- gsub("\\)", "", listed)
      } else {
        listed <- ""
      }
    })
    
    ### SHOW FORMS ###
    
    # show PROPOSAL form when 'propose_project' button is clicked
    observeEvent(input$propose, {
      showModal(proposal_form())
    })
    
    # show PROJECT INTEREST form when 'get_involved' button is clicked
    observeEvent(input$get_involved, {
      showModal(project_interest_form())
    })
    
    # show PROJECT INTEREST form if a project is selected
    highlight <- reactive({
      if(length(selected_row_num()) > 0){
        highlight <- 1
      } else{
        highlight <- 0
      }
      return(highlight)
    })
    
    observe({
      if(highlight()){
        showModal(project_interest_form())
      }
    })
    
    ### TOGGLE BUTTON ON FORM ###
    
    # activated when 'get_involved' button is clicked
    # enables submit button when all required fields are filled
    observeEvent(input$get_involved, {
      observe({
        toggleButton(
          button_name = "submit_btn2",
          fieldsMandatory = c("name2", "email2")
        )
      })
    })
    
    # activated when 'propose' button is clicked
    # enables submit button when all required fields are filled
    observeEvent(input$propose, {
      observe({
        toggleButton(
          button_name = "submit_btn3",
          fieldsMandatory = c("name3", "email3")
        )
      })
    })
    
    ### SUBMIT FORM ###
    
    # activated when submit button for PROJECT INTEREST form is clicked
    # submits the form when all conditions are met
    observeEvent(input$submit_btn2, {
      
      submitForm(
        file = 'interest_form.csv', 
        fieldsAll = c("name2", "email2", "message2")
      )
      
      closeForm()
      
    })
    
    # activated when submit button for PROPOSAL form is clicked
    # submits the form when all conditions are met
    observeEvent(input$submit_btn3, {
      
      submitForm(
        file = 'proposal_form.csv', 
        fieldsAll = c("name3", "email3", "message3")
      )
      
      closeForm()
      
    })
    
    
  })
  
  
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

