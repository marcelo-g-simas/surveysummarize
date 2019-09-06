library(shiny)
library(data.table)
library(shinyFiles)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- function(request) {
  fluidPage(
   useShinyjs(),
   # Application title
   titlePanel("HTS Configuration"),

   # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput('study_name', 'Study Name'),
      textInput('study_directory','Study Directory'),
      h4('Documentation:'),
      selectizeInput('variables', 'Variables (optional)', choices = study_files),
      selectizeInput('values', 'Values (optional)', choices = study_files),
      hr(),
      # bookmarkButton()
      actionButton('save_configuration','Save Configuration', icon = icon('save'),
                   style="color: #f9f9f9; background-color: #3b5998; border-color: #8b9dc3; font-weight: bold")
    ),
    mainPanel(

      tabsetPanel(
        type = "tabs",
        #=================================================================================================================#
        tabPanel(
          title = "Household",
          fluidRow(
            br(),
            column(
              width = 4,
              h4('Household Data'),
              textInput('household_id', 'Household ID'),
              selectizeInput('household_data', 'Choose Household Data (csv)', choices = study_files)
            ),
            column(
              width = 8,
              h4('Household Weights (optional)'),
              selectizeInput('household_weights', 'Choose Household Weights (csv)', choices = study_files),
              textInput('household_final', 'Household Final Weight'),
              textInput('household_replicate_base', 'Household Replicate Weight Base Name'),
              sliderInput("household_replicate_range", "Household Replicate Suffix:", min = 1, max = 250, value = c(1,100))
            )
          )
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Person",
          fluidRow(
            br(),
            column(
              width = 4,
              h4('Person Data'),
              textInput('person_id', 'Person ID\'s (i.e. SAMPNO,PERNO)'),
              # selectizeInput('person_id', 'Person ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
              selectizeInput('person_data', 'Person Household Data (csv)', choices = study_files)
            ),
            column(
              width = 8,
              h4('Person Weights (optional)'),
              selectizeInput('person_weights', 'Choose Person Weights (csv)', choices = study_files),
              textInput('person_final', 'Person Final Weight'),
              textInput('person_replicate_base', 'Person Replicate Weight Base Name'),
              sliderInput("person_replicate_range", "Person Replicate Suffix:", min = 1, max = 250, value = c(1,100))
            )
          )
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Trip",
          h4('Trip Data'),
          textInput('trip_id', 'Trip ID\'s (i.e. SAMPNO,PERNO,TRIPNO)'),
          # selectizeInput('trip_id', 'Trip ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          selectizeInput('trip_data', 'Choose Trip Data (csv)', choices = study_files)
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Tour (optional)",
          h4('Trip Data'),
          textInput('tour_id', 'Tour ID\'s (i.e. SAMPNO,PERNO,TOURNO)'),
          # selectizeInput('tour_id', 'Tour ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          selectizeInput('tour_data', 'Choose Tour Data (csv)', choices = study_files)
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Vehicle (optional)",
          h4('Vehicle Data'),
          textInput('vehicle_id', 'Vehicle ID\'s (i.e. SAMPNO,VEHNO)'),
          # selectizeInput('vehicle_id', 'Vehicle ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          selectizeInput('vehicle_data', 'Choose Vehicle Data (csv)', choices = study_files)
        )
      )
    )

  )
)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  updateTextInput(session, 'study_directory', value = study_directory)
  disable('study_directory')

  observeEvent(input$save_configuration, {
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
    cat(url)
    # study_log <- data.table(study = input$study_name, state = url)
    # fwrite(study_log, file = 'study_log.csv', append = TRUE)
  })

  onRestore(function(state) {
    updateSelectizeInput(session, "person_id", selected=state$input$person_id)
    updateSelectizeInput(session, "trip_id", selected=state$input$trip_id)
  })


}


study_directory <- tcltk::tk_choose.dir()
study_files <- c('<NA>' = NA, sort(list.files(study_directory, pattern = '.csv')))
shinyApp(ui, server, enableBookmarking = "url")

