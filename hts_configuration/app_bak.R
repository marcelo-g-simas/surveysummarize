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
      shinyDirButton('study_dir', label = 'Choose Study Directory', title = 'Choose the Study Directory:', icon = icon('folder'), buttonType = 'primary'),
      textInput('path','Study Directory'),
      h4('Documentation:'),
      shinyFilesButton(
        id = 'variables',
        label='Variables (optional)',
        title='Select the Variable Documentation File:',
        icon = icon('book'),
        buttonType = 'primary',
        multiple=FALSE
      ),
      textInput('variables_path','Variables'),
      shinyFilesButton(
        id = 'values',
        label='Values (optional)',
        title='Select the Values Documentation File:',
        icon = icon('book'),
        buttonType = 'primary',
        multiple=FALSE
      ),
      textInput('values_path','Values'),
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
              # fileInput('household_data', 'Household Data (csv)')
              shinyFilesButton(
                id = 'household_data',
                label='Choose Household Data (csv)',
                title='Select the Household Data File:',
                icon = icon('table'),
                buttonType = 'primary',
                multiple=FALSE
              ),
              textInput('household_data_path','Household Data')
            ),
            column(
              width = 8,
              h4('Household Weights (optional)'),
              shinyFilesButton(
                id = 'household_weights',
                label='Choose Household Weights (csv)',
                title='Select the Household Weights File:',
                icon = icon('table'),
                buttonType = 'primary',
                multiple=FALSE
              ),
              textInput('household_weights_path','Household Weights'),
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
              selectizeInput('person_id', 'Person ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
              # fileInput('person_data', 'Person Data (csv)')
              shinyFilesButton(
                id = 'person_data',
                label='Choose Person Data (csv)',
                title='Select the Person Data File:',
                icon = icon('table'),
                buttonType = 'primary',
                multiple=FALSE
              ),
              textInput('person_data_path','Person Data')
            ),
            column(
              width = 8,
              h4('Person Weights (optional)'),
              shinyFilesButton(
                id = 'person_weights',
                label='Choose Person Weights (csv)',
                title='Select the Person Weights File:',
                icon = icon('table'),
                buttonType = 'primary',
                multiple=FALSE
              ),
              textInput('person_weights_path','Person Weights'),
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
          selectizeInput('trip_id', 'Trip ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          # fileInput('trip_data', 'Trip Data (csv)')
          shinyFilesButton(
            id = 'trip_data',
            label='Choose Trip Data (csv)',
            title='Select the Trip Data File:',
            icon = icon('table'),
            buttonType = 'primary',
            multiple=FALSE
          ),
          textInput('trip_data_path','Trip Data')
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Tour (optional)",
          h4('Trip Data'),
          selectizeInput('tour_id', 'Tour ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          # fileInput('tour_data', 'Tour Data (csv)')
          shinyFilesButton(
            id = 'tour_data',
            label='Choose Tour Data (csv)',
            title='Select the Tour Data File:',
            icon = icon('table'),
            buttonType = 'primary',
            multiple=FALSE
          ),
          textInput('tour_data_path','Tour Data')
        ),
        #=================================================================================================================#
        tabPanel(
          title = "Vehicle (optional)",
          h4('Vehicle Data'),
          selectizeInput('vehicle_id', 'Vehicle ID', choices = list(), options = list(create = TRUE), multiple = TRUE),
          # fileInput('vehicle_data', 'Vehicle Data (csv)')
          shinyFilesButton(
            id = 'vehicle_data',
            label='Choose Vehicle Data (csv)',
            title='Select the Vehicle Data File:',
            icon = icon('table'),
            buttonType = 'primary',
            multiple=FALSE
          ),
          textInput('vehicle_data_path','Vehicle Data')
        )
      )
    )

  )

)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyDirChoose(input, 'study_dir', roots=volumes)

  # Diable path displays
  disable('path')
  disable('household_data_path')
  disable('household_weights_path')
  disable('person_data_path')
  disable('person_weights_path')
  disable('trip_data_path')
  disable('tour_data_path')
  disable('vehicle_data_path')
  disable('variables_path')
  disable('values_path')

  observe({

    if (length(input$path) == 0 | input$path == '') {
      study_dir <- parseDirPath(volumes, input$study_dir)
      updateTextInput(session = session, 'path', value = study_dir)
    }

    study_root <- c('Study Directory' = input$path)

    if (input$path != '') {
      # print(input$path)
      shinyFileChoose(input, 'household_data', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'household_weights', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'person_data', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'person_weights', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'trip_data', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'tour_data', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'vehicle_data', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'variables', roots = study_root, filetypes='csv')
      shinyFileChoose(input, 'values', roots = study_root, filetypes='csv')
    }

    # print(input$household_data)
    # print(parseFilePaths(study_root, input$household_data)$datapath)
    updateTextInput(session = session, 'household_data_path', value = parseFilePaths(study_root, input$household_data)$datapath)
    updateTextInput(session = session, 'household_weights_path', value = parseFilePaths(study_root, input$household_weights)$datapath)
    updateTextInput(session = session, 'person_data_path', value = parseFilePaths(study_root, input$person_data)$datapath)
    updateTextInput(session = session, 'person_weights_path', value = parseFilePaths(study_root, input$person_weights)$datapath)
    updateTextInput(session = session, 'trip_data_path', value = parseFilePaths(study_root, input$trip_data)$datapath)
    updateTextInput(session = session, 'tour_data_path', value = parseFilePaths(study_root, input$tour_data)$datapath)
    updateTextInput(session = session, 'vehicle_data_path', value = parseFilePaths(study_root, input$vehicle_data)$datapath)
    updateTextInput(session = session, 'variables_path', value = parseFilePaths(study_root, input$variables)$datapath)
    updateTextInput(session = session, 'values_path', value = parseFilePaths(study_root, input$values)$datapath)

  })

  # observeEvent(input$save_configuration, {
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })

  # onBookmarked(function(url) {
  #   updateQueryString(url)
  #   cat(url)
  #   study_log <- data.table(study = input$study_name, state = url)
  #   fwrite(study_log, file = 'study_log.csv', append = TRUE)
  # })

}


shinyApp(ui, server, enableBookmarking = "server")

