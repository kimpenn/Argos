# Module for Displaying Cell Images

# UI function ------------------------------------------------

cellImageUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    # Column 1: Outlier Images  ----------------
    box(
      width = 6,
      align = "center",
      status = "warning",
      
      wellPanel(
        selectInput(
          ns("select_outliers"),
          "Select the image of the outliers",
          choices = character(0),
          multiple = FALSE
        )
      ),
      tabBox(
        width = 12,
        id = "photos",
        tabPanel("Before", imageOutput(ns(
          "outlier_img_before"
        ))),
        tabPanel("After", imageOutput(ns(
          "outlier_img_after"
        )))
      )
    ),
    
    # Column 2: All Images    ----------------
    box(
      width = 6,
      align = "center",
      status = "primary",
      wellPanel(fluidRow(
        column(
          width = 6,
          selectInput(
            ns("select_cell_image_group"),
            "Select the cell group",
            selected = "C",
            choices = c("C", "1", "2", "3", "7"),
            multiple = FALSE
          )
        ),
        column(
          width = 6,
          selectInput(
            ns("select_cell_image_id"),
            "Select the cell id",
            choices = character(0),
            multiple = FALSE
          )
        )
      )),
      tabBox(
        width = 12,
        id = "any-photos",
        tabPanel("Before", imageOutput(ns("any_img_before"))),
        tabPanel("After", imageOutput(ns("any_img_after")))
      )
    )
  )
}

# Server function ---------------------------------------------

cellImageServer <- function(id, DatasetRVs, theOutliers) {
  moduleServer(id, function(input, output, session) {
    # Cell Images: Outliers ------------------------------------
    
    # Update selections
    observe({
      req(theOutliers())
      
      updateSelectInput(session, "select_outliers",
                        choices = theOutliers())
    })
    
    # Render Cell Images
    observeEvent(input$select_outliers, {
      file_name_list <- get_image_path(input$select_outliers)
      
      output$outlier_img_before <- renderImage({
        list(src = file_name_list[1],
             height = 300)
      }, deleteFile = FALSE)
      
      output$outlier_img_after <- renderImage({
        list(src = file_name_list[2],
             height = 300)
      }, deleteFile = FALSE)
    })
    
    # Cell Images: All Images ------------------------------------
    
    # Update selections
    observe({
      req(input$select_cell_image_group)
      req(DatasetRVs$colData)
      
      updateSelectInput(
        session,
        "select_cell_image_id",
        choices = get_sample_list(input$select_cell_image_group,
                                  DatasetRVs$colData)
      )
    })
    
    # Render Cell Images
    observeEvent(input$select_cell_image_id, {
      file_name_list <- get_image_path(input$select_cell_image_id)
      
      output$any_img_before <- renderImage({
        list(src = file_name_list[1],
             height = 300)
      }, deleteFile = FALSE)
      
      output$any_img_after <- renderImage({
        list(src = file_name_list[2],
             height = 300) # width = "100%"
      }, deleteFile = FALSE)
    })
  })
}