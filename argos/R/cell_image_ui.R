cell_image_ui <- function() {
  fluidRow(
    ###################################
    # Column 1: Outlier Images
    ##################################
    box(
      width = 6,
      align = "center",
      status = "warning",
      wellPanel(
        selectInput(
          "select_outliers"
          ,
          "Select the image of the outliers"
          ,
          choices = character(0)
          ,
          multiple = FALSE
        )
      ),
      tabBox(
        width = 12,
        id = "photos",
        tabPanel("Before", uiOutput("outlier_img_before")),
        tabPanel("After", uiOutput("outlier_img_after"))
      )
    ),
    ###################################
    # Column 2: All Images
    ##################################
    box(
      width = 6,
      align = "center",
      status = "primary",
      wellPanel(fluidRow(
        column(
          width = 6,
          selectInput(
            "select_cell_image_group"
            ,
            "Select the cell group"
            ,
            selected = "c",
            choices = c("c", "1", "2", "3", "7")
            ,
            multiple = FALSE
          )
        ),
        column(
          width = 6,
          selectInput(
            "select_cell_image_id"
            ,
            "Select the cell id"
            ,
            choices = character(0)
            ,
            multiple = FALSE
          )
        )
      ))
      ,
      tabBox(
        width = 12,
        id = "any-photos",
        tabPanel("Before", uiOutput("any_img_before")),
        tabPanel("After", uiOutput("any_img_after"))
      )
    )
  )
}