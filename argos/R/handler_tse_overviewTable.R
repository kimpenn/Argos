#' A set of helper functions generating overview tables 
#' for the second tab in Time Series Explorer (tse) Component.


renderTableHandler <- function(the_data) {
  the_median <- median(the_data$Target)
  
  DT::renderDataTable({
    datatable(
      the_data,
      options = list(
        dom = 't',
        pageLength = -1,
        columnDefs = list(list(
          targets = 1, visible = FALSE
        ))
      ),
      rownames = the_data$Sample
    ) %>%
      formatRound(columns = colnames(the_data)[-1],
                  digits = 1) %>%
      formatStyle('Target',
                  target = 'row',
                  backgroundColor = styleInterval(the_median, c('#B5E2FA', '#EDDEA4')))
  })
}

overviewTableBox <- function (id, the_table) {
  box(
    width = 4,
    align = "center",
    status = "warning",
    DT::dataTableOutput(id)
  )
}

tse_overview_table <- function(input_data,
                               input_col_data,
                               input_target,
                               input_gene_list) {
  the_data <- input_data
  treatment_list <- input_col_data$Group
  
  # Replace the "-" to "." to match the column names in normalized dataset
  input_col_data$Sample <-
    str_replace_all(input_col_data$Sample, "-", ".")
  
  
  f <- function(the_sample,
                the_data,
                gene_target,
                gene_target_list) {
    # Get one row of data from the_cell
    # - The first column is the sample id.
    # - The second column is the normalized counts of the target gene.
    # - The third column is the sum of normalized counts of the target gene list.
    # - The fourth column is the sum of the normalized reads in each sample.
    # the_col_name <- c("Sample", "Target", "List", "All")
    
    val_1 <- the_sample
    val_2 <- the_data[gene_target, the_sample]
    val_3 <- sum(the_data[gene_target_list, the_sample])
    val_4 <- sum(the_data[, the_sample])
    res <- c(val_1, val_2, val_3, val_4)
    names(res) <- c("Sample", "Target", "List", "All")
    res["Sample"] <-
      str_split(res["Sample"], "\\.", n = 2, simplify = TRUE)[, 2]
    res
  }
  
  g <- function (the_group,
                 the_data,
                 gene_target,
                 gene_target_list) {
    sample_list <-
      input_col_data %>% filter(Group == the_group) %>% pull(Sample)
    
    as_tibble(do.call(
      rbind,
      lapply(
        sample_list,
        f,
        the_data = the_data,
        gene_target = input_target,
        gene_target_list = input_gene_list
      )
    )) %>%
      mutate(
        Sample = as.character(Sample),
        Target = as.numeric(Target),
        List = as.numeric(List),
        All = as.numeric(All)
      ) %>%
      arrange(desc(Target))
    
  }
  
  group_list <- c("C", "1", "2", "3", "7")
  
  lapply(
    group_list,
    g,
    the_data = the_data,
    gene_target = input_target,
    gene_target_list = input_gene_list
  )
  
}

##############################
# Mocking Test
##############################
# load_dataset <- function(input_path) {
#   my_df <- as.data.frame(read_csv(input_path))
#   rownames(my_df) <- my_df$symbol
#   my_df[,-1]
# }
#
# load_coldata <- function(input_path) {
#   read_csv(input_path)
# }
#
#
#
# input_target <- "Hras"
# input_gene_list <-
#   c("Creb1", "Hras", "Map2k6", "Ntrk1", "Sh2b1", "Src")
# the_data <- load_dataset("dataset/ngf2/count-matrix-norm.csv")
# the_col_dat <- load_coldata("dataset/ngf2/design-table.csv")
