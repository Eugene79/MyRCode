plot_tsne <- function(mat, seed, perplexity, label, ...){
  # creates pca plot from a matrix with annotations from label
  #
  # Args:
  #     mat: Matrix of numerical values. Features are in rows, observation in columns.
  #     label: Dataframe with one column annotating observations to various classes. Row names should coincide with column names of the mat object.
  #     perplexity: numeric
  # seed: numeric for seed
  #     ...: additional arguments for Rtsne function, e.g. theta, max_iter
  #
  # Returns:
  #     A ggplot object with tsne plot.

  require(Rtsne)
  require(plotly)
  
  set.seed(seed = as.integer(seed))
  tsne_model <- Rtsne(t(mat)
                      , check_duplicates=FALSE
                      , pca=TRUE
                      , dims = 3
                      , perplexity=as.integer(perplexity)
                      , ...
                      )

  data_coordinates = as.data.frame(tsne_model$Y)
  data_coordinates$samples = colnames(mat)

  data_coordinates <- merge.data.frame(
    x = data_coordinates
    , y = label
    , by.x = 'samples'
    , by.y = 'row.names')
    
  plot_object <- plot_ly(data_coordinates
                , x = ~V1, y = ~V2, z = ~V3
                , color = as.formula(paste0('~',names(label)))
                , colors = c('#FFE1A1', '#683531')
                ) %>% add_markers()

  return(plot_object)
}
