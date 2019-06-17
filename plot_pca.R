plot_pca = function (mat, label, number_genes = 1000, ...) {
  # creates pca plot from a matrix with annotations from label
  #
  # Args:
  #     mat: Matrix of numerical values. Features are in rows, observation in columns.
  #     label: Dataframe with one column annotating observations to various classes. Row names should coincide with column names of the mat object.
  #     number_genes: Integer. How many features to use for pca creation.
  #     ...: additional arguments for prcomp function, e.g. center, scale, etc.
  #
  # Returns:
  #     A ggplot object with pca.

  require(ggplot2)
  variance_genes <- apply(X = mat, MARGIN = 1, FUN = var)
  pca_ana <- prcomp(x = t(mat[order(variance_genes, decreasing = T)[1:number_genes],]), ...)

  explained_variance <- pca_ana$sdev**2/sum(pca_ana$sdev**2)

  dat <- data.frame(x = pca_ana$x[,1], y = pca_ana$x[,2])
  dat <- merge(x = dat, y = label, by = 'row.names')

  p = ggplot(data = dat, mapping = aes_string(x = 'x', y = 'y', col = names(label))) +
    xlab(paste('PC 1: ', 100*round(explained_variance[1], digits = 3), '%', sep = '')) +
    ylab(paste('PC 2: ', 100*round(explained_variance[2], digits = 3), '%', sep = '')) +
    geom_point()
  return(p)
}
