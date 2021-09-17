#' Bind two factors
#'
#' Create a new factor from two existing factors, where the new factor's levels
#' are the union of the levels of the input factors.
#'
#' @param x factor
#'
#' @return factor
#' @export
#'



density_function = function(x) {
  # density_output <- stats::density(x, bw = 'SJ')
  density_output <- stats::density(x, n = 512)
  return(density_output$y)
}

function_1 <- function(data, method, k) {
  if (method == "pdf") {
    density_results <- apply(X = data,
                             FUN = density_function,
                             MARGIN = 2)
    density_results <- t(density_results)
    density_results <- as.data.frame(density_results)

  }

  else {
    data_t <- t(data)
  }

  Similarity_Methods <-
    c(
      "euclidean",
      "manhattan",
      "chebyshev",
      "sorensen",
      "gower",
      "soergel",
      "kulczynski_d",
      "canberra",
      "lorentzian",
      "intersection",
      "wavehedges",
      "czekanowski",
      "motyka",
      "tanimoto",
      "inner_product",
      "harmonic_mean",
      "cosine",
      "jaccard",
      "dice",
      "hassebrook",
      "fidelity",
      "bhattacharyya",
      "squared_chord"
    )
  # Similarity_Methods <- c("fidelity")

  silhouette_score <- c()

  increment <- 1
  for (value in Similarity_Methods)

  {
    if (method == "pdf") {
      distance_values <-
        philentropy::distance(density_results,
                              method = value,
                              use.row.names = TRUE)
    }

    else {
      distance_values <-
        philentropy::distance(data_t, method = value, use.row.names = TRUE)
    }


    fclustering <-
      cluster::fanny(
        x = distance_values,
        k = k,
        maxit = 1500,
        memb.exp = 3
      )



    print(fclustering[10]$silinfo$avg.width)
    print(fclustering[10]$silinfo$clus.avg.widths)



    Predicted_Clusters <- fclustering$clustering

    if ((fclustering[10]$silinfo$clus.avg.widths > 0.20) == TRUE) {
      silhouette_score[increment] <- fclustering[10]$silinfo$avg.width
      names(silhouette_score)[increment] <- value
      increment <- increment + 1
    }

    else {
      increment <- increment
    }


  }
  print(names(silhouette_score))
  omtimised_distance_measure <- names(which.max(silhouette_score))
  distance_values_1 <-
    philentropy::distance(t(density_results),
                          method = omtimised_distance_measure,
                          use.row.names = TRUE)
  fclustering_1 <-
    cluster::fanny(
      x = distance_values_1,
      k = k,
      maxit = 1500,
      memb.exp = 3
    )


  plot(fclustering_1)



  factoextra::fviz_cluster(
    fclustering_1,
    ellipse.type = "norm",
    repel = TRUE,
    palette = "jco",
    legend = "right"
  )




}
