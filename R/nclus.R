#' Evaluate optimal k for clustering
#'
#' @param data data frame of scaled variables in columns, obs in rows, no row labels
#' @param min_k minimum k to allow
#' @param max_k maximum k to allow
#' @keywords k selection by criteria
#'
nclus <- function(data, min_k=2, max_k=10){
  library(NbClust)

  nc <- try(NbClust(data, min.nc=min_k, max.nc=max_k,
                    method="kmeans", index="alllong"),
            silent=TRUE)

  counts <- table(nc$Best.n[1,])

  metrics <- as.matrix(nc$Best.n[1,])
  metrics<- as.data.frame(cbind(rownames(metrics), metrics))
  colnames(metrics) <- c("method", "NumClus")
  rownames(metrics) <- NULL
  metrics$NumClus <- as.numeric(metrics$NumClus)
  metrics <- arrange(metrics, desc(NumClus))

  metrics2 <- metrics %>%
    filter(!is.na(NumClus)) %>%
    summarise(num_criteria=n())

  b <- barplot2(counts,
                main = paste0("Number of Clusters Chosen by ", metrics2, " Criteria"),
                cex.main = 0.9,
                ylim = c(0, (max(counts)+3)),
                xlab = "Number of Clusters",
                ylab = "Number of Criteria",
                cex.lab = 0.8,
                col="dark green")
  text(b, counts + 0.5, round(counts, 1),
       cex=0.75, col="darkblue")
  print(b)

  return(metrics)
}
