library(ggplot2)
library(data.table)
library(BBmisc)

load("fbs_results_ranking.RData")
if (!dir.exists("../Plots")) dir.create("../Plots")

# replace missing values by zeros
results.ranking$scores = lapply(results.ranking$scores, function(s) {
  w = which(is.na(s))
  if (length(w) > 0) {
    s[w] = 0
  }
  return(s)
})


############################################################################
# similarity scores based on ordered lists

filters = sort(unique(results.ranking$filter))

mean.scores = function(scores) {
  m.scores = matrix(0, nrow = length(filters), ncol = length(filters))
  for (i in seq_along(filters)) {
    for (j in seq_along(filters)) {
      values = unlist(lapply(scores, function(x) x[i, j]))
      m.scores[i, j] = mean(values, na.rm = TRUE)
    }
  }

  colnames(m.scores) = rownames(m.scores) = filters

  hc = hclust(as.dist(1 - m.scores), method = "average")
  o = rev(hc$order)

  dendro = as.dendrogram(hc)

  return(list(mean.scores = m.scores, o = o))
}


compareListsLinear = function(l1, l2, max.rank) {
  weights = (max.rank:1) / sum((max.rank:1) * (1:max.rank))
  overlaps = numeric(max.rank)
  overlaps[1] = as.numeric(l1[1] == l2[1])
  if (max.rank > 1) {
    for (r in 2:max.rank) {
      overlaps[r] = overlaps[r - 1] +
        as.numeric(l1[r] %in% l2[1:r]) +
        as.numeric(l2[r] %in% l1[1:(r - 1)])
    }
  }

  res = sum(weights * overlaps)
  res = res
  return(res)
}


ol.fun = function(x, names, max.rank = 100) {
  x = lapply(x, function(z) names(sort(z, decreasing = TRUE)))
  n = length(names)
  ol = matrix(NA_real_, nrow = n, ncol = n)
  if (max.rank == Inf) max.rank = length(x[[1]])
  for (i in 1:n) {
    for (j in i:n) {
      cl = compareListsLinear(x[[i]], x[[j]], max.rank = max.rank)
      ol[i, j] = ol[j, i] = cl
    }
  }
  colnames(ol) = rownames(ol) = names

  # add NA columns and rows for missing results
  missing = setdiff(filters, names)
  n.missing = length(missing)
  if (n.missing > 0) {
    na.cols = matrix(NA_real_, nrow = nrow(ol), ncol = n.missing)
    colnames(na.cols) = missing
    ol = cbind(ol, na.cols)
    na.rows = matrix(NA_real_, nrow = n.missing, ncol = ncol(ol))
    ol = rbind(ol, na.rows)
    rownames(ol) = colnames(ol)
  }
  ol = ol[filters, filters]
  return(ol)
}


# calulation for OL scores for the max.rank top features
scores.ol = results.ranking[, list(ol100 = list(ol.fun(scores, filter, max.rank = 100))),
  by = "dataset"]


# function for plotting the similarities between the filter methods
# based on the OL scores
plot_mat_ol = function(dat, names, title = "") {
  colnames(dat) = rownames(dat) = names
  plt.data = cbind(measure = names, as.data.frame(dat))
  plt.data = reshape2::melt(plt.data, id.vars = "measure")
  plt.data$measure = factor(plt.data$measure, levels = names)
  plt.data$variable = factor(plt.data$variable, levels = names)

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient(low = "white", high = "black", name = "OL", limits = 0:1) +
    theme_grey() +
    labs(x = element_blank(), y = element_blank(), title = element_blank()) +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12))

  if (title != "") {
    gg = gg +
      ggtitle(title) +
      theme(title = element_text(size = 13))
  }

  return(gg)
}

# calculate mean OL scores across data sets
mean.ol = mean.scores(scores.ol$ol100)
o = mean.ol$o

# OL scores plot
pdf("../Plots/ol100.pdf", height = 6, width = 7)
plot_mat_ol(mean.ol$mean.scores[o, o], filters[o])
dev.off()
