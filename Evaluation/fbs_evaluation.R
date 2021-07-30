library(ggplot2)
library(data.table)
library(mco)
library(mlr3)

if (!dir.exists("../Plots")) dir.create("../Plots")

load("fbs_results_tuning.RData")
load("fbs_results_stability.RData")

# symbols for plotting
filters = sort(unique(results.tuned.aggr$filter))
n.methods = length(filters)
syms.basic = c(0:2, 4:6, 8, 11)
syms = rep(syms.basic, ceiling(n.methods / length(syms.basic)))


##########################################################################################
# determine order for plotting: average ranks
results.tuned.aggr.ranks = results.tuned.aggr[,
  list(filter = filter,
    graf.test.rank = rank(graf.test.mean, ties.method = "first")),
  by = "dataset"]

results.tuned.aggr.ranks.mean = results.tuned.aggr.ranks[,
  list(mean.rank = mean(graf.test.rank, trim = 1/11)),
  by = "filter"]

o.perf = order(results.tuned.aggr.ranks.mean$mean.rank)

results.tuned$filter = factor(results.tuned$filter, levels = results.tuned.aggr.ranks.mean$filter[o.perf])
results.tuned.aggr$filter = factor(results.tuned.aggr$filter, levels = results.tuned.aggr.ranks.mean$filter[o.perf])

##############################################################################################
# boxplots and scatterplots

# integrated Brier scores
pdf("../Plots/graf_box.pdf", width = 10, height = 8.25)
ggplot(data = results.tuned, mapping = aes(y = graf.test, x = filter)) +
  geom_boxplot() +
  facet_wrap("dataset", ncol = 4, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("Filter method") +
  ylab("Integrated Brier score")
dev.off()


# proportions of selected features
results.tuned.no.km = results.tuned[filter != "Kaplan-Meier", ]
results.tuned.no.km$filter = droplevels(results.tuned.no.km$filter)

pdf("../Plots/prop_box.pdf", width = 10, height = 8)
ggplot(data = results.tuned.no.km, mapping = aes(y = selected.perc, x = filter)) +
  geom_boxplot() +
  facet_wrap("dataset", ncol = 4, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  xlab("Filter method") +
  ylab("Proportion of selected features")
dev.off()

perc.med = results.tuned.no.km[, list(mean.selected = mean(selected.perc)), by = "filter"]

scaleFUN = function(x) {
  sprintf("%.2f", x)
}

pdf("../Plots/prop_box_aggr.pdf", width = 5, height = 4)
ggplot(data = results.tuned.no.km, mapping = aes(y = selected.perc, x = filter)) +
  geom_boxplot() +
  geom_text(data = perc.med, aes(x = filter, y = -0.04,
    label = scaleFUN(mean.selected)), size = 2.75) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_text(size = 10)) +
  xlab("Filter method") +
  ylab("Proportion of selected features")
dev.off()


# integrated Brier score vs. run time
pdf("../Plots/graf_time.pdf", width = 12, height = 8)
ggplot(data = results.tuned.aggr, mapping = aes(x = graf.test.mean, y = time.test.median, shape = filter, color = filter)) +
  geom_point(size = 2, stroke = 2) +
  facet_wrap("dataset", ncol = 4) +
  scale_y_log10() +
  theme_bw() +
  scale_shape_manual(values = syms, name = "Filter method") +
  scale_color_discrete(name = "Filter method") +
  guides(shape = guide_legend(ncol = 1), color = guide_legend(ncol = 1)) +
  xlab("Integrated Brier score") +
  ylab("Run time (in seconds)")
dev.off()


# feature selection stability
results.stability2 = merge(results.tuned, results.stability, by = c("dataset", "job.id"))
results.stability2 = results.stability2[filter != "none", ]

pdf("../Plots/stability_box_aggr.pdf", width = 5, height = 4)
ggplot(data = results.stability2, mapping = aes(x = filter, y = stability)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title = element_text(size = 10)) +
  xlab("Filter method") +
  ylab("Feature selection stability")
dev.off()


##########################################################################
# performances aggregated over data sets

# matrix of number of wins and losses
dominated = function(x1, x2) {
  (x1 < x2) + 0.5 * (x1 == x2)
}

dom.matrix = function(measure) {
  mat = matrix(0L, ncol = n.methods, nrow = n.methods)
  for (i in 1:n.methods) {
    for (j in setdiff(1:n.methods, i)) {
      mat[i, j] = dominated(measure[i], measure[j])
    }
  }
  return(mat)
}

domin = results.tuned.aggr[, list(dom.mat = list(dom.matrix(graf.test.mean))), by = "dataset"]

dom.mats = domin$dom.mat
n.dominated = dom.mats[[1]]
for (i in 2:length(dom.mats)) {
  n.dominated = n.dominated + dom.mats[[i]]
}

rownames(n.dominated) = colnames(n.dominated) = results.tuned.aggr[dataset == "OV", ]$filter

plot_mat = function(dat, names, n.datasets = 11, title = "") {
  colnames(dat) = rownames(dat) = names
  plt.data = cbind(measure = names, as.data.frame(dat))
  plt.data = reshape2::melt(plt.data, id.vars = "measure")
  plt.data$measure = factor(plt.data$measure, levels = names)
  plt.data$variable = factor(plt.data$variable, levels = names)

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", name = "Times",
      limits = c(0, n.datasets), midpoint = n.datasets / 2) +
    geom_text(aes(label = value, color = abs(value - n.datasets/2) >= n.datasets / 4), size = 5) +
    scale_color_manual(guide = FALSE, values = c("black", "white")) +
    theme_grey() +
    labs(x = "", y = "", title = title) +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 0.75) +
    xlab("Losses") +
    ylab("Wins") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13))

  print(gg)
  invisible(NULL)
}

dom.n = names(sort(rowSums(n.dominated), decreasing = FALSE))
dom.o = n.dominated[dom.n, dom.n]
diag(dom.o) = NA

pdf("../Plots/better_graf.pdf", width = 8, height = 6)
plot_mat(t(dom.o), dom.n, n.datasets = length(unique(results.tuned.aggr$dataset)))
dev.off()


####################
# aggregate accuracy and run time accross data sets:
# consider differences to best methods per data set

# make the minimum 0
s0 = function(x) {
  return(x - min(x, na.rm = TRUE))
}

rfs = results.tuned.aggr[, list(
  filter = filter,
  graf.test.mean = s0(graf.test.mean),
  log.time.test.median = s0(log10(time.test.median))
 ), by = "dataset"]

rfs.aggr = rfs[, list(
  graf.median = median(graf.test.mean),
  graf.min = min(graf.test.mean),
  graf.max = max(graf.test.mean),
  log.time.median = median(log.time.test.median),
  log.time.min = min(log.time.test.median),
  log.time.max = max(log.time.test.median)
), by = "filter"]


gg.graf.time.aggr = ggplot(data = rfs.aggr,
  mapping = aes(x = graf.median, y = log.time.median, color = filter, shape = filter)) +
  theme_bw() +
  geom_errorbar(aes(ymin = log.time.min, ymax = log.time.max), size = 0.5, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = graf.min, xmax = graf.max), size = 0.5, show.legend = FALSE) +
  geom_point(size = 3, stroke = 1.5) +
  scale_shape_manual(values = syms, name = "Filter method",
    guide = guide_legend(ncol = 1)) +
  scale_color_discrete(drop = FALSE, name = "Filter method") +
  xlab("Relative mean integrated Brier score") +
  ylab("Relative logarithmic median run time") +
  theme(axis.text = element_text(size = 12),
    axis.title = element_text(size = 12.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12))

pdf("../Plots/graf_time_aggr.pdf", width = 7, height = 6)
print(gg.graf.time.aggr)
dev.off()


# Pareto optimal methods (wrt accuracy and run time) based on aggregation
merge(paretoFilter(as.matrix(rfs.aggr[, c("graf.median", "log.time.median")])), rfs.aggr)
merge(paretoFilter(as.matrix(rfs.aggr[, c("graf.max", "log.time.max")])), rfs.aggr)

# Pareto optimal methods (wrt accuracy and run time) based on original data
pareto.fun = function(m) {
  pf = paretoFilter(as.matrix(m))
  res = list(pf[, 1], pf[, 2])
  names(res) = colnames(pf)
  return(res)
}
results.tuned.aggr.po = results.tuned.aggr[, pareto.fun(.SD), .SDcols = 3:4, by = "dataset"]
results.tuned.aggr.po = merge(results.tuned.aggr, results.tuned.aggr.po, by = colnames(results.tuned.aggr.po))

tab.po = sort(table(results.tuned.aggr.po$filter), decreasing = TRUE)
tab.po[tab.po > 0]
