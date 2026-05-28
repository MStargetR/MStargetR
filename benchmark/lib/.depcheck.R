pkgs <- c("MStargetR","bench","peakRAM","digest","ggplot2","vegan",
          "RhpcBLASctl","qcrlscR","sva","ropls","dplyr","tidyr",
          "rmarkdown","data.table","statTarget")
for (p in pkgs) {
  cat(sprintf("%-14s %s\n", p,
      if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "MISSING"))
}
