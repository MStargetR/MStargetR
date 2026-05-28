# MetaboAnalystR scripting image for the MStargetR head-to-head benchmark.
#
# NOTE: this is deliberately NOT the official xia-lab/MetaboAnalyst_Docker
# image -- that one is an Rserve/Payara web app pinned to v4.0. We build a
# minimal Rscript-able image around MetaboAnalystR (the R engine behind the
# MetaboAnalyst 6.0 web service) so individual processing stages can be timed
# non-interactively.
#
# Build:
#   docker build -t mstargetr-bench/metaboanalystr:4.2.0 benchmark/docker
#
# The R version is pinned to keep the benchmark reproducible.
FROM rocker/r-ver:4.4.2

# System libraries required by MetaboAnalystR's Bioconductor dependency stack
# (XML parsing, netCDF for mass-spec IO, GSL/GLPK for stats, cairo for plots).
RUN apt-get update && apt-get install -y --no-install-recommends \
        libcairo2-dev \
        libnetcdf-dev \
        libxml2-dev \
        libxt-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libudunits2-dev \
        libgsl-dev \
        libglpk-dev \
        libgit2-dev \
        zlib1g-dev \
        pandoc \
    && rm -rf /var/lib/apt/lists/*

# Bioconductor dependencies of MetaboAnalystR (incl. sva for ComBat, used by
# PerformBatchCorrection). Pinned to no-update/no-ask for reproducibility.
RUN R -e 'install.packages("BiocManager", repos="https://cloud.r-project.org"); \
  BiocManager::install(c( \
    "impute","pcaMethods","globaltest","GlobalAncova","Rgraphviz", \
    "preprocessCore","genefilter","SSPA","sva","limma","KEGGgraph", \
    "siggenes","BiocParallel","MSnbase","multtest","crmn","fgsea" \
  ), update=FALSE, ask=FALSE)'

# CRAN deps + benchmark utilities (peakRAM for in-container memory, optparse
# for the runner CLI, digest to echo the input hash).
RUN R -e 'install.packages(c( \
    "remotes","data.table","Rcpp","peakRAM","optparse","digest","qs" \
  ), repos="https://cloud.r-project.org")'

# Additional Bioconductor deps MetaboAnalystR requires that were not in the
# first list. RBGL (Boost Graph Library interface, needs `graph`) is a hard
# dependency that previously broke the install_github step; edgeR is used by
# MetaboAnalystR's statistics. qs2 is listed as a Suggests by MetaboAnalystR
# but is loaded at namespace-load time (lazy-load fails without it), so
# install_github does not auto-pull it -- install it explicitly here.
# Kept in a SEPARATE layer so the expensive pwiz/mzR compile in the layer
# above stays cached on rebuild.
RUN R -e 'BiocManager::install(c("graph","RBGL","edgeR"), update=FALSE, ask=FALSE); \
  install.packages("qs2", repos="https://cloud.r-project.org")'

# MetaboAnalystR itself (engine behind MetaboAnalyst 6.0).
RUN R -e 'remotes::install_github("xia-lab/MetaboAnalystR", \
    build=TRUE, build_vignettes=FALSE, upgrade="never")'

WORKDIR /data
CMD ["R", "--version"]
