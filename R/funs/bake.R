

#------------------------------------------------------------------------------#
# Overcoming Compound Racial Inequity code repository
# Author: Pedro Nascimento de Lima
# Copyright (C) 2022 by The RAND Corporation
# See README.md for information on usage and licensing
#------------------------------------------------------------------------------#

# The bake function is adapted from the pomp package bake function:
# We use it here to perform reproducible, time-consuming tasks.
# https://github.com/kingaa/pomp/blob/master/R/bake.R
bake <- function (file, expr, use_bake = T, seed = NULL, kind = NULL, normal.kind = NULL) {
  if (file.exists(file) & use_bake) {
    readRDS(file)
  } else {
    seed <- as.integer(seed)
    rng.control <- length(seed) > 0
    if (rng.control) {
      if (!exists(".Random.seed",envir=.GlobalEnv)) set.seed(NULL)
      save.seed <- get(".Random.seed",envir=.GlobalEnv)
      set.seed(seed,kind=kind,normal.kind=normal.kind)
    }
    tmg <- system.time(val <- eval(expr))
    if (rng.control)
      assign(".Random.seed",save.seed,envir=.GlobalEnv)
    if (is.null(val)) {
      pWarn("bake","expression evaluates to NULL")
      val <- paste0("NULL result returned by ",sQuote("bake"))
    }
    if (rng.control) {
      attr(val,"seed") <- seed
      attr(val,"kind") <- kind
      attr(val,"normal.kind") <- normal.kind
    }
    attr(val,"system.time") <- tmg
    saveRDS(val,file=file)
    val
  }
}
