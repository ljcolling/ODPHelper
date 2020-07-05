
## ---------------------------------------------------------------------
##  This function contains helper functions from other packages
##  Specifically, the apa package and the userfriendlyscience package
##  They're included here to keep package installation to a minimum
## ---------------------------------------------------------------------

# function for calculating partial eta-squqared
# extracted from the `apa` package

petasq_ <- function(ss_effect, ss_error) {
  ss_effect / (ss_effect + ss_error)
}

# function for printing anova output
print_anova <- function(x, effect) {
  anova <- x$ANOVA
  if (!all(c("SSn", "SSd") %in% names(anova))) {
    stop("Parameter 'detailed' needs to be set to TRUE in call to `ezANOVA`")
  }
  if (!effect %in% anova$Effect) {
    stop("Specified effect not found")
  } else {
    row <- which(anova$Effect == effect)
  }
  glue::glue(
    "*F*({anova[row,'DFn']}, {anova[row,'DFd']}) = ",
    "{rd(anova[row,'F'])}, *p* {rp(anova[row,'p'])}, ",
    "$\\eta^2_p$ {rp(petasq_(anova[row, 'SSn'], anova[row, 'SSd']))}"
  )
}

rd <- purrr::partial(sprintf, fmt = "%.3f")

rp <- function(.x) {
  if_else(.x < 0.001, "< .001",
    stringr::str_replace(
      string = rd(.x),
      pattern = "0.",
      replacement = "= ."
    )
  )
}

rp2 <- function(.x) {
  if_else(abs(.x) < 0.001, "< .001",
    stringr::str_replace(
      string = rd(.x),
      pattern = "0.",
      replacement = "."
    )
  )
}


convert.t.to.r <- function(t, n) {
  return(t / (sqrt(n - 2 + t^2)))
}

convert.r.to.d <- function(r) {
  return((r * 2) / sqrt(1 - r^2))
}

cohensdCI <- function(d, n, conf.level = 0.95, plot = FALSE, silent = TRUE) {
  if (length(conf.level) != 1) {
    stop("Only specify one value for argument 'conf.level'!")
  }
  ci.bound.lo <- (1 - conf.level) / 2
  ci.bound.hi <- 1 - (1 - conf.level) / 2
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  myWarnings <- NULL
  if (length(d) == length(n)) {
    res <- t(sapply(1:length(d), function(i) {
      return(withCallingHandlers(c(qCohensd(ci.bound.lo,
        n[i],
        populationD = d[i]
      ), qCohensd(ci.bound.hi,
        n[i],
        populationD = d[i]
      )), warning = wHandler))
    }))
  }
  else if ((length(d) == 1) || (length(n) == 1)) {
    res <- withCallingHandlers(matrix(c(qCohensd(ci.bound.lo,
      n,
      populationD = d
    ), qCohensd(ci.bound.hi, n, populationD = d)),
    ncol = 2
    ), warning = wHandler)
  }
  else {
    stop(
      "Either specify vectors of equal length as 'd' and 'n', or a ",
      "single value for one and a vector for the other."
    )
  }
  colnames(res) <- c("lo", "hi")
  if (plot) {
    if ((length(d) > 1) || (length(n) > 1) || (length(conf.level) >
      1)) {
      warning(
        "I can only produce a plot if you supply only one value for ",
        "arguments d, n, and conf.level!"
      )
    }
    else {
      df <- data.frame(d = seq(min(res) - 0.5, max(res) +
        0.5, 0.001))
      df$density <- withCallingHandlers(dd(df$d, df = n -
        2, populationD = d), warning = wHandler)
      cilo <- min(res)
      cihi <- max(res)
      dValue <- d
      plot <- ggplot(df, aes(x = d, y = density)) +
        theme_bw() +
        theme(axis.title.x.top = element_blank()) +
        scale_x_continuous(sec.axis = dup_axis(breaks = c(
          cilo,
          dValue, cihi
        ), labels = round(c(
          cilo, dValue,
          cihi
        ), 2))) +
        geom_vline(aes(xintercept = cilo),
          linetype = "dashed"
        ) +
        geom_vline(aes(xintercept = dValue),
          linetype = "dashed"
        ) +
        geom_vline(aes(xintercept = cihi),
          linetype = "dashed"
        ) +
        geom_ribbon(data = df[df$d >=
          min(res) & df$d <= max(res), ], aes(
          ymin = 0,
          ymax = density
        ), fill = "#cadded") +
        geom_segment(
          x = min(res),
          xend = min(res), y = 0, yend = dd(min(res), df = n -
            2, populationD = d), color = "#2a5581", size = 1.5
        ) +
        geom_segment(
          x = max(res), xend = max(res), y = 0,
          yend = dd(max(res), df = n - 2, populationD = d),
          color = "#2a5581", size = 1.5
        ) +
        geom_line(size = 1.5)
      attr(res, "plot") <- plot
      class(res) <- "cohensdCI"
    }
  }
  d <- paste0("d=", d)
  n <- paste0("n=", n)
  rownames(res) <- paste0(d, ", ", n)
  if ((!silent) && (length(myWarnings) > 0)) {
    precisionWarnings <- grepl("full precision may not have been achieved in 'pnt{final}'",
      myWarnings,
      fixed = TRUE
    )
    if (any(precisionWarnings)) {
      cat0(
        "Function 'qt', which is used under the hood of this function (see ?qt for more information), ",
        "warned that 'full precision may not have been achieved'. ",
        "This is normally no cause for concern, because with sample sizes this big, small deviations ",
        "have little impact, but informing you seemed appropriate nonetheless.\n\n"
      )
    }
    if (!all(precisionWarnings)) {
      cat0("One or more ", ifelse(any(precisionWarnings),
        "additional", ""
      ), " warnings were encountered:\n")
      lapply(myWarnings[!precisionWarnings], function(x) {
        cat0(
          x$message,
          "\n"
        )
      })
      cat("\n")
    }
  }
  return(res)
}

qCohensd <- function(p, df, populationD = 0, lower.tail = TRUE) {
  return(convert.t.to.d(qt(p, df, ncp = convert.d.to.t(
    d = populationD,
    df = df
  ), lower.tail = lower.tail), df + 2))
}

convert.t.to.d <- function(t, df = NULL, n1 = NULL, n2 = NULL, proportion = 0.5) {
  if (is.null(df) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1
    groupSize2 <- n2
  }
  else if (!is.null(df) && is.null(n1) && is.null(n2)) {
    groupSize1 <- proportion * (df + 2)
    groupSize2 <- (1 - proportion) * (df + 2)
  }
  else {
    warning("Specify either df (and ideally proportion) or n1 and n2! Returning NA.")
    return(NA)
  }
  multiplier <- sqrt((1 / groupSize1) + (1 / groupSize2))
  d <- t * multiplier
  return(d)
}

convert.d.to.t <- function(d, df = NULL, n1 = NULL, n2 = NULL, proportion = 0.5) {
  if (is.null(df) && !is.null(n1) && !is.null(n2)) {
    groupSize1 <- n1
    groupSize2 <- n2
  }
  else if (!is.null(df) && is.null(n1) && is.null(n2)) {
    groupSize1 <- proportion * (df + 2)
    groupSize2 <- (1 - proportion) * (df + 2)
  }
  else {
    warning("Specify either df (and ideally proportion) or n1 and n2! Returning NA.")
    return(NA)
  }
  multiplier <- sqrt((1 / groupSize1) + (1 / groupSize2))
  t <- d / multiplier
  return(t)
}

# session info

desc_session <- function() {
  list(
    sessionInfo = utils::sessionInfo(),
    cran = options("repos")$repos[[1]],
    citations = invisible(knitr::write_bib())
  )
}

keys_from_session <- function(citations) {
  keys <- unname(unlist(map(citations, function(x) {
    suppressMessages({
      readr::write_lines(x[[1]], path = "temp128")
      bib2df::bib2df(file = "temp128")$BIBTEXKEY
    })
  })))
  file.remove("temp128")
  invisible(keys)
}

