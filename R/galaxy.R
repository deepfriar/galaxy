#' LaTeX regression tables
#'
#' @param x a model object, \code{wickr::\link[wickr]{sumer}} object, or \code{list} thereof
#' @param .captn character. Caption for the table. Default \code{NULL}.
#' @param .paren character. Value to parenthesize. \code{"p.value"}, \code{"none"}, or \code{"std.error"} (the default).
#' @param .starz numeric. Significance levels. Default \code{c(0.05, 0.01, 0.001)}.
#' @param .charz character. Character by which to symbolize statistical significance. Default \code{"*"}.
#' @param .digit numeric. Number of places to round decimals to. Default \code{3}.
#' @param .above character. Names of model metadata values to include ABOVE table. Default \code{NULL}.
#' @param .below character. Names of model metadata values to include BELOW table. Default \code{NULL}.
#' @param ... other arguments passed to \code{\link[xtable]{print.xtable}} and/or \code{\link[wickr]{sumer}}.
#' @return whatever \code{print.xtable} returns
#' @export
galaxy <- function(x,
                   .captn = NULL,
                   .paren = "std.error",
                   .starz = c(0.05, 0.01, 0.001),
                   .charz = "*",
                   .digit = 3,
                   .above = NULL,
                   .below = NULL,
                   ...) {
  ## ensure valid values
  .paren <- if(.paren %in% c("p.value", "std.error", "none")) {.paren} else {
    warning(paste(.paren, "isn't an option for the parenthesized value; defaulting to 'std.error'"))
    "std.error"
  }

  .starz <- sort(.starz, decreasing = TRUE)

  #### Extraction block

  x <- if(!methods::is(x, "list")) {list(x)} else {x}

  y <- wickr::sumer(x, ...) ## a tidy summary of each model. recurses over lists of models
  z <- attr(y, "sumer")     ## a table of metadata values for the models in x

  #### First munging block
  #### TODO: make the steps in this block less crap
  #### TODO: put reordering of columns in here somewhere to support ZOMG ROTATION

  ## create a model number column called "model"
  termcol <- which(colnames(y) == "term")
  leftcol <- 1:ncol(y)
  leftcol <- leftcol[leftcol < termcol]
  y$model <- Reduce(.slap, y[, leftcol, drop=FALSE])
  # y$model <- as.numeric(as.factor(y$model)) # what happens if we use the constructed model names directly...?

  ## do likewise for the metadata
  # z       <- data.frame(z, check.names=FALSE)
  z$model <- Reduce(.slap, z[, leftcol, drop=FALSE])
  z$model <- as.numeric(as.factor(z$model))

  ## define groups of columns and the order we want columns to appear in from left to right
  yescols <- intersect(c("model", "term"),                                 colnames(y))
  occcols <- intersect(c("level", "baseline", "outcome", "versus"),        colnames(y))
  numcols <- intersect(c("estimate", "std.error", "statistic", "p.value"), colnames(y))
  allcols <- c(yescols, occcols, numcols)
  y <- y[, allcols]

  ## put something in place of missing values for columns that may not be meaningful for all models
  y[, occcols] <- lapply(y[, occcols, drop=FALSE], function(w) {ifelse(is.na(w), "...", as.character(w))})

  ## if the column to be included in parentheses doesn't exist, forget about it
  .paren <- intersect(.paren, colnames(y))

  ## format the numeric values that will be retained in the table
  y$` `       <- Reduce(paste0, lapply(lapply(.starz, `>=`, y$p.value), ifelse, yes=".charz", no=""))
  y$estimate  <- paste0(.round(y$estimate, .digit), "^{", y$` `, "}")
  y[, .paren] <- paste0("(", .round(y[, .paren], .digit), ")") # surprisingly, works even when .paren is character(0)

  #### End first munging block

  ## THE MOMENT OF TRUTH!
  ## TURN IT ON ITS SIDE!
  Y <- reshape2::melt(y, id.vars=c(yescols, occcols), measure.vars=c("estimate", .paren))
  Y <- reshape2::dcast(Y, ... ~ model)

  ## Turn the metadata its side, too
  z <- reshape2::melt(z, id.vars="model", measure.vars=setdiff(colnames(z), c(colnames(z)[leftcol], "model")), variable.name="term")
  z <- z[stats::complete.cases(z), ]

  z$value <- paste0("\\multicolumn{1}{c}{", .round(z$value, .digit, 0), "}")

  Z <- reshape2::dcast(z, ... ~ model)

  #### Second munging block

  ## TODO: put any other reordering steps here, like suppressing or shuffling threshold terms
  Y <- Y[order(stringr::str_detect(Y$term, stringr::fixed("|"))), ]

  ## identify and take out non-varying columns
  s <- sapply(lapply(Y, unique), length) == 1
  S <- sapply(Y[, s, drop=FALSE], unique)
  Y <- Y[, setdiff(colnames(Y), names(S))]

  ## erase repeated values
  key_col <- which(colnames(Y)=="variable") - 1
  y_cols  <- ncol(Y) - key_col - 1
  x_cols  <- (1:key_col)[-key_col]

  Y[                        , key_col] <- paste0("\\rule{0pt}{2.25ex}", Y[, key_col])
  Y[Y$variable != "estimate", key_col] <- ""

  Y$variable <- NULL

  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], as.character)
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], function(x) { ## duplicated() ought to work here, but fails :(
    x[which(c(1, diff(as.numeric(as.factor(x)))) == 0)] <- ""
    x
  })

  ## do our own sanitizing
  ## TODO: must be much more comprehensive
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace, pattern="_", replacement=" ")

  ## TODO: also sanitize the metadata

  ## retain only those metadata requested in the arguments
  W <- Z[Z$term %in% .above, ]
  Z <- Z[Z$term %in% .below, ]

  ## also mung the metadata
  Z$term <- as.character(Z$term)
  Z$term <- ifelse(Z$term==Z$term[1], paste0("\\rule{0pt}{2.25ex}", Z$term[1]), Z$term)

  #### End second munging block

  rows <- nrow(Y)

  ## Bolt the metadata onto the main table
  Y <- plyr::rbind.fill(Y, Z)

  ## Add notes

  z_rows <- nrow(Y)

  star_note <- sapply(lapply(seq_len(length(.starz)), rep.int, x=".charz"), paste0, collapse="")
  star_note <- paste(star_note, rep("p <", length(.starz)), .starz)
  star_note <- Reduce(.slap, star_note)

  par_note  <- c("p.value" = "p-values", "std.error" = "standard errors")[.paren]
  par_note  <- paste(par_note, rep("in parentheses", length(par_note)))

  bar_note  <- Reduce(.slap, c(star_note, par_note))
  foo_note  <- Reduce(.slap, paste(names(S), as.character(S), sep=": "))

  note      <- Reduce(.slap, c(foo_note, bar_note))

  note      <- paste0(rep(paste0("\\hline \\multicolumn{", ncol(Y), "}{r}{"), length(note)), note, rep("} \\\\", length(note)))

  #### MOVE THAT BUS!

  a <- xtable::xtable(Y, .captn, align=c(rep("r", key_col + 1), rep("d", y_cols)))

  print(a, table.placement="!htb", include.rownames = FALSE, comment=FALSE, ...,
        hline.after = c(0, rows),
        add.to.row = list(pos=list(z_rows)[length(note)], command=as.character(note)),
        sanitize.text.function = function(x) {x},
        sanitize.colnames.function = function(x) {paste0("\\multicolumn{1}{c}{", x, "}")})
}

## TODO: hang on, nsmall < .digit helps in integer cases but hurts in others
## maybe figure out how to do this on a per-column basis for the unrotated metadata
.round <- function(x, .digit, nsmall=.digit) {
  y <- as.numeric(x)
  ifelse(is.na(y), x, sapply(round(y, .digit), format, nsmall=nsmall))
}

.slap <- function(x, y) {paste(x, y, sep="; ")}
