#' LaTeX regression tables
#'
#' @param x a model object, \code{wickr::\link[wickr]{sumer}} object, or \code{list} thereof
#' @param .captn character. Caption for the table. Default \code{NULL}.
#' @param .paren character. Value to parenthesize. \code{"p.value"}, \code{"none"}, or \code{"std.error"} (the default).
#' @param .starz numeric.   Significance levels. Default \code{c(0.05, 0.01, 0.001)}.
#' @param .charz character. Character by which to symbolize statistical significance. Default \code{"*"}.
#' @param .digit numeric.   Number of places to round decimals to. Default \code{3}.
#' @param .above character. Names of model metadata values to include ABOVE table. Default \code{NULL}.
#' @param .below character. Names of model metadata values to include BELOW table. Default \code{NULL}.
#' @param .place character. LaTeX table placement. Default \code{"!htb"}.
#' @param .tomit character. (Partial) names of terms to omit from table. Default \code{NULL}.
#' @param .scien logical.   Use scientific notation? Default \code{NA} (auto).
#' @param .ndrop logical.   Leave a note about dropped columns? Actually rather annoying. Default \code{FALSE}.
#' @param .incol character. Columns by which to widen data. Only \code{"outcome"} supported. Default \code{NULL}.
#' @param .merge character. Columns to merge with \code{"term"}. Only \code{"level"} supported. Default \code{NULL}.
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
                   .place = "!htb",
                   .tomit = NULL,
                   .scien = NA,
                   .ndrop = FALSE,
                   .incol = character(0),
                   .merge = NULL,
                   ...) {
  ## TODO: make use of .above, which is effectively ignored for now
  
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
  
  # merge levels into term column if told to
  y$term      <- Reduce(.lifna, y[, c("term", .merge), drop=FALSE])
  y[, .merge] <- NULL
  
  ## define groups of columns and the order we want columns to appear in from left to right
  yescols <- intersect(c("component", setdiff("outcome", .incol), "term"), colnames(y))
  occcols <- intersect(c("level", "baseline", "versus"),                   colnames(y))
  numcols <- intersect(c("estimate", "std.error", "statistic", "p.value"), colnames(y))
  allcols <- c("model", yescols, occcols, numcols)

  ## create a model number column called "model"
  termcol <- which(colnames(y) == yescols[1])
  leftcol <- 1:ncol(y)
  leftcol <- leftcol[leftcol < termcol]
  
  # y <- y[do.call(order, y[, c(yescols, occcols)]), ]
  y <- y[order(stringr::str_detect(y$term, stringr::fixed("|"))), ]
  y <- y[do.call(order, y[, leftcol, drop=FALSE]), ]
  
  y$model <- Reduce(.slap, y[, leftcol, drop=FALSE])

  ## do likewise for the metadata
  # z       <- data.frame(z, check.names=FALSE)
  # z$model <- Reduce(.slap, z[, leftcol, drop=FALSE])
  # z$model <- as.numeric(as.factor(z$model))
  z <- plyr::join(z, unique(y[, c(colnames(y)[leftcol], "model") ])) # this version ends up with the right # of cols
  
  # keep only desired columns
  y <- y[, allcols]

  ## put something in place of missing values for columns that may not be meaningful for all models
  y[, occcols] <- lapply(y[, occcols, drop=FALSE], function(w) {ifelse(is.na(w), "...", as.character(w))})

  ## if the column to be included in parentheses doesn't exist, forget about it
  .paren <- intersect(.paren, colnames(y))
  
  y[is.na(y)] <- NA

  ## format the numeric values that will be retained in the table
  y$` `       <- Reduce(paste0, lapply(lapply(.starz, `>=`, y$p.value), ifelse, yes=.charz, no=""))
  y$estimate  <- paste0(.round(y$estimate, .digit, .scien=.scien), "^{", y$` `, "}")
  y[, .paren] <- paste0("(", .round(y[, .paren], .digit, .scien=.scien), ")") # surprisingly, works even when .paren is character(0)
  
  y[, c("estimate", .paren)] <- if(is.null(y$statistic)) {y[, c("estimate", .paren)]} else {
    lapply(y[, c("estimate", .paren)], ifelse, test=is.na(y$statistic), yes=y$statistic)
  }
  
  ## establish the order that threshold terms are in to start with, and retain that order for later
  p <- unique(y$term[stringr::str_detect(y$term, stringr::fixed("|"))])

  #### End first munging block

  ## THE MOMENT OF TRUTH!
  ## TURN IT ON ITS SIDE!
  Y <- reshape2::melt(y, id.vars=c("model", yescols, occcols), measure.vars=c("estimate", .paren))
  Y[, setdiff(colnames(Y), c("variable", "value"))] <- lapply(Y[, setdiff(colnames(Y), c("variable", "value")), drop=FALSE], .fsort)
  Y <- reshape2::dcast(Y, ... ~ model)
  
  ## TODO: split the model name by "; ", keep only the last element, and rotate the rest and add to W below

  ## Turn the metadata its side, too
  z <- reshape2::melt(z, id.vars="model", measure.vars=setdiff(colnames(z), c(colnames(z)[leftcol], "model")), variable.name="term")
  z <- z[stats::complete.cases(z), , drop=FALSE]

  z$value <- if(nrow(z)) {paste0("\\multicolumn{1}{c}{", .round(z$value, .digit, 0, .scien=.scien), "}")} else {character(0)}
  z$model <- .fsort(z$model)

  Z <- reshape2::dcast(z[union(1, seq_along(z$term)), ], ... ~ model)

  #### Second munging block

  ## shuffle threshold terms
  ## TODO: put any other reordering steps here
  Y <- Y[order(!Y$term=="(Intercept)"), ]
  Y <- Y[order(stringr::str_detect(Y$term, stringr::fixed("|")), as.numeric(factor(Y$term, levels=p))), ]
  Y <- Y[order(if("component" %in% colnames(Y)) {Y$component} else {seq_along(Y[, 1])}), ]
  
  ## suppress unwanted terms
  ## why is this here instead of at the very beginning?
  omits <- lapply(.tomit, stringr::str_detect, string=Y$term)
  omits <- lapply(omits, which)
  omits <- Reduce(union, omits)
  
  Y <- Y[setdiff(seq_along(Y$term), omits), ]

  ## identify and take out non-varying columns
  s <- sapply(lapply(Y[, 1:which(colnames(Y)=="variable")], unique), length) == 1
  S <- sapply(Y[, which(s), drop=FALSE], unique)
  S <- if(nrow(Y)==1) {S[intersect(names(S), c("level", "baseline"))]} else {S}
  Y <- Y[, setdiff(colnames(Y), setdiff(names(S), c("variable", "term")))]
  S <- S[!S=="..."]

  ## erase repeated values
  key_col <- which(colnames(Y)=="variable") - 1
  y_cols  <- ncol(Y) - key_col - 1
  # x_cols  <- (1:key_col)[-key_col] # wtf?
  x_cols  <- (1:key_col)

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
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace_all, pattern="_", replacement=" ")

  ## TODO: also sanitize the metadata

  ## retain only those metadata requested in the arguments
  ## TODO: actually use W for something
  ## TODO: nah, fuck W, allow metadata below the table only
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

  star_note <- sapply(lapply(seq_len(length(.starz)), rep.int, x=.charz), paste0, collapse="")
  star_note <- paste(star_note, rep("p <", length(.starz)), .starz)
  star_note <- Reduce(.sslap, star_note)

  par_note  <- c("p.value" = "p-values", "std.error" = "standard errors")[.paren]
  par_note  <- paste(par_note, rep("in parentheses", length(par_note)))

  bar_note  <- Reduce(.sslap, c(star_note, par_note))
  foo_note  <- if(.ndrop) {Reduce(.sslap, paste(names(S), as.character(S), sep=": "))} else {NULL}

  note      <- Reduce(.sslap, c(foo_note, bar_note))

  note      <- paste0(rep(paste0("\\hline \\multicolumn{", key_col + y_cols, "}{r}{"), length(note)), note, rep("} \\\\", length(note)))
  
  ## Add nested column headers, in the jankiest way imaginable in this world or any other
  
  foo <- stringr::str_split(colnames(Y), "___")
  foo <- lapply(foo, rev)
  
  colnames(Y) <- sapply(foo, `[[`, i=1)
  foo <- lapply(foo, `[`, i=-1)
  
  chead <- .chead(foo) # recursively create hierarchy of headers from nested model list names
  chead <- paste(chead, collapse = " ")

  #### MOVE THAT BUS!

  a <- xtable::xtable(Y[, 1:(key_col + y_cols)], .captn, align=c(rep("r", key_col + 1), rep("d", y_cols)))
  colnames(a) <- colnames(Y) # sic
  colnames(a) <- stringr::str_replace(colnames(a), "^term$", " ")

  print(a, table.placement=.place, include.rownames = FALSE, comment=FALSE, ...,
        hline.after = c(0, rows),
        add.to.row = list(pos=list(-1, z_rows[length(note)]), command=c(chead, as.character(note))),
        sanitize.text.function = function(x) {x},
        sanitize.colnames.function = function(x) {paste0("\\multicolumn{1}{c}{", x, "}")})
}

## TODO: hang on, nsmall < .digit helps in integer cases but hurts in others
## maybe figure out how to do this on a per-column basis for the unrotated metadata
.round <- function(x, .digit, nsmall=.digit, .scien=.scien) {
  y <- as.numeric(x)
  ifelse(is.na(y), x, sapply(round(y, .digit), format, nsmall=nsmall, scientific=.scien))
}

.slap  <- function(x, y) {paste(x, y, sep="___")}
.sslap <- function(x, y) {paste(x, y, sep="; ")}
.cslap <- function(x, y) {paste(x, y, sep=": ")}

# recursive function to create hierarchical column headers
.chead <- function(foo) {
  bar <- sapply(foo, `[`, i=1)
  
  if(all(is.na(bar))) {return(NULL)}
  
  bar[is.na(bar)] <- " "
  
  # baz <- table(bar, useNA="ifany")[unique(bar)]
  baz <- rle(bar)
  
  # mouthful
  quack <- paste(
    paste0(
      "\\multicolumn{", 
      baz$lengths, 
      "}{c}{",
      baz$values, 
      "}", 
      collapse = " & "), 
    " \\\\ ",
    paste0(
      paste0(
        "\\cmidrule(l){",
        cumsum(baz$lengths) - baz$lengths + 1,
        "-",
        cumsum(baz$lengths)
      )[baz$values != " "],
      "}",
      collapse=" "
    )
  )
  
  c(.chead(lapply(foo, `[`, i=-1)), quack)
}

# factor but take the levels in the order they're presented instead of sorting
.fsort <- function(x) {factor(x, levels=unique(x))}

.lifna <- function(l, r) {ifelse(is.na(r), as.character(l), as.character(r))}
