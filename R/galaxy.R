#' LaTeX regression tables
#'
#' @param x a model object, \code{wickr::\link[wickr]{sumer}} object, or \code{list} thereof
#' @param .captn character. Caption for the table. Default \code{NULL}.
#' @param .paren character. Value to parenthesize. \code{"p.value"}, \code{"none"}, or \code{"std.error"} (the default).
#' @param .starz numeric.   Significance levels. Default \code{c(0.05, 0.01, 0.001)}.
#' @param .charz character. Symbols for sugbufucabce kevels. Default \code{c("*", "**", "***")}.
#' @param .digit numeric.   Number of places to round decimals to. Default \code{3}.
#' @param .above character. Names of model metadata values to include ABOVE table. Default \code{NULL}.
#' @param .below character. Names of model metadata values to include BELOW table. Default \code{NULL}.
#' @param .place character. LaTeX table placement. Default \code{"!htb"}.
#' @param .tomit character. (Partial) names of terms to omit from table. Default \code{NULL}.
#' @param .comit character. (Partial) names of _components_ to omit from table. Default \code{NULL}.
#' @param .scien logical.   Use scientific notation? Default \code{NA} (auto).
#' @param .ndrop logical.   Leave a note about dropped columns? Actually rather annoying. Default \code{FALSE}.
#' @param .incol character. Columns by which to widen data. Only \code{"outcome"} supported. Default \code{NULL}.
#' @param .merge character. merge \code{level} into \code{term}? Default \code{FALSE}.
#' @param .float logical.   Do we all float down here? Default \code{TRUE}.
#' @param .order character. Terms to pull to the top of the table, in order. Default \code{NULL}.
#' @param .naout logical.   Drop rows for vars with NA estimates? Don't actually do this. Default \code{FALSE}.
#' @param .print logical.   Barf the result to the screen? \code{FALSE} to return _only_. Default \code{TRUE}.
#' @param .oterm logical.   Omit the \code{term} column if only one term is reported? Default \code{FALSE}.
#' @param .ttype character. Table type. \code{"latex"} or \code{"html"}. Default \code{"latex"}.
#' @param .space character. Space between rows, in LaTeX notation. Default \code{"2.25ex"}.
#' @param .title character. Columns to transform to title case. Default \code{NULL}.
#' @param .capup logical.   Caption at top of table if \code{.float==FALSE}? Default \code{FALSE} (at bottom).
#' @param .headr logical.   Display hierarchical header? Default \code{TRUE}.
#' @param .check character. Hierarchical organization columns to display below instead of above table. Default \code{NULL}.
#' @param .hinot character. STuff to put atop the hierarchical header (if any). Default \code{NULL}.
#' @param .point logical.   Align columns on decimal point? Default \code{TRUE}.
#' @param .notit logical.   Notes at bottom in italics? Default \code{FALSE}.
#' @param .toppr character. LaTeX code to put at the very top of the table. Default \code{NULL}.
#' @param .modno logical.   Number models? Default \code{TRUE}.
#' @param .align character. Alignment of left-side columns. Default \code{"r"}.
#' @param .tbold character. Bold these terms. Default \code{setdiff(.order, "(Intercept)")}.
#' @param .snote logical.   Note what \code{.charz} go with what \code{.starz}? Default \code{TRUE}.
#' @param .pnote logical.   Note what quantity is in parentheses? Default \code{FALSE}.
#' @param ... other arguments passed to \code{\link[xtable]{print.xtable}} and/or \code{\link[wickr]{sumer}}.
#' @return whatever \code{print.xtable} returns
#' @export
galaxy <- function(x,
                   .captn = NULL,
                   .paren = "std.error",
                   .starz = c(0.05, 0.01, 0.001),
                   .charz = c("*", "**", "***"),
                   .digit = 3,
                   .above = NULL,
                   .below = NULL,
                   .place = "!htb",
                   .tomit = NULL,
                   .comit = NULL,
                   .scien = NA,
                   .ndrop = FALSE,
                   .incol = character(0),
                   .merge = FALSE,
                   .float = TRUE,
                   .order = NULL,
                   .naout = FALSE,
                   .print = TRUE,
                   .oterm = FALSE,
                   .ttype = "latex",
                   .space = "2.25ex",
                   .title = NULL,
                   .capup = FALSE,
                   .headr = TRUE,
                   .check = NULL,
                   .hinot = NULL,
                   .point = TRUE,
                   .notit = FALSE,
                   .toppr = NULL,
                   .modno = TRUE,
                   .snote = TRUE,
                   .pnote = FALSE,
                   .align = "r",
                   .tbold = setdiff(.order, "(Intercept)"),
                   ...) {
  
  #### ensure valid values ----
  
  .paren <- if(.paren %in% c("p.value", "std.error", "none")) {.paren} else {
    warning(paste(.paren, "isn't an option for the parenthesized value; defaulting to 'std.error'"))
    "std.error"
  }

  .stord <- order(.starz, decreasing = TRUE)
  .charz <- c("",  .charz[.stord])
  .starz <- c(Inf, .starz[.stord])
  
  .merge <- any(isTRUE(as.logical(.merge))) | any(.merge == "level") # permissive but w/e
  
  .toppr <- Reduce(paste0, .toppr)

  #### Extraction block ----

  # x <- if(!(methods::is(x, "list") | methods::is(x, "tbl"))) {list(x)} else {x}

  y <- wickr::sumer(x, ...) ## a tidy summary of each model. recurses over lists of models
  
  if("component" %in% colnames(y)) {y$component[is.na(y$component)] <- "mean"} # allow beta model tables side by side with others
  
  z <- attr(y, "sumer")     ## a table of metadata values for the models in x

  #### First munging block
  #### TODO: make the steps in this block less crap
  
  # drop rows for vars with NA coefs if told to (though what a poor idea)
  y <- if(.naout) {dplyr::filter(y, !is.na(.data$estimate))} else {y}
  
  # merge levels into term column if told to
  # y$term      <- Reduce(.lifna, y[, c("term", .merge), drop=FALSE])
  # y[, .merge] <- NULL
  
  ## suppress unwanted terms
  omits <- lapply(.tomit, stringr::str_detect, string=y$term)
  omits <- lapply(omits, which)
  omits <- Reduce(union, omits)
  
  y <- y[setdiff(seq_along(y$term), omits), ]
  
  ## suppress unwanted components
  omits <- lapply(lapply(.comit, purrr::quietly(stringr::str_detect), string=y$component), `[[`, i="result")
  omits <- lapply(omits, which)
  omits <- Reduce(union, omits)
  
  y <- if("component" %in% colnames(y)) {y[setdiff(seq_along(y$component), omits), ]} else {y}
  
  # NOW merge levels into term column if told to
  # AND        empty out level column if told to
  y <- if(.merge) {dplyr::mutate(y, term = ifelse(is.na(.data$level), .data$term, .data$level), level = NA)} else {y}
  
  #### Experiment ----
  
  # drop non-varying non-essential columns
  Numcols <- intersect(c("estimate", "std.error", "statistic", "p.value"), colnames(y))
  Yescols <- intersect(c("term"), colnames(y))
  Yescols <- c(Yescols, Numcols)
  Zedcols <- intersect(colnames(z), colnames(y))
  
  # unset (and preserve) columns that don't vary
  y <- dplyr::group_by_at(y, Yescols)
  v <- dplyr::select_if(y, function(i) {length(table(i)) <= 1})
  y <- dplyr::select_if(y, function(i) {length(table(i)) >  1})
  
  v <- dplyr::group_by(v)
  v <- dplyr::select_at(v, setdiff(colnames(v), Yescols))
  v <- dplyr::group_by_all(v)
  v <- dplyr::summarise(v)
  v <- tidyr::gather(v, "fixcols", "fixvals")
  
  z <- dplyr::group_by(z)
  z <- dplyr::select_at(z, setdiff(colnames(z), setdiff(Zedcols, colnames(y))))
  
  # numeric columns to the right
  y <- dplyr::group_by_at(y, setdiff(colnames(y), Numcols))
  y <- dplyr::select_at(y, Numcols)
  
  # occasional (?) columns to the right
  # TODO: carve outcome back out, conditionally
  Occcols <- intersect(c("outcome", "component", "term", "level", "baseline", "versus"), colnames(y))
  Allcols <- c(Occcols, Numcols)
  y <- dplyr::group_by_at(y, setdiff(colnames(y), Allcols))
  y <- dplyr::select_at(y, Allcols)
  
  Rowcols <- dplyr::group_vars(y)
  
  z <- dplyr::group_by_at(z, Rowcols)
  z <- dplyr::select_at(z, setdiff(colnames(z), Rowcols))
  
  # get out model labels and assign numbers
  y <- dplyr::group_by_at(y, Rowcols)
  W <- dplyr::summarise(y)
  W <- dplyr::group_by(W)
  W <- dplyr::mutate(W, modno__ = 1:dplyr::n())
  W <- if(.modno) {W} else {dplyr::mutate(W, modno__ = factor(.data$modno__, labels = .data[[dplyr::last(Rowcols)]]))}
  W <- dplyr::group_by(W, .data$modno__)
  W <- dplyr::select_at(W, setdiff(colnames(W), "modno__"))
  
  # replace model labels with model numbers in y and z
  y <- dplyr::left_join(W, y)
  z <- dplyr::left_join(W, z)
  
  y <- dplyr::select_at(y, setdiff(colnames(y), Rowcols))
  z <- dplyr::select_at(z, setdiff(colnames(z), Rowcols))
  
  W <- if(.modno) {W} else {dplyr::select_at(W, setdiff(colnames(W), dplyr::last(Rowcols)))}
  
  # order threshold columns to bottom
  # TODO: this isn't done right
  # and it's super not clear why it's done here instead of only later
  y <- dplyr::mutate(y, is_threshold__ = stringr::str_detect(.data$term, stringr::fixed("|")))
  y <- dplyr::arrange_at(y, intersect(c("modno__", Occcols, "is_threshold__"), colnames(y)))
  y <- dplyr::select(y, -.data$is_threshold__)
  
  #### end experiment ----
  
  ## define groups of columns and the order we want columns to appear in from left to right
  # yescols <- intersect(c("component", setdiff("outcome", .incol), "term"), colnames(y))
  # occcols <- intersect(c("level", "baseline", "versus"),                   colnames(y))
  # numcols <- intersect(c("estimate", "std.error", "statistic", "p.value"), colnames(y))
  # 
  # outcols <- setdiff(colnames(y), c(occcols, numcols, "component", "term"))
  # yescols <- intersect(c("component", "term", setdiff(outcols, .incol)), colnames(y))
  # allcols <- c("model", yescols, occcols, numcols)
  # 
  # ## unset columns that don't vary
  # othcols <- setdiff(colnames(y), c("component", "term", setdiff("outcome", .incol), occcols, numcols))
  # fixcols <- othcols[sapply(lapply(y[, othcols, drop=FALSE], table, useNA="ifany"), length) < 2]
  # fixvals <- sapply(y[, fixcols, drop=FALSE], unique)
  # fixcols <- setdiff(fixcols, "Level 0") # bug fix?
  # 
  # y <- y[, setdiff(colnames(y), fixcols), drop=FALSE]
  # z <- z[, setdiff(colnames(z), fixcols), drop=FALSE]
  # 
  # y <- y[, c(setdiff(yescols, c("component", "term", fixcols)),
  #            intersect(yescols, c("component", "term")),
  #            occcols, numcols, .incol)]
  # 
  # ## create a model number column called "model"
  # termcol <- which(colnames(y) == yescols[1])
  # leftcol <- 1:ncol(y)
  # leftcol <- leftcol[leftcol < termcol]
  # 
  # modcols <- colnames(y)[leftcol]
  # 
  # # y <- y[do.call(order, y[, c(yescols, occcols)]), ]
  # y <- y[order(stringr::str_detect(y$term, stringr::fixed("|"))), ]
  # h <- y[, leftcol, drop=FALSE] # this kludge brought to you by a y with a col named "method", which also names an arg to order()
  # h <- if(ncol(h)) {`colnames<-`(h, paste0("_", colnames(h)))} else {h}
  # y <- if(length(leftcol) > 0) {y[do.call(order, h), ]} else {y}
  # 
  # y$model <- Reduce(.slap, y[, leftcol, drop=FALSE])
  # 
  # # sanitize model column, I guess?
  # # TODO: moar
  # y$model <- stringr::str_replace_all(y$model, "&", "\\\\&")
  # 
  # ## do likewise for the metadata
  # # z       <- data.frame(z, check.names=FALSE)
  # # z$model <- Reduce(.slap, z[, leftcol, drop=FALSE])
  # # z$model <- as.numeric(as.factor(z$model))
  # 
  # # this version ends up with the right # of cols
  # z <- dplyr::left_join(z, unique(y[, c(colnames(y)[leftcol], "model") ]))
  # 
  # # TODO: HERE
  # # HERE ----
  # 
  # # keep only desired columns
  # y <- y[, setdiff(c(.incol, allcols), c(fixcols, modcols))]
  # 
  # ## put something in place of missing values for columns that may not be meaningful for all models
  # y[, occcols] <- lapply(y[, occcols, drop=FALSE], function(w) {ifelse(is.na(w), "...", as.character(w))})
  
  # turn NA in character columns into "..." and turn all NAs into NA
  y <- dplyr::mutate_at(y, setdiff(colnames(y), c("modno__", Numcols)), function(i) {ifelse(is.na(i), "...", i)})
  y[is.na(y)] <- NA
  
  ## if the column to be included in parentheses doesn't exist, forget about it
  .paren <- intersect(.paren, colnames(y))

  ## format the numeric values that will be retained in the table
  y <- dplyr::mutate(y, " " = .charz[rowSums(outer(.data$p.value, .starz, "<"))])
  y <- dplyr::mutate(y, " " = ifelse(is.na(.data$` `), "", .data$` `))
  y <- dplyr::mutate(y, estimate = paste0(.round(.data$estimate, .digit, .scien=.scien), "^{", .data$` `, "}"))
  y <- dplyr::mutate_at(y, .paren, function(i) {paste0("(", .round(i, .digit, .scien=.scien), ")")})
  # y <- if(is.null(y$statistic)) {y} else {
  #   dplyr::mutate(y, estimate  = ifelse(is.na(.data$statistic), as.character(.data$statistic), .data$estimate))
  # }
  # TODO: how to do this tidily?
  # y <- if(is.null(y$statistic)) {y} else {
  #   dplyr::mutate_at(y, .paren, function(i, j) {ifelse(is.na(j), j, i)}, j = .data$statistic)
  # }
  
  # y$` `       <- .charz[rowSums(outer(y$p.value, .starz, "<"))]
  # y$estimate  <- paste0(.round(y$estimate, .digit, .scien=.scien), "^{", y$` `, "}")
  # y[, .paren] <- if(length(.paren)) {paste0("(", .round(y[[.paren]], .digit, .scien=.scien), ")")} # oh come on
  
  # y[, c("estimate", .paren)] <- if(is.null(y$statistic)) {y[, c("estimate", .paren)]} else {
  #   lapply(y[, c("estimate", .paren), drop=FALSE], ifelse, test=is.na(y$statistic), yes=y$statistic)
  # }
  
  ## establish the order that threshold terms are in to start with, and retain that order for later
  # p <- unique(y$term[stringr::str_detect(y$term, stringr::fixed("|"))])

  #### End first munging block ----

  ## THE MOMENT OF TRUTH! 
  ## TURN IT ON ITS SIDE! ----
  ## TODO: pivot to tidyr::pivot_wider() and tidyr::pivot_longer()
  # Y <- reshape2::melt(y, id.vars=setdiff(c("model", yescols, occcols), fixcols), measure.vars=c("estimate", .paren))
  # Y <- tidyr::gather(y, "variable", "value", .data$estimate, .data[[.paren]])
  Y <- tidyr::gather(y, "variable", "value", tidyselect::one_of(c(Numcols, " ")))
  Y <- dplyr::filter(Y, .data$variable %in% c("estimate", .paren))
  Y <- dplyr::mutate(Y, value = stringr::str_replace(.data$value, "^\\(NA\\)$", ""))
  Y <- dplyr::group_by(Y)
  Y <- dplyr::mutate_at(Y, setdiff(colnames(Y), c("modno__", "variable", "value")), .fsort)
  Y <- dplyr::mutate(Y, value = if(.point) {.data$value} else {paste0("\\multicolumn{1}{c}{$", .data$value, "$}")})
  Y <- tidyr::spread(Y, .data$modno__, .data$value)
  # Y[, setdiff(colnames(Y), c("variable", "value"))] <- lapply(Y[, setdiff(colnames(Y), c("variable", "value")), drop=FALSE], .fsort)
  # Y <- reshape2::dcast(Y, ... ~ model)
  # Y <- tidyr::spread(Y, .data$model, .data$value)

  ## Turn the metadata its side, too
  z <- tidyr::nest(z)
  z <- dplyr::mutate(z, data = list(tidyr::gather(dplyr::first(.data$data), "term", "val")))
  z <- tidyr::unnest(z, .data$data)
  z <- dplyr::filter_all(z, function(i) {!is.na(i)})
  z <- dplyr::mutate(z, val = paste0("\\multicolumn{1}{c}{", .round(.data$val, .digit, 0, .scien=.scien), "}"))
  z <- dplyr::group_by(z)
  z <- dplyr::mutate(z, term = .fsort(.data$term))
  Z <- tidyr::spread(z, .data$modno__, .data$val)

  # z <- reshape2::melt(z, 
  #                     id.vars=c("model", .incol),
  #                     measure.vars=setdiff(colnames(z), c(colnames(z)[leftcol], "model")), 
  #                     variable.name="term")
  # z <- z[stats::complete.cases(z), , drop=FALSE]
  # 
  # z$value <- if(nrow(z)) {paste0("\\multicolumn{1}{c}{", .round(z$value, .digit, 0, .scien=.scien), "}")} else {character(0)}
  # z$model <- .fsort(z$model)
  # 
  # Z <- reshape2::dcast(z[union(1, seq_along(z$term)), ], ... ~ model)
  
  ## Process the labels
  U <- dplyr::select_at(W, .check)
  U <- dplyr::mutate(U, OK____ = TRUE)
  U <- dplyr::mutate_if(U, is.logical, function(i) {ifelse(i, "\\checkmark{}", "")})
  U <- tidyr::gather(U, "term", "val", -.data$modno__)
  U <- dplyr::mutate(U, val = paste0("\\multicolumn{1}{c}{", .data$val, "}"))
  U <- tidyr::spread(U, .data$modno__, .data$val)
  U <- dplyr::filter(U, !.data$term == "OK____")
  
  W <- dplyr::group_by(W)
  W <- dplyr::mutate(W, OK____ = TRUE)
  W <- dplyr::select(W, -.data$modno__)
  W <- tidyr::gather(W, "col", "value")
  W <- dplyr::group_by(W, .data$col)
  W <- dplyr::summarise(W, rle = list(rle(.data$value)))
  W <- dplyr::group_by(W, .data$col)
  W <- dplyr::mutate(W, 
                     rle = list(lapply(`names<-`(names(dplyr::first(.data$rle)), names(dplyr::first(.data$rle))), 
                                       getElement, 
                                       object=dplyr::first(.data$rle))))
  W <- dplyr::mutate(W, rle = list(dplyr::as_tibble(dplyr::first(.data$rle))))
  W <- tidyr::unnest(W, .data$rle)
  W <- dplyr::mutate(W, to = cumsum(.data$lengths) + length(Occcols))
  W <- dplyr::mutate(W, from = .data$to - c(dplyr::first(.data$to) - length(Occcols) - 1, diff(.data$to) - 1))
  W <- dplyr::mutate(W, 
                     cell = paste0("\\multicolumn{", .data$lengths, "}{c}{", .data$values, "}"),
                     line = paste0("\\cmidrule(l){", .data$from, "-", .data$to, "}"))
  W <- dplyr::mutate(W, line = ifelse(.data$from < .data$to, .data$line, ""))
  H <- dplyr::summarise(W,
                        row = paste0(paste0(rep("\\multicolumn{1}{c}{ } & ", length(Occcols)), collapse = ""),
                                     paste0(.data$cell, collapse = " & "),
                                     " \\\\ ",
                                     paste0(.data$line, collapse = " ")))
  H <- dplyr::mutate(H, col = factor(.data$col, levels=Rowcols))
  H <- dplyr::filter(H, !.data$col %in% .check)
  H <- dplyr::filter(H, !.data$col  ==  "OK____")
  
  # paste(paste0("\\multicolumn{", lengths, "}{c}{", values, "}", collapse = "&"), 
  #       "\\\\", 
  #       paste0(paste0("\\cmidrule{l}{", to, "-", from), "}", collapse = " "))
  

  #### Second munging block ----

  # reorder terms (is this not done already?)
  Y <- dplyr::arrange(Y, !.data$term=="(Intercept)")                                  # intercept to top
  Y <- dplyr::mutate(Y, term = .fsort(as.character(.data$term)))                      # choose this order
  Y <- dplyr::arrange(Y, .data$term)                                                  # order by this order (???)
  Y <- dplyr::arrange(Y, stringr::str_detect(.data$term, stringr::fixed("|")))        # threshold terms to bottom
  Y <- if("component" %in% colnames(Y)) {dplyr::arrange(Y, .data$component)} else {Y} # sort by component
  # Y <- Y[order(!Y$term=="(Intercept)"), ]
  # Y <- Y[order(factor(Y$term, levels=c(.order, setdiff(unique(Y$term), .order)))), ]
  # Y <- Y[order(stringr::str_detect(Y$term, stringr::fixed("|"))), ]
  # # Y <- Y[order(if("component" %in% colnames(Y)) {Y$component} else {seq_along(Y[, 1])}), ]
  # P <- setdiff(union(intersect(.incol, colnames(Y)), colnames(Y)[1]), "term")
  # Y <- if(length(P)) {Y[do.call(order, Y[, P]), ]} else {Y}

  ## identify and take out non-varying columns (right side -- left side was done above)
  # s <- sapply(lapply(Y[, 1:which(colnames(Y)=="variable")], unique), length) == 1
  # S <- sapply(Y[, which(s), drop=FALSE], unique)
  # S <- if(nrow(Y)==1) {S[intersect(names(S), c("level", "baseline"))]} else {S}
  # Y <- Y[, setdiff(colnames(Y), setdiff(names(S), c("variable", "term")))]
  # S <- S[!S=="..."]
  
  nterm <- length(table(Y$term))

  ## erase repeated values
  key_col <- which(colnames(Y)=="variable") - 1
  y_cols  <- ncol(Y) - key_col - 1
  # x_cols  <- (1:key_col)[-key_col] # wtf?
  x_cols  <- (1:key_col)
  
  # To Title Case
  Y[, intersect(colnames(Y)[x_cols], .title)] <- lapply(Y[, intersect(colnames(Y)[x_cols], .title), drop = FALSE],
                                                        stringr::str_to_title)

  Y <- data.frame(Y, check.names=FALSE)
  # Y <- dplyr::mutate(Y, term = ifelse(.data$term %in% .tbold, paste0("\\textbf{", .data$term, "}"), .data$term))
  Y <- dplyr::mutate(Y, sp____ = ifelse(.data$term == dplyr::first(.data$term), "2.25ex", .space))
  Y[                        , key_col] <- paste0("\\rule{0pt}{", Y$sp____, "}", Y[, key_col])
  Y <- dplyr::select(Y, -.data$sp____)
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
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace_all, pattern="%", replacement="\\\\%")
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace_all, pattern="#", replacement="\\\\#")
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace_all, pattern="&", replacement="\\\\&")
  Y[, x_cols] <- lapply(Y[, x_cols, drop=FALSE], stringr::str_replace_all, pattern="`", replacement="")
  
  # TODO: the rest of the above for the colnames as well
  # Y <- `colnames<-`(Y, stringr::str_replace_all(colnames(Y), "&", "\\\\&"))

  ## TODO: also sanitize the metadata

  ## retain only those metadata requested in the arguments
  Z <- Z[Z$term %in% .below, ]

  ## also mung the metadata
  Z$term <- as.character(Z$term)
  Z$term <- ifelse(Z$term==Z$term[1], paste0("\\rule{0pt}{2.25ex}", Z$term[1]), Z$term)
  
  U$term <- as.character(U$term)
  U$term <- ifelse(U$term==U$term[1], paste0("\\rule{0pt}{2.25ex}", U$term[1]), U$term)

  #### End second munging block

  rows <- nrow(Y)

  ## Bolt the metadata onto the main table
  ## TODO: should work with dplyr::bind_rows
  Y <- dplyr::bind_rows(Y, U, Z)
  # Y <- plyr::rbind.fill(Y, Z)

  ## Add notes
  
  z_rows <- nrow(Y)

  # star_note <- sapply(lapply(seq_len(length(.starz)), rep.int, x=.charz), paste0, collapse="")
  # star_note <- paste(star_note, rep("p <", length(.starz)), .starz)
  star_note <- paste(.charz, rep("p <", length(.starz)), .starz)[-1]
  star_note <- Reduce(.sslap, star_note)

  par_note  <- c("p.value" = "p-values", "std.error" = "standard errors")[.paren]
  par_note  <- paste(par_note, rep("in parentheses", length(par_note)))

  foo_note  <- Reduce(.sslap, c(if(.snote) {star_note}, if(.pnote) {par_note}))
  # bar_note  <- if(.ndrop) {Reduce(.sslap, paste(names(S),       as.character(S),       sep=": "))} else {NULL}
  baz_note  <- if(.ndrop) {Reduce(.sslap, paste(v$fixcols, as.character(v$fixvals), sep=": "))} else {NULL}

  note      <- Reduce(.sslap, c(baz_note, 
                                # bar_note, 
                                foo_note))
  # note      <- if(.notit) {paste0("\\textit{", note, "}")} else {note}
  
  note      <- paste0(rep(paste0("\\hline \\multicolumn{", key_col, "}{l}{", 
                                 if(.notit) {"\\textit{"} else {""},
                                 "Note:",
                                 if(.notit) {"}"}         else {""},
                                 "}"),
                          length(note)),
                      rep(" & ", length(note)),
                      rep(paste0("\\multicolumn{", y_cols, "}{r}{"), length(note)), 
                      note,
                      rep("} \\\\", length(note)))
  note      <- c(note, "")[1] # in case note comes out NULL
  
  ## Add nested column headers
  
  chead <- paste(rev(H$row), collapse = " ")
  # foo <- stringr::str_split(colnames(Y), "___")
  # foo <- lapply(foo, rev)
  # 
  # colnames(Y) <- sapply(foo, `[[`, i=1)
  # foo <- lapply(foo, `[`, i=-1)
  # 
  # chead <- .chead(foo) # recursively create hierarchy of headers from nested model list names
  # chead <- paste(chead, collapse = " ")
  
  ## Format upper note
  .hinot <- paste0(rep(paste0("\\multicolumn{", key_col, "}{c}{ } & \\multicolumn{", y_cols, "}{c}{"), length(.hinot)),
                   .hinot,
                   rep("}", length(.hinot)))
  .hinot <- paste0(.hinot, collapse = " \\\\ ")[sign(length(.hinot))]
  .hinot <- paste0(.hinot,
                   " \\\\ "[sign(length(.hinot))],
                   paste0("\\cmidrule(l){", key_col + 1, "-", ncol(Y), "}")[sign(length(.hinot))])

  #### MOVE THAT BUS!

  a <- xtable::xtable(Y[, 1:(key_col + y_cols)], .captn, align=c(rep(.align, key_col + 1), rep("d", y_cols))) # does siunitx not work?
  colnames(a) <- colnames(Y) # sic
  
  if(.oterm & (nterm < 2)) {a$term <- ""}
  
  colnames(a) <- stringr::str_replace(colnames(a), "^term$",      " ")
  colnames(a) <- stringr::str_replace(colnames(a), "^component$", "  ")
  
  pre <- if(.float) {} else {paste0("\\begin{center}\n", if(.capup) {paste0("\\captionof{table}{", .captn, "}\n")})}
  end <- if(.float) {} else {paste0(if(!.capup) {paste0("\\captionof{table}{", .captn, "}\n")}, "\\end{center}\n")}
  mid <- invisible(print(a, table.placement=.place, include.rownames = FALSE, comment=FALSE, ...,
                         type = .ttype,
                         file="/dev/null", # up yours, Windows
                         floating = .float,
                         hline.after = c(0, rows),
                         add.to.row = list(pos=list(-1, 
                                                    -1,
                                                    -1,
                                                    rows + 1,
                                                    z_rows[length(note)]),
                                           command=c(if(length(.toppr)) {.toppr}    else {""}, 
                                                     if(length(.hinot)) {.hinot}    else {""},
                                                     if(.headr)         {chead}     else {""},
                                                     if(sign(nrow(U)))  {"\\hline"} else {""},
                                                     as.character(note))),
                         sanitize.text.function = function(x) {x},
                         sanitize.colnames.function = function(x) {paste0("\\multicolumn{1}{c}{", x, "}")}))
  
  if(.print) {
    cat(pre)
    cat(mid)
    cat(end)
  }
  
  invisible(paste(pre, mid, end)) # now returns something
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

#' @importFrom rlang .data
#' @export
rlang::.data


