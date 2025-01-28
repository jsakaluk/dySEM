#' @name scriptHelpers
#' @rdname scriptHelpers
#'
#' @title Helper-functions for scripting free, fixed, and equated families of parameters
#'
#' @param dvn input dvn list from scrapeVarCross
#' @param lvar input character for whether scripting helpers target latent "X" or :Y" indicator variables in dvn
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "fixed", "free", "equated", or "equated_mv" in estimation
#' @family helpers

#' @noRd
multifac_loads <- function(dvn, partner, type = "free") {
  # Check for valid partner input
  if (!partner %in% c("1", "2")) {
    stop("Invalid partner argument. Use '1' or '2'.")
  }

  # Get the relevant variable names and starting label index
  if (partner == "1") {
    xvarnames <- dvn$p1xvarnames
    label_start <- 1
    label_end <- dvn$xindper
    latent_suffix <- "1"
  } else if (partner == "2") {
    xvarnames <- dvn$p2xvarnames
    latent_suffix <- "2"
    if (type == "equated") {
      label_start <- 1
      label_end <- dvn$xindper
    } else if (type %in% c("free", "fixed")) {
      label_start <- dvn$xindper + 1
      label_end <- dvn$indnum
    } else {
      stop("Invalid type argument. Use 'free', 'fixed', or 'equated'.")
    }
  }

  # Handle empty variable names
  if (length(xvarnames) == 0) {
    return(character(0))
  }

  # Generate labels for indicators
  if (length(label_start:label_end) != length(unlist(xvarnames))) {
    stop("Mismatch between label indices and number of indicators.")
  }
  labels <- paste0("lx", label_start:label_end)

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in xvarnames
  label_index <- 1 # Counter for labels
  for (latent_var in names(xvarnames)) {
    # Extract indicators for the latent variable
    indicators <- xvarnames[[latent_var]]

    # Add the suffix to the latent variable name
    latent_var_name <- paste0(latent_var, latent_suffix)

    # Construct the loading statement based on the type
    if (type == "free") {
      loading_statement <- paste0(
        latent_var_name, " =~ NA*", indicators[1], " + ",
        paste(paste0(labels[label_index:(label_index + length(indicators) - 1)], "*", indicators), collapse = " + ")
      )
    } else if (type == "fixed") {
      loading_statement <- paste0(
        latent_var_name, " =~ 1*", indicators[1], " + ",
        paste(paste0(labels[label_index:(label_index + length(indicators) - 1)], "*", indicators), collapse = " + ")
      )
    } else if (type == "equated") {
      loading_statement <- paste0(
        latent_var_name, " =~ NA*", indicators[1], " + ",
        paste(paste0(labels[label_index:(label_index + length(indicators) - 1)], "*", indicators), collapse = " + ")
      )
    } else {
      stop("Invalid type argument. Use 'free', 'fixed', or 'equated'.")
    }

    # Add the loading statement to the output
    lavaan_statements <- c(lavaan_statements, loading_statement)

    # Update the label index
    label_index <- label_index + length(indicators)
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_intercepts <- function(dvn, partner, type = "free") {
  # Check for valid partner input
  if (!partner %in% c("1", "2")) {
    stop("Invalid partner argument. Use '1' or '2'.")
  }

  # Check for valid type input
  if (!type %in% c("free", "fixed", "equated")) {
    stop("Invalid type argument. Use 'free', 'fixed', or 'equated'.")
  }

  # Get the relevant variable names and starting label index
  if (partner == "1") {
    xvarnames <- dvn$p1xvarnames
    label_start <- 1
    label_end <- dvn$xindper
  } else if (partner == "2") {
    xvarnames <- dvn$p2xvarnames
    if (type == "equated") {
      label_start <- 1
      label_end <- dvn$xindper
    } else if (type %in% c("free", "fixed")) {
      label_start <- dvn$xindper + 1
      label_end <- dvn$indnum
    }
  }

  # Handle empty variable names
  if (length(xvarnames) == 0) {
    return(character(0))
  }

  # Generate labels for intercepts
  labels <- paste0("t", label_start:label_end)

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in xvarnames
  label_index <- 1 # Counter for labels
  for (latent_var in names(xvarnames)) {
    # Extract indicators for the latent variable
    indicators <- xvarnames[[latent_var]]

    # Generate intercept statements
    for (i in seq_along(indicators)) {
      if (type == "free") {
        intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
      } else if (type == "fixed" && i == 1) {
        intercept_statement <- paste0(indicators[i], " ~ 0*1")
        lavaan_statements <- c(lavaan_statements, intercept_statement)
        intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
      } else if (type == "fixed") {
        intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
      } else if (type == "equated" && partner == "2") {
        intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
      } else {
        intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
      }

      # Add the intercept statement to the output
      lavaan_statements <- c(lavaan_statements, intercept_statement)

      # Update the label index
      label_index <- label_index + 1
    }
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_resids <- function(dvn, partner = "1", type = "free") {
  # Check for valid partner input
  if (!partner %in% c("1", "2")) {
    stop("Invalid partner argument. Use '1' or '2'.")
  }

  # Check for valid type input
  if (!type %in% c("free", "equated")) {
    stop("Invalid type argument. Use 'free' or 'equated'.")
  }

  # Get the relevant variable names and starting label index
  if (partner == "1") {
    xvarnames <- dvn$p1xvarnames
    label_start <- 1
    label_end <- dvn$xindper
  } else if (partner == "2") {
    xvarnames <- dvn$p2xvarnames
    if (type == "equated") {
      label_start <- 1
      label_end <- dvn$xindper
    } else if (type == "free") {
      label_start <- dvn$xindper + 1
      label_end <- dvn$indnum
    }
  }

  # Handle empty variable names
  if (length(xvarnames) == 0) {
    return(character(0))
  }

  # Generate labels for residual variances
  labels <- paste0("th", label_start:label_end)

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in xvarnames
  label_index <- 1 # Counter for labels
  for (latent_var in names(xvarnames)) {
    # Extract indicators for the latent variable
    indicators <- xvarnames[[latent_var]]

    # Generate residual variance statements
    for (i in seq_along(indicators)) {
      residual_statement <- paste0(indicators[i], " ~~ ", labels[label_index], "*", indicators[i])

      # Add the residual variance statement to the output
      lavaan_statements <- c(lavaan_statements, residual_statement)

      # Update the label index
      label_index <- label_index + 1
    }
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_coresids <- function(dvn, type = "free") {
  # Check for valid type input
  if (!type %in% c("free", "zero")) {
    stop("Invalid type argument. Use 'free' or 'zero'.")
  }

  # Get the relevant variable names for both partners
  p1xvarnames <- dvn$p1xvarnames
  p2xvarnames <- dvn$p2xvarnames

  # Handle empty variable names
  if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
    return(character(0))
  }

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in p1xvarnames (assuming p1 and p2 match)
  for (latent_var in names(p1xvarnames)) {
    # Extract indicators for each latent variable
    p1_indicators <- p1xvarnames[[latent_var]]
    p2_indicators <- p2xvarnames[[latent_var]]

    # Check if the number of indicators matches between partners
    if (length(p1_indicators) != length(p2_indicators)) {
      stop(paste("Mismatch in the number of indicators for latent variable:", latent_var))
    }

    # Generate residual covariance statements
    for (i in seq_along(p1_indicators)) {
      if (type == "free") {
        residual_statement <- paste0(p1_indicators[i], " ~~ ", p2_indicators[i])
      } else if (type == "zero") {
        residual_statement <- paste0(p1_indicators[i], " ~~ 0*", p2_indicators[i])
      }

      # Add the residual covariance statement to the output
      lavaan_statements <- c(lavaan_statements, residual_statement)
    }
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_lvars <- function(dvn, partner, type = "free") {
  # Check for valid partner input
  if (!partner %in% c("1", "2")) {
    stop("Invalid partner argument. Use '1' or '2'.")
  }

  # Check for valid type input
  if (!type %in% c("free", "constrain", "equate")) {
    stop("Invalid type argument. Use 'free', 'constrain', or 'equate'.")
  }

  # Get the relevant variable names and starting label index
  if (partner == "1") {
    xvarnames <- dvn$p1xvarnames
    label_start <- 1
    label_end <- length(xvarnames)
    suffix <- "1"
  } else if (partner == "2") {
    xvarnames <- dvn$p2xvarnames
    if (type == "equate") {
      label_start <- 1
      label_end <- length(dvn$p1xvarnames)
    } else {
      label_start <- length(dvn$p1xvarnames) + 1
      label_end <- length(dvn$p1xvarnames) + length(xvarnames)
    }
    suffix <- "2"
  }

  # Handle empty variable names
  if (length(xvarnames) == 0) {
    return(character(0))
  }

  # Generate labels for latent variances
  labels <- paste0("psi", label_start:label_end)

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in xvarnames
  label_index <- 1 # Counter for labels
  for (latent_var in names(xvarnames)) {
    # Add the partner-specific suffix to the latent variable name
    latent_var_name <- paste0(latent_var, suffix)

    # Construct the latent variance statement based on the type
    if (type == "free") {
      variance_statement <- paste0(
        latent_var_name, " ~~ ", labels[label_index], "*", latent_var_name, " + NA*", latent_var_name
      )
    } else if (type == "constrain") {
      variance_statement <- paste0(
        latent_var_name, " ~~ ", labels[label_index], "*", latent_var_name, " + 1*", latent_var_name
      )
    } else if (type == "equate" && partner == "2") {
      variance_statement <- paste0(
        latent_var_name, " ~~ ", labels[label_index], "*", latent_var_name, " + NA*", latent_var_name
      )
    } else if (type == "equate" && partner == "1") {
      variance_statement <- paste0(
        latent_var_name, " ~~ ", labels[label_index], "*", latent_var_name, " + NA*", latent_var_name
      )
    }

    # Add the variance statement to the output
    lavaan_statements <- c(lavaan_statements, variance_statement)

    # Update the label index
    label_index <- label_index + 1
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_lcovars <- function(dvn, type = "free") {
  # Check for valid type input
  if (!type %in% c("free", "zero")) {
    stop("Invalid type argument. Use 'free' or 'zero'.")
  }

  # Get the relevant variable names for both partners
  p1_vars <- names(dvn$p1xvarnames)
  p2_vars <- names(dvn$p2xvarnames)

  # Combine latent variable names with partner suffixes
  if (length(p1_vars) == 0 && length(p2_vars) == 0) {
    return(character(0))
  }

  all_latent_vars <- c(paste0(p1_vars, "1"), paste0(p2_vars, "2"))

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Generate latent covariances
  for (i in seq_along(all_latent_vars)) {
    for (j in seq_along(all_latent_vars)) {
      if (i < j) { # Avoid duplicates and self-covariances
        # Create the label using the indices of the latent variables
        label <- paste0("psi", i, j)

        if (type == "free") {
          covariance_statement <- paste0(
            all_latent_vars[i], " ~~ ", label, "*", all_latent_vars[j]
          )
        } else if (type == "zero") {
          covariance_statement <- paste0(
            all_latent_vars[i], " ~~ ", label, "*", all_latent_vars[j], " + 0*", all_latent_vars[j]
          )
        }

        # Add the covariance statement to the output
        lavaan_statements <- c(lavaan_statements, covariance_statement)
      }
    }
  }

  # Ensure statements remain in their original order
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
multifac_lmeans <- function(dvn, partner, type = "free") {
  # Check for valid partner input
  if (!partner %in% c("1", "2")) {
    stop("Invalid partner argument. Use '1' or '2'.")
  }

  # Check for valid type input
  if (!type %in% c("free", "fixed", "equated")) {
    stop("Invalid type argument. Use 'free', 'fixed', or 'equated'.")
  }

  # Get the relevant variable names and label start index
  if (partner == "1") {
    xvarnames <- dvn$p1xvarnames
    label_start <- 1
    suffix <- "1"
  } else if (partner == "2") {
    xvarnames <- dvn$p2xvarnames
    if (type == "equated") {
      label_start <- 1
    } else {
      label_start <- length(dvn$p1xvarnames) + 1
    }
    suffix <- "2"
  }

  # Handle empty variable names
  if (length(xvarnames) == 0) {
    return(character(0))
  }

  # Generate labels for latent means
  labels <- paste0("a", label_start:(label_start + length(xvarnames) - 1))

  # Initialize an empty character vector for output
  lavaan_statements <- c()

  # Iterate through the named vectors in xvarnames
  label_index <- 1 # Counter for labels
  for (latent_var in names(xvarnames)) {
    # Add the partner-specific suffix to the latent variable name
    latent_var_name <- paste0(latent_var, suffix)

    # Construct the latent mean statement based on the type
    if (type == "free") {
      mean_statement <- paste0(
        latent_var_name, " ~ ", labels[label_index], "*1"
      )
    } else if (type == "fixed") {
      mean_statement <- paste0(
        latent_var_name, " ~ ", labels[label_index], "*1 + 0*1"
      )
    } else if (type == "equated") {
      mean_statement <- paste0(
        latent_var_name, " ~ ", labels[label_index], "*1"
      )
    }

    # Add the mean statement to the output
    lavaan_statements <- c(lavaan_statements, mean_statement)

    # Update the label index
    label_index <- label_index + 1
  }

  # Return the lavaan statements as character output
  return(lavaan_statements)
}

#' @rdname scriptHelpers
#' @noRd
loads <- function(dvn, lvar = "X", lvname, partner="1", type = "free"){

  if(partner == "1" & type == "fixed" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "fixed" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "free" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist1"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_mv" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_mv" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "1" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist1"]], dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "fixed" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "fixed" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*",lvname, dvn[["dist2"]])
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_mv" & lvar == "X"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x[[i]]=sprintf("lx%s*%s",i, dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_mv" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ 1*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x[[i]]=sprintf("ly%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",(dvn[["xindper"]]+i), dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "2" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%s%s =~ NA*%s+",lvname, dvn[["dist2"]], dvn[["p2yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",(dvn[["yindper"]]+i), dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "free" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*",lvname)
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1xvarnames"]], collapse = "+"), "+",paste(dvn[["p2xvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "free" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*",lvname)
    eta.x = gsub(" ", "",paste(eta_x,paste(dvn[["p1yvarnames"]], collapse = "+"), "+",paste(dvn[["p2yvarnames"]], collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lxg%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x2[[i]]=sprintf("lxg%s*%s",i, dvn[["p2xvarnames"]][i])
    }

    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("lyg%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x2[[i]]=sprintf("lgy%s*%s",i, dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated_source" & lvar == "X"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1xvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x1[[i]]=sprintf("lx%s*%s",i, dvn[["p1xvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["xindper"]]) {
      eta.x2[[i]]=sprintf("lx%s*%s",(dvn[["xindper"]]+i), dvn[["p2xvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }else if(partner == "g" & type == "equated_source" & lvar == "Y"){
    eta_x = sprintf("%sDy =~ NA*%s+",lvname, dvn[["p1yvarnames"]][1])
    eta.x1 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x1[[i]]=sprintf("ly%s*%s",i, dvn[["p1yvarnames"]][i])
    }
    eta.x2 = list()
    for (i in 1:dvn[["yindper"]]) {
      eta.x2[[i]]=sprintf("ly%s*%s",(dvn[["yindper"]]+i), dvn[["p2yvarnames"]][i])
    }
    eta.x = gsub(" ", "",paste(eta_x,paste(eta.x1, collapse = "+"),"+",paste(eta.x2, collapse = "+")), fixed = T)
    return(eta.x)
  }
}

#' @rdname scriptHelpers
#' @noRd
intercepts <- function(dvn, lvar = "X", partner="1", type = "free"){
  if(partner == "1" & type == "fixed" & lvar == "X"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p1xvarnames"]][1])
    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "fixed" & lvar == "Y"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p1yvarnames"]][1])
    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "free" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p1yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p1yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated_mv" & lvar == "X"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + tx1*1", dvn[["p1xvarnames"]][1])

    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p1xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "1" & type == "equated_mv" & lvar == "Y"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + ty1*1", dvn[["p1yvarnames"]][1])

    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p1yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "fixed" & lvar == "X"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p2xvarnames"]][1])
    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "fixed" & lvar == "Y"){
    xints = list()
    xints[[1]] = sprintf("%s ~ 0*1", dvn[["p2yvarnames"]][1])
    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2xvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ 1", dvn[["p2yvarnames"]][i])
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    xints = list()
    for (i in 1:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    xints = list()
    for (i in 1:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p2yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated_mv" & lvar == "X"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + tx1*1", dvn[["p2xvarnames"]][1])

    for (i in 2:dvn[["xindper"]]) {
      xints[[i]]=sprintf("%s ~ tx%s*1", dvn[["p2xvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }else if(partner == "2" & type == "equated_mv" & lvar == "Y"){
    xints = list()
    xints[[1]] <- sprintf("%s ~ 0*1 + ty1*1", dvn[["p2yvarnames"]][1])

    for (i in 2:dvn[["yindper"]]) {
      xints[[i]]=sprintf("%s ~ ty%s*1", dvn[["p2yvarnames"]][i], i)
    }
    xints = paste(xints, collapse = "\n")
    return(xints)
  }
}

#' @rdname scriptHelpers
#' @noRd
resids <- function(dvn, lvar = "X", partner="1", type = "free"){
  if(partner == "1" & type == "free" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "free" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p1yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ thx%s*%s",dvn[["p1xvarnames"]][i],i, dvn[["p1xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "1" & type == "equated" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ thy%s*%s",dvn[["p1yvarnames"]][i],i, dvn[["p1yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p2xvarnames"]][i], dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "free" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ %s",dvn[["p2yvarnames"]][i], dvn[["p2yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated" & lvar == "X"){
    res = list()
    for (i in 1:dvn[["xindper"]]) {
      res[[i]]=sprintf("%s ~~ thx%s*%s",dvn[["p2xvarnames"]][i],i, dvn[["p2xvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }else if(partner == "2" & type == "equated" & lvar == "Y"){
    res = list()
    for (i in 1:dvn[["yindper"]]) {
      res[[i]]=sprintf("%s ~~ thy%s*%s",dvn[["p2yvarnames"]][i],i, dvn[["p2yvarnames"]][i])
    }
    res = paste(res, collapse = "\n")
    return(res)
  }
}

#' @rdname scriptHelpers
#' @noRd
coresids <- function(dvn, lvar = "X", type = "free"){
  if(lvar == "X" & type == "free"){
    coresids <- list()
    for (i in 1:dvn[["xindper"]]) {
      coresids[[i]] <- sprintf("%s ~~ %s",dvn[["p1xvarnames"]][i], dvn[["p2xvarnames"]][i])
    }
    coresids <- paste(coresids, collapse = "\n")
  }else if(lvar == "Y" & type == "free"){
    coresids <- list()
    for (i in 1:dvn[["yindper"]]) {
      coresids[[i]] <- sprintf("%s ~~ %s",dvn[["p1yvarnames"]][i], dvn[["p2yvarnames"]][i])
    }
    coresids <- paste(coresids, collapse = "\n")
  }
  return(coresids)
}

#' @rdname scriptHelpers
#' @noRd
lvars <- function(dvn, lvar = "X", lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    return(lvar)
  }else if(partner == "1" & type == "equated"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ psix*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ psiy*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }
    return(lvar)
  }else if(partner == "1" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psix*%s%s", lvname, dvn[["dist1"]],lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psiy*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist1"]],lvname, dvn[["dist1"]])
    }
    return(lvar)
  }else if (partner == "2" & type == "fixed"){
    lvar <- sprintf("%s%s ~~ 1*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "free"){
    lvar <- sprintf("%s%s ~~ NA*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    return(lvar)
  }else if(partner == "2" & type == "equated"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ psix*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ psiy*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }
    return(lvar)
  }else if(partner == "2" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psix*%s%s", lvname, dvn[["dist2"]],lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      lvar <- sprintf("%s%s ~~ 1*%s%s + psiy*%s%s",lvname, dvn[["dist2"]],lvname, dvn[["dist2"]],lvname, dvn[["dist2"]])
    }
    return(lvar)
  }else if(partner == "g" & type == "fixed"){
    lvar <- sprintf("%sDy ~~ 1*%sDy",lvname,lvname)
    return(lvar)
  }else if(partner == "g" & type == "free"){
    lvar <- sprintf("%sDy ~~ NA*%sDy",lvname,lvname)
    return(lvar)
  }else if(partner == "g" & type == "equated_ff"){
    if(lvar == "X"){
      lvar <- sprintf("%sDy ~~ NA*%sDy + psix*%sDy",lvname, lvname, lvname)
    }else if(lvar == "Y"){
      lvar <- sprintf("%sDy ~~ NA*%sDy + psiy*%sDy",lvname, lvname, lvname)
    }
    return(lvar)
  }
}

#' @rdname scriptHelpers
#' @noRd
lcovars <- function(dvn, lvname, type = "free"){
  if(type == "free"){
    lcovar <- sprintf("%s%s ~~ %s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])
  }else if(type == "zero"){
    lcovar <- sprintf("%s%s ~~ 0*%s%s",lvname, dvn[["dist1"]],lvname, dvn[["dist2"]])
  }
  return(lcovar)
}


#' @rdname scriptHelpers
#' @noRd
lmeans <- function(dvn, lvar = "X", lvname, partner = "1", type = "free"){
  if(partner == "1" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist1"]])
    return(alpha)
  }else if(partner == "1" & type == "equated"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ alphax*1",lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ alphay*1",lvname, dvn[["dist1"]])
    }
    return(alpha)
  }else if(partner == "1" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ 0*1 + alphax*1",lvname, dvn[["dist1"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ 0*1 + alphay*1",lvname, dvn[["dist1"]])
    }
    return(alpha)
  }else if (partner == "2" & type == "fixed"){
    alpha <- sprintf("%s%s ~ 0*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "free"){
    alpha <- sprintf("%s%s ~ NA*1",lvname, dvn[["dist2"]])
    return(alpha)
  }else if(partner == "2" & type == "equated"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ alphax*1",lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ alphay*1",lvname, dvn[["dist2"]])
    }
    return(alpha)
  }else if(partner == "2" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%s%s ~ 0*1 + alphax*1",lvname, dvn[["dist2"]])
    }else if(lvar == "Y"){
      alpha <- sprintf("%s%s ~ 0*1 + alphay*1",lvname, dvn[["dist2"]])
    }
    return(alpha)
  }else if(partner == "g" & type == "free"){
    alpha <- sprintf("%sDy ~ NA*1",lvname)
    return(alpha)
  }else if(partner == "g" & type == "fixed"){
    alpha <- sprintf("%sDy ~ 0*1",lvname)
    return(alpha)
  }else if(partner == "g" & type == "equated_ff"){
    if(lvar == "X"){
      alpha <- sprintf("%sDy ~ 0*1 + alphax*1",lvname)
    }else if(lvar == "Y"){
      alpha <- sprintf("%sDy ~ 0*1 + alphay*1",lvname)
    }
    return(alpha)
  }
}
