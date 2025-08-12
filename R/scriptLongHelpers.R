#' @name scriptLongHelpers
#' @rdname scriptLongHelpers
#'
#' @title Helper-functions for scripting free, fixed, and equated families of parameters
#' for longitudinal dyadic SEMs
#'
#' @param dvn input dvn list from scrapeVarLong
#' @param lvar input character for whether scripting helpers target latent "X" or "Y" indicator variables in dvn
#' @param lvname input character to (arbitrarily) name LV in lavaan syntax
#' @param partner input character to indicate parameters for first or second dyad member
#' @param type input character to indicate whether parameters "fixed", "free", "equated_dy", "equated_long", or "equated_dy_long" in estimation
#' @family helpers

#' @noRd
longLoads <- function(dvn, lvar = "X", lvname, partner = "1", type = "free") {
    # Check for valid partner input
    if (!partner %in% c("1", "2")) {
        stop("Invalid partner argument. Use '1' or '2'.")
    }

    # Check for valid type input
    if (!type %in% c("free", "fixed", "equated_dy", "equated_long", "equated_dy_long")) {
        stop("Invalid type argument. Use 'free', 'fixed', 'equated_dy', 'equated_long', or 'equated_dy_long'.")
    }

    # Get the relevant variable names and number of waves
    if (partner == "1") {
        xvarnames <- dvn$p1xvarnames
        waves <- dvn$p1waves
        dist <- dvn$dist1
    } else if (partner == "2") {
        xvarnames <- dvn$p2xvarnames
        waves <- dvn$p2waves
        dist <- dvn$dist2
    }

    # Handle empty variable names
    if (length(xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate loading statements based on type
    if (type == "free") {
        # Free loadings - no constraints
        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            indicators <- xvarnames[[wave]]
            loading_statement <- paste0(
                latent_var_name, " =~ NA*", indicators[1], " + ",
                paste(indicators[-1], collapse = " + ")
            )
            lavaan_statements <- c(lavaan_statements, loading_statement)
        }
    } else if (type == "fixed") {
        # Fixed loadings - first indicator fixed to 1
        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            indicators <- xvarnames[[wave]]
            loading_statement <- paste0(
                latent_var_name, " =~ 1*", indicators[1], " + ",
                paste(indicators[-1], collapse = " + ")
            )
            lavaan_statements <- c(lavaan_statements, loading_statement)
        }
    } else if (type == "equated_dy") {
        # Equated across dyad members - same labels for both partners
        # Total labels needed: xindnum/2 (since same labels used for both partners)
        total_labels <- dvn$xindnum / 2
        labels <- paste0("l", tolower(lvar), 1:total_labels)

        label_index <- 1
        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            indicators <- xvarnames[[wave]]

            # Create loading statement with sequential labels
            labeled_indicators <- c()
            for (i in seq_along(indicators)) {
                if (label_index <= length(labels)) {
                    labeled_indicators[i] <- paste0(labels[label_index], "*", indicators[i])
                    label_index <- label_index + 1
                } else {
                    labeled_indicators[i] <- indicators[i]
                }
            }

            loading_statement <- paste0(
                latent_var_name, " =~ NA*", indicators[1], " + ",
                paste(labeled_indicators, collapse = " + ")
            )
            lavaan_statements <- c(lavaan_statements, loading_statement)
        }
    } else if (type == "equated_long") {
        # Equated across time - same labels across time for each partner
        # Labels per partner: p1xindnum/p1waves (or p2xindnum/p2waves)
        if (partner == "1") {
            labels_per_partner <- dvn$p1xindnum / dvn$p1waves
        } else {
            labels_per_partner <- dvn$p2xindnum / dvn$p2waves
        }

        # Generate labels for this partner
        if (partner == "1") {
            label_start <- 1
            label_end <- labels_per_partner
        } else {
            label_start <- labels_per_partner + 1
            label_end <- labels_per_partner * 2
        }

        labels <- paste0("l", tolower(lvar), label_start:label_end)

        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            indicators <- xvarnames[[wave]]

            # Create loading statement with sequential labels within wave
            labeled_indicators <- c()
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    labeled_indicators[i] <- paste0(labels[i], "*", indicators[i])
                } else {
                    labeled_indicators[i] <- indicators[i]
                }
            }

            loading_statement <- paste0(
                latent_var_name, " =~ NA*", indicators[1], " + ",
                paste(labeled_indicators, collapse = " + ")
            )
            lavaan_statements <- c(lavaan_statements, loading_statement)
        }
    } else if (type == "equated_dy_long") {
        # Equated across both dyad and time - same labels for everything
        # Labels needed: xindperwave (same labels for both partners and all time points)
        labels_needed <- dvn$xindperwave
        labels <- paste0("l", tolower(lvar), 1:labels_needed)

        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            indicators <- xvarnames[[wave]]

            # Create loading statement with sequential labels
            labeled_indicators <- c()
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    labeled_indicators[i] <- paste0(labels[i], "*", indicators[i])
                } else {
                    labeled_indicators[i] <- indicators[i]
                }
            }

            loading_statement <- paste0(
                latent_var_name, " =~ NA*", indicators[1], " + ",
                paste(labeled_indicators, collapse = " + ")
            )
            lavaan_statements <- c(lavaan_statements, loading_statement)
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longIntercepts <- function(dvn, lvar = "X", partner = "1", type = "free") {
    # Check for valid partner input
    if (!partner %in% c("1", "2")) {
        stop("Invalid partner argument. Use '1' or '2'.")
    }

    # Check for valid type input
    if (!type %in% c("free", "fixed", "equated_dy", "equated_long", "equated_dy_long")) {
        stop("Invalid type argument. Use 'free', 'fixed', 'equated_dy', 'equated_long', or 'equated_dy_long'.")
    }

    # Get the relevant variable names and number of waves
    if (partner == "1") {
        xvarnames <- dvn$p1xvarnames
        waves <- dvn$p1waves
    } else if (partner == "2") {
        xvarnames <- dvn$p2xvarnames
        waves <- dvn$p2waves
    }

    # Handle empty variable names
    if (length(xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate intercept statements based on type
    if (type == "free") {
        # Free intercepts - all variables get intercepts
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                intercept_statement <- paste0(indicators[i], " ~ 1")
                lavaan_statements <- c(lavaan_statements, intercept_statement)
            }
        }
    } else if (type == "fixed") {
        # Fixed intercepts - first indicator fixed to 0, others free
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i == 1) {
                    intercept_statement <- paste0(indicators[i], " ~ 0*1")
                } else {
                    intercept_statement <- paste0(indicators[i], " ~ 1")
                }
                lavaan_statements <- c(lavaan_statements, intercept_statement)
            }
        }
    } else if (type == "equated_dy") {
        # Equated across dyad members - same labels for both partners
        # Total labels needed: xindnum/2 (since same labels used for both partners)
        total_labels <- dvn$xindnum / 2
        labels <- paste0("t", tolower(lvar), 1:total_labels)

        label_index <- 1
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (label_index <= length(labels)) {
                    intercept_statement <- paste0(indicators[i], " ~ ", labels[label_index], "*1")
                    label_index <- label_index + 1
                } else {
                    intercept_statement <- paste0(indicators[i], " ~ 1")
                }
                lavaan_statements <- c(lavaan_statements, intercept_statement)
            }
        }
    } else if (type == "equated_long") {
        # Equated across time - same labels across time for each partner
        # Labels per partner: p1xindnum/p1waves (or p2xindnum/p2waves)
        if (partner == "1") {
            labels_per_partner <- dvn$p1xindnum / dvn$p1waves
        } else {
            labels_per_partner <- dvn$p2xindnum / dvn$p2waves
        }

        # Generate labels for this partner
        if (partner == "1") {
            label_start <- 1
            label_end <- labels_per_partner
        } else {
            label_start <- labels_per_partner + 1
            label_end <- labels_per_partner * 2
        }

        labels <- paste0("t", tolower(lvar), label_start:label_end)

        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    intercept_statement <- paste0(indicators[i], " ~ ", labels[i], "*1")
                } else {
                    intercept_statement <- paste0(indicators[i], " ~ 1")
                }
                lavaan_statements <- c(lavaan_statements, intercept_statement)
            }
        }
    } else if (type == "equated_dy_long") {
        # Equated across both dyad and time - same labels for everything
        # Labels needed: xindperwave (same labels for both partners and all time points)
        labels_needed <- dvn$xindperwave
        labels <- paste0("t", tolower(lvar), 1:labels_needed)

        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    intercept_statement <- paste0(indicators[i], " ~ ", labels[i], "*1")
                } else {
                    intercept_statement <- paste0(indicators[i], " ~ 1")
                }
                lavaan_statements <- c(lavaan_statements, intercept_statement)
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longResids <- function(dvn, lvar = "X", partner = "1", type = "free") {
    # Check for valid partner input
    if (!partner %in% c("1", "2")) {
        stop("Invalid partner argument. Use '1' or '2'.")
    }

    # Check for valid type input
    if (!type %in% c("free", "fixed", "equated_dy", "equated_long", "equated_dy_long")) {
        stop("Invalid type argument. Use 'free', 'fixed', 'equated_dy', 'equated_long', or 'equated_dy_long'.")
    }

    # Get the relevant variable names and number of waves
    if (partner == "1") {
        xvarnames <- dvn$p1xvarnames
        waves <- dvn$p1waves
    } else if (partner == "2") {
        xvarnames <- dvn$p2xvarnames
        waves <- dvn$p2waves
    }

    # Handle empty variable names
    if (length(xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate residual variance statements based on type
    if (type == "free") {
        # Free residual variances - all variables get residual variances
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                residual_statement <- paste0(indicators[i], " ~~ ", indicators[i])
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "fixed") {
        # Fixed residual variances - first indicator fixed to 0, others free
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i == 1) {
                    residual_statement <- paste0(indicators[i], " ~~ 0*", indicators[i])
                } else {
                    residual_statement <- paste0(indicators[i], " ~~ ", indicators[i])
                }
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "equated_dy") {
        # Equated across dyad members - same labels for both partners
        # Total labels needed: xindnum/2 (since same labels used for both partners)
        total_labels <- dvn$xindnum / 2
        labels <- paste0("th", tolower(lvar), 1:total_labels)

        label_index <- 1
        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (label_index <= length(labels)) {
                    residual_statement <- paste0(indicators[i], " ~~ ", labels[label_index], "*", indicators[i])
                    label_index <- label_index + 1
                } else {
                    residual_statement <- paste0(indicators[i], " ~~ ", indicators[i])
                }
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "equated_long") {
        # Equated across time - same labels across time for each partner
        # Labels per partner: p1xindnum/p1waves (or p2xindnum/p2waves)
        if (partner == "1") {
            labels_per_partner <- dvn$p1xindnum / dvn$p1waves
        } else {
            labels_per_partner <- dvn$p2xindnum / dvn$p2waves
        }

        # Generate labels for this partner
        if (partner == "1") {
            label_start <- 1
            label_end <- labels_per_partner
        } else {
            label_start <- labels_per_partner + 1
            label_end <- labels_per_partner * 2
        }

        labels <- paste0("th", tolower(lvar), label_start:label_end)

        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    residual_statement <- paste0(indicators[i], " ~~ ", labels[i], "*", indicators[i])
                } else {
                    residual_statement <- paste0(indicators[i], " ~~ ", indicators[i])
                }
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "equated_dy_long") {
        # Equated across both dyad and time - same labels for everything
        # Labels needed: xindperwave (same labels for both partners and all time points)
        labels_needed <- dvn$xindperwave
        labels <- paste0("th", tolower(lvar), 1:labels_needed)

        for (wave in 1:waves) {
            indicators <- xvarnames[[wave]]
            for (i in seq_along(indicators)) {
                if (i <= length(labels)) {
                    residual_statement <- paste0(indicators[i], " ~~ ", labels[i], "*", indicators[i])
                } else {
                    residual_statement <- paste0(indicators[i], " ~~ ", indicators[i])
                }
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longCoresidsDyad <- function(dvn, lvar = "X", type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "zero", "equated_long")) {
        stop("Invalid type argument. Use 'free', 'zero', or 'equated_long'.")
    }

    # Get the relevant variable names and number of waves
    p1xvarnames <- dvn$p1xvarnames
    p2xvarnames <- dvn$p2xvarnames
    waves <- dvn$p1waves # Should be same as p2waves

    # Handle empty variable names
    if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate residual covariance statements based on type
    if (type == "free") {
        # Free residual covariances - all cross-partner covariances at same time
        for (wave in 1:waves) {
            p1_indicators <- p1xvarnames[[wave]]
            p2_indicators <- p2xvarnames[[wave]]

            # Check if the number of indicators matches between partners
            if (length(p1_indicators) != length(p2_indicators)) {
                stop(paste("Mismatch in the number of indicators for wave:", wave))
            }

            # Generate residual covariance statements for this wave
            for (i in seq_along(p1_indicators)) {
                residual_statement <- paste0(p1_indicators[i], " ~~ ", p2_indicators[i])
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "zero") {
        # Zero residual covariances - all cross-partner covariances constrained to 0
        for (wave in 1:waves) {
            p1_indicators <- p1xvarnames[[wave]]
            p2_indicators <- p2xvarnames[[wave]]

            # Check if the number of indicators matches between partners
            if (length(p1_indicators) != length(p2_indicators)) {
                stop(paste("Mismatch in the number of indicators for wave:", wave))
            }

            # Generate zero residual covariance statements for this wave
            for (i in seq_along(p1_indicators)) {
                residual_statement <- paste0(p1_indicators[i], " ~~ 0*", p2_indicators[i])
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    } else if (type == "equated_long") {
        # Equated across time - same labels across time for cross-partner covariances
        # Labels needed: xindperwave (same labels for all time points)
        labels_needed <- dvn$xindperwave
        labels <- paste0("th", tolower(lvar), "d", 1:labels_needed)

        for (wave in 1:waves) {
            p1_indicators <- p1xvarnames[[wave]]
            p2_indicators <- p2xvarnames[[wave]]

            # Check if the number of indicators matches between partners
            if (length(p1_indicators) != length(p2_indicators)) {
                stop(paste("Mismatch in the number of indicators for wave:", wave))
            }

            # Generate labeled residual covariance statements for this wave
            for (i in seq_along(p1_indicators)) {
                if (i <= length(labels)) {
                    residual_statement <- paste0(p1_indicators[i], " ~~ ", labels[i], "*", p2_indicators[i])
                } else {
                    residual_statement <- paste0(p1_indicators[i], " ~~ ", p2_indicators[i])
                }
                lavaan_statements <- c(lavaan_statements, residual_statement)
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longCoresidsTime <- function(dvn, lvar = "X", type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "zero", "equated_dy")) {
        stop("Invalid type argument. Use 'free', 'zero', or 'equated_dy'.")
    }

    # Get the relevant variable names and number of waves
    p1xvarnames <- dvn$p1xvarnames
    p2xvarnames <- dvn$p2xvarnames
    waves <- dvn$p1waves # Should be same as p2waves

    # Handle empty variable names
    if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate residual covariance statements based on type
    if (type == "free") {
        # Free residual covariances - all cross-time covariances for same variable within partner
        for (partner in c("1", "2")) {
            if (partner == "1") {
                xvarnames <- p1xvarnames
            } else {
                xvarnames <- p2xvarnames
            }

            # For each variable, create covariances across time
            for (var_index in 1:length(xvarnames[[1]])) {
                # Get the variable name pattern (e.g., "X1", "X2", etc.)
                var_name <- xvarnames[[1]][var_index]
                # Extract the base variable name (remove partner and time suffix)
                base_var <- gsub("\\.[AB]\\.[0-9]+$", "", var_name)

                # Create covariances for this variable across time
                for (wave1 in 1:(waves - 1)) {
                    for (wave2 in (wave1 + 1):waves) {
                        # Get the actual variable names for these time points
                        var1 <- xvarnames[[wave1]][var_index]
                        var2 <- xvarnames[[wave2]][var_index]

                        residual_statement <- paste0(var1, " ~~ ", var2)
                        lavaan_statements <- c(lavaan_statements, residual_statement)
                    }
                }
            }
        }
    } else if (type == "zero") {
        # Zero residual covariances - all cross-time covariances constrained to 0
        for (partner in c("1", "2")) {
            if (partner == "1") {
                xvarnames <- p1xvarnames
            } else {
                xvarnames <- p2xvarnames
            }

            # For each variable, create zero covariances across time
            for (var_index in 1:length(xvarnames[[1]])) {
                # Get the variable name pattern (e.g., "X1", "X2", etc.)
                var_name <- xvarnames[[1]][var_index]
                # Extract the base variable name (remove partner and time suffix)
                base_var <- gsub("\\.[AB]\\.[0-9]+$", "", var_name)

                # Create zero covariances for this variable across time
                for (wave1 in 1:(waves - 1)) {
                    for (wave2 in (wave1 + 1):waves) {
                        # Get the actual variable names for these time points
                        var1 <- xvarnames[[wave1]][var_index]
                        var2 <- xvarnames[[wave2]][var_index]

                        residual_statement <- paste0(var1, " ~~ 0*", var2)
                        lavaan_statements <- c(lavaan_statements, residual_statement)
                    }
                }
            }
        }
    } else if (type == "equated_dy") {
        # Equated across dyad members - same labels for cross-time covariances across partners
        # For each variable, we need to equate its cross-time covariances between partners
        # The number of unique cross-time covariances per variable is: waves * (waves - 1) / 2
        # But since we're equating across partners, we need labels for each unique covariance pattern

        # Calculate total unique cross-time covariances per variable
        unique_covs_per_var <- waves * (waves - 1) / 2

        # Generate labels for the cross-time covariances
        labels <- paste0("th", tolower(lvar), "t", 1:unique_covs_per_var)

        # Process both partners
        for (partner in c("1", "2")) {
            if (partner == "1") {
                xvarnames <- p1xvarnames
            } else {
                xvarnames <- p2xvarnames
            }

            # For each variable, create labeled covariances across time
            for (var_index in 1:length(xvarnames[[1]])) {
                # Create covariances for this variable across time
                for (wave1 in 1:(waves - 1)) {
                    for (wave2 in (wave1 + 1):waves) {
                        # Get the actual variable names for these time points
                        var1 <- xvarnames[[wave1]][var_index]
                        var2 <- xvarnames[[wave2]][var_index]

                        # Calculate which label to use based on the wave combination
                        # This ensures the same wave combination gets the same label across variables and partners
                        label_index <- (wave1 - 1) * waves - (wave1 * (wave1 - 1)) / 2 + (wave2 - wave1)

                        if (label_index <= length(labels)) {
                            residual_statement <- paste0(var1, " ~~ ", labels[label_index], "*", var2)
                        } else {
                            residual_statement <- paste0(var1, " ~~ ", var2)
                        }

                        lavaan_statements <- c(lavaan_statements, residual_statement)
                    }
                }
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longLvars <- function(dvn, lvar = "X", lvname, partner = "1", type = "free") {
    # Check for valid partner input
    if (!partner %in% c("1", "2")) {
        stop("Invalid partner argument. Use '1' or '2'.")
    }

    # Check for valid type input
    if (!type %in% c("free", "fixed")) {
        stop("Invalid type argument. Use 'free' or 'fixed'.")
    }

    # Get the relevant variable names and number of waves
    if (partner == "1") {
        xvarnames <- dvn$p1xvarnames
        waves <- dvn$p1waves
        dist <- dvn$dist1
    } else if (partner == "2") {
        xvarnames <- dvn$p2xvarnames
        waves <- dvn$p2waves
        dist <- dvn$dist2
    }

    # Handle empty variable names
    if (length(xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate latent variance statements based on type
    if (type == "free") {
        # Free latent variances - all latent variables get free variances
        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            variance_statement <- paste0(latent_var_name, " ~~ NA*", latent_var_name)
            lavaan_statements <- c(lavaan_statements, variance_statement)
        }
    } else if (type == "fixed") {
        # Fixed latent variances - all latent variables fixed to 1
        for (wave in 1:waves) {
            latent_var_name <- paste0(lvname, wave, dist)
            variance_statement <- paste0(latent_var_name, " ~~ 1*", latent_var_name)
            lavaan_statements <- c(lavaan_statements, variance_statement)
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longLcovarsIntraclass <- function(dvn, type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "zero")) {
        stop("Invalid type argument. Use 'free' or 'zero'.")
    }

    # Get the relevant variable names and number of waves
    p1xvarnames <- dvn$p1xvarnames
    p2xvarnames <- dvn$p2xvarnames
    waves <- dvn$p1waves # Should be same as p2waves

    # Handle empty variable names
    if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate intraclass latent covariance statements (same time, different partners)
    for (wave in 1:waves) {
        # Create latent variable names for this time point
        latent1 <- paste0("X", wave, dvn$dist1) # e.g., X1A
        latent2 <- paste0("X", wave, dvn$dist2) # e.g., X1B

        if (type == "free") {
            covariance_statement <- paste0(latent1, " ~~ ", latent2)
        } else if (type == "zero") {
            covariance_statement <- paste0(latent1, " ~~ 0*", latent2)
        }

        lavaan_statements <- c(lavaan_statements, covariance_statement)
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longLcovarsIntraind <- function(dvn, type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "zero", "equated_dy")) {
        stop("Invalid type argument. Use 'free', 'zero', or 'equated_dy'.")
    }

    # Get the relevant variable names and number of waves
    p1xvarnames <- dvn$p1xvarnames
    p2xvarnames <- dvn$p2xvarnames
    waves <- dvn$p1waves # Should be same as p2waves

    # Handle empty variable names
    if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate intraindividual latent covariance statements (same partner, different times)
    for (partner in c("1", "2")) {
        if (partner == "1") {
            dist <- dvn$dist1
        } else {
            dist <- dvn$dist2
        }

        # For each latent variable, create covariances with subsequent latent variables
        for (wave1 in 1:(waves - 1)) {
            for (wave2 in (wave1 + 1):waves) {
                latent1 <- paste0("X", wave1, dist)
                latent2 <- paste0("X", wave2, dist)

                if (type == "free") {
                    covariance_statement <- paste0(latent1, " ~~ ", latent2)
                } else if (type == "zero") {
                    covariance_statement <- paste0(latent1, " ~~ 0*", latent2)
                } else if (type == "equated_dy") {
                    # Calculate label index based on wave combination
                    # Same labels applied to both partners
                    label_index <- (wave1 - 1) * waves - (wave1 * (wave1 - 1)) / 2 + (wave2 - wave1)
                    covariance_statement <- paste0(latent1, " ~~ psyxintra", label_index, "*", latent2)
                }

                lavaan_statements <- c(lavaan_statements, covariance_statement)
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longLcovarsInterind <- function(dvn, type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "zero", "equated_dy")) {
        stop("Invalid type argument. Use 'free', 'zero', or 'equated_dy'.")
    }

    # Get the relevant variable names and number of waves
    p1xvarnames <- dvn$p1xvarnames
    p2xvarnames <- dvn$p2xvarnames
    waves <- dvn$p1waves # Should be same as p2waves

    # Handle empty variable names
    if (length(p1xvarnames) == 0 || length(p2xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate interindividual latent covariance statements (different partners, different times)
    for (wave1 in 1:waves) {
        for (wave2 in 1:waves) {
            if (wave1 != wave2) { # Skip same time combinations (handled by intraclass)
                latent1 <- paste0("X", wave1, dvn$dist1) # e.g., X1A
                latent2 <- paste0("X", wave2, dvn$dist2) # e.g., X2B

                if (type == "free") {
                    covariance_statement <- paste0(latent1, " ~~ ", latent2)
                } else if (type == "zero") {
                    covariance_statement <- paste0(latent1, " ~~ 0*", latent2)
                } else if (type == "equated_dy") {
                    # Calculate label index based on wave combination
                    # Same labels applied to both partners
                    label_index <- (wave1 - 1) * waves + wave2
                    covariance_statement <- paste0(latent1, " ~~ psyxinter", label_index, "*", latent2)
                }

                lavaan_statements <- c(lavaan_statements, covariance_statement)
            }
        }
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}

#' @rdname scriptLongHelpers
#' @noRd
longLmeans <- function(dvn, lvar = "X", partner = "1", type = "free") {
    # Check for valid type input
    if (!type %in% c("free", "fixed")) {
        stop("Invalid type argument. Use 'free' or 'fixed'.")
    }

    # Get the relevant variable names and number of waves
    if (partner == "1") {
        xvarnames <- dvn$p1xvarnames
        dist <- dvn$dist1
    } else {
        xvarnames <- dvn$p2xvarnames
        dist <- dvn$dist2
    }

    # Handle empty variable names
    if (length(xvarnames) == 0) {
        return(character(0))
    }

    # Initialize an empty character vector for output
    lavaan_statements <- c()

    # Generate latent mean statements for each time point
    for (wave in 1:length(xvarnames)) {
        latent_var_name <- paste0(lvar, wave, dist)

        if (type == "free") {
            mean_statement <- paste0(latent_var_name, " ~ NA*1")
        } else if (type == "fixed") {
            mean_statement <- paste0(latent_var_name, " ~ 0*1")
        }

        lavaan_statements <- c(lavaan_statements, mean_statement)
    }

    # Return the lavaan statements as character output
    return(lavaan_statements)
}
