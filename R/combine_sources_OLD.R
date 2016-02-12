# combine_Ctrax_idTracker_old <- function(tracks1, tracks2, err_cutoff, report) {
#   # Input handling -------------------------------------------------------------
#   ctrax <- switch(tracks1$param$source,
#                   'Ctrax' = tracks1,
#                   'idTracker' = tracks2)
#   ct <- ctrax$tr
#   idtracker <- switch(tracks2$param$source,
#                       'Ctrax' = tracks1,
#                       'idTracker' = tracks2)
#   id <- idtracker$tr
#
#   # Helper functions -----------------------------------------------------------
#   super_spread <- function(dat) {
#     Xs <- dplyr::select_(dat, ~animal, ~frame, ~X)
#     Xs <- tidyr::spread_(Xs, 'animal', 'X')
#     names(Xs) <- c('frame', paste0('X', 1:(length(Xs) - 1)))
#     Ys <- dplyr::select_(dat, ~animal, ~frame, ~Y)
#     Ys <- tidyr::spread_(Ys, 'animal', 'Y')
#     names(Ys) <- c('frame', paste0('Y', 1:(length(Ys) - 1)))
#     dplyr::full_join(Xs, Ys, by = 'frame')
#   }
#
#   # Combining ------------------------------------------------------------------
#   # Strategy as follows: start with ctrax, and for each animal in each trial we
#   # merge the appropriate coordinates from the 'true' idtracker animals. For
#   # each frame, we calculate differences, and then we assign minimal difference
#   # id's. We keep track of how well they fit together.
#
#   # Start by splitting per trial
#   ctsplit <- split(ct, ct$trial)
#   idsplit <- split(id, id$trial)
#   idsplit <- lapply(idsplit, super_spread)
#   # check trial names
#   if (any(names(ctsplit) != names(idsplit)))
#     stop('Trial names for Ctracks and idTracker tracks are not the same.',
#          call. = FALSE)
#   # Join the spread out idtracker tables to the ctrax tables
#   d <- mapply(dplyr::left_join, ctsplit, idsplit, by = 'frame',SIMPLIFY = FALSE)
#   d <- dplyr::bind_rows(d)
#   max.X <- substring(tail(names(d)[grep('X', names(d))], 1), 2)
#   dots <- lapply(1:max.X, function(i)
#     lazyeval::interp(~abs(X - x) ^ 2 + abs(Y - y) ^ 2,
#                      x = as.name(paste0('X', i)), y = as.name(paste0('Y', i))))
#   dots <- setNames(dots, paste0('dist', 1:max.X))
#   d <- dplyr::mutate_(d, .dots = dots)
#   message('Currently only works with two individuals in the idTracker part.')
#   d <- dplyr::mutate_(d, combine_err = ~pmin(dist1, dist2))
#   d$match <- max.col(cbind(d$dist1, d$dist2) * -1)
#   # Get rid of all the extra columns we've created
#   d <- dplyr::select(d, one_of(c(names(ct), 'match', 'combine_err')))
#
#   # Important to note is that we may have matched the same idtrack id to
#   # multiple ctrax id's in the same frame. We'll select the one with the lowest
#   # error and filter out the rest. Ideally, one would take this into account
#   # while matching. We also lose any frames that ctrax tracked, but idtracker
#   # did not (in most cases this will be hands etc., so no big deal?).
#   # This operation is very slow (rewrite in data.table??).
#   d <- dplyr::group_by_(d, ~trial, ~frame, ~match)
#   d <- dplyr::slice_(d, ~which.min(combine_err))
#   ctrax_lost <- (table(ct$trial) - table(d$trial)) / table(ct$trial)
#
#   # Now we join idtracker back in, so we can start to select which vars to use.
#   id$animal <- as.numeric(id$animal)
#   d <- dplyr::full_join(id, d, by = c('trial', 'frame', 'animal' = 'match'))
#   d$animal <- as.factor(d$animal)
#   # We take the average for X and Y, and drop the ones where we don't have a
#   # match. (Maybe make this drop optional..)
#   d <- dplyr::mutate_(d,
#                       X = ~rowMeans(cbind(X.x, X.y)),
#                       Y = ~rowMeans(cbind(Y.x, Y.y)))
#   d <- dplyr::select_(d, ~-X.x, ~-X.y, ~-Y.x, ~-Y.y, ~-animal.y)
#   d <- dplyr::filter_(d, ~!is.na(X))
#   idtracker_lost <- (table(id$trial) - table(d$trial)) / table(id$trial)
#
#   if (!is.null(err_cutoff)) {
#     cut_off <- table(d$combine_err > 25 ^ 2, d$trial)[2,] / table(d$trial)
#     d <- filter(d, combine_err < err_cutoff)
#   }
#
#   if (report) {
#     tab <- round(data.frame(Ctrax.no.match = c(ctrax_lost),
#                             idTracker.no.match = c(idtracker_lost),
#                             above.cutoff = c(cut_off)) * 100, 2)
#     tab <- rbind(tab, Average = round(apply(tab, 2, mean), 2))
#     cat('The percentages of id\'s that weren\'t matched:\n')
#     if (requireNamespace('knitr', quietly = TRUE))
#       print(knitr::kable(tab))
#     else {
#       print(tab)
#     }
#   }
#   return(d)
# }
