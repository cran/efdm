# @export
#transition_grow <- function(act, var, howmuch) {
#   A <- act$A
#   var <- paste0(var, "1")
#   v <- A[[var]]
#   if(length(howmuch) == 1)
#     howmuch <- rep(howmuch, max(v))
#   if(length(howmuch) < max(v))
#     stop("Not enough levels in howmuch")
#   v <- pmax(pmin(v + howmuch[v], max(v)), 1)
#   A[[var]] <- v
#   names1 <- c("nobs", "N", "prob")
#   A <- aggregate(A[names1], A[setdiff(names(A), names1)], sum)
#   A$prob <- A$nobs / A$N
#   act$A <- A
#   act
# }
#


# yhdistä transition matriiseja
# tarkista että muuttujanimet on samat
# muuttujien luokat on samat (ei tule NA:ta)
# combine_transitions <- function(act1, act2, weight1, weight2) {
#   A <- merge(transprobs(thin), transprobs(noman), by=setdiff(names(transprobs(thin)), c("nobs", "N", "prob")), all=TRUE)
#   A <- A %>% mutate(prob = (80*coalesce(prob.x, 0) + 20*coalesce(prob.y, 0))/100) %>%
#     select(-c(nobs.x, N.x, prob.x, nobs.y, N.y, prob.y))
#   nt <- define_activity("noman20_thin80", c("vol", "age"))
#   transprobs(nt) <- A
# }
