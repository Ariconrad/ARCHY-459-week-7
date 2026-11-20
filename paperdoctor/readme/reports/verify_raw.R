# Compare narrative claims against RAW (undeduped) DE+FR data
signbase <- readr::read_csv("https://ndownloader.figshare.com/files/24118688", show_col_types = FALSE)
focus <- dplyr::filter(signbase, country %in% c("Germany", "France"))

raw_freq <- focus |>
  tidyr::pivot_longer(line:other, names_to = "sign", values_to = "present") |>
  dplyr::summarise(prop = mean(present, na.rm = TRUE), n_present = sum(present, na.rm = TRUE), .by = c(country, sign))

pct <- function(ctry, s) round(100 * dplyr::filter(raw_freq, country == ctry, sign == s)$prop)
cat("=== RAW DATA (412 objects, no dedup) ===\n")
cat("notch DE:", pct("Germany","notch"), "% FR:", pct("France","notch"), "%\n")
cat("line  DE:", pct("Germany","line"),  "% FR:", pct("France","line"),  "%\n")
cat("obline DE:", pct("Germany","obline"), "% FR:", pct("France","obline"), "%\n")
cat("vulva DE:", pct("Germany","vulva"), "% FR:", pct("France","vulva"), "%\n")

# any German vulva objects at all?
v <- dplyr::filter(focus, vulva == 1)
cat("\nraw vulva=1 objects by country:\n")
print(table(v$country))

# most frequent signs per country (raw)
for (ct in c("Germany","France")) {
  top <- raw_freq |> dplyr::filter(country == ct) |> dplyr::arrange(dplyr::desc(prop))
  cat(ct, "top4:", paste(head(top$sign, 4), collapse = ", "), "\n")
}

# raw site shares
sc <- focus |> dplyr::count(country, site_name) |>
  dplyr::mutate(share = n / sum(n), .by = country)
t1 <- sc |> dplyr::slice_max(n, n = 1, by = country, with_ties = FALSE)
cat("\nraw top site DE:", t1$site_name[t1$country=="Germany"], round(100*t1$share[t1$country=="Germany"]), "%\n")
cat("raw top site FR:", t1$site_name[t1$country=="France"], round(100*t1$share[t1$country=="France"]), "%\n")
t3 <- sc |> dplyr::filter(country=="Germany") |> dplyr::slice_max(n, n = 3, with_ties = FALSE)
cat("raw top3 DE:", paste(t3$site_name, collapse=", "), "=", round(100*sum(t3$share)), "%\n")

cat("\nraw sites DE:", dplyr::n_distinct(focus$site_name[focus$country=="Germany"]),
    " FR:", dplyr::n_distinct(focus$site_name[focus$country=="France"]), "\n")

# obline/vulva raw counts (for the "fewer than ten objects" claim)
cat("\nraw obline counts: DE", sum(focus$obline[focus$country=="Germany"], na.rm=TRUE),
    " FR", sum(focus$obline[focus$country=="France"], na.rm=TRUE), "\n")
cat("raw vulva counts:  DE", sum(focus$vulva[focus$country=="Germany"], na.rm=TRUE),
    " FR", sum(focus$vulva[focus$country=="France"], na.rm=TRUE), "\n")
cat("raw notch counts:  DE", sum(focus$notch[focus$country=="Germany"], na.rm=TRUE),
    " FR", sum(focus$notch[focus$country=="France"], na.rm=TRUE), "\n")
