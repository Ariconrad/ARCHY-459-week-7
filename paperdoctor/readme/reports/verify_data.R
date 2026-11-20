# Verification script: reproduce the paper's pipeline and diagnose the dedup step
signbase <- readr::read_csv("https://ndownloader.figshare.com/files/24118688", show_col_types = FALSE)

cat("=== RAW DATA ===\n")
cat("rows:", nrow(signbase), "\n")
cat("distinct object_id:", dplyr::n_distinct(signbase$object_id), "\n")
cat("exact duplicate rows (all cols):", sum(duplicated(signbase)), "\n")
cat("duplicate object_id:", sum(duplicated(signbase$object_id)), "\n")
cat("countries:", dplyr::n_distinct(signbase$country), " sites:", dplyr::n_distinct(signbase$site_name), "\n")

focus <- dplyr::filter(signbase, country %in% c("Germany", "France"))
cat("\n=== DE+FR BEFORE DEDUP ===\n")
cat("rows:", nrow(focus), " (DE:", sum(focus$country == "Germany"), " FR:", sum(focus$country == "France"), ")\n")

dedup <- dplyr::distinct(focus, site_name, object_type, .keep_all = TRUE)
cat("\n=== AFTER distinct(site_name, object_type) ===\n")
cat("rows:", nrow(dedup), " (DE:", sum(dedup$country == "Germany"), " FR:", sum(dedup$country == "France"), ")\n")
cat("rows dropped:", nrow(focus) - nrow(dedup), "\n")

# How many dropped rows are actually distinct objects (unique object_id)?
dropped <- dplyr::anti_join(focus, dedup, by = c("site_name", "object_type"))
cat("dropped rows with unique object_id:", dplyr::n_distinct(dropped$object_id), "of", nrow(dropped), "\n")
cat("example dropped object_ids:", paste(head(dropped$object_id, 8), collapse = ", "), "\n")

# What would true duplicate removal (by object_id) give?
cat("\n=== TRUE DEDUP by object_id ===\n")
cat("rows:", nrow(dplyr::distinct(focus, object_id, .keep_all = TRUE)), "\n")

# Largest (site, object_type) groups that got collapsed
big <- focus |>
  dplyr::count(site_name, object_type, sort = TRUE) |>
  dplyr::filter(n > 1)
cat("\nbiggest collapsed groups:\n")
print(utils::head(big, 6))

# Reproduce paper numbers on the deduped sample
sign_freq <- dedup |>
  tidyr::pivot_longer(line:other, names_to = "sign", values_to = "present") |>
  dplyr::summarise(prop = mean(present, na.rm = TRUE), .by = c(country, sign))

pct <- function(ctry, s) round(100 * dplyr::filter(sign_freq, country == ctry, sign == s)$prop)
cat("\n=== PAPER NUMBERS ===\n")
cat("notch DE:", pct("Germany","notch"), "% FR:", pct("France","notch"), "%\n")
cat("line  DE:", pct("Germany","line"),  "% FR:", pct("France","line"),  "%\n")
cat("obline DE:", pct("Germany","obline"), "% FR:", pct("France","obline"), "%\n")
cat("vulva DE:", pct("Germany","vulva"), "% FR:", pct("France","vulva"), "%\n")

site_counts <- dedup |>
  dplyr::count(country, site_name) |>
  dplyr::mutate(share = n / sum(n), .by = country)
top1 <- site_counts |>
  dplyr::slice_max(n, n = 1, by = country, with_ties = FALSE)
cat("\ntop site DE:", top1$site_name[top1$country=="Germany"], top1$share[top1$country=="Germany"], "\n")
cat("top site FR:", top1$site_name[top1$country=="France"], top1$share[top1$country=="France"], "\n")

top3 <- site_counts |>
  dplyr::filter(country == "Germany") |>
  dplyr::slice_max(n, n = 3, with_ties = FALSE)
cat("top3 DE sites:", paste(top3$site_name, collapse = ", "),
    " = ", round(100 * sum(top3$share)), "%\n", sep = "")

# most frequent sign per country
cat("\nmost frequent sign DE:",
    dplyr::slice_max(dplyr::filter(sign_freq, country=="Germany"), prop, n=1, with_ties=FALSE)$sign, "\n")
cat("most frequent sign FR:",
    dplyr::slice_max(dplyr::filter(sign_freq, country=="France"), prop, n=1, with_ties=FALSE)$sign, "\n")

# sites per country
cat("sites DE:", dplyr::n_distinct(dedup$site_name[dedup$country=="Germany"]),
    " sites FR:", dplyr::n_distinct(dedup$site_name[dedup$country=="France"]), "\n")

# second most frequent sign in each country (for the "notches and lines" claim)
sf <- sign_freq |> dplyr::filter(country=="Germany") |> dplyr::arrange(dplyr::desc(prop))
cat("\nDE top4 signs:", paste(head(sf$sign,4), collapse=", "), "\n")
sf2 <- sign_freq |> dplyr::filter(country=="France") |> dplyr::arrange(dplyr::desc(prop))
cat("FR top4 signs:", paste(head(sf2$sign,4), collapse=", "), "\n")

# any site name shared across countries (left_join risk)?
de_sites <- unique(focus$site_name[focus$country=="Germany"])
fr_sites <- unique(focus$site_name[focus$country=="France"])
cat("\nsites in both countries:", paste(intersect(de_sites, fr_sites), collapse=", "), "\n")
