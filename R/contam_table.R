# --- 1. Data ----
thresholds_tbl <- tribble(
  ~Meals_month, ~Hg_Sensitive, ~Hg_General, ~PCB,     ~Dioxin_TEQ, ~Mirex,   ~Photomirex, ~Toxaphene,
  0,            ">0.5",        ">1.8",      ">844",   ">21.6",     ">657",   ">122",      ">1877",
  1,            NA,            NA,          "422-844","10.8-21.6", "329-657","61-122",    "939-1877",
  2,            NA,            NA,          "211-422","5.4-10.8",  "164-329","31-61",     "469-939",
  4,            "0.25-0.5",    "0.6-1.2",   "105-211","2.7-5.4",   "82-164", "15-31",     "235-469",
  8,            "0.16-0.25",   "0.4-0.6",   "70-105", "1.8-2.7",   "55-82",  "10-15",     "156-235",
  12,           "0.12-0.16",   "0.3-0.4",   "53-70",  "1.3-1.8",   "41-55",  "8-10",      "111-156",
  16,           "0.06-0.12",   "0.15-0.3",  "26-53",  "0.7-1.3",   "21-41",  "4-8",       "59-117",
  32,           "<0.06",       "<0.15",     "<26",    "<0.7",      "<21",    "<4",        "<59"
)

# --- 2. Flextable with multi-row header ----
contam_thr_ft <- flextable(thresholds_tbl) |> 
  # second header row (units + Meals column)
  set_header_labels(
    Meals_month = "Meals/\nmonth",
    Hg_Sensitive = "Sensitive",
    Hg_General   = "General",
    PCB          = "",
    Dioxin_TEQ   = "",
    Mirex        = "",
    Photomirex   = "",
    Toxaphene    = ""
  ) |>
  # top header row: group labels, with colwidths to merge
  add_header_row(
    values    = c("", "Hg (µg/g)", "PCB (ng/g)", "Dioxin/Furan/\ndlPCB TEQ (pg/g)",
                  "Mirex (ng/g)", "Photomirex (ng/g)", "Toxaphene (ng/g)"),
    colwidths = c(1,      2,         1,    1,                          1,       1,           1)
  ) |>
  merge_h(part = "header") |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  autofit()
contam_thr_ft = bold(contam_thr_ft, part = "header")

contam_thr_ft
