# SPDX-License-Identifier: GPL-3.0-or-later
# ──────────────────────────────────────────────────────────────────────────
# Machine-specific input locations - TEMPLATE                            ####
# ──────────────────────────────────────────────────────────────────────────
#
# Copy this file to "local_paths.R" in the same folder and fill in the
# values for this machine. "local_paths.R" is listed in ".gitignore" and is
# never committed; this template is tracked, so keep it free of real paths.
#
# "HPLI score.R" and "HPLI weights.R" read it through load_local_paths()
# ("HPLI import.R") at the top of their settings section, and stop with an
# explicit message if it is absent, if a required setting below is missing
# from it, or if a file does not exist.
#
# Only INPUT locations, and what describes them, belong here. A script's
# own output paths (output_file, weights_file) are settings of that script,
# declared with the rest of its settings, and are not read from this file.

# The AERU PPDB export workbook, exactly as delivered (e.g.
# "PPDB-<licensee>-24-05-03.xlsx"), unmodified: the code reads its sheets
# "General", "Fate", "Aquatic Ecotox", "Terrestrial Ecotox" and "Human".
# Required - the export is licensed material and is not distributed with
# this code, so there is no default. Forward slashes work on Windows too.
ppdb_export_file <- ""

# Optional. The export's date, "YYYY-MM-DD". Leave it out when the file
# name ends in "-YY-MM-DD.xlsx", as AERU exports do: the date is read from
# there. When set, it is used instead, and a file name that says otherwise
# is reported.
# ppdb_export_date <- "2024-05-03"
