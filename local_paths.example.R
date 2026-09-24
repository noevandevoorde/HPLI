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
# explicit message if it is absent, if a setting below is missing from it,
# or if a folder does not exist.
#
# Only INPUT locations belong here. A script's own output paths
# (output_file, weights_file) are settings of that script, declared with
# the rest of its settings, and are not read from this file.

# Folder holding an AERU PPDB export as four files: General.xlsx,
# Fate.xlsx, Ecotox.xlsx, Human.xlsx. Required - the export is licensed
# material and is not distributed with this code, so there is no default.
# Forward slashes work on Windows too.
ppdb_export_dir <- ""
