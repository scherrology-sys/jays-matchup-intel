# Code

Reproducible R code for Jays Matchup Intel previews and retrospectives, starting April 10, 2026.

Each preview and retro runner sources `jays_matchup_helpers.R`, which contains the constants and utility functions used across all scripts. The core model logic lives in a private file and is not shared here.

## Data

Statcast data is pulled directly from Baseball Savant using `download.file()`. No data files are included in this repository. Run the pull scripts to generate the inputs before running a preview or retro runner.

## Structure

```
code/
  jays_matchup_helpers.R          # constants and utility functions
  preview_runner_2026-04-10.R     # MIN @ TOR · April 10
```

Retro runners are added after each game is scored.
