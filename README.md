# jays-matchup-intel

Daily Statcast matchup intelligence for the Toronto Blue Jays. Bayesian pitcher 
previews, pitch arsenal breakdowns, and model retrospectives. Built in R, 
branded for [@scherrology](https://x.com/scherrology).

## What This Is

Every Blue Jays starter gets a full analytical breakdown before first pitch:

- **Pitch arsenal** — usage mix, velocity, movement profile, whiff and CSW rates
- **Handedness splits** — how the pitch mix changes vs LHH and RHH
- **Head-to-head history** — Blue Jays hitters vs this pitcher, outcome by outcome
- **Bayesian wOBA model** — Beta-Binomial posteriors with 90% credible intervals, 
  prior anchored to each hitter's full season wOBA, updated with pitcher-specific 
  plate appearance history
- **Tonight's call** — top targets, pitch to hunt, and the social narrative

## Structure
```
games/YYYY-MM-DD-pitcher/   each start gets its own folder
retrospective/              model validation, did the posteriors hold up
assets/                     shared theme and brand components
```

## Tech Stack

- **R** — tidyverse, ggplot2, base Beta-Binomial inference via qbeta()
- **Data** — Baseball Savant Statcast exports
- **Viz** — vanilla HTML + Canvas API, no frameworks
- **Hosting** — GitHub Pages

## Live Site

[scherrology-sys.github.io/jays-matchup-intel](https://scherrology-sys.github.io/jays-matchup-intel)

## Arm Chair Analyst
Built by **Arm Chair Analyst** · [@scherrology](https://x.com/scherrology)
