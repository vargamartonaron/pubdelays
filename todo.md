- [x] can't supply limit when transforming
  - `transform`, `transform-shards`, and `list-inputs` accept `--limit`.
- [x] md.5 download flags error, probably a false positive
  - download verification now checks only requested current `.md5` sidecars.
- [x] delete is_psych
  - removed from canonical schema, transform output, analysis outputs, docs, and tests.
- [x] add process_data_local peer review join and calculation to articles
  - raw peer-review preprocessing and article-level delay calculations are ported.
- [x] strip AI warnings about regressions & legacy codebase comparison from the docs
  - removed the regression warning block and colleague-worktree comparison note.
- [x] when filtering, we lose information about where the filtering happens,
  for example whether we exclude articles from Q4 rather than Q1
  see: process_data_local_temp.R
  - transform sidecars include stage drop counts; validation now writes excluded-row breakdowns by reason, quartile, discipline, NPI level, and open-access status.
- [x] save out data that got excluded so we can compare missing vs. non-missing
  - `validate-analysis` writes configured excluded rows to `validation.excluded_output` / `--excluded-output`.
- [x] plot and analyze the data that got excluded, to be informed about the nature
  of the data that has missing fields
  `naniar` package
  - validation writes missingness tables by year/quartile/discipline/NPI/open-access and pairwise co-missingness for reproducible analysis.
- [x] write free text methods section about data handling
  - added a methods note to `docs/internals/validation.md` describing kept/excluded rows and missingness outputs.
