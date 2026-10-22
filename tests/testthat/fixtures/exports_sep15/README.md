Frozen exports from the September 15, 2026 IRCC release, used by
`tests/testthat/test-exports.R` to check that the Downloads note rebuilt from
`data/baseline.rds` matches these expected values:

- `note_body.md`: note body paragraphs (before the dated context line on the byline)
- `body_numbers.csv`: the 64 rounded body numbers and the facts behind them (Appendix D)
- `facts.csv`: fact ids, values and display strings from `exports/R/note_q2_facts.R`
- `geography_table.csv`, `mix_2025.csv`: Tables C1 and C2
