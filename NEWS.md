# fairpub 0.0.1.9000

## Major changes

- Rename package (`faircitations` becomes `fairpub`)
- Prefix functions with `fp_*()`
- Rename function `get_dafnee_journals()` in `fp_list_dafnee_journals()`
- Rename function `citation_ratio()` in `fp_compute_ratio()`

## New features

- `fb_doi_from_bibtex()`: imports a BibTeX and returns DOI
- `fb_clean_doi()`: cleans DOI
- `fp_check_mailto()`: checks that user sends its email to OpenAlex
- `fp_article_fairness()`: gets the fairness status of an article
- `fp_journal_fairness()`: gets the fairness status of a journal

## Fix bugs

- Remove duplicated journals in internal dataset

## Documentation

- Add a Get started vignette

## Unit tests

- Setup `testthat`
- Setup code coverage GHA

# fairpub 0.0.1

First stable release.
