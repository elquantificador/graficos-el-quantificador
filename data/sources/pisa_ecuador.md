# PISA Ecuador mean scores

## Data files

- Chart-ready extract: `data/raw/pisa_ecuador/pisa_ecuador_mean_scores.csv`.
- Official OECD workbook: `data/raw/pisa_ecuador/pisa_2025_tables_ib1_2a_36_38.xlsx`.
- OECD source page: <https://www.oecd.org/en/data/datasets/pisa-2025-database.html>.
- StatLink for the downloaded workbook: <https://stat.link/mrq53f>.

## Extraction

The CSV contains Ecuador's mean scores and standard errors for science,
reading, and mathematics in 2017 and 2025. Values were extracted from OECD
PISA 2025 Results Volume I tables I.B1.2a.36, I.B1.2a.37, and I.B1.2a.38.

In those tables, Ecuador's value under the PISA 2018 column is explicitly
identified as the result of the PISA for Development assessment conducted in
2017. The 2025 workbook therefore provides the smallest official source that
contains both comparable Ecuador observations for these three subjects.

## Interpretation

The chart-ready series has only two Ecuador observations per subject. It can
support a two-point line chart, but it does not establish a detailed annual
trend. The OECD country note also cautions that the apparent decline in
mathematics and reading is related in part to the expansion of secondary
education and the inclusion of more previously marginalised students.

## Microdata investigation

OECD publishes public-use microdata for both PISA-D and PISA 2025. The
student files contain questionnaire responses and student performance
estimates, so they could support subgroup or distributional analysis after
filtering to Ecuador. They are not Ecuador-only files.

- PISA 2025 student SPSS file: `CY09_MS_STU_PUF.zip`, about 947 MB compressed.
  Direct file: <https://webfs.oecd.org/pisa2022/2025/CY09_MS_STU_PUF.zip>.
- PISA 2025 school SPSS file: `CY09_MS_SCH_PUF.zip`, about 5.1 MB compressed.
  Direct file: <https://webfs.oecd.org/pisa2022/2025/CY09_MS_SCH_PUF.zip>.
- PISA-D public-use files: <https://www.oecd.org/en/data/datasets/pisa-d-pisa-for-development.html>.
  The current OECD link routes through a user-data-collection form before
  granting access to the files. The in-school student file covers Ecuador
  together with the other PISA-D countries; it is not a country-only file.

For a mean-score chart, the published OECD tables are preferable: the
microdata analysis would need the country filter, final student weights,
plausible values, and the OECD calculation code. Microdata become worthwhile
if we want Ecuador-specific gaps by sex, socioeconomic status, school type,
or score distributions. They would not add more Ecuador time points because
the available cycles are still PISA-D 2017 and PISA 2025.

## Participation history

Ecuador was not in the standard PISA cycles before 2025. The OECD Ecuador
participant page lists 2025 as the year of participation, and the OECD
participation-summary workbook records Ecuador only in the 2025 column. The
separate PISA for Development school-based assessment tested Ecuador in 2017.
The PISA 2025 trend tables place that PISA-D result under the PISA 2018
column, with a footnote identifying it as a 2017 assessment. It should be
labelled PISA-D 2017 in our chart, not PISA 2018.

The current HTML participant list appears to show additional years for
Ecuador, but that conflicts with both the downloadable summary workbook and
Ecuador's country page. I have treated the workbook and country page as the
authoritative participation records.

## Retrieval date

Downloaded on 2026-09-08 from the OECD StatLink above. The workbook states
that it was last updated on 2026-09-08.
