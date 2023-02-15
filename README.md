# Overview

This is a template project folder for Core Clinical Sciences. **It is expected any additional specifications will build upon this folder structure.**

## Folders organization

While different projects will require its own specific organizations, it is important to establish a common template. The minimum organization of folder structures will include the following:


```
Folder Template
  |__ Background-Materials
      |__Client-Materials
      |__Key-References 
  |__ data
      |__raw-data
      |__processed-data
  |__ outputs
      |__figures
      |__listings
      |__tables
  |__ reference-manager
      |__references.bib
  |__ scripts
      |__functions
      |__temp
  |__ technical-reports
      |__clinical-study-reports
      |__manuscripts
  |__ templates

```

Each folders will contain the following: 

-   `Background-materials`: A folder to store scientific and other reference documents.

    -   `Client-Materials`: store key materials shared by clients here

    -   `Key-References`: store other key materials here

-   `Data`: A folder to store your data.

    -   `Raw-Data`: store the raw datasets here

    -   `Processed-Data`: store the processed datasets here

-   `Outputs`: This is where you save all the tables, listings, figures to be used in presentations/reports.

    -   `Figures`
    -   `Listings`
    -   `Tables`

-   `Presentations`: A folder for slide decks

-   `Reference-Manager`: store a reference manager (e.g., Endnote here) to output bib files

    -   `references.bib`: A bib file to be used in reports/presentations

-   `R`: This is where you keep your R (or other language) scripts

    -   `Functions`: store your functions here
    -   `Temp`: (Not necessary) save temporary outputs or scripts generated for exploratory or testing purposes

-   `Technical-Reports`: This is where you work on deliverables (e.g., clinical study reports, manuscripts, and other technical reports)

    -   `Clinical-Study-Reports`
    -   `Manuscripts`

-   `Templates`: Contains template documents for docx, tex, css, csl, and scss

    -   `doc` template: Use `Word-Template-Times.docx` to style MS Word documents
    -   `tex` files: Use to style Latex/PDF documents
    -   `CSS` files: Use to style HTML documents
    -   `CSL` files:
        -   Use to define formatting of citations and bibliographies

        -   Short for Citation Style Language (CSL)
    -   `SCSS` files: Use for HTML documents as [Quarto themes](https://quarto.org/docs/output-formats/html-themes-more.html)

## Resources

-   [R Project Folder template accessible for CCS members](https://github.com/CoreClinicalSciences/00-Project-Folder-Template)
