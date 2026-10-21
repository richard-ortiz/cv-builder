# CV Builder

A step-by-step R Shiny app that helps students build an academic CV and export it as a Word or PDF file.

## Running it

1. Install R and RStudio (both free).
2. Put `app.R` in its own folder.
3. Open `app.R` in RStudio and click **Run App** (top right of the editor).

The first run installs the packages it needs automatically (shiny, bslib, officer, flextable, jsonlite).

## How students use it

1. **Start:** Enter your name and contact details, choose your career stage (undergraduate, graduate, or postdoc/faculty), and pick a font and heading color. The stage turns on sensible sections for you; you can switch any section on or off.
2. **Add your experience:** Work through one section at a time. Each section has a tip on what to include, and the live preview updates as you type.
3. **Check:** The app flags mistakes:
   - End dates before start dates
   - Two entries with identical dates
   - Duplicate DOIs
   - Missing fields
   - Your name missing from an author list
   - Common typos
   - Article titles that aren't in APA sentence case

   Click **Fix** to jump straight to the entry.
4. **Preview & download:** Download a Word (.docx) or PDF file, and click **Save my progress** to get a `.json` file you can load next semester to update your CV.

**The progress file is how students update their CV later.** Word and PDF files can't be loaded back into the app, so the app reminds students to save the progress file in three places: a highlighted box on the download page, a pop-up after every Word or PDF download, and an "Upload progress file" box at the top of the Start tab for when they come back.

Work also autosaves in the browser. If you close the tab, you'll be offered your CV back the next time you open the app.

## Features

- **Layouts:** On the Start tab, choose **Standard (one column)** or **Compact (two columns)**. In the two-column layout the name and contact details run across the top, and sections flow down the left column and then the right. Both layouts use 0.5" margins all around; change `PAGE_MARGIN` at the top of `app.R` to use a different margin. One column is recommended for applications, because upload systems can scramble two-column files.
- **"(CONT.)" headings:** When a section runs past the bottom of a column or page, it continues with a heading like "PUBLICATIONS (CONT.)". This is always on. To turn it off, set `CONT_HEADINGS <- FALSE` at the top of `app.R`. The preview works out where the pages break, and the Word file puts its breaks in the same places. If a student edits the Word file afterwards and the text shifts, those headings may need moving by hand.
- **Certifications and Training:** A built-in section for CITI courses, IACUC and animal-handling training, lab safety, CPR, and similar. It's on by default for undergraduates and graduate students.
- **Your own sections:** On the Add tab, **Add your own section** creates a section with any heading, such as Leadership, Languages, or Invited Talks. Students choose between dated entries (like Research Experience) and a simple one-line-per-item list. They can add as many as they need, and rename or remove them later. These sections appear just before References, and they're saved in the progress file.
- **Section order:** On the Add tab, students drag a section by its grip handle (⋮⋮) to move it up or down. A colored line shows where it will land, and the CV follows that order. The order is saved in the progress file, and **Reset to the standard order** undoes it. A newly added section goes just above References.
- **Sorting:** Entries are sorted most recent first. "Present" entries go to the top, and manuscripts in preparation are listed before published papers.
- **Citations:** Publications and presentations are formatted in APA 7th edition, and the student's own name is bolded automatically.
- **DOI lookup:** Paste a DOI and click **Fill in from DOI** to get the full citation from Crossref. This needs an internet connection.
- **Picking dates:** Dates are chosen from dropdown menus, so an impossible date like June 31 can't be entered.
- **Example CVs:** **See a filled-in example** loads an example that matches the stage picked on the Start tab: an undergraduate, a graduate student (M.A. with publications, grants, and teaching), or a faculty member (two-column layout with grants, mentoring, and a custom "Invited Talks" section). All the people are fictional, and PDFs of the three examples are included in this folder.

## How the PDF is made

The **Download PDF** button tries, in order:

1. Chrome, Edge, or Brave in headless mode, which produces the best-looking PDF.
2. LibreOffice, converting the Word file.
3. If neither is installed, the browser's print window opens instead. Choose **Save as PDF** as the printer.

Most students already have Chrome or Edge, so option 1 usually works. If your browser is installed somewhere unusual, set the `CV_CHROME` environment variable to its full path.

## Putting it online (no installing for students)

You can host the app for free on [shinyapps.io](https://www.shinyapps.io):

```r
install.packages("rsconnect")
rsconnect::deployApp("path/to/folder")
```

Students then just open a link. On the hosted version, the PDF button uses the browser's **Save as PDF** option, because the server has no Chrome. Word export works the same as on a computer.

## Customizing

Everything is in `app.R`:

- **Section list:** The `SECTIONS` list near the top defines every section: its fields, its tip text, and which career stages turn it on by default. To add or change a section, edit an entry there; the forms, preview, Word file, and checks all update from it.
- **Fonts and colors:** `FONTS` and `ACCENTS` hold the choices.
- **Typo list:** `TYPOS` is the list of misspellings the checker looks for.
- **Example CVs:** `sample_ug()`, `sample_grad()`, and `sample_fac()` hold the three examples.

## License and credit

CV Builder is free to use, share, and adapt at no charge. It may not be sold, or included in anything that is sold, without written permission from Richard J. Ortiz (rortiz@niu.edu). See `LICENSE` for the full terms.
It was made by the Ortiz Neuroscience Lab, Department of Psychology, Northern Illinois University (2026).
The footer text is in `app.R` under `footer =`, and the version and year are set at the top (`APP_VERSION`, `APP_YEAR`).
