# PathLab SEM on your own computer

PathLab SEM runs on your computer and opens in your web browser, in the same way as
`bibliometrix::biblioshiny()`. Your data never leave your computer.

## Install (once)

From r-universe (recommended), in R:

```r
install.packages("pathlab", repos = c("https://bartonessel.r-universe.dev", "https://cloud.r-project.org"))
pathlab::run()
```

Or from the installer bundle:

1. **Install R** (version 4.2 or newer) from <https://cloud.r-project.org>. RStudio is optional but convenient:
   <https://posit.co/download/rstudio-desktop/>.
2. **Unzip** `PathLab_0.17.16_install.zip` (right-click, then Extract All) and keep the files together.
3. **Install PathLab SEM:** in RStudio, open `install_pathlab.R` and click **Source** (top right of the editor). In R
   without RStudio, run `source(file.choose())` and pick `install_pathlab.R`.

   This also installs the R packages PathLab uses (seminr, lavaan, jmv, caret, bibliometrix, quanteda and others).
   The first installation can take 10 to 30 minutes. On Windows and macOS the packages arrive ready-built; on Linux
   some are compiled, which needs the usual system libraries (for example `libcurl`, `libxml2`, `libgsl`).

   Without the installer, the same in two lines (do not use RStudio's "Install from package archive", which skips
   the packages PathLab needs):

   ```r
   install.packages("remotes")
   remotes::install_local("pathlab_0.17.16.tar.gz")
   ```

## Start

```r
pathlab::run()
```

The app opens in your browser. R waits while the app runs: press **Esc** (RStudio) or **Ctrl+C** (console) to stop it.
To keep using R while the app runs, start it with `pathlab::run(background = TRUE)` and stop it with
`pathlab::stop_app()`.

### In RStudio or Positron

- **RStudio:** open the **Addins** menu (toolbar) and choose **Start PathLab SEM** (web browser) or
  **Start PathLab SEM in the Viewer** (the Viewer pane). R stays free while the app runs; **Stop PathLab SEM** in the
  same menu stops it.
- **Positron** (and RStudio): type `pathlab::run()` in the Console. `pathlab::run(viewer = TRUE)` shows the app in the
  Viewer pane instead of the browser; the browser gives it more room.
- **RStudio Server and Posit Cloud** (RStudio in a web browser): `pathlab::run()` works the same way and opens the app at
  the address RStudio forwards to it.

**Save your work** with the app's Save button. A project lives in memory while the app runs; saved `.plj` files open
again later.

## If something is missing

```r
pathlab::check_setup()      # lists missing packages, Java and Quarto
pathlab::install_extras()   # optional packages: WEKA learners, audio/video, PDF documents
```

Optional extras:
- **WEKA learners** (Machine learning) need a 64-bit Java, for example Eclipse Temurin (<https://adoptium.net>),
  and then `pathlab::install_extras()`.
- **Word reports** need Quarto (<https://quarto.org/docs/get-started/>).

## Cite

```r
citation("pathlab")
```

Essel, H. B. (2026). *PathLab SEM* (Version 0.17.16) [Computer software].

## Licence

GPL-3 (GNU General Public License, version 3): you may use, share and change PathLab SEM; versions you share
must stay under the same licence. See <https://www.gnu.org/licenses/gpl-3.0.html>.
