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

### The quickest way, in one line

```r
install.packages("pathlab", repos = c("https://bartonessel.r-universe.dev", "https://cloud.r-project.org"))
```

This takes a ready-built package, so nothing is compiled, and `update.packages()` keeps it current afterwards.

**On R 4.4 or older, add `type = "source"`:**

```r
install.packages("pathlab", repos = c("https://bartonessel.r-universe.dev", "https://cloud.r-project.org"), type = "source")
```

r-universe builds for the current R versions only — today R 4.5, 4.6 and 4.7 on Windows and macOS. Without
`type = "source"` an older R finds nothing there and leaves the version you already have in place, which looks like
the command doing nothing at all. PathLab has no compiled code of its own, so building it from source needs no build
tools, and the packages it uses still come ready-built from CRAN. Check which version you ended up with:

```r
packageVersion("pathlab")
```

### From the installer bundle

This route works on any R from 4.2, and needs no repository.

1. **Install R** (version 4.2 or newer) from <https://cloud.r-project.org>. RStudio is optional but convenient:
   <https://posit.co/download/rstudio-desktop/>.
2. **Unzip** `PathLab_1.3.11_install.zip` (right-click, then Extract All) and keep the files together.
3. **Install PathLab SEM:** in RStudio, open `install_pathlab.R` and click **Source** (top right of the editor). In R
   without RStudio, run `source(file.choose())` and pick `install_pathlab.R`.

   This also installs the R packages PathLab uses (seminr, lavaan, jmv, caret, quanteda and others).
   The first installation can take 10 to 30 minutes. On Windows and macOS the packages arrive ready-built.

   **On Linux** the packages are compiled from source, so install the system libraries they need first. On Fedora,
   RHEL and openSUSE:

   ```
   sudo dnf install libcurl-devel openssl-devel libxml2-devel zlib-devel gsl-devel libsodium-devel freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel fontconfig-devel cairo-devel harfbuzz-devel fribidi-devel cmake v8-devel
   ```

   On Debian and Ubuntu:

   ```
   sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev zlib1g-dev libgsl-dev libsodium-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libfontconfig1-dev libcairo2-dev libharfbuzz-dev libfribidi-dev cmake libv8-dev
   ```

   If a package still fails to build, its own error names the library it wants; install that one and run the
   installer again, which skips everything already installed. A slow connection may also time out on a large
   download: `options(timeout = 600)` before the installer gives each one ten minutes.

   Without the installer, the same in two lines (do not use RStudio's "Install from package archive", which skips
   the packages PathLab needs):

   ```r
   install.packages("remotes")
   remotes::install_local("pathlab_1.3.11.tar.gz")
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
pathlab::install_extras()   # optional packages: Bibliometrics, GSCA, WEKA learners, audio/video, PDF, Bayesian CB-SEM
```

Everything else works without these. Each feature names the package it needs when you open it, and
`install_extras()` installs them.

Optional extras:
- **Bibliometric analysis** needs `bibliometrix` (and `bibliometrixData` for the sample collection). On Linux it
  builds `pdftools`, which needs Poppler: `sudo dnf install poppler-cpp-devel` (Fedora) or
  `sudo apt install libpoppler-cpp-dev` (Debian and Ubuntu).
- **GSCA analyses** need `cSEM`. On Linux it builds `symmoments`, which needs GMP: `sudo dnf install gmp-devel` or
  `sudo apt install libgmp-dev`.
- **WEKA learners** (Machine learning) need a 64-bit Java, for example Eclipse Temurin (<https://adoptium.net>),
  and then `pathlab::install_extras()`.
- **Word reports** need Quarto (<https://quarto.org/docs/get-started/>).

## Cite

```r
citation("pathlab")
```

Essel, H. B. (2026). *PathLab SEM* (Version 1.3.11) [Computer software].

## Licence

GPL-3 (GNU General Public License, version 3): you may use, share and change PathLab SEM; versions you share
must stay under the same licence. See <https://www.gnu.org/licenses/gpl-3.0.html>.
