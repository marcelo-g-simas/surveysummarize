# surveysummarize Install Instructions

### For the steps below, use the latest Direct Download link or browse for a more recent version.

### Accept the program installation defaults as you are prompted.

1) R
    - Windows
	    - Latest Download: https://cran.r-project.org/bin/windows/
        - Direct Download: https://cran.r-project.org/bin/windows/base/R-4.0.3-win.exe
    - Mac
        - Latest Download: https://cran.r-project.org/bin/macosx/
        - Direct Download: https://cran.r-project.org/bin/macosx/R-4.0.3.pkg

2) Tools
    - Windows:
        - Latest Download: https://cran.r-project.org/bin/windows/Rtools/
        - Direct Download: https://cran.r-project.org/bin/windows/Rtools/rtools40-x86_64.exe
    - Mac:
		- Latest Download: https://cran.r-project.org/bin/macosx/tools/
        - Direct Download: https://cran.r-project.org/bin/macosx/tools/clang-8.0.0.pkg

3) RStudio
    - Windows
        - Latest Download: https://www.rstudio.com/products/rstudio/download/#download
        - Direct Download: https://download1.rstudio.org/desktop/windows/RStudio-1.4.1103.exe
    - Mac
        - Latest Download: https://www.rstudio.com/products/rstudio/download/#download
        - Direct Download: https://download1.rstudio.org/desktop/macos/RStudio-1.4.1103.dmg

### Now in RStudio, install surveysummarize from the console.
```r
install.packages('devtools')
devtools::install_github('Westat-Transportation/surveysummarize')
```
