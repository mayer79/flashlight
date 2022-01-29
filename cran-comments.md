This is a maintenance release, see NEWS.md

## R CMD check results seem okay

checking for unstated dependencies in examples ... OK
   WARNING
  'qpdf' is needed for checks on size reduction of PDFs

## Online check results seem okay as well

Using

- check_win_devel()
- check_rhub(env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))


checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Michael Mayer <mayermichael79@gmail.com>'
  
    DOI: 10.1214/aos/1013203451
      From: DESCRIPTION
      Status: Internal Server Error
  Found the following (possibly) invalid DOIs:
      Message: 500

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
