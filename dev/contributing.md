---
title: "Contributing TO R AQSAPI"
author: "Clinton Mccrowey"
date: February 25, 2021
output:
  rmarkdown::github_document:
bibliography: ../vignettes/AQSAPI.bib
lang: en-US
csl: ../vignettes/acs-nano.csl
---



| <span style="color:red"> This project is very early in it's development stage. Please watch this document for important changes in the future. </span> |
| -- |

# Introduction to Contributing to RAQSAPI
 The goal of this document is to help make contributing code as easy as
 possible. As with any collaborative project, consistency is key and
 communication is of the utmost importance. The document serves as the
 working Standard Operating Procedure that anyone desiring
 to contribute code to this project should follow to avoid your submission
 from being rejected.
 
 As of now Only members of the US Environmental Protection Agency's (EPA) AQS
 Data Analytics workgroup has write permissions to the repository. Anyone
 outside of this workgroup will not be granted write permissions to the RAQSAPI
 repository, thus they will not be able to contribute code. We are working
 toward allowing outside code in the future. If you are a current EPA employee
 who would like to be added to the AQS Data Analytics workgroup please contact
 one of the repository's administrators.
 
 Since only members of the EPA's Data Analytics workgroup have write permissions
 to the RAQSAPI repository anyone outside of this workgroup wanting to request
 a change to the project or wanting to submit code should do so through the
 official issue tracker at
 . We plan on 
 accepting outside submissions in the future.
 
# Getting started
 If this is your first time contributing code to the RAQSAPI project it is
 highly recommended that you first familiarize yourself with how R packages work
 and to create and maintain them. Hadley Wickham has a free online book entitled
 *R packages* (@book:RPackages)(@website:RPackages),
 print copies are published by O'Reily Media Inc.. The online version is freely
 available on the internet. The current edition as of the writing of this
 document was published in 2015 for the hard copy, online version 2017.
 
 For a more detailed reference see [R Manuals - Writing R extensions](
 https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
 (@WritingRexensions). It is not necessary to read the entire *Writing R
 Extensions* Manual but it is recommended that you keep this reference handy for
 more complicated issues, especially dealing with roxygen2 and DESCRIPTION file
 metadata issues.
 
# Making changes to RAQSAPI
## submit a bug report/feature request to issue tracker
 Before starting, if necessary submit a issue on the issue tracker to
 communicate to the rest of the team what changes you intend
 on making. This also allows us to document and track what changes are being
 made and when they happened. This is especially important if the changes will
 affect the RAQSAPI functional interface. 
 
## Keep your local repository in sync with the remote repository
 Pull the latest version of RAQSAPI code from the RAQSAPI repository before
 you begin making changes, keep your local repository in sync with the RAQSAPI
 remote repository to prevent merge conflicts later on. Do this at least once
 per day and more often if others have pushed code to the RAQSAPI remote
 repository.
 
## Code style
This project follows the style guide as documented in the
[Tidyverse style guide](https://style.tidyverse.org/)(@Tidyversestyleguide)
with a few notable changes. Please adhere strictly to these rules.

1. Section 2.4.1 I prefer that curly braces start on their own line and end on
     their own line with the beginning and ending braces aligned horizontally.
     This is makes it a lot easier to pinpoint the beginning and ending of large
     code blocks than just space alignment alone. Especially when working on a
     small laptop screen.
2. Section 2.8.2 In addition to typing out the entire word "TRUE" and "FALSE"
     for logical variables RAQSAPI includes code to check for the usage of "T"
     or "F" and will throw an error if either is used. 
3. Section 3.2 as a former C/C++ programmer I prefer explicit return calls over
     implicit returns especially for large or complex functions.
     Although this may slow down the execution of code, the slowdown penalty is
     very minor especially on modern day computers and the
     implicit return call makes it obvious to others reading your code of what
     you intend on doing. For short functions that are a few simple lines long
     where it it obvious what should be returned you may omit the return
     statement.
4. Chapter 4 Use the magrittr pipe and the modifier in place pipe
     ('%>%' and '%<>%' respectively) were appropriate to minimize the
     introduction of unnecessary intermediate or temporary objects. 
     a. Section 4.6
       i. As opposed to Wickham's suggestion, Please feel free to use the
          modifier in place pipe ('%<>%') from magrittr. It is slick, makes code
          cleaner, it avoids having to create temporary variables and avoid
          using the right side assignment operator.
       ii. To keep my sanity please do not use the right side assignment
           operator ('->'). it makes the code much more unreadable. This is the
           most ugly, unholy and evil operator in all of R. Use the modifier in
           place operator instead!!!!!!!!!!!1111111
5. Although Wickham states in certain places to avoid being implicit in specific
     situations(i.e. section 2.4.3 - avoiding implicit type coercion) I am
     going to take this one step further and suggest that you should avoid
     being implicit everywhere where it is feasible to do so and to be explicit
     where possible. Other people might have to review your code and it is not
     fun trying to figure out what another programmer is trying to do.
     There are certain instances where it is ok to be implicit what your code is
     doing, especially if doing so gives a meaningful increase to processing
     efficiency or produces a tangible improvement to code layout or
     readability. But please if you must include code where the result is not
     obvious include comments so that others can quickly and easily understand
     what your code is attempting to do. This includes using named variables
     in function calls, and in declaring vectors, list dataframe, tables,
     and matrices and using the col_types variables during import of external
     data; do not allow imported functions to guess the data type. Also, as
     stated earlier, please do explicitly use the return command to make it
     plainly obvious to others reviewing your code what object that your
     function intends on returning if it is not plainly obvious what your
     function is doing.
     
The maintainers do realize that there are a some violations of these rules in
    the current code base, we are working to address these issues. Especially
    regarding spacing, currently I have this set to four spaces, in the future
    this will be fixed to go down to the recommended two spaces. This project
    was started before the team agreed to using the tidyverse style guidelines
    so legacy spacing style issues still lurk in the code.
 
 
 If your code addresses an issue that has been issued a
 issue key include that issue key in the commit message so that the gitHub issue
 tracker can automatically track the status of that issue.
 
## Workflow
Before editing RAQSAPI it is suggested that you update all of your installed R
packages each day that you intend on writing or testing code for this project,
this is not a required step but can make the process easier for you
To develop RAQSAPI code. If you don't the R environment will ask you to update
dependencies of RAQSAPI during the build process. The maintainers will attempt
to ensure that the code in this library works with the latest versions of all
of it's dependencies (within reason) so it is important that any one
contributing code to this project use the latest version of it's dependencies to
test any new code. Dependencies are updated frequently so it is suggested that
contributors check for package updates frequently to ensure compatibility.

first open the RAQSAPI.proj that is located in the
root RAQSAPI folder, this will cause your R environment to set the working
directory to the path where the code is located. Next call the function

> devtools::load_all()

this will attach all the necessary RAQSAPI dependencies listed in the DESCRIPTION
file and collate the attachment of files needed to run RAQSAPI. You can then
run and edit the source files as needed.

## Build and run automated test on your code
CRAN has strict policies (@CRANRepoPolicy) about accepting code that produces
warnings, significant notes or build errors. In addition they implement rigid
guidelines for code quality, legal  requirements and build quality. In order to
comply with CRAN's code submission policies please source the install_RAQSAPI.R
file located in the dev subfolder of the package root directory and run these
functions after writing any new code in an environment with the project file
open:

> RAQSAPICLEAN()  
RAQSAPIBUILD()  
RAQSAPICHECK()  

This will run automated checks to ensure that any modifications to the package
does not introduce new issues. Again, CRAN will not accept code with build
issues or significant warnings. These simple checks will help catch these issues
before we submit changes to CRAN. Some of these test are repeated but shouldn't
cause too much trouble. Due to CRAN's submission guidelines this project will
not accept code that produces new warnings or errors so please check your code
before pushing changes to the repository.   

# How can I contribute
Even if you are not an experienced R programmer there are plenty of
opportunities for you to contribute. Most of the people involved in this project
contribute to it outside of their primary EPA duties so resources are limited so
any assistance with this project is very much appreciated. Some areas where
help is needed include:

* Documentation: This is a major time-consuming aspect of any programming
project. The README, RAQSAPIvignettes, roxygen2 notes, the reference manual and
changelogs are important tools used to communicate to end users how to setup,
use and contribute to the RAQSAPI project. Since the aim of the project is to
create a easy to use library for users who may not be experts in using the R
programming language, programming in general, dealing with air pollution
monitoring data or statistical software is it imperative that the project's
documentation is relatively easy to understand. Therefore it is imperative that
the project's documentation is kept up-to-date, and free from spelling or
grammatical errors. If you spot issues or areas in the documentation that could
be improved please submit a push request or a issue report at
https://github.com/USEPA/RAQSAPI/issues.
* Suggest improvements: If you have a suggestion on improving/refactoring the
code to make it easier for the end user, improving code quality or adding new
features we.
* Testing: an important aspect for producing a quality software product requires
having robust automated unit tests and people willing to QA the software.
* coding: If you do have experience programming, have experience using R or want
to learn feel free to join us.

If you would like to contribute in any way please contact the project's
maintainers.


# References
