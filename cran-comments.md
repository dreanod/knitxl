## Resubmission

This is a resubmission. In this version I addressed the following issues:

1. The Description field is intended to be a (one paragraph) description of
what the package does and why it may be useful. Please add more details
about the package functionality and implemented methods in your
Description text.
  - The description field was expanded as instructed.

2. If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
  - There is no relevant reference applicable. 

3. You write information messages to the console that cannot be easily
suppressed. -> R/testing.R
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print()/cat() rather use message()/warning() or
if(verbose)cat(..) (or maybe stop()) if you really have to write text to
the console. (except for print, summary, interactive functions)
  - R/testing.R was modified to call testthat::expect() instead of print() for 
  writing information messages.
  
## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
  
