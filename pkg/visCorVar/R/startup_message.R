.onAttach <- function(libname, pkgname)
{ 
  packageStartupMessage("\nLoaded visCorVar ",as.character(utils::packageDescription("visCorVar")[["Version"]]),
                                                               
                                                               "\n vignette : https://gitlab.com/bilille/viscorvar/-/blob/master/vignettes/visCorVar.pdf "

  )}

# .onAttach <- function(libname, pkgname){ packageStartupMessage("\nLoaded mixOmics ",as.character(packageDescription("mixOmics")[["Version"]]),
#                                                                
#                                                                "\n\nThank you for using mixOmics!",
#                                                                "\n\nHow to apply our methods: http://www.mixOmics.org for some examples.",
#                                                                "\nQuestions or comments: email us at mixomics[at]math.univ-toulouse.fr  ",
#                                                                "\nAny bugs? https://bitbucket.org/klecao/package-mixomics/issues",
#                                                                "\nCite us:  citation('mixOmics')"
#                                                                
# )}