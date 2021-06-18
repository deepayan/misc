\name{does_not_matter_either}
\alias{fun2}
\alias{fun3}
\alias{fun4}
\title{A duplicate alias}
\description{
  This file documents \code{fun2}, which is already documented in \link{fun1}.
}
\usage{
fun2(x, ...)
fun3(x, ...)
}
\arguments{
  \item{x}{An object.}
  \item{\dots}{Extra arguments.}
}
\details{
  The existence of this help page should trigger a WARNING during
  \code{R CMD check}.

  Other than that, this file checks for possible problems in how
  within-package links are handled, as well as potential problems with
  special characters and case.

  The usual forms of a within-package link to a topic \code{foo} are

  \itemize{

    \item \code{\\link{foo}} 

    \item \code{\\link[=foo]{link-text}} 

  }

  These are meant to behave like \code{help(foo)}, presumably with
  preference to the current package, although this is maybe not clearly
  documented.  Links to a file by name are not supported. The following
  are examples of the first form:

  \itemize{

    \item \code{\\link{fun1}}: \code{\link{fun1}} is a topic in both
    \code{fun2.Rd} and \code{funs.Rd}

    \item \code{\\link{fun2}}: \code{\link{fun2}} is a topic only in \code{funs.Rd}

    \item \code{\\link{fun3}}: \code{\link{fun3}} is a topic only in
    \code{fun2.Rd} (this file)

    \item \code{\\link{funs}}: \code{\link{funs}} is a not a topic, but
    a file, so the link should be invalid. It may go to the same topic
    in another package (e.g., \pkg{dplyr}), if available, even if that
    package is not loaded.

    \item \code{\\link{fit}}: \code{\link{fit}} should go to either
    \code{generics::fit} or \code{parsnip::fit}; if both packages are
    loaded, it should give a choice.

    \item \code{\\link{foobar}}: \code{\link{foobar}} really shouldn't
    go anywhere.

  }

  For the last two examples, the resolution to a topic in another
  package is attempted at package installation time for static HTML, so
  this is one situation where the static behaviour cannot easily be
  changed to match the dynamic behaviour.

  Corresponding examples of the second form are:

  \itemize{

    \item \code{\\link[=fun1]{link-text}}: \code{\link[=fun1]{link-text}}

    \item \code{\\link[=fun2]{link-text}}: \code{\link[=fun2]{link-text}}

    \item \code{\\link[=fun3]{link-text}}: \code{\link[=fun3]{link-text}}

    \item \code{\\link[=funs]{link-text}}: \code{\link[=funs]{link-text}}

    \item \code{\\link[=fit]{link-text}}: \code{\link[=fit]{link-text}}

  }

  Such ambiguous links are documented to be OK, in fact recommended, for
  base and recommended packages (see WRE):

  \itemize{

    \item \code{\\link{print.default}}: \code{\link{print.default}} (base)

    \item \code{\\link{plot.default}}: \code{\link{plot.default}} (graphics)

    \item \code{\\link{plot}}: \code{\link{plot}} (both base
    and graphics; interactive help gives a choice)

    \item \code{\\link{mle}}: \code{\link{mle}} (stats4)

    \item \code{\\link{panel.qqmath}}: \code{\link{panel.qqmath}} (lattice)

  }

}

\section{Special characters}{

  Math operator aliases in particular could lead to weird
  filenames. Here are some examples:

  \itemize{
    
    \item \code{\link[base]{Arithmetic}} (file name, but also alias)
    \item \code{\link[base]{+}} 
    \item \code{\link[base]{-}} 
    \item \code{\link[base]{*}} 
    \item \code{\link[base]{**}} 
    \item \code{\link[base]{/}} 
    \item \code{\link[base]{^}} 
    \item \code{\link[base]{\%\%}} 
    \item \code{\link[base]{\%/\%}} 
    \item \code{\link[magrittr]{\%>\%}} 
    \item \code{\link[base]{\%*\%}} 
    \item \code{\link[base]{\%in\%}} 

  }

  For base and recommended packages, it is safer to \emph{not} specify the packagename, as R will explicitly translate the topic to the appropriate filename when creating the links.

  \itemize{

    \item \code{\link{Arithmetic}} (file name, but also alias)
    \item \code{\link{+}}
    \item \code{\link{-}}
    \item \code{\link{*}}
    \item \code{\link{**}}
    \item \code{\link{/}}
    \item \code{\link{^}}
    \item \code{\link{\%\%}}
    \item \code{\link{\%/\%}}
    \item \code{\link{\%*\%}}
    \item \code{\link{\%in\%}}

  }


}

\section{Case mismatch}{

  Filenames on Windows / Mac (?) are not case-sensitive; this could lead
  to problems particularly with aliases that differ by case.

  These links to grid have the wrong case: \code{\link[grid]{rectgrob}};
  \code{\link[grid]{GRID.RECT}}.

  The following pairs of aliases are both valid, but are in different files.

  Explicit package links: \code{\link[topichelp]{FUN4}}; \code{\link[topichelp]{fun4}}.

  Implicit package links: \code{\link{FUN4}}; \code{\link{fun4}}.

}

\keyword{documentation}

