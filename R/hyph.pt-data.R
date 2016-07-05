# Copyright 2016 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.lang.pt.
#
# koRpus.lang.pt is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus.lang.pt is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.lang.pt.  If not, see <http://www.gnu.org/licenses/>.


#' Hyphenation patterns for Portuguese
#' 
#' Hyphenation patterns for Portuguese to be used by \code{\link[koRpus:hyphen]{hyphen}}.
#' These data objects are not really intended to be used directly, but rather to be consulted
#' by the \code{hyphen()} function without further user interaction.
#'
#' @format The \code{pattern} slot of each hyphenation pattern object has three colums:
#'    \describe{
#'      \item{\code{orig}}{The original pattern in patgen style format.}
#'      \item{\code{char}}{Only the character elements of the pattern which can be matched to parts of an actual word.}
#'      \item{\code{nums}}{A code of digits defining the possibility to split syllables at respective places in this pattern.}
#'    }
#'
#' @source The patterns (as they are present in the \code{"orig"} column described above) were originally provided by the LaTeX developers[1],
#' under the terms of the LaTeX Project Public License[2]. Refer to Liang (1983) for a detailed explaination.
#' From these original patterns the values in the remaining columns were created using \code{\link[koRpus:read.hyph.pat]{read.hyph.pat}}.
#' 
#' In case any changes to the patterns were necessary to be used in this package, they are fully
#' documented in the file "ChangeLog_hyph_patterns.txt" in the sources for this package. The
#' unchanged original patterns can be found under [1].

#' @aliases hyph.pt
#' @docType data
#' @keywords datasets
#' @name hyph.pt
#' @usage hyph.pt
#' @seealso
#'    \code{\link[koRpus:read.hyph.pat]{read.hyph.pat}},
#'    \code{\link[koRpus:manage.hyph.pat]{manage.hyph.pat}}
#' @references
#' Liang, F.M. (1983). \emph{Word Hy-phen-a-tion by Com-put-er}.
#'   Dissertation, Stanford University, Dept. of Computer Science.
#'
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/}
#'
#' [2] \url{http://www.ctan.org/tex-archive/macros/latex/base/lppl.txt}
NULL
