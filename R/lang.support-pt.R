# Copyright 2016-2018 Meik Michalke <meik.michalke@hhu.de>
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


# this script is providing additional support for language "pt".
# adjustments kindly provided by Jan Vanhove and Katharina Karges

#' Language support for Portuguese
#' 
#' This function adds support for Portuguese to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' being loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "pt" to be used with koRpus
#'  \item \code{treetag}: The additional preset "pt", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.pt:hyph.support.pt]{sylly.pt}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Portuguese-Tagset.html}
#' @export
#' @importFrom koRpus set.lang.support
#' @examples
#' lang.support.pt()

lang.support.pt <- function(...) {

  # here you have to adjust the parameters according to the contents of the TreeTagger
  # scripts for your language (see ?set.lang.support for details)
  #  - there's only UTF-8 scripts
  #  - add both the unix and windows equivalents
  #  - if some setting is missing, just set it to an empty vector (c())
  koRpus::set.lang.support(target="treetag",
    value=list(
      "pt"=list(
        ## preset: "pt"
        # tags UTF-8 encoded text files
        lang      = "pt",
        encoding  = "UTF-8",
        preset    = function(TT.cmd, TT.bin, TT.lib, unix.OS){
          # note: these objects are set here for convenience, the
          # actual important part is the return value below
          TT.abbrev     <- file.path(TT.lib, "portuguese-abbreviations-utf8")
          TT.separator  <- file.path(TT.bin, "separate-punctuation")
          TT.posttagger <- file.path(TT.cmd, "portuguese-post-tagging")
          TT.filter     <- paste("-token -lemma -sgml |", TT.posttagger, "-no")
          # generally, the parts below are combined in this order:
          # TT.tokenizer TT.tknz.opts "|" TT.lookup.command TT.tagger TT.opts TT.params TT.filter.command
          if(isTRUE(unix.OS)){
            # preset for unix systems
            return(
              list(
                # you should change these according to the TreeTagger script
                TT.splitter         = file.path(TT.cmd, "portuguese-splitter.perl"),
                TT.splitter.opts    = paste("| sed \"s/\\([\\)\\\"\\'\\?\\!]\\)\\([\\.\\,\\;\\:]\\)/ \\1 \\2/g\" |"),
                TT.tokenizer        = file.path(TT.bin, "separate-punctuation"),
                TT.tagger           = file.path(TT.bin, "tree-tagger"),
                TT.abbrev           = c(),
                TT.params           = file.path(TT.lib, "portuguese-utf8.par"),

                TT.tknz.opts        = paste("+1 +s +l", TT.abbrev),
                TT.filter.command   = TT.filter,
                TT.pre.tagger       = "grep -v '^$' |"
              )
            )
          } else {
            # preset for windows systems
            return(
              list(
                TT.splitter         = file.path(TT.cmd, "portuguese-splitter.perl"),
                TT.splitter.opts    = paste("| sed \"s/\\([\\)\\\"\\'\\?\\!]\\)\\([\\.\\,\\;\\:]\\)/ \\1 \\2/g\" |"),
                TT.tokenizer        = file.path(TT.bin, "separate-punctuation"),
                TT.tagger           = file.path(TT.bin, "tree-tagger.exe"),
                TT.abbrev           = c(),
                TT.params           = file.path(TT.lib, "portuguese-utf8.par"),

                TT.tknz.opts        = paste("+1 +s +l", TT.abbrev),
                TT.filter.command   = TT.filter,
                TT.pre.tagger       = "grep -v '^$' |"
              )
            )
          }
        }
      )
    ),
    ...
  )

  # finally, add the POS tagset information (see ?set.lang.support for details)
  # the list is split into three parts, to be able to distinct between
  # words (including numbers etc.), normal punctuation and sentence ending punctuation.
  # this is mainly used for filtering purposes and statistics.
  # 
  # note that each tag must be defined by three values:
  #   - the original TreeTagger abbreviation
  #   - a global "word class" definition like "noun", "verb" etc.
  #   - a human readable explaination of the abbreviation
  koRpus::set.lang.support("kRp.POS.tags",
    ## tag and class definitions
    # por -- portuguese
    # see http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Portuguese-Tagset.html
    list(
      "pt"=list(
        tag.class.def.words=matrix(c(
          "AQ0","adjective","adjetivo qualificativo",
          "AQA","adjective","adjetivo qualificativo aumentativo",
          "AQC","adjective","adjetivo qualificativo diminutivo",
          "AQS","adjective","adjetivo qualificativo superlativo",
          "AO0","adjective","adjetivo ordinal",
          "AOA","adjective","adjetivo ordinalaumentativo",
          "AOC","adjective","adjetivo ordinal diminutivo",
          "AOS","adjective","adjetivo ordinal superlativo",
          "A00","adjective","adjetivo",
          "A0A","adjective","adjetivo aumentativo",
          "A0C","adjective","adjetivo diminutivo",
          "A0S","adjective","adjetivo superlativo",
          "RG","adverb","adverbio general",
          "RN","adverb","adverbio negativo",
          "DD0","determiner","determinante demonstrativo",
          "DD1","determiner","determinante demonstrativo primeira",
          "DD2","determiner","determinante demonstrativo segunda",
          "DD3","determiner","determinante demonstrativo terceira",
          "DP0","determiner","determinante possessivo",
          "DP1","determiner","determinante possessivo primeira",
          "DP2","determiner","determinante possessivo segunda",
          "DP3","determiner","determinante possessivo terceira",
          "DT0","determiner","determinante interrogativo",
          "DT1","determiner","determinante interrogativo primeira",
          "DT2","determiner","determinante interrogativo segunda",
          "DT3","determiner","determinante interrogativo terceira",
          "DE0","determiner","determinante exclamativo",
          "DE1","determiner","determinante exclamativo primeira",
          "DE2","determiner","determinante exclamativo segunda",
          "DE3","determiner","determinante exclamativo terceira",
          "DI0","determiner","determinante indefenido",
          "DI1","determiner","determinante indefenido primeira",
          "DI2","determiner","determinante indefenido segunda",
          "DI3","determiner","determinante indefenido terceira",
          "DA0","determiner","determinante artigo",
          "DA1","determiner","determinante artigo primeira",
          "DA2","determiner","determinante artigo segunda",
          "DA3","determiner","determinante artigo terceira",
          "NCMS","noun","nome comum masculino singular",
          "NCMSSP","noun","nome comum masculino singular pessoa",
          "NCMSG0","noun","nome comum masculino singular lugar",
          "NCMSO0","noun","nome comum masculino singular organizacao",
          "NCMSV0","noun","nome comum masculino singular outros",
          "NCMP","noun","nome comum masculino plural",
          "NCMPSP","noun","nome comum masculino plural pessoa",
          "NCMPG0","noun","nome comum masculino plural lugar",
          "NCMPO0","noun","nome comum masculino plural organizacao",
          "NCMPV0","noun","nome comum masculino plural outros",
          "NCMN","noun","nome comum masculino invariavel",
          "NCMNSP","noun","nome comum masculino invariavel pessoa",
          "NCMNG0","noun","nome comum mascilino invariavel lugar",
          "NCMNO0","noun","nome comum masculino invariavel organizacao",
          "NCMNV0","noun","nome comum masculino invariavel outros",
          "NCFS","noun","nome comum feminino singular",
          "NCFSSP","noun","nome comum feminino singular pessoa",
          "NCFSG0","noun","nome comum feminino singular lugar",
          "NCFSO0","noun","nome comum feminino singular organizacao",
          "NCFSV0","noun","nome comum feminino singular outros",
          "NCFP","noun","nome comum feminino plural",
          "NCFPSP","noun","nome comum feminino plural pessoa",
          "NCFPG0","noun","nome comum feminino plural lugar",
          "NCFPO0","noun","nome comum feminino plural oranizacao",
          "NCFPV0","noun","nome comum feminino plural outros",
          "NCFN","noun","nome comum feminino invariavel",
          "NCFNSP","noun","nome comum feminino invariavel pessoa",
          "NCFNG0","noun","nome comum feminino invariavel lugar",
          "NCFNO0","noun","nome comum feminino invariavel organizacao",
          "NCFNV0","noun","nome comum feminino invariavel outros",
          "NCCS","noun","nome comum comum singular",
          "NCCSSP","noun","nome comum comum singular pessoa",
          "NCCSG0","noun","nome comum comum singular lugar",
          "NCCSO0","noun","nome comum comum singular organizacao",
          "NCCSV0","noun","nome comum comum singular outros",
          "NCCP","noun","nome comum comum plural",
          "NCCPSP","noun","nome comum comum plural pessoa",
          "NCCPG0","noun","nome comum comum plural lugar",
          "NCCPO0","noun","nome comum comum plural organizacao",
          "NCCPV0","noun","nome comum comum plural outros",
          "NCCN","noun","nome comum comum invariavel",
          "NCCNSP","noun","nome comum comum invariavel pessoa",
          "NCCNG0","noun","nome comum comum invariavel lugar",
          "NCCNO0","noun","nome comum comum invariavel organizacao",
          "NCCNV0","noun","nome comum comum invariavel outros",
          "NP0","noun","nome proprio",
          "NPMS","noun","nome proprio masculino singular",
          "NPMSSP","noun","nome proprio masculino singular pessoa",
          "NPMSG0","noun","nome proprio masculino singular lugar",
          "NPMSO0","noun","nome proprio masculino singular organizacao",
          "NPMSV0","noun","nome proprio masculino singular outros",
          "NPMP","noun","nome proprio masculino plural",
          "NPMPSP","noun","nome proprio masculino plural pessoa",
          "NPMPG0","noun","nome proprio masculino plural lugar",
          "NPMPO0","noun","nome proprio masculino plural organizacao",
          "NPMPV0","noun","nome proprio masculino plural outros",
          "NPMN","noun","nome proprio masculino invariavel",
          "NPMNSP","noun","nome proprio masculino invariavel pessoa",
          "NPMNG0","noun","nome proprio masculino invariavel lugar",
          "NPMNO0","noun","nome proprio masculino invariavel organizacao",
          "NPMNV0","noun","nome proprio masculino invariavel outros",
          "NPFS","noun","nome proprio feminino singular",
          "NPFSSP","noun","nome proprio feminino singular pessoa",
          "NPFSG0","noun","nome proprio feminino singular lugar",
          "NPFSO0","noun","nome proprio feminino singular organizacao",
          "NPFSV0","noun","nome proprio feminino singular outros",
          "NPFP","noun","nome proprio feminino plural",
          "NPFPSP","noun","nome proprio feminino plural pessoa",
          "NPFPG0","noun","nome proprio feminino plural lugar",
          "NPFPO0","noun","nome proprio feminino plural organizacao",
          "NPFPV0","noun","nome proprio feminino plural outros",
          "NPFN","noun","nome proprio feminino invariavel",
          "NPFNSP","noun","nome proprio feminino invariavel pessoa",
          "NPFNG0","noun","nome proprio feminino invariavel lugar",
          "NPFNO0","noun","nome proprio feminino invariavel organizacao",
          "NPFNV0","noun","nome proprio feminino invariavel outros",
          "NPCS","noun","nome proprio comum singular",
          "NPCSSP","noun","nome proprio comum singular pessoa",
          "NPCSG0","noun","nome proprio comum singular lugar",
          "NPCSO0","noun","nome proprio comum singular organizacao",
          "NPCSV0","noun","nome proprio comum singular outros",
          "NPCP","noun","nome proprio comum plural",
          "NPCPSP","noun","nome proprio comum plural pessoa",
          "NPCPG0","noun","nome proprio comum plural lugar",
          "NPCPO0","noun","nome proprio comum plural organizacao",
          "NPCPV0","noun","nome proprio comum plural outros",
          "NPCN","noun","nome proprio comum invariavel",
          "NPCNSP","noun","nome proprio comum invariavel pessoa",
          "NPCNG0","noun","nome proprio comum invariavel lugar",
          "NPCNO0","noun","nome proprio comum invariavel organizacao",
          "NPCNV0","noun","nome proprio comum invariavel outros",
          "CC","conjunction","conjuncao coordenada",
          "CS","conjunction","conjuncao subordenada",
          "I","interjection","interjecao",
          "SPS","adposition","adposicao preposicao simples",
          "SPC","adposition","adposicao preposicao contraida",
          "Z","number","chifra",
          "PP0","pronoun","pronome pessoal",
          "PP1","pronoun","pronome pessoal primeira",
          "PP2","pronoun","pronome pessoal segunda",
          "PP3","pronoun","pronome pessoal terceira",
          "PD0","pronoun","pronome demonstrativo",
          "PD1","pronoun","pronome demonstrativo primeira",
          "PD2","pronoun","pronome demonstrativo segunda",
          "PD3","pronoun","pronome demonstrativo terceira",
          "PX0","pronoun","pronome possessivo",
          "PX1","pronoun","pronome possessivo primeira",
          "PX2","pronoun","pronome possessivo segunda",
          "PX3","pronoun","pronome possessivo terceira",
          "PI0","pronoun","pronome indefenido",
          "PI1","pronoun","pronome indefenido primeira",
          "PI2","pronoun","pronome indefenido segunda",
          "PI3","pronoun","pronome indefenido terceira",
          "PT0","pronoun","pronome interrogativo",
          "PT1","pronoun","pronome interrogativo primeira",
          "PT2","pronoun","pronome interrogativo segunda",
          "PT3","pronoun","pronome interrogativo terceira",
          "PR0","pronoun","pronome relativo",
          "PR1","pronoun","pronome relativo primeira",
          "PR2","pronoun","pronome relativo segunda",
          "PR3","pronoun","pronome relativo terceira",
          "PE0","pronoun","pronome exclamativo",
          "PE1","pronoun","pronome exclamativo primeira",
          "PE2","pronoun","pronome exclamativo segunda",
          "PE3","pronoun","pronome exclamativo terceira",
          "VMI","verb","verbo principal indicativo",
          "VMS","verb","verbo principal subjuntivo",
          "VMM","verb","verbo principal imperativo",
          "VMN","verb","verbo principal infinitivo",
          "VMG","verb","verbo principal gerundio",
          "VMP","verb","verbo principal participio",
          "VAI","verb","verbo auxiliar indicativo",
          "VAS","verb","verbo auxiliar subjuntivo",
          "VAM","verb","verbo auxiliar imperativo",
          "VAN","verb","verbo auxiliar infinitivo",
          "VAG","verb","verbo auxiliar gerundio",
          "VAP","verb","verbo auxiliar participio",
          "VSI","verb","verbo semiauxiliar indicativo",
          "VSS","verb","verbo semiauxiliar subjuntivo",
          "VSM","verb","verbo semiauxiliar imperativo",
          "VSN","verb","verbo semiauxiliar infinitivo",
          "VSG","verb","verbo semiauxiliar gerundio",
          "VSP","verb","verbo semiauxiliar participio",
          "XY", "nonword", "nonword"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
        tag.class.def.punct=matrix(c(
          "F","punctuation","pontuacao",
          "Faa","punctuation","¡",
          "Fat","punctuation","!",
          "Fc","punctuation",",",
          "Fca","punctuation","[",
          "Fct","punctuation","]",
          "Fd","punctuation",":",
          "Fe","punctuation","\"",
          "Fg","punctuation","-",
          "Fh","punctuation","/",
          "Fia","punctuation","¿",
          "Fit","punctuation","?",
          "Fla","punctuation","{",
          "Flt","punctuation","}",
          "Fpa","punctuation","(",
          "Fpt","punctuation",")",
          "Fra","punctuation","«",
          "Frc","punctuation","»",
          "Fs","punctuation","…",
          "Ft","punctuation","%",
          "Fx","punctuation",";",
          "Fz","punctuation","_+="
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
        tag.class.def.sentc=matrix(c(
          "Fp","fullstop","."
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
      )
    ),
    ...
  )
}

# this internal, non-exported function causes the language support to be
# properly added when the package gets loaded
#' @importFrom sylly.pt hyph.support.pt
.onAttach <- function(...) {
  lang.support.pt()
  sylly.pt::hyph.support.pt()
}
