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


# this script is providing additional support for language "pt".
# adjustments kindly provided by Jan Vanhove and Katharina Karges

#' Language support for Portuguese
#' 
#' This function adds support for Portuguese to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "pt" to be used with koRpus
#'  \item \code{hyphen}: An additional set of hyphenation patterns (see \code{\link{hyph.pt}})
#'  \item \code{treetag}: The additional preset "pt-utf8", implemented according to the respective
#'    TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#'
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/Portuguese-Tagset.html}
#' @import koRpus
#' @export
lang.support.pt <- function() {

  # first tell koRpus where to find hyphenation patterns (see ?set.lang.support for details)
  koRpus::set.lang.support(target="hyphen",
    value=list(
      "pt"=c("pt", package="koRpus.lang.pt")
    )
  )

  # here you have to adjust the parameters according to the contents of the TreeTagger
  # scripts for your language (see ?set.lang.support for details)
  #  - if there's both UTF-8 and Latin1 scripts, add them both (as "pt-utf8" and "pt")
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
    )
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
    # JV: I've added a couple of these (see mailing list inquiry), but I should note I don't really know Portuguese.
    list(
      "pt"=list(
        tag.class.def.words=matrix(c(
          "A00","adjective","adjetivo",
          "A0A","adjective","adjetivo aumentativo",
          "A0C","adjective","adjetivo diminutivo",
          "A0S","adjective","adjetivo superlativo",
          "AO0","adjective","adjetivo ordinal",
          "AOA","adjective","adjetivo ordinalaumentativo",
          "AOC","adjective","adjetivo ordinal diminutivo",
          "AOS","adjective","adjetivo ordinal superlativo",
          "AQ0","adjective","adjetivo qualificativo",
          "AQA","adjective","adjetivo qualificativo aumentativo",
          "AQC","adjective","adjetivo qualificativo diminutivo",
          "AQD","adjective","adjetivo qualificativo diminutivo", # JV: not in guidelines, but seems to be identical to AQS?
          "AQS","adjective","adjetivo qualificativo superlativo",
          "CC","conjunction","conjuncao coordenada",
          "CS","conjunction","conjuncao subordenada",
          "DA0","determiner","determinante artigo",
          "DA1","determiner","determinante artigo primeira",
          "DA2","determiner","determinante artigo segunda",
          "DA3","determiner","determinante artigo terceira",
          "DD0","determiner","determinante demonstrativo",
          "DD1","determiner","determinante demonstrativo primeira",
          "DD2","determiner","determinante demonstrativo segunda",
          "DD3","determiner","determinante demonstrativo terceira",
          "DE0","determiner","determinante exclamativo",
          "DE1","determiner","determinante exclamativo primeira",
          "DE2","determiner","determinante exclamativo segunda",
          "DE3","determiner","determinante exclamativo terceira",
          "DI0","determiner","determinante indefenido",
          "DI1","determiner","determinante indefenido primeira",
          "DI2","determiner","determinante indefenido segunda",
          "DI3","determiner","determinante indefenido terceira",
          "DP0","determiner","determinante possessivo",
          "DP1","determiner","determinante possessivo primeira",
          "DP2","determiner","determinante possessivo segunda",
          "DP3","determiner","determinante possessivo terceira",
          "DT0","determiner","determinante interrogativo",
          "DT1","determiner","determinante interrogativo primeira",
          "DT2","determiner","determinante interrogativo segunda",
          "DT3","determiner","determinante interrogativo terceira",
          "I","interjection","interjecao",
          "NCCN","noun","nome comum comum invariavel",
          "NCCNG0","noun","nome comum comum invariavel lugar",
          "NCCNO0","noun","nome comum comum invariavel organizacao",
          "NCCNSP","noun","nome comum comum invariavel pessoa",
          "NCCNV0","noun","nome comum comum invariavel outros",
          "NCCP","noun","nome comum comum plural",
          "NCCPG0","noun","nome comum comum plural lugar",
          "NCCPO0","noun","nome comum comum plural organizacao",
          "NCCPSP","noun","nome comum comum plural pessoa",
          "NCCPV0","noun","nome comum comum plural outros",
          "NCCS","noun","nome comum comum singular",
          "NCCSG0","noun","nome comum comum singular lugar",
          "NCCSO0","noun","nome comum comum singular organizacao",
          "NCCSSP","noun","nome comum comum singular pessoa",
          "NCCSV0","noun","nome comum comum singular outros",
          "NCFN","noun","nome comum feminino invariavel",
          "NCFNG0","noun","nome comum feminino invariavel lugar",
          "NCFNO0","noun","nome comum feminino invariavel organizacao",
          "NCFNSP","noun","nome comum feminino invariavel pessoa",
          "NCFNV0","noun","nome comum feminino invariavel outros",
          "NCFP","noun","nome comum feminino plural",
          "NCFPG0","noun","nome comum feminino plural lugar",
          "NCFPO0","noun","nome comum feminino plural oranizacao",
          "NCFPSP","noun","nome comum feminino plural pessoa",
          "NCFPV0","noun","nome comum feminino plural outros",
          "NCFS","noun","nome comum feminino singular",
          "NCFSG0","noun","nome comum feminino singular lugar",
          "NCFSO0","noun","nome comum feminino singular organizacao",
          "NCFSSP","noun","nome comum feminino singular pessoa",
          "NCFSV0","noun","nome comum feminino singular outros",
          "NCMN","noun","nome comum masculino invariavel",
          "NCMNG0","noun","nome comum mascilino invariavel lugar",
          "NCMNO0","noun","nome comum masculino invariavel organizacao",
          "NCMNSP","noun","nome comum masculino invariavel pessoa",
          "NCMNV0","noun","nome comum masculino invariavel outros",
          "NCMP","noun","nome comum masculino plural",
          "NCMPG0","noun","nome comum masculino plural lugar",
          "NCMPO0","noun","nome comum masculino plural organizacao",
          "NCMPSP","noun","nome comum masculino plural pessoa",
          "NCMPV0","noun","nome comum masculino plural outros",
          "NCMS","noun","nome comum masculino singular",
          "NCMSG0","noun","nome comum masculino singular lugar",
          "NCMSO0","noun","nome comum masculino singular organizacao",
          "NCMSSP","noun","nome comum masculino singular pessoa",
          "NCMSV0","noun","nome comum masculino singular outros",
          "NP0","noun","nome proprio",
          "NPCN","noun","nome proprio comum invariavel",
          "NPCNG0","noun","nome proprio comum invariavel lugar",
          "NPCNO0","noun","nome proprio comum invariavel organizacao",
          "NPCNSP","noun","nome proprio comum invariavel pessoa",
          "NPCNV0","noun","nome proprio comum invariavel outros",
          "NPCP","noun","nome proprio comum plural",
          "NPCPG0","noun","nome proprio comum plural lugar",
          "NPCPO0","noun","nome proprio comum plural organizacao",
          "NPCPSP","noun","nome proprio comum plural pessoa",
          "NPCPV0","noun","nome proprio comum plural outros",
          "NPCS","noun","nome proprio comum singular",
          "NPCSG0","noun","nome proprio comum singular lugar",
          "NPCSO0","noun","nome proprio comum singular organizacao",
          "NPCSSP","noun","nome proprio comum singular pessoa",
          "NPCSV0","noun","nome proprio comum singular outros",
          "NPFN","noun","nome proprio feminino invariavel",
          "NPFNG0","noun","nome proprio feminino invariavel lugar",
          "NPFNO0","noun","nome proprio feminino invariavel organizacao",
          "NPFNSP","noun","nome proprio feminino invariavel pessoa",
          "NPFNV0","noun","nome proprio feminino invariavel outros",
          "NPFP","noun","nome proprio feminino plural",
          "NPFPG0","noun","nome proprio feminino plural lugar",
          "NPFPO0","noun","nome proprio feminino plural organizacao",
          "NPFPSP","noun","nome proprio feminino plural pessoa",
          "NPFPV0","noun","nome proprio feminino plural outros",
          "NPFS","noun","nome proprio feminino singular",
          "NPFSG0","noun","nome proprio feminino singular lugar",
          "NPFSO0","noun","nome proprio feminino singular organizacao",
          "NPFSSP","noun","nome proprio feminino singular pessoa",
          "NPFSV0","noun","nome proprio feminino singular outros",
          "NPMN","noun","nome proprio masculino invariavel",
          "NPMNG0","noun","nome proprio masculino invariavel lugar",
          "NPMNO0","noun","nome proprio masculino invariavel organizacao",
          "NPMNSP","noun","nome proprio masculino invariavel pessoa",
          "NPMNV0","noun","nome proprio masculino invariavel outros",
          "NPMP","noun","nome proprio masculino plural",
          "NPMPG0","noun","nome proprio masculino plural lugar",
          "NPMPO0","noun","nome proprio masculino plural organizacao",
          "NPMPSP","noun","nome proprio masculino plural pessoa",
          "NPMPV0","noun","nome proprio masculino plural outros",
          "NPMS","noun","nome proprio masculino singular",
          "NPMSG0","noun","nome proprio masculino singular lugar",
          "NPMSO0","noun","nome proprio masculino singular organizacao",
          "NPMSSP","noun","nome proprio masculino singular pessoa",
          "NPMSV0","noun","nome proprio masculino singular outros",
          "PD0","pronoun","pronome demonstrativo",
          "PD1","pronoun","pronome demonstrativo primeira",
          "PD2","pronoun","pronome demonstrativo segunda",
          "PD3","pronoun","pronome demonstrativo terceira",
          "PE0","pronoun","pronome exclamativo",
          "PE1","pronoun","pronome exclamativo primeira",
          "PE2","pronoun","pronome exclamativo segunda",
          "PE3","pronoun","pronome exclamativo terceira",
          "PI0","pronoun","pronome indefenido",
          "PI1","pronoun","pronome indefenido primeira",
          "PI2","pronoun","pronome indefenido segunda",
          "PI3","pronoun","pronome indefenido terceira",
          "PP+PP","pronoun","pronome pessoal", # JV: not in guidelines
          "PP0","pronoun","pronome pessoal",
          "PP1","pronoun","pronome pessoal primeira",
          "PP2","pronoun","pronome pessoal segunda",
          "PP3","pronoun","pronome pessoal terceira",
          "PR0","pronoun","pronome relativo",
          "PR1","pronoun","pronome relativo primeira",
          "PR2","pronoun","pronome relativo segunda",
          "PR3","pronoun","pronome relativo terceira",
          "PT0","pronoun","pronome interrogativo",
          "PT1","pronoun","pronome interrogativo primeira",
          "PT2","pronoun","pronome interrogativo segunda",
          "PT3","pronoun","pronome interrogativo terceira",
          "PX0","pronoun","pronome possessivo",
          "PX1","pronoun","pronome possessivo primeira",
          "PX2","pronoun","pronome possessivo segunda",
          "PX3","pronoun","pronome possessivo terceira",
          "RG","adverb","adverbio general",
          "RN","adverb","adverbio negativo",
          "SP+DA","adposition","adposicao preposicao contraida", # JV: not in guidelines
          "SP+DD","determiner","determinante demonstrativo", # JV: not in guidelines, unsure
          "SPC","adposition","adposicao preposicao contraida",
          "SPS","adposition","adposicao preposicao simples",
          "SPS+DI0","determiner","determinante indefenido", # JV: not in guidelines
          "SPS+PD+PI","pronoun","pronome indefenido", # JV: not in guidelines; not sure
          "SPS+PI0","pronoun","pronome indefenido", # JV: not in guidelines
          "SPS+PP3","adposition","adposicao preposicao contraida", # JV: not in guidelines
          "SPS+RG", "adverb", "adverbio general", # JV: not in guidelines
          "VAG","verb","verbo auxiliar gerundio",
          "VAI","verb","verbo auxiliar indicativo",
          "VAM","verb","verbo auxiliar imperativo",
          "VAN","verb","verbo auxiliar infinitivo",
          "VAP","verb","verbo auxiliar participio",
          "VAS","verb","verbo auxiliar subjuntivo",
          "VMG","verb","verbo principal gerundio",
          "VMI","verb","verbo principal indicativo",
          "VMM","verb","verbo principal imperativo",
          "VMN","verb","verbo principal infinitivo",
          "VMP","verb","verbo principal participio",
          "VMS","verb","verbo principal subjuntivo",
          "VSG","verb","verbo semiauxiliar gerundio",
          "VSI","verb","verbo semiauxiliar indicativo",
          "VSM","verb","verbo semiauxiliar imperativo",
          "VSN","verb","verbo semiauxiliar infinitivo",
          "VSP","verb","verbo semiauxiliar participio",
          "VSS","verb","verbo semiauxiliar subjuntivo",
          "XY", "nonword", "nonword"
          "Z","number","chifra",
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
    )
  )
}

# this internal, non-exported function causes the language support to be
# properly added when the package gets loaded
.onAttach <- function(...) {
  lang.support.pt()
}
