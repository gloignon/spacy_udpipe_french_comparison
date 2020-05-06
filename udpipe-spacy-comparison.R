library(udpipe)
library(spacyr)
library(data.table)

##############################################################################################
## UD_French-Sequoia
##  + Evaluation data from https://github.com/UniversalDependencies/UD_French-Sequoia
##  + udpipe + spacy were trained on the same data
##  + Note: no xpos in this treebank
##############################################################################################
#download.file(url = "https://raw.githubusercontent.com/UniversalDependencies/UD_French-Sequoia/master/fr_sequoia-ud-test.conllu", 
#              destfile = "gold.conllu")
#download.file(url = "https://github.com/UniversalDependencies/UD_French-Sequoia/archive/r2.0-test.zip",
#              destfile = "r2.0-test.zip")
#unzip("r2.0-test.zip", list = TRUE)
#unzip("r2.0-test.zip", files = "UD_French-Sequoia-r2.0-test/fr_sequoia-ud-test.conllu", exdir = getwd(), junkpaths = TRUE)
#file.copy("fr_sequoia-ud-test.conllu", "gold.conllu", overwrite = TRUE)


gold <- udpipe_read_conllu("gold.conllu")
sentences <- unique(gold$sentence)
sentences <- rle(gold$sentence)$values
sentences <- readLines("gold.conllu", encoding = "UTF-8")
sentences <- grep(pattern = "# text =", sentences, value=TRUE)
sentences <- gsub("# text = ", "", sentences)

## Annotation with UDPipe GSD
load_model_french <-
  udpipe_load_model(file = "french-gsd-ud-2.4-190531.udpipe") #chargement du modèle linguistique
anno <- udpipe_annotate(udmodel_french, sentences)
cat(anno$conllu, file = file("predictions_udpipe.conllu", encoding = "UTF-8"))
x <- as.data.frame(anno)


## Annotation with UDPipe Sequoia
load_model <- udpipe_load_model("french-sequoia-ud-2.1-20180111.udpipe")
anno <- udpipe_annotate(udmodel_french, sentences)
cat(anno$conllu, file = file("predictions_udpipe.conllu", encoding = "UTF-8"))
x <- as.data.frame(anno)

## Annotation with Spacy
#spacy_finalize()
#spacy_download_langmodel("fr")
spacy_initialize(model = "fr")
names(sentences) <- sprintf("%s %s", seq_along(sentences), sentences)
x <- spacy_parse(sentences, pos = TRUE, tag = TRUE, lemma = TRUE, dependency = TRUE, entity = FALSE)
sum(duplicated(x[, c("doc_id", "sentence_id", "token_id")]))
x$sentence <- x$doc_id
x$feats <- sapply(strsplit(x$tag, "__"), FUN=function(x) if(length(x) == 1) return(NA) else tail(x, 1))
x$upos <- x$pos
x$dep_rel <- txt_recode(x$dep_rel, from = "ROOT", to = "root")
x$head_token_id <- ifelse(x$dep_rel == "root", 0, x$head_token_id)
cat(as_conllu(x), file = file("predictions_spacy.conllu", encoding = "UTF-8"))
spacy_finalize()

## Compare evaluations
system("python evaluation_script/conll17_ud_eval.py -v gold.conllu predictions_udpipe.conllu")
system("python evaluation_script/conll17_ud_eval.py -v gold.conllu predictions_spacy.conllu")
