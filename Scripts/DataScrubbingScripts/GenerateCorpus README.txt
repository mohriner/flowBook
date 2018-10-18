This script takes the source files of verse transcriptions and creates the corpus object. In doing so, it adds the following variables to the transcriptions:
* CMU_accent
* accent, through the algorithm in “AnnotateAccent.r” and with reference to the accent override table in “accentOverride.txt”
* line, done through the lyric segmentations in “Source Data/Lyrics/Line Segmentation”
* breath, done through the lyric segmentations in “Source Data/Lyrics/Breath Segmentation”
* beatIndex, the elapsed beats since downbeat 0. This requires the determination of the bar and beat of the first syllable, given in “verseMetadata.txt”
* toggled, Boolean value reflecting whether the accent was overridden.
* verse
* artist
* corpus
