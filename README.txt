Yórùdí - A Standardized & Downloadable Comprehensive Yoruba-Multilingual Dictionary

DESCRIPTION
The Yorudi project aims to compile a complete multi-lingual lexical database with Yoruba as the pivot language. The project is modelled after the CC-CEDICT project by Paul Andrew Denisowski which was itself modeled on the highly successful EDICT project by Jim Breen. The former being a Chinese-English Electronic Dictionary and the latter, a Japanese-English Dictionary.

ENCODING
Every entry in a Yorudi dictionary must be made up of 4 major parts
1. A simplified Yoruba form of the word
2. The complete Yoruba decomposition of the word
3. The target language glossary
4. (Optional) An attribute list

ENTRY FORM
simplified yoruba [yoruba . decomposition]  /language  /glossary < attrib. | list >

1. SIMPLIFIED YORUBA FORM
This is simply the word in the standard roman alphabet.
* It should be recorded as it is spoken in the Oyo dialect for consistency
* Neither tone nor decomposition should be indicated e.g ati, jeun, loke, sugbon

2. YORUBA DECOMPOSITION
Here the word must be fully specified to include the following properties
* Tone marks: These are indicated using accented vowels and/or a semi-colon where necessary to represent a dotted character 
  e.g. è é e; è;. The middle tone (re) is never indicated.
* Component words: Where applicable, the word should be broken up into its components, with each composite word separated by a period 
  e.g ade [a . dé]
* Roots: Where applicable, the root word is to be indicated by an asterix e.g soro [sò;* . ro;]
* Elision: If any part of a component word or vowel is elided during speech it must still be recorded in the decomposition
  with a `minus arrow` e.g foso [fo; . <-aso;] :- Here, The 'a' is elided
* Assimilation: If any part of a component word or vowel is assimilated during speech it must be recorded in the decomposition
  with a `plus arrow` e.g kuule [kú+> . <-ilé] :- Here, The 'u' is assimilated and the 'i' elided

3. GLOSSARY
The glossary is a list of synonymous words and phrases in the target language
* Each synonym must be separated by a forward slash e.g leti [ní-> . eti*]  /near  /within earshot
* Each glossary entry may optionally feature short annotations in parentheses
  e.g tiantian [tían . tían]  /very high (for flying objects)
* For readability, each slash in the glossary should be two (2) spaces away from the last entry

4. ATTRIBUTE LIST
The attribute list may be used to indicate special properties such as indexes into other Yòrúdí language dictionaries. In most cases a contributor need not concern themselves with these.
* The attribute list must be denoted by angle brackets of the form < attrib. list >
* Each attribute consist of a key-value pair separated by a colon and must be separated by a 
   vertical bar e.g bi [bí]  /if  <fr: 567 | ru: 234>
* For readability, there should always be a spaces between vertical-bars and attributes as well as the
  colon and value in the key value pair (as in the previous example)

CREATING A YÒRUDI TRANSLATION FILE
A Yorudi translation file is a simple text file encoded in UTF-8 that contains a list of entries such as described above. At the top of the file must be a header specifying the target language and optional additional details such as the file author, date of creation, and so on; where each line begins with a #

Example:
#lang: en
#summary: Colloquial Yoruba - English Translations
#author: Damola Mabogunje
#date: 12-12-13

Such files may be created with any text editor able to save a .txt file. Although to be recognized as a translation file, the extension of a Yòrúdí file must be changed to .yor
