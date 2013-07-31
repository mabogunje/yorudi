# Yòrúdí 
#### A Standardized & Downloadable Comprehensive Yoruba Multilingual Dictionary

The Yòrúdí project aims to compile a complete multi-lingual lexical database with [Yoruba](http://en.wikipedia.org/wiki/Yoruba_language) 
as the pivot language. The project is modelled after the [CC-CEDICT project](http://cc-cedict.org/wiki/) by Paul Andrew Denisowski which 
was itself modeled on the highly successful [EDICT project](http://www.csse.monash.edu.au/~jwb/edict.html) by Jim Breen. 
The former being a Chinese-English Electronic Dictionary and the latter, a Japanese-English Dictionary.

### An Introduction to Yoruba & The Problem
Yoruba is the native tongue of the [Yoruba people of West Africa](http://en.wikipedia.org/wiki/Yoruba_people). It is tonal (like Chinese), 
with a romanized writing system for demarcating tone and pronounciation. That is to say, like [Chinese Pinyin](http://en.wikipedia.org/wiki/Pinyin), 
and [Japanese Romaji](https://en.wikipedia.org/wiki/Romanization_of_Japanese), Yoruba can be written entirely within the extended
Latin alphabet. 

That notwithstanding, the construction of words in Yoruba is still fundamentally different from other languages, and it is my belief that
because existing databases do not take this into account, they fail to provide an adequate level of detail in their definitions.
In particular, the way most Yoruba words are made up of other Yoruba words is not taken advantage of.

#### Contractions in Yoruba
At its core, Yoruba has very few self-contained words over 4 letters (if any at all). All other words, are created through the combination
and permutation of the vocabulary: and as such, the direct meaning of any word is little more than the sum of its parts. 

Similarly, the spellings of words are always the result of merging their components. This merging may be done in any of **3 ways**.

1. **Addition** :- This is a simple joining of words

        bi + bọ = bibọ i.e "ask" + "to worship" = "that which is to be worshipped" 

2. **Elision** :- This is the deletion of a vowel when joining words

        ní + ilé = n'ílé i.e "in" + "house" = "in the house" 

3. **Assimilation** :- This is the inheritance by a vowel of another vowel sound when joining words 

        kú + ilé = kúulé i.e "greet" + "house" = "greetings!"

> _To learn more about the Yoruba people and their language, see http://yorupedia.com/_


### Creating a Yòrúdí File
Check out [this sample dictionary](https://github.com/mabogunje/yorudi/blob/master/dicts/sample.yor) and others in the dicts folder for 
examples. 

Such files may be easily created with any text editor able to save to _.txt_. 
Once created, you can change the extension to _.yor_ so it will be recognized as a translation file.

#### Understanding Yòrúdí Entries
Given the unique properties of the Yoruba language (as detailed above), a specialized input format is used to accurately record words. 
Details of this format are given below:

                yoruba decomposition (2)                   optional attribute list (4)
                        v                                             v
            gbogbo [gbó . gbó]  /all  /many  /every  <first: attribute | second: attribute>
               ^                              ^                       
        simplified yoruba (1)       glossary of definitions (3)


##### 1. SIMPLIFIED YORUBA
This is simply the word in the standard roman alphabet.
+ It should be recorded as it is spoken in the Oyo dialect for consistency
+ Neither tone nor decomposition should be indicated e.g ati, jeun, loke, sugbon

##### 2. YORUBA DECOMPOSITION
Here the word must be fully specified to include the following properties
+ Tone marks
+ Component words (making sure to identify the root)
+ Linguistic properties i.e  Assimilation and Elision

##### 3. GLOSSARY
The glossary is a list of synonymous words and phrases in the target language
+ Each synonym must be separated by a forward slash 
+ Each glossary entry may optionally feature short annotations in parentheses
+ For readability, each slash in the glossary should be two (2) spaces away from the last entry

##### 4. ATTRIBUTE LIST
The attribute list may be used to indicate special properties such as indexes into other Yòrúdí language dictionaries. In most cases a contributor need not concern themselves with these.
+ The attribute list must be denoted by angle brackets of the form < attrib. list >
+ Each attribute consist of a key-value pair separated by a colon and must be separated by a vertical bar 
+ For readability, there should always be a spaces between vertical-bars and attributes as well as the colon and value in the key value pair (as in the previous example)


## ADDITIONAL NOTES
Writing some Yoruba characters requires that your keyboard is configured for writing accented and underdotted letters. The way to do this varies by operating system.

### Mac Configuration:
1. Go to System Preferences -> Keyboard -> Input Sources
2. Check the US Extended and US International Keyboards

Accenting a letter is best done with the US International Keyboard. 
* Acute accents are added by pressing ['] then the letter
* Grave accents are added by pressing [`] then the letter

Underdotting a letter is best done with the US Extended Keyboard.
* Press [Option] + [X] at the same time, then press the letter. 
                      OR
* Press the letter, then press [Option] + [Shift] + [X] at the same time
