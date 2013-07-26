Yorudi - A Standardized & Downloadable Comprehensive Yoruba Multilingual Dictionary

DESCRIPTION
The Yorudi project aims to compile a complete multi-lingual lexical database with Yoruba as the pivot language. The project is modelled after the CC-CEDICT project by Paul Andrew Denisowski which was itself modeled on the highly successful EDICT project by Jim Breen. The former being a Chinese-English Electronic Dictionary and the latter, a Japanese-English Dictionary.

ABOUT YORUBA
Yoruba is the native tongue of the Yoruba people of West Africa. It is a tonal language (like Chinese) with a writing system very similar to Chinese Pinyin for demarcating tones and pronounciation. However, a few differences exist.

A. Different Tones 
There are only 3 tones in Yoruba, namely:
1. "Do": The low tone - represented by a grave accent e.g 'ò'
2. "Re": The mid tone - represented by an overline (or not at all) e.g 'õ' or 'o' 
3. "Mi": The high tone - represented by an acute accent e.g 'ó'

As with Chinese though, each word is unique by tones. So there are no homophones, while homographs abound.

B. Underdotted Characters 
The Yoruba alphabet features several underdotted letters. The underdot is used indicate a modified pronounciation of the normal sound. For example, the letter 'ṣ', is pronounced 'sh'

C. Contraction
Yoruba often uses assimilation and elision to contract words when forming other words, phrases, and sentences. These 2 processes are outlined below:
1. Elision:- The deletion of a vowel e.g ní + ilé = n'ílé i.e "in" + "house" = "in the house"
2. Assimilation :- The inheritance by a vowel of another vowel sound. e.g kú + ilé = kúulé i.e "greet" + "house" = "greetings" (used when visiting people in their homes)

This use of contraction sometimes leads to a certain ambiguity between words and phrases.

To learn more about the Yoruba people and their language, see http://yorupedia.com/

ENCODING
Given the unique properties of the Yoruba language (as detailed above), a specialized input format is necessary to accurately record words. Details of this input format are given below.

Every entry in a Yorudi dictionary must be made up of 4 major parts
1. A simplified Yoruba form of the word
2. The complete Yoruba decomposition of the word
3. The target language glossary of definitions
4. An attribute list (Optional)

INPUT FORMAT

          yoruba decomposition (2)                   optional attribute list (4)
                v                                             v
    gbogbo [gbó . gbó]  /all  /many  /every  <first: attribute | second: attribute>
       ^                       ^                       
simplified yoruba (1) glossary of definitions (3)


1. SIMPLIFIED YORUBA
This is simply the word in the standard roman alphabet.
(a) It should be recorded as it is spoken in the Oyo dialect for consistency
(b) Neither tone nor decomposition should be indicated e.g ati, jeun, loke, sugbon

2. YORUBA DECOMPOSITION
Here the word must be fully specified to include the following properties
(a) Tone marks
(b) Component words: Each component word must be separated by a period e.g ade [a . dé]

  More on Component Words
  -----------------------
  Because words are often derived from other words, and each of these component words may be modified for contraction, such properties must also be recorded as follows:
    - Elision: Elided letters may be recorded with a `minus arrow` to the left and/or right of the word 
        e.g foso [fọ . <-asọ] :- Here, the 'a' in aso is elided
            reja [ra-> . ẹja] :- Here, the 'a' in ra is elided
    - Assimilation: Assimilated letters may be recorded with a `plus arrow` to the left and/or right of the word 
        e.g kuule [kú+> . <-ilé] :- Here, The 'u' is assimilated and the 'i' elided
            arooke [ará-> . <+òkè] :- Here, the ò is assimilated and the á elided
    - Roots: The root of a word must always be indicated by an asterix (after all contractions, if present)  
        e.g nigba [ní . <-ìgbà*] :- Here, ìgbà is the root word, but it's 'ì' is elided when forming "nígbà"

3. GLOSSARY
The glossary is a list of synonymous words and phrases in the target language
(a) Each synonym must be separated by a forward slash 
    e.g leti [ní-> . eti*]  /near  /within earshot
(b) Each glossary entry may optionally feature short annotations in parentheses
    e.g tiantian [tían . tían]  /very high (for flying objects)
(c) For readability, each slash in the glossary should be two (2) spaces away from the last entry

4. ATTRIBUTE LIST
The attribute list may be used to indicate special properties such as indexes into other Yòrúdí language dictionaries. In most cases a contributor need not concern themselves with these.
(a) The attribute list must be denoted by angle brackets of the form < attrib. list >
(b) Each attribute consist of a key-value pair separated by a colon and must be separated by a vertical bar 
    e.g bi [bí]  /if  <fr: 567 | ru: 234>
(c) For readability, there should always be a spaces between vertical-bars and attributes as well as the colon and value in the key value pair (as in the previous example)

CREATING A YORUDI TRANSLATION FILE
A Yorudi translation file is a simple text file encoded in UTF-8 that contains a list of entries such as described above. At the top of the file must be a header specifying the target language and optional additional details such as the file author, date of creation, and so on; where each line begins with a #.

Example:
#lang: en
#summary: Colloquial Yoruba - English Translations
#author: Damola Mabogunje
#date: 12-12-13

These files may be created with any text editor able to save a .txt file. Although to be recognized as a translation file, the extension of a Yòrúdí file must be changed to .yor

ADDITIONAL NOTES
Writing some Yoruba characters requires that your keyboard is configured for writing accented and underdotted letters. The way to do this varies by operating system.

(a) Mac Configuration:
1. Go to System Preferences -> Keyboard -> Input Sources
2. Check the US Extended and US International Keyboards

Accenting a letter is best done with the US International Keyboard. 
- Acute accents are added by pressing ['] then the letter
- Grave accents are added by pressing [`] then the letter

Underdotting a letter is best done with the US Extended Keyboard.
- Press [Option] + [X] at the same time, then press the letter. 
                      OR
- Press the letter, then press [Option] + [Shift] + [X] at the same time (necessary when underdotting accented letters)
