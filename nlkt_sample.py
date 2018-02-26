from nltk import sent_tokenize, word_tokenize, PunktSentenceTokenizer, pos_tag
from nltk.corpus import stopwords, state_union

"""POS tag list:

CC	coordinating conjunction
CD	cardinal digit
DT	determiner
EX	existential there (like: "there is" ... think of it like "there exists")
FW	foreign word
IN	preposition/subordinating conjunction
JJ	adjective	'big'
JJR	adjective, comparative	'bigger'
JJS	adjective, superlative	'biggest'
LS	list marker	1)
MD	modal	could, will
NN	noun, singular 'desk'
NNS	noun plural	'desks'
NNP	proper noun, singular	'Harrison'
NNPS	proper noun, plural	'Americans'
PDT	predeterminer	'all the kids'
POS	possessive ending	parent\'s
PRP	personal pronoun	I, he, she
PRP$	possessive pronoun	my, his, hers
RB	adverb	very, silently,
RBR	adverb, comparative	better
RBS	adverb, superlative	best
RP	particle	give up
TO	to	go 'to' the store.
UH	interjection	errrrrrrrm
VB	verb, base form	take
VBD	verb, past tense	took
VBG	verb, gerund/present participle	taking
VBN	verb, past participle	taken
VBP	verb, sing. present, non-3d	take
VBZ	verb, 3rd person sing. present	takes
WDT	wh-determiner	which
WP	wh-pronoun	who, what
WP$	possessive wh-pronoun	whose
WRB	wh-abverb	where, when"""

example_text = 'Hello Mr. Smith, how are you doing today? Hope all is well! Lets go get some coffee later today. Are you ready?'
print(sent_tokenize(example_text),'\n')

words = word_tokenize(example_text)
print(words, '\n')

#now going to remove unimportant words
#trick, can append words to this stop list to act as a further filter
stop_words = set(stopwords.words("english"))

clean_list = []
#option 1
for i in words:
    if i not in stop_words:
        clean_list.append(i)

print('using option 1', clean_list, '\n')


#option 2 - using list comprehenssion
clean_list2 = [i for i in words if not i in stop_words]
print('using option 2 ', clean_list2, '\n')

#Speech tagging

custom_sent_tockenizer = PunktSentenceTokenizer(example_text)

def process_content():
    for i in words:
            words_two = word_tokenize(i)
            tagged = pos_tag(words_two)
            print(tagged)

process_content()
