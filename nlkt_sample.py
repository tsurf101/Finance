from nltk import sent_tokenize, word_tokenize, PunktSentenceTokenizer, pos_tag
from nltk.corpus import stopwords, state_union

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
