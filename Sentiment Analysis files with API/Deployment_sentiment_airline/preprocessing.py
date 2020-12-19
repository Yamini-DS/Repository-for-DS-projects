#import all necessary packages
import nltk
nltk.download('punkt')
from nltk.stem import PorterStemmer
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
nltk.download('stopwords')
stop_words = set(stopwords.words('english'))
stop_words.remove("not")
import re
from bs4 import BeautifulSoup
from tqdm import tqdm

def text_preprocessing(reviews):
    
    
    """ This will clean the text data, remove html tags, remove special characters and then tokenize the reviews to apply Stemmer on each word token."""
    
    pre_processed_reviews=[]
    
    for review in tqdm(reviews):
        #review= BeautifulSoup(review,'lxml').getText()#remove html tags
    
        review=re.sub('[^A-Za-z]+',' ',review) #remove special chars
        review=re.sub("n't","not",review)
        review=nltk.word_tokenize(str(review.lower())) #tokenize the reviews into word tokens
        review=' '.join(PorterStemmer().stem(word) for word in review if word not in stopwords.words('english'))
        pre_processed_reviews.append(review.strip())
    return pre_processed_reviews


