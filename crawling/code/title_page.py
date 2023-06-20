from urllib.request import Request, urlopen
from urllib.error import URLError,HTTPError
from bs4 import BeautifulSoup
import pandas as pd
import time
from	wordcloud import	WordCloud
from	konlpy.tag import	Okt
from	collections	import	Counter
import	matplotlib.pyplot as	plt
import	platform

name=[]
# 페이지 추출 
for year in range(2015,2024):
    url=f'https://medium.com/daangn/archive/{year}'
    req=Request(f'https://medium.com/daangn/archive/{year}',headers={'User-Agent':'Mozilla/5.0'})
    html=urlopen(req)
    soup = BeautifulSoup(html.read(), 'html.parser')
    time.sleep(5)
    # print(url)
    stremItem=soup.select('h3.graf')
    for i in stremItem:
        # print(title)
        title=i.text.replace("\xa0","").replace("\u200a","").replace("�","").replace("\u200d","").split(' ')
        for i in title:
            name.append(i)
# print(name)

def	make_wordcloud(word_count,	title_list):
    okt =	Okt()
    sentences_tag =	[]
    #	형태소 분석하여 리스트에 넣기
    for	sentence	in	title_list:
        morph	=	okt.pos(sentence)
        sentences_tag.append(morph)
    noun_adj_list =	[]
    for	sentence1	in	sentences_tag:
        for	word,	tag	in	sentence1:
            if	tag	== 'Noun':
                noun_adj_list.append(word)
    #	형태소별 count
    counts	=	Counter(noun_adj_list)
    tags	=	counts.most_common(word_count)

    if	platform.system()	==	'Windows':
        path	=	r'c:\Windows\Fonts\malgun.ttf'
    elif platform.system()	==	'Darwin':		
        path	=	r'/System/Library/Fonts/AppleGothic'
    else:
        path	=	r'/usr/share/fonts/truetype/name/NanumMyeongjo.ttf'
    wc =	WordCloud(font_path=path,	background_color='white',	width=800,	height=600)
    print(dict(tags))
    cloud	=	wc.generate_from_frequencies(dict(tags))
    plt.figure(figsize=(20,	8))
    plt.axis('off')
    plt.imshow(cloud)
    plt.show()

make_wordcloud(20,name)