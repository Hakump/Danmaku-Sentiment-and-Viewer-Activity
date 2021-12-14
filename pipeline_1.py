from os import error
import numpy as np
import pandas as pd
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer, pipeline

model_name = "liam168/c2-roberta-base-finetuned-dianping-chinese"
class_num = 2 # positive or negative
model = AutoModelForSequenceClassification.from_pretrained(model_name, num_labels=class_num)
tokenizer = AutoTokenizer.from_pretrained(model_name)
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

df = pd.read_pickle("danmukus_all_min.pkl")
filenames = df.filename.unique().astype(int)

classifier = pipeline('sentiment-analysis', model=model, tokenizer=tokenizer)
max_text_len = 512 - 2
for _i, filename in enumerate(filenames):
  # if _i > 2:
  #   break
  print(_i, filename)
  try:
    curr_video = df[df.filename == filename]
    by_seconds = curr_video.groupby(['time'], as_index = False).agg({'text': ' '.join})
    our_text = by_seconds.text.values
    by_seconds["filename"] = filename

    model_output = []
    for one_text in our_text:
      if len(one_text) > max_text_len:
        one_text = one_text[:max_text_len]
      one_output = classifier(one_text)[0]
      one_output = 1 if one_output['label'] == 'positive' else 0
      model_output.append(one_output)
    model_output = np.array(model_output)
    by_seconds["label"] = model_output

    by_seconds.to_pickle('output/'+str(filename)+'.pkl')
  except Exception as e:
    print(e)

  # print(by_seconds.head())
