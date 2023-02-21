# phrase-project
To detect new phrases or unknown words for texts in Japanese


# Example
df_texts = pd.read_table("text.tsv", header=None, lineterminator='\n', names=["sentence"])
jp = jphrase(**params)
jp.get_dfphrase(df_texts["sentence"])

