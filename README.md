# phrase-project

To detect new phrases or unknown words for texts in Japanese

日本語テキストから新しいフレーズや未知語を検出します。


## スクリプトの使用例
    from jphrase import extracter

    df_texts = pd.read_table("text.tsv", header=None, lineterminator='\n', names=["sentence"])
    jpex = extracter(**params)
    jpex.get_dfphrase(df_texts["sentence"])



## オプション

    params = {}#フレーズ検出におけるパラメータの設定    
    params["verbose"] = 1    
    params["size_sentence"] = 5000#一回で処理するセンテンスの数：大きすぎると計算が終わらないので注意    
    params["min_count"] = 10#文字連カウントの最小数閾値：小さくすると計算終わらない    
    params["min_length"] = 4 #文字の長さ最小値    
    params["max_length"] = 16#文字の長さ最大値
    params["weight_freq"] = 1 #頻度への重み（たくさんある連なりを重視）
    params["weight_len"] = 2   #長さへの重み（長い連なりを重視）    
    params["removes"] = "⠀ #�\n.：.…!！？?･ｰ￣*～\r\u3000、/＼／「」『』【】"#走査前に除去する文字
    params["unnecesary"] = ["http", "www", "ｗｗｗ", "&amp;", "&gt;"]#走査後に除去する文字列
    params["selection"] = 1# 0: セレクションの有無
    params["positive_filter"] = ["_2","__2", "___2","____2TZ___",#英単語（半角英文字のみ、ただし大文字が１つ以上入る  
                                "_______2__",#全角英文字のみ
                                "_1_10___00","__110___00",#カナ漢（e.g. データ分析 etc.  途中で挟まるひらがなは構わない、数字の混入は認めない） #ｶﾅ漢
                                "00011___00","0001___100",#漢英#漢全英
                                ]
    params["negative_filter"] = {"start":[0,8,9,-1], "end":[0,8,9,-1],#ひらがな/数字 での開始or終了
                                "periodic": True, "smalla":True, "kanantsu":True, "less_than_maxlen":True }
                                #周期性は不要、ひらがなや数字の開始終了も不要 　文字連の最大値は不使用
    params["threshold_originality"] = 0.50#独自性の閾値（.0 全く絞らず ～ .9 順位の低い似たフレーズが除去される）
    params["knowns"] = []
    
    jpex = extcter(**params)
    jpex.get_dfphrase(df_texts["sentence"])

