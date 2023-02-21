# coding:utf-8
__author__      = "Takeshi SHIMIZU"
__copyright__   = "Copyright 2023"

import numpy as np
import pandas as pd
from collections import Counter
from IPython.display import display
import re

class extracter():

    def __init__(self, min_count=6, max_length=-1, min_length=2, weight_freq=1.0, weight_len=1.0,
                removes="#�\n。. 、,！？!?「」【】()（）～:：\u3000",#走査時にあらかじめ除去する文字(ノイズ)
                unnecesary = ["http", "www", "＼", "／"],#既知の不要語
                threshold_originality = 0.5,#他と重複のあるフレーズを除去
                size_sentence = 3000,#一度にスキャンする配列のサイズ
                knowns=[],#カウント時に　優先してほしい既知語
                positive_filter = ["_2_","__2","___2","____2","______2",#カタカナのみ#半カナのみ
                                   "T1_10_","T_110_", "00011_", ],#漢字のみ#アルファベットのみ#全角アルファベットのみ#カナ漢#半カナ漢#アルファ漢                                                                                                                                                  
                negative_filter = {"start":[0,8,9,-1], "end":[0,8,9,-1],#ひらがな・数字の開始終了、その他文字での開始終了
                                   "periodic": True, "smalla":True,#周期性と小文字アルファベットは不要
                                   "kanantsu": True, "less_than_maxlen": True },
                selection = 1,#Falseの場合、選定しない結果を取得する
                verbose = 1,
                  ):

        #ハイパーパラメータ
        self.min_count = min_count
        self.weight_freq = weight_freq
        self.weight_len = weight_len
        self.max_length = max_length
        self.min_length = min_length
        self.removes = removes        #除去文字（探索時に不要な文字を除外する）
        self.unnecessary = unnecesary# ["http", "www"]        #不要語（出力時に欲しくない文字の連なり）
        self.knowns = knowns
        self.size_sentence = size_sentence#一度に処理するセンテンスのリストのサイズ
        self.threshold_originality = threshold_originality#類似度の閾値（独自性の無い語を除去する
        self.selection = selection
        self.verbose = verbose

        #取り出すフレーズのルール ポジティブとネガティブ
        self.positive_filter = positive_filter
        #例
        #["_2_",#全てカタカナのフレーズを拾う
        # "__2",#全て半カナのフレーズを拾う
        # "___2",#全て漢字のフレーズを拾う
        # "____2",#全てアルファベットのフレーズを拾う
        # "______2",#全て全角アルファベットのフレーズを拾う
        # "T1_10_",#カナ漢＝カタカナと漢字の混合、ひらがな混入ＯＫ
        # "T_110_",#半カナ漢
        # "00011_",#アルファ漢
        # ]
        self.negative_filter = negative_filter
        # {"start":[0,8,-1], "end":[0,8,-1], "periodic": True, "smalla":True }#周期性は不要
        #除外するフレーズのルール        #周期        #開始が　数字、ひらがな、記号        #終了が　数字、ひらがな、記号

    #列名
    clm_seqchar = "seqchar"
    clm_sc = "sc_index"
    clm_freq = "freq"
    clm_length = "length"
    clm_originality = "originality"
    clm_knowns = "knowns"
    clm_periodic = "periodic"

    #文字列定義
    others = "others"


    # 第1水準の漢字 (亜-腕)
    FIRST_KANJI=\
    '一丁七万丈三上下不与丑且世丘丙丞両並中串丸丹主乃久之乍乎乏乗乙九乞也乱乳乾亀了予争事二云互五井亘亙些亜亡交亥亦亨享京亭亮人什仁仇今介仏仔仕他付仙代令以仮仰仲件任企伊伍伎伏伐休会伝伯伴伶伸伺似伽佃但位低'\
    '住佐佑体何余作佳併佼使侃例侍供依侠価侭侮侯侵侶便係促俄俊俗保信俣修俳俵俸俺倉個倍倒倖候借倣値倦倫倭倶倹偉偏停健偲側偵偶偽傍傑傘備催傭債傷傾僅働像僑僕僚僧僻儀億儒償優儲允元兄充兆兇先光克免兎児党兜入全八'\
    '公六共兵其具典兼内円冊再冒冗写冠冥冨冬冴冶冷凄准凋凌凍凝凡処凧凪凱凶凸凹出函刀刃分切刈刊刑列初判別利到制刷券刺刻剃則削前剖剛剣剤剥副剰割創劃劇劉力功加劣助努劫励労効劾勃勅勇勉動勘務勝募勢勤勧勲勺勾勿匁'\
    '匂包化北匙匝匠匡匪匹区医匿十千升午半卑卒卓協南単博卜占卦卯印危即却卵卸卿厄厘厚原厨厩厭厳去参又叉及友双反収叔取受叙叛叡叢口古句叩只叫召可台叱史右叶号司吃各合吉吊吋同名后吏吐向君吟吠否含吸吹吻吾呂呆呈呉'\
    '告呑周呪味呼命咋和咲咳咽哀品哉員哨哩哲唄唆唇唐唖唯唱唾啄商問啓善喉喋喚喜喝喧喪喫喬喰営嗣嘆嘉嘗嘘嘩嘱噂噌噛器噴噸噺嚇嚢囚四回因団困囲図固国圃圏園土圧在圭地坂均坊坐坑坤坦坪垂型垢垣埋城埜域埠埴執培基埼堀'\
    '堂堅堆堕堤堪堰報場堵堺塀塁塊塑塔塗塘塙塚塞塩填塵塾境墓増墜墨墳墾壁壇壊壌壕士壬壮声壱売壷変夏夕外夙多夜夢大天太夫央失夷奄奇奈奉奏契奔套奥奨奪奮女奴好如妃妄妊妓妖妙妥妨妬妹妻妾姉始姐姑姓委姥姦姪姫姶姻姿'\
    '威娃娘娠娩娯娼婁婆婚婦婿媒媛嫁嫉嫌嫡嬉嬢嬬嬰子孔字存孜孝孟季孤学孫宅宇守安宋完宍宏宕宗官宙定宛宜宝実客宣室宥宮宰害宴宵家容宿寂寄寅密富寒寓寛寝察寡寧審寮寵寸寺対寿封専射将尉尊尋導小少尖尚尤尭就尺尻尼尽'\
    '尾尿局居屈届屋屍屑展属屠屡層履屯山岐岡岨岩岬岱岳岸峠峡峨峯峰島峻崇崎崖崩嵐嵩嵯嶋嶺巌川州巡巣工左巧巨差己巳巴巷巻巽巾市布帆希帖帝帥師席帯帰帳常帽幅幌幕幡幣干平年幸幹幻幼幽幾庁広庄庇床序底庖店庚府度座庫'\
    '庭庵庶康庸廃廉廊廓廟廠延廷建廻廼廿弁弄弊式弐弓弔引弗弘弛弟弥弦弧弱張強弼弾彊当形彦彩彪彫彬彰影役彼往征径待律後徐徒従得御復循微徳徴徹徽心必忌忍志忘忙応忠快念忽怒怖怜思怠急性怨怪怯恋恐恒恕恢恥恨恩恭息恰'\
    '恵悉悌悔悟悠患悦悩悪悲悶悼情惇惑惚惜惟惣惨惰想惹愁愈愉意愚愛感慈態慌慎慕慢慣慧慨慮慰慶慾憂憎憐憤憧憩憲憶憾懇懐懲懸戊戎成我戒或戚戟戦戯戴戸戻房所扇扉手才打払托扮扱扶批承技抄把抑投抗折抜択披抱抵抹押抽担'\
    '拍拐拒拓拘拙招拝拠拡括拭拳拶拷拾持指按挑挙挟挨挫振挺挽挿捉捌捕捗捜捧捨据捲捷捺捻掃授掌排掘掛掠採探接控推掩措掬掲掴掻揃描提揖揚換握揮援揺損搬搭携搾摂摘摩摸摺撃撒撚撞撤撫播撮撰撲撹擁操擢擦擬擾支改攻放政'\
    '故敏救敗教敢散敦敬数整敵敷文斉斌斎斐斑斗料斜斡斤斥斧斬断斯新方於施旅旋族旗既日旦旧旨早旬旭旺昂昆昇昌明昏易昔星映春昧昨昭是昼時晃晋晒晦晩普景晴晶智暁暇暑暖暗暢暦暫暮暴曇曙曜曝曲曳更書曹曽曾替最月有朋服'\
    '朔朕朗望朝期木未末本札朱朴机朽杉李杏材村杓杖杜束条杢来杭杯東杵杷松板枇析枕林枚果枝枠枢枯架柁柄柊柏某柑染柔柘柚柱柳柴柵査柾柿栂栃栄栓栖栗校栢株栴核根格栽桁桂桃案桐桑桓桔桜桝桟桧桶梁梅梓梗梢梧梨梯械梱梶'\
    '梼棄棉棋棒棚棟森棲棺椀椅椋植椎椙椛検椴椿楊楓楕楚楠楢業楯楳極楼楽概榊榎榔榛構槌槍様槙槻槽樋樗標樟模権横樫樵樹樺樽橋橘機橡橿檀檎櫓櫛櫨欄欝欠次欣欧欲欺欽款歌歎歓止正此武歩歪歯歳歴死殆殉殊残殖殴段殺殻殿毅'\
    '母毎毒比毘毛氏民気水氷永氾汀汁求汎汐汗汚汝江池汰汲決汽沃沈沌沓沖沙没沢沫河沸油治沼沿況泉泊泌法泡波泣泥注泰泳洋洗洛洞津洩洪洲活派流浄浅浜浦浩浪浬浮浴海浸消涌涙涛涜涯液涼淀淋淑淘淡淫深淳淵混添清渇済渉渋'\
    '渓渚減渠渡渥渦温測港湊湖湘湛湧湯湾湿満溌源準溜溝溢溶溺滅滋滑滝滞滴漁漂漆漉漏演漕漠漢漣漫漬漸潅潔潜潟潤潮潰澄澗澱激濁濃濠濡濫濯瀕瀞瀦瀧瀬灘火灯灰灸灼災炉炊炎炭点為烈烏烹焔焚無焦然焼煉煎煙煤照煩煮煽熊熔'\
    '熟熱燃燈燐燕燥燦燭爆爪爵父爺爽爾片版牌牒牙牛牝牟牡牢牧物牲特牽犀犠犬犯状狂狐狗狙狛狩独狭狸狼狽猛猟猪猫献猶猷猿獄獅獣獲玄率玉王玖玩玲珂珊珍珠珪班現球理琉琢琳琴琵琶瑚瑛瑞瑠瑳璃環璽瓜瓢瓦瓶甑甘甚甜生産甥'\
    '用甫田由甲申男町画界畏畑畔留畜畝畠畢略畦番異畳畷畿疋疎疏疑疫疲疹疾病症痔痕痘痛痢痩痴療癌癒癖発登白百的皆皇皐皮皿盃盆盈益盗盛盟監盤目盲直相盾省眉看県真眠眺眼着睡督睦瞥瞬瞭瞳矛矢知矧矩短矯石砂研砕砥砦砧'\
    '砲破砺砿硝硫硬硯硲碁碇碍碑碓碕碗碧碩確磁磐磨磯礁礎示礼社祁祇祈祉祐祖祝神祢祥票祭祷禁禄禅禍禎福禦禰禽禾禿秀私秋科秒秘租秤秦秩称移稀程税稔稗稚稜種稲稼稽稿穀穂穆積穎穏穐穣穫穴究空穿突窃窄窒窓窟窪窮窯窺竃'\
    '立竜章竣童竪端競竹竺竿笈笑笛笠笥符第笹筆筈等筋筏筑筒答策箆箇箔箕算管箪箭箱箸節範篇築篠篤篭簡簸簾簿籍米籾粁粂粉粋粍粒粕粗粘粛粟粥粧精糊糎糖糞糟糠糧糸系糾紀約紅紋納紐純紗紘紙級紛素紡索紫紬累細紳紹紺終絃'\
    '組経結絞絡絢給統絵絶絹継続綜綬維綱網綴綻綾綿緊緋総緑緒線締編緩緬緯練縁縄縛縞縦縫縮績繁繊繋繍織繕繭繰纂纏缶罪罫置罰署罵罷羅羊美群羨義羽翁翌習翠翫翰翻翼耀老考者而耐耕耗耳耶耽聖聞聡聯聴職聾肇肉肋肌肖肘肝'\
    '股肢肥肩肪肯肱育肴肺胃胆背胎胞胡胤胴胸能脂脅脆脇脈脊脚脱脳脹腎腐腔腕腫腰腸腹腺腿膏膚膜膝膨膳膿臆臓臣臥臨自臭至致臼興舌舎舗舘舛舜舞舟航般舵舶舷船艇艦艮良色艶芋芙芝芥芦芭芯花芳芸芹芽苅苑苓苔苗苛若苦苧苫'\
    '英茂茄茅茎茜茨茶茸草荊荏荒荘荷荻莞莫莱菅菊菌菓菖菜菟菩華菰菱萄萌萎萩萱落葉葎著葛葡董葦葬葱葵葺蒋蒐蒔蒙蒜蒲蒸蒼蓄蓉蓋蓑蓬蓮蔀蔑蔓蔚蔦蔭蔵蔽蕃蕉蕊蕎蕗蕨蕩蕪薄薗薙薦薩薪薫薬薮薯藁藍藤藩藷藻蘇蘭虎虐虚虜虞'\
    '虫虹虻蚊蚕蚤蛇蛋蛍蛎蛙蛤蛭蛮蛸蛾蜂蜘蜜蝉蝋蝕蝦蝶蝿融螺蟹蟻血衆行術街衛衝衡衣表衰衷衿袈袋袖被袴袷裁裂装裏裕補裟裡裳裸製裾複褐褒襖襟襲西要覆覇見規視覗覚覧親観角解触言訂計訊討訓託記訟訣訪設許訳訴診註証詐'\
    '詑詔評詞詠詣試詩詫詮詰話該詳誇誉誌認誓誕誘語誠誤説読誰課誹誼調談請諌諏諒論諜諦諭諮諸諺諾謀謁謂謄謎謙講謝謡謬謹識譜警議譲護讃讐谷豆豊豚象豪豹貌貝貞負財貢貧貨販貫責貯貰貴買貸費貼貿賀賂賃賄資賊賎賑賓賛賜'\
    '賞賠賢賦質賭購贈贋赤赦赫走赴起超越趣趨足距跡跨路跳践踊踏蹄蹟蹴躍身躯車軌軍軒軟転軸軽較載輔輝輩輪輯輸輿轄轍轟轡辛辞辰辱農辺辻込辿迂迄迅迎近返迦迩迫迭述迷追退送逃逆透逐逓途逗這通逝速造逢連逮週進逸逼遁遂'\
    '遅遇遊運遍過道達違遜遠遡遣遥適遭遮遵遷選遺遼避還邑那邦邪邸郁郊郎郡部郭郵郷都鄭酉酋酌配酎酒酔酢酪酬酵酷酸醇醍醐醒醗醜醤醸釆采釈里重野量金釘釜針釣釦釧鈍鈎鈴鈷鉄鉛鉢鉦鉱鉾銀銃銅銑銘銚銭鋒鋤鋪鋭鋲鋳鋸鋼錆'\
    '錐錘錠錦錨錫錬錯録鍋鍍鍔鍛鍬鍵鍾鎌鎖鎗鎚鎧鎮鏑鏡鐘鐙鐸鑑鑓長門閃閉開閏閑間関閣閤閥閲闇闘阜阪防阻阿陀附降限陛院陣除陥陪陰陳陵陶陸険陽隅隆隈隊階随隔隙際障隠隣隷隻隼雀雁雄雅集雇雌雑雛離難雨雪雫雰雲零雷電'\
    '需震霊霜霞霧露青靖静非面革靭靴鞄鞍鞘鞠鞭韓韮音韻響頁頂頃項順須預頑頒頓頗領頚頬頭頴頻頼題額顎顔顕願顛類顧風飛食飢飯飲飴飼飽飾餅養餌餐餓館饗首香馨馬馳馴駁駄駅駆駈駐駒駕駿騎騒験騨騰驚骨骸髄高髪髭鬼魁魂魅'\
    '魔魚魯鮎鮒鮪鮫鮭鮮鯉鯖鯛鯨鯵鰍鰐鰭鰯鰹鰻鱈鱒鱗鳥鳩鳳鳴鳶鴇鴎鴛鴨鴫鴬鴻鵜鵠鵡鵬鶏鶴鷲鷹鷺鹸鹿麓麗麟麦麹麺麻麿黄黍黒黙黛鼎鼓鼠鼻齢龍'
        

    dict_features = {
            "hiragana": re.compile('[ぁ-ゖ]+'),#ー を除外 #ゔゞ゛゜ー]+'),
            "katakana": re.compile('[ァ-ヶー]+'),#カタカナ
            "hankana": re.compile('[ｦ-ﾟ]+'),#半角カタカナ
            "kanji": re.compile(f'[{FIRST_KANJI}]+'),#漢字
            "alphabet": re.compile("[a-zA-Z]+"),#アルファベット
            "smalla": re.compile("[a-z]+"),#小文字アルファベット
            "largea": re.compile("[A-Z]+"),#大文字アルファベット
            "zalpha": re.compile("[Ａ-Ｚ]+"),#全角英文字
            "number": re.compile("[0-9]+"),#半角数字
            "znumber": re.compile("[０-９]+"),#全角数字
                         }


    def chopup(self, sentences):
        
        max_length = self.max_length
        if max_length == -1:
            max_length = len(sentences)//2
        min_length = self.min_length

        phrases = []
        for a_sentence in sentences:            
            for x in self.removes:
                a_sentence = a_sentence.replace(x, "")#除去文字の処理

            for char_length in range(1, max_length + 1):
                for i, c in enumerate(a_sentence):
                    if i + char_length - 1 < len(a_sentence):
                        phr = "".join(a_sentence[i:i+char_length])
                        #phr = phr.strip(self.removes)
                        if len(phr) >= min_length:
                            phrases.append(phr)
        return phrases


    def count_characters(self, phrases):
        
        cnt_ = Counter(phrases)
        seqchars, lengths, freqs = [],[],[]
        for k,v in cnt_.most_common():
            if v > self.min_count:
                seq_char = k
                seqchars.append(seq_char)
                lengths.append(len(seq_char))
                freqs.append(v)

        df_ret = pd.DataFrame({ self.clm_seqchar:seqchars, self.clm_length:lengths, self.clm_freq:freqs,})
        return df_ret

    def count_knowns(self, sentences):#既知語は必ず数える

        def count_all(sent, target):
            def find_all(a_str, sub):
                start = 0
                while True:
                    start = a_str.find(sub, start)
                    if start == -1: return
                    yield start
                    start += len(sub) # use start += 1 to find overlapping matches
            return len(list(find_all(sent, target)))
            
        dict_n = {}
        for k in self.knowns:
            dict_n[k] = 0
            for s in sentences:
                dict_n[k] += count_all(s, k)

        df = pd.DataFrame( {self.clm_seqchar: dict_n.keys(),
                            self.clm_length : [len(k) for k in dict_n.keys()],
                            self.clm_freq : dict_n.values()} )
        return df


    def hold_higherrank(self, df):#情報量でソートして包含関係にある「下位」のフレーズは除外
        
        df[self.clm_sc] = self.weight_freq * np.log( 1+ df[self.clm_freq] ) + self.weight_len * np.log( df[self.clm_length] )#sc_index

        df[self.clm_knowns] = df[self.clm_seqchar].astype(str).apply(lambda x: x in self.knowns)
        df = df.sort_values(by= [self.clm_knowns, self.clm_sc], ascending=False).reset_index()#既知語は上位表示
        
        #ソート上位との重複のフラグを立て除外
        dups = []
        for i, row in df.iterrows():
            flags = [(row[ self.clm_seqchar ] in higher_phrase) for higher_phrase in df.loc[:i-1, self.clm_seqchar].values]
            dups.append(any(flags))
        if len(df):
            df = df.loc[~np.array(dups)]
        return df


    #不要文字列含有シーケンスを除外：少しでもseqcharに混ざったら、そのseqcharを捨ててよい　e.g.) http, www
    def exclude_unnecessary(self , df ):
        mask_unnec = np.array( [False] * len(df) )
        for unnec in self.unnecessary:
            mask_unnec = mask_unnec | df[self.clm_seqchar].str.contains(unnec)
        df = df[~mask_unnec]
        return df

    def set_features(self, df):
        #フレーズには、ひらがなやカタカナ、漢字やアルファベットなどの含有率に特有の規則がある
        def contains_pattern(text, pattern):
            pattern_search = pattern.search(text)
            if pattern_search:
                span = pattern_search.span()
                if (span[0] == 0) and (len(text) == span[1]):
                    return 2#全てが該当
                return 1#一部が該当
            return 0#該当せず

        for k, pattern in self.dict_features.items():
            df[k] = df[self.clm_seqchar].map(lambda text: contains_pattern(text, pattern))

        #指定した位置の文字種類を列情報化
        def what_is(a_char):
            for k, pattern in self.dict_features.items():
                if a_char in "ンッーﾝｯｰ、んっ":#"ン,ッ,ー、んっ"は先頭に来ないものと定義（ンジャメナのような例外も？）
                    return "kanantsu"
                if pattern.search(a_char):
                    return k
            return self.others# "others"
        
        df["start_what"] = df[self.clm_seqchar].str[0].map(lambda a_char: what_is(a_char))
        df["end_what"] = df[self.clm_seqchar].str[-1].map(lambda a_char: what_is(a_char))

        #周期的な語（"回レ回レ回レ"）
        def doubt_periodic_letter(str_scan, len_period=2):
            if len(str_scan) <= 2:#二文字以下はスルー
                return 0
            if len(str_scan) == 3:#３文字の扱い e.g. ゲゲゲ:True
                if (str_scan[0] == str_scan[1]) & (str_scan[0] == str_scan[2]):
                    return 1
                else:
                    return 0
            doubt = str_scan[0:len_period]
            ret = True
            step = len(doubt)
            for i in range(0, len(str_scan), step):
                if i + 1 == len(str_scan):
                    break#奇数長文字対策
                ret *= int(doubt in str_scan[i: i+step])
            return ret
            doubt_periodic_letter("ガルガルガルガ")#くり返しでも奇数の場合に判定できるかどうか
            doubt_periodic_letter("ーーー")

        df[self.clm_periodic] = df[self.clm_seqchar].map(doubt_periodic_letter)
        return df



    def select_phrase(self, df):
        
        list_feature = list(self.dict_features.keys())
        list_feature.append(self.others)

        #ポジティブマスク#hiragana, katakana, hankana, kanji, alpha, small, large, zalpha, number, znumber, 
        def filter_from(str_filter):
            ret_mask = np.array([True] * len(df))
            for i, c in enumerate(str_filter):
                if c == "_": continue
                elif c == "Z": ret_mask = ret_mask & (df[list_feature[i]] != 0)#1 or 2
                elif c == "E": ret_mask = ret_mask & (df[list_feature[i]] != 1)#0 or 2
                elif c == "T": ret_mask = ret_mask & (df[list_feature[i]] != 2)#0 or 1
                else: ret_mask = ret_mask & (df[list_feature[i]] == int(c))# df["hiragana"] == 2
            return ret_mask
        masks = [filter_from(str_filter) for str_filter in self.positive_filter]

        #ネガティブマスク
        negative_mask = np.array([True] * len(df))
        for idx in self.negative_filter["start"]:#ひらがな・数字の開始は認めない
            negative_mask = negative_mask & ~( df["start_what"] == list_feature[idx] )
        for idx in self.negative_filter["end"]:#ひらがな・数字で終了も認めない
            negative_mask = negative_mask & ~( df["end_what"] == list_feature[idx] )        
        if self.negative_filter[self.clm_periodic]:
            negative_mask = negative_mask & ~(df[self.clm_periodic] == 1)            
        if self.negative_filter["smalla"]:#アルファベット小文字のみは除外対象
            negative_mask = negative_mask & ~filter_from("_____2")#smalla
        if self.negative_filter["kanantsu"]:#ン/ッ/ーでの開始はフレーズとして認めない
            for idx in self.negative_filter["start"]:
                negative_mask = negative_mask & ~( df["start_what"] == "kanantsu")
        if self.negative_filter["less_than_maxlen"]:#走査する長さ最大値未満のseqcharに限定（アルゴリズムの性質上、最大長ではフレーズは取得しにくい）
            negative_mask = negative_mask & ~(df[self.clm_length] == self.max_length)
            
        # ポジティブマスクとネガティブマスクを使って、フレーズを選定
        is_known = df[self.clm_knowns] == True
        df_newphrase = df[is_known].sort_values(by=self.clm_sc, ascending=False)# "sc_index"

        for mask in masks:
            mask_ = mask & ~is_known & negative_mask# negative_mask#消したい条件
            df_tmp = df[mask_].sort_values(by=self.clm_sc , ascending=False)#"sc_index"
            if len(df_tmp):
                df_newphrase = pd.concat([ df_newphrase, df_tmp ])

        return df_newphrase


    def find_uniques(self, sentences):
        manymany_stones = self.chopup(sentences)#切り刻む
        df_count = self.count_characters(manymany_stones)#数える
        df_knowns = self.count_knowns(sentences)#既知語は別にカウント
        df_concat = pd.concat([df_knowns, df_count])

        if not len(df_concat):
            return df_concat

        df_sorted = self.hold_higherrank(df_concat)#情報量でソート、重複除去        
        df_sorted = self.exclude_unnecessary(df_sorted)#不要語/不要文字　を含むseqcharを排除
        df_sorted = self.set_features(df_sorted)
        df_sorted.drop(columns="index", inplace=True)

        if self.selection > 0:
            return self.select_phrase(df_sorted)        
        return df_sorted


    def gen_sentences(self, sent_array):#分析対象となるテキストは多いこともあるので、yieldで分けて処理することで　計算時間が安定する
        sentences = []
        for multiple_sentence in sent_array:
            #センテンスの区切り文字になりそうなものは、あらかじめ統一して"\n"にしておく
            for delim in ["\r","。","．"]:
                multiple_sentence = multiple_sentence.replace(delim, "\n")

            for a_sentence in multiple_sentence.split("\n"):
                if len(a_sentence):
                    sentences.append(a_sentence)
                if len(sentences) >= self.size_sentence:
                    yield np.array(sentences)
                    sentences = []
        yield sentences


    def remove_similar(self, df_tmp):#上位に位置する語に関して類似度を計算して、独自性のあるフレーズのみを残す

        def get_originality(i):#上位に位置するフレーズとの類似度を得て独自性の高いフレーズを残す
            phrase = df_tmp.loc[i,self.clm_seqchar]
            max_similarity = 0.0
            for j in range(i):#当該インデックスよりも上位のフレーズを走査
                phrase_above = df_tmp.loc[j, self.clm_seqchar]
                sim = self.similarity(phrase, phrase_above)
                max_similarity = max(sim, max_similarity)
            return 1 - max_similarity
        
        df_tmp[self.clm_originality] = df_tmp.index.map(get_originality)
        mask_similar = df_tmp[self.clm_originality] > self.threshold_originality#閾値        
        return df_tmp[mask_similar]

    #レーベンシュタイン距離から類似性を計算
    def similarity(self, seq1,seq2):
        d = self.levenshtein(seq1, seq2)
        seq_length = (len(seq1) + len(seq2) )/2
        d_ratio = d/seq_length #文字長に占める　異なる文字数の割合
        return 1 - d_ratio

    def levenshtein(self, seq1, seq2):
        size_x = len(seq1) + 1
        size_y = len(seq2) + 1
        matrix = np.zeros ((size_x, size_y))
        for x in range(size_x):
            matrix [x, 0] = x
        for y in range(size_y):
            matrix [0, y] = y

        for x in range(1, size_x):
            for y in range(1, size_y):
                if seq1[x-1] == seq2[y-1]:
                    matrix [x,y] = min(matrix[x-1,y] + 1, matrix[x-1,y-1], matrix[x, y-1] + 1)
                else:
                    matrix [x,y] = min(matrix[x-1,y] + 1, matrix[x-1,y-1] + 1, matrix[x,y-1] + 1 )
        return (matrix[size_x - 1, size_y - 1])



    def get_dfphrase(self, sentences):

        sentences = np.array(sentences).reshape(-1,)
        
        def dict_agg(df_concat):#groupbyでdfを集計するときに文字列も統一的に扱う
            return  { c: ("first" if d == object else ("sum" if c == self.clm_freq else "mean"))\
                        for c, d in zip(df_concat.columns, df_concat.dtypes) }

        df_concat = pd.DataFrame()
        for partial_sentences in self.gen_sentences(sentences):
            df_tmp = self.find_uniques(partial_sentences)
            df_concat = pd.concat([df_concat, df_tmp])

            if len(df_concat) & self.verbose >= 1:
                print("途中経過")#暫定平均で集計
                df_toshow = df_concat\
                    .groupby(self.clm_seqchar, as_index =False).agg(dict_agg(df_concat))\
                    .sort_values(by=[self.clm_knowns, self.clm_sc], ascending=False)
                display(df_toshow.iloc[:5,:5])#

        if not len(df_concat):
            if self.verbose >= 1:
                print("フレーズが見つかりませんでした。")
            return df_concat

        else:
            if self.verbose >= 1:
                print("concat終了 -> ソーティング -> セレクション -> 重複除去")
            #一括化とソート&選定(下記では groupbyで平均できない文字列の扱いを定義)
            df_uniques_all = df_concat.groupby(self.clm_seqchar, as_index=False).agg(dict_agg(df_concat))
            df_uniques_all = self.hold_higherrank(df_uniques_all)#ここ時間かかる

            if self.selection <= 0:
                df_uniques_all.drop(columns="index", inplace=True)
                return df_uniques_all
            
            df_phrase = self.select_phrase(df_uniques_all)
            df_phrase = df_phrase.drop(columns="index").reset_index(drop=True)
            df_phrase = self.remove_similar(df_phrase)#時間かかる（最後の過程）
            return df_phrase
                        


#与えられたセンテンスの中から有意味な文字連＝フレーズを抽出したい（自己教師あり学習）
#実際には、走査対象となるテキストは 複数のファイルに分かれていたり、１つのセンテンスが膨大であることも多い
#進捗が分かりにくい verboseを引数にとる
#sc_indexでソートする、とか。　groupを処理しておくとか？

if __name__ == "__main__":
    print("フレーズの検知を始めます。")
    params = {}#ハイパーパラメータのデフォルト値を設定
    params["verbose"] = 1
    params["size_sentence"] = 5000#一回で処理するセンテンスの数：大きすぎると計算が終わらない
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
                                "_1_10___00","__110___00",#カナ漢（情報コミュニケーション学、途中で挟まるひらがなは構わない、数字の混入は認めない） #ｶﾅ漢
                                "00011___00","0001___100",#漢英#漢全英
                                ]
    params["negative_filter"] = {"start":[0,8,9,-1], "end":[0,8,9,-1],#ひらがな/数字 での開始or終了
                                "periodic": True, "smalla":True, "kanantsu":True, "less_than_maxlen":True }
                                #周期性は不要、ひらがなや数字の開始終了も不要 　文字連の最大値は不使用
    params["threshold_originality"] = 0.60#独自性の閾値（.0にすれば全く絞らず、.9なら順位の低い似たフレーズが除去される）
    params["knowns"] = []
    jp = extracter(**params)

    df_texts = pd.read_table("jphrase/text.tsv", header=None, lineterminator='\n', names=["sentence"])
    jp = extracter(**params)
    jp.get_dfphrase(df_texts["sentence"])
    #jp.get_dfphrase(["あいうあぶ","あぶぶぶ"])
