
# Instalando pacotes
install.packages("SentimentAnalysis")
install.packages("RSentiment")
install.packages("rjson")
install.packages("jsonlite")
install.packages("C:\\Users\\vitor\\Downloads\\Rstem_0.4-1.tar.gz", repos = NULL, type="source")
install.packages("C:\\Users\\vitor\\Downloads\\sentiment_0.2.tar.gz", repos = NULL, type="source")
install.packages("Rstem", repos = "http://www.omegahat.org/R")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("sentiment")
install.packages("gapminder")
install.packages("tibble")
install.packages("lubridate")


# Carregando os pacotes
library(gapminder)
library(widyr)
library(ggraph)
library(igraph)
library(sentiment)
library(SentimentAnalysis)
library(tm)
library(SnowballC)
library(wordcloud)
library(jsonlite)
library(rjson)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidytext)
library(magrittr)
library(plotly)
library(Rstem)
library(ggExtra)
library(magrittr) 
library(lubridate)
library(stringr)
library(tidytext)
library(lexiconPT)
library(RColorBrewer)
library(fpc)
library(topicmodels)
library(ggplot2)
library(stringr)
library(cluster)
library(tm)
library(wesanderson)
library(rtweet)
library(tibble)
library(lubridate)

# Obter token e chave do twitter no site apps.twitter.com
api_key             <- "seu_api_key"
api_secret          <- "seu_api_secret"
access_token        <- "seu_ccess_token "
access_token_secret <- "seu_access_token_secret"
twitter_app         <- "Twitter Sentiment Analysis."

# Acessamos o Twitter por meio dos dados do token
create_token(
  app             = twitter_app,
  consumer_key    = api_key,
  consumer_secret = api_secret,
  access_token    = access_token,
  access_secret   = access_token_secret)

# Para extrair os tweets com os termos desejados
tweets_ansiedade    <- search_tweets("ansiedade", n = 40000, include_rts = FALSE, retryonratelimit = TRUE, lang="pt")


# Aponta para o diretorio
setwd("C:\\Users\\vitor\\Desktop\\PROJETO FINAL\\extração 19.07")
getwd()


# carregando o arquivo
consulta <- read_json("consolidado_semdupli.json", simplifyVector = TRUE)


# Função para limpeza dos tweets
f_clean_tweets <- function (tweets) {
  
  clean_tweets <- tweets$text
  # remove retweet 
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', ' ', clean_tweets)
  # remove nomes pessoas
  clean_tweets = gsub('@\\w+', ' ', clean_tweets)
  # remove pontuação
  clean_tweets = gsub('[[:punct:]]', ' ', clean_tweets)
  # remove números
  clean_tweets = gsub('[[:digit:]]', ' ', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', ' ', clean_tweets)
  # remove espaços desnecessários
  clean_tweets = gsub('[ \t]{2,}', ' ', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', ' ', clean_tweets)
  # remove emojis e caracteres especiais
  clean_tweets = gsub('<.*>', ' ', enc2native(clean_tweets))
  # remove quebra de linha
  clean_tweets = gsub('\\n', ' ', clean_tweets)
  # remove espaços desnecessários
  clean_tweets = gsub('[ \t]{2,}', ' ', clean_tweets)
  # coloca tudo em minúsculo
  clean_tweets = tolower(clean_tweets)
  
  # remove tweets duplicados
  tweets$text <- clean_tweets
  tweets <- tweets[!duplicated(tweets$text),]
  tweets
}

# Aplica a função F_clean_tweets na base de dados
df_tweets    <- f_clean_tweets(consulta)

# Obtendo o texto dos tweets e a Data
tweets_textos <- df_tweets[,c(3,5)]

# Pegar só a data do campo "created_at"
tweets_textos$created_at = str_sub(tweets_textos$created_at, 1, 10)

# Para remover caracterer especiais e stopwords
x = tweets_textos$text

df_stopwords = removeWords(x, c("v", "co", "t", "–","—"," ","‘","“","”","»","…","•","b","c","d","f","g","“—","av","bd","bg","bm","bz","cq","dg","dh","dk","dq","eb","ec","ee","ej","eo","fk","fu","fw","gg","gl","hb","hd","hh","hk","ho","ht","hw","hx","jm","jt","jx","kd","kh","kt","ku","lc","lv","m…","mw","nf","nk","nl","np","nx","nz","oa","ob","oc","pj","px","pz","qc","qj","rl","rn","sc","tj","ud","ut","ux","uz","vl","wg","wr","wy","wz","xf","xp","xq","yf","yp","ys","yw","yx","zh","zi","zj","zm","zn","zo","zv","zx","zz","“po","“pq","“so","“ur","aam","aax","aay","abg","abl","abm","abo","abp","abs","acg","ach","ack","adr","aes","aex","afa","afd","afs","agx","ahm","ahn","ahp","aic","aij","ais","aiy","ajb","ajk","ajr","ajv","aka","akh","akk","aku","ala","alt","alz","amd","amv","amy","ane","anw","aox","api","aqp","aqy","art","arx","ass","atd","ath","atl","atr","atv","atz","aut","avl","axx","axz","aya","ayn","ayp","azd","azk","azt","azz","baa","bai","bbn","bbo","bcf","bdk","bdn","bea","bee","bef","bex","bey","bez","bfb","bff","bfo","bfq","bgi","bgk","bgm","bha","bhu","bib","bid","bih","bin","bit","bje","bji","bjo","bka","bkg","bkm","bls","bmo","bnn","boc","boe","bog","bor","box","bqy","bri","brt","bsj","bsz","btb","bte","bua","buc","bud","bue","buj","bum","but","buy","bvf","bvk","bvn","bvs","bwr","bxl","byo","bzz","cac","cak","cam","can","cao","ccd","ccq","cda","cde","cdo","cdv","cei","cej","cfg","cfv","cgw","cho","chu","cic","cid","cit","ciu","cjb","cjk","cjl","cjm","cjw","ckm","cla","clc","cll","clr","clw","cma","cmn","cmv","cnf","cnj","col","cpa","cpt","cpx","cql","crt","cru","cry","csa","csd","csl","cst","cti","ctl","ctm","cum","cur","cuz","cvb","cvv","cwi","cxk","cyt","cyz","czf","czj","czx","dag","dah","dan","dax","dba","dbf","dbv","dca","dch","dcp","dcs","dct","dde","ddm","dem","deo","dev","dfp","dhl","dhw","dic","dij","dix","dju","djv","dkm","dkv","dla","dle","dlo","dmh","dml","dmv","dnr","doa","dpb","dqu","dra","dri","drl","dsa","dsd","dsr","dtj","dtk","dua","duc","dur","dvd","dvm","dxq","dxs","dxv","dyc","dyo","dzr","eaa","eai","ebd","ebs","ebu","ebz","eca","ect","ecu","edo","edp","edy","eek","eeo","eep","eex","eey","eff","efz","egk","egm","ego","egp","ehe","ehf","ehi","ehj","ehs","eii","eik","eim","eja","ejl","ekg","ekn","elk","elm","elu","emi","enm","enp","eof","eok","eop","eot","epa","epc","eph","epq","ept","eqb","eqt","eqz","erc","ere","erh","err","ese","eta","etx","eub","eun","eur","euu","eva","eve","evu","ewt","exg","exp","eyc","eyf","eyh","ezq","fae","fap","far","fat","fbc","fbe","fbo","fbw","fcl","fcs","fdd","fdo","fdv","fdz","feh","fep","fer","ffi","ffv","fgl","fgs","fhj","fhn","fhy","fih","fio","fiq","fiv","fjb","fjw","fjy","fkh","fkv","fli","fll","flo","flr","flw","fly","fmb","fml","fmp","fmw","fnx","fod","foj","fou","fox","fpp","fpx","fqb","frx","ftc","fto","ftv","ftz","fub","fud","fun","fut","fvd","fve","fvo","fwk","fxm","fxp","fxt","fyh","fyu","fzc","fzj","fzu","gal","gbf","gcf","gcl","gcm","gcn","gdm","gds","gee","gem","geq","gfd","gfr","gfs","ggb","ggj","ggs","gil","giq","git","gjz","gla","glp","gmn","gna","gog","goi","gok","gol","gom","gow","gpn","gqg","gql","gqo","grj","grr","gsm","gtg","gui","guk","gup","gvf","gvw","gwu","gxi","gxo","gxq","gyk","gyx","gze","gzu","hag","hah","hao","haw","hbf","hbu","hbv","hce","hcq","hct","hda","hdb","hdo","hdr","hej","hek","hfl","hfo","hga","hgc","hge","hgg","hgs","hgx","hho","hhs","hix","hjm","hjt","hlc","hlq","hmn","hnz","hoh","hoj","hok","hoy","hqn","hqs","hqu","hrf","hrk","huo","hvx","hwg","hwj","hxk","hxl","hxs","hyb","hyg","hyp","hyq","hzt","hzx","ian","iat","iaw","ibz","icc","icf","icn","ict","icy","idc","ids","idt","iec","iex","igg","ign","igp","ihg","ihi","ihr","ihu","ihw","iib","iip","ijr","iki","ikw","ili","ill","iln","ilx","ima","iml","inb","ing","ini","ioa","ioi","ioz","ipe","ipv","iqq","irb","irg"))
df_stopwords = removeWords(df_stopwords, c("irn","irp","irv","isc","iss","isy","ita","itk","itm","itx","iuq","iuy","ivb","ivp","ivy","ivz","iwc","iwh","iwq","ixb","ixc","ixf","iyk","ize","izk","izr","izs","jah","jat","jbd","jbl","jbm","jbz","jcd","jcn","jdq","jej","jen","jey","jez","jfr","jgf","jgj","jgk","jgm","jgn","jgu","jgv","jha","jhe","jig","jin","jjd","jjm","jke","jli","jln","jlv","jlx","jly","jmo","jmv","jnd","jnz","job","joe","joh","jot","joy","jpn","jps","jqi","jsx","jtd","jtn","jtq","jtw","jtz","jup","juy","jve","jvp","jxq","jxv","jyg","jzn","jzp","kab","kak","kan","kay","kaz","kba","kbh","kdh","kdt","keb","ken","kff","kgn","khd","khz","kio","kjm","kjw","klj","klm","kls","kmu","kni","kof","koj","koo","kpm","kpt","kqk","kql","kqu","kqv","kra","krq","ksb","ksi","ksz","ktb","kuc","kuf","kun","kur","kuw","kvg","kvv","kwo","kws","kxc","kxl","kxn","kxw","kxz","kyb","kyf","kyv","kyz","kzd","kzj","lab","lai","lax","lbc","lbd","lbk","lbu","ldy","lea","lee","let","lfd","lfe","lfh","lgc","lgd","lgi","lhg","lhj","lhl","lho","lhp","lig","lik","lin","liq","lis","lit","liu","ljj","ljm","ljn","ljp","lju","llk","llw","llz","lmk","lmt","lmu","lnr","loc","lox","lpe","lph","lpq","lpv","lpz","lra","lrz","lsb","lsg","lst","lth","ltj","luf","luh","lup","luv","luw","lvy","lwm","lwx","lwy","lxw","lyc","lyj","lyn","lyv","lzw","mac","mak","max","may","mbu","mcl","mcr","mcv","mdz","meh","mei","men","met","mev","mfj","mfn","mfu","mge","mhv","mhx","mhy","mhz","mic","mio","miq","mjh","mjr","mkg","mkk","mkm","mlr","mlx","mma","mmo","mna","mnt","mob","moh","mov","mpb","mpe","mpj","mrd","mrs","mrw","msa","msr","msv","mtv","mue","muy","muz","mvw","mwd","mwj","mwm","mxd","mxh","mxj","myo","mzl","nah","nak","nam","nar","nat","nbm","nbs","ndg","ndi","nea","neb","nee","nen","neo","ner","new","nfe","nfg","nfs","nge","nhq","nhy","nic","nig","nih","niq","nkd","nke","nku","nlg","nma","nmr","nng","nnt","noa","noc","nom","noq","noz","npi","nql","nqo","nrd","nrg","nrq","nsa","nsi","nta","ntc","ntj","ntz","nuj","nuw","nvf","nvg","nvp","nvt","nwt","nww","nzh","nzm","oaf","oai","oal","oap","oba","obe","obr","ocm","ocu","odu","odv","oef","ofd","oft","ofx","ogv","ohh","ohw","oik","oil","ois","oiu","ojd","oju","okh","oki","okm","olc","omb","ome","onr","ooh","oom","oon","oop","oos","opa","ope","opk","ops","oqo","oqx","oqz","orh","orn","osg","osh","ost","ota","otk","otu","ouf","oum","ovb","own","owy","oxf","oxn","oxr","oxx","oxz","oye","oyf","oyn","pac","pag","pal","pap","par","pas","pax","pbe","pbg","pcx","pdw","pej","peo","pes","pet","pew","pey","pff","pgd","pgo","pgp","pgz","phd","phj","phl","phy","pic","pir","pit","pjd","pjo","pjz","pla","pls","plu","pmr","pna","pnc","pnh","pnk","pnn","poc","pog","poj","pol","pov","poy","ppi","ppp","ppr","ppw","pqa","pql","pri","prn","prr","pss","pte","ptg","ptm","ptu","pub","pus","pvf","pvl","pvo","pvz","pwl","pwv","pxi","pya","pzg","pzt","qab","qbm","qbt","qdp","qdr","qeu","qgc","qge","qgg","qgx","qhe","qje","qjk","qkt","qlq","qmw","qng","qnq","qod","qol","qpb","qqg","qql","qqr","qqz","qra","qre","qrl","qro","qsa","qsr","qta","qtf","qui","quj","qut","qvw","qwf","qwl","qwm","qwx","qxr","qye","qyo","qyw","qyy","qzj","qzs","raa","rak","rat","rca","rcc","rcd","rci","rcp","rdb","rdk","rdt","rdv","rel","rff","rfh","rfl","rgd","rhe","rht","rhy","rid","rie","rif","rim","riv","rjg","rjy","rkg","rkt","rkw","rlg","rlr","rlz","rmv","rnv","rov","roy","rpa","rpz","rqe","rqj","rqx","rre","rrf","rrl","rsl","rth","rts","rty","rut","ruu","ruy","rvc","rvp","rvv","rwe","rwj","rwr","rwx","rxb","rzu","saj","sam","sap","saw","sca","scb","scd","sdp","sdw","seg","sev","sgd","sgp","shd","shi","shn","shr","shx","sid","sir","sjj","sjp","sli","slr","sly","sme","smg","smo","snk","sns","snz","soa","soo","sos","sow","soy","spf","spl","spm","spr","spy","spz","sqf","squ","srn","srp","ssd","ssn","sso","sss","ssx","stc","stf","str","stx","sue","suh","sum","sve","svg","svy","swr","sww","swz","sxr","sxx"))
df_stopwords = removeWords(df_stopwords, c("sxy","sxz","syi","syt","szu","taf","taj","tar","tay","tbh","tbq","tbt","tcc","tcn","tco","tcp","tcq","tdp","teg","tei","teo","tfa","tfe","tfk","tfp","tfq","thd","tho","thr","tiy","tjc","tjf","tjj","tka","tkc","tko","tkw","tkz","tlb","tlf","tlh","tlk","tlz","tmd","tmj","tnw","tob","tog","toz","tpr","tqh","tqj","tqm","tqp","tqt","try","tsm","ttt","tuw","tvg","twh","twi","twl","txb","txj","tyg","tyq","tys","tza","tzd","uaa","ube","ubp","ubs","ucf","ucv","udc","udi","udq","udx","ueu","ufn","ugb","ugp","uhg","uhm","uhp","uia","ujg","ujo","ukd","ukg","ukk","ule","ull","ulz","umx","una","uno","unt","uof","uov","uow","upc","upn","ups","uqq","uqv","ure","urq","urw","urz","usc","use","usz","utt","utu","uuf","uum","uun","uvq","uvr","uvs","uwd","uwq","uxk","uxm","uxz","uze","uzj","uzu","vau","vax","vbi","vda","vds","ved","vee","vel","ven","veo","vet","vfs","vhc","vhh","vic","vih","viq","vjq","vkj","vld","vlz","vmo","vmu","vmz","vna","vnq","voa","vom","vpa","vpe","vpn","vqb","vqk","vqw","vre","vri","vrr","vrs","vsv","vsw","vtq","vtr","vtw","vud","vun","vwo","vxh","vxi","vzb","vzm","wab","wae","wak","wal","wan","was","wav","wax","way","wbi","wbj","wbr","wbw","wco","wcq","wdd","wdn","wdw","wer","wev","wfp","wga","wgh","wgx","whc","whe","wht","whu","whx","why","wil","wio","wja","wkl","wkv","wll","wlt","wmm","wow","wox","wpd","wpr","wqm","wqv","wri","wrl","wse","wsf","wsh","wsu","wtt","wum","wuy","wva","wvn","wvt","wvw","wxb","wxy","wzi","wzm","xav","xbp","xbq","xbs","xcq","xdh","xdw","xeb","xel","xfa","xfn","xfs","xfw","xgb","xhi","xhl","xhm","xhs","xif","xij","xim","xiq","xiw","xix","xjk","xjt","xke","xma","xmi","xnr","xnz","xoq","xoz","xpc","xpd","xpg","xpn","xqr","xro","xrt","xrw","xsw","xto","xtu","xus","xut","xvl","xvr","xwb","xxz","xyx","xzq","xzy","yac","yah","yan","yay","ybu","ycn","ycp","ydh","ydk","yeb","yee","yej","yey","yft","yfu","yfz","yga","yhf","yhn","yhz","yif","yih","yin","yjk","yjm","ykm","ykr","yla","yma","ymg","ymi","ymk","ymt","ync","yof","ypo","ysf","yss","ytn","ytt","yty","yun","yux","yvr","yvu","yvx","yws","yxf","yxv","yyy","yzp","zac","zah","zam","zbb","zbl","zcb","zcd","zcr","zcv","zdj","zdu","zek","zen","zes","zfu","zfx","zgt","zhb","zhd","zhy","zik","zip","zix","zkn","zlj","zln","zlp","zmh","zmx","zmz","zne","zni","znn","zoe","zol","zop","zor","zou","zpa","zqc","zqe","zro","zsg","ztt","zuq","zux","zvc","zvv","zvw","zwa","zwq","zxh","zxn","zyn","zzg","zzo","zzz","“n","aj","aw","ba","bf","bn","bo","bs","bv","bx","cg","ck","ct","cz","db","dv","dy","ef","ey","fo","fq","fs","fx","fz","ge","gk","gm","gu","gy","he","hf","hq","hs","hu","hy","id","ik","iq","iv","ix","iy","jb","je","ju","jv","jw","jz","kc","kf","kr","kv","kw","kz","lf","lg","lh","lq","lz","md","mf","mj","nm","nt","nw","od","oj","ol","oy","oz","pb","pu","pw","qa","qf","qg","qi","qw","qy","rf","ro","rr","ru","sd","sg","sj","sr","st","sw","tc","tf","tk","uc","un","uo","uv","uw","vd","vp","vt","vx","wh","wj","wk","wn","wt","wu","xb","xe","xh","xi","xl","xr","xw","xx","yb","yl","ym","yv","zc","zf","zk","zs","zu","zw","“to","aau","acc","ace","agt","amr","aqi","ash","atf","aue","avc","axp","bah","bal","bao","bbb","bbs","big","bil","bis","bol","bot","btw","cat","cod","cof","cre","ctt","cun","daj","ddd","did","die","doc","dom","doo","dql","dst","dum","duo","dxa","eae","eis","end","epi","euf","eyz","fac","ffo","fia","fla","flu","flv","foe","fpr","fps","fuj","fuz","glr","god","gon","got","gou","grz","hey","hfm","hip","hjv","hop","how","hrs","hum","iba","ida","iev","ijn","ikb","inl","ipd","ise","jay","jdb","jel","jem","jiu","jvn","kct","kgs","kid","kkj","lan","lek","lhr","lie","liz","loo","lsd","maa","mdu","med","meg","mid","moa","mol","mor","mow","mru","mwu","nay","nct","ney","ngn","nqh","nyt","oia","oli","opz","ora","osx","out","pae","pam","pan","pge","piu","poh","psa","pse","qar","qcw","qur","qxd","ray","rbd","rin","riu","sab","sax","sbt","see","set","sex","sky"))
df_stopwords = removeWords(df_stopwords, c("skz","sst","sus","szl","tda","tdm","tdo","tnt","toa","too","tpb","tsc","tst","tta","ttk","tvi","twd","two","uag","ued","ueq","ugh","uhw","umu","une","vit","vlh","voc","vxr","vys","vzs","war","wei","win","wyr","xau","xgq","xie","yag","ycy","yvg","zqj","ac","ad","ag","an","ax","ay","az","bj","bq","bw","cc","cf","cv","cw","di","dl","dt","dw","eg","en","ev","fh","fj","fp","fv","gd","gi","gn","gr","gs","gx","hg","hn","hv","ih","ij","il","jc","jd","jl","jq","js","kb","kj","lb","lj","lk","ll","lm","ln","lr","lx","mp","mr","ms","mu","nb","nr","ns","nu","oe","om","oo","ov","pi","pn","pp","pr","py","qe","qo","qz","rb","rd","rh","rm","rq","sh","sk","sn","sq","sx","sy","tg","tn","ts","tx","ty","ua","uj","ul","uq","ur","vb","vk","vn","vq","vr","vu","wf","wm","wo","wq","xc","xs","xt","xv","xy","xz","yc","yi","yk","yn","yo","yr","yu","yy","yz","za","zd","zg","zp","zq","“vc","adc","ame","ari","ave","bgl","bia","bjs","bnt","bro","cis","cix","dlc","dpp","elw","emo","exo","fei","get","gta","hot","jim","kim","kpx","leu","lrt","mad","mel","meo","mia","mrm","nba","oje","omg","oms","ons","ooc","ore","our","oxi","pib","poa","poe","psg","put","qie","rlx","run","scr","sob","son","tae","tas","ten","usp","veu","who","wii","woo","xom","xxi","yjx","aa","ak","al"))
df_stopwords = removeWords(df_stopwords, c("am","ap","bh","bu","ci","cj","ea","ek","er","et","fg","fm","gb","gf","gq","gz","hp","ib","ie","if","ig","ip","iz","jg","jh","ji","ke","kl","kn","kp","ks","kx","ls","mb","mg","mk","ny","ot","ow","ph","pk","pt","qh","qr","qu","rc","rg","rk","rw","rx","ry","rz","sf","sv","sz","th","tq","tr","tz","ug","uk","uu","uy","wa","wb","wp","ws","wv","ww","xn","ya","yd","yj","yq","zb","zl","zr","zy","“ah","age","alo","ban","ces","coe","daq","eba","elo","fld","fzd","gas","hmm","isa","its","koe","lei","leo","net","nfl","ngc","now","oii","oxe","qnt","qse","quw","red","roi","rue","sad","sul","sun","tim","tnc","tvd","unf","web","wue","zoa","bc","bi","bt","cp","du","ew","fn","gh","gw","hi","hl","ic","ii","io","iw","jn","jr","jy","ki","ld","mh","mq","mx","ng","nj","or","pg","pv","qb","qk","qn","qs","qt","re","rv","sb","sl","tm","vf","vg","vj","vm","vs","wc","wi","xu","ye","zt","“ai","bob","cap","con","cut","don","fan","hxh","lar","lia","los","lua","mau","mts","obs","obv","ovo","pao","rey","slk","sto","tlg","wpp","wtf","bk","by","cd","ch","cn","cs","df","dn","dp","ds","eq","ez","ff","fl","fy","go","gv","hc","im","jf","ko","lp","lu","ly","mi","mz","nc","nh","nv","pl","qp","qv","qx","rj","sa","sm","tp","ui","vh","vy","vz","wd","wx","xa","xd","xj","xk","xo","yg","aii","ain","ami","ata","der","fzr","grt","hbo","hpv","las","lil","pow","rap","rei","she","tio","tom","uai","vlw","at","bl","cb","cl","cr","cx","dd","ed","fb","fd","jj","jk","ka","lw","mc","og","ox","rp","rt","uf","vv","wl","yh","yt","ato","ben","bio","cha","del","dog","hiv","iam","jao","mat","msg","not","ab","bb","dj","dr","dx","ml","mm","mv","ra","tw","uh","vw","and","blz","dez","fix","luz","mar","nss","ola","slc","smp","tds","uso","be","es","fi","ga","gp","jo","jp","km","mn","op","pa","pd","qq","sp","up","us","all","are","att","bar","boy","edu","ein","ent","neh","tmb","ae","it","qd","su","we","ze","aff","cor","ctz","day","eps","ido","mpn","qdo","rio","hr","ma","ti","adm","crl","dei","ead","off","zap","br","dc","fr","ni","pm","aaa","ana","doq","dou","msc","old","rpg","tou","el","hm","pe","ps","ri","rs","bad","fic","som","usa","vim","fc","lo","pc","eua","ira","sdd","toc","ca","ft","ss","ama","avo","fav","min","pop","uau","you","af","dm","ei","fe","my","qm","va","app","krl","lol","via","vir","bla","po","ali","cai","doi","fds","man","t.co","https","n","pq","q","pra","é"))
df_stopwords = removeWords(df_stopwords, c("t.co","r","s", "y", "o", "k", "a", "e", "i", "u", "da", "do", "de"))

texto=chartr("áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ", "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC", df_stopwords)

texto = removeWords (texto, c ("gt","it","aos","mas","vcs","ao","as","os","so","ne","se","t","co","a","e","i","o","u","fui","foi","me","da","de","pra","ela", "ele", "uma","um", "oi","eu", "vc", "que", "me", "em", "com", "pq", "to","la", "por", "p", "no", "na", "vai", "vou", "foi", "ou","ai"))

tweets_textos$texto_limpo = texto 
tweets_textos <- tweets_textos[,c(1,3)]


#Cria a função para calcular o score dos tweets
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Subir o arquivo com termos positivos
pos <- scan('termos_pos.csv', what='character', comment.char=';') #folder with positive dictionary

# Subir o arquivo com termos negativos
neg <- scan('termos_neg.csv', what='character', comment.char=';') #folder with negative dictionary

pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')
Dataset <- tweets_textos
Dataset$texto_limpo <- as.factor(Dataset$texto_limpo)
scores <- score.sentiment(Dataset$texto_limpo, pos.words, neg.words, .progress='text')

# Cria uma coluna com as polaridades a partir do score
stat <- scores
stat <- mutate(stat, polaridade=ifelse(stat$score > 0, 'Positivo', ifelse(stat$score < 0, 'Negativo', 'Neutro')))

# Gráfico de Polaridade
pol_sum <- stat %>% select(polaridade) %>% count(polaridade)

ggplot(pol_sum,aes(x = polaridade, y = n, fill = polaridade)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,500000,100000), limits = c(0,500000), labels = label_number(), name = "Total de Tweets") +
  scale_x_discrete(name = "Polaridade") +
  scale_fill_manual(values = c("#A92626", "#366392", "#5B882C")) +
  theme_bw() +
  theme(legend.title = element_blank(),  
        legend.position = "bottom",
        text = element_text(size = 16)
  ) 

require(scales)


#Evolução da polaridade por dia

#É preciso criar uma tabela com 4 colunas,
#uma com a data e as outras três com as quantidades de tweets seguindo as polaridades
polaridade2 <-read.csv("data_polaridade0909.csv", sep =";", header = T)
polaridade2$created <- as.Date(polaridade2$created)

gg <- ggplot(polaridade2, aes(created, positive)) +
  geom_line(aes(colour = "#366392"), size = 1) +
  geom_line(aes(y = negative, colour = "#A92626"), size = 1) +
  geom_line(aes(y = neutral, colour = "#5B882C"), size = 1) +
  labs(color = "Polaridade", x = "Data", y = "Total de tweets por polaridade") +
  scale_x_date(date_labels = "%d/%b/%Y", date_breaks = "3 days") +
  scale_y_continuous(n.breaks = 10) +
  scale_color_identity(
    name = "Polaridade",
    breaks = c("#366392", "#A92626", "#5B882C"),
    labels = c("Neutro", "Negativo", "Positivo"),
    guide = "legend"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 16)
  )  
gg


#NUVEM DE PALAVRAS

# Visualiza os tweets  por polaridade  
stat <- stat%>%
  group_by(polaridade) %>%
  summarise(pasted=paste(text, collapse=" "))


# Criando o corpus
corpus = Corpus(VectorSource(stat$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = stat$polaridade

tdm_df <- as.data.frame(tdm)
tdm_df <- tdm_df %>% mutate(name = row.names(.)) 

tdm_mx <- as.matrix(tdm_df[-4])
row.names(tdm_mx) <- tdm_df$name


comparison.cloud(tdm_mx, colors = c("#A92626", "#5B882C", "#366392"), max.words = 200, 
                 scale = c(4,1), random.order = FALSE, title.size = 1.5)


# GRÁFICO TWEETS POR HORA
tweets_textos3 <-read.csv("tweets_porhora.csv", sep =",", header = T)
tweets_textos3 <- df_tweets[,c(3,5)]


tweets_textos3$date <- day(tweets_textos3$created_at)
tweets_textos3$hour <- hour(tweets_textos3$created_at)
library(ggplot2)

dev.off()
ghour <- ggplot(tweets_textos3, aes(x = hour)) +
  geom_density() +
  theme_bw() +
  scale_x_continuous(n.breaks = 13) +
  scale_y_continuous(n.breaks = 8) +
  labs(x = "Hora", y = "Tweets")

ghour


####################################
# Análise de Correlação

# Verificando correlações entre palavras

install.packages("pairwise")
install.packages("PairwiseD")


library(dplyr)
library(tidyr)
library(magrittr)
library(tidytext)
library(pairwise)
library(PairwiseD)
library(widyr)
library(igraph)
library(ggplot2)
library(tidygraph)
library(ggraph)


# Criar id unico para cada tweet
tweets_textos %<>% mutate(tweet_id = row_number())

# Criar uma linha para cada palavra de um tweet
df_tweets_words <- tweets_textos %>% unnest_tokens(term, texto_limpo)

correlacao <- df_tweets_words %>%
  group_by(term) %>%
  filter(n() > 100) %>%
  pairwise_cor(term, tweet_id, sort = TRUE)

correlacao

correlacao %>%
  filter(correlation > 0.50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') + 
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) + 
  geom_node_point(color = 'lightblue', size = 5) + 
  geom_node_text(aes(label = name), repel = TRUE) + 
  theme_graph() +
  labs(title = "Correlações de Palavras no Twitter")

####
#MÉTRICAS
####

install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(forcats)
library(janeaustenr)

#1.contagem tweets
contagem = nrow(df_tweets)
contagem

#2.tweets por polaridade
freq <-stat %>%
  select(polaridade) %>%
  group_by(polaridade) %>%
  count(polaridade)

percent
tabela = data.frame(percent)
tabela

freq = tabela$n/sum(tabela$n)
as.data.frame(freq)


#3.Contagem de tweets por polaridade e por dia
stat2 <- stat
stat2$created <- Dataset$created_at
dia = wday(stat2$created)
stat2$dia = dia

stat2 = stat2 %>% 
  mutate(dia_semana = case_when(
    dia == 1 ~ "domingo",
    dia == 2 ~ "segunda",
    dia == 3 ~ "terça",
    dia == 4 ~ "quarta",
    dia == 5 ~ "quinta",
    dia == 6 ~ "sexta",
    dia == 7 ~"sábado"
  )) 


stat2$mix = paste(stat2$polaridade,stat2$dia,stat2$dia_semana)

d_pol = stat2 %>%
  select(mix) %>%
  group_by(mix) %>%
  count(mix)
data.frame(d_pol)

names(d_pol)[1]<-"dia_polaridade"
names(d_pol)[2]<-"qtde"

q = d_pol$qtde
mq<- matrix(q, nrow = 7, ncol = 3)
mq

row_names<- c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado")
col_names<- c("Negativo", "Neutro", "Positivo")
rownames(mq) <- row_names
colnames(mq) <- col_names
mq


#4.MÉDIA DE TWEETS POR DIA DA SEMANA
stat2$dd = paste(stat2$created, stat2$dia)
a=data.frame(data=unique(stat2$dd))
a1=data.frame(do.call("rbind", strsplit(as.character(a$data), " ", fixed = TRUE)))
conta_dia = a1 %>%
  select(X2) %>%
  group_by(X2) %>%
  count(X2)
conta_dia

m_dia = stat2 %>%
  select(dia) %>%
  group_by(dia) %>%
  count(dia)
m_dia

m_dia$ndia = conta_dia$n
data.frame(m_dia)

m_dia$media = m_dia$n/m_dia$ndia
m_dia

# Contagem de palavras que aparecem mais de 20 vezes nos tweets
palava <- tweets_texto %>%
  unnest_tokens(word, texto_limpo) %>%
  group_by(word) %>%
  filter(n() > 20) %>%
  ungroup()

word_count <- count(palavra, word, sort = TRUE)

#contagem 
minfreq_unigram<-1

token_delim <- " \\t\\r\\n.!?,;\"()"
unitoken <- ngram_tokenize(text,char = FALSE, ngmin=1, ngmax=1)
uni_word <- data.frame(table(unitoken))
sort_one <- uni_word[order(uni_word$Freq,decreasing=TRUE),]


