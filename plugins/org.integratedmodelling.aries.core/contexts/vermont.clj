;;; Copyright 2011 The ARIES Consortium (http://www.ariesonline.org)
;;;
;;; This file is part of ARIES.
;;;
;;; ARIES is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; ARIES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ARIES.  If not, see <http://www.gnu.org/licenses/>.

(ns core.contexts.vermont
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model])
  (:refer geospace :only [grid]))

(defcontext raven-ridge
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext raven-ridge-large
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-74.439038 45.188716, -71.877970 45.188716, -71.877970 43.346402, -74.439038 43.346402,-74.439038 45.188716))"))

(defcontext lye-brook
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.097615 43.167086, -72.938627 43.167086, -72.938627 43.051168, -73.097615 43.051168, -73.097615 43.167086))"))

(defcontext vt256
  ""
  (grid
   256
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext vt512
  ""
  (grid
   512
   "EPSG:4326 POLYGON((-73.437902 45.016731, -71.465281 45.016731, -71.465281 42.727110, -73.437902 42.727110, -73.437902 45.016731))"))

(defcontext vtcoverage
  ""
  (grid
   256
   "EPSG:4326 POLYGON ((-73.151 44.27 ,-73.144  44.27 ,-73.144  44.286 , -73.151 44.286 ,-73.151  44.27 ))"))

(defcontext vt-simple
  ""
  (grid
   512
   "EPSG:4326 POLYGON ((-71.46528176222033 45.0136832226448, -71.63336591213641 44.750048537748114, -71.57686650739787 44.50236705743422, -72.03287227690927 44.32052732132643, -72.0315363920979 44.07937899156674, -72.38050646075203 43.57382871186157, -72.55722886459505 42.853682769843914, -72.45842479251264 42.72711895351649, -73.27619328348301 42.745989373360246, -73.24148772974226 43.53280954970073, -73.43133354459462 43.58792612069299, -73.34293424084181 45.01072500108264, -71.46528176222033 45.0136832226448))"))

(defcontext vt-complex
  ""
  (grid
   "1 km"
   "EPSG:4326 POLYGON ((-71.46528176222033 45.0136832226448, -71.46754734040816 45.01154774735782, -71.47731480965479 45.0094010087147, -71.48119343729317 45.00228383070883, -71.4888133955303 45.00092527377905, -71.49493774284788 45.00451553669152, -71.49758916847394 45.00389543896693, -71.50376666304193 45.00813557443148, -71.50874186265676 45.007981709183674, -71.51344441105769 45.004821796752, -71.52842554283775 45.00048943091791, -71.53704837785794 44.99341501870659, -71.53332312573801 44.990773033892374, -71.53775022273565 44.989934763907065, -71.5414502962258 44.984016053000836, -71.53707861535065 44.984225657546496, -71.5372788754013 44.980783643375034, -71.5334291522383 44.98012652547861, -71.53129708829303 44.97502060481831, -71.52722552913569 44.972799312884426, -71.52546347015115 44.96961333030861, -71.52162355708376 44.968937989446026, -71.52366524280488 44.967307892152455, -71.52206707645249 44.965955840743966, -71.52401718478313 44.96463621714154, -71.523529781095 44.96371166319283, -71.51725670854057 44.96434004233665, -71.5153071228099 44.95845609131023, -71.51623861182217 44.9526149545661, -71.51949817181784 44.949482816926036, -71.51724046784857 44.948631027449004, -71.51811801712073 44.945003089752284, -71.51522806908999 44.94408643024802, -71.5155611811701 44.94248554795508, -71.51741237380044 44.93915684048482, -71.52022661741508 44.93869418893438, -71.52133986246264 44.94028544349099, -71.52175030096781 44.93793956899323, -71.51623242104621 44.93498026313316, -71.5161070163179 44.93157396586393, -71.51382400565625 44.92988329432294, -71.51554591273093 44.926785481395264, -71.50951381680017 44.92375635011069, -71.50394561505078 44.918472614372625, -71.50001801005988 44.91093501752871, -71.49345400495723 44.91148926635418, -71.49629870201535 44.909253900237076, -71.49491271412202 44.904944787797945, -71.49689934959336 44.90379943180413, -71.50083315332648 44.90481127807405, -71.50349076862886 44.901598443488524, -71.50758473644952 44.899598837128735, -71.50849990679828 44.89658471856564, -71.51415041255535 44.894362340310465, -71.51137454492344 44.88989400874155, -71.51516333217388 44.88730899046765, -71.51518719023602 44.88517647542928, -71.51963547376467 44.88434888366558, -71.52322589330278 44.8795078650644, -71.52777080078194 44.87844879198225, -71.5294348087246 44.87316065135375, -71.53359676761565 44.869902461423905, -71.5458263608389 44.86624117274518, -71.54980159349488 44.861986363835804, -71.5502727543123 44.860064526979144, -71.54799017891848 44.85500885518714, -71.55420904214246 44.85148797749424, -71.55608654587546 44.848024635334795, -71.55163850008888 44.841570676635065, -71.55123792868211 44.83746286361567, -71.55800598969657 44.83309413362529, -71.56264526785598 44.823466757037394, -71.5655218598837 44.82439994328102, -71.57368776987691 44.82061223553665, -71.5771441451107 44.81531483409233, -71.5772240317523 44.81338160881175, -71.56873478567235 44.8061594375383, -71.57289782302699 44.79973116402194, -71.57158802865504 44.79392159286895, -71.57326863864338 44.79092868870477, -71.57902681388185 44.78525021476891, -71.58339332153945 44.785300554285534, -71.59325140889497 44.78220171436766, -71.59606744980881 44.779377660228725, -71.59552717074746 44.77528352089289, -71.60114234588629 44.7730924155777, -71.60341094914108 44.769001395420595, -71.60699330405203 44.76663203964488, -71.61161198805665 44.765230111652144, -71.61370861612413 44.7611158556038, -71.61322511817167 44.75907562436495, -71.616422169504 44.7566949668324, -71.62971862289064 44.75383908993878, -71.63336591213641 44.750048537748114, -71.62818325360593 44.7473392560121, -71.62582994425321 44.74402842720299, -71.62688420927951 44.742349171631524, -71.6254604652026 44.736968634295884, -71.6260625177596 44.72801224812753, -71.62374025665943 44.72693436260903, -71.61759190773017 44.72874271833666, -71.61882430714354 44.72182942815278, -71.61294465974075 44.718990594591645, -71.60501789540395 44.708113911426565, -71.59996578768533 44.70549695408083, -71.59902209707094 44.70334502631962, -71.60130411706712 44.70061297020657, -71.60101608662403 44.698686693222804, -71.59403292475102 44.69581388531255, -71.59674842382098 44.6948148955866, -71.59856898128241 44.692033523738914, -71.59439726826841 44.683114057872814, -71.59704147402314 44.678188258224345, -71.58738710567152 44.674073687772115, -71.58179445108303 44.67327562916338, -71.58794921535609 44.670399504609, -71.58538620300865 44.664220071103394, -71.58812595037064 44.66133974884174, -71.5876553050246 44.659543354940176, -71.58270013258765 44.65558748960561, -71.57487676842042 44.655199421384516, -71.57551206965867 44.65345431553142, -71.58092892175799 44.65223014313098, -71.57553894213747 44.65002503394891, -71.56891013438388 44.6501231773485, -71.56625380031876 44.653428487388375, -71.56377718834892 44.6529034467465, -71.56254107957355 44.64750108401706, -71.55830069507724 44.64180039361737, -71.55957447197937 44.639768971462416, -71.56404758308464 44.639218971132536, -71.5640524160518 44.63724845490299, -71.55343642460868 44.629707809857756, -71.54972480034394 44.630594905605285, -71.54692452313225 44.629493463817745, -71.5521499482186 44.62735581273176, -71.55471956492467 44.62975623474489, -71.5570994668106 44.62944032832799, -71.55653490055649 44.6271912602278, -71.54821465437594 44.62521177510459, -71.54628000390946 44.62320860579164, -71.54748502170942 44.62219203087747, -71.55511052317497 44.62531133531732, -71.5566379372625 44.61761630209147, -71.55391911967004 44.60561518558748, -71.55586474920118 44.60381082763363, -71.56201745702874 44.60307769225064, -71.56186523014613 44.60079602926041, -71.55662037448481 44.59943992162985, -71.55046337259911 44.600703597506225, -71.55381381693536 44.598428764042566, -71.55488966746742 44.59320360738353, -71.54550630158751 44.59693495885206, -71.54732815553186 44.59142650818357, -71.54505811689121 44.59070110588129, -71.53814105402557 44.59408257973421, -71.5400333073829 44.58849582216904, -71.5354087697627 44.587139270733815, -71.53822037295049 44.582301659530415, -71.54618418013152 44.57796968584683, -71.54934780961896 44.57816435123812, -71.55208232824174 44.58111905130589, -71.55456690855392 44.58140125638299, -71.55347368964844 44.57699271551347, -71.54777226334407 44.57159982366824, -71.55081445626536 44.56927020986552, -71.5567723205411 44.57049380215436, -71.5583647035648 44.56962420054198, -71.55850115544743 44.56491967439771, -71.55987058859542 44.56396163924229, -71.56909199413977 44.562834958379376, -71.57655712607958 44.565094098208576, -71.58921566350824 44.565841722880194, -71.59259035135702 44.56456548490323, -71.59707542882231 44.55939771396934, -71.59811775517852 44.555935898284446, -71.59726589200993 44.55389360847733, -71.57663534951548 44.54239246744431, -71.57214749936794 44.53839655796488, -71.57317938324987 44.534178026368515, -71.57986216246628 44.52541276897271, -71.58675403059634 44.52236879022249, -71.59274780480013 44.52339488463954, -71.59399913822409 44.520875001222386, -71.59143582289212 44.517459348159875, -71.58618599690476 44.51459264324783, -71.58383184633166 44.509111236608284, -71.57739317413426 44.504804664422146, -71.57686650739787 44.50236705743422, -71.57995808668184 44.50153256045233, -71.58398704816042 44.502670884246385, -71.58536933796083 44.50569127606035, -71.58767314457404 44.50674193503828, -71.58834708992556 44.50500683090751, -71.58614492347638 44.503490828982684, -71.58692020372945 44.50135444919511, -71.5858480890799 44.499503691876015, -71.58968647483655 44.49843903137415, -71.59105968800154 44.50119848110855, -71.5935898554629 44.50122911906789, -71.59547411764304 44.49281456777822, -71.6000274707707 44.48588326616858, -71.60994173044746 44.484279054667866, -71.61763290945092 44.48593856560806, -71.6243493444234 44.47563117136102, -71.63118310276309 44.4739337763763, -71.63476916238494 44.47604725447803, -71.63412669115841 44.47901733549966, -71.62740197404321 44.4797995940444, -71.62471287571115 44.48168231062, -71.6258737100948 44.48369717795818, -71.6318325149037 44.48422499958687, -71.6344345405041 44.48083575840525, -71.6450470338636 44.475503511965854, -71.6480352014234 44.470658121944965, -71.63951569546342 44.46444770342414, -71.64453458606269 44.460679526426276, -71.65312010127329 44.460472626301474, -71.65655992834661 44.4557302427391, -71.65815862561418 44.4459650730515, -71.66159928348603 44.44024968413965, -71.6669574573687 44.43694008055964, -71.67856140171332 44.43574799487031, -71.6796129512659 44.43483391172291, -71.67677171252528 44.428872249420145, -71.67771740462078 44.42697386037183, -71.68277351546097 44.42795780080419, -71.68522038472304 44.42370761146525, -71.69468217746147 44.41986810516048, -71.7026005555681 44.41425475406406, -71.71520523156977 44.41013476364439, -71.72357098739667 44.41154538371186, -71.73527279190517 44.41026998733522, -71.7390748545515 44.407830309103716, -71.74260384174663 44.401602661266736, -71.74455264051457 44.40118097781417, -71.74885627274175 44.401708838074896, -71.75559737017477 44.40629388719225, -71.76229172531448 44.40705998652874, -71.77855315336299 44.399886710264106, -71.79005797464748 44.40033678224533, -71.7951948187303 44.39847319315822, -71.803283338046 44.39220060573497, -71.8030228592881 44.3898880098935, -71.8000205289407 44.386365384878296, -71.80116590548074 44.38384971865396, -71.80854535384879 44.384011625087034, -71.81461330303827 44.38130061626711, -71.81544015202284 44.3746881553677, -71.81226882977435 44.372088191991196, -71.8161974569721 44.36710249401464, -71.81248487680647 44.35682628406074, -71.81528843735406 44.35419452657814, -71.83228968332712 44.3504270326716, -71.852004754378 44.34116022293284, -71.86215278339908 44.3400977821799, -71.87088302816382 44.336727940306716, -71.89941125877054 44.34676135037659, -71.9077161978328 44.34816510584348, -71.91740661526372 44.34647440600147, -71.92529767884093 44.34180764073127, -71.92870389725307 44.338027671615194, -71.93539026679983 44.33578579668638, -71.94463122281148 44.337654437230036, -71.9652815887605 44.33653963543586, -71.98071956569008 44.33748023099855, -71.98464183995517 44.33616154073043, -71.98770219584739 44.3303992128295, -72.01005287561259 44.32193053596276, -72.01828030500968 44.320246403267966, -72.02940850173655 44.322372445103156, -72.03287227690927 44.32052732132643, -72.03364176273887 44.31734294488834, -72.03154194409848 44.31372276317227, -72.0327789473668 44.30236665218952, -72.03853464965378 44.29642141478734, -72.04380785286672 44.295405913829526, -72.04585854718547 44.29240717560412, -72.05377283715485 44.290244223641764, -72.05883954847586 44.286358904225814, -72.06845613128974 44.2712752823096, -72.06662612143317 44.26804102676005, -72.05823614670965 44.2693913480845, -72.05946380467502 44.267470132006515, -72.05865188413998 44.265504231004854, -72.06132969341701 44.263097107613476, -72.05982518851339 44.256604434897525, -72.04765066241515 44.23949976394529, -72.05360758944795 44.225060427911416, -72.0528999380371 44.2170749523551, -72.05488328544844 44.213254057205425, -72.05959538483862 44.210198048267905, -72.05773312200238 44.20605360233041, -72.05825684156439 44.20376254428888, -72.06023021443993 44.20001458790765, -72.06446374831411 44.19743154285287, -72.06611340917229 44.18998318869667, -72.0586842597396 44.181816274876354, -72.05193320061613 44.16672417382959, -72.0484058731058 44.16464317393454, -72.0469413997262 44.16158899749734, -72.0427741672865 44.16181197439184, -72.04386969353311 44.15815147608001, -72.03902545357779 44.15553562951866, -72.04279762582779 44.15027418939567, -72.04184683295756 44.1372393776162, -72.033257958779 44.13191971578621, -72.03695213394852 44.12531721466053, -72.04548256504711 44.12442446797111, -72.05243185061045 44.11945940708188, -72.053169025617 44.114477441812255, -72.05543643507679 44.11082155920714, -72.052535360672 44.10077508225375, -72.04841332782723 44.09683852497827, -72.043488267274 44.09673066121713, -72.04179544352156 44.10154890951323, -72.03959522210465 44.10332558605255, -72.03126586021831 44.100263196055174, -72.03073422324496 44.09732177235864, -72.03281913293006 44.090876454743515, -72.03674309062565 44.08888631510877, -72.04379355087414 44.08889281518676, -72.04500608043992 44.08711131399726, -72.03384418215181 44.08178695588356, -72.0315363920979 44.07937899156674, -72.03677166569952 44.076728118610106, -72.04348449001284 44.079363456682735, -72.05222711168625 44.07697522855324, -72.05270454102137 44.07523188894364, -72.04921100493512 44.072286824095464, -72.04839360957978 44.06947673185056, -72.05710260564044 44.058719679022104, -72.06802105878842 44.05751307367727, -72.06917522146881 44.05443488562573, -72.06269285693101 44.05187761953962, -72.0618514009356 44.0499628556906, -72.06911887677003 44.04800596201199, -72.07855290869442 44.04298837540087, -72.07927944534782 44.03962328402715, -72.07473058797797 44.03280420846357, -72.08129495313202 44.028473268197224, -72.0823872529984 44.02201612439562, -72.08604226335453 44.02150381386016, -72.09154049522571 44.02445127232447, -72.09520863991581 44.02190917353791, -72.09549701481014 44.01710247195307, -72.0904780358049 44.011635872141014, -72.09043453018218 44.00969437171193, -72.09289896515499 44.00923156795417, -72.09297637015075 44.012847136375456, -72.09670432059653 44.015166297016634, -72.10249901846765 44.014800404644184, -72.10590901066446 44.01221115833175, -72.10360159170442 44.00280182539735, -72.1096287837358 43.99993341491927, -72.11688301884573 43.993878313751985, -72.11677988020733 43.99186911536413, -72.11271411916464 43.988039305457285, -72.1117166884672 43.98519643311745, -72.1145804690082 43.96931535999322, -72.11378921675869 43.967495623094855, -72.11114198722609 43.96702313081889, -72.10575077539619 43.969967546831334, -72.09233635488582 43.966878383647, -72.09108904445426 43.96545089315789, -72.10025497223594 43.963007439582846, -72.100587875128 43.96016829411121, -72.09898892003541 43.95731050251813, -72.1058133886967 43.949662305522104, -72.11855561088052 43.94585792030897, -72.11686333049369 43.93367680728233, -72.11802768274707 43.92324841492915, -72.12089733223851 43.91916551276097, -72.15366392666473 43.89987102579166, -72.15852013135262 43.893035534421884, -72.15991471421526 43.887587251753686, -72.17011237092382 43.88632657658148, -72.17289894137974 43.88423749182523, -72.17381982649805 43.87967241478773, -72.16803419097691 43.86910275762405, -72.18370544733683 43.865061907659474, -72.18818008541007 43.85550672942878, -72.18298790128858 43.844517370130674, -72.18248614132747 43.83318635238621, -72.18889544003527 43.821827349058736, -72.18372927459285 43.81142216413871, -72.18375544430258 43.80764021521762, -72.18551088166765 43.80423835757184, -72.19125267785329 43.80027406477199, -72.19657360203583 43.790438923567905, -72.20543899305916 43.784994274430055, -72.20580679357427 43.78196653040535, -72.20401176995139 43.7794310247962, -72.20622655954749 43.77044794764654, -72.21797077821651 43.766007655817255, -72.2310060950856 43.74855449089604, -72.2351536611549 43.74587838954246, -72.24666259838983 43.742681261417964, -72.2643478378051 43.733972717423796, -72.27100711934982 43.73404673697083, -72.27507737960373 43.727734379728545, -72.2820529885188 43.72349229011019, -72.29239947111259 43.71099593003008, -72.29949135144076 43.70669257962013, -72.30537346035648 43.69134115306405, -72.30601609541284 43.68353591813893, -72.3026647390298 43.6795042441624, -72.30136068232915 43.67514733795153, -72.30298691404495 43.66893522612928, -72.31366420042296 43.657104256073495, -72.31517431117588 43.649419053559726, -72.31445777512448 43.642234272974314, -72.32929535305658 43.63527088041141, -72.32970290926379 43.63317781033689, -72.3274503990146 43.63078750752421, -72.329431464055 43.626580798547536, -72.33257852876896 43.624827410249125, -72.33480058791835 43.61824032653293, -72.33351381345598 43.61088927481445, -72.32837493724892 43.606789800270164, -72.3286024094505 43.601231213128855, -72.3335669461566 43.59921155413714, -72.34962800313039 43.58812115016922, -72.37190415533128 43.580619666609635, -72.38050646075203 43.57382871186157, -72.37999606423294 43.56707830514766, -72.38323853437967 43.5630384500087, -72.38095013412256 43.54064886452406, -72.38811502990319 43.52998617594075, -72.39543372140088 43.52642511610742, -72.39866978928353 43.51298136061102, -72.39691795590213 43.508817730493384, -72.38499254336831 43.50069383434329, -72.38021546804956 43.4925979973701, -72.38167916873253 43.47866689584441, -72.38336388830047 43.47476216758197, -72.39280454526276 43.46588366261103, -72.39033094157305 43.45126823309746, -72.3959093444215 43.437223525342155, -72.39591211615034 43.42904798439517, -72.40020930283097 43.415014530103164, -72.40379742066621 43.39206605197776, -72.4136344656031 43.38333139906699, -72.4161609929352 43.37658867153803, -72.41500175897966 43.3652996492277, -72.41340900788184 43.362938510391565, -72.40379516248095 43.35789886367921, -72.39278786202253 43.357947943589856, -72.38995276468157 43.35604940827599, -72.40073663879954 43.3460441175449, -72.41044587659641 43.331400394327936, -72.40586242804471 43.324059849625975, -72.39732679383093 43.31695938814926, -72.39542353565913 43.31327774705076, -72.40289818972369 43.302411996294005, -72.40459486741761 43.29229528612436, -72.41580838307843 43.27065220150529, -72.42183501232849 43.263266067184055, -72.43571039332797 43.25777377078125, -72.4386234595421 43.2524960446299, -72.43953378472428 43.244510110359464, -72.43423241651008 43.231404605809146, -72.44122050839502 43.217408263269725, -72.43816634647037 43.20328333235834, -72.43944008505942 43.20073155044245, -72.45042767521743 43.192908683483864, -72.44900563742927 43.18827993185372, -72.44421182969985 43.182634372784534, -72.44397860984957 43.179538184112296, -72.45262462383783 43.17233069189455, -72.45194573082381 43.1535126594539, -72.45721031834583 43.147803070673994, -72.45218941780044 43.13852269868851, -72.44104892754959 43.136737796509294, -72.44182476561649 43.128829296188535, -72.43284105027521 43.11941813831395, -72.43384080332568 43.11063664066264, -72.44181532231607 43.105852370427, -72.4435247594409 43.10231521940878, -72.4342661127866 43.08391726489105, -72.435603608225 43.08018565796423, -72.43934496702153 43.076499350504406, -72.45148755834956 43.067719304827506, -72.45619008546458 43.06196910932743, -72.46318751101748 43.05801540934113, -72.46791304241182 43.052414608439776, -72.46630783438108 43.04735085242538, -72.46044380957328 43.04130970752825, -72.46316225694991 43.028212878574436, -72.46261684555404 43.025410720997556, -72.45744167601205 43.01775911318851, -72.44370909196196 43.00990794366861, -72.44347008375534 43.0071674985262, -72.44539020508745 43.0036399940006, -72.44996655123231 43.00024993436171, -72.4587411177231 43.00101849733157, -72.46261852932166 42.99734979010965, -72.46556115322767 42.98952839994292, -72.46172596134397 42.98314677248839, -72.47331069649044 42.972239281033474, -72.47578994399876 42.97149006759245, -72.48177752620961 42.973924288589274, -72.49240761535243 42.967635978885575, -72.51849045232255 42.96310553753437, -72.53227277347021 42.95479527948797, -72.53445243188125 42.94941970728802, -72.52817632243848 42.94508836214104, -72.52719703360434 42.94229494917732, -72.52689278279473 42.92762118042261, -72.52438818550206 42.91925309480556, -72.5245703725481 42.91538816195324, -72.53010171600224 42.91169622231817, -72.53146877190491 42.90703968358552, -72.53135879880445 42.89857219530096, -72.53291239594907 42.89593481096787, -72.54056380614851 42.889449848921195, -72.55269196980375 42.884815455957025, -72.55532044607472 42.87413847209378, -72.55412770569278 42.860804889078004, -72.55722886459505 42.853682769843914, -72.54832443944905 42.84160152952474, -72.54648488148771 42.82416131026572, -72.54235071844758 42.815897999952334, -72.54172143280563 42.80746433522556, -72.52426587672218 42.79470921189767, -72.50897217309829 42.780991710159675, -72.50936845652676 42.77505133165661, -72.51614173321742 42.769412157153724, -72.51529507490883 42.76581482278298, -72.51107345928109 42.76398705556941, -72.50748769084655 42.76468870636556, -72.50118108908615 42.767321543050244, -72.49662225464733 42.77361446923062, -72.49023891682099 42.77218374838822, -72.48521001460247 42.76594182061793, -72.47684765221463 42.760462605720804, -72.47394781173716 42.747445971854205, -72.46355367968704 42.736602754232386, -72.45842479251264 42.72711895351649, -72.8463592235145 42.7373779099783, -73.27619328348301 42.745989373360246, -73.2908893530832 42.801964080077475, -73.28351950157597 42.81405867715845, -73.28697006168714 42.82024992811326, -73.28530927234047 42.83407222815325, -73.2842025843582 42.83494256573306, -73.27867898716329 42.83348106960438, -73.27820600771462 42.837399528313306, -73.27082294536076 43.01589141250231, -73.2565656320318 43.2577107515836, -73.24678095046426 43.51558536705597, -73.24810202858173 43.51595936129583, -73.24676556901355 43.51863049026132, -73.2468794715832 43.525768216570484, -73.24227480285813 43.52886491400466, -73.24329987554651 43.53043872845096, -73.24148772974226 43.53280954970073, -73.24477074404454 43.53924007352007, -73.24650631279937 43.54005404862531, -73.24572662675334 43.54169559365996, -73.25062525438328 43.54403869764586, -73.24905220994665 43.545521563722204, -73.25063678771717 43.55022314508316, -73.24879441107072 43.55156546996011, -73.24952261431338 43.5542042619706, -73.25280543300417 43.55568305175111, -73.25873617087034 43.56503518946492, -73.26409334693301 43.567524165448944, -73.26463949768906 43.56929548591303, -73.26819634480887 43.56924463827929, -73.26802366219107 43.5722844016444, -73.27539092504021 43.57311405501158, -73.27717152492714 43.57463562116285, -73.28108249561907 43.57412472070554, -73.28184018233627 43.57814562545034, -73.2851544508473 43.5794630483008, -73.2897937495298 43.57787844417741, -73.29579360819554 43.578009562660995, -73.2973323593343 43.57936517042412, -73.29242503838108 43.58463688495816, -73.29712467657097 43.587609502945476, -73.29322578033424 43.592782310331785, -73.29272364394787 43.59759574577837, -73.29101666455394 43.59923311857007, -73.2928622922366 43.60167476633214, -73.29250973276983 43.60352867893832, -73.29826637881837 43.60993302479469, -73.30019341258134 43.611132860243266, -73.30241955382371 43.610095664631885, -73.30076205115537 43.613610399028744, -73.30164724182619 43.61426755369845, -73.3006391669607 43.61812056796101, -73.30318930195956 43.620351885873035, -73.30182951402857 43.62436678113533, -73.306083166541 43.628088299586146, -73.31066875059885 43.624013622330374, -73.31814678569617 43.62853625053998, -73.32939471832798 43.625774672920684, -73.33437538770552 43.62741437101059, -73.34138335214818 43.624589865214794, -73.34328000441732 43.62650826566436, -73.34926968412499 43.62152528626331, -73.35186630616316 43.62276958480147, -73.3538396985946 43.6259671485843, -73.36613274055809 43.62337055996377, -73.37189140674278 43.624577889234224, -73.37248659124648 43.622038729722526, -73.37007644201132 43.61927594905189, -73.37648574868506 43.61256919391438, -73.37486973827731 43.61063749187947, -73.37676787529699 43.60934123594265, -73.37382630594777 43.60744397931042, -73.3728325000139 43.604890753699785, -73.37780256576029 43.600396488109915, -73.37726935037738 43.59857080639146, -73.38332541673174 43.597548482908834, -73.38227235259316 43.59159096283326, -73.38721867436136 43.590760608674174, -73.3868405251212 43.58862134648218, -73.38239380917932 43.58618151843533, -73.38558465849951 43.58317495837346, -73.38292599385132 43.579021834229415, -73.38397453161447 43.57552599384706, -73.3909945396023 43.574031360485726, -73.39181944475195 43.569972834768045, -73.39571279493587 43.569135919598516, -73.39619631608764 43.56789181207843, -73.40669756733337 43.57151348835338, -73.4165348283198 43.57736750033969, -73.42094463025022 43.581732479208874, -73.42794266739395 43.58350377741598, -73.43133354459462 43.58792612069299, -73.4219880038581 43.60286104712166, -73.42419117899522 43.611256208303125, -73.41787242678488 43.62091077243109, -73.41910137773776 43.62421408143142, -73.42809850659202 43.63515165433355, -73.42676526413824 43.642065952849066, -73.42408181201723 43.645540360796616, -73.41920127894043 43.647661491816834, -73.41544002345731 43.65273208434205, -73.41436665967122 43.65871337340996, -73.40854528985115 43.66878019140069, -73.40442437072588 43.679896145473755, -73.40345841439613 43.68469034654037, -73.40487446285722 43.6900135734475, -73.3954715761168 43.696913730507745, -73.38713566189392 43.7100411973142, -73.37045199025644 43.72506542612404, -73.36954368681263 43.74375291333061, -73.35076201123336 43.77139523572116, -73.35536616072567 43.77793477588989, -73.35815693121211 43.786482990116646, -73.37679476423455 43.79916526983159, -73.38072507348102 43.81082641927621, -73.38956245428486 43.81704660478419, -73.39252211675466 43.821347354031474, -73.39224815271739 43.824837083373716, -73.3881899275748 43.83266651204472, -73.37666150135162 43.83944583431207, -73.37258885869353 43.845365990575026, -73.37363090283519 43.847532352864995, -73.3802534517889 43.85193044132369, -73.3823083777304 43.8560654963949, -73.3741516940747 43.87624317694777, -73.38754974088388 43.89699689873081, -73.39657437738295 43.903939333629815, -73.40206459993604 43.919249601157475, -73.408552057696 43.93190015136446, -73.40855362771742 43.93732002402374, -73.40565031181964 43.94807491524601, -73.40572834056861 43.95593593760168, -73.40707273225469 43.96819873179373, -73.41204793517932 43.977613779131175, -73.41252568229979 43.982220687531715, -73.406008942601 44.01142760116345, -73.40764938714501 44.02111927868636, -73.41251282657866 44.028353470572384, -73.4228862451638 44.03255763437076, -73.42689656204287 44.036700241773744, -73.43672816523559 44.04264266929364, -73.43790289997294 44.045134074717694, -73.43112537425576 44.06777427670899, -73.42971736395813 44.07859474422955, -73.41521941110283 44.10135323678446, -73.41393684268472 44.10785333503171, -73.41151203974695 44.111961237271345, -73.41201774504604 44.11963871069567, -73.416139714882 44.13236394196964, -73.41197679618719 44.13778913998122, -73.40304761023779 44.144992642599966, -73.40287745217003 44.1504909847651, -73.40029209406511 44.1543715601795, -73.39885776924183 44.161922920624804, -73.39581041568468 44.166274118295235, -73.39717528313508 44.17384665689956, -73.38982659756843 44.181764573827415, -73.3906277663314 44.19107584072554, -73.38458500394019 44.193205460089665, -73.38193635472498 44.19774208497817, -73.37480468757586 44.20025119196834, -73.3708635759524 44.20453446932646, -73.3622643500739 44.20852670898482, -73.36222527215101 44.21246894644377, -73.35622905816298 44.21896415962445, -73.35537533538752 44.22316529705727, -73.35138398393656 44.22598875814415, -73.3504782156282 44.229900447171744, -73.34283478446444 44.234453507982124, -73.34337921293617 44.2381980088213, -73.33640167906282 44.2397891830488, -73.33038002443595 44.244320337271745, -73.32371976221573 44.244168043886994, -73.32306874926972 44.24790397043406, -73.31904764193237 44.25029370438239, -73.31729528914339 44.253383200088955, -73.31712126093728 44.26300522897867, -73.31328133100551 44.264147707374555, -73.31121259231251 44.27098425384668, -73.31126566995715 44.27788310285897, -73.31939303482388 44.29290940074757, -73.32185644280125 44.30049081140093, -73.32455817875065 44.31683003949819, -73.32444032090382 44.33626824382801, -73.33470164745752 44.355897483739085, -73.33493234634844 44.36946526706589, -73.33332843667176 44.37374910057751, -73.31805364210818 44.387693810992296, -73.31216978862862 44.40594006822351, -73.29599934121285 44.428743489987816, -73.29340075483431 44.43750731200786, -73.29400989221251 44.443818039339064, -73.29959486755483 44.4546853930813, -73.29821690335358 44.465622927217694, -73.29963480718312 44.47745711849027, -73.30346021500675 44.484463343870885, -73.30790140206588 44.499668778765695, -73.31941704281982 44.51363374134847, -73.31977822211923 44.52357880959765, -73.32760579073312 44.52702680534715, -73.33215837407697 44.53081297013335, -73.33803669382417 44.54039912982168, -73.33939212916663 44.54839302056659, -73.35424707139516 44.557279243564125, -73.37457200596559 44.57568982881806, -73.375931329668 44.58268608452924, -73.38171468471565 44.58960954838495, -73.37673262171252 44.59548933488261, -73.37678034122506 44.5995502489305, -73.38021835136246 44.604253724812224, -73.38287300055173 44.61219469231369, -73.38929652090236 44.61677748323366, -73.39018989156865 44.61894203909759, -73.38648021928208 44.62688382819318, -73.38705261925948 44.63591171761494, -73.37884859050895 44.64126947002187, -73.38438132248342 44.64676729428025, -73.38396372525868 44.64772444182613, -73.37843017650188 44.65204805369518, -73.37891383987734 44.65704256772158, -73.37440196847228 44.66222644005957, -73.36976855505254 44.66377179321898, -73.37276683284018 44.66904492621139, -73.37112356420356 44.672937819086734, -73.37193295915283 44.67709914852214, -73.36731455201395 44.67862628554099, -73.36736884147177 44.68102225493698, -73.37007484044904 44.68497658956294, -73.36524553268633 44.68776161633547, -73.36127964161274 44.69498729566714, -73.36598505308858 44.69798972314595, -73.36561986462762 44.74181630552179, -73.35480959647136 44.75454318213365, -73.34667575194322 44.773672774726315, -73.33496123002273 44.78304427498427, -73.3333432624864 44.78727940483497, -73.33352073314335 44.798344951383946, -73.335174528327 44.80426578994909, -73.35417352457642 44.8208710260339, -73.36758898274677 44.827581085459364, -73.37537866656832 44.83623510148534, -73.3791582331219 44.8377105926572, -73.3815891399596 44.849232028030286, -73.37947868701234 44.851839888059125, -73.37985069422089 44.856796391214004, -73.37170688132291 44.8626667133931, -73.3689362372027 44.867317740709396, -73.36116246522275 44.89540969623399, -73.35735076971271 44.90279939816071, -73.35253295005919 44.90844621484761, -73.34067424332483 44.91522472557335, -73.3387158214539 44.91904875771223, -73.3396280888245 44.94505060980737, -73.3381880738022 44.96480734419449, -73.35050565243348 44.976507798942656, -73.35476279825052 44.98680451425334, -73.34293424084181 45.01072500108264, -73.06536286945529 45.01597072974534, -72.9668536558064 45.01415656129358, -72.84700675256404 45.01667400047918, -72.63467711943956 45.01469696517789, -72.58475126749747 45.01157753059431, -72.55539921494064 45.00802758150166, -72.45043863520836 45.008571746994306, -72.3119552557624 45.003797816192446, -71.94575417129658 45.00837925080339, -71.91398151188007 45.0076376784572, -71.75410168846628 45.011346421358624, -71.46528176222033 45.0136832226448))"))