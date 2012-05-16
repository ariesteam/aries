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

(ns core.contexts.colorado
  (:refer-clojure :rename {count length})
  (:refer modelling :only [defcontext model transform])
  (:refer geospace :only [grid shape]))

;; --------------------------------------------------------------------------
;; Using a variable for the resolution, so you can change it here and it gets
;; changed in all contexts. 
;; ---------------------------------------------------------------------------
(def resolution 1024)

;; -----------------------------------------------------------------------------
;; FV pre-defining the inline WKT-specified shapes as variables to keep the context
;; definition easier to edit and understand. 
;; This is optional and cosmetic. It allows to just use the vars
;; below instead of the (shape ...) forms and in the (grid ...) statements.
;; 
;; FV using inline specs even if the gazetteer works fine to avoid naming conflict
;; due to Ken using different templates when initializing the gazetteer, and to eliminate a variable in case of 
;; bugs.
;; -----------------------------------------------------------------------------

(def grand-county
  (shape "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))")) ; Run this at 512

(def grand-boulder-larimer
  (shape "EPSG:4326 POLYGON((-106.66 41.01, -104.93 41.01, -104.93 39.67, -106.66 39.67, -106.66 41.01))")) ; Run this at 1024

(def blue
  (shape "EPSG:4326 POLYGON((-106.475 40.05, -105.76 40.05, -105.76 39.35, -106.475 39.35, -106.475 40.05))")) ; Intended to be run at 512

(def upper-south-platte
  (shape "EPSG:4326 POLYGON((-106.215 39.77, -104.8 39.77, -104.8 38.73, -106.215 38.73, -106.215 39.77))")) ; Intended to be run at 512

(defcontext no-mountain-pine-beetle-carbon
  "Conditions with no pine beetle damage"
  (grid resolution grand-boulder-larimer)
;; Changes to carbon model
  (transform 'colorado:MountainPineBeetleDamageClass 'colorado:NoDamage))

(defcontext no-mountain-pine-beetle-aesthetic-view
  "Conditions with no pine beetle damage"
  (grid resolution grand-county)
;; Changes to aesthetic view model
  (transform 'colorado:GrayBeetleKill 'colorado:GrayKillAbsent)  
  (transform 'colorado:GreenGrayBeetleKill 'colorado:GreenGrayKillAbsent))

(defcontext no-mountain-pine-beetle-sediment
  "Conditions with no pine beetle damage"
  (grid resolution grand-county) ; Can also sub blue or upper-south-platte
;; Changes to sediment model
  (transform 'colorado:MountainPineBeetleDamageClass 'colorado:NoDamage))

(defcontext colorado
  "Whole state of Colorado, currently just for data testing purposes"
  (grid
   "1 km"
   "EPSG:4326 POLYGON((-109 41, -102 41, -102 37, -109 37, -109 41))"))

(defcontext co-grand-county400
  "Grand County, CO, for viewshed & sediment analysis"
  (grid
   "50 m"
   "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))"))

(defcontext co-grand-county512
  "Grand County, CO, for viewshed & sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.66 40.5, -105.62 40.5, -105.62 39.67, -106.66 39.67, -106.66 40.5))"))

(defcontext co-blue
  "Colorado Blue River watershed, including Dillon and Green Mountain Reservoirs, for sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.475 40.05, -105.76 40.05, -105.76 39.35, -106.475 39.35, -106.475 40.05))"))

(defcontext upper-south-platte
  "Upper South Platte watershed, including several reservoirs, for sediment analysis"
  (grid
   512
   "EPSG:4326 POLYGON((-106.215 39.77, -104.8 39.77, -104.8 38.73, -106.215 38.73, -106.215 39.77))"))

(defcontext co-grand-boulder-larimer
  "Grand, Boulder, and Larimer counties, CO, for carbon analysis"
  (grid
   1024
   "EPSG:4326 POLYGON((-106.66 41.01, -104.93 41.01, -104.93 39.67, -106.66 39.67, -106.66 41.01))"))

(defcontext co-south-platte
  "Colorado Water Division 1 (South Platte River watershed), for water analysis"
  (grid
   "1 km"
   "EPSG:4326 POLYGON((-106.19096686771802 40.99763419315913, -104.85523418284549 40.99795141037875, -104.50021256219605 41.00164045198053, -102.05109929939924 41.00214404618644, -102.05168950202416 39.889938488494415, -102.04511481541627 38.662437336387256, -102.12115910517717 38.66127666780756, -102.14307303942385 38.6656163541393, -102.21878782218688 38.69246191465876, -102.28152277333443 38.70436190305744, -102.30552972571985 38.72147729547954, -102.32772968211007 38.727509847547616, -102.37822765754039 38.7178058462711, -102.45989096877344 38.70806728820641, -102.49707703201771 38.706657380553736, -102.52755047207211 38.73437629496782, -102.5486499607994 38.761544224878435, -102.55510602120287 38.77830726725343, -102.54881938483852 38.80827306700877, -102.53794210741286 38.83735328762535, -102.5415443810305 38.861192504569686, -102.5385379403322 38.90067295870142, -102.54173923882881 38.915205526918065, -102.55557957022573 38.93156956179991, -102.57346227987719 38.93991638879145, -102.60781679709532 38.969299304504574, -102.62856854053821 38.97679564808362, -102.66953674393568 38.98341007739079, -102.68673833155628 38.98979806841523, -102.73969043838746 38.99801254896218, -102.75539337121621 39.00299193672087, -102.82169667504412 39.0404961546147, -103.00714005372818 39.03719017137217, -103.05627995956559 39.03061286519721, -103.07774077499461 39.037910850928846, -103.09205793437263 39.051683806267064, -103.12108441259531 39.05024927861254, -103.1821245903992 39.05588832912088, -103.20605043493083 39.074090229247616, -103.23483431746986 39.0865627289613, -103.2990479919829 39.13900698170033, -103.34004513460786 39.190047096415796, -103.34990326218397 39.21355990390612, -103.39783231020637 39.225104481464534, -103.4079636927984 39.23560529065448, -103.47407698249633 39.260160635216806, -103.5733861100016 39.29092942099083, -103.59365395556728 39.30056952816809, -103.60521561975978 39.31056162577228, -103.62425363531872 39.337071763037365, -103.64199055563381 39.35028668892022, -103.67487744177066 39.366808068673095, -103.7067387819029 39.37246472270861, -103.74785038347306 39.364485166003895, -103.79310752936317 39.37702390252654, -103.86103437057159 39.34950574596235, -103.87823884618953 39.32057774323291, -103.89873162436822 39.30963183817807, -103.92674828963047 39.301229824281094, -103.9742409993763 39.295156844807714, -103.97911909442604 39.288392164153734, -103.97922691987348 39.28096379947004, -103.97037583808029 39.26322487645719, -103.99355416489642 39.23268741957989, -104.02442663401465 39.21365814565639, -104.03787578014135 39.20878709523399, -104.05731598486268 39.208333398523685, -104.07325555762404 39.20323508967243, -104.1127553011919 39.18020012581026, -104.17354775321475 39.15447968913633, -104.19407994790957 39.15223117455984, -104.21358261595411 39.155347147244775, -104.35044334221303 39.13244303841542, -104.41956742559078 39.10668535813708, -104.43506038860879 39.092032933663496, -104.45504987048497 39.08122931056666, -104.53323078214994 39.07467123929682, -104.5515015204973 39.06955262632722, -104.56733795177463 39.060646395244255, -104.62023257459018 39.03948779632549, -104.66130700885886 39.03441956209841, -104.71621283096228 39.03672918733988, -104.72340727037766 39.041076480812876, -104.73165326325909 39.06099285723939, -104.7410708048067 39.06534416641057, -104.77934777008839 39.06974844268312, -104.79043451240828 39.074094354655074, -104.82254324364803 39.10659293515489, -104.84694966217333 39.118310724724616, -104.9202428547842 39.130495282887395, -104.98245513190243 39.12489839611024, -105.00244965798537 39.12014178418664, -105.02021974126433 39.11234983520294, -105.02798937799771 39.10023703381983, -105.02853827985376 39.084660791282374, -105.02020395150856 39.056109944089265, -105.00133803550735 39.04010734158757, -104.99745510659127 39.03318202638637, -104.99800986805774 39.0279914459476, -105.0168657534626 39.01804166178782, -105.01686333594235 39.00765594127658, -105.0212972416413 39.001166964886984, -105.04125515106442 38.99813378181973, -105.05787934815214 38.98817332040225, -105.08194112296506 38.953247462825395, -105.10064017289984 38.93990454288309, -105.12260080276593 38.90764398920525, -105.12505662721243 38.89646222575256, -105.13055124801703 38.89301825430501, -105.2042628304527 38.89377404644794, -105.21470275752135 38.889884873019355, -105.22237742712719 38.88170140989669, -105.28311575821132 38.87297341527281, -105.2960459765209 38.87466274117044, -105.30213153195314 38.88324443195694, -105.31148307931885 38.883648017659894, -105.31696481209157 38.879551007146894, -105.32575730869148 38.878021944614204, -105.33706275500003 38.88396475637921, -105.34494936708623 38.884842979670694, -105.35248008671033 38.88206749107052, -105.36073440298074 38.87183660075978, -105.37020455275443 38.86763464111871, -105.40870855947077 38.858229118845685, -105.44482158737235 38.84743223902518, -105.4590064016825 38.83995639309596, -105.48397966137105 38.833944232222095, -105.54436261053065 38.835882507871176, -105.58697960235027 38.82686321474201, -105.59708484882105 38.83006081668598, -105.60488235461352 38.84068605835556, -105.61202146501243 38.843433540356976, -105.632198572547 38.8451781835269, -105.6428513339317 38.84280417570169, -105.66999681560367 38.828738017653464, -105.68404255001775 38.809642268663936, -105.68785230601722 38.774375368150004, -105.68355033402295 38.75817049172735, -105.6923849085406 38.75301797732332, -105.80598598969026 38.795880311694155, -105.8523820026211 38.81039355646988, -105.88324052409804 38.813408800540074, -105.8986095415791 38.81004759836994, -105.91046666488303 38.81082102092744, -105.94198885196923 38.84122851932957, -105.93934567504417 38.84543109915762, -105.94260412036286 38.853307928503966, -105.9251754016544 38.87186649761599, -105.94222928120837 38.88564394313432, -105.94955109323264 38.883241746428084, -105.96451133635935 38.88608404150802, -105.97448871305406 38.88269412774847, -105.97979738618137 38.89167916536883, -105.97876984312525 38.89504901445893, -105.96797245128892 38.90196051801604, -105.96591226667576 38.905933546014644, -105.96933902666305 38.908797827777235, -105.9756144730937 38.91025953067325, -105.97669415740701 38.91371988302274, -105.98557746266347 38.917979776457514, -105.99891252781124 38.91582100945243, -106.00804157644515 38.926997721598525, -106.02911629568386 38.942826955717976, -106.03879175482588 38.940614623828324, -106.05728905009397 38.9421517302374, -106.06545445095117 38.93224623843722, -106.10521121135669 38.94005891740825, -106.10838392638686 38.93804637756238, -106.11256131068643 38.94150299793464, -106.11460221402959 38.951639149200304, -106.10455836050448 38.970207522412665, -106.10914826318026 38.98607769342687, -106.14765795751566 38.99910488231983, -106.15904416865696 38.99406328911051, -106.16061508483625 38.988659045950214, -106.16844055184565 38.99011284798611, -106.17610197552942 39.0000381803602, -106.178654950307 39.0103119571364, -106.19313539031779 39.01770695461551, -106.18963787757627 39.021941762369224, -106.19183797015995 39.02969573973778, -106.18278202914256 39.0379247566824, -106.18575071003897 39.05140196472289, -106.19099838904715 39.05624141777461, -106.19128601987953 39.06862842982608, -106.16954308560354 39.08721260091721, -106.17195875060119 39.095676563919696, -106.1794101980888 39.10353998141369, -106.2101034262352 39.10461318641663, -106.20259993514948 39.11439543370493, -106.1889139672755 39.1207712438718, -106.18628163894314 39.12857435857644, -106.17269171701004 39.138551600183526, -106.17930006316813 39.16538201897349, -106.17826597123144 39.173817498390285, -106.18497576021211 39.1854904223731, -106.17913357526196 39.198984602520504, -106.18471414047931 39.20925422798892, -106.17886059711867 39.21296290415643, -106.17779872449715 39.2192090107705, -106.17101780023476 39.223386204798906, -106.16851298556139 39.2299981269016, -106.17172928515524 39.23586778985403, -106.18503576489584 39.24203163296869, -106.18113500979557 39.248414707123246, -106.18062807893185 39.26089956354315, -106.1869168650243 39.278730721748985, -106.18114541011006 39.298225212511454, -106.18410657625182 39.30960250699404, -106.17665978688602 39.31212885819031, -106.17065983242972 39.32119987481164, -106.15128998507913 39.321302890570884, -106.14011851564213 39.33978530934159, -106.15294180570311 39.36266544249562, -106.1488148381792 39.36818961032631, -106.13862577186929 39.37204733710872, -106.13585749111166 39.37959274588124, -106.11972778536244 39.37650418580729, -106.09696368017775 39.37793516722235, -106.08544240670516 39.36968166861266, -106.0735818673509 39.36833934314141, -106.05352436524124 39.35809923922895, -106.0344838297865 39.35841283596383, -106.02085873970731 39.36206629448397, -106.01727299017179 39.36563563033994, -106.0186022254027 39.38113286137794, -106.0142477083403 39.38677539644207, -105.99653763523722 39.3944851593158, -105.99253175686707 39.40196189341619, -105.98453898693165 39.40184881151858, -105.97670950005154 39.408604926716215, -105.9516982710127 39.41462609107926, -105.95476038224426 39.42625285936023, -105.96408348262881 39.432485234954505, -105.96663146390276 39.43834301744371, -105.94407392331735 39.440806820110915, -105.9389333920093 39.458255784575435, -105.92702975251929 39.45953256431662, -105.9116461019901 39.456815117694305, -105.90597848308887 39.45963765000656, -105.89220953099526 39.48428151532091, -105.87892448913193 39.483948379003515, -105.87853207816816 39.489643284675736, -105.87463646417447 39.49352484999971, -105.8665923876777 39.49412668972418, -105.86360225916246 39.50258646193042, -105.85400529196833 39.50469316369595, -105.86811789383434 39.51944971599287, -105.85967619916237 39.53162075746136, -105.84541499014158 39.52390230635223, -105.83079316757669 39.53196889388114, -105.82306139559243 39.53001313469745, -105.81778788575761 39.53838228965043, -105.81900115415755 39.54564378315249, -105.81589564942321 39.552137629060134, -105.8253723831214 39.55525649016625, -105.83010038896305 39.566143548755605, -105.8367898045251 39.568835199039704, -105.83844344099298 39.57266877626051, -105.82653429886506 39.58033820386027, -105.80779923989591 39.58366189203712, -105.8004078034361 39.59339584852878, -105.7759964842551 39.60525489892275, -105.77615386636229 39.61099064857751, -105.78272306542742 39.61795893186418, -105.78272376775296 39.62921257437806, -105.79340956040535 39.63636606025971, -105.81706507963182 39.63350656171093, -105.82107830179415 39.6426960458221, -105.83483748072247 39.64198223629302, -105.84109826499791 39.646241218119044, -105.84876189762885 39.64417480534751, -105.86488961106207 39.66724155438475, -105.8793526321222 39.66360483201315, -105.88483649785036 39.65702349678301, -105.89395795967138 39.659469384239415, -105.89676508315075 39.66460006252492, -105.91116472852629 39.66245012676182, -105.9265586339607 39.673479213235645, -105.91947436105896 39.69036246488123, -105.92455819114704 39.69889963808783, -105.92407333231931 39.70474137331407, -105.92094406854851 39.71026065969904, -105.91166923088898 39.7152566486611, -105.90728757322282 39.727092882804676, -105.89131202119802 39.730399919209106, -105.88384656712623 39.736020389533415, -105.88228228240874 39.7459961429224, -105.88479038422456 39.75196038766438, -105.88043709402837 39.75945668737921, -105.8863394369569 39.76592228532645, -105.8925027358801 39.78245969441333, -105.88765177329795 39.78885976969694, -105.88749702196472 39.79702672460073, -105.87579141309674 39.7992770145415, -105.87218571901572 39.79679486582986, -105.85494256373495 39.79617985856529, -105.85155101892097 39.78771216082752, -105.8441999251709 39.78950550452945, -105.82299779104044 39.78564076508916, -105.81944336588536 39.78956694370317, -105.81074504472139 39.79136240268791, -105.80834882963958 39.79740462320322, -105.80195294331818 39.799246626422665, -105.7992247810137 39.80327190375219, -105.78478649881546 39.80237935715659, -105.76398821497177 39.79492692154789, -105.7360800119941 39.804854610356855, -105.73351333381635 39.81269397773883, -105.72411313730004 39.81826794412883, -105.72383509474251 39.824436540039095, -105.7189318010998 39.82755631737954, -105.7188351100252 39.83216737276164, -105.71344091980083 39.83798574695897, -105.70337919122984 39.836713947365624, -105.69580114469123 39.839196998325676, -105.69032157232652 39.85195690265396, -105.696182901325 39.8619180241525, -105.69678176390251 39.871399581139926, -105.70082050292733 39.87879304981845, -105.6977039121902 39.88417988254829, -105.69970036573139 39.89159150741709, -105.69117247004228 39.8972866936271, -105.6872906112292 39.90408733659881, -105.68793807593434 39.9116422034287, -105.68216100191965 39.91742021308754, -105.68232008921807 39.92192395028661, -105.67510838213259 39.933224865060794, -105.68302610205922 39.93387155164215, -105.69378498533221 39.942370679275605, -105.69177365774985 39.94645860662943, -105.69430808102967 39.95034319064042, -105.68515075777785 39.95873738941844, -105.6883514537147 39.959793214983335, -105.69039182191376 39.96992386609104, -105.68486200291846 39.97952384082275, -105.68750155885087 39.985434128622884, -105.68304145730916 39.99296621471536, -105.69138667959507 39.99745663060422, -105.68842589117753 40.00698172402839, -105.68970560605962 40.013798383592516, -105.66172187487378 40.01644569494769, -105.65200359368164 40.02088243391725, -105.65157372726966 40.02602652557488, -105.64076767612411 40.03493918281226, -105.63764192835878 40.04598782863986, -105.6445617222596 40.04987410105259, -105.65114865099471 40.05848192865969, -105.64668520340233 40.065293158079584, -105.63889714371308 40.06707593055951, -105.64117145938516 40.07310153669729, -105.63528563567118 40.07510508478486, -105.63268554052723 40.08143056085582, -105.63610768018468 40.084546688619994, -105.63372519806303 40.08852578789833, -105.63469992968508 40.09526511945907, -105.63204474592906 40.09739326490501, -105.63603860870055 40.1012127929564, -105.63672838997456 40.107199631870614, -105.63476628417872 40.11322475684158, -105.63017252761315 40.11401571012964, -105.62651900621603 40.118684320020115, -105.62873557967829 40.123404249823345, -105.62625665723922 40.12737574728625, -105.6301483190066 40.13067115337728, -105.62976458486561 40.137838580091085, -105.63547805453746 40.14039728459581, -105.6358790409935 40.14620792350258, -105.64722217675595 40.148950201350466, -105.64741516478163 40.15105073225928, -105.6558893371152 40.15126291186514, -105.6676889323143 40.15836064166755, -105.6715163209666 40.16571723872022, -105.66685487226569 40.17002240870158, -105.66984040279776 40.17437582870783, -105.66837620004118 40.17703437301424, -105.67196568963658 40.17999857995999, -105.67064306458524 40.18557200455276, -105.67768813429092 40.18924923033882, -105.67776496201895 40.202262475582074, -105.67065005920225 40.207684853082455, -105.67040481947028 40.21948170290137, -105.66314914703877 40.21997956219786, -105.66903732252956 40.22440869210582, -105.65994248061182 40.248888783704345, -105.64391775297125 40.24990270954161, -105.65103133721057 40.253892906096354, -105.65452557929885 40.261494312204974, -105.66583253138866 40.26713414989581, -105.67807396776406 40.268033107469435, -105.68251289454686 40.27249885402701, -105.67953213299924 40.27894231654567, -105.68073849959254 40.28423533027797, -105.68679562805696 40.28830455099233, -105.68126099729304 40.29198609455767, -105.68766609308128 40.29385928154555, -105.68600163284599 40.30300283895255, -105.69138507225708 40.30514744047715, -105.69038125525057 40.308773411027666, -105.70245383689493 40.312274615091965, -105.69784891696315 40.31845699024077, -105.70193153534301 40.32448870100219, -105.71326650530116 40.32567229066314, -105.72844959725089 40.334630580475704, -105.73662260681252 40.34665694787109, -105.74260579020415 40.3493553447939, -105.74830734513054 40.347690526729444, -105.75879048661957 40.352700155286016, -105.78723045289904 40.38099140998113, -105.79093090965827 40.38714936788408, -105.7909449576809 40.398793705857216, -105.81738754630634 40.42448188697827, -105.81433316391148 40.429124833838, -105.81601219603549 40.43724585973058, -105.80577487966829 40.44762615036407, -105.80767220110157 40.45200346799708, -105.80443491477006 40.46411724613781, -105.81114548001447 40.47087225474941, -105.80715247496796 40.47413663451943, -105.85478911882458 40.486205004751525, -105.85257436861527 40.50695913037477, -105.85692213879604 40.507647678359724, -105.86891175774464 40.52058493322587, -105.8794693532335 40.523414906630784, -105.89407837317391 40.520203746811035, -105.90468218266702 40.51431051397341, -105.90793646215626 40.51628494819903, -105.91065145073512 40.52245264191935, -105.90576237318581 40.52963506050404, -105.90498275145467 40.53680316699115, -105.91146492566835 40.55334638233429, -105.91035629044488 40.56650791090056, -105.92161455524925 40.57514810027951, -105.92982267786806 40.60589207902003, -105.94210930819641 40.60986441917992, -105.94707005505381 40.61754426374777, -105.94501512576154 40.621371758219425, -105.94940530313063 40.62729045905367, -105.94863334101382 40.64111603758737, -105.95470873066272 40.64784043610081, -105.95608582643192 40.65929698105597, -105.96427032731488 40.66751680123167, -105.96692497581166 40.67628695298177, -105.9714154958967 40.68078055279976, -105.97003276943546 40.684837016253, -105.97380043732808 40.68982304717135, -105.97223163228873 40.69533147288945, -105.97681827727381 40.702598654488426, -105.9889947508386 40.710539551140336, -105.9882029101594 40.72190603152762, -105.99132137822389 40.728644646967325, -106.00995893205884 40.7403561737202, -106.01514025944871 40.75203068754363, -106.02227660712936 40.75473328589157, -106.02077996055297 40.7571878342475, -106.02669021715013 40.76794555383553, -106.02496651286457 40.77071746775682, -106.02929220063423 40.773453559553715, -106.02978060906494 40.78140365526086, -106.03926462966953 40.78545347580842, -106.04544719605369 40.79200078203136, -106.04704369434431 40.80272431834737, -106.05221273248537 40.806451797250546, -106.05397363000984 40.81257042777173, -106.08753466414096 40.83286095359043, -106.09340779951087 40.840813816501125, -106.10009258833786 40.84211049721746, -106.099144217847 40.849920729397134, -106.1077723431326 40.8517299086554, -106.10483509790346 40.860342981788136, -106.12347534545013 40.86754064175535, -106.13089489571017 40.87426032933291, -106.12748751090872 40.886175560953845, -106.1311051361833 40.89661668136664, -106.15927998830225 40.91134443864508, -106.15933685666772 40.91443366842304, -106.16958626810454 40.91960891237386, -106.16947785003431 40.9231051745295, -106.18535103478591 40.934095103533174, -106.18949331796252 40.94530356089441, -106.18447964241217 40.955489305744685, -106.18699813614579 40.95782351273786, -106.18685600542352 40.9711299033808, -106.19047627393593 40.97442554305734, -106.19066824660796 40.98436844724088, -106.19528572542619 40.98761758399514, -106.19096686771802 40.99763419315913))"))

(defcontext co-south-platte-lowres
  "Colorado Water Division 1 (South Platte River watershed), for water analysis"
  (grid
   "400 m"
   "EPSG:4326 POLYGON((-106.25 41.01, -102.04 41.01, -102.04 38.63, -106.25 38.63, -106.25 41.01))"))