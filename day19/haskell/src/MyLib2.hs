-- language directives at top of file
{-# LANGUAGE QuasiQuotes #-}

-- mdoule declaration
--    containing import statements
--    followed by definitions

module MyLib2 (someFunc , strs , byteCount ,charCount ,byteCount2 ) where
-- https://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html
import Text.RawString.QQ

-- embedded puzzle input directly into haskell source file
-- avoided interpolation 
multi :: String
multi = [r|"sjdivfriyaaqa\xd2v\"k\"mpcu\"yyu\"en"
"vcqc"
"zbcwgmbpijcxu\"yins\"sfxn"
"yumngprx"
"bbdj"
"czbggabkzo\"wsnw\"voklp\"s"
"acwt"
"aqttwnsohbzian\"evtllfxwkog\"cunzw"
"ugvsgfv"
"xlnillibxg"
"kexh\"pmi"
"syvugow"
"m\"ktqnw"
"yrbajyndte\\rm"
"f\"kak\x70sn\xc4kjri"
"yxthr"
"alvumfsjni\"kohg"
"trajs\x5brom\xf1yoijaumkem\"\"tahlzs"
"\"oedr\"pwdbnnrc"
"qsmzhnx\""
"\"msoytqimx\\tbklqz"
"mjdfcgwdshrehgs"
"\"rivyxahf\""
"ciagc\x04bp"
"xkfc"
"xrgcripdu\x4c\xc4gszjhrvumvz\"mngbirb"
"gvmae\"yiiujoqvr\"mkxmgbbut\"u"
"ih"
"ncrqlejehs"
"mkno\x43pcfdukmemycp"
"uanzoqxkpsksbvdnkji\"feamp"
"axoufpnbx\\ao\x61pfj\"b"
"dz\\ztawzdjy"
"ihne\"enumvswypgf"
"\"dgazthrphbshdo\\vuqoiy\""
"dlnmptzt\\zahwpylc\\b\"gmslrqysk"
"mhxznyzcp"
"rebr\"amvxw\x5fmbnfpkkeghlntavj"
"lades\x47ncgdof\"\"jmbbk"
"dwxuis\xa5wdkx\\z\"admgnoddpgkt\\zs"
"g\\k\x27qsl\x34hwfglcdxqbeclt\xca\\"
"lhyjky\\m\"pvnm\\xmynpxnlhndmahjl"
"c\"uxabbgorrpprw\"xas\\vefkxioqpt"
"rfrvjxpevcmma\x71gtfipo"
"fgh\"kcwoqwfnjgdlzfclprg\"q"
"onxnwykrba"
"hkkg\x60f\"tjzsanpvarzgkfipl"
"\"aintes\"ofq\"juiaqlqxmvpe\\a"
"wiyczzs\"ciwk"
"mfqeu"
"v\xe1z\x7ftzalmvdmncfivrax\\rjwq"
"k\"vtg"
"exhrtdugeml\xf0"
"behnchkpld"
"mhgxy\"mfcrg\xc5gnp\"\"osqhj"
"rlvjy"
"awe"
"ctwy"
"vt"
"\x54t"
"zugfmmfomz"
"cv\"cvcvfaada\x04fsuqjinbfh\xa9cq\xd2c\"d"
"oj"
"xazanf\"wbmcrn"
"\\\\zkisyjpbzandqikqjqvee"
"dpsnbzdwnxk\\v"
"sj\"tuupr\\oyoh"
"myvkgnw\x81q\xaaokt\\emgejbsyvxcl\\\xee"
"ejeuqvunjcirdkkpt\"nlns"
"twmlvwxyvfyqqzu"
"\"xwtzdp\x98qkcis\"dm\\\"ep\"xyykq"
"vvcq\\expok"
"wgukjfanjgpdjb"
"\"mjcjajnxy\\dcpc"
"wdvgnecw\\ab\x44klceduzgsvu"
"dqtqkukr\"iacngufbqkdpxlwjjt"
"\"xj\"\x66qofsqzkoah"
"nptiwwsqdep"
"gsnlxql\x30mjl"
"yeezwokjwrhelny\""
"bjauamn\\izpmzqqasid"
"tvjdbkn\"tiziw\x82r"
"w"
"xwoakbbnjnypnaa\xa9wft\"slrmoqkl"
"vwxtnlvaaasyruykgygrvpiopzygf\"vq"
"qdancvnvmhlmpj\\isdxs"
"xzc\\elw"
"b\"wxeqvy\"qf\"g\xcaoklsucwicyw\"dovr"
"yomlvvjdbngz\"rly\"afr"
"bfb\"x\"aweuwbwmoa\x13\"t\"zhr"
"\"dmfoxb\"qvpjzzhykt\xd2\"\"ryhxi"
"psqef\"yu\\qiflie\"\x79w"
"arzewkej\"lqmh\\sayyusxxo\\"
"vuvvp"
"hc\"lg\x6bcpupsewzklai\"l"
"cjdfygc\"auorqybnuqghsh\x10"
"j"
"wqjexk\"eyq\\lbroqhk\\dqzsqk"
"dws\"ru\"dvxfiwapif\"oqwzmle"
"agcykg\\jt\\vzklqjvknoe"
"kksd\"jmslja\\z\"y\\b\xaagpyojct"
"nnpipxufvbfpoz\"jno"
"dtw"
"xlolvtahvgqkx\\dgnhj\\spsclpcxv\\"
"mxea\\mbjpi"
"lgbotkk\"zmxh\\\\qji\"jszulnjsxkqf"
"lwckmhwhx\"gmftlb\x91am"
"xxdxqyxth"
"\"lmqhwkjxmvayxy"
"tf"
"qy"
"wdqmwxdztax\"m\"\x09\x11xdxmfwxmtqgwvf"
"\xcbnazlf\"ghziknszmsrahaf"
"e\x6aupmzhxlvwympgjjpdvo\"kylfa"
"\x81vhtlillb\xactgoatva"
"dvnlgr"
"f"
"xg\xfacwizsadgeclm"
"vnnrzbtw\"\\prod\\djbyppngwayy\""
"lrt\xf4jahwvfz"
"aqpnjtom\"ymkak\\dadfybqrso\\fwv"
"gz\"aac\"mrbk\"ktommrojraqh"
"wycamwoecsftepfnlcdkm"
"nrhddblbuzlqsl\x9cben"
"vckxhyqkmqmdseazcykrbysm"
"sil\xbbtevmt\"gvrvybui\"faw\"j"
"cjex\\tp\x45pzf"
"asjobvtxszfodgf\"ibftg"
"gkyjyjdrxdcllnh\"sjcibenrdnxv"
"oswsdpjyxpbwnqbcpl\"yrdvs\\zq"
"\"\"tyowzc\\fycbp\"jbwrbvgui"
"cbpcabqkdgzmpgcwjtrchxp"
"iyrzfh\x45gw\"fdlfpiaap\x31xqq"
"evgksznidz"
"b\\w\\"
"loufizbiy\x57aim\"bgk"
"qjfyk"
"g\"anmloghvgr\x07zwqougqhdz"
"usbbmwcxd\\bdgg"
"htitqcpczml"
"eke\\cqvpexqqk\"to\"tqmljrpn\xe6lji\""
"g\xd2ifdsej"
"h\"sk\"haajajpagtcqnzrfqn\xe6btzo"
"wfkuffdxlvm\\cvlyzlbyunclhmpp"
"myaavh\"spue"
"hqvez\x68d\"eo\"eaioh"
"s\"qd\"oyxxcglcdnuhk"
"ilqvar"
"srh"
"puuifxrfmpc\"bvalwi\x2blu\\"
"yywlbutufzysbncw\\nqsfbhpz\"mngjq"
"zbl\\jfcuop"
"hjdouiragzvxsqkreup\\"
"qi"
"ckx\\funlj\xa7ahi"
"k"
"ufrcnh\"ajteit"
"cqv\"bgjozjj\x60x\xa8yhvmdvutchjotyuz"
"hkuiet\"oku\x8cfhumfpasl"
"\"\\sbe\x4d"
"vhknazqt"
"eyyizvzcahgflvmoowvs\\jhvygci"
"kki\x3ewcefkgtjap\"xtpxh\"lzepoqj"
"wvtk"
"\"ynet"
"zh\\obk\"otagx\x59txfzf"
"ocowhxlx\xe6zqg\x63wx\\tclkhq\\vmaze"
"w\"cf"
"qpniprnrzrnvykghqnalr"
"jctcqra\"\x05dhlydpqamorqjsijt\\xjdgt"
"sig"
"qhlbidbflwxe\"xljbwls\x20vht"
"irmrebfla\xefsg\"j"
"nep"
"hjuvsqlizeqobepf"
"guzbcdp\"obyh"
"\"mjagins\xf9tqykaxy\""
"knvsdnmtr\"zervsb"
"hzuy"
"zza\"k\"buapb\\elm\xfeya"
"lrqar\"dfqwkaaqifig\"uixjsz"
"\"azuo\x40rmnlhhluwsbbdb\x32pk\\yu\"pbcf"
"dplkdyty"
"rfoyciebwlwphcycmguc"
"ivnmmiemhgytmlprq\\eh"
"lhkyzaaothfdhmbpsqd\\yyw"
"tnlzifupcjcaj"
"\\qiyirsdrfpmu\\\x15xusifaag"
"\\lcomf\\s"
"uramjivcirjhqcqcg"
"kkbaklbxfxikffnuhtu\xc6t\"d"
"n\xefai"
"\"toy\"bnbpevuzoc\"muywq\"gz\"grbm"
"\"muu\\wt"
"\\srby\"ee"
"erf\"gvw\"swfppf"
"pbqcgtn\"iuianhcdazfvmidn\\nslhxdf"
"uxbp"
"up\\mgrcyaegiwmjufn"
"nulscgcewj\\dvoyvhetdegzhs\""
"masv\"k\\rzrb"
"qtx\x79d\"xdxmbxrvhj"
"fid\\otpkgjlh\"qgsvexrckqtn\xf4"
"tagzu"
"bvl\\\"noseec"
"\\xgicuuh"
"w\"a\"npemf"
"sxp"
"nsmpktic\x8awxftscdcvijjobnq\"gjd"
"uks\"\"jxvyvfezz\"aynxoev\"cuoav"
"m"
"lkvokj"
"vkfam\"yllr\"q\x92o\x4ebecnvhshhqe\\"
"efdxcjkjverw"
"lmqzadwhfdgmep\x02tzfcbgrbfekhat"
"cpbk\x9azqegbpluczssouop\x36ztpuoxsw"
"cqwoczxdd\"erdjka"
"cwvqnjgbw\\fxdlby"
"mvtm"
"lt\"bbqzpumplkg"
"ntd\xeeuwweucnuuslqfzfq"
"y\xabl\"dbebxjrlbmuoo\\\x1au"
"qjoqx\\a"
"pu\"ekdnfpmly\xbago\""
"fjhhdy"
"arl"
"xcywisim\"bwuwf\"\"raepeawwjub"
"pbe"
"dbnqfpzyaumxtqnd\xc5dcqrkwyop"
"ojv\x40vtkwgkqepm\x8bzft\\vedrry"
"wggqkfbwqumsgajqwphjec\"mstxpwz"
"zjkbem"
"icpfqxbelxazlls"
"pvpqs\\abcmtyielugfgcv\"tjxapxqxnx"
"oqddwlvmtv\"\x39lyybylfb\"jmngnpjrdw"
"gisgbve"
"\"aglg"
"y\"\"ss\xafvhxlrjv"
"qbgqjsra"
"ihshbjgqpdcljpmdwdprwloy"
"djja\\wcdn\"svkrgpqn\"uz\"hc\x43hj"
"cbjm"
"pnn"
"pqvh\"noh"
"\"\\fdktlp"
"ncea"
"pqgzphiyy"
"\xbedovhxuipaohlcvkwtxwmpz\"ckaif\"r"
"arjuzbjowqciunfwgxtph\"vlhy\"n"
"c"
"nrpdxunulgudqzlhtae"
"iefheu\"uru\""
"aqijysxuijud\"np\\opbichhudil\xbesum"
"pfpevmtstl\"lde\"bzr\"vspdxs"
"vparfbdjwvzsocpnzhp"
"g\x4ffxaarafrsjthq\\\xc1rw"
"ng\\rqx\\gwpzucbh\xafl"
"rw\"nf\\dna"
"jkkeahxurxla\\g\xb3czrlsyimmwcwthr"
"twaailoypu\"oas\"kpuuyedlaw\\\xb0vzt"
"hznex\\gdiqvtugi"
"imdibsunjeswhk"
"ta\\icileuzpxro\"cfmv\"mzp"
"coykr\x57luiysucfaflmilhlehmvzeiepo"
"u\x3dfh\xd4yt"
"piw\x1bz\"eowy\"vfk\"wqiekw"
"gan\"y"
"p\"bevidoazcznr\"hddxuuq\""
"bwzucczznutbxe"
"z\"viqgyqjisior\\iecosmjbknol"
"dmlpcglcfkfsctxydjvayhymv\x3c\\gp"
"bfvkqrintbbvgfv"
"xlzntrgdck\"cprc\xadczyarbznqmuhxyuh"
"uqdxnuwioc\"kdytxq\\ig"
"xrafmucpmfi"
"vr\"hltmfrge"
"eonf\"nt\\wtcnsocs"
"j\xb7xoslyjeyjksplkqixncgkylkw"
"njw\"pefgfbez\x9axshdmplxzquqe"
"di\x58bvptfsafirpc"
"l\x1fkco"
"x"
"mprndo\"n"
"psegit"
"svbdnkkuuqs\"sqxu\"oqcyz\"aizashk"
"cwkljukxer\\\"\\nff\"esjwiyaoy"
"ilxrkgbjjxpvhdtq\"cpiuoofdnkpp"
"hlngi\"ulxep\\qohtmqnqjb\"rkgerho"
"gxws\"bcgm\"p"
"bv\"mds\\zhfusiepgrz\\b\x32fscdzz"
"l\xfampwtme\x69qvxnx\"\"\xc4jruuymjxrpsv"
"qqmxhrn"
"xziq\\\x18ybyv\x9am\"neacoqjzytertisysza"
"aqcbvlvcrzceeyx\\j\"\"x"
"yjuhhb"
"\x5em\"squulpy"
"dpbntplgmwb"
"utsgfkm\\vbftjknlktpthoeo"
"ccxjgiocmuhf\"ycnh"
"lltj\"kbbxi"|]


split2 :: String -> String -> [String] -> [String]
split2 [] [] s = s
split2 [] w s = w : s 
split2 (h : t) w s = if h == '\n' then  split2 t [] ((reverse w) : s)
                    else split2 t (h : w) s

split :: String -> [String]
split xs = reverse (split2 xs [] [])

  

strs :: [String]
strs = ["\"sjdivfriyaaqa\\xd2v\\\"k\\\"mpcu\\\"yyu\\\"en\"","\"vcqc\"","\"zbcwgmbpijcxu\\\"yins\\\"sfxn\"","\"yumngprx\"","\"bbdj\"","\"czbggabkzo\\\"wsnw\\\"voklp\\\"s\"","\"acwt\"","\"aqttwnsohbzian\\\"evtllfxwkog\\\"cunzw\"","\"ugvsgfv\"","\"xlnillibxg\"","\"kexh\\\"pmi\"","\"syvugow\"","\"m\\\"ktqnw\"","\"yrbajyndte\\\\rm\"","\"f\\\"kak\\x70sn\\xc4kjri\"","\"yxthr\"","\"alvumfsjni\\\"kohg\"","\"trajs\\x5brom\\xf1yoijaumkem\\\"\\\"tahlzs\"","\"\\\"oedr\\\"pwdbnnrc\"","\"qsmzhnx\\\"\"","\"\\\"msoytqimx\\\\tbklqz\"","\"mjdfcgwdshrehgs\"","\"\\\"rivyxahf\\\"\"","\"ciagc\\x04bp\"","\"xkfc\"","\"xrgcripdu\\x4c\\xc4gszjhrvumvz\\\"mngbirb\"","\"gvmae\\\"yiiujoqvr\\\"mkxmgbbut\\\"u\"","\"ih\"","\"ncrqlejehs\"","\"mkno\\x43pcfdukmemycp\"","\"uanzoqxkpsksbvdnkji\\\"feamp\"","\"axoufpnbx\\\\ao\\x61pfj\\\"b\"","\"dz\\\\ztawzdjy\"","\"ihne\\\"enumvswypgf\"","\"\\\"dgazthrphbshdo\\\\vuqoiy\\\"\"","\"dlnmptzt\\\\zahwpylc\\\\b\\\"gmslrqysk\"","\"mhxznyzcp\"","\"rebr\\\"amvxw\\x5fmbnfpkkeghlntavj\"","\"lades\\x47ncgdof\\\"\\\"jmbbk\"","\"dwxuis\\xa5wdkx\\\\z\\\"admgnoddpgkt\\\\zs\"","\"g\\\\k\\x27qsl\\x34hwfglcdxqbeclt\\xca\\\\\"","\"lhyjky\\\\m\\\"pvnm\\\\xmynpxnlhndmahjl\"","\"c\\\"uxabbgorrpprw\\\"xas\\\\vefkxioqpt\"","\"rfrvjxpevcmma\\x71gtfipo\"","\"fgh\\\"kcwoqwfnjgdlzfclprg\\\"q\"","\"onxnwykrba\"","\"hkkg\\x60f\\\"tjzsanpvarzgkfipl\"","\"\\\"aintes\\\"ofq\\\"juiaqlqxmvpe\\\\a\"","\"wiyczzs\\\"ciwk\"","\"mfqeu\"","\"v\\xe1z\\x7ftzalmvdmncfivrax\\\\rjwq\"","\"k\\\"vtg\"","\"exhrtdugeml\\xf0\"","\"behnchkpld\"","\"mhgxy\\\"mfcrg\\xc5gnp\\\"\\\"osqhj\"","\"rlvjy\"","\"awe\"","\"ctwy\"","\"vt\"","\"\\x54t\"","\"zugfmmfomz\"","\"cv\\\"cvcvfaada\\x04fsuqjinbfh\\xa9cq\\xd2c\\\"d\"","\"oj\"","\"xazanf\\\"wbmcrn\"","\"\\\\\\\\zkisyjpbzandqikqjqvee\"","\"dpsnbzdwnxk\\\\v\"","\"sj\\\"tuupr\\\\oyoh\"","\"myvkgnw\\x81q\\xaaokt\\\\emgejbsyvxcl\\\\\\xee\"","\"ejeuqvunjcirdkkpt\\\"nlns\"","\"twmlvwxyvfyqqzu\"","\"\\\"xwtzdp\\x98qkcis\\\"dm\\\\\\\"ep\\\"xyykq\"","\"vvcq\\\\expok\"","\"wgukjfanjgpdjb\"","\"\\\"mjcjajnxy\\\\dcpc\"","\"wdvgnecw\\\\ab\\x44klceduzgsvu\"","\"dqtqkukr\\\"iacngufbqkdpxlwjjt\"","\"\\\"xj\\\"\\x66qofsqzkoah\"","\"nptiwwsqdep\"","\"gsnlxql\\x30mjl\"","\"yeezwokjwrhelny\\\"\"","\"bjauamn\\\\izpmzqqasid\"","\"tvjdbkn\\\"tiziw\\x82r\"","\"w\"","\"xwoakbbnjnypnaa\\xa9wft\\\"slrmoqkl\"","\"vwxtnlvaaasyruykgygrvpiopzygf\\\"vq\"","\"qdancvnvmhlmpj\\\\isdxs\"","\"xzc\\\\elw\"","\"b\\\"wxeqvy\\\"qf\\\"g\\xcaoklsucwicyw\\\"dovr\"","\"yomlvvjdbngz\\\"rly\\\"afr\"","\"bfb\\\"x\\\"aweuwbwmoa\\x13\\\"t\\\"zhr\"","\"\\\"dmfoxb\\\"qvpjzzhykt\\xd2\\\"\\\"ryhxi\"","\"psqef\\\"yu\\\\qiflie\\\"\\x79w\"","\"arzewkej\\\"lqmh\\\\sayyusxxo\\\\\"","\"vuvvp\"","\"hc\\\"lg\\x6bcpupsewzklai\\\"l\"","\"cjdfygc\\\"auorqybnuqghsh\\x10\"","\"j\"","\"wqjexk\\\"eyq\\\\lbroqhk\\\\dqzsqk\"","\"dws\\\"ru\\\"dvxfiwapif\\\"oqwzmle\"","\"agcykg\\\\jt\\\\vzklqjvknoe\"","\"kksd\\\"jmslja\\\\z\\\"y\\\\b\\xaagpyojct\"","\"nnpipxufvbfpoz\\\"jno\"","\"dtw\"","\"xlolvtahvgqkx\\\\dgnhj\\\\spsclpcxv\\\\\"","\"mxea\\\\mbjpi\"","\"lgbotkk\\\"zmxh\\\\\\\\qji\\\"jszulnjsxkqf\"","\"lwckmhwhx\\\"gmftlb\\x91am\"","\"xxdxqyxth\"","\"\\\"lmqhwkjxmvayxy\"","\"tf\"","\"qy\"","\"wdqmwxdztax\\\"m\\\"\\x09\\x11xdxmfwxmtqgwvf\"","\"\\xcbnazlf\\\"ghziknszmsrahaf\"","\"e\\x6aupmzhxlvwympgjjpdvo\\\"kylfa\"","\"\\x81vhtlillb\\xactgoatva\"","\"dvnlgr\"","\"f\"","\"xg\\xfacwizsadgeclm\"","\"vnnrzbtw\\\"\\\\prod\\\\djbyppngwayy\\\"\"","\"lrt\\xf4jahwvfz\"","\"aqpnjtom\\\"ymkak\\\\dadfybqrso\\\\fwv\"","\"gz\\\"aac\\\"mrbk\\\"ktommrojraqh\"","\"wycamwoecsftepfnlcdkm\"","\"nrhddblbuzlqsl\\x9cben\"","\"vckxhyqkmqmdseazcykrbysm\"","\"sil\\xbbtevmt\\\"gvrvybui\\\"faw\\\"j\"","\"cjex\\\\tp\\x45pzf\"","\"asjobvtxszfodgf\\\"ibftg\"","\"gkyjyjdrxdcllnh\\\"sjcibenrdnxv\"","\"oswsdpjyxpbwnqbcpl\\\"yrdvs\\\\zq\"","\"\\\"\\\"tyowzc\\\\fycbp\\\"jbwrbvgui\"","\"cbpcabqkdgzmpgcwjtrchxp\"","\"iyrzfh\\x45gw\\\"fdlfpiaap\\x31xqq\"","\"evgksznidz\"","\"b\\\\w\\\\\"","\"loufizbiy\\x57aim\\\"bgk\"","\"qjfyk\"","\"g\\\"anmloghvgr\\x07zwqougqhdz\"","\"usbbmwcxd\\\\bdgg\"","\"htitqcpczml\"","\"eke\\\\cqvpexqqk\\\"to\\\"tqmljrpn\\xe6lji\\\"\"","\"g\\xd2ifdsej\"","\"h\\\"sk\\\"haajajpagtcqnzrfqn\\xe6btzo\"","\"wfkuffdxlvm\\\\cvlyzlbyunclhmpp\"","\"myaavh\\\"spue\"","\"hqvez\\x68d\\\"eo\\\"eaioh\"","\"s\\\"qd\\\"oyxxcglcdnuhk\"","\"ilqvar\"","\"srh\"","\"puuifxrfmpc\\\"bvalwi\\x2blu\\\\\"","\"yywlbutufzysbncw\\\\nqsfbhpz\\\"mngjq\"","\"zbl\\\\jfcuop\"","\"hjdouiragzvxsqkreup\\\\\"","\"qi\"","\"ckx\\\\funlj\\xa7ahi\"","\"k\"","\"ufrcnh\\\"ajteit\"","\"cqv\\\"bgjozjj\\x60x\\xa8yhvmdvutchjotyuz\"","\"hkuiet\\\"oku\\x8cfhumfpasl\"","\"\\\"\\\\sbe\\x4d\"","\"vhknazqt\"","\"eyyizvzcahgflvmoowvs\\\\jhvygci\"","\"kki\\x3ewcefkgtjap\\\"xtpxh\\\"lzepoqj\"","\"wvtk\"","\"\\\"ynet\"","\"zh\\\\obk\\\"otagx\\x59txfzf\"","\"ocowhxlx\\xe6zqg\\x63wx\\\\tclkhq\\\\vmaze\"","\"w\\\"cf\"","\"qpniprnrzrnvykghqnalr\"","\"jctcqra\\\"\\x05dhlydpqamorqjsijt\\\\xjdgt\"","\"sig\"","\"qhlbidbflwxe\\\"xljbwls\\x20vht\"","\"irmrebfla\\xefsg\\\"j\"","\"nep\"","\"hjuvsqlizeqobepf\"","\"guzbcdp\\\"obyh\"","\"\\\"mjagins\\xf9tqykaxy\\\"\"","\"knvsdnmtr\\\"zervsb\"","\"hzuy\"","\"zza\\\"k\\\"buapb\\\\elm\\xfeya\"","\"lrqar\\\"dfqwkaaqifig\\\"uixjsz\"","\"\\\"azuo\\x40rmnlhhluwsbbdb\\x32pk\\\\yu\\\"pbcf\"","\"dplkdyty\"","\"rfoyciebwlwphcycmguc\"","\"ivnmmiemhgytmlprq\\\\eh\"","\"lhkyzaaothfdhmbpsqd\\\\yyw\"","\"tnlzifupcjcaj\"","\"\\\\qiyirsdrfpmu\\\\\\x15xusifaag\"","\"\\\\lcomf\\\\s\"","\"uramjivcirjhqcqcg\"","\"kkbaklbxfxikffnuhtu\\xc6t\\\"d\"","\"n\\xefai\"","\"\\\"toy\\\"bnbpevuzoc\\\"muywq\\\"gz\\\"grbm\"","\"\\\"muu\\\\wt\"","\"\\\\srby\\\"ee\"","\"erf\\\"gvw\\\"swfppf\"","\"pbqcgtn\\\"iuianhcdazfvmidn\\\\nslhxdf\"","\"uxbp\"","\"up\\\\mgrcyaegiwmjufn\"","\"nulscgcewj\\\\dvoyvhetdegzhs\\\"\"","\"masv\\\"k\\\\rzrb\"","\"qtx\\x79d\\\"xdxmbxrvhj\"","\"fid\\\\otpkgjlh\\\"qgsvexrckqtn\\xf4\"","\"tagzu\"","\"bvl\\\\\\\"noseec\"","\"\\\\xgicuuh\"","\"w\\\"a\\\"npemf\"","\"sxp\"","\"nsmpktic\\x8awxftscdcvijjobnq\\\"gjd\"","\"uks\\\"\\\"jxvyvfezz\\\"aynxoev\\\"cuoav\"","\"m\"","\"lkvokj\"","\"vkfam\\\"yllr\\\"q\\x92o\\x4ebecnvhshhqe\\\\\"","\"efdxcjkjverw\"","\"lmqzadwhfdgmep\\x02tzfcbgrbfekhat\"","\"cpbk\\x9azqegbpluczssouop\\x36ztpuoxsw\"","\"cqwoczxdd\\\"erdjka\"","\"cwvqnjgbw\\\\fxdlby\"","\"mvtm\"","\"lt\\\"bbqzpumplkg\"","\"ntd\\xeeuwweucnuuslqfzfq\"","\"y\\xabl\\\"dbebxjrlbmuoo\\\\\\x1au\"","\"qjoqx\\\\a\"","\"pu\\\"ekdnfpmly\\xbago\\\"\"","\"fjhhdy\"","\"arl\"","\"xcywisim\\\"bwuwf\\\"\\\"raepeawwjub\"","\"pbe\"","\"dbnqfpzyaumxtqnd\\xc5dcqrkwyop\"","\"ojv\\x40vtkwgkqepm\\x8bzft\\\\vedrry\"","\"wggqkfbwqumsgajqwphjec\\\"mstxpwz\"","\"zjkbem\"","\"icpfqxbelxazlls\"","\"pvpqs\\\\abcmtyielugfgcv\\\"tjxapxqxnx\"","\"oqddwlvmtv\\\"\\x39lyybylfb\\\"jmngnpjrdw\"","\"gisgbve\"","\"\\\"aglg\"","\"y\\\"\\\"ss\\xafvhxlrjv\"","\"qbgqjsra\"","\"ihshbjgqpdcljpmdwdprwloy\"","\"djja\\\\wcdn\\\"svkrgpqn\\\"uz\\\"hc\\x43hj\"","\"cbjm\"","\"pnn\"","\"pqvh\\\"noh\"","\"\\\"\\\\fdktlp\"","\"ncea\"","\"pqgzphiyy\"","\"\\xbedovhxuipaohlcvkwtxwmpz\\\"ckaif\\\"r\"","\"arjuzbjowqciunfwgxtph\\\"vlhy\\\"n\"","\"c\"","\"nrpdxunulgudqzlhtae\"","\"iefheu\\\"uru\\\"\"","\"aqijysxuijud\\\"np\\\\opbichhudil\\xbesum\"","\"pfpevmtstl\\\"lde\\\"bzr\\\"vspdxs\"","\"vparfbdjwvzsocpnzhp\"","\"g\\x4ffxaarafrsjthq\\\\\\xc1rw\"","\"ng\\\\rqx\\\\gwpzucbh\\xafl\"","\"rw\\\"nf\\\\dna\"","\"jkkeahxurxla\\\\g\\xb3czrlsyimmwcwthr\"","\"twaailoypu\\\"oas\\\"kpuuyedlaw\\\\\\xb0vzt\"","\"hznex\\\\gdiqvtugi\"","\"imdibsunjeswhk\"","\"ta\\\\icileuzpxro\\\"cfmv\\\"mzp\"","\"coykr\\x57luiysucfaflmilhlehmvzeiepo\"","\"u\\x3dfh\\xd4yt\"","\"piw\\x1bz\\\"eowy\\\"vfk\\\"wqiekw\"","\"gan\\\"y\"","\"p\\\"bevidoazcznr\\\"hddxuuq\\\"\"","\"bwzucczznutbxe\"","\"z\\\"viqgyqjisior\\\\iecosmjbknol\"","\"dmlpcglcfkfsctxydjvayhymv\\x3c\\\\gp\"","\"bfvkqrintbbvgfv\"","\"xlzntrgdck\\\"cprc\\xadczyarbznqmuhxyuh\"","\"uqdxnuwioc\\\"kdytxq\\\\ig\"","\"xrafmucpmfi\"","\"vr\\\"hltmfrge\"","\"eonf\\\"nt\\\\wtcnsocs\"","\"j\\xb7xoslyjeyjksplkqixncgkylkw\"","\"njw\\\"pefgfbez\\x9axshdmplxzquqe\"","\"di\\x58bvptfsafirpc\"","\"l\\x1fkco\"","\"x\"","\"mprndo\\\"n\"","\"psegit\"","\"svbdnkkuuqs\\\"sqxu\\\"oqcyz\\\"aizashk\"","\"cwkljukxer\\\\\\\"\\\\nff\\\"esjwiyaoy\"","\"ilxrkgbjjxpvhdtq\\\"cpiuoofdnkpp\"","\"hlngi\\\"ulxep\\\\qohtmqnqjb\\\"rkgerho\"","\"gxws\\\"bcgm\\\"p\"","\"bv\\\"mds\\\\zhfusiepgrz\\\\b\\x32fscdzz\"","\"l\\xfampwtme\\x69qvxnx\\\"\\\"\\xc4jruuymjxrpsv\"","\"qqmxhrn\"","\"xziq\\\\\\x18ybyv\\x9am\\\"neacoqjzytertisysza\"","\"aqcbvlvcrzceeyx\\\\j\\\"\\\"x\"","\"yjuhhb\"","\"\\x5em\\\"squulpy\"","\"dpbntplgmwb\"","\"utsgfkm\\\\vbftjknlktpthoeo\"","\"ccxjgiocmuhf\\\"ycnh\"","\"ixbbk\"\\jtll\""]

strs2 :: [String]
strs2 = split multi

sameStr :: Bool
sameStr = strs == strs2


--  chars in string
-- drop outside " two quotes first as they do not contribute to string "
cc2 :: String -> Int -> Int
cc2 [] n = n
cc2 ('\\' : '"' : t) n = cc2 t (n + 1)
cc2 ('\\' : '\\' : t) n = cc2 t (n + 1)
cc2 ('\\' : 'x' : _ : _ : t) n = cc2 t (n + 1)
cc2 (h : t) n = cc2 t (n + 1)


-- remove count outer quote characters 
charCount s = let all = cc2 s 0
              in all - 2
                 

-- bytes in string
bc :: String -> Int -> Int
bc [] n = n
bc ('\\' : '"' : t) n = bc t (n + 2)
bc ('\\' : '\\' : t) n = bc t (n + 2)
bc ('\\' : 'x' : _ : _ : t) n = bc t (n + 4)
bc (h : t) n = bc t (n + 1)

byteCount s = bc s 0

byteCount2 s = length s

byteSum = foldr (+) 0 (map byteCount strs)
charSum = foldr (+) 0 (map charCount strs)
diffSum = byteSum - charSum

someFunc :: IO ()
someFunc = do putStrLn ("From MyLib2 ... " ++ " ... " ++ " ... "  )
              putStrLn ("byte sum " ++ show byteSum ++ " ... "  )
              putStrLn ("char sum " ++ show charSum ++ " ... "  )
              putStrLn ("proposed solution  " ++ show diffSum ++ " ... "  )

{-
ghci> map byteCount strs
[38,6,27,10,6,28,6,36,9,12,11,9,10,16,22,7,18,38,18,11,21,17,14,13,6,39,32,4,12,22,28,25,14,19,28,34,11,33,26,37,37,35,35,25,29,12,30,32,15,7,34,8,17,12,30,7,5,6,4,7,12,43,4,16,27,16,17,41,25,17,36,13,16,19,29,30,22,13,16,19,22,21,3,34,35,23,10,39,24,32,35,26,29,7,27,29,3,30,30,25,34,21,5,35,13,36,25,11,18,4,4,40,28,33,25,8,3,20,34,16,34,29,23,23,26,32,17,24,31,31,30,25,32,12,8,23,7,29,17,13,39,13,35,31,14,23,22,8,5,29,35,13,23,4,19,3,16,39,26,13,10,31,35,6,8,25,38,7,23,39,5,30,20,5,18,15,24,19,6,26,29,42,10,22,23,26,15,30,12,19,29,9,36,11,12,18,36,6,21,30,15,22,33,7,15,11,13,5,35,34,3,8,38,14,34,38,19,19,6,17,25,30,10,23,8,5,32,5,31,34,33,8,17,36,38,9,8,20,10,26,36,6,5,11,12,6,11,38,32,3,21,15,38,30,21,28,24,13,36,38,18,16,28,37,15,29,8,28,16,31,35,17,38,24,13,14,20,32,32,20,10,3,11,8,35,32,32,35,15,35,42,9,42,25,8,16,13,27,20,13]
ghci> map byteCount2 strs
[38,6,27,10,6,28,6,36,9,12,11,9,10,16,22,7,18,38,18,11,21,17,14,13,6,39,32,4,12,22,28,25,14,19,28,34,11,33,26,37,37,35,35,25,29,12,30,32,15,7,34,8,17,12,30,7,5,6,4,7,12,43,4,16,27,16,17,41,25,17,36,13,16,19,29,30,22,13,16,19,22,21,3,34,35,23,10,39,24,32,35,26,29,7,27,29,3,30,30,25,34,21,5,35,13,36,25,11,18,4,4,40,28,33,25,8,3,20,34,16,34,29,23,23,26,32,17,24,31,31,30,25,32,12,8,23,7,29,17,13,39,13,35,31,14,23,22,8,5,29,35,13,23,4,19,3,16,39,26,13,10,31,35,6,8,25,38,7,23,39,5,30,20,5,18,15,24,19,6,26,29,42,10,22,23,26,15,30,12,19,29,9,36,11,12,18,36,6,21,30,15,22,33,7,15,11,13,5,35,34,3,8,38,14,34,38,19,19,6,17,25,30,10,23,8,5,32,5,31,34,33,8,17,36,38,9,8,20,10,26,36,6,5,11,12,6,11,38,32,3,21,15,38,30,21,28,24,13,36,38,18,16,28,37,15,29,8,28,16,31,35,17,38,24,13,14,20,32,32,20,10,3,11,8,35,32,32,35,15,35,42,9,42,25,8,16,13,27,20,13]

ghci> map byteCount2 strs == map byteCount strs
True

byte count is just the length of the visible characters
file contains invisible newlines or white space they have already been stripped
so byte count just needs to map over strs

total byte count = foldl 0 ( map byteCount strs ) 

fold left
ghci> foldl (+) 0 (map byteCount strs)
6310

fold-right
ghci> foldr (+) 0 (map byteCount strs)
6310



-}
