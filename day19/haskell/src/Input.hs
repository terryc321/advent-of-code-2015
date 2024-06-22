
--module Input (subs,mode) where
-- or can silently export everything 
module Input (subs,mole) where

-- given substitutions
subs :: [(String,String)]
subs = [("Al" , "ThF"),
        ("Al" , "ThRnFAr"),
        ("B" , "BCa"),
        ("B" , "TiB"),
        ("B" , "TiRnFAr"),
        ("Ca" , "CaCa"),
        ("Ca" , "PB"),
        ("Ca" , "PRnFAr"),
        ("Ca" , "SiRnFYFAr"),
        ("Ca" , "SiRnMgAr"),
        ("Ca" , "SiTh"),
        ("F" , "CaF"),
        ("F" , "PMg"),
        ("F" , "SiAl"),
        ("H" , "CRnAlAr"),
        ("H" , "CRnFYFYFAr"),
        ("H" , "CRnFYMgAr"),
        ("H" , "CRnMgYFAr"),
        ("H" , "HCa"),
        ("H" , "NRnFYFAr"),
        ("H" , "NRnMgAr"),
        ("H" , "NTh"),
        ("H" , "OB"),
        ("H" , "ORnFAr"),
        ("Mg" , "BF"),
        ("Mg" , "TiMg"),
        ("N" , "CRnFAr"),
        ("N" , "HSi"),
        ("O" , "CRnFYFAr"),
        ("O" , "CRnMgAr"),
        ("O" , "HP"),
        ("O" , "NRnFAr"),
        ("O" , "OTi"),
        ("P" , "CaP"),
        ("P" , "PTi"),
        ("P" , "SiRnFAr"),
        ("Si" , "CaSi"),
        ("Th" , "ThCa"),
        ("Ti" , "BP"),
        ("Ti" , "TiTi"),
        ("e" , "HF"),
        ("e" , "NAl"),
        ("e" , "OMg")]
       

--- given molecule
mole :: String
mole = "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF"

