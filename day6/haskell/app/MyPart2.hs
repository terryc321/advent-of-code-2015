

module MyPart2 (rec) where

--- certain weakness haskell cannot show function (Bulb -> Bulb) 
input :: [Bulb -> Bulb]
input = [rangeToggle (Rg (Pt 461 550) (Pt 564 900)) , rangeOff (Rg (Pt 370 39) (Pt 425 839)) , rangeOff (Rg (Pt 464 858) (Pt 833 915)) , rangeOff (Rg (Pt 812 389) (Pt 865 874)) , rangeOn (Rg (Pt 599 989) (Pt 806 993)) , rangeOn (Rg (Pt 376 415) (Pt 768 548)) , rangeOn (Rg (Pt 606 361) (Pt 892 600)) , rangeOff (Rg (Pt 448 208) (Pt 645 684)) , rangeToggle (Rg (Pt 50 472) (Pt 452 788)) , rangeToggle (Rg (Pt 205 417) (Pt 703 826)) , rangeToggle (Rg (Pt 533 331) (Pt 906 873)) , rangeToggle (Rg (Pt 857 493) (Pt 989 970)) , rangeOff (Rg (Pt 631 950) (Pt 894 975)) , rangeOff (Rg (Pt 387 19) (Pt 720 700)) , rangeOff (Rg (Pt 511 843) (Pt 581 945)) , rangeToggle (Rg (Pt 514 557) (Pt 662 883)) , rangeOff (Rg (Pt 269 809) (Pt 876 847)) , rangeOff (Rg (Pt 149 517) (Pt 716 777)) , rangeOff (Rg (Pt 994 939) (Pt 998 988)) , rangeToggle (Rg (Pt 467 662) (Pt 555 957)) , rangeOn (Rg (Pt 952 417) (Pt 954 845)) , rangeOn (Rg (Pt 565 226) (Pt 944 880)) , rangeOn (Rg (Pt 214 319) (Pt 805 722)) , rangeToggle (Rg (Pt 532 276) (Pt 636 847)) , rangeToggle (Rg (Pt 619 80) (Pt 689 507)) , rangeOn (Rg (Pt 390 706) (Pt 884 722)) , rangeToggle (Rg (Pt 17 634) (Pt 537 766)) , rangeToggle (Rg (Pt 706 440) (Pt 834 441)) , rangeToggle (Rg (Pt 318 207) (Pt 499 530)) , rangeToggle (Rg (Pt 698 185) (Pt 830 343)) , rangeToggle (Rg (Pt 566 679) (Pt 744 716)) , rangeToggle (Rg (Pt 347 482) (Pt 959 482)) , rangeToggle (Rg (Pt 39 799) (Pt 981 872)) , rangeOn (Rg (Pt 583 543) (Pt 846 710)) , rangeOff (Rg (Pt 367 664) (Pt 595 872)) , rangeOn (Rg (Pt 805 439) (Pt 964 995)) , rangeToggle (Rg (Pt 209 584) (Pt 513 802)) , rangeOff (Rg (Pt 106 497) (Pt 266 770)) , rangeOn (Rg (Pt 975 2) (Pt 984 623)) , rangeOff (Rg (Pt 316 684) (Pt 369 876)) , rangeOff (Rg (Pt 30 309) (Pt 259 554)) , rangeOff (Rg (Pt 399 680) (Pt 861 942)) , rangeToggle (Rg (Pt 227 740) (Pt 850 829)) , rangeOn (Rg (Pt 386 603) (Pt 552 879)) , rangeOff (Rg (Pt 703 795) (Pt 791 963)) , rangeOff (Rg (Pt 573 803) (Pt 996 878)) , rangeOff (Rg (Pt 993 939) (Pt 997 951)) , rangeOn (Rg (Pt 809 221) (Pt 869 723)) , rangeOff (Rg (Pt 38 720) (Pt 682 751)) , rangeOff (Rg (Pt 318 732) (Pt 720 976)) , rangeToggle (Rg (Pt 88 459) (Pt 392 654)) , rangeOff (Rg (Pt 865 654) (Pt 911 956)) , rangeToggle (Rg (Pt 264 284) (Pt 857 956)) , rangeOff (Rg (Pt 281 776) (Pt 610 797)) , rangeToggle (Rg (Pt 492 660) (Pt 647 910)) , rangeOff (Rg (Pt 879 703) (Pt 925 981)) , rangeOff (Rg (Pt 772 414) (Pt 974 518)) , rangeOn (Rg (Pt 694 41) (Pt 755 96)) , rangeOn (Rg (Pt 452 406) (Pt 885 881)) , rangeOff (Rg (Pt 107 905) (Pt 497 910)) , rangeOff (Rg (Pt 647 222) (Pt 910 532)) , rangeOn (Rg (Pt 679 40) (Pt 845 358)) , rangeOff (Rg (Pt 144 205) (Pt 556 362)) , rangeOn (Rg (Pt 871 804) (Pt 962 878)) , rangeOn (Rg (Pt 545 676) (Pt 545 929)) , rangeOff (Rg (Pt 316 716) (Pt 413 941)) , rangeToggle (Rg (Pt 488 826) (Pt 755 971)) , rangeToggle (Rg (Pt 957 832) (Pt 976 992)) , rangeToggle (Rg (Pt 857 770) (Pt 905 964)) , rangeToggle (Rg (Pt 319 198) (Pt 787 673)) , rangeOn (Rg (Pt 832 813) (Pt 863 844)) , rangeOn (Rg (Pt 818 296) (Pt 818 681)) , rangeOn (Rg (Pt 71 699) (Pt 91 960)) , rangeOff (Rg (Pt 838 578) (Pt 967 928)) , rangeToggle (Rg (Pt 440 856) (Pt 507 942)) , rangeToggle (Rg (Pt 121 970) (Pt 151 974)) , rangeToggle (Rg (Pt 391 192) (Pt 659 751)) , rangeOn (Rg (Pt 78 210) (Pt 681 419)) , rangeOn (Rg (Pt 324 591) (Pt 593 939)) , rangeToggle (Rg (Pt 159 366) (Pt 249 760)) , rangeOff (Rg (Pt 617 167) (Pt 954 601)) , rangeToggle (Rg (Pt 484 607) (Pt 733 657)) , rangeOn (Rg (Pt 587 96) (Pt 888 819)) , rangeOff (Rg (Pt 680 984) (Pt 941 991)) , rangeOn (Rg (Pt 800 512) (Pt 968 691)) , rangeOff (Rg (Pt 123 588) (Pt 853 603)) , rangeOn (Rg (Pt 1 862) (Pt 507 912)) , rangeOn (Rg (Pt 699 839) (Pt 973 878)) , rangeOff (Rg (Pt 848 89) (Pt 887 893)) , rangeToggle (Rg (Pt 344 353) (Pt 462 403)) , rangeOn (Rg (Pt 780 731) (Pt 841 760)) , rangeToggle (Rg (Pt 693 973) (Pt 847 984)) , rangeToggle (Rg (Pt 989 936) (Pt 996 958)) , rangeToggle (Rg (Pt 168 475) (Pt 206 963)) , rangeOn (Rg (Pt 742 683) (Pt 769 845)) , rangeToggle (Rg (Pt 768 116) (Pt 987 396)) , rangeOn (Rg (Pt 190 364) (Pt 617 526)) , rangeOff (Rg (Pt 470 266) (Pt 530 839)) , rangeToggle (Rg (Pt 122 497) (Pt 969 645)) , rangeOff (Rg (Pt 492 432) (Pt 827 790)) , rangeOn (Rg (Pt 505 636) (Pt 957 820)) , rangeOn (Rg (Pt 295 476) (Pt 698 958)) , rangeToggle (Rg (Pt 63 298) (Pt 202 396)) , rangeOn (Rg (Pt 157 315) (Pt 412 939)) , rangeOff (Rg (Pt 69 789) (Pt 134 837)) , rangeOff (Rg (Pt 678 335) (Pt 896 541)) , rangeToggle (Rg (Pt 140 516) (Pt 842 668)) , rangeOff (Rg (Pt 697 585) (Pt 712 668)) , rangeToggle (Rg (Pt 507 832) (Pt 578 949)) , rangeOn (Rg (Pt 678 279) (Pt 886 621)) , rangeToggle (Rg (Pt 449 744) (Pt 826 910)) , rangeOff (Rg (Pt 835 354) (Pt 921 741)) , rangeToggle (Rg (Pt 924 878) (Pt 985 952)) , rangeOn (Rg (Pt 666 503) (Pt 922 905)) , rangeOn (Rg (Pt 947 453) (Pt 961 587)) , rangeToggle (Rg (Pt 525 190) (Pt 795 654)) , rangeOff (Rg (Pt 62 320) (Pt 896 362)) , rangeOn (Rg (Pt 21 458) (Pt 972 536)) , rangeOn (Rg (Pt 446 429) (Pt 821 970)) , rangeToggle (Rg (Pt 376 423) (Pt 805 455)) , rangeToggle (Rg (Pt 494 896) (Pt 715 937)) , rangeOn (Rg (Pt 583 270) (Pt 667 482)) , rangeOff (Rg (Pt 183 468) (Pt 280 548)) , rangeToggle (Rg (Pt 623 289) (Pt 750 524)) , rangeOn (Rg (Pt 836 706) (Pt 967 768)) , rangeOn (Rg (Pt 419 569) (Pt 912 908)) , rangeOn (Rg (Pt 428 260) (Pt 660 433)) , rangeOff (Rg (Pt 683 627) (Pt 916 816)) , rangeOn (Rg (Pt 447 973) (Pt 866 980)) , rangeOn (Rg (Pt 688 607) (Pt 938 990)) , rangeOn (Rg (Pt 245 187) (Pt 597 405)) , rangeOff (Rg (Pt 558 843) (Pt 841 942)) , rangeOff (Rg (Pt 325 666) (Pt 713 834)) , rangeToggle (Rg (Pt 672 606) (Pt 814 935)) , rangeOff (Rg (Pt 161 812) (Pt 490 954)) , rangeOn (Rg (Pt 950 362) (Pt 985 898)) , rangeOn (Rg (Pt 143 22) (Pt 205 821)) , rangeOn (Rg (Pt 89 762) (Pt 607 790)) , rangeToggle (Rg (Pt 234 245) (Pt 827 303)) , rangeOn (Rg (Pt 65 599) (Pt 764 997)) , rangeOn (Rg (Pt 232 466) (Pt 965 695)) , rangeOn (Rg (Pt 739 122) (Pt 975 590)) , rangeOff (Rg (Pt 206 112) (Pt 940 558)) , rangeToggle (Rg (Pt 690 365) (Pt 988 552)) , rangeOn (Rg (Pt 907 438) (Pt 977 691)) , rangeOff (Rg (Pt 838 809) (Pt 944 869)) , rangeOn (Rg (Pt 222 12) (Pt 541 832)) , rangeToggle (Rg (Pt 337 66) (Pt 669 812)) , rangeOn (Rg (Pt 732 821) (Pt 897 912)) , rangeToggle (Rg (Pt 182 862) (Pt 638 996)) , rangeOn (Rg (Pt 955 808) (Pt 983 847)) , rangeToggle (Rg (Pt 346 227) (Pt 841 696)) , rangeOn (Rg (Pt 983 270) (Pt 989 756)) , rangeOff (Rg (Pt 874 849) (Pt 876 905)) , rangeOff (Rg (Pt 7 760) (Pt 678 795)) , rangeToggle (Rg (Pt 973 977) (Pt 995 983)) , rangeOff (Rg (Pt 911 961) (Pt 914 976)) , rangeOn (Rg (Pt 913 557) (Pt 952 722)) , rangeOff (Rg (Pt 607 933) (Pt 939 999)) , rangeOn (Rg (Pt 226 604) (Pt 517 622)) , rangeOff (Rg (Pt 3 564) (Pt 344 842)) , rangeToggle (Rg (Pt 340 578) (Pt 428 610)) , rangeOn (Rg (Pt 248 916) (Pt 687 925)) , rangeToggle (Rg (Pt 650 185) (Pt 955 965)) , rangeToggle (Rg (Pt 831 359) (Pt 933 536)) , rangeOff (Rg (Pt 544 614) (Pt 896 953)) , rangeToggle (Rg (Pt 648 939) (Pt 975 997)) , rangeOn (Rg (Pt 464 269) (Pt 710 521)) , rangeOff (Rg (Pt 643 149) (Pt 791 320)) , rangeOff (Rg (Pt 875 549) (Pt 972 643)) , rangeOff (Rg (Pt 953 969) (Pt 971 972)) , rangeOff (Rg (Pt 236 474) (Pt 772 591)) , rangeToggle (Rg (Pt 313 212) (Pt 489 723)) , rangeToggle (Rg (Pt 896 829) (Pt 897 837)) , rangeToggle (Rg (Pt 544 449) (Pt 995 905)) , rangeOff (Rg (Pt 278 645) (Pt 977 876)) , rangeOff (Rg (Pt 887 947) (Pt 946 977)) , rangeOn (Rg (Pt 342 861) (Pt 725 935)) , rangeOn (Rg (Pt 636 316) (Pt 692 513)) , rangeToggle (Rg (Pt 857 470) (Pt 950 528)) , rangeOff (Rg (Pt 736 196) (Pt 826 889)) , rangeOn (Rg (Pt 17 878) (Pt 850 987)) , rangeOn (Rg (Pt 142 968) (Pt 169 987)) , rangeOn (Rg (Pt 46 470) (Pt 912 853)) , rangeOn (Rg (Pt 182 252) (Pt 279 941)) , rangeToggle (Rg (Pt 261 143) (Pt 969 657)) , rangeOff (Rg (Pt 69 600) (Pt 518 710)) , rangeOn (Rg (Pt 372 379) (Pt 779 386)) , rangeToggle (Rg (Pt 867 391) (Pt 911 601)) , rangeOff (Rg (Pt 174 287) (Pt 900 536)) , rangeToggle (Rg (Pt 951 842) (Pt 993 963)) , rangeOff (Rg (Pt 626 733) (Pt 985 827)) , rangeToggle (Rg (Pt 622 70) (Pt 666 291)) , rangeOff (Rg (Pt 980 671) (Pt 985 835)) , rangeOff (Rg (Pt 477 63) (Pt 910 72)) , rangeOff (Rg (Pt 779 39) (Pt 940 142)) , rangeOn (Rg (Pt 986 570) (Pt 997 638)) , rangeToggle (Rg (Pt 842 805) (Pt 943 985)) , rangeOff (Rg (Pt 890 886) (Pt 976 927)) , rangeOff (Rg (Pt 893 172) (Pt 897 619)) , rangeOff (Rg (Pt 198 780) (Pt 835 826)) , rangeToggle (Rg (Pt 202 209) (Pt 219 291)) , rangeOff (Rg (Pt 193 52) (Pt 833 283)) , rangeToggle (Rg (Pt 414 427) (Pt 987 972)) , rangeOn (Rg (Pt 375 231) (Pt 668 236)) , rangeOff (Rg (Pt 646 598) (Pt 869 663)) , rangeToggle (Rg (Pt 271 462) (Pt 414 650)) , rangeOff (Rg (Pt 679 121) (Pt 845 467)) , rangeToggle (Rg (Pt 76 847) (Pt 504 904)) , rangeOff (Rg (Pt 15 617) (Pt 509 810)) , rangeToggle (Rg (Pt 248 105) (Pt 312 451)) , rangeOff (Rg (Pt 126 546) (Pt 922 879)) , rangeOn (Rg (Pt 531 831) (Pt 903 872)) , rangeToggle (Rg (Pt 602 431) (Pt 892 792)) , rangeOff (Rg (Pt 795 223) (Pt 892 623)) , rangeToggle (Rg (Pt 167 721) (Pt 533 929)) , rangeToggle (Rg (Pt 813 251) (Pt 998 484)) , rangeToggle (Rg (Pt 64 640) (Pt 752 942)) , rangeOn (Rg (Pt 155 955) (Pt 892 985)) , rangeOn (Rg (Pt 251 329) (Pt 996 497)) , rangeOff (Rg (Pt 341 716) (Pt 462 994)) , rangeToggle (Rg (Pt 760 127) (Pt 829 189)) , rangeOn (Rg (Pt 86 413) (Pt 408 518)) , rangeToggle (Rg (Pt 340 102) (Pt 918 558)) , rangeOff (Rg (Pt 441 642) (Pt 751 889)) , rangeOn (Rg (Pt 785 292) (Pt 845 325)) , rangeOff (Rg (Pt 123 389) (Pt 725 828)) , rangeOn (Rg (Pt 905 73) (Pt 983 270)) , rangeOff (Rg (Pt 807 86) (Pt 879 276)) , rangeToggle (Rg (Pt 500 866) (Pt 864 916)) , rangeOn (Rg (Pt 809 366) (Pt 828 534)) , rangeToggle (Rg (Pt 219 356) (Pt 720 617)) , rangeOff (Rg (Pt 320 964) (Pt 769 990)) , rangeOff (Rg (Pt 903 167) (Pt 936 631)) , rangeToggle (Rg (Pt 300 137) (Pt 333 693)) , rangeToggle (Rg (Pt 5 675) (Pt 755 848)) , rangeOff (Rg (Pt 852 235) (Pt 946 783)) , rangeToggle (Rg (Pt 355 556) (Pt 941 664)) , rangeOn (Rg (Pt 810 830) (Pt 867 891)) , rangeOff (Rg (Pt 509 869) (Pt 667 903)) , rangeToggle (Rg (Pt 769 400) (Pt 873 892)) , rangeOn (Rg (Pt 553 614) (Pt 810 729)) , rangeOn (Rg (Pt 179 873) (Pt 589 962)) , rangeOff (Rg (Pt 466 866) (Pt 768 926)) , rangeToggle (Rg (Pt 143 943) (Pt 465 984)) , rangeToggle (Rg (Pt 182 380) (Pt 569 552)) , rangeOff (Rg (Pt 735 808) (Pt 917 910)) , rangeOn (Rg (Pt 731 802) (Pt 910 847)) , rangeOff (Rg (Pt 522 74) (Pt 731 485)) , rangeOn (Rg (Pt 444 127) (Pt 566 996)) , rangeOff (Rg (Pt 232 962) (Pt 893 979)) , rangeOff (Rg (Pt 231 492) (Pt 790 976)) , rangeOn (Rg (Pt 874 567) (Pt 943 684)) , rangeToggle (Rg (Pt 911 840) (Pt 990 932)) , rangeToggle (Rg (Pt 547 895) (Pt 667 935)) , rangeOff (Rg (Pt 93 294) (Pt 648 636)) , rangeOff (Rg (Pt 190 902) (Pt 532 970)) , rangeOff (Rg (Pt 451 530) (Pt 704 613)) , rangeToggle (Rg (Pt 936 774) (Pt 937 775)) , rangeOff (Rg (Pt 116 843) (Pt 533 934)) , rangeOn (Rg (Pt 950 906) (Pt 986 993)) , rangeOn (Rg (Pt 910 51) (Pt 945 989)) , rangeOn (Rg (Pt 986 498) (Pt 994 945)) , rangeOff (Rg (Pt 125 324) (Pt 433 704)) , rangeOff (Rg (Pt 60 313) (Pt 75 728)) , rangeOn (Rg (Pt 899 494) (Pt 940 947)) , rangeToggle (Rg (Pt 832 316) (Pt 971 817)) , rangeToggle (Rg (Pt 994 983) (Pt 998 984)) , rangeToggle (Rg (Pt 23 353) (Pt 917 845)) , rangeToggle (Rg (Pt 174 799) (Pt 658 859)) , rangeOff (Rg (Pt 490 878) (Pt 534 887)) , rangeOff (Rg (Pt 623 963) (Pt 917 975)) , rangeToggle (Rg (Pt 721 333) (Pt 816 975)) , rangeToggle (Rg (Pt 589 687) (Pt 890 921)) , rangeOn (Rg (Pt 936 388) (Pt 948 560)) , rangeOff (Rg (Pt 485 17) (Pt 655 610)) , rangeOn (Rg (Pt 435 158) (Pt 689 495)) , rangeOn (Rg (Pt 192 934) (Pt 734 936)) , rangeOff (Rg (Pt 299 723) (Pt 622 847)) , rangeToggle (Rg (Pt 484 160) (Pt 812 942)) , rangeOff (Rg (Pt 245 754) (Pt 818 851)) , rangeOn (Rg (Pt 298 419) (Pt 824 634)) , rangeToggle (Rg (Pt 868 687) (Pt 969 760)) , rangeToggle (Rg (Pt 131 250) (Pt 685 426)) , rangeOff (Rg (Pt 201 954) (Pt 997 983)) , rangeOn (Rg (Pt 353 910) (Pt 832 961)) , rangeOff (Rg (Pt 518 781) (Pt 645 875)) , rangeOff (Rg (Pt 866 97) (Pt 924 784)) , rangeToggle (Rg (Pt 836 599) (Pt 857 767)) , rangeOn (Rg (Pt 80 957) (Pt 776 968)) , rangeToggle (Rg (Pt 277 130) (Pt 513 244)) , rangeOff (Rg (Pt 62 266) (Pt 854 434)) , rangeOn (Rg (Pt 792 764) (Pt 872 842)) , rangeOff (Rg (Pt 160 949) (Pt 273 989)) , rangeOff (Rg (Pt 664 203) (Pt 694 754)) , rangeToggle (Rg (Pt 491 615) (Pt 998 836)) , rangeOff (Rg (Pt 210 146) (Pt 221 482)) , rangeOff (Rg (Pt 209 780) (Pt 572 894)) , rangeOn (Rg (Pt 766 112) (Pt 792 868)) , rangeOn (Rg (Pt 222 12) (Pt 856 241))  ]


--- Point type with constructor called Point
data Point = Pt Int Int

instance Show Point where
  show (Pt x y) = "Pt(" ++ show x ++ "," ++ show y ++ ")"

instance Eq Point where
  (Pt x y) == (Pt x2 y2) = ( x == x2 && y == y2 )

--- range takes two points
data Range = Rg Point Point

instance Show Range where
  show (Rg x y) = "Rg(" ++ show x ++ " -> " ++ show y ++ ")"

instance Eq Range where
  (Rg x y) == (Rg x2 y2) = ( x == x2 && y == y2 )

-- no way prevent a bad range being constructed
-- but we can let mkRange do this for us ,be aware
-- mkRange :: Point -> Point -> Range
-- mkRange p1 p2 = let (x1,y1) = p1
--                     (x2,y2) = p2
--                 in let lo = (Pt (minimum [x1,x2]) (minimum [y1,y2]))
--                        hi = (Pt (maximum [x1,x2]) (maximum [y1,y2]))
--                        in Rg lo hi

-- loRange :: Point -> Point -> Point
-- loRange p1 p2 = let (x1,y1) = p1
--                     (x2,y2) = p2
--                 in let lo = Pt (minimum [x1,x2]) (minimum [y1,y2])
--                        hi = Pt (maximum [x1,x2]) (maximum [y1,y2])
--                        in lo

loRange :: Point -> Point -> Point
loRange (Pt x1 y1) (Pt x2 y2) = Pt (minimum [x1,x2]) (minimum [y1,y2])

hiRange :: Point -> Point -> Point
hiRange (Pt x1 y1) (Pt x2 y2) = Pt (maximum [x1,x2]) (maximum [y1,y2])

mkRange :: Point -> Point -> Range
mkRange p1 p2 = let a = loRange p1 p2
                    b = hiRange p1 p2
                in Rg a b

-- test = let (x,y) = (2,3)
--            (x2,y2) = (4 ,5)
--        in [x,x,y,y,x2,x2,y2,y2]

inRange :: Range -> Point -> Bool
inRange (Rg (Pt x1 y1) (Pt x2 y2)) (Pt x3 y3) =
     (x3 >= x1 && x3 <= x2) && (y3 >= y1 && y3 <= y2)

-- ----
-- data BulbBool = On | Off
-- instance Show BulbBool where
--   show On = "on"
--   show Off = "off"
-- 
-- instance Eq BulbBool where
--   On == On = True
--   On == Off = False
--   Off == On = False
--   Off == Off = True
-- ----

--- This bulb is either On True or Off
--- now bulb has a brightness Int 0 off to some positive value
data Bulb = Bulb Point Int

instance Show Bulb where
  show (Bulb p b) = "Bulb(" ++ show p ++ "," ++ show b ++ ")"

instance Eq Bulb where
  (Bulb x y) == (Bulb x2 y2) = ( x == x2 && y == y2 )
----
-- instance Show (Bulb -> Bulb) where
--   show a = "Bulbfn{ bulb -> bulb }"
  

-- tried making rangeOff work on a few parameters like ocaml
-- but it wanted them all together

-- Off now means to decrease if in range
rangeOff :: Range -> ( Bulb -> (Bulb ))
rangeOff = \r -> \bu -> case bu of
                        (Bulb p b) -> case inRange r p of
                                      True -> Bulb p (maximum [0 ,(b - 1)])
                                      False -> Bulb p b

--- increase brightness by 1 if in range
rangeOn :: Range -> Bulb -> Bulb
rangeOn r (Bulb p b) = if inRange r p then Bulb p (b + 1)
                                      else Bulb p b


-- increase brightness by 2 if in range
rangeToggle :: Range -> Bulb -> Bulb
rangeToggle r (Bulb p b) = if inRange r p then Bulb p (b + 2)
                           else Bulb p b
                                

-- given a bulb initially off at position x , y
-- what is final state of bulb ?
-- given a list of procedures either toggle bulb on/off, turn bulb on, turn bulb off
-- return me the bulb after been through all these functions
tunnel :: [Bulb -> Bulb] -> Bulb -> Bulb
tunnel [] b = b
tunnel (f : xs) b = tunnel xs (f b)


ex1 :: Bulb
ex1 = tunnel input (Bulb (Pt 0 0) 0)

-- vary x from 0 to 999 inclusive - so thousand wide
-- vary y from 0 to 999 inclusive - so thousand high
-- list comprehensions .... build a long list tho , we dont want that
-- matrix :: [(Int,Int)]
-- matrix = [ (x,y) | x <- [0 .. 999]  , y <- [0 .. 999]]
-- 
-- after :: [Bulb]
-- after = [ tunnel input (Bulb (Pt x y) Off) | (x,y) <- matrix ]
-- 
-- solution = length $ filter (\(Bulb _ s) -> s == On) after

        
-- rec2 :: Int -> Int -> Int
rec2 x y f s = if x > 999 then rec2 0 (y + 1) f s
               else if y > 999 then s
                    else rec2 (x + 1) y f (f x y s)


-- rec :: Integer
rec = let x = 0
          y = 0
          count = 0
          fn = \x1 -> \y1 -> \s1 -> let out = (tunnel input (Bulb (Pt x1 y1) 0))
                                    in case out of
                                       (Bulb _ b) -> s1 + b
      in rec2 x y fn count




{-

Hello, Haskell!
solution 2 = [ 14687245 ] 

real	0m3.215s
user	0m3.029s
sys	0m0.184s


-}




       
       
