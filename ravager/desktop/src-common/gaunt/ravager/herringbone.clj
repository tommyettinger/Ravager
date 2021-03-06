(ns gaunt.ravager.herringbone)

(def filler [
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
             ])
(def horiz [
  ["##......%####.....##"
   "###.......###.....##"
   "..##......###%##...."
   "...#####..##........"
   "...#####..##........"
   "...#####..##....##.."
   "....#%....##..#%##.."
   "..........##....####"
   "##.....#####.....###"
   "##.....#####......##"]

  ["##..##%######.....##"
   "##..#$~$##%##.....%#"
   "##..#~~~#...#......."
   "##..#~~~#...####...."
   "##..#$~$#...#~~#...."
   "##..%~~~%...%~~%...."
   "...................."
   "..................##"
   "####..######......##"
   "####..######..######"]

  ["##......##%##.....##"
   "#%......#$~$#.....##"
   "........#~~~#......."
   "........#~~~#......."
   "...#..###~~~####..##"
   "...#..###$~~%###..##"
   "...#..###~~~$$~~~~##"
   "...#..###~~~~~~~~~##"
   "####..###$~~~~~$~$##"
   "####..######..######"]

  ["##..####%####.....##"
   "##..###$~~$%....####"
   "##....#$~~~........."
   "##....#~~~~........."
   "####..#$~~$#..###%##"
   "###%..######..%$~~$#"
   "......#........~~~$#"
   "......#........~~~~#"
   "####..######..#$~~$#"
   "####..######..######"]

  ["##..##%###%###%#..##"
   "##................##"
   ".##................."
   "..######..######...."
   "...##%##..#%####...."
   ".............####..."
   "...............%##.."
   "......#####.....####"
   "####..######.....###"
   "####..######......##"]

  ["##......##........##"
   "##......##%#......%#"
   "##..##%##~~~~~~#...."
   "##..~~~$#~~~~~~#...."
   "##..~~~$#~~###%#...."
   "##..#%###~~#........"
   "........#~~#........"
   "........#$$#..##...#"
   "#####..#####..##...#"
   "##.....#####..######"]

  ["##..####%#######..##"
   "#%..#$~$~~$~~$##..##"
   "....#~~~~~~~~##....."
   "....#~~~~~~~##......"
   "....#$~~~~$##%...###"
   "....###..####...####"
   "...............#####"
   "..............######"
   "####..######..######"
   "####..######..######"]

  ["##..#####%######..##"
   "##..###$~~~$%###..##"
   "##..##$.....~$##...."
   "##..##$......~##...."
   "##..###~.....$##..##"
   "#%...###~~~~$###..##"
   "......##..###%##..##"
   ".......#..........##"
   "##.....#..........##"
   "##.....#####..######"]

  ["###..########.....##"
   "#..................#"
   "...................."
   ".....########......."
   "...#############...."
   "...%###########%...."
   ".....%######%......."
   "...................#"
   "#..................#"
   "####..######......##"]

  ["##......#%###.....##"
   "###....##~+##......#"
   "###.....~~+##......."
   "##......~~+##......."
   "##...########......."
   "##..%####%###......."
   "......##$~~##......#"
   "......##$~~~.......#"
   "####..##$~~~.......#"
   "####..######..######"]

  ["##..####%#%##.....##"
   "#...##~$~~~$#......#"
   "....##$~~~~~#......."
   "....##~~~~~~........"
   "....##$~~~~~.....###"
   "....####~~~~########"
   "....%###$~~~%...####"
   "......##~~~~....####"
   "#.....##$~~~......##"
   "####..######......##"]

  ["##......#%###.....##"
   "###........##......#"
   "####.......##..#...."
   "########...##..##..."
   "######%#...##..#####"
   "##%###....##...%####"
   "...##....##%....####"
   "........###......###"
   "#......###........##"
   "##.....#####......##"]

  ["#%..#########%##..##"
   "#....########......#"
   "......%####%........"
   ".......####........."
   "........##....###..."
   "..##....##....####.."
   "..##....##....#####."
   ".###...####...######"
   "####...####...######"
   "####..######..######"]

  ["##......####%###..##"
   "#.......##$~~~$#...#"
   "#.......##$~~~~#...."
   "#.......##~~~~~....."
   "#..#######~~~~~....."
   "#..##%####..%###...."
   "..............##...."
   "..............######"
   "####..######..######"
   "####..######..######"]

  ["##......########..##"
   "###.....######%#...#"
   ".###...######......."
   "..#%..######........"
   "......####.....#####"
   "......####...#%#++##"
   "..##..%##.........+#"
   ".###...##..........#"
   "###....##.........+#"
   "##.....#####......##"]

  ["##..############..##"
   "##..###%#####%##..%#"
   "#$~~~~$~$###........"
   "#~~~~~~~~###........"
   "#$~~~~~~$###..######"
   "###%#...####..######"
   ".........###..######"
   ".........###..######"
   "##.......###..######"
   "##.....#####..######"]

  ["#%..####%###%.....##"
   "#+..###............%"
   "#...+%#............."
   ".....+#..#####%....."
   "......#..#+........."
   "......#..#+........."
   "......#..%##%##...#."
   "......#.......#...##"
   "#+....#.......#...##"
   "####..######..######"]

  ["#%......##%##.....%#"
   "#...........##.....#"
   "#...........###....."
   "#....#####..###....."
   "#....####%..###....."
   "#....###+~~~###....."
   ".....###~~~~###....."
   ".....%##++~+#%.....#"
   "#.....#######......#"
   "##.....#####......##"]

  ["#%..#%#######.....%#"
   "#.......+####......#"
   "#........+###......."
   "..........##%......."
   "....###...##+......#"
   "...####...##+......#"
   "....#%#...##.......#"
   ".........+##.......#"
   "#.......+###.......#"
   "##.....#####..######"]

  ["##......#####.....##"
   "##......%###%......#"
   "##+................."
   "##++................"
   "#####%######..%#####"
   "##$~~~~~.~~~~~~++###"
   "..~~~~~~.~~~~~~~~###"
   "..~~~~~~#~~~~~~~~###"
   "##$~~~~$#+~~~~~++###"
   "####..######..######"]

  ["##..######%#####..##"
   "##..##.........%..%#"
   "#%..##.............."
   "....##..####........"
   "....%#..####........"
   "........####+......."
   "........####....++.."
   "....%#######..######"
   "##.....#####..######"
   "##.....#####..######"]

  ["##......%#######..##"
   "##......~~~~$$##..%#"
   "#####...~~~~~$##...."
   "####%...~~~~$$##...."
   "#.......#######%...."
   "#..................."
   "...................."
   ".....%######......##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##..############..##"
   "#%..%####%######..%#"
   "......#$$~$#........"
   "......#$~~$#........"
   "......#~~~~#..%#####"
   "......#~~~~#....++.."
   "......#~~~~%........"
   "......#$~~~...#....."
   "####..#$$~~...#....."
   "####..######..#....."]

  ["##......########..##"
   "##......#######%..%#"
   "##.....+#..........."
   "##....++#..........."
   "##..############..##"
   "#%..%######%###%..##"
   "..................##"
   "..................##"
   "####..######.....+##"
   "####..######......##"]

  ["###..#######%.....##"
   "#.......####$~~~~~~%"
   "#........###$.....~."
   "...#......##~.~~~.~."
   "...#..#...##~.~.~.~."
   "...#.....###~.~.~.~."
   ".........###~.~~~.~."
   ".....#..####~.....$#"
   "#......#####~~~$~$$#"
   "##.....#####..######"]

  ["#%......#####.....##"
   "#........#%..~~~~..%"
   "#........#..~~~~~~.."
   "#......###.~~##~~#~."
   "#..%#####..~%#~~~#~."
   "#.....#....~~#%~~%~."
   "......%....~##~~~~~."
   ".........#..~~~~~~.#"
   "#........#...~~~~..#"
   "####..######......##"]

  ["##..%#######%.....##"
   "#%................%#"
   "...................."
   ".....##..%###......."
   "....##......########"
   "...##..........#####"
   "..##............%###"
   "..#....##........###"
   "##%....#####......##"
   "##.....#####......##"]

  ["##......###%#.....##"
   "##......#.........%#"
   "##...####..........."
   "##....%########....."
   "##............#...##"
   "#%............#..###"
   "..........##..%..%##"
   "..........##......##"
   "####..######......##"
   "####..######......##"]

  ["##......########..##"
   "#.......#####......#"
   "#....%...###........"
   ".........##........."
   ".........##...##...."
   "...%.....##...#....."
   ".....#...##........."
   "........###....#...#"
   "#......#####.......#"
   "####..######......##"]

  ["#%..###########%..%#"
   "#~~~~~#~~~~~.......#"
   "#~~~~~#~~~~~........"
   "#~~~~~%~~~~~#......."
   "#~~~~~.~~~~~#......."
   "#~~~~~.~~~~~#......."
   "...#########%......."
   "...#...............#"
   "####...............#"
   "####..######..######"]

  ["##..############..##"
   "#%..######%#####..%#"
   "....####~~~~~###...."
   "....#%##~~~~~###...."
   ".......#~~~~~###..##"
   ".......#~~~~~##%..##"
   ".......#~~~~~.....##"
   ".......#~~~~~.....##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##......###%####..##"
   "##......#$$~$$##..%#"
   "##......#$~~~$##...."
   "###..###%~~~~~##...."
   "###..#$$~~~~~~##..##"
   "##%..#$~~~~~~~##..##"
   ".....%###~~~~~#%..##"
   ".......##$~~~~....##"
   "##.....##$$~~~....##"
   "##.....#####..######"]

  ["##..########%.....##"
   "##..####+.........%#"
   "#%..####+..........."
   "....####...#%###...."
   "....####...~~~$#...."
   "....%###...~~~$#...."
   "......##...####%...."
   "......##+.........##"
   "####..##+.........##"
   "####..######......##"]

  ["##......####%.....##"
   "##......#.........%#"
   "##......#..........."
   "##+....+#..####....."
   "###....##..####+...."
   "##%#..%##..##%#....."
   "...........#........"
   "...........#.......#"
   "####..######..######"
   "####..######..######"]

  ["#%..#####%###%....##"
   "#~~~~~$~$~~#......##"
   ".~~~~~~~~~$#........"
   ".~~%~~~~%~$#........"
   ".~~~~~~~~~~#..#%####"
   ".~~~~~+~~~$#.......#"
   ".~~%~~~~%~~#.......#"
   ".~~~~~~~~~~#.......#"
   "#$$~~~~~$$~#.......#"
   "####..######..######"]

  ["##..###%##%##.....##"
   "##..#+~++#++#.....%#"
   "##..#+~~~#..#......."
   "##..#~~~~#..#......."
   "##..#+~~~#..#......#"
   "#%..#%#..#..#......#"
   ".........#..%####..#"
   ".........#.........#"
   "##.......#.........#"
   "##.....#####..######"]

  ["##......########..##"
   "##......#####%##..%#"
   "##%##...%##++~~~~~~~"
   "..++#.....#+~~~~~~~~"
   "...+#.....#~~~~~~~~~"
   "....####..#+~~~~~~~~"
   "....##%#..#++~~~~~~~"
   "..........#####%..##"
   "##........##......##"
   "##.....#####......##"]

  ["##......########..##"
   "##......%######%..##"
   "##+........#........"
   "##++.......#........"
   "#########..#..#....."
   "##%%##%##..#..#+...."
   "...........#..#++..."
   "...........#..##%###"
   "####..######......##"
   "####..######......##"]

  ["##..############..##"
   "#%..#%##%%####%#..%#"
   "#~~~~~++~+~+~++#...."
   ".~~~~~~~~~~~~~+#...."
   ".~~#........#~~#..##"
   ".~~..........~~...##"
   ".~~#........#~~...##"
   ".~~~~~~~~~~~~~~#..##"
   "#++~~~~+~++~~~+#..##"
   "####..######..######"]

  ["##..###########%..##"
   "##..#####%........%#"
   "##..####+..........."
   "##..####+...##%##..."
   "##..######..~~~$#..#"
   "#%..#%####..~~~$#..#"
   "......##+...#~$$#..#"
   "......##+...##%##..#"
   "####..####.........#"
   "####..#####.......##"]

  ["##......#####.....##"
   "###.....#####.....##"
   "..##.....###.....##."
   "...##...........##.."
   "....###........##..."
   ".....####..#####...."
   "......###..####....."
   "...................#"
   "##................##"
   "##.....#####..######"]

  ["##..#########.....##"
   "#$~~~~~~~~~$#.....##"
   "#$~~~~~~~~~$#......."
   "###..########..#...."
   "#.....#+~~~....#...."
   "#.....#~~~~....#...."
   "...#..#~~~~#........"
   "...#..#+~~~#......##"
   "####..#+~++#......##"
   "##....######......##"]

  ["##......#####.....##"
   "##..............####"
   "................#..."
   "......###########..."
   "..##...............#"
   "..##...............#"
   "......##############"
   "...................#"
   "#..................#"
   "####..#######..#####"]

  ["###..########.....##"
   "#....#+............#"
   "#....#+............."
   "#..#############...."
   "#..#...............#"
   "#..#...............#"
   "......##########...#"
   "......#+...........#"
   "#++#..#+...........#"
   "####..######......##"]

  ["##......########..##"
   "...................."
   "...................."
   "..###############..."
   "..##................"
   "..##................"
   "..##..###########..."
   "..##..#+...........#"
   "####..#+...........#"
   "####..######..######"]

  ["###..###########..##"
   "###..###..........##"
   "###..###............"
   "###..###..##...#...."
   "###..###..##+......."
   "###..###..##+......."
   "..........##........"
   "..........##..#...##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##..############..##"
   "##....~~~~~~~++#..##"
   "......~~~~~~~~+#...."
   "....##~~~~#~~~~#...."
   "...###~~~~~~~~+#..##"
   "...###+~~~~~+~+#..##"
   "...#####...#####..##"
   "...#.......#......##"
   "####.......#......##"
   "####..######..######"]

  ["##......###%####..##"
   "##.~~~~.#.....##..%#"
   "##.~##~.#.....##...."
   "##.~+.~.%..#..##...."
   "##.~.+~....#..##..##"
   "#%.~%%~....#..##..##"
   "...~~~~.#..#..##..##"
   "........#..#..#%..##"
   "####....#..#......##"
   "####..######......##"]

  ["##......###%####..##"
   "##..#####..~~++#..%#"
   "##..%......~~~+#...."
   "##.........#~~~%...."
   "##......#~~#~~~~...."
   "#%..####%~~#+~~~...."
   "......#$~~~#++~#...."
   "......#$~~~#####..##"
   "####..#$~$$#......##"
   "####..######......##"]

  ["##......#####.....##"
   "#.......#++#%.....%#"
   "#...####%~~#........"
   "#...%~~+~~+#........"
   "#....~~~~~~#..##...#"
   "%....~~~~~+#..#...##"
   "....#~+~~~~#.....###"
   "....####~~+#....####"
   "##.....#~++#...#####"
   "##.....#####..######"]

  ["##......%####.....##"
   "##........#.......%#"
   "##........#....#...."
   "####%..#..#..##%...."
   "##++~~~#..#..#+....."
   "#%+~~~~#..%..#+....."
   "..~~~~~#.....#+....."
   "..~~~~~#.....#+...##"
   "#####..######%##..##"
   "##.....#####......##"]

  ["#%......##%#####..##"
   "#+......#.....#...%#"
   "#.......#.....#....."
   "#...#...#..#..#....."
   "#...#+..#..#..#....#"
   "#...#...%..#..%#%..#"
   "....#+.....#..~~~~~#"
   "....#+.....#..~~~~~#"
   "##..%###+.+#..~~~$$#"
   "##.....#####..######"]

  ["#%.....#####%###..##"
   "#......#++#.......%#"
   "#......#~~#........."
   "#....##%~~#........."
   "#....#+~~~~..#######"
   "#....#+~~~~...#+.+##"
   ".....#+~~~#...#+.+##"
   ".....%#~~~#...%...##"
   "##.......+#.......##"
   "##.......###......##"]

  ["###..########.....##"
   "#%...###%####%....%#"
   "#....#.............."
   "#...##.............."
   "#..###..#..#..##..##"
   "#..%##..#++#..##..##"
   "....+...%###..%#..##"
   "..................##"
   "##................##"
   "##.....#####..######"]

  ["#%~~~~~######%##..##"
   "#+~~~~~+#......#..%#"
   "#~~~~~~~%..........."
   "#+~~~~~~...#........"
   "##+~~~~~...#++.#...."
   "#####%##########...."
   "..............##...."
   "..............#%..##"
   "####..######......##"
   "####..######......##"]

  ["##..#%#######.....%#"
   "#......#####%......#"
   ".........###........"
   ".........###....#..."
   "...##.....%.....##.."
   "....%##.........##.."
   "......##........###."
   ".......##......#####"
   "##.....####....#####"
   "##.....#####..######"]

  ["##......%####.....##"
   "##......~~++#.....%#"
   "#####%..~~~+##......"
   "........~~++##......"
   "........###%####%..."
   "...#%####$$~~~......"
   "......++#$~~~~......"
   ".......+#~~~~~######"
   "##.....+#$$~~~######"
   "##.....#####..######"]

  ["##......%####%##..##"
   "##..........~~$#..%#"
   "##+.........~~$#...."
   "##.....+.+.#~~$#...."
   "##+...######~~$#..##"
   "#%....#+++~####%..##"
   "......#~~~~%......##"
   "......#~~~~.......##"
   "####..#+++~.......##"
   "####..######......##"]

  ["##......###%##....##"
   "#.......#$$~$#....##"
   "#...%#..#$~~~##%..%."
   "#....######~~#......"
   "#....#++~~#~~......."
   "##%..#+~~~#~~......."
   ".....###~~%#..####.."
   ".......#~~......+###"
   "##%....#~~......+###"
   "##.....#####..######"]

  ["##......###%####..##"
   "##.......~~~~~$#..%#"
   "##%##....~~~~~$#...."
   "....%...#####$$#...."
   "........##++####..##"
   "........##+....#..##"
   "....#####%##...#..##"
   "..........+#...%..##"
   "##........+#......##"
   "##.....#####......##"]

  ["#%.....#%###%#....%#"
   "#.......+#+........#"
   "#+.~~~~~.%.~~~~~...."
   "%..~~~~~...~~~~~...."
   "...~~~~~...~~~~~...."
   "...~~~~~...~~~~~.%##"
   "..................+#"
   "......#....#......+#"
   "#+....#++++#......+#"
   "###...######.....###"]

  ["##......%#######..##"
   "##................##"
   ".###..............%."
   ".%###%#%#%#%#%##...."
   "..............+#...."
   "...............%...."
   ".#.....~~~~........."
   ".#+...#~~~~#......##"
   "##+...#++++#.....###"
   "####..######......##"]

  ["#%..#####%%#####..%#"
   "#..................%"
   "#..................."
   "#..###%#####..%##..."
   "#..##+~~~~~~~~+##..#"
   "...##+~~~~~~~~+##..#"
   "...%##..#%#####%%..#"
   "...................#"
   "#..................#"
   "####..######..######"]

  ["##..%###########..##"
   "##....##########...%"
   "#......###..###%...."
   "##.....#......#....."
   "###................#"
   "#%.................#"
   "...................#"
   "......#....#..#...##"
   "#..#..##..##..##.###"
   "####..######..######"]

 ])

 (def vert [
  ["####..####"
   "####..%#%#"
   "#%#%......"
   ".........."
   ".........."
   "..###%%..."
   "..#.+....."
   "..#......."
   "###+.....#"
   "######+..#"
   "######..+#"
   "##%###...#"
   "....##+.+#"
   "....###%##"
   "##..###%##"
   "##..#%...."
   "##........"
   "##......##"
   "######..##"
   "######..##"]

  ["##.....###"
   "###%..%###"
   "###....###"
   "###....###"
   "###%..####"
   "......%###"
   ".........."
   "###......."
   "####%#..%#"
   "###~$~~~~#"
   "###~~~~~$#"
   "###$~~~~~#"
   "..#~~$~~$#"
   "..#%##%###"
   "........##"
   ".........."
   "..#......."
   "###....###"
   "###....###"
   "###.....##"]

  ["#......###"
   "##.....###"
   "###..##%##"
   ".%#..#$~$#"
   ".....#$~~#"
   ".....%~~~#"
   ".........."
   "..#......."
   "###..%####"
   "###......#"
   "###......#"
   "#%#####..#"
   "...####..#"
   "...####..."
   "#...###..."
   "#...###..."
   "##..##%..."
   "##........"
   "##.......#"
   "######..##"]

  ["####..####"
   "####..####"
   "####..##.."
   "####..##.."
   "###%..%#.."
   ".........."
   "..~~~~...."
   "##~~~~#..."
   "##+~~+####"
   "##~~~~####"
   "##+~~+####"
   "##%####%##"
   "...##~$~$#"
   "...%#~~~~~"
   ".....~~~~~"
   ".....~~~~~"
   "...##~~~~~"
   "#..##~~~~~"
   "#..##$~~$#"
   "######~~##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#######..."
   "##%##$~~~~"
   "....#~~~~~"
   "....#$~~~~"
   "##..#%#..."
   "##.......#"
   "##.......#"
   "##.......#"
   "##%####..#"
   ".....##..#"
   ".....##..#"
   "#..####..#"
   "#..#+%#..."
   "#..#..+..."
   "#........#"
   "#.......+#"
   "###.....##"]

  ["#%.....###"
   "#......###"
   "#....#####"
   "....######"
   "...#######"
   "..###%####"
   "..###....."
   "...##....."
   "....%..###"
   "#......###"
   "##.....###"
   "###....###"
   "..%%...###"
   "..%#####.."
   ".......#.."
   ".......%.."
   "..##......"
   "####......"
   "####...###"
   "###.....##"]

  ["####..####"
   "###%..####"
   "#........."
   ".........."
   "....##...."
   "...#####.."
   "....####.."
   ".....####."
   "##...%####"
   "###...####"
   "###....###"
   "####....##"
   "..###....#"
   "...###...."
   "#...##...."
   "#....%...."
   "##........"
   "##........"
   "###......#"
   "###.....##"]

  ["####...###"
   "####.....#"
   "#####....."
   "######...."
   "#%####...."
   "...###...."
   "....##...."
   "#...##...."
   "##...%...#"
   "##......##"
   "##.....###"
   "#...#..%##"
   "....#~~~$#"
   "...##~~~~~"
   "####$~~~~~"
   "###~~~~#~~"
   "##$~$~~~~~"
   "###~~~~~~~"
   "####$~~~$#"
   "######~~##"]

  ["####..####"
   "####..####"
   "###%..####"
   ".......###"
   ".......###"
   "...##..###"
   "...##....."
   "...%#....."
   "###%######"
   "###%######"
   "#......###"
   "#......%##"
   "...##....#"
   "...##....#"
   "#######..#"
   "####%....."
   "####......"
   "###......#"
   "###......#"
   "###.....##"]

  ["##.....###"
   "##......##"
   "#####...##"
   "#####...##"
   "####%...##"
   "........##"
   ".........."
   "##+###...."
   "#...######"
   "#......###"
   "#......%##"
   "#..#.....#"
   "...##....#"
   "...###...."
   "...####..."
   "...####..."
   "....%#...."
   "#........."
   "##.......#"
   "######..##"]

  ["####..####"
   "####.....#"
   "###......."
   "#%...##..."
   "#...##%..."
   "...###+..."
   "..###..~.."
   "####.+~~+."
   "#######~##"
   "######+~##"
   "######~###"
   "######~###"
   "..##.~~~.."
   "..##..~..."
   ".~~~~~...."
   ".~~~~~#..."
   ".~~~~~#..."
   "#~~~~~%##."
   "#~~~~~..##"
   "###.....##"]

  ["##.....###"
   "#......###"
   "#.....####"
   "......####"
   ".....#####"
   ".....%####"
   "..#...+..."
   "..#......."
   "##%.....##"
   "#.+.....##"
   "#....#####"
   "###+######"
   "....######"
   "......%###"
   "#.......##"
   "#..##....."
   "#...###..."
   "#.....%###"
   "##......##"
   "###.....##"]

  ["####..####"
   "####..#%##"
   "##....~~$~"
   "##....~~~~"
   "##..##$~~~"
   "....##~~~~"
   "....##$~~~"
   "##..##~~$~"
   "##..###%##"
   "##..##+~~#"
   "##..##~~+#"
   "##..##~~~#"
   ".#..##+~##"
   ".#..##...."
   ".%..#%...."
   ".........."
   ".........."
   "######...."
   "######..##"
   "######..##"]

  ["##.....###"
   "##......##"
   "##......##"
   ".#......##"
   ".%..##..##"
   "....##..##"
   "....##...."
   "....##...."
   "###%######"
   "###~##~$$#"
   "##~~~#$~$#"
   "##+~+#~~~#"
   "..~~~#$~~#"
   "..~~+#~~~."
   "##~~~#~~~."
   "##..#%~~~."
   "##....~~~."
   "##....~~~."
   "##......##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....###"
   "###...####"
   "###%..####"
   "......####"
   "......#..."
   "#.....#..."
   "#..##%#..#"
   "#........#"
   "#........#"
   "######%###"
   "........##"
   "........##"
   "##%###..##"
   "#~~$~#...."
   "#$~~~#...."
   "#~~~~...##"
   "#~$~~...##"
   "######..##"]

  ["####..####"
   "####..####"
   "####...###"
   "..##....%."
   "...##....."
   "....%#...."
   ".........."
   ".........."
   "##.....#+#"
   "########.#"
   "##.......#"
   "#......###"
   ".......#.#"
   "...#...#.."
   "...#...#.."
   "...#...%.."
   "...#......"
   "###%......"
   "###......."
   "###......."]

  ["####..####"
   "####..####"
   "####..####"
   "####..####"
   "##%#..#%##"
   ".........."
   ".........."
   ".........."
   "#####%####"
   "#####%####"
   "###%%%####"
   "###%######"
   ".....#####"
   "......####"
   "###....%##"
   "####......"
   "#####....."
   "######..##"
   "######..##"
   "######..##"]

  ["##.....%##"
   "#........#"
   "#...~~...."
   "...~##~..."
   "..~####~.."
   "..~####~.."
   "...~%#~..."
   "....~~...."
   "##......##"
   "###....###"
   "###+######"
   "##..######"
   "...#######"
   "...####%.."
   "...####..."
   "....%#...."
   ".........."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "####..####"
   "####..%#.."
   "####......"
   "###%......"
   ".........."
   ".........."
   "##.######."
   "##...#####"
   "####...###"
   "##%.....##"
   "##......##"
   ".....#..##"
   "....##..##"
   "#~~%##..%#"
   "#~~~~#...."
   "#$~~$#...."
   "#~~~~#..##"
   "#$~~$#..##"
   "######..##"]

  ["####..####"
   "####..####"
   "####..####"
   "####..####"
   "##%#..####"
   "......#%##"
   ".........."
   "##%#......"
   "##.#######"
   "##%#######"
   "##..###.%#"
   "#...##...#"
   "....##...#"
   "....##...."
   "....##...."
   "....##...."
   ".........."
   "#........."
   "#........#"
   "###.....##"]

  ["##.....###"
   "#......#%#"
   "#......#.."
   "#......#.."
   "###%#..#.."
   ".........."
   ".........."
   "###+#....."
   "##..######"
   "##...#####"
   "#$$%######"
   "#~~~######"
   ".~~~~#%###"
   ".~~~~...##"
   ".~~~~...##"
   ".~~~~#...."
   ".~~~$#...."
   "#~~~~#..##"
   "#~$~$#..##"
   "######..##"]

  ["####..#%##"
   "#........#"
   "#........#"
   "....%##..#"
   "......#..#"
   "......#..#"
   "......#..."
   "......#..."
   "#..#######"
   "#..###%###"
   "#..#.....#"
   "#..#.....#"
   "...#.....#"
   "...#.....#"
   "#..%..#..."
   "#.....#..."
   "#.....#..."
   "#.....%###"
   "#.......##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....#.."
   "#####..#.."
   "##%##..%.."
   ".........."
   ".........."
   "######...."
   "######...#"
   "#$$~~~~~.#"
   "#~~~##~~~#"
   "#$~$######"
   "~~~~%##%##"
   "~~~~$~~$~~"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "#$~~~~~~~~"
   "#~$~~~~~~#"
   "###.....##"]

  ["##.....%##"
   "##.......#"
   "####.....#"
   "...##....#"
   "....##...#"
   ".....##..#"
   "......#..."
   "......#..."
   "##....#..#"
   "###......#"
   "###......#"
   "#%....#..#"
   "......#..#"
   ".....##..#"
   ".....#%..#"
   "....##...."
   "...###...."
   "#####....#"
   "#####....#"
   "######..##"]

  ["##%#..#%##"
   "#+~~~~~~+#"
   "#~~~~~~~+#"
   "#~~##%#~~#"
   "#~~#~~#~~#"
   ".~~#$~#~~#"
   ".~~#~~.~~."
   "#~~#~~.~~."
   "#~~#$~#~~#"
   "#~~#~~#~~#"
   "#~~#~~#~~#"
   "#~~%~$#~~#"
   ".~~.~~#~~#"
   ".~~.~~#~~#"
   "#~~#~$#~~#"
   "#~~#~~#~~."
   "#~~#%##~~."
   "#+~~~~~~+#"
   "#~+~~~~~+#"
   "###.....##"]

  ["####..####"
   "####..####"
   "####..#..."
   "...#..#..."
   "...#..#..."
   "...#..#..."
   "......#..."
   "......#..."
   "#..####..#"
   "#..~~....#"
   "#..~~....#"
   "###~~#####"
   "...~~#####"
   "...~~##..."
   "###~~##..."
   "###~~##..."
   "###~~##..."
   "###~~....."
   "###~~....#"
   "######..##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#........#"
   "####.....#"
   "...##....#"
   "....###..."
   "#........."
   "#........#"
   "####.....#"
   "####+#####"
   "#........#"
   ".........#"
   "..##..#..."
   "..##..#..."
   "......#..."
   "......#..."
   "#.##..###."
   "###.....##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "###...##.."
   ".#%...%#.."
   "..~~~~~..."
   "..~~~~~..."
   "..~~~~~..."
   "..~~~~~..."
   "##~+~+~###"
   "##~~######"
   "##%~######"
   "#~~~....##"
   ".~~~....##"
   ".~~~##..##"
   "#~~~##..%#"
   "#~~$##...."
   "#$~~##...."
   "#$~$##..##"
   "######..##"
   "######..##"]

  ["####..####"
   "###...%###"
   "##.....###"
   "#.......##"
   "....#....#"
   "...###~..."
   "..##~~~~.."
   "####~###.."
   "####~~####"
   "#####~####"
   "##%##~~~##"
   "#$~$$##~##"
   ".~~~~##%##"
   ".~~~~##..."
   "#~~~$#...."
   "#~~~$#...."
   "#..###...."
   "#........."
   "#........#"
   "######..##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#........."
   "###%##...."
   ".....#...."
   ".....#...."
   "#........."
   "#........#"
   "#%########"
   "#%########"
   "#%########"
   "....###%##"
   "....##...."
   "....##...."
   "....##...."
   ".....%...."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "#.....####"
   "#.....####"
   "#..#######"
   "#..#%#####"
   ".....##%##"
   ".....##..."
   "###..##..."
   "###..##..#"
   "###..##..#"
   "###..##..#"
   "#%#..##..#"
   ".....##..#"
   ".....##..#"
   "#..###%..#"
   "#........."
   "#........."
   "######..##"
   "######..##"
   "######..##"]

  ["##%#..####"
   "#.....#..#"
   "#.....#..."
   "....##%..."
   "....#....."
   "....#....."
   ".........."
   ".........."
   "#...#....#"
   "#####%####"
   "###%%%####"
   "###%###%##"
   ".~$~~$~~$#"
   ".~~~~~~~~."
   ".~~~~~~$~."
   ".~~~$~~~~."
   ".~~~~~~~~."
   "#$~~~~~~~."
   "#~$~~~~~$#"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##..######"
   "##..#%#%##"
   "##~~$~$~$#"
   "..~.....~#"
   "..~.~~~.~."
   "##~.~#~.~."
   "##~.~~~.~#"
   "##~.~~~.$#"
   "##~.~~~.$#"
   "##~.~~~.$#"
   "..~.~~~.$#"
   "..~.~~~.$#"
   "##~.~#~.~#"
   "##$.~~~.~."
   "##$.....~."
   "##~$$~~~$#"
   "######..##"
   "######..##"]

  ["####..#%##"
   "####.....#"
   "####......"
   ".#####...."
   "..####...."
   "...###...."
   "...###...."
   "...#%#...."
   "#........#"
   "#........#"
   "###+######"
   "###%######"
   ".....#####"
   ".....#...."
   "###..#...."
   "#........."
   "#........."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "####...%##"
   "##%##...##"
   "...###..##"
   "....##..##"
   "....##..##"
   "....##...."
   "....##...."
   "#...#....#"
   "#.......##"
   "#......###"
   "###...####"
   "..+..#####"
   "..#####%##"
   "...###...#"
   "....##...."
   ".....%...."
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#..##....."
   "#..###...."
   "...####..."
   "...#####.."
   "#%%%#####."
   "#.......##"
   "#........#"
   "##~~%#...#"
   "#~~~$##..#"
   "~~~~~##..#"
   "~~~~~$#..#"
   "~~~~~~#..#"
   "~~~~~$#..."
   "~~~~~##..."
   "#$~~$#%..#"
   "##$$##...#"
   "######..##"]

  ["##%...##%#"
   "##....#..#"
   "#....##..."
   "#...##~..."
   "#..##$~..."
   "...##~~..."
   "...##$~..."
   "#..%###..."
   "#........#"
   "#........#"
   "#####+####"
   "#####%#%##"
   ".........#"
   ".........#"
   "#..#~~#..#"
   "#..#~~#..."
   "#..####..."
   "#........#"
   "#........#"
   "###.....##"]

  ["##%#..####"
   "#........#"
   "#........#"
   "...##%#..#"
   "...##....#"
   "...##....#"
   "...##....."
   "...##....."
   "#..##....#"
   "####%....#"
   "###%%#...#"
   "###%######"
   ".....###%#"
   "......##.."
   "###...##.."
   "#.##..%#.."
   "#........."
   "#........."
   "#........#"
   "###.....##"]

  ["##.....###"
   "###....###"
   "####..##.."
   "####..##.."
   "####..%#.."
   ".........."
   ".........."
   "###......."
   "####..####"
   "####.~####"
   "####~.####"
   "#%##~~####"
   "...#~~##%#"
   "...#~~#..."
   "...#~~#..."
   "...#......"
   ".........."
   "#........."
   "#.......##"
   "######..##"]

  ["####..####"
   "####.....#"
   "####.....#"
   ".####%#..#"
   "..##.....#"
   "...#.....#"
   "...%..#..."
   "......#..."
   "#.....####"
   "#.....####"
   "###%######"
   "#........#"
   ".........#"
   ".........#"
   "##..###..#"
   "##..%##..."
   "#....##..."
   "#.....####"
   "#......###"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....###"
   "###...####"
   "#%##..####"
   "......##%#"
   "......#..."
   "#..#%##..."
   "#........#"
   "#........#"
   "###+######"
   "###.######"
   ".....#####"
   "......%..."
   "##........"
   "####......"
   "#####....."
   "#####....."
   "######...#"
   "######..##"]

  ["##.....###"
   "#......###"
   "#....####."
   "#...##%#.."
   "#...#....."
   ".........."
   ".........."
   "###%......"
   "###%#%####"
   "##%%%%%###"
   "##%#%#%###"
   "#$~$~$~$~#"
   "~~~~~~~~$#"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "~~~~~#~~~~"
   "~~~~~~~~~~"
   "#$~~~~~~~~"
   "#~$~~~~~$#"
   "###.....##"]

  ["#%##..#%##"
   "#..#.....#"
   "#..#.....#"
   "...#.....#"
   ".........#"
   ".........#"
   "...####..."
   "...###%..."
   "#..#~$~~~#"
   "#..#$~~~~#"
   "#..#$~~~~#"
   "#..#~$~~~#"
   "...####..#"
   "...###%..#"
   "#####+~~~#"
   "#####~~~~."
   "#####~~~~."
   "#####+~~~#"
   "#####+~~~#"
   "######..##"]

  ["##%#..####"
   "#........#"
   "#........."
   "#........."
   "#..#####.."
   "...#####.."
   "...#####.."
   "#+######.."
   "#....%####"
   "#........#"
   "#........#"
   "#..###...#"
   "...####..#"
   "....###..."
   "....###..."
   "....##%..."
   "...###...."
   "######...."
   "######...#"
   "######..##"]

  ["##.....###"
   "#......###"
   "#......###"
   "#####..###"
   "##%##..###"
   ".~~+#..%##"
   ".~~+#....."
   "#~~~#....."
   "#.~~##%..#"
   "#.~~.~~~~#"
   "#.~~.~~~~#"
   "#+###~~~~#"
   "...##~~~$#"
   "...##$$$~#"
   "#..#######"
   "#..####..."
   "#..#%##..."
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....%##"
   "#.......##"
   "#.......##"
   "....##..%#"
   "....##...#"
   "....###..#"
   "...####..."
   "..#####..."
   "#%########"
   "#.%#######"
   "#..#######"
   "#...###..#"
   "....##...#"
   ".....#...."
   ".....%...."
   ".........."
   ".........."
   "###......."
   "######...#"
   "######..##"]

  ["####..####"
   "####..##%#"
   "####..#..."
   "####..#..."
   "##%#..#..."
   "...#..#..."
   "...#..#..."
   "#..#..#..."
   "#..%..%..#"
   "#........#"
   "#........#"
   "##.#..####"
   "...#######"
   "....#%##.."
   "#.....##.."
   "##.....%.."
   "###......."
   "####......"
   "#####....#"
   "######..##"]

  ["##%...####"
   "##....####"
   "##...#####"
   "#....#####"
   ".......%##"
   "........##"
   "...##....."
   "#######..."
   "########.#"
   "########%#"
   "###%#$$~~#"
   "##~~~~~~~#"
   "..~~~~~~~#"
   "..~~~~~~~#"
   "##$~$~#..#"
   "#####%#..."
   "####......"
   "####.....#"
   "###......#"
   "###.....##"]

  ["##%#..##%#"
   "#........#"
   "#........."
   "#.....#..."
   "##%#..#..."
   "......#..."
   "......#..."
   "#.....#..."
   "#.....#..#"
   "##.#######"
   "##%####%##"
   "#$~$#$~~$#"
   ".~.~#~..~#"
   ".~.~#~..~#"
   "#~.~#~..~#"
   "#~.~%~..~."
   "#~.~~~..~."
   "#~......~#"
   "#$~~~~~~~#"
   "######..##"]

  ["##.....###"
   "##...##%##"
   "###..#$~$#"
   "..#..#~~~#"
   "..#..#~~~#"
   "..%..%~~~%"
   ".........."
   ".........."
   "#........#"
   "###+######"
   "###%######"
   "#.......##"
   "........##"
   "...##...#."
   "...##....."
   "...##....."
   "...##....."
   "####%..##."
   "###.....##"
   "###.....##"]

  ["####..#%##"
   "...#~~~~~#"
   "...#~~~~~#"
   "...#+~~~~#"
   "...#~+~~~#"
   "...##%#..#"
   ".........."
   ".........."
   "#%%%%#####"
   "###%######"
   "#~$~~~~$~#"
   "#$~~~~~~$#"
   ".~~~~#~~~#"
   ".~~~$#$~$#"
   "#~~~$#####"
   "#~~~~#%..."
   "#$~~~....."
   "#$~~~....#"
   "#~$$~....#"
   "######..##"]

  ["####..%###"
   "#...~~~~+#"
   "#...~~~~+#"
   "#..#~~~~~#"
   "#..#~+~~~#"
   "...####%##"
   ".........."
   "#........."
   "#..###%..#"
   "#..##$~~~#"
   "#..##$~~~#"
   "#%###~~~~#"
   "...##~~~~#"
   "...##~$~$#"
   "...#######"
   ".........."
   ".........."
   "#####%..##"
   "###.....##"
   "###.....##"]

  ["##.....###"
   "##.....%##"
   "##........"
   ".#........"
   ".#........"
   ".###%##..."
   ".........."
   ".........."
   "#..#..#..#"
   "#..#..#..#"
   "#..####..#"
   "#..###%..#"
   "...##....."
   "...##....."
   "##%##...#."
   "#$$~#...#."
   "#~~~#...#."
   "#~~~....##"
   "#~~~....##"
   "######..##"]

  ["#%##..####"
   "#........#"
   "#........."
   "#..#%##..."
   "#..#~~#..."
   "...#~~#..."
   "...#~~#..."
   "####~~#..."
   "#...~~%###"
   "#...~~~~$#"
   "#..#~~~~$#"
   "#..#~~~~$#"
   "...#~~~###"
   "...#~~~#.."
   "...#~$$#.."
   "...##%##.."
   ".........."
   "#........."
   "#.......##"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#..#%##..#"
   "#..#..#..#"
   "...#..#..#"
   "...#......"
   "#..#......"
   "#..#..#%##"
   "#..#...~+#"
   "#..#...~~#"
   "#..#...~~#"
   "...#..#~~#"
   "...#..#~+#"
   "##%#..%###"
   "#$~~~~~~.."
   "#~~~~~~~.."
   "#~~~~~~~##"
   "#$~~~~~~##"
   "###.....##"]

  ["##%#..####"
   "#..#.....#"
   "#..#......"
   "#..#......"
   "#..####..."
   "......#..."
   "......#..."
   "#..##%#..."
   "#........#"
   "#........#"
   "######.###"
   "######%###"
   ".........#"
   ".........."
   "###%##...."
   "#~$~~~...."
   "#$~~~~...."
   "#$~~~~...."
   "#~$~~~...#"
   "######..##"]

  ["###%..####"
   "#$~~~~~$~#"
   "#~~~~~~~$#"
   ".~~~~~~~~#"
   ".~~$~$~$~#"
   "...##%####"
   ".........."
   ".........."
   "#######%##"
   "#........#"
   "#........#"
   "#..####..#"
   "......#..#"
   "......#..#"
   "......#..#"
   "......#..."
   "......#..."
   "####..%###"
   "###.....##"
   "###.....##"]

  ["##.....%##"
   "#........#"
   "#........#"
   "...##....#"
   "....#....#"
   "....#..###"
   "....#....."
   "....#....."
   "##%##~~###"
   "##+##~~++#"
   "#$$$#~~~~#"
   "#~~~#+~~~#"
   ".~~$#+~~+#"
   ".~~~######"
   "#~~~#%####"
   "#~~~~~~~.."
   "#$~~~~~~.."
   "#$~~~~~~##"
   "#~$$~~~~##"
   "######..##"]

  ["#%##..####"
   "#........#"
   "#........#"
   "#..##%#..#"
   "#..#$~~~~#"
   "...#~~~~~#"
   "...#$~~~~."
   "####~~~~~."
   "#..#$~~~~#"
   "#..##..%##"
   "#..#+~~~+#"
   "#..#+~~~+#"
   "...#+~~~+#"
   "...#+~~~+#"
   "#..##%..##"
   "#..#......"
   "#..#......"
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........."
   "...##%#..."
   "...#~~#..."
   "...#~~#..."
   "...#~~#..."
   "...#~~#..."
   "#..%~~%..#"
   "#........#"
   "#$$$..$$$#"
   "######%###"
   "....#$+$.#"
   "....#$...."
   ".#..#$...."
   ".#..#$...."
   ".#..%....."
   "##........"
   "##.......#"
   "######..##"]

  ["#%##..%###"
   "#...~~~~~#"
   "#...~~~~+#"
   "#..#~~~~~#"
   "#..#~+~+~#"
   "...#######"
   ".........."
   "#........."
   "#..###%###"
   "#..#~$~$~#"
   "#..#$~~~$#"
   "#..%~~~~$#"
   "....~~~~$#"
   "....~~~~~#"
   "#..#$~~~~#"
   "#..#~$~~~."
   "#..#%##..."
   "#.....####"
   "#.......##"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#........#"
   "##%#%#%..#"
   ".~~$~$~~~#"
   ".~......~."
   "#~......~."
   "#~......~#"
   "#$......$#"
   "#~$~$$~$~#"
   "###%####%#"
   ".~+~+#...#"
   ".~~~+#...."
   "#~~~+#...."
   "#~~~~#...."
   "#~~~~#...."
   "#+~~~....."
   "#++~~...##"
   "######..##"]

  ["####..####"
   "####..##%#"
   "####..#..."
   "...#..#..."
   "...#..#..."
   "...%..%..."
   ".........."
   ".........."
   "#####%####"
   "##$$~~$~$#"
   "##~.....$#"
   "##~.....$#"
   ".#~.....~#"
   ".#~~~~$$~#"
   ".#..#%####"
   ".........."
   ".........."
   "##.......#"
   "##.......#"
   "###.....##"]

  ["##%#..####"
   "#........#"
   "#........#"
   "#..####..#"
   "#.....#..#"
   "......#..#"
   "......#..."
   "#.....#..."
   "##.####.##"
   "##%#%##%##"
   "#$~$~$$~$#"
   "#~......$#"
   ".~.~~~~.~#"
   ".~.~~~~.~."
   "#$.~~~~.~."
   "#$.~~~~.~."
   "#$.~~~~.~."
   "#$......~."
   "#~$$$~~~$#"
   "######..##"]

 ])

