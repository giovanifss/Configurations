Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
       , borderColor = "#000000"    -- black
       , border = TopB
       , bgColor = "#000000"        -- black
       , fgColor = "#dddddd"        -- lightgrey
       , position = TopW L 100
       , commands = [ Run Network "wlp3s0" ["-L","1000","-H","2000000","--normal","#00c500","--high","#cc0000"] 10                                -- lightgreen and red
                    , Run Network "eth1" ["-L","1000","-H","2000000","--normal","#00c500","--high","#cc0000"] 10                                  -- lightgreen and red
                    , Run Cpu ["-L","20","-H","50","--normal","#00c500","--high","#cc0000"] 10                                                    -- lightgreen and red
                    , Run Memory ["-t","Mem: <usedratio>%", "-L", "30", "-H", "70", "--normal", "#00c500", "--high", "#cc0000"] 10                -- lightgreen and red
                    , Run Battery ["--template" , "Batt: <acstatus>", "--Low", "10", "--High", "50", "--low", "#cc0000", "--normal", "#ee9a00"    -- red and orange
                                  , "--"                                          -- battery specific options
                                  , "-o", "<left>% (<timeleft>)"                  -- discharging status
                                  , "-O", "<left>% [<fc=#ee9a00>Charging</fc>]"   -- AC "on" status - Orange color
                                  , "-i", "<left>% [<fc=#008000>Charged</fc>]"    -- charged status - Darkgreen color
                                  ] 50
                    , Run Swap ["-t", "Swap: <usedratio>%", "-L", "5", "-H", "15", "--normal", "#00c500", "--high", "#cc0000"] 10                 -- lightgreen and red
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Com "whoami" [] "" 36000
                    --, Run Volume "pulse" "Master" ["-t", "<volume>% <status>", "--", "-on", "x"] 10
                    , Run Date "%a %H:%M:%S - %b %d %Y" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% | %cpu% | %memory% * %swap% | %wlp3s0% }<fc=#ee9a00>%date%</fc>{%uname% | <fc=#ee9a00>%whoami%</fc> | %battery% "
       }
