Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
    , borderColor = "black"
    , border = TopB
    , bgColor = "black"
    , fgColor = "grey"
    , position = TopW L 100
    , commands = [ Run Weather "CYVR" ["-t","<tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                   , Run Network "eth0" ["-L","1","-H","2000000","--normal","green","--high","red"] 10
                   , Run Network "eth1" ["-L","1","-H","2000000","--normal","green","--high","red"] 10
                   , Run Cpu ["-L","20","-H","60","--normal","green","--high","red"] 10
                   , Run Memory ["-t","Mem: <usedratio>%", "-L", "30", "-H", "70", "--normal", "green", "--high", "red"] 10
                   , Run Battery ["--template" , "Batt: <acstatus>", "--Low", "10", "--High", "50", "--low", "darkred", "--normal", "darkorange"
                                       -- battery specific options
                                       , "--"
                                       -- discharging status
                                       , "-o", "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O", "<left>% [<fc=#dAA520>Charging</fc>]"
                                       -- charged status
                                       , "-i", "<left>% [<fc=#006000>Charged</fc>]"
                             ] 50
                   , Run Swap ["-t", "Swap: <usedratio>%", "-L", "5", "-H", "15", "--normal", "green", "--high", "red"] 10
                   , Run Com "uname" ["-s","-r"] "" 36000
                   , Run Com "whoami" [] "" 36000
                   , Run Date "%a %H:%M:%S - %b %_d %Y" "date" 10
                   , Run StdinReader
                   ]
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%StdinReader% | %cpu% | %memory% * %swap% | %wlp3s0% }<fc=#ee9a00>%date%</fc>{%uname% | <fc=#ee9a00>%whoami%</fc> | %battery% "
}
