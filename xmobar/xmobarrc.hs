Config {

  -- Appearance
    font = "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=tru"
  , borderColor = "#000000"    -- black
  , border = TopB
  , bgColor = "#000000"        -- black
  , fgColor = "#dddddd"        -- lightgrey
  , position = TopW L 100

  -- Layout
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% | %cpu% | %memory% * %swap% | %wlp3s0% }<fc=#ee9a00>%date%</fc>{%uname% | <fc=#ee9a00>%whoami%</fc> | %battery% "

  -- Plugins
  , commands =

        -- Network activiy for primarily interface
        [ Run Network "wlp3s0" [ "-L", "1000"
                               , "-H", "2000000"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- Network activiy for second interface
        , Run Network "eth1"   [ "-L", "1000"
                               , "-H", "2000000"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- CPU activity monitor
        , Run Cpu              [ "-L", "20"
                               , "-H", "50"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- Memory usage monitor
        , Run Memory           [ "-t", "Mem: <usedratio>%"
                               , "-L", "30"
                               , "-H", "70"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- Battery monitor
        , Run Battery          [ "--template", "Batt: <acstatus>"
                               , "--Low", "10"
                               , "--High", "50"
                               , "--low", "#cc0000"      -- red
                               , "--normal", "#ee9a00"   -- orange
                               , "--"                    -- battery specific options
                               , "-o", "<left>% (<timeleft>)"                  -- discharging status
                               , "-O", "<left>% [<fc=#ee9a00>Charging</fc>]"   -- AC "on" status - Orange color
                               , "-i", "<left>% [<fc=#008000>Charged</fc>]"    -- charged status - Darkgreen color
                               ] 50

        -- Swap usage monitor
        , Run Swap             [ "-t", "Swap: <usedratio>%"
                               , "-L", "5"
                               , "-H", "15"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- Kernel information
        , Run Com "uname"      [ "-s", "-r"
                               ] "" 36000

        -- User information
        , Run Com "whoami"     [] "" 36000

        -- Date information
        , Run Date             "%a %H:%M:%S - %b %d %Y" "date" 10

        -- Field read from stdin
        , Run StdinReader
        ]
  }
