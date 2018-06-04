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
  , template = "%StdinReader% | %cpu% | %memory% * %swap% | %<IF-1>% }<fc=#ee9a00>%date%</fc>{%uname% | <fc=#ee9a00>%whoami%</fc> | %battery% "

  -- Plugins
  , commands =

        -- Network activiy for primarily interface
        [ Run Network "<IF-1>" [ "-L", "0"
                               , "-H", "2000000"
                               , "--normal", "#00c500"   -- lightgreen
                               , "--high", "#cc0000"     -- red
                               ] 10

        -- Network activiy for second interface
        , Run Network "eth1"   [ "-L", "0"
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
        , Run Com "<XMOBAR-DIR>/scripts/battery.sh" [] "battery" 400

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
