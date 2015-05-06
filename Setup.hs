import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
  { postInst = \_ _ _ _ -> putStrLn notice
  }

notice =
  "                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\
  \                                                                               \n\
  \              Notice: some stackage plugins have moved!                        \n\
  \                                                                               \n\
  \      cabal install stackage-cabal                                             \n\
  \        # will get you the plugins:                                            \n\
  \          # stackage-init                                                      \n\
  \          # stackage-purge                                                     \n\
  \          # stackage-upgrade                                                   \n\
  \                                                                               \n\
  \      cabal install stackage-sandbox                                           \n\
  \        # will get you the stackage-sandbox plugin                             \n\
  \                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
