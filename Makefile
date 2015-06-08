.PHONY: liquidDoShit

liquidDoShit:
	@liquid -g-no-user-package-db -g"-package-db /Users/lukecycon/Development/School/tournament/.cabal-sandbox/x86_64-osx-ghc-7.8.4-packages.conf.d" Tournament/Controller/Course.hs
