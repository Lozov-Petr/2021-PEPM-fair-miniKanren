TESTS = ReversoBiasedForwardOpt30 ReversoBiasedForwardPes30  ReversoNaiveForwardOpt30   \
        ReversoNaiveForwardPes30  ReversoFairWQOForwardOpt30 ReversoFairWQOForwardPes30 \
				ReversoBiasedForwardOpt60 ReversoBiasedForwardPes60  ReversoNaiveForwardOpt60   \
        ReversoNaiveForwardPes60  ReversoFairWQOForwardOpt60 ReversoFairWQOForwardPes60 \
				ReversoBiasedForwardOpt90 ReversoBiasedForwardPes90  ReversoNaiveForwardOpt90   \
				ReversoNaiveForwardPes90  ReversoFairWQOForwardOpt90 ReversoFairWQOForwardPes90

compile:
	stack build fair-miniKanren --ghc-options="-j -rtsopts"

profile:
	stack build fair-miniKanren --ghc-options="-j" --profile

clean:
	stack clean

run:
	stack exec -- fair +RTS -H128m -K64m -RTS

profile_run:
	stack exec --profile -- cpd +RTS -p

compile_test:
	stack build fair-miniKanren --ghc-options="-j -rtsopts -olololo -main-is simplForwardOpt30"



define TESTRULES

run_$(1):
	stack exec -- fair $(1)

endef
$(foreach i,$(TESTS),$(eval $(call TESTRULES,$(i))))
