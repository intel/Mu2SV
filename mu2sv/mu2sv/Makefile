MU_FILES = $(wildcard ../examples/*.m)
SV_FILES = $(patsubst ../examples/%.m,../examples/%.sv,$(MU_FILES))

all:

examples: $(SV_FILES)

../examples/%.sv: ../examples/%.m
	python mu2sv.py $<

clean:
	rm -f ../examples/*.sv ../examples/*.tcl
