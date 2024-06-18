BSC = bsc
P ?=
M ?=

BUILDDIR = build
OUTDIR = -bdir $(BUILDDIR) -info-dir $(BUILDDIR) -simdir $(BUILDDIR)/test -vdir $(BUILDDIR)/verilog
WORKDIR = -fdir $(abspath .)
BSVSRCDIR = $(abspath src)
BSVTESTDIR = $(abspath test)
BSVCONTDIR = $(abspath bsc-contrib)
DIRFLAGS = -p +:$(BSVSRCDIR) $(OUTDIR) $(WORKDIR) -p +:$(BSVCONTDIR)/Libraries/AMBA_TLM3/Axi4 -p +:$(BSVCONTDIR)/Libraries/AMBA_TLM3/Axi -p +:$(BSVCONTDIR)/Libraries/AMBA_TLM3/TLM3 -p +:$(BSVCONTDIR)/Libraries/Bus

SIMSCRIPT = $(BUILDDIR)/$(M).sh

BSC_FLAGS += $(DIRFLAGS)
BSC_FLAGS += -check-assert -show-range-conflict -show-module-use

compile_test:
	mkdir -p $(BUILDDIR)/test
	bsc -elab -sim -verbose  $(BSC_FLAGS) -p +:$(BSVTESTDIR) -g $(M) -u $(BSVTESTDIR)/$(P)

link_test: compile_test
	bsc -sim  $(BSC_FLAGS) -p +:$(BSVTESTDIR) -e $(M) -o $(SIMSCRIPT)

test_gen:
	mkdir -p $(BUILDDIR)/verilog
	bsc -verilog $(BSC_FLAGS) -p +:$(BSVTESTDIR) -g $(M) -u $(BSVTESTDIR)/$(P)

test: link_test test_gen
	$(SIMSCRIPT) -V $(BUILDDIR)/test/$(M).vcd -m 200000

gen:
	mkdir -p $(BUILDDIR)/verilog
	bsc -verilog  $(BSC_FLAGS) -g $(M) -u $(BSVTESTDIR)/$(P)

clean:
	rm -rf $(BUILDDIR)

.PHONY: compile link test clean