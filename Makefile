test-agg-sum:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-sum.mfotl -log ./examples/misc/agg.log

test-agg-cnt:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-cnt.mfotl -log ./examples/misc/agg.log

test-agg-sup:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-sup.mfotl -log ./examples/misc/agg.log

test-agg-min:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-min.mfotl -log ./examples/misc/agg.log

test-agg-group:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-group.mfotl -log ./examples/misc/agg.log

test-agg-no-group:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-no-group.mfotl -log ./examples/misc/agg.log

test-agg-multi:
	dune build && ./bin/whymon.exe -sig ./examples/misc/agg.sig -formula ./examples/misc/agg-multi.mfotl -log ./examples/misc/agg.log

test-basic-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/basic-agg.sig -formula ./examples/misc/basic-agg.mfotl -log ./examples/misc/basic-agg.log

test-complement-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/complement-agg.sig -formula ./examples/misc/complement-agg.mfotl -log ./examples/misc/complement-agg.log

test-inf-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/inf-agg.sig -formula ./examples/misc/inf-agg.mfotl -log ./examples/misc/inf-agg.log

test-strsup-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/strsup-agg.sig -formula ./examples/misc/strsup-agg.mfotl -log ./examples/misc/strsup-agg.log

test-strsup-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/strsup-agg.sig -formula ./examples/misc/strsup-agg.mfotl -log ./examples/misc/strsup-agg.log

test-empty-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/empty-agg.sig -formula ./examples/misc/empty-agg.mfotl -log ./examples/misc/empty-agg.log

test-vg-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/vg-agg.sig -formula ./examples/misc/vg-agg.mfotl -log ./examples/misc/vg-agg.log

test-dupl-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/dupl-agg.sig -formula ./examples/misc/dupl-agg.mfotl -log ./examples/misc/dupl-agg.log

test-cplex-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/cplex-agg.sig -formula ./examples/misc/cplex-agg.mfotl -log ./examples/misc/cplex-agg.log

test-reorder-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/reorder-agg.sig -formula ./examples/misc/reorder-agg.mfotl -log ./examples/misc/reorder-agg.log

test-gt:
	dune build && ./bin/whymon.exe -sig ./examples/misc/gt.sig -formula ./examples/misc/gt.mfotl -log ./examples/misc/gt.log

test-lt:
	dune build && ./bin/whymon.exe -sig ./examples/misc/lt.sig -formula ./examples/misc/lt.mfotl -log ./examples/misc/lt.log

test-gt-cplex:
	dune build && ./bin/whymon.exe -sig ./examples/misc/gt-cplex.sig -formula ./examples/misc/gt-cplex.mfotl -log ./examples/misc/gt-cplex.log

test-abc-agg:
	dune build && ./bin/whymon.exe -sig ./examples/misc/abc-agg.sig -formula ./examples/misc/abc-agg.mfotl -log ./examples/misc/abc-agg.log

build:
	dune build