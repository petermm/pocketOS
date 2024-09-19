iconsmk_path := $(abspath $(lastword $(MAKEFILE_LIST)))
iconsmk_dir := $(dir $(iconsmk_path))

tools_dir := $(iconsmk_dir)../tools

MINRGB := escript $(tools_dir)/minrgb.erl

$(info $$MINRGB is [${MINRGB}])
