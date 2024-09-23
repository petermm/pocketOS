include ../../icons.mk

PRIV := ../../../priv
DEST_PATH := $(PRIV)/icons/apps

.PHONY: directories

all: \
		directories \
    $(DEST_PATH)/terminal.rgba

directories: $(DEST_PATH)

MKDIR_P := mkdir -p

$(DEST_PATH):
	$(MKDIR_P) $(DEST_PATH)

$(DEST_PATH)/%.rgba: %.png
	$(MINRGB) rgba8888 $< $@
