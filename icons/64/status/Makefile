include ../../icons.mk

PRIV := ../../../priv
DEST_PATH := $(PRIV)/icons/status

.PHONY: directories

all: \
		directories \
    $(DEST_PATH)/critical.rgba \
    $(DEST_PATH)/idea.rgba \
    $(DEST_PATH)/info.rgba \
    $(DEST_PATH)/warning.rgba

directories: $(DEST_PATH)

MKDIR_P := mkdir -p

$(DEST_PATH):
	$(MKDIR_P) $(DEST_PATH)

$(DEST_PATH)/%.rgba: %.png
	$(MINRGB) rgba8888 $< $@
