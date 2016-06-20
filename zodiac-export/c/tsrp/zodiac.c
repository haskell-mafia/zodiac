#include <stdlib.h>
#include <HsFFI.h>

#include "zodiac.h"

#include "../../dist/build/Zodiac/Export/TSRP_stub.h"

void z_tsrp_init(void) {
	int argc = 2;
	char *argv[] = { "+RTS", "-A1m", NULL };
	char **pargv = argv;

	hs_init(&argc, &pargv);
}

void z_tsrp_end(void) {
	hs_exit();
}

void z_tsrp_gen_key_id(uint8_t *buf) {
	_z_tsrp_gen_key_id(buf);
}
