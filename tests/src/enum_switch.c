typedef enum flag {
	FLAG_ALL_GOOD,
	FLAG_NOT_SO_GOOD,
	FLAG_EH_BEEN_BETTER,
	FLAG_CAPTURED,
} flag_t;

typedef enum { false, true } bool;

bool testing(flag_t flag) {
	switch (flag) {
		case FLAG_ALL_GOOD:
			puts("nice");
			break;

		case FLAG_NOT_SO_GOOD:
		case FLAG_EH_BEEN_BETTER:
			puts("ok");
			break;

		case FLAG_CAPTURED:
		default:
			puts("yeah");
			break;
	}
}
