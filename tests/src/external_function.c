const int foobizz = 10;

typedef enum flag {
	FLAG_GOOD,
	FLAG_BAD,
	FLAG_CAPTURED,
	FLAG_WAVING,
} flag_t;

union bar {
	int s;
	unsigned u;
};

typedef struct {
	int a, b;
} meh_t;

struct bar {
	char baz[16];
};

typedef unsigned int uint32_t;

unsigned int puts(const char *s);
int *baz(unsigned int *foo);

typedef int (*fptr)(unsigned);
fptr ret_a_function(int (*compare)(const void *, const void *));

int struct test {
	struct blarg {
		int k;
	} k;

	int a;
	unsigned b;
} foobar(void) {
	struct blarg;
	return (struct test){0, 1};
}

void somefunc() {
	typedef struct foobizz {
		uint32_t fuzzbang;
		char     overflow[4];
	};

	typedef char buzz;
}

typedef struct noname {
	int has_no_name;
};

typedef char;

typedef struct realdecl {
	int a, real, declaration;
} realdecl_t;

int main(int argc, char *argv[]) {
	;;;
	typedef;
	puts("Hell, O World!");
	return 0;
}
