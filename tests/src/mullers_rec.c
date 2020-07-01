double rec(double a, double b){
	return 108 - ((815 - 1500/b)/a);
}

int main(int argc, char *argv[]){
	int iters = 50;

	if (argc > 1) {
		iters = atoi(argv[1]);
	}

	double x = 4;
	double y = 4.25;

	for (unsigned i = 0; i < iters; i++) {
		double temp = rec(y, x);

		printf("%.08g\n", temp);

		x = y;
		y = temp;
	}

	return 0;
}
