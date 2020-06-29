shared char* alloc_cstr_not_implemented(unsigned size) {
	return shared(char[size]);
}

unique char* do_thing_not_implemented(unique char* thing) {
	return thing;
}

dynamic char* gc_thing(unsigned size) {
	return dynamic(char[size]);
}
