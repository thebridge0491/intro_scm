#include "intro_c/classic.h"
#include "intro_scm/classic_stubs.h"

ScmObj Classic_fact_i(unsigned int n) {
	unsigned long res_c = fact_i(n);
	ScmObj res = Scm_MakeInteger(res_c);
	return res;
}

ScmObj Classic_fact_lp(unsigned int n) {
	unsigned long res_c = fact_lp(n);
	ScmObj res = Scm_MakeInteger(res_c);
	return res;
}

ScmObj Classic_expt_i(float b, float n) {
	float res_c = expt_i(b, n);
	ScmObj res = Scm_MakeFlonum(res_c);
	return res;
}

ScmObj Classic_expt_lp(float b, float n) {
	float res_c = expt_lp(b, n);
	ScmObj res = Scm_MakeFlonum(res_c);
	return res;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_classic_stubslib(ScmModule *mod);

void Scm_Init_classic_stubs(void) {
	ScmModule *mod;

    /* Register this DSO to Gauche */
	SCM_INIT_EXTENSION(classic_stubs);

    /* Create the module if it doesn't exist yet. */
	mod = SCM_MODULE(SCM_FIND_MODULE("intro_scm.classic", TRUE));

    /* Register stub-generated procedures */
	Scm_Init_classic_stubslib(mod);
}

/*
 * To create classic_stubslib.c:
 *   cd build ; gauche-package compile -c -n ../intro_scm/classic_stubslib.stub
 * To compile/link dynamic lib:
 *   CPPFLAGS := $(CPPFLAGS) -I `gauche-config --incdirs | sed 's|:| -I|g'`
 *   LDFLAGS := $(LDFLAGS) -L `gauche-config --archdirs | sed 's|:| -L|g'`
 *   LDLIBS := $(LDLIBS) `gauche-config -l` -lintro_c-practice
 *   $(CC) $(CPPFLAGS) $(LDFLAGS) $(CFLAGS) -fPIC -shared classic_stubslib.c 
 *     ../intro_scm/classic_stubs.c -o intro_scm-classic_stubs.so $(LDLIBS)
 *   OR
 *   gauche-package compile -v --cppflags='-I..' --ldflags="-L."
 *     --libs='-lintro_c-practice' intro_scm-classic_stubs classic_stubslib.c ../intro_scm/classic_stubs.c
 */
