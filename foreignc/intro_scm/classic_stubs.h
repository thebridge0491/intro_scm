#ifndef GAUCHE_CLASSIC_STUBS_H
#define GAUCHE_CLASSIC_STUBS_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

ScmObj Classic_fact_i(unsigned int n);
ScmObj Classic_fact_lp(unsigned int n);

ScmObj Classic_expt_i(float b, float n);
ScmObj Classic_expt_lp(float b, float n);

//void Scm_Init_classic_stubs(void);

SCM_DECL_END

#endif  /* GAUCHE_CLASSIC_STUBS_H */
