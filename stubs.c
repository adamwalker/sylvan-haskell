#include <sylvan.h>

BDD sylvan_ite_stub(BDD a, BDD b, BDD c){
    return sylvan_ite(a, b, c);
}

BDD sylvan_exists_stub(BDD a, BDD variables){
    return sylvan_exists(a, variables);
}

/*
BDD sylvan_relprod_stub(BDD a, BDD b, BDD vars){
    return sylvan_relprod(a, b, vars);
};
*/

BDD sylvan_compose_stub(BDD f, BDDMAP m){
    return sylvan_compose(f, m);
};

