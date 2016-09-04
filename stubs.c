#include <sylvan.h>

BDD sylvan_ite_stub(BDD a, BDD b, BDD c){
    LACE_ME;
    return sylvan_ite(a, b, c);
}

BDD sylvan_and_stub(BDD a, BDD b){
    LACE_ME;
    return sylvan_and(a, b);
}

BDD sylvan_xor_stub(BDD a, BDD b){
    LACE_ME;
    return sylvan_xor(a, b);
}

BDD sylvan_exists_stub(BDD a, BDD variables){
    LACE_ME;
    return sylvan_exists(a, variables);
}

BDD sylvan_compose_stub(BDD f, BDDMAP m){
    LACE_ME;
    return sylvan_compose(f, m);
}

void sylvan_gc_stub(){
    LACE_ME;
    sylvan_gc();
}

