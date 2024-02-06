pragma circom 2.0.7;

include "libs/comparators.circom";

template UnaryEnc(w) {
    signal input inp;
    signal output out[w];
    signal output success;
    var lc=0;

    for (var i=0; i<w; i++) {
        out[i] <-- (inp > i) ? 1 : 0;
        out[i] * (out[i] - 1) === 0;
        lc = lc + out[i];
    }
    for (var i=w-1; i>0; i--) {
        out[i] * (out[i - 1] + out[i] - 2) === 0;
    }

    component ie = IsEqual();
    ie.in[0] <== lc;
    ie.in[1] <== inp;
    ie.out ==> success;
    (success - 1) * (out[w - 1] - 1) === 0;
}

template UnaryEncCaller(w) {
    signal input inp;
    signal output out[w];

    component enc = UnaryEnc(w);
    enc.inp <== inp;
    for (var i = 0; i < w; i++) {
        out[i] <== enc.out[i];
    }
    enc.success === 1;
}

component main = UnaryEncCaller(3);
