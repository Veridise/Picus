pragma circom 2.0.0;

include "../libs/circomlib-cff5ab6/multiplexer.circom";

template MyDecoder(w) {
    signal input inp;
    signal output out[w];
    signal output success;
    component dec = Decoder(2);
    inp === 0;
    dec.inp <== inp;
    dec.out[0] ==> out[0];
    dec.out[1] ==> out[1];
    out[0] === 0;
    out[1] === 0;
    dec.success ==> success;
}


component main = MyDecoder(2);
