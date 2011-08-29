interface OutputPulseFifo;
  method Action write(Bool x);
  method Bool notFull;
  method Bool doneBefore;
  method Bool done;
  method Action reset;
endinterface

function OutputPulseFifo getOutputPulseFifo(OutputPulse_ a);
  return (interface OutputPulseFifo;
            method Action write(Bool x) if(a.notFull);
              a.enq;
              if(x)
                a;
            endmethod

            method Bool notFull = a.notFull;
            method Bool doneBefore = a.doneBefore;
            method Bool done = a.done;
            method Action reset = a.reset;
          endinterface);
endfunction

interface ConditionalOutputFifo#(type t);
  method Action write(Maybe#(t) x);
  method Bool notFull;
  method Bool doneBefore;
  method Bool done;
  method Action reset;
endinterface

function ConditionalOutputFifo#(t) getConditionalOutputFifo(ConditionalOutput_#(t) a);
  return (interface ConditionalOutputFifo;
            method Action write(Maybe#(t) x) if(a.notFull && a.enNotFull);
              a.write(validValue(x));
              a.en(isValid(x));
            endmethod

            method Bool notFull = a.notFull && a.enNotFull;
            method Bool doneBefore = a.doneBefore && a.enConsumedBefore;
            method Bool done = a.done && a.enConsumed;

            method Action reset;
              a.reset;
              a.enReset;
            endmethod
          endinterface);
endfunction

interface ConditionalInputFifo#(type t);
  method Action deq;
  method Bool notEmpty;
  method Maybe#(t) first;
endinterface

function ConditionalInputFifo#(t) getConditionalInputFifo(ConditionalOutput#(t) a);
  return (interface ConditionalInputFifo;
            method Action deq if(a.notEmpty && a.enNotEmpty);
              a.deq;
              a.enDeq;
            endmethod

            method Bool notEmpty = a.notEmpty && a.enNotEmpty;

            method Maybe#(t) first if(a.notEmpty && a.enNotEmpty);
              return a.en? tagged Valid (a._read): tagged Invalid;
            endmethod
          endinterface);
endfunction
