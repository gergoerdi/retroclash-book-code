module Top
  ( input        CLK100MHZ,
    input        BTN,
    output [1:0] LED
    );
   
   topEntity u_topEntity
     ( .CLK(CLK100MHZ),
       .BTN(BTNC),
       .LED(LED[1:0])
       );
   
endmodule
