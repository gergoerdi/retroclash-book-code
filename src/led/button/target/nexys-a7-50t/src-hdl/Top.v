module Top(input        BTNL,
           input        BTNR,
           output [1:0] LED
           );
   
   topEntity u_topEntity
     (.BTN_1(BTNL),
      .BTN_2(BTNR),
      .LED_1(LED[0]),
      .LED_2(LED[1])
      );
   
endmodule
