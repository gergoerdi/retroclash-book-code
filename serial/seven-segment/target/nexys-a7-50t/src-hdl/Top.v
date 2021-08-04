module Top(
           input        CLK100MHZ,
           input        UART_TXD_IN,
           output [7:0] AN,
           output [6:0] SEG,
           output       DP      
           );

   assign AN[7:4] = 4'b1111;
   
   SerialSS u_SerialSS
     (.CLK(CLK100MHZ),
      .RESET(1'b0),
      .ENABLE(1'b1),
      .RX(UART_TXD_IN),
      .SS_AN(AN[3:0]),
      .SS_SEG(SEG[6:0]),
      .SS_DP(DP)
      );
   
endmodule