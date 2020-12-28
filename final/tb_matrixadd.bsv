package tb_matrixadd;

import matrixAddition:: *;


(* synthesize *)
module mkTest (Empty);
	
	
	MatrixAdder_ifc v <- mkMatrixAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// rule to initailze the memory

	rule step0(step == 0);
	   v.mem_store(80,-60);
	   step <= 1;
	endrule	

	rule step1(step == 1);		
	   v.mem_store(81,330);
   	   step <= 2;
	endrule

	rule step2(step == 2);	
    	   v.mem_store(82,122);
	   step <= 3;
	endrule

	rule step3(step == 3);
	   v.mem_store(90,10);
       	   step <= 4;
	endrule
				
	  rule step4(step == 4);
	   v.mem_store(91, -100);
       	   step <= 5;
	endrule

	   rule step5(step == 5);
	   v.mem_store(92,-130);
       	   step <= 6;
	endrule
	
	rule step6(step == 6);
	   v.mem_store(93,800);
       	   step <= 7;
	endrule

	rule step7(step == 7);
	   v.mem_store(83,-4200);
       	   step <= 8;
	endrule
		
	// run the accelerator..
	rule rl_go(step == 8);
	
	v.put_src1 (80);
	v.put_s1rows(2);
	v.put_s1cols(2);

	v.put_src2 (90);
	v.put_s2rows(2);
	v.put_s2cols(2);

	v.put_dst (100);
	step <= 9;
	endrule


	// get the results and print them

	rule rl_getres1(step == 9 && v.iscomplete() );
	x <= unpack(v.mem_load( 100 ));
	step <= 10;
	endrule

	rule rl_getres2(step == 10 && v.iscomplete() );
	y <= unpack(v.mem_load( 101 ));
	step <= 11;
	endrule
	
	rule rl_getre3(step == 11 && v.iscomplete() );
	z <= unpack(v.mem_load( 102 ));
	step <= 12;
	endrule

	rule rl_getre4(step == 12 && v.iscomplete() );
	w <= unpack(v.mem_load( 103 ));
	step <= 13;
	endrule
	
	
	rule rl_getres4(step == 13 && v.iscomplete() );
	$display ("The final matrix:\n [ %d %d \n",x,y);
	$display (" %d %d ]\n",z,w);

	$display("\n\n");
	$finish();
	endrule

endmodule

endpackage
