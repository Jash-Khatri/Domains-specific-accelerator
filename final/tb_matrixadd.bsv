package tb_matrixadd;

import matrixAddition:: *;
import StmtFSM :: *;

(* synthesize *)
module mkTest (Empty);
	
	
	MatrixAdder_ifc v <- mkMatrixAdder;
	Reg#(int) step <- mkReg(0);

   	Reg#(Int#(CBDATASIZE)) x <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) y <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) z <- mkReg(0);
	Reg#(Int#(CBDATASIZE)) w <- mkReg(0);
		
	// rule to initailze the memory

	// rule to initailze the memory
	Stmt test_seq = seq

	   	v.mem_store(80,200);		// initialize the mem where matrix1 is present
	   	v.mem_store(81,30);
    	   	v.mem_store(82,-5);
		v.mem_store(83,-12000);

	   	v.mem_store(90, 5010);		// initialize the mem where matrix2 is present
	   	v.mem_store(91, 110);
	   	v.mem_store(92,-1);
	   	v.mem_store(93,8000);
	   	
      
		// run the accelerator..
		par
		v.put_src1 (80);		// init src1
		v.put_s1rows(2);
		v.put_s1cols(2);

		v.put_src2 (90);		// init src2
		v.put_s2rows(2);
		v.put_s2cols(2);

		v.put_dst (100);
		step <= 1;	
		endpar
	endseq;	
		
	Stmt test_seq1 = seq
	   	x <= unpack(v.mem_load( 100 ));		// get the values stored from the dst 
		y <= unpack(v.mem_load( 101 ));
		z <= unpack(v.mem_load( 102 ));
		w <= unpack(v.mem_load( 103 ));
		step <= 2;

	endseq;	


	FSM testfsm <- mkFSM (test_seq);
	FSM testfsm1 <- mkFSM (test_seq1);

	rule run1 ( True );
		testfsm.start ;
	endrule

	rule run2 (v.iscomplete() && step == 1  );								
		testfsm1.start ;
	endrule
	
	rule rl_print(step == 2);
	$display ("The final matrix:\n [ %d %d \n",x,y);
	$display (" %d %d ]\n",z,w);
	$display("\nTotal Time taken is: %d units", $time);
	$display("\n");
	$finish();
	endrule

endmodule

endpackage
