package simpleAddition;

import CBusMod::*;
import Vector :: * ;	// Memory is defined as the vector

////////////////////////////////////////////////////////////////////////////////
///
//// Specific configuration bus size parameters
typedef 10 CBADDRSIZE; //size of configuration address bus to decode
typedef 10 CBDATASIZE; //size of configuration data bus 

//Vector #(1024, Int#(10)) memory_array;  	// defining a memory here...

typedef ModWithCBus#(CBADDRSIZE, CBDATASIZE, i) MyModWithCBus#(type i);
typedef CBus#(CBADDRSIZE, CBDATASIZE) MyCBus;

////////////////////////////////////////////////////////////////////////////////
///
// Defining all the register present in the accelerator here and there associated actions...

interface CSR1#(type size_t);
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR2#(type size_t);
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface Src1Addr#(type size_t);
   method Action increment();
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type 
endinterface

interface Src2Addr#(type size_t);
   method Action increment();
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type
endinterface

interface DstAddr#(type size_t);
   method Action increment();
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type
endinterface

interface Control_reg1#(type size_t);		// controls the access to the memory
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type
endinterface

interface Control_reg2#(type size_t);		// controls the access to the memory
   method Bool   isZero();   
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type
endinterface

interface MAR1#(type size_t);			// memory address register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);     // this should be address type instead of Bit#(size_t) type
endinterface

// defining interface for the DMemory
interface DMem;
method (Bit#(CBDATASIZE)) load (Bit#(CBADDRSIZE) mar);
method Action store (Bit#(CBADDRSIZE) mar, Bit#(CBDATASIZE) mbr);
endinterface

// module for the DMem
module mkDMem(DMem);
	
	Vector #( 1024, Reg#(Bit#(CBDATASIZE)) ) memory_array  <- replicateM (mkReg(10));
	Reg#(Bit#(CBDATASIZE)) r <- mkReg(0);

	method (Bit#(CBDATASIZE)) load (Bit#(CBADDRSIZE) mar);
		return memory_array[mar];
	endmethod

	method Action store (Bit#(CBADDRSIZE) mar, Bit#(CBDATASIZE) mbr);
		memory_array[mar] <= mbr;
	endmethod

endmodule

// DMem defination end..

////////////////////////////////////////////////////////////////////////////////
///
// Exposing the accelerator's register to the CBus to they can be access by the CPU and MEMORY...

(* synthesize *)
module mkCSR1Synth(IWithCBus#(MyCBus, CSR1#(8)));
   let ifc();
   exposeCBusIFC#(mkCSR1) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkCSR1(CSR1#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr1 <- mkCBRegRW(10, 0);
   method Bool isZero();
      return (csr1 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkCSR2Synth(IWithCBus#(MyCBus, CSR2#(8)));
   let ifc1();
   exposeCBusIFC#(mkCSR2) _temp(ifc1);
   return (ifc1);
endmodule

module [MyModWithCBus] mkCSR2(CSR2#(size_t))
   provisos(Add#(size_t, k1, CBDATASIZE));
    
   Reg#(Bit#(size_t)) csr2 <- mkCBRegRW(11, 0);
   method Bool isZero();
      return (csr2 == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      csr2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc1AddrSynth(IWithCBus#(MyCBus, Src1Addr#(8)));
   let ifc2();
   exposeCBusIFC#(mkSrc1Addr) _temp(ifc2);
   return (ifc2);
endmodule

module [MyModWithCBus] mkSrc1Addr(Src1Addr#(size_t))
   provisos(Add#(size_t, k3, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src1 <- mkCBRegRW(12, 0);
    method Action increment();
      src1 <= src1 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2AddrSynth(IWithCBus#(MyCBus, Src2Addr#(8)));
   let ifc3();
   exposeCBusIFC#(mkSrc2Addr) _temp(ifc3);
   return (ifc3);
endmodule

module [MyModWithCBus] mkSrc2Addr(Src2Addr#(size_t))
   provisos(Add#(size_t, k4, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src2 <- mkCBRegRW(13, 0);
    method Action increment();
      src2 <= src2 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstAddrSynth(IWithCBus#(MyCBus, DstAddr#(8)));
   let ifc4();
   exposeCBusIFC#(mkDstAddr) _temp(ifc4);
   return (ifc4);
endmodule

module [MyModWithCBus] mkDstAddr(DstAddr#(size_t))
   provisos(Add#(size_t, k5, CBDATASIZE));
    
   Reg#(Bit#(size_t)) dst <- mkCBRegRW(14, 0);
    method Action increment();
      dst <= dst + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      dst <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg1Synth( IWithCBus#(MyCBus, Control_reg1#(8)) );
   let ifc5();
   exposeCBusIFC#(mkControlReg1) _temp(ifc5);
   return (ifc5);
endmodule

module [MyModWithCBus] mkControlReg1(Control_reg1#(size_t))
   provisos(Add#(size_t, k6, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr1 <- mkCBRegRW(15, 0);
   method Bool isZero();
      return (cr1 == 0);
   endmethod   

   method Action load(Bit#(size_t) newval);
      cr1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg2Synth( IWithCBus#(MyCBus, Control_reg2#(8)) );
   let ifc6();
   exposeCBusIFC#(mkControlReg2) _temp(ifc6);
   return (ifc6);
endmodule

module [MyModWithCBus] mkControlReg2(Control_reg2#(size_t))
   provisos(Add#(size_t, k7, CBDATASIZE));
    
   Reg#(Bit#(size_t)) cr2 <- mkCBRegRW(16, 0);
   method Bool isZero();
      return (cr2 == 0);
   endmethod   
   
   method Action load(Bit#(size_t) newval);
      cr2 <= newval;
   endmethod

endmodule

//MAR1
(* synthesize *)
module mkMAR1Synth( IWithCBus#(MyCBus, MAR1#(8)) );
   let ifc7();
   exposeCBusIFC#(mkMAR1) _temp(ifc7);
   return (ifc7);
endmodule

module [MyModWithCBus] mkMAR1(MAR1#(size_t))
   provisos(Add#(size_t, k8, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mar <- mkCBRegRW(17, 0);   
   method Reg#(Bit#(size_t)) value();
      return mar;
   endmethod
   method Action load(Bit#(size_t) newval);
      mar <= newval;
   endmethod

endmodule

// end of exposing the register to CBus 

////////////////////////////////////////////////////////////////////////////////
///
// Simple test-bench code...

(* synthesize *)
module mkCBusExample(Empty);

	// indicates if all the operation are finished and should we end the simulation
   Reg#(Bool) doneread1 <- mkReg(False);
   Reg#(Bool) doneread2 <- mkReg(False);
   Reg#(Bool) donesim <- mkReg(False);

   Reg#(Bit#(CBDATASIZE)) op1 <- mkReg(0);
   Reg#(Bit#(CBDATASIZE)) op2 <- mkReg(0);
   //memory_array[50] <- 20;


	// instaintate the memory module..
	DMem memory <- mkDMem;

   let csr1_ifc();
   mkCSR1Synth add_csr1(csr1_ifc);

   let csr2_ifc1();
   mkCSR2Synth add_csr2(csr2_ifc1);

	let src1_ifc2();
   mkSrc1AddrSynth add_src1(src1_ifc2);

	let src2_ifc3();
   mkSrc2AddrSynth add_src2(src2_ifc3);

	let dst_ifc4();
   mkDstAddrSynth add_dst(dst_ifc4);

	let control_reg1();
   mkControlReg1Synth add_cr1(control_reg1);

	let control_reg2();
   mkControlReg2Synth add_cr2(control_reg2);

	let mar();
   mkMAR1Synth add_mar(mar);
   
   rule display_value (True);
      let read_value = fromMaybe(0, csr1_ifc.cbus_ifc.read(10));
      $display ("Current Value of CSR1 %2d at time:", read_value, $time);

	let read_value1 = fromMaybe(0, csr2_ifc1.cbus_ifc.read(11));
      $display ("Current Value of CSR2 %2d at time:", read_value1, $time);

	let read_value2 = fromMaybe(0, src1_ifc2.cbus_ifc.read(12));
      $display ("Current address at source1 %2d at time:", read_value2, $time);

	let read_value3 = fromMaybe(0, src2_ifc3.cbus_ifc.read(13));
      $display ("Current address at source2 %2d at time:", read_value3, $time);

	let read_value4 = fromMaybe(0, dst_ifc4.cbus_ifc.read(14));
      $display ("Current address at dest %2d at time:", read_value4, $time);

	let read_value5 = fromMaybe(0, control_reg1.cbus_ifc.read(15));
      $display ("Current address at cr1 %2d at time:", read_value5, $time);

	let read_value6 = fromMaybe(0, control_reg2.cbus_ifc.read(16));
      $display ("Current address at cr2 %2d at time:", read_value6, $time);

	let read_value7 = fromMaybe(0, mar.cbus_ifc.read(17));
      $display ("Current address at MAR %2d at time:", read_value7, $time);

   endrule

	// step1 accelerator shows it readiness
   	rule init_csr2_via_acc (csr2_ifc1.device_ifc.isZero());
   	   csr2_ifc1.device_ifc.load(1);			// Device is telling to cpu that it is ready for the data transfer
	   memory.store(50,1);
	   memory.store(100,2);
   	endrule



	// step2 CPU initializes the registers of the accelerator. 
   	rule init_csr1_reg_via_cpu (!csr2_ifc1.device_ifc.isZero());
      		csr1_ifc.cbus_ifc.write(10,1);			// CPU is telling that it has initilaized the addresses in src1, scr2, dst by setting csr1 as 1.
		 src1_ifc2.cbus_ifc.write(12,50);		// CPU initilaized the address in src1
		src2_ifc3.cbus_ifc.write(13,100);		// CPU initilaized the address in scr2
		dst_ifc4.cbus_ifc.write(14,150);		// CPU initilaized the address in dst
  	endrule



	// step3 Memory shows its readiness by making cr1 as 1. 
	rule memory_read_ready ( !csr1_ifc.device_ifc.isZero() );
		control_reg1.cbus_ifc.write(15,1);		// Memory will make the memory enable as true..
	endrule




	// step4 if the Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR and do read operation from the memory.
	rule read_op1_from_memory ( !control_reg1.device_ifc.isZero() && !doneread1 );

		// preprocessing
		mar.device_ifc.load(50);			// accelarator will write into the MAR (###This 50 is hard-coded need to take it from scr1###)
		control_reg2.cbus_ifc.write(16,0);		// Memory will make cr2 as 0 indicates that we are doing read operation..

		// invoke memory module's read
		op1 <= memory.load(50);			//(###This 50 is hard-coded need to take it from scr1###)
		let z = memory.load(50);			//(###This 50 is hard-coded need to take it from scr1###)
		$display ("\n\nValue at memory location [50] read is = %d", z);		

		doneread1 <= True;			// finish read1		
   	endrule



	// step5 if doneread1 and Memory is ready for the operation(i.e. cr1 == 1), then Accelerator loads the address to be read from memory in the MAR do read operation from memory
	rule read_op2_from_memory ( !control_reg1.device_ifc.isZero() && doneread1 && !doneread2);
		
		// preprocessing
		mar.device_ifc.load(100);			// accelarator will write into the MAR (###This 100 is hard-coded need to take it from scr2###)
		control_reg2.cbus_ifc.write(16,0);		// Memory will make cr2 as 0 indicates that we are doing read operation..
		
		// invoke memory module's read	
		op2 <= memory.load(100);			//(###This 100 is hard-coded need to take it from scr1###)
		let z = memory.load(100);			//(###This 100 is hard-coded need to take it from scr1###)
		$display ("\n\nValue at memory location [100] read is = %d", z);	

		doneread2 <= True;			// finish read2
	endrule



	//step6 if doneread1 and doneread2, then do the addition operation and store it in op1. Write the op1 to memory.
	rule do_addition ( doneread2 && doneread1);
	
	// do the addition op1 <= op1 + op2
	op1 <= op1 + op2;

	//load 150 to mar than write results stored in op1 to memory..
	mar.device_ifc.load(150);			// accelarator will write into the MAR (###This 150 is hard-coded need to take it from dst###)
	control_reg2.cbus_ifc.write(16,1);		// Memory will make cr2 as 1 indicates that we are doing write operation..

	// invoke module's write
	memory.store(150,op1);				// (###This 150 is hard-coded need to take it from dst###)

		donesim <= True; 			//finish the simulation
	endrule



	//step7 if donesim is set then addition is finished print the final value at dst and end the simulation
	rule done (donesim);
		let z = memory.load(150);
		$display ("\n\nValue at memory location [150] read is = %d", z);	
      	 	$finish();
   	endrule

   
endmodule

////////////////////////////////////////////////////////////////////////////////

endpackage

