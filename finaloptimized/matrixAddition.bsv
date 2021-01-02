/***
Author : Jash Khatri (CS19S018)
Time of creation : 24 December 2020 at 15:26 IST.
Bluespec Code for : parameterized Domain specific accelerator for Integer Matrix Addition with dual ported memory.
***/

/**
* Main Module Name : mkMatrixAdder

* Parameters: 
	[in] Src1: Stores the starting address for the source Matrix1.
	[in] Src1numRows: Number of rows in Matrix starting at address Src1. 
	[in] Src1numCols: Number of columns in Matrix starting at address Src1.
	[in] Src2: Stores the starting address for the source Matrix2.
	[in] Src2numRows: Number of rows in Matrix starting at address Src2. 
	[in] Src2numCols: Number of columns in Matrix starting at address Src2. 
	[out] Dst: Stores the starting adress for the destination Matrix.

* Returns  : 
	SUCCESS: Operation successful. That is Matrix addition was completed successfully.
	FAIL: Matrix size check failed. That is sizes of both the matrix are not compatible for addition.

* Note : Matrices starting at the memory address Src1, Src2 and Dst are stored in Row Major Order in the Memory. 

* Module Description: 

  Element-by-element addition of two Matrices.
  [ A11 .... A1n	[ B11 .... B1n		[ A11+B11 .... A1n+B1n
    :	      :     +      :	    :	  =	     :		  :
    :	      :		   :	    :		     :		  :
    Am1 .... Amn ]	  Bm1 .... Bmn ]	  Am1+Bm1 .... Amn+Bmn ]
	
  Initially, the accelerator module mkMatrixAdder checks if Src1numRows, Src2numRows and Src1numCols, Src2numCols are matching or not.
  If they are not matching then FAIL signal is returned.
  Else the accelerator module mkMatrixAdder read the elements stored at memory location Src1 and Src2 does the addition and stores the results at the Dst memory location.
  Later the Src1, Src2 and Dst are incremented to point the next element in the all Matrices. This process is continued until Src1numRows*Src1numCols number of elements are processed.
  Once this is finished the SUCCESS signal is returned.

* DataType supported and Parameterization: Elements of the Matrices can be an Integers of any size. Size of the integer can be modified by changing value corresponding to the CBDATASIZE in the parameters.bsv file.
	
	For instance: 8-bit integer Matrix addition can be done by following modification in the parameters.bsv:
	 typedef 8 CBDATASIZE;

	Similarly, 20-bit integer Matrix addition can be done by following modification in the parameters.bsv:
	 typedef 20 CBDATASIZE;

*/

package matrixAddition;

import CBusMod::*;	// importing the CBus Module
import Vector :: *;	// Memory is defined as the vector
import parameters :: *; // import the tunable parameter from the file

////////////////////////////////////////////////////////////////////////////////
///

typedef ModWithCBus#(CBADDRSIZE, CBDATASIZE, i) MyModWithCBus#(type i);
typedef CBus#(CBADDRSIZE, CBDATASIZE) MyCBus;

////////////////////////////////////////////////////////////////////////////////
///
// Defining all the register present in the accelerator here and there associated actions...

interface SUCCESS#(type size_t);		// control bit to interact between accelerator and CPU informing CPU that accelerator has successfully finished matrix addition
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface MISMATCH#(type size_t);		// control bit to interact between accelerator and CPU informing CPU that matrix size check has failed
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR1#(type size_t);		// Interface for CSR bit using this accelerator know if CPU has initialized the src1, src2, dst addresses and row and column values of both the matrices
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface CSR2#(type size_t);		// CSR bit to interact between accelerator and CPU. Using this bit accelerator shows its readyness for the execution.
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);
endinterface

interface Src1Addr#(type size_t);	// Interface for register which stores the source1 address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Src2Addr#(type size_t);	// Interface for register which Stores the source2 address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstAddr#(type size_t);	// Interface for register which stores the destination address.
   method Reg#(Bit#(size_t)) value();
   method Action increment();
   method Action load(Bit#(size_t) newval);    
endinterface

// define the interface for src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum

interface Src1Rows#(type size_t);	// interface to store the number of rows of the source matrix1
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src2Rows#(type size_t);	// interface to store the number of rows of the source matrix2
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstRows#(type size_t);	// interface to store the number of rows of the destination matrix  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src1Cols#(type size_t);	// interface to store the number of cols of the source matrix1  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface Src2Cols#(type size_t);	// interface to store the number of cols of the source matrix2
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface DstCols#(type size_t);	// interface to store the number of cols of the destination matrix  
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface
// defined all the regs to stores the rows and cols info

interface Control_reg1#(type size_t);		// Interface to register which controls the access to the memory. Memory sets this bit if it is ready for operation else reset.
   method Bool   isZero();
   method Action load(Bit#(size_t) newval);     
endinterface

interface Control_reg2#(type size_t);		// Interface to register which indicates which memory operation(read/write) is being performed on the memory.
   method Bool   isZero();   
   method Action load(Bit#(size_t) newval);     
endinterface

interface MAR1#(type size_t);			// Interface for memory address register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR1#(type size_t);			// Interface for memory buffer register1..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MAR2#(type size_t);			// Interface for memory address register2..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

interface MBR2#(type size_t);			// Interface for memory buffer register2..
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// define interface for one more register attached to the accelerator that will store the number of elements to be processed in matrix
interface BLKSIZE#(type size_t);			// stores number of elements to be processed in matrix
   method Reg#(Bit#(size_t)) value();
   method Action load(Bit#(size_t) newval);    
endinterface

// defining interface for the Data Memory
interface DMem;
method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);
method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);

method (Bit#(CBDATASIZE)) load1 (Bit#(CBDATASIZE) mar);
method Action store1 (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);
endinterface

// module for the Data Memory
module mkDMem(DMem);
	
	Vector #( 256, Reg#(Bit#(CBDATASIZE)) ) memory_array  <- replicateM (mkReg(0));
	Reg#(Bit#(CBDATASIZE)) r <- mkReg(0);

	method (Bit#(CBDATASIZE)) load (Bit#(CBDATASIZE) mar);		// load operation to the memory
		return memory_array[mar];
	endmethod

	method Action store (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);		// store operation in to the memory
		memory_array[mar] <= mbr;
	endmethod

	method (Bit#(CBDATASIZE)) load1 (Bit#(CBDATASIZE) mar);		// Dual ported hence two load methods to load at same time in memory
		return memory_array[mar];
	endmethod

	method Action store1 (Bit#(CBDATASIZE) mar, Bit#(CBDATASIZE) mbr);	// Dual ported hence two store methods to store at same time in memory
		memory_array[mar] <= mbr;
	endmethod

endmodule

// Data Memory defination end..

// defining a CPU interface
interface CPU;
method (Bit#(CBDATASIZE)) returnflag();		// CPU returns the flag which indicates if CPU has initialized Src1, Src2, Dst and row,column-sizes of the input matrices
method (Bit#(CBDATASIZE)) returnsrc1addr();	// CPU gives the source1 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnsrc2addr();	// CPU gives the source2 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returndstaddr();	// CPU gives the destination address to accelerator using this method.

method (Bit#(CBDATASIZE)) returnsrc1rows();	// CPU gives the number of rows for matrix starting at source1 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnsrc1cols();	// CPU gives the number of columns for matrix starting at source1 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnsrc2rows();	// CPU gives the number of rows for matrix starting at source2 address to accelerator using this method.
method (Bit#(CBDATASIZE)) returnsrc2cols();	// CPU gives the number of columns for matrix starting at source2 address to accelerator using this method.

method Action putsrc1addr (Bit#(CBDATASIZE) s1); // Method used to put Src1 address inside the CPU obtained from the Test-bench
method Action putsrc2addr (Bit#(CBDATASIZE) s2); // Method used to put Src2 address inside the CPU obtained from the Test-bench
method Action putdstaddr (Bit#(CBDATASIZE) d);	// Method used to put Dst address inside the CPU obtained from the Test-bench

method Action puts1rows (Bit#(CBDATASIZE) sr1);	// Method used to put number of rows for matrix1 inside the CPU obtained from the Test-bench
method Action puts1cols (Bit#(CBDATASIZE) sc1);	// Method used to put number of columns for matrix1 inside the CPU obtained from the Test-bench
method Action puts2rows (Bit#(CBDATASIZE) sr2); // Method used to put number of rows for matrix2 inside the CPU obtained from the Test-bench
method Action puts2cols (Bit#(CBDATASIZE) sc2); // Method used to put number of columns for matrix2 inside the CPU obtained from the Test-bench

endinterface

// Declare and Define the methods to return the rows and cols for scr1,2.
// module for CPU..
module mkCPU(CPU);
	Reg#(Bit#(CBDATASIZE)) flag <- mkReg(1);	// register used to store the flag value
	Reg#(Bit#(CBDATASIZE)) src1 <- mkReg(0);	// register used to store the source1 address 
	Reg#(Bit#(CBDATASIZE)) src2 <- mkReg(0);	// register used to store the source2 address
	Reg#(Bit#(CBDATASIZE)) dst <- mkReg(0);		// register used to store the Destination address

	Reg#(Bit#(CBDATASIZE)) r1 <- mkReg(0);		// register used to store the matrix1 rows
	Reg#(Bit#(CBDATASIZE)) c1 <- mkReg(0);		// register used to store the matrix1 columns
	Reg#(Bit#(CBDATASIZE)) r2 <- mkReg(0);		// register used to store the matrix2 rows
	Reg#(Bit#(CBDATASIZE)) c2 <- mkReg(0);		// register used to store the matrix2 columns

	method (Bit#(CBDATASIZE)) returnsrc1rows();
		return r1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc1cols();
		return c1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2rows();
		return r2;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2cols();
		return c2;
	endmethod

	method (Bit#(CBDATASIZE)) returnflag();
		return flag;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc1addr();
		return src1;
	endmethod

	method (Bit#(CBDATASIZE)) returnsrc2addr();
		return src2;
	endmethod
	
	method (Bit#(CBDATASIZE)) returndstaddr();
		return dst;
	endmethod

	method Action putsrc1addr (Bit#(CBDATASIZE) s1);
		src1 <= s1;
	endmethod

	method Action putsrc2addr (Bit#(CBDATASIZE) s2);
		src2 <= s2;
	endmethod

	method Action putdstaddr (Bit#(CBDATASIZE) d);
		dst <= d;
	endmethod

	method Action puts1rows (Bit#(CBDATASIZE) sr1);
		r1 <= sr1;
	endmethod

	method Action puts1cols (Bit#(CBDATASIZE) sc1);
		c1 <= sc1;
	endmethod	

	method Action puts2rows (Bit#(CBDATASIZE) sr2);
		r2 <= sr2;
	endmethod

	method Action puts2cols (Bit#(CBDATASIZE) sc2);
		c2 <= sc2;
	endmethod

endmodule

// CPU interface ends here..

////////////////////////////////////////////////////////////////////////////////
///
// Exposing the accelerator's register to the CBus so they can be access by the CPU and MEMORY...

//SUCCESS
(* synthesize *)
module mkSUCCESSSynth(IWithCBus#(MyCBus, SUCCESS#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSUCCESS) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSUCCESS(SUCCESS#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) sc <- mkCBRegRW(8, 0);
   method Bool isZero();
      return (sc == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      sc <= newval;
   endmethod

endmodule

//MISMATCH
(* synthesize *)
module mkMISMATCHSynth(IWithCBus#(MyCBus, MISMATCH#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkMISMATCH) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkMISMATCH(MISMATCH#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) fail <- mkCBRegRW(9, 0);
   method Bool isZero();
      return (fail == 0);
   endmethod
   
   method Action load(Bit#(size_t) newval);
      fail <= newval;
   endmethod

endmodule

(* synthesize *)
module mkCSR1Synth(IWithCBus#(MyCBus, CSR1#(CBDATASIZE)));
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
module mkCSR2Synth(IWithCBus#(MyCBus, CSR2#(CBDATASIZE)));
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
module mkSrc1AddrSynth(IWithCBus#(MyCBus, Src1Addr#(CBDATASIZE)));
   let ifc2();
   exposeCBusIFC#(mkSrc1Addr) _temp(ifc2);
   return (ifc2);
endmodule

module [MyModWithCBus] mkSrc1Addr(Src1Addr#(size_t))
   provisos(Add#(size_t, k3, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src1 <- mkCBRegRW(12, 0);
   method Reg#(Bit#(size_t)) value();
      return src1;
   endmethod

    method Action increment();
      src1 <= src1 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2AddrSynth(IWithCBus#(MyCBus, Src2Addr#(CBDATASIZE)));
   let ifc3();
   exposeCBusIFC#(mkSrc2Addr) _temp(ifc3);
   return (ifc3);
endmodule

module [MyModWithCBus] mkSrc2Addr(Src2Addr#(size_t))
   provisos(Add#(size_t, k4, CBDATASIZE));
    
   Reg#(Bit#(size_t)) src2 <- mkCBRegRW(13, 0);
   method Reg#(Bit#(size_t)) value();
      return src2;
   endmethod

    method Action increment();
      src2 <= src2 + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      src2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstAddrSynth(IWithCBus#(MyCBus, DstAddr#(CBDATASIZE)));
   let ifc4();
   exposeCBusIFC#(mkDstAddr) _temp(ifc4);
   return (ifc4);
endmodule

module [MyModWithCBus] mkDstAddr(DstAddr#(size_t))
   provisos(Add#(size_t, k5, CBDATASIZE));
    
   Reg#(Bit#(size_t)) dst <- mkCBRegRW(14, 0);
   method Reg#(Bit#(size_t)) value();
      return dst;
   endmethod

    method Action increment();
      dst <= dst + 1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      dst <= newval;
   endmethod

endmodule

(* synthesize *)
module mkControlReg1Synth( IWithCBus#(MyCBus, Control_reg1#(CBDATASIZE)) );
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
module mkControlReg2Synth( IWithCBus#(MyCBus, Control_reg2#(CBDATASIZE)) );
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
module mkMAR1Synth( IWithCBus#(MyCBus, MAR1#(CBDATASIZE)) );
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

//MBR1
(* synthesize *)
module mkMBR1Synth( IWithCBus#(MyCBus, MBR1#(CBDATASIZE)) );
   let ifc8();
   exposeCBusIFC#(mkMBR1) _temp(ifc8);
   return (ifc8);
endmodule

module [MyModWithCBus] mkMBR1(MBR1#(size_t))
   provisos(Add#(size_t, k9, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mbr <- mkCBRegRW(18, 0);   
   method Reg#(Bit#(size_t)) value();
      return mbr;
   endmethod
   method Action load(Bit#(size_t) newval);
      mbr <= newval;
   endmethod

endmodule

//MAR2
(* synthesize *)
module mkMAR2Synth( IWithCBus#(MyCBus, MAR2#(CBDATASIZE)) );
   let ifc7();
   exposeCBusIFC#(mkMAR2) _temp(ifc7);
   return (ifc7);
endmodule

module [MyModWithCBus] mkMAR2(MAR2#(size_t))
   provisos(Add#(size_t, k8, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mar2 <- mkCBRegRW(26, 0);   
   method Reg#(Bit#(size_t)) value();
      return mar2;
   endmethod
   method Action load(Bit#(size_t) newval);
      mar2 <= newval;
   endmethod

endmodule

//MBR2
(* synthesize *)
module mkMBR2Synth( IWithCBus#(MyCBus, MBR2#(CBDATASIZE)) );
   let ifc8();
   exposeCBusIFC#(mkMBR2) _temp(ifc8);
   return (ifc8);
endmodule

module [MyModWithCBus] mkMBR2(MBR2#(size_t))
   provisos(Add#(size_t, k9, CBDATASIZE));
    
   Reg#(Bit#(size_t)) mbr2 <- mkCBRegRW(27, 0);   
   method Reg#(Bit#(size_t)) value();
      return mbr2;
   endmethod
   method Action load(Bit#(size_t) newval);
      mbr2 <= newval;
   endmethod

endmodule


// exposing the register attached to the accelerator that will store the blocksize

//Blksize
(* synthesize *)
module mkBLKSIZESynth( IWithCBus#(MyCBus, BLKSIZE#(CBDATASIZE)) );
   let ifc9();
   exposeCBusIFC#(mkBLKSIZE) _temp(ifc9);
   return (ifc9);
endmodule

module [MyModWithCBus] mkBLKSIZE(BLKSIZE#(size_t))
   provisos(Add#(size_t, k10, CBDATASIZE));
    
   Reg#(Bit#(size_t)) bsize <- mkCBRegRW(19, 0);   
   method Reg#(Bit#(size_t)) value();
      return bsize;
   endmethod
   method Action load(Bit#(size_t) newval);
      bsize <= newval;
   endmethod

endmodule

// exposing the src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum to cbus
(* synthesize *)
module mkSrc1RowsSynth(IWithCBus#(MyCBus, Src1Rows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc1Rows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc1Rows(Src1Rows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r1 <- mkCBRegRW(20, 0);
   method Reg#(Bit#(size_t)) value();
      return r1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r1 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc1ColsSynth(IWithCBus#(MyCBus, Src1Cols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc1Cols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc1Cols(Src1Cols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c1 <- mkCBRegRW(21, 0);
   method Reg#(Bit#(size_t)) value();
      return c1;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c1 <= newval;
   endmethod

endmodule

//Src2Rows Src2Cols

(* synthesize *)
module mkSrc2RowsSynth(IWithCBus#(MyCBus, Src2Rows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc2Rows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc2Rows(Src2Rows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r2 <- mkCBRegRW(22, 0);
   method Reg#(Bit#(size_t)) value();
      return r2;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r2 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkSrc2ColsSynth(IWithCBus#(MyCBus, Src2Cols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkSrc2Cols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkSrc2Cols(Src2Cols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c2 <- mkCBRegRW(23, 0);
   method Reg#(Bit#(size_t)) value();
      return c2;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c2 <= newval;
   endmethod

endmodule

// DstRows DstCols

(* synthesize *)
module mkDstRowsSynth(IWithCBus#(MyCBus, DstRows#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkDstRows) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkDstRows(DstRows#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) r3 <- mkCBRegRW(24, 0);
   method Reg#(Bit#(size_t)) value();
      return r3;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      r3 <= newval;
   endmethod

endmodule

(* synthesize *)
module mkDstColsSynth(IWithCBus#(MyCBus, DstCols#(CBDATASIZE)));
   let ifc();
   exposeCBusIFC#(mkDstCols) _temp(ifc);
   return (ifc);
endmodule

module [MyModWithCBus] mkDstCols(DstCols#(size_t))
   provisos(Add#(size_t, k, CBDATASIZE));
    
   Reg#(Bit#(size_t)) c3 <- mkCBRegRW(25, 0);
   method Reg#(Bit#(size_t)) value();
      return c3;
   endmethod
   
   method Action load(Bit#(size_t) newval);
      c3 <= newval;
   endmethod

endmodule

// end of exposing the register to CBus 

////////////////////////////////////////////////////////////////////////////////
///

// Interface defining the methods that can be accessed by the test-bench
interface MatrixAdder_ifc;

	method Action put_src1 (Bit#(CBDATASIZE) src1);		// This method allows test-bench to put the address of source1 in CPU.
	method Action put_src2 (Bit#(CBDATASIZE) src2);		// This method allows test-bench to put the address of source2 in CPU.
	method Action put_dst (Bit#(CBDATASIZE) dst);		// This method allows test-bench to put the address of destination to CPU.
	method Action put_s1rows (Bit#(CBDATASIZE) sr1);	// This method allows test-bench to put the number of rows in Matrix1 to the CPU.
	method Action put_s1cols (Bit#(CBDATASIZE) sc1);	// This method allows test-bench to put the number of columns in Matrix1 to the CPU.
	method Action put_s2rows (Bit#(CBDATASIZE) sr2);	// This method allows test-bench to put the number of rows in Matrix2 to the CPU.
	method Action put_s2cols (Bit#(CBDATASIZE) sc2);	// This method allows test-bench to put the number of columns in Matrix2 to the CPU.

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );		// This method allows test-bench to load data from the memory.
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );	// This method allows test-bench to store the data to the memory.

	method (Bit#(CBDATASIZE)) mem_load1 ( Bit#(CBDATASIZE) addr );		// Dual-ported memory hence two load operation that can be accessed by test-bench
	method Action mem_store1 ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );	// Dual-ported memory hence two store operation that can be accessed by test-bench

	method (Bool)  iscomplete();	// This method informs the test-bench if the accelarator has finished its execution or not.
	method (Bool)  sizemismatch();	// This method informs the test-bench if there is mismatch in the size of the matrices that are being added. 

endinterface: MatrixAdder_ifc

// The main accelerator module. It implements the MatrixAdder_ifc interface for interacting with the external test-bench.
(* synthesize *)
module mkMatrixAdder(MatrixAdder_ifc);

	// indicates if all the operation are finished and should we end the simulation
	// specific register declaration

   Reg#(Bool) cpufinishinit <- mkReg(False);		// Bool Register which is set to true when the CPU has initialized the Src1, Src2, Dst and rows and column of matrices
   Reg#(Bool) memorysignalsent <- mkReg(False);		// Bool Register which is set to true when memory sets the memory enable operation as true by setting control_reg1 to 1.

   Reg#(Bool) loadread1addr <- mkReg(False);		// Bool Register which is set to true when Src1 address and Src2 address are loaded into the MAR and MAR1
   Reg#(Bool) doneread1 <- mkReg(False);		// Bool Register which is set to true when value at the address Src1 and Src2 are successfully read from the DMem and loaded into MBR and MBR1

   Reg#(Bool) doneadd <- mkReg(False);			// Bool Register which is set to true when addition operation of values from MBR and MBR1 is finished
   Reg#(Bool) donesim <- mkReg(False);			// Bool Register which is set to true when results of the addition are stored back to Dst memory location

	// will need to check if there is size missmatch between src1 and src2 
	Reg#(Bool) mismatch <- mkReg(False);
	
	// Register that will count the number of elements that are already processed for the addition in Matrix1 and Matrix2. Initially no element is processed.
	Reg#(Bit#(CBDATASIZE)) countelements <- mkReg(0);

	// bool register to check if the values are initialized by testbench or not..
	Reg#(Bool) got_src1 <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the source1 address
   	Reg#(Bool) got_src2 <- mkReg(False); 	// Bool Register which is set to true when test-bench initializes the source2 address
	Reg#(Bool) got_dst <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the destination address

   	Reg#(Bool) got_s1r <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the num-of-rows of Matrix1
	Reg#(Bool) got_s1c <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the num-of-columns of Matrix1
	Reg#(Bool) got_s2r <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the num-of-rows of Matrix2
	Reg#(Bool) got_s2c <- mkReg(False);	// Bool Register which is set to true when test-bench initializes the num-of-columns of Matrix2

	// instaintate the memory module..
	DMem memory <- mkDMem;

	// instiantiate the CPU module..
	CPU cpu <- mkCPU;

	// instantiate the register which store the execution status. That is execution is complete or not.
   let successbit();
   mkSUCCESSSynth add_success(successbit);

	// instantiate the register which store the execution status showing if there is matrix size mismatch or not.
   let mismatchbit();
   mkMISMATCHSynth add_mismatch(mismatchbit);

	// instantiate the register which store signal sent from CPU after the initialization from the CPU.
   let csr1_ifc();
   mkCSR1Synth add_csr1(csr1_ifc);

	// instantiate the register which store the ready signal sent from the accelerator.
   let csr2_ifc1();
   mkCSR2Synth add_csr2(csr2_ifc1);

	// instantiate the register which store the source1 address
	let src1_ifc2();
   mkSrc1AddrSynth add_src1(src1_ifc2);

	// instantiate the register which store the source2 address
	let src2_ifc3();
   mkSrc2AddrSynth add_src2(src2_ifc3);

	// instantiate the register which store the destination address
	let dst_ifc4();
   mkDstAddrSynth add_dst(dst_ifc4);

	// instantiate the register which store the ready signal from memory
	let control_reg1();
   mkControlReg1Synth add_cr1(control_reg1);

	// instantiate the register which store the type of operation(read or write) performed on the memory.
	let control_reg2();
   mkControlReg2Synth add_cr2(control_reg2);

	// instantiate the first Memory Address Register
	let mar();
   mkMAR1Synth add_mar(mar);

	// instantiate the first Memory Buffer Register
	let mbr();
   mkMBR1Synth add_mbr(mbr);

	// instantiate the second Memory Address Register
	let mar1();
   mkMAR2Synth add_mar1(mar1);

	// instantiate the second Memory Buffer Register
	let mbr1();
   mkMBR2Synth add_mbr1(mbr1);

	// instiantiate the src1rownum, src1colnum, src2rownum, src2colnum, dstrownum, dstcolnum
	let src1rows();
   	mkSrc1RowsSynth add_src1rows(src1rows);

	let src1cols();
   	mkSrc1ColsSynth add_src1cols(src1cols);

	let src2rows();
   	mkSrc2RowsSynth add_src2rows(src2rows);

	let src2cols();
   	mkSrc2ColsSynth add_src2cols(src2cols);

	let dstrows();
   	mkDstRowsSynth add_dstrows(dstrows);

	let dstcols();
   	mkDstColsSynth add_dstcols(dstcols);


	// instiantiate the register which store the blocksize
	let  bsize();
   mkBLKSIZESynth add_bsize(bsize);

	// step1 accelerator shows it readiness
   	rule init_csr2_via_acc (got_src1 && got_src2 && got_dst && got_s1r && got_s1c && got_s2r && got_s2c && csr2_ifc1.device_ifc.isZero() );
   	   csr2_ifc1.device_ifc.load(1);		// Device is telling to cpu that it is ready for the data transfer
   	endrule

	// step2 CPU initializes the registers of the accelerator. 
   	rule init_csr1_reg_via_cpu (!csr2_ifc1.device_ifc.isZero() && !cpufinishinit );
      		csr1_ifc.cbus_ifc.write( 10,cpu.returnflag() );			// CPU is telling that it has initilaized the addresses in src1, scr2, dst by setting csr1 as 1.
		 src1_ifc2.cbus_ifc.write( 12,cpu.returnsrc1addr() );		// CPU initilaized the address in src1
		src2_ifc3.cbus_ifc.write( 13,cpu.returnsrc2addr() );		// CPU initilaized the address in scr2
		dst_ifc4.cbus_ifc.write( 14,cpu.returndstaddr() );		// CPU initilaized the address in dst

	// CPU will initialize src1rownum, src1colnum, src2rownum, src2colnum.	
		src1rows.cbus_ifc.write( 20,cpu.returnsrc1rows() );
		src1cols.cbus_ifc.write( 21,cpu.returnsrc1cols() );

		src2rows.cbus_ifc.write( 22,cpu.returnsrc2rows() );
		src2cols.cbus_ifc.write( 23,cpu.returnsrc2cols() );

		cpufinishinit <= True;
  	endrule

	// Condition to check if rows and cols of src1 and src2 are matching. If yes then set bsize=rows*cols and execute rest of the rule and set SUCCESS bit on sim finish
	// else set the MISMATCH bit and finish the sim immediately after it.

	// step3 Memory shows its readiness by making cr1 as 1. Accelerator will check the compatibility of matrices for addition and will inititalize the bsize. 
	rule memory_read_ready ( !csr1_ifc.device_ifc.isZero() && !memorysignalsent );
		control_reg1.cbus_ifc.write(15,1);		// Memory will make the memory enable as true..
		
		// function that checks if rows and cols of src1 and src2 are matching and will set the	mismatch flag	
		if(src1rows.device_ifc.value() != src2rows.device_ifc.value() || src1cols.device_ifc.value() != src2cols.device_ifc.value() ) 	// Accelarator will check for mismatch
			mismatch <= True;		// if size is mismatching then set this flag

		let x = src1rows.device_ifc.value() * src1cols.device_ifc.value(); 	// Accelarator initialize the bsize value from rows and cols size.
		bsize.device_ifc.load(x);		// initialize the block-size with value x

		memorysignalsent <= True;		// Memory has indicated that it is ready for taking read,write operations hence set this flag 
	endrule


	// step4 if the Memory is ready for the operation(i.e. cr1 == 1) and successbit is reset, then Accelerator loads the address to be read from memory in the MAR
	rule read_op1_from_memory ( !control_reg1.device_ifc.isZero() && !loadread1addr && successbit.device_ifc.isZero()  );

		// preprocessing
		mar.device_ifc.load( src1_ifc2.device_ifc.value() );	// accelarator will read the value of scr1 and will write into the MAR 
		mar1.device_ifc.load(src2_ifc3.device_ifc.value());	// accelarator will read the value of scr2 and will write into the MAR 
	
		control_reg2.device_ifc.load(0);			// accelarator will make cr2 as 0 indicates that it is doing read operation from memory..

		loadread1addr <= True;				// address loaded to the MARs hence set this flag to true
   	endrule

	// do read operation from src1 and src2 address..
	rule readsrc1( !control_reg1.device_ifc.isZero() && !doneread1 && loadread1addr );

		// invoke memory module's read
		mbr.cbus_ifc.write(18, memory.load( fromMaybe(0, mar.cbus_ifc.read(17) ) ) ); // Memory will read the value of mar do the load op from addr [mar] and then write to mbr		
		// invoke memory module's read	
		mbr1.cbus_ifc.write(27, memory.load1( fromMaybe(0, mar1.cbus_ifc.read(26) ) ) );	// Memory will read the value of mar1 do the load op from addr [mar1] and then write to mbr1
				
		doneread1 <= True;		// reading finished hence set this flag 
	endrule


	//step5 if doneread1 then do the addition operation and store results in MBR. 
	rule do_addition (!control_reg1.device_ifc.isZero() && doneread1 && !doneadd );
		// do the addition
		mbr.device_ifc.load( mbr1.device_ifc.value() + mbr.device_ifc.value() );	// accelarator takes value store in mbr1 and mbr and adds them store back to mbr
		//load dst to mar
		mar.device_ifc.load(dst_ifc4.device_ifc.value());			// accelarator will read the value of dst and will write into the MAR 
		control_reg2.device_ifc.load(1);					// accelarator will make cr2 as 1 indicates that it is doing write operation to the memory..
		doneadd <= True;
	endrule
	
	rule writeresults(!control_reg1.device_ifc.isZero() && doneread1 && doneadd && !donesim );
	    // write to the memory location given by [mar] the value stored in mbr
	    memory.store( fromMaybe(0, mar.cbus_ifc.read(17) ), fromMaybe(0, mbr.cbus_ifc.read(18) ) ); // Memory will read the value of mar and mbr from cbus and will write mbr value at the addr [mar]
	    countelements <= countelements + 1;		// increment the number of elements processed
	    
	    donesim <= True; 			//finished addition of current element of the Matrix and stored it properly in memory hence set it to True
	endrule



	//step6 if donesim is set do the bookkeeping work.
	rule donestep (donesim );
	
		// make all the 4 registers defined in this module as False
   		loadread1addr <= False;
   		doneread1 <= False;

   		doneadd <= False;
   		donesim <= False;

		src1_ifc2.device_ifc.increment();		// accelerator increments the src1 address
		src2_ifc3.device_ifc.increment();		// accelerator increments the src2 address
		dst_ifc4.device_ifc.increment();		// accelerator increments the dst address

		// check if done by comparing the register value which counts number of cycles with the register which stores the blocksize
		// if both matches then end the simulation
		if( countelements == bsize.device_ifc.value() )			// accelerator checks if number of elements process from Matrix1,2 is equal to the block size
		begin			
			successbit.device_ifc.load(1);			// matrix addition is finished hence accelerator will set successbit to 1
		end
   	endrule

	rule ismismatch(mismatch);
		mismatchbit.device_ifc.load(1);			// size mismatch hence set mismatch bit to 1. 
		successbit.device_ifc.load(1);			// Matrix addition is finished hence accelerator will set successbit to 1
	endrule

	// Working of the below defined methods are explained in MatrixAdder_ifc

	method Action put_src1 (Bit#(CBDATASIZE) src1) if (! got_src1);
		cpu.putsrc1addr(src1);			// put the src1 address in the cpu
		got_src1 <= True;
	endmethod

	method Action put_src2 (Bit#(CBDATASIZE) src2) if (! got_src2);
		cpu.putsrc2addr(src2);			// put the src2 address in the cpu
		got_src2 <= True;
	endmethod

	method Action put_dst (Bit#(CBDATASIZE) dst) if (! got_dst);
		cpu.putdstaddr(dst);			// put the dst address in the cpu
		got_dst <= True;
	endmethod

	method Action put_s1rows (Bit#(CBDATASIZE) sr1) if(!got_s1r);
		cpu.puts1rows(sr1);			// put the number of rows of matrix1 in the cpu
		got_s1r <= True;
	endmethod
	
	method Action put_s1cols (Bit#(CBDATASIZE) sc1) if(!got_s1c);
		cpu.puts1cols(sc1);			// put the number of columns of matrix1 in the cpu
		got_s1c <= True;
	endmethod

	method Action put_s2rows (Bit#(CBDATASIZE) sr2) if(!got_s2r);
		cpu.puts2rows(sr2);			// put the number of rows of matrix2 in the cpu
		got_s2r <= True;
	endmethod

	method Action put_s2cols (Bit#(CBDATASIZE) sc2) if(!got_s2c);
		cpu.puts2cols(sc2);			// put the number of columns of matrix2 in the cpu
		got_s2c <= True;
	endmethod

	method (Bit#(CBDATASIZE)) mem_load ( Bit#(CBDATASIZE) addr );
		 return memory.load(addr);		// loads the value from memory location addr
	endmethod
	
	method Action mem_store ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
   		memory.store(addr,val);			// store the value val at memory locaction addr
	endmethod

	method (Bit#(CBDATASIZE)) mem_load1 ( Bit#(CBDATASIZE) addr );
		 return memory.load1(addr);		// loads the value from memory location addr
	endmethod
	
	method Action mem_store1 ( Bit#(CBDATASIZE) addr , Bit#(CBDATASIZE) val );
   		memory.store1(addr,val);		// store the value val at memory locaction addr
	endmethod

	method (Bool) iscomplete();
		return ( !successbit.device_ifc.isZero() );		// return the status of success bit to get the status of execution(complete or not)
	endmethod

	method (Bool)  sizemismatch();
		return ( !mismatchbit.device_ifc.isZero() );		// return if there was a mismatch in matrix size while comparision
	endmethod
   
endmodule

////////////////////////////////////////////////////////////////////////////////

endpackage

