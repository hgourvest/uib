(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developers of the Original Code are
 *   fastmove     : John O'Harrow (john@almcrest.demon.co.uk)
 *   FastInt64Div : Dennis Kjaer Christensen
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

unit fastcode;

interface

procedure Move(const Source; var Dest; Count : Integer);
function FastInt64Div(X, Y : PInt64) : Int64;
function FastInt64Mul(X, Y : PInt64) : Int64;
function CompareText(const S1, S2: string): Integer;
procedure FillChar(var Dest; count: Integer; Value: Char);

implementation

const
  SMALLMOVESIZE = 36;

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01,@@Fwd02,@@Fwd03,@@Fwd04,@@Fwd05,@@Fwd06,@@Fwd07,@@Fwd08
  dd      @@Fwd09,@@Fwd10,@@Fwd11,@@Fwd12,@@Fwd13,@@Fwd14,@@Fwd15,@@Fwd16
  dd      @@Fwd17,@@Fwd18,@@Fwd19,@@Fwd20,@@Fwd21,@@Fwd22,@@Fwd23,@@Fwd24
  dd      @@Fwd25,@@Fwd26,@@Fwd27,@@Fwd28,@@Fwd29,@@Fwd30,@@Fwd31,@@Fwd32
  dd      @@Fwd33,@@Fwd34,@@Fwd35,@@Fwd36
@@Fwd36:
  mov     ecx,[eax-36]
  mov     [edx-36],ecx
@@Fwd32:
  mov     ecx,[eax-32]
  mov     [edx-32],ecx
@@Fwd28:
  mov     ecx,[eax-28]
  mov     [edx-28],ecx
@@Fwd24:
  mov     ecx,[eax-24]
  mov     [edx-24],ecx
@@Fwd20:
  mov     ecx,[eax-20]
  mov     [edx-20],ecx
@@Fwd16:
  mov     ecx,[eax-16]
  mov     [edx-16],ecx
@@Fwd12:
  mov     ecx,[eax-12]
  mov     [edx-12],ecx
@@Fwd08:
  mov     ecx,[eax-8]
  mov     [edx-8],ecx
@@Fwd04:
  mov     ecx,[eax-4]
  mov     [edx-4],ecx
  ret
@@Fwd35:
  mov     ecx,[eax-35]
  mov     [edx-35],ecx
@@Fwd31:
  mov     ecx,[eax-31]
  mov     [edx-31],ecx
@@Fwd27:
  mov     ecx,[eax-27]
  mov     [edx-27],ecx
@@Fwd23:
  mov     ecx,[eax-23]
  mov     [edx-23],ecx
@@Fwd19:
  mov     ecx,[eax-19]
  mov     [edx-19],ecx
@@Fwd15:
  mov     ecx,[eax-15]
  mov     [edx-15],ecx
@@Fwd11:
  mov     ecx,[eax-11]
  mov     [edx-11],ecx
@@Fwd07:
  mov     ecx,[eax-7]
  mov     [edx-7],ecx
  mov     ecx,[eax-4]
  mov     [edx-4],ecx
  ret
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3],cx
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1],cl
  ret
@@Fwd34:
  mov     ecx,[eax-34]
  mov     [edx-34],ecx
@@Fwd30:
  mov     ecx,[eax-30]
  mov     [edx-30],ecx
@@Fwd26:
  mov     ecx,[eax-26]
  mov     [edx-26],ecx
@@Fwd22:
  mov     ecx,[eax-22]
  mov     [edx-22],ecx
@@Fwd18:
  mov     ecx,[eax-18]
  mov     [edx-18],ecx
@@Fwd14:
  mov     ecx,[eax-14]
  mov     [edx-14],ecx
@@Fwd10:
  mov     ecx,[eax-10]
  mov     [edx-10],ecx
@@Fwd06:
  mov     ecx,[eax-6]
  mov     [edx-6],ecx
@@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2],cx
  ret
@@Fwd33:
  mov     ecx,[eax-33]
  mov     [edx-33],ecx
@@Fwd29:
  mov     ecx,[eax-29]
  mov     [edx-29],ecx
@@Fwd25:
  mov     ecx,[eax-25]
  mov     [edx-25],ecx
@@Fwd21:
  mov     ecx,[eax-21]
  mov     [edx-21],ecx
@@Fwd17:
  mov     ecx,[eax-17]
  mov     [edx-17],ecx
@@Fwd13:
  mov     ecx,[eax-13]
  mov     [edx-13],ecx
@@Fwd09:
  mov     ecx,[eax-9]
  mov     [edx-9],ecx
@@Fwd05:
  mov     ecx,[eax-5]
  mov     [edx-5],ecx
@@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1],cl
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01,@@Bwd02,@@Bwd03,@@Bwd04,@@Bwd05,@@Bwd06,@@Bwd07,@@Bwd08
  dd      @@Bwd09,@@Bwd10,@@Bwd11,@@Bwd12,@@Bwd13,@@Bwd14,@@Bwd15,@@Bwd16
  dd      @@Bwd17,@@Bwd18,@@Bwd19,@@Bwd20,@@Bwd21,@@Bwd22,@@Bwd23,@@Bwd24
  dd      @@Bwd25,@@Bwd26,@@Bwd27,@@Bwd28,@@Bwd29,@@Bwd30,@@Bwd31,@@Bwd32
  dd      @@Bwd33,@@Bwd34,@@Bwd35,@@Bwd36
@@Bwd36:
  mov     ecx,[eax+32]
  mov     [edx+32],ecx
@@Bwd32:
  mov     ecx,[eax+28]
  mov     [edx+28],ecx
@@Bwd28:
  mov     ecx,[eax+24]
  mov     [edx+24],ecx
@@Bwd24:
  mov     ecx,[eax+20]
  mov     [edx+20],ecx
@@Bwd20:
  mov     ecx,[eax+16]
  mov     [edx+16],ecx
@@Bwd16:
  mov     ecx,[eax+12]
  mov     [edx+12],ecx
@@Bwd12:
  mov     ecx,[eax+8]
  mov     [edx+8],ecx
@@Bwd08:
  mov     ecx,[eax+4]
  mov     [edx+4],ecx
@@Bwd04:
  mov     ecx,[eax]
  mov     [edx],ecx
  ret
@@Bwd35:
  mov     ecx,[eax+31]
  mov     [edx+31],ecx
@@Bwd31:
  mov     ecx,[eax+27]
  mov     [edx+27],ecx
@@Bwd27:
  mov     ecx,[eax+23]
  mov     [edx+23],ecx
@@Bwd23:
  mov     ecx,[eax+19]
  mov     [edx+19],ecx
@@Bwd19:
  mov     ecx,[eax+15]
  mov     [edx+15],ecx
@@Bwd15:
  mov     ecx,[eax+11]
  mov     [edx+11],ecx
@@Bwd11:
  mov     ecx,[eax+7]
  mov     [edx+7],ecx
@@Bwd07:
  mov     ecx,[eax+3]
  mov     [edx+3],ecx
  mov     ecx,[eax]
  mov     [edx],ecx
  ret
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1],cx
  movzx   ecx, byte ptr [eax]
  mov     [edx],cl
  ret
@@Bwd34:
  mov     ecx,[eax+30]
  mov     [edx+30],ecx
@@Bwd30:
  mov     ecx,[eax+26]
  mov     [edx+26],ecx
@@Bwd26:
  mov     ecx,[eax+22]
  mov     [edx+22],ecx
@@Bwd22:
  mov     ecx,[eax+18]
  mov     [edx+18],ecx
@@Bwd18:
  mov     ecx,[eax+14]
  mov     [edx+14],ecx
@@Bwd14:
  mov     ecx,[eax+10]
  mov     [edx+10],ecx
@@Bwd10:
  mov     ecx,[eax+6]
  mov     [edx+6],ecx
@@Bwd06:
  mov     ecx,[eax+2]
  mov     [edx+2],ecx
@@Bwd02:
  movzx   ecx, word ptr [eax]
  mov     [edx],cx
  ret
@@Bwd33:
  mov     ecx,[eax+29]
  mov     [edx+29],ecx
@@Bwd29:
  mov     ecx,[eax+25]
  mov     [edx+25],ecx
@@Bwd25:
  mov     ecx,[eax+21]
  mov     [edx+21],ecx
@@Bwd21:
  mov     ecx,[eax+17]
  mov     [edx+17],ecx
@@Bwd17:
  mov     ecx,[eax+13]
  mov     [edx+13],ecx
@@Bwd13:
  mov     ecx,[eax+9]
  mov     [edx+9],ecx
@@Bwd09:
  mov     ecx,[eax+5]
  mov     [edx+5],ecx
@@Bwd05:
  mov     ecx,[eax+1]
  mov     [edx+1],ecx
@@Bwd01:
  movzx   ecx, byte ptr[eax]
  mov     [edx],cl
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards;
asm
  push    ebx
  mov     ebx,edx
  fild    qword ptr [eax]
  add     eax,ecx {QWORD Align Writes}
  add     ecx,edx
  add     edx,7
  and     edx,-8
  sub     ecx,edx
  add     edx,ecx {Now QWORD Aligned}
  sub     ecx,16
  neg     ecx
@FwdLoop:
  fild    qword ptr [eax+ecx-16]
  fistp   qword ptr [edx+ecx-16]
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx,16
  jle     @FwdLoop
  fistp   qword ptr [ebx]
  neg     ecx
  add     ecx,16
  pop     ebx
  jmp     SmallForwardMove
end; {Forwards}

{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards;
asm
  push    ebx
  fild    qword ptr [eax+ecx-8]
  lea     ebx,[edx+ecx] {QWORD Align Writes}
  and     ebx,7
  sub     ecx,ebx
  add     ebx,ecx {Now QWORD Aligned, EBX = Original Length}
  sub     ecx,16
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fild    qword ptr [eax+ecx+8]
  fistp   qword ptr [edx+ecx+8]
  fistp   qword ptr [edx+ecx]
  sub     ecx,16
  jge     @BwdLoop
  fistp   qword ptr [edx+ebx-8]
  add     ecx,16
  pop     ebx
  jmp     SmallBackwardMove
end; {Backwards}

procedure Move(const Source; var Dest; Count : Integer);
asm
  cmp     ecx,SMALLMOVESIZE
  ja      @Large
  cmp     eax,edx
  lea     eax,[eax+ecx]
  jle     @SmallCheck
@SmallForward:
  add     edx,ecx
  jmp     SmallForwardMove
@SmallCheck:
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     eax,ecx
  jmp     SmallBackwardMove
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax,edx
  jg      Forwards
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  push    eax
  add     eax,ecx
  cmp     eax,edx
  pop     eax
  jg      Backwards {Source/Dest Overlap}
  jmp     Forwards
@Done:
end;

//Author:            Dennis Kjaer Christensen
//Date:              13/12 2003
//Optimized for:     RTL Replacement
//Instructionset(s): IA32
//Original Name:     Int64DivDKCFPU4
function FastInt64Div(X, Y : PInt64) : Int64;
asm
  fnstcw [esp-16].Word         //Get current controlword
  mov    cx, [esp-16]          //into ecx
  or     cx, 0000111100000000B //Bit 10-11 is rounding and bit10 = 1 & bit11 = 1 is truncation
  mov    [esp-8], cx           //Bit 8-9 is rounding and bit8 = 1 & bit9 = 1 is Extended
  fldcw  word ptr [esp-8]
  fild   qword ptr [X]
  fild   qword ptr [Y]
  fdivp
  mov    ecx,esp
  and    ecx,-8
  fistp  qword ptr [ecx-8]
  wait
  fldcw  word ptr [esp-16]     //Restore controlword
  mov    eax, [ecx-8]
  mov    edx, [ecx-4]
end;

function FastInt64Mul(X, Y : PInt64) : Int64;
asm  {36 Bytes}
  push  ebx
  push  edi
  lea   ebx, [eax]      //@X
  lea   edi, [edx]      //@Y
  mov   edx, [ebx+4]    //X-Hi
  mov   ecx, [edi+4]    //Y-Hi
  or    edx, ecx        //Both 0?
  mov   eax, [ebx]      //X-Lo
  jz    @@LowMul
  mov   edx, [edi]      //Y-Lo
  imul  ecx, eax        //Y-Hi * X-Lo
  imul  edx, [ebx+4]    //Y-Lo * X-Hi
  add   ecx, edx
@@LowMul:
  mul   dword ptr [edi]
  add   edx, ecx
  pop   edi
  pop   ebx
end;

//Author:            John O'Harrow
//Date:              N/A
//Optimized for:     RTL
//Instructionset(s): N/A
//Original Name:     CompareTextJOH_IA32_3

function CompareText(const S1, S2: string): Integer;
asm
  test   eax, eax
  jnz    @@CheckS2
  test   edx, edx
  jz     @@Ret
  mov    eax, [edx-4]
  neg    eax
@@Ret:
  ret
@@CheckS2:
  test   edx, edx
  jnz    @@Compare
  mov    eax, [eax-4]
  ret
@@Compare:
  push   ebx
  push   ebp
  push   esi
  mov    ebp, [eax-4]     {length(S1)}
  mov    ebx, [edx-4]     {length(S2)}
  sub    ebp, ebx         {Result if All Compared Characters Match}
  sbb    ecx, ecx
  and    ecx, ebp
  add    ecx, ebx         {min(length(S1),length(S2)) = Compare Length}
  lea    esi, [eax+ecx]   {Last Compare Position in S1}
  add    edx, ecx         {Last Compare Position in S2}
  neg    ecx
  jz     @@SetResult      {Exit if Smallest Length = 0}
@@Loop: {Load Next 2 Chars from S1 and S2 - May Include Null Terminator}
  movzx  eax, word ptr [esi+ecx]
  movzx  ebx, word ptr [edx+ecx]
  cmp    eax, ebx
  je     @@Next           {Next 2 Chars Match}
  cmp    al, bl
  je     @@SecondPair     {First Char Matches}
  mov    ah, 0
  mov    bh, 0
  cmp    al, 'a'
  jl     @@UC1
  cmp    al, 'z'
  jg     @@UC1
  sub    eax, 'a'-'A'
@@UC1:
  cmp    bl, 'a'
  jl     @@UC2
  cmp    bl, 'z'
  jg     @@UC2
  sub    ebx, 'a'-'A'
@@UC2:
  sub    eax, ebx         {Compare Both Uppercase Chars}
  jne    @@Done           {Exit with Result in EAX if Not Equal}
  movzx  eax, word ptr [esi+ecx] {Reload Same 2 Chars from S1}
  movzx  ebx, word ptr [edx+ecx] {Reload Same 2 Chars from S2}
  cmp    ah, bh
  je     @@Next           {Second Char Matches}
@@SecondPair:
  shr    eax, 8
  shr    ebx, 8
  cmp    al, 'a'
  jl     @@UC3
  cmp    al, 'z'
  jg     @@UC3
  sub    eax, 'a'-'A'
@@UC3:
  cmp    bl, 'a'
  jl     @@UC4
  cmp    bl, 'z'
  jg     @@UC4
  sub    ebx, 'a'-'A'
@@UC4:
  sub    eax, ebx         {Compare Both Uppercase Chars}
  jne    @@Done           {Exit with Result in EAX if Not Equal}
@@Next:
  add    ecx, 2
  jl     @@Loop           {Loop until All required Chars Compared}
@@SetResult:
  mov    eax, ebp         {All Matched, Set Result from Lengths}
@@Done:
  pop    esi
  pop    ebp
  pop    ebx
end;

//Author:            John O'Harrow
//Date:              N/A
//Optimized for:     RTL
//Instructionset(s): IA32
//Original Name:     FillCharJOH_FPU

procedure FillChar(var Dest; count: Integer; Value: Char);
asm {Size = 153 Bytes}
  cmp   edx, 32
  mov   ch, cl                    {Copy Value into both Bytes of CX}
  jl    @@Small
  mov   [eax  ], cx               {Fill First 8 Bytes}
  mov   [eax+2], cx
  mov   [eax+4], cx
  mov   [eax+6], cx
  sub   edx, 16
  fld   qword ptr [eax]
  fst   qword ptr [eax+edx]       {Fill Last 16 Bytes}
  fst   qword ptr [eax+edx+8]
  mov   ecx, eax
  and   ecx, 7                    {8-Byte Align Writes}
  sub   ecx, 8
  sub   eax, ecx
  add   edx, ecx
  add   eax, edx
  neg   edx
@@Loop:
  fst   qword ptr [eax+edx]       {Fill 16 Bytes per Loop}
  fst   qword ptr [eax+edx+8]
  add   edx, 16
  jl    @@Loop
  ffree st(0)
  ret
  nop
  nop
  nop
@@Small:
  test  edx, edx
  jle   @@Done
  mov   [eax+edx-1], cl       {Fill Last Byte}
  and   edx, -2               {No. of Words to Fill}
  neg   edx
  lea   edx, [@@SmallFill + 60 + edx * 2]
  jmp   edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov   [eax+28], cx
  mov   [eax+26], cx
  mov   [eax+24], cx
  mov   [eax+22], cx
  mov   [eax+20], cx
  mov   [eax+18], cx
  mov   [eax+16], cx
  mov   [eax+14], cx
  mov   [eax+12], cx
  mov   [eax+10], cx
  mov   [eax+ 8], cx
  mov   [eax+ 6], cx
  mov   [eax+ 4], cx
  mov   [eax+ 2], cx
  mov   [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

end.
