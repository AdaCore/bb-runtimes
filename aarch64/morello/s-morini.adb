------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          S Y S T E M . M O R E L L O . I N I T I A L I Z A T I O N       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  On Morello, accesses to global objects go through a capability in the
--  Global Offset Table (GOT). This package initializes these global
--  capabilities by processing the linker-generated tables and deriving each
--  capability from the Program Counter Capability (PCC) or Default Data
--  Capability (DDC), with the bounds and permissions defined in the table.
--
--  Note that the method used to initialise the GOT depends on the linker used:
--    - LLD generates a __cap_table which describes each capability to be
--      created: the address, bounds, and permissions of the capability, and
--      the address in memory at which the resulting capability is stored
--      (in the GOT).
--    - LD emits R_MORELLO_RELATIVE dynamic relocations (.rela.dyn) to
--      initialize capabilities.

with Interfaces;              use Interfaces;
with Interfaces.CHERI;
with System.Storage_Elements; use System.Storage_Elements;

package body System.Morello.Initialization is

   package SSE renames System.Storage_Elements;

   type Unsigned_56 is mod 2**56 with Size => 56;

   EH_Frame_Hdr : constant Character with
     Import, Convention => C, External_Name => "__eh_frame_hdr";

   EH_Frame_Hdr_Limit : constant Character with
     Import, Convention => C, External_Name => "__eh_frame_hdr_limit";
   --  Marks the upper limit of the bounds for __eh_frame_hdr_cap

   EH_Frame_Hdr_Cap : System.Address with
     Export, Convention => C, External_Name => "__eh_frame_hdr_cap";
   --  A capability that points to __eh_frame_hdr with bounds up to
   --  __eh_tables_end.
   --
   --  The linker script must ensure that all exception handling frames/tables
   --  are located between these two symbols.

   procedure Initialize_Global_Capabilities;
   --  Walk through the __cap_table and generate a capability for each entry

   procedure Process_Dynamic_Relocations;
   --  Process dynamic relocations (.rela.dyn entries).
   --
   --  This processes only R_MORELLO_RELATIVE relocations which are used
   --  to initialize some global capabilities.

   procedure Initialize_Exception_Handling_Frame_Header_Capability;
   --  Create a capability that points to __eh_frame_hdr with bounds up to
   --  __eh_frame_hdr_limit.
   --
   --  By default, the __eh_frame_hdr symbol defined in the linker script only
   --  has bounds for the section it's defined in (.eh_frame_hdr). The unwinder
   --  uses this pointer to access the various exception handling data in other
   --  sections (.eh_frame, .gcc_except_table, and some parts of
   --  .data.rel.ro), but they are are not within the bounds of __eh_frame_hdr.
   --  This procedure creates a capability with sufficient bounds for the
   --  unwinder to use to access those tables.

   ----------------------
   -- Capability Table --
   ----------------------

   type Cap_Table_Entry is record
      Cap_Location : SSE.Integer_Address;
      Base         : SSE.Integer_Address;
      Offset       : SSE.Storage_Offset;
      Size         : Interfaces.CHERI.Bounds_Length;
      Permissions  : Unsigned_64;
   end record with
     Size => 64 * 5;
   --  Layout of each __cap_table entry

   for Cap_Table_Entry use record
      Cap_Location at  0 range 0 .. 63;
      Base         at  8 range 0 .. 63;
      Offset       at 16 range 0 .. 63;
      Size         at 24 range 0 .. 63;
      Permissions  at 32 range 0 .. 63;
   end record;

   subtype Count is SSE.Storage_Count;

   type Cap_Table_Entry_Array is array (Count range <>) of Cap_Table_Entry;

   function Get_Cap_Relocs_Start return System.Address with
     Import,
     Convention    => Asm,
     External_Name => "__get_cap_relocs_start";

   function Get_Cap_Relocs_End return System.Address with
     Import,
     Convention    => Asm,
     External_Name => "__get_cap_relocs_end";

   function Is_Code_Relocation
     (Reloc : Cap_Table_Entry)
      return Boolean is
        ((Reloc.Permissions and 16#8000_0000_0000_0000#) /= 0);

   function Is_RO_Data_Relocation
     (Reloc : Cap_Table_Entry)
      return Boolean is
        ((Reloc.Permissions and 16#4000_0000_0000_0000#) /= 0);

   -------------------------
   -- Dynamic Relocations --
   -------------------------

   type ELF64_Rela is record
      R_Offset : SSE.Integer_Address;
      R_Info   : Unsigned_64;
      R_Addend : SSE.Storage_Offset;
   end record with
     Size => 64 * 3;
   --  Layout of an ELF64 relative entry

   for ELF64_Rela use record
      R_Offset at  0 range 0 .. 63;
      R_Info   at  8 range 0 .. 63;
      R_Addend at 16 range 0 .. 63;
   end record;

   R_Morello_Relative : constant := 59395;
   --  Value of R_Info for R_MORELLO_RELATIVE relocations

   type Morello_Relative_Fragment is record
      Virtual_Address : SSE.Integer_Address;
      Length          : Unsigned_56;
      Permissions     : Unsigned_8;
   end record with
     Size => 128;
   --  Layout of R_MORELLO_RELATIVE fragments

   for Morello_Relative_Fragment use record
      Virtual_Address at 0 range  0 .. 63;
      Length          at 8 range  0 .. 55;
      Permissions     at 8 range 56 .. 63;
   end record;

   type ELF64_Rela_Array is array (Count range <>) of ELF64_Rela;

   function Get_Rela_Dyn_Start return System.Address with
     Import,
     Convention    => Asm,
     External_Name => "__get_rela_dyn_start";

   function Get_Rela_Dyn_End return System.Address with
     Import,
     Convention    => Asm,
     External_Name => "__get_rela_dyn_end";

   function Is_Code_Relocation
     (Reloc : Morello_Relative_Fragment)
      return Boolean is
        (Reloc.Permissions = 4);

   function Is_RO_Data_Relocation
     (Reloc : Morello_Relative_Fragment)
      return Boolean is
        (Reloc.Permissions = 1);

   -----------------------------
   -- Permission Declarations --
   -----------------------------

   --  These definitions are used to restrict the inherited permissions of
   --  the capabilities created at start-up, depending on the kind of
   --  capability being created.

   use type CHERI.Permissions_Mask;

   Code_Permissions : constant CHERI.Permissions_Mask :=
     not (CHERI.Permit_Seal or
          CHERI.Permit_Store or
          CHERI.Permit_Store_Capability);
   --  Pointers to code cannot be used to seal or store

   RO_Data_Permissions : constant CHERI.Permissions_Mask :=
     not (CHERI.Permit_Seal or
          CHERI.Permit_Store or
          CHERI.Permit_Store_Capability or
          CHERI.Permit_Store_Local or
          CHERI.Permit_Execute);
   --  Pointers to constant data cannot be written to, executed, or sealed

   RW_Data_Permissions : constant CHERI.Permissions_Mask :=
     not (CHERI.Permit_Seal or CHERI.Permit_Execute);
   --  Pointers to read/write data cannot be executed or sealed

   -----------------------------------------------------------
   -- Initialize_Exception_Handling_Frame_Header_Capability --
   -----------------------------------------------------------

   procedure Initialize_Exception_Handling_Frame_Header_Capability is
      Base  : constant Integer_Address := To_Integer (EH_Frame_Hdr'Address);
      Limit : constant Integer_Address :=
                To_Integer (EH_Frame_Hdr_Limit'Address);
   begin
      EH_Frame_Hdr_Cap := CHERI.Capability_With_Address_And_Bounds
        (Cap     => CHERI.Get_DDC and RO_Data_Permissions,
         Address => Base,
         Length  => CHERI.Bounds_Length (Limit - Base));
   end Initialize_Exception_Handling_Frame_Header_Capability;

   ------------------------------------
   -- Initialize_Global_Capabilities --
   ------------------------------------

   procedure Initialize_Global_Capabilities is
      Relocs_Start : constant Address := Get_Cap_Relocs_Start;
      Relocs_End   : constant Address := Get_Cap_Relocs_End;

      Num_Relocs : constant Count := Count
        ((To_Integer (Relocs_End) - To_Integer (Relocs_Start))
         / (Cap_Table_Entry'Size / Storage_Unit));

      Cap_Relocs : constant Cap_Table_Entry_Array (1 .. Num_Relocs) with
        Import, Address => Relocs_Start;

      Code_Cap    : CHERI.Capability;
      RW_Data_Cap : CHERI.Capability;
      RO_Data_Cap : CHERI.Capability;

      Parent_Cap : CHERI.Capability;
      Cap        : CHERI.Capability;

   begin
      --  Restrict permissions depending on the kind of pointer stored

      Code_Cap    := CHERI.Get_PCC and Code_Permissions;
      RW_Data_Cap := CHERI.Get_DDC and RW_Data_Permissions;
      RO_Data_Cap := CHERI.Get_DDC and RO_Data_Permissions;

      --  Process each relocation

      for Reloc of Cap_Relocs loop
         --  Select which parent capability is used to derive the target
         --  capability. This determines which permissions are inherited.

         Parent_Cap := (if Is_Code_Relocation (Reloc)       then Code_Cap
                        elsif Is_RO_Data_Relocation (Reloc) then RO_Data_Cap
                        else RW_Data_Cap);

         --  Setup the capability with its bounds

         Cap := CHERI.Capability_With_Address_And_Bounds
                  (Cap     => Parent_Cap,
                   Address => Reloc.Base,
                   Length  => Reloc.Size);
         Cap := Cap + Reloc.Offset;

         --  Convert function pointers to sentries

         if Is_Code_Relocation (Reloc) then
            Cap := CHERI.Create_Sentry (Cap);
         end if;

         --  Write the capability to its target location

         declare
            Target_Cap : CHERI.Capability with
              Import,
              Address => CHERI.Capability_With_Address
                           (RW_Data_Cap, Reloc.Cap_Location);
         begin
            Target_Cap := Cap;
         end;
      end loop;

   end Initialize_Global_Capabilities;

   ------------------------
   -- Initialize_Morello --
   ------------------------

   procedure Initialize_Morello is
   begin
      Initialize_Global_Capabilities;
      Process_Dynamic_Relocations;
      Initialize_Exception_Handling_Frame_Header_Capability;
   end Initialize_Morello;

   ---------------------------------
   -- Process_Dynamic_Relocations --
   ---------------------------------

   procedure Process_Dynamic_Relocations is
      Relocs_Start : constant Address := Get_Rela_Dyn_Start;
      Relocs_End   : constant Address := Get_Rela_Dyn_End;

      Num_Relocs : constant Count := Count
        ((To_Integer (Relocs_End) - To_Integer (Relocs_Start))
         / (ELF64_Rela'Size / Storage_Unit));

      Relocs : constant ELF64_Rela_Array (1 .. Num_Relocs) with
        Import, Address => Relocs_Start;

      Code_Cap    : CHERI.Capability;
      RW_Data_Cap : CHERI.Capability;
      RO_Data_Cap : CHERI.Capability;

      Parent_Cap : CHERI.Capability;
      Cap        : CHERI.Capability;

      Target_Address : Address;

   begin
      --  Restrict permissions depending on the kind of pointer stored

      Code_Cap    := CHERI.Get_PCC and Code_Permissions;
      RW_Data_Cap := CHERI.Get_DDC and RW_Data_Permissions;
      RO_Data_Cap := CHERI.Get_DDC and RO_Data_Permissions;

      --  Process each relocation

      for Reloc of Relocs loop
         Target_Address :=
           CHERI.Capability_With_Address (Relocs_Start, Reloc.R_Offset);

         if Reloc.R_Info = R_Morello_Relative then
            declare
               Fragment : constant Morello_Relative_Fragment with
                 Import, Address => Target_Address;
            begin
               Parent_Cap :=
                 (if Is_Code_Relocation (Fragment)       then Code_Cap
                  elsif Is_RO_Data_Relocation (Fragment) then RO_Data_Cap
                  else RW_Data_Cap);

               --  Setup the capability with its bounds

               Cap := CHERI.Capability_With_Address_And_Bounds
                        (Cap     => Parent_Cap,
                         Address => Fragment.Virtual_Address,
                         Length  => CHERI.Bounds_Length (Fragment.Length));
               Cap := Cap + Reloc.R_Addend;

               --  Convert function pointers to sentries

               if Is_Code_Relocation (Fragment) then
                  Cap := CHERI.Create_Sentry (Cap);
               end if;
            end;

            --  Write the capability to its target location

            declare
               Target_Cap : CHERI.Capability with
                 Import,
                 Address => Target_Address;
            begin
               Target_Cap := Cap;
            end;
         end if;
      end loop;

   end Process_Dynamic_Relocations;

end System.Morello.Initialization;
