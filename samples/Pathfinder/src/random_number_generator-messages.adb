--------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Fibonacci;
with Read_Number.API;

package body Random_Number_Generator.Messages is
   ML : Message_Loop;

   ---------------
   -- Message Loop
   ---------------

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "L[" & Pri'Image & " ]: ";
   begin
      --  Initialize the internal workings of the module (if required)
      --  before processing any of its messages.  It may instead be
      --  appropriate for the package to initialize itself at
      --  elaboration time.  Note that SPARK requires that
      --  applications use the configuration pragma of
      --  Partition_Elaboration_Policy set to Sequential.  This
      --  ensures that all packages are elaborated before any library
      --  level tasks start.
      --
      Initialize;

      --  Process messages as they arrive.  This simple loop may be
      --  all that is needed.  It may also be appropriate to do some
      --  prechecks or preprocessing of messages before calling
      --  Process_Message.
      --
      loop
         Ada.Text_IO.Put_Line
           (Call_Sign & "Wasting time generating a Fibonacci number...");
         Fib_Number := Fibonacci.Gen_Slowest (FSeed);

         Ada.Text_IO.Put (Call_Sign & "Fibonacci(" & FSeed'Image & " ) is");
         Ada.Text_IO.Put_Line (Fib_Number'Image);

         Ada.Text_IO.Put_Line
           (Call_Sign & "Getting next message to process...");
         Message_Manager.Fetch_Message (ID, Incoming_Message);
         Process (Incoming_Message);
      end loop;
   end Message_Loop;

   --  The package initializer, if needed.  This procedure might be
   --  called as the message loop (see below) is starting, or perhaps
   --  during package elaboration.  If this procedure is not needed,
   --  it should be removed to avoid SPARK flow issues.
   --
   procedure Initialize is
   begin
      null;
   end Initialize;

   -----------------------------------
   --  Message Decoding and Dispatching
   -----------------------------------

   -- This procedure processes exactly one message.
   procedure Process (Message : in Message_Record) is
   begin
      if Random_Number_Generator.API.Is_Generate_Number_Request (Message) then
         Handle_Generate_Number_Request (Message);
      else
         --  An unknown message type has been received. What should be
         --  done about that?
         null;
      end if;
      --  When this procedure returns the message loop will
      --  immediately try to receive the next message.  Note that all
      --  CubedOS send operations are non-blocking so sending an
      --  outgoing message will not delay execution.
   end Process;

   --------------- Message Handling ---------------

   --  Here is where the details of handing the messages is done.
   --  Probably there will be a separate subprogram for each message,
   --  but the exact organization is, obviously, dependent on the
   --  needs of the module.  It might be useful to put these message
   --  handling subprograms into a private sibling package.  That
   --  would move the complex details of message handling to a
   --  different file and reduce clutter in this file.  This file is
   --  really just about decoding and dispatching the messages.  We
   --  recommend that if a single internal package is used that it
   --  sould be called Sample_Module.Core (for example).

   procedure Handle_Generate_Number_Request (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Status           : Message_Status_Type;
      Value            : Positive;
      Call_Sign        : constant String :=
        "L[" & Pri'Image & " ] -- Handle Request: ";
   begin
      Random_Number_Generator.API.Generate_Number_Request_Decode
        (Message, Status, Value);
      --  Act on the request message.

      if Status = Malformed then
         Ada.Text_IO.Put_Line
           (Call_Sign & "Error: Message status is Malformed!");
         Ada.Text_IO.New_Line;
         return;
      end if;

      Ada.Text_IO.Put (Call_Sign & "The random number is");
      Ada.Text_IO.Put_Line (Value'Image);

      Ada.Text_IO.Put_Line (Call_Sign & "Sending Reply...");
      Ada.Text_IO.New_Line;

      Outgoing_Message :=
        Read_Number.API.Read_Number_Reply_Encode
          (Receiver_Domain => Domain_ID, Receiver => Read_Number.ID,
           Request_ID      => Read_Number.R_ID, Priority => Read_Number.Pri,
           Value           => Value);

      Message_Manager.Route_Message (Outgoing_Message);

   end Handle_Generate_Number_Request;
end Random_Number_Generator.Messages;