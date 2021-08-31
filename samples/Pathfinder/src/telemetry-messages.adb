--------------------------------------------------------------------------------
--
--  FILE   : Telemetry-messages.adb
--  SUBJECT: Body of a package that implements the main part of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Fibonacci;
with System_Bus.API;

package body Telemetry.Messages is
   ML : Message_Loop;

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "M[" & Pri'Image & " ]: ";
   begin
      loop
         Ada.Text_IO.Put_Line
           (Call_Sign & "Wasting time generating a Fibonacci number...");
         Fib_Number := Fibonacci.Gen_Slowest (FSeed);

         Ada.Text_IO.Put (Call_Sign & "Fibonacci(" & FSeed'Image & " ) is");
         Ada.Text_IO.Put_Line (Fib_Number'Image);

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           (Call_Sign & "Getting next message to process...");

         Message_Manager.Fetch_Message (ID, Incoming_Message);
         Process (Incoming_Message);

         Ada.Text_IO.Put_Line (Call_Sign & "Processed message.");
         Ada.Text_IO.New_Line;
      end loop;
   end Message_Loop;

   procedure Process (Message : in Message_Record) is
   begin
      if Telemetry.API.Is_Telemetry_Request (Message) then
         Handle_Telemetry (Message);
      end if;
   end Process;

   procedure Handle_Telemetry (Message : in Message_Record) is
      Outgoing_Message : Message_Record;
      Status           : Message_Status_Type;
      Call_Sign : constant String := "M[" & Pri'Image & " ] -- Telemetry: ";
   begin
      Telemetry.API.Telemetry_Decode (Message, Status);
      --  Act on the request message.

      if Status = Malformed then
         Ada.Text_IO.Put_Line
           (Call_Sign & "Error: Message status is Malformed!");
         Ada.Text_IO.New_Line;
         return;
      end if;

      Ada.Text_IO.Put_Line
        (Call_Sign & "Processed message, sending reply to SysBus");
      Ada.Text_IO.New_Line;

      Outgoing_Message :=
        System_Bus.API.Telemetry_Reply_Encode
          (Receiver_Domain => Domain_ID, Receiver => System_Bus.ID,
           Request_ID      => System_Bus.R_ID, Priority => Message.Priority);

      Message_Manager.Route_Message (Outgoing_Message);

      -- Creating request to cause loop.
      Outgoing_Message :=
        System_Bus.API.Telemetry_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
           Priority      => Pri);
      for I in 1 .. 8 loop
         Message_Manager.Route_Message (Outgoing_Message);
      end loop;

   end Handle_Telemetry;
end Telemetry.Messages;
