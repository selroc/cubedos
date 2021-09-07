--------------------------------------------------------------------------------
--
-- FILE   : system_bus-messages.adb
-- SUBJECT: Body of a package that implements the main part of the module.
-- AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Telemetry.API;
with Random_Number_Generator.API;
with Read_Number.API;

package body System_Bus.Messages is

   ML    : Message_Loop;
   Count : Positive := 1;

   task body Message_Loop is
      Incoming_Message : Message_Manager.Message_Record;
      Fib_Number       : Natural;
      Call_Sign        : constant String := "S[" & Pri'Image & " ]: ";
   begin
      Initialize;
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

         Ada.Text_IO.Put_Line (Call_Sign & "Loop #" & Count'Image);
         Count := Count + 1;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Error: " & Ada.Exceptions.Exception_Message (E));
         GNAT.OS_Lib.OS_Exit (Status => 1);
   end Message_Loop;

   procedure Initialize is
      Outgoing_Message : Message_Record;
   begin
      Outgoing_Message :=
        System_Bus.API.Telemetry_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
           Priority      => Pri);

      for I in 1 .. 4 loop
         Outgoing_Message.Priority := I;
         Message_Manager.Route_Message (Outgoing_Message);
      end loop;
   end Initialize;

   procedure Process (Message : in Message_Record) is
   begin
      if System_Bus.API.Is_Random_Number_Request (Message) then
         Handle_Random_Number_Request (Message);
      elsif System_Bus.API.Is_Random_Number_Reply (Message) then
         Handle_Random_Number_Reply (Message);
      elsif System_Bus.API.Is_Telemetry_Request (Message) then
         Handle_Telemetry_Request (Message);
      elsif System_Bus.API.Is_Telemetry_Reply (Message) then
         Handle_Telemetry_Reply (Message);
      end if;
   end Process;

   -----------------
   -- Random Number
   -----------------
   procedure Handle_Random_Number_Request (Message : in Message_Record) is
      Status           : Message_Status_Type;
      Outgoing_Message : Message_Record;
   begin
      System_Bus.API.Random_Number_Request_Decode (Message, Status);

      Outgoing_Message :=
        Random_Number_Generator.API.Generate_Number_Request_Encode
          (Sender_Domain => Domain_ID, Sender => ID,
           Priority      => Message.Priority, Request_ID => R_ID);

      Message_Manager.Route_Message (Outgoing_Message);
   end Handle_Random_Number_Request;

   procedure Handle_Random_Number_Reply (Message : in Message_Record) is
      Status           : Message_Status_Type;
      Outgoing_Message : Message_Record;
      Value            : Positive := 42;
   begin
      System_Bus.API.Random_Number_Reply_Decode (Message, Status, Value);

      -- Reply to read
      Outgoing_Message :=
        Read_Number.API.Read_Number_Reply_Encode
          (Receiver_Domain => Domain_ID, Receiver => Read_Number.ID,
           Request_ID      => Read_Number.R_ID, Value => Value);

      Message_Manager.Route_Message (Outgoing_Message);
   end Handle_Random_Number_Reply;

   -------------
   -- Telemetry
   -------------
   procedure Handle_Telemetry_Request (Message : in Message_Record) is
      Status           : Message_Status_Type;
      Outgoing_Message : Message_Record;
   begin
      System_Bus.API.Telemetry_Request_Decode (Message, Status);

      Outgoing_Message :=
        Telemetry.API.Telemetry_Encode
          (Sender_Domain => Domain_ID, Sender => ID, Request_ID => R_ID,
           Priority      => Message.Priority);

      Message_Manager.Route_Message (Outgoing_Message);
   end Handle_Telemetry_Request;

   procedure Handle_Telemetry_Reply (Message : in Message_Record) is
      Status    : Message_Status_Type;
      Call_Sign : constant String := "S[" & Pri'Image & " ]: ";
   begin
      System_Bus.API.Telemetry_Reply_Decode (Message, Status);

      if Status = Malformed then
         Ada.Text_IO.Put_Line
           (Call_Sign & "Error: Message status is Malformed!");
         Ada.Text_IO.New_Line;
         return;
      end if;

      Ada.Text_IO.Put_Line
        (Call_Sign & "Received Telemetry, Priority:" & Message.Priority'Image);
      Ada.Text_IO.New_Line;
   end Handle_Telemetry_Reply;

end System_Bus.Messages;
