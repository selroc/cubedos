------------------------------------------------------------------------------
--
--  FILE   : random_number_generator-api.ads
--  SUBJECT: Specification of a package that simplifies use of the module.
--  AUTHOR : (C) Copyright 2020 by Vermont Technical College
--
--  All the subprograms in this package must be task safe. They can be
--  simultaneously called from multiple tasks.  If possible, make
--  every subprogram here a pure function.
--
------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with Message_Manager; use Message_Manager;
with System;

package Random_Number_Generator.API is

   type Status_Type is (Success, Failure);
   type Message_Type is (Generate_Number_Request);

   function Generate_Number_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record with
      Global => null;

   function Is_Generate_Number_Request
     (Message : Message_Record) return Boolean is
     (Message.Receiver = ID and
      Message.Message_ID = Message_Type'Pos (Generate_Number_Request));

   procedure Generate_Number_Request_Decode
     (Message : in     Message_Record; Decode_Status : out Message_Status_Type;
      Value   :    out Positive) with
      Global  => null,
      Pre     => Is_Generate_Number_Request (Message),
      Depends => (Decode_Status => Message, Value => Message);

end Random_Number_Generator.API;
