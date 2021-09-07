--------------------------------------------------------------------------------
--
-- FILE    : SAMPLE_MODULE-api.adb
-- SUBJECT : Body of a package that simplifies use of the module.
-- AUTHOR  : (C) Copyright 2020 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);

with CubedOS.Lib.XDR; use CubedOS.Lib;

package body System_Bus.API is

   -----------------
   -- Random Number
   -----------------
   function Random_Number_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri)
      return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain, Domain_ID, Sender, ID, Request_ID,
           Message_Type'Pos (Random_Number_Request), Priority);
   begin
      -- Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Random_Number_Request_Encode;

   function Random_Number_Reply_Encode
    (Receiver_Domain : Domain_ID_Type; Receiver : Module_ID_Type;
      Request_ID      : Request_ID_Type; Status : Status_Type := Success;
      Priority        : System.Priority := Pri; Value : Positive)
      return Message_Record
   is
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Domain_ID, Receiver_Domain, ID, Receiver, Request_ID,
           Message_Type'Pos (Random_Number_Reply), Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      --  Set a starting position.
      Position := 0;

      --  Encode one parameter (decoding logic must be consistent).
      --  Set Position to get ready for the next parameter.
      XDR.Encode
        (Value    => XDR.XDR_Integer (Value), Data => Message.Payload,
         Position => Position, Last => Last);
      Position := Last + 1;

      --  Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Random_Number_Reply_Encode;

   procedure Random_Number_Request_Decode
     (Message : in Message_Record; Decode_Status : out Message_Status_Type)
   is
   begin
      -- Decode the given message and return via out parameters (not
      --  shown) the fields.
      null;
   end Random_Number_Request_Decode;

   procedure Random_Number_Reply_Decode
     (Message : in     Message_Record; Decode_Status : out Message_Status_Type;
      Value   :    out Positive)
   is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
   begin
      --  Set a starting position.
      Position := 0;
      Value    := 42;

      --  Decode one parameter (encoding logic must be consistent).
      --  Set position to get ready for next parameter.
      XDR.Decode
        (Data => Message.Payload, Position => Position, Value => Raw_Value,
         Last => Last);

      Position := Last + 1;

      --  Convert raw XDR primitive type into appropriate result. Note
      --  runtime check needed!
      if Integer (Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value         := Positive (Raw_Value);
         Decode_Status := Success;
      end if;
   end Random_Number_Reply_Decode;

   -------------
   -- Telemetry
   -------------
   function Telemetry_Request_Encode
     (Sender_Domain : Domain_ID_Type; Sender : Module_ID_Type;
      Request_ID    : Request_ID_Type; Priority : System.Priority := Pri) return Message_Record
   is
      Message : constant Message_Record :=
        Make_Empty_Message
          (Sender_Domain, Domain_ID, Sender, ID, Request_ID,
           Message_Type'Pos (Telemetry_Request), Priority);
   begin
      -- Fill in the message by encoding the other parameters (not
      --  shown) as required.
      return Message;
   end Telemetry_Request_Encode;

   function Telemetry_Reply_Encode
     (Receiver_Domain : Domain_ID_Type; Receiver : Module_ID_Type;
      Request_ID      : Request_ID_Type; Status : Status_Type := Success;
      Priority        : System.Priority := Pri) return Message_Record
   is
      pragma Unreferenced (Status);
      -- The skeletal message knows its sender (this module).
      Message : Message_Record :=
        Make_Empty_Message
          (Domain_ID, Receiver_Domain, ID, Receiver, Request_ID,
           Message_Type'Pos (Telemetry_Reply), Priority);

      Position : Data_Index_Type;
      Last     : Data_Index_Type;
   begin
      --  Set a starting position.
      Position := 0;

      --  Encode one parameter (decoding logic must be consistent).
      --  Set Position to get ready for the next parameter.
      XDR.Encode
        (Value => XDR.XDR_Integer (42),
      -- (Value => XDR.XDR_Unsigned (Status_Type'Pos (Status)),
      Data        => Message.Payload,
         Position => Position, Last => Last);
      Position := Last + 1;

      --  Set the message size.
      Message.Size := Last + 1;
      return Message;
   end Telemetry_Reply_Encode;

   procedure Telemetry_Request_Decode
     (Message : in Message_Record; Decode_Status : out Message_Status_Type)
   is
   begin
      -- Decode the given message and return via out parameters (not
      --  shown) the fields.
      null;
   end Telemetry_Request_Decode;

   procedure Telemetry_Reply_Decode
     (Message : in Message_Record; Decode_Status : out Message_Status_Type)
   is
      Position  : Data_Index_Type;
      Last      : Data_Index_Type;
      Raw_Value : XDR.XDR_Unsigned;
      Value     : Positive; -- Commonly, this would be an out parameter.
      pragma Unreferenced (Value);
   begin
      --  Set a starting position.
      Position := 0;
      Value    := 42;

      --  Decode one parameter (encoding logic must be consistent).
      --  Set position to get ready for next parameter.
      XDR.Decode
        (Data => Message.Payload, Position => Position, Value => Raw_Value,
         Last => Last);

      Position := Last + 1;

      --  Convert raw XDR primitive type into appropriate result. Note
      --  runtime check needed!
      if Integer (Raw_Value) not in Positive then
         Decode_Status := Malformed;
      else
         Value         := Positive (Raw_Value);
         Decode_Status := Success;
      end if;
   end Telemetry_Reply_Decode;

end System_Bus.API;
