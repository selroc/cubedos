--------------------------------------------------------------------------------
-- FILE   : cubedos-generic_message_manager.adb
-- SUBJECT: Body of a package for message passing in CubedOS.
-- AUTHOR : (C) Copyright 2017 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with Ada.Text_IO;

package body CubedOS.Generic_Message_Manager with
   Refined_State => (Mailboxes => Message_Storage,
    Request_ID_Generator => Request_ID_Gen)
is
   -- A protected object for generating request ID values.
   protected Request_ID_Gen is
      procedure Generate_Next_ID (Request_ID : out Request_ID_Type);
   private
      Next_Request_ID : Request_ID_Type := 1;
   end Request_ID_Gen;

   -- A protected type for holding messages.
   protected type Mailbox is

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the returned status indicates this.
      procedure Send (Message : in Message_Record; Status : out Status_Type);

      -- Send the indicated message. This procedure returns at once without waiting for the
      -- message to be received. If the mailbox is full the message is lost.
      procedure Unchecked_Send (Message : in Message_Record);

      -- Returns the number of messages in the mailbox.
      function Message_Count return Message_Count_Type;

      -- Receive a message. This entry waits indefinitely for a message to be available.
      entry Receive (Message : out Message_Record);

   private
      Messages        : Message_Array;
      Count           : Message_Count_Type := 0;
      Next_In         : Message_Index_Type := 1;
      Next_Out        : Message_Index_Type := 1;
      Message_Waiting : Boolean            := False;

      procedure Sort_Mail;
      procedure Insertion_Sort;
      procedure Exchange (I, J : Message_Index_Type);
      procedure Print_Mailbox;
      procedure Replace_Last_Message(Message : in Message_Record);
      function Calc_Next (Next : Message_Index_Type) return Message_Index_Type;
   end Mailbox;

   -- One mailbox for each module.
   Message_Storage : array (Module_ID_Type) of Mailbox;

   ------------------
   -- Implementations
   ------------------

   protected body Request_ID_Gen is

      procedure Generate_Next_ID (Request_ID : out Request_ID_Type) is
      begin
         Request_ID      := Next_Request_ID;
         Next_Request_ID := Next_Request_ID + 1;
      end Generate_Next_ID;

   end Request_ID_Gen;

   protected body Mailbox is
      --------
      -- Send
      --------
      procedure Send (Message : in Message_Record; Status : out Status_Type) is
      begin
         if Count = Mailbox_Size then
            Status := Mailbox_Full;
            Replace_Last_Message(Message);
         else
            Unchecked_Send (Message);
            Status := Accepted;
         end if;
      end Send;

      procedure Unchecked_Send (Message : in Message_Record) is
      begin
         if Count /= Mailbox_Size then
            Messages (Next_In) := Message;

            Next_In := Calc_Next (Next_In);

            Count           := Count + 1;
            Message_Waiting := True;
         else
            Replace_Last_Message(Message);
         end if;
      end Unchecked_Send;

      procedure Replace_Last_Message (Message : in Message_Record) is
      begin
         Sort_Mail;
         if Messages (Messages'Last).Priority < Message.Priority then
            Messages(Messages'Last) := Message;
         end if;
         Sort_Mail;
      end Replace_Last_Message;

      function Calc_Next (Next : Message_Index_Type) return Message_Index_Type
      is
      begin
         if Next = Mailbox_Size then
            return 1;
         else
            return Next + 1;
         end if;
      end Calc_Next;

      -----------
      -- Receive
      -----------
      entry Receive (Message : out Message_Record) when Message_Waiting is
      begin
         Sort_Mail;
         Print_Mailbox;
         Message := Messages (Next_Out); -- Make copy

         Messages (Next_Out).Priority := System.Priority'First;

         Next_Out := Calc_Next (Next_Out);

         Count := Count - 1;
         if Count = 0 then
            Message_Waiting := False;
         end if;
      end Receive;

      procedure Sort_Mail is
      begin
         Insertion_Sort;
         Next_Out := Message_Index_Type'First;
         Next_In  := Calc_Next (Count);
      end Sort_Mail;

      procedure Insertion_Sort is
      begin
         for I in Messages'Range loop
            for J in I .. Messages'Last loop
               if Messages (I).Priority < Messages (J).Priority then
                  Exchange (I, J);
               end if;
            end loop;
         end loop;
      end Insertion_Sort;

      procedure Exchange (I, J : Message_Index_Type) is
         Temp : Message_Record;
      begin
         Temp         := Messages (J);
         Messages (J) := Messages (I);
         Messages (I) := Temp;
      end Exchange;

      procedure Print_Mailbox is
      begin
         for I in Messages'Range loop
            Ada.Text_IO.Put ("Slot" & I'Image & ": ");
            Ada.Text_IO.Put ("(Pri)" & Messages (I).Priority'Image);
            Ada.Text_IO.Put (", (ID)" & Messages (I).Message_ID'Image);
            Ada.Text_IO.Put (", (Sender)" & Messages (I).Sender'Image);
            Ada.Text_IO.Put (", (Receiver)" & Messages (I).Receiver'Image);
            Ada.Text_IO.New_Line;
         end loop;
         Ada.Text_IO.Put_Line
           ("Message Count:" & Message_Count'Image & " /" &
            Mailbox_Size'Image);
         Ada.Text_IO.Put_Line (Next_Out'Image & " is the current slot");
      end Print_Mailbox;

      function Message_Count return Message_Count_Type is
      begin
         return Count;
      end Message_Count;
   end Mailbox;

   function Make_Empty_Message
     (Sender_Domain : Domain_ID_Type; Receiver_Domain : Domain_ID_Type;
      Sender        : Module_ID_Type; Receiver : Module_ID_Type;
      Request_ID    : Request_ID_Type; Message_ID : Message_ID_Type;
      Priority      : System.Priority := System.Priority'First)
      return Message_Record
   is
      Message : Message_Record;
   begin
      Message.Sender_Domain   := Sender_Domain;
      Message.Receiver_Domain := Receiver_Domain;
      Message.Sender          := Sender;
      Message.Receiver        := Receiver;
      Message.Request_ID      := Request_ID;
      Message.Message_ID      := Message_ID;
      Message.Priority        := Priority;
      Message.Size            := 0;
      Message.Payload         := (others => 0);
      return Message;
   end Make_Empty_Message;

   procedure Get_Next_Request_ID (Request_ID : out Request_ID_Type) is
   begin
      Request_ID_Gen.Generate_Next_ID (Request_ID);
   end Get_Next_Request_ID;

   procedure Route_Message
     (Message : in Message_Record; Status : out Status_Type)
   is
   begin
-- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage (Message.Receiver).Send (Message, Status);
   end Route_Message;

   procedure Route_Message (Message : in Message_Record) is
   begin
-- For now, let's ignore the domain and just use the receiver Module_ID only.
      Message_Storage (Message.Receiver).Unchecked_Send (Message);
   end Route_Message;

   procedure Fetch_Message
     (Module : in Module_ID_Type; Message : out Message_Record)
   is
   begin
      Message_Storage (Module).Receive (Message);
   end Fetch_Message;

   procedure Get_Message_Counts (Count_Array : out Message_Count_Array) is
   begin
      for I in Module_ID_Type loop
         Count_Array (I) := Message_Storage (I).Message_Count;
      end loop;
   end Get_Message_Counts;

end CubedOS.Generic_Message_Manager;
