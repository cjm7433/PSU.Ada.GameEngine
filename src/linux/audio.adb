with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;
with System;
with System.Address_To_Access_Conversions;

package body Audio is

   use Interfaces;
   use Interfaces.C;
   use Ada.Text_IO;
   use System;

   type pw_main_loop is limited null record;
   type pw_loop      is limited null record;
   type pw_context   is limited null record;
   type pw_stream    is limited null record;

   type pw_main_loop_access is access all pw_main_loop;
   type pw_loop_access      is access all pw_loop;
   type pw_context_access   is access all pw_context;
   type pw_stream_access    is access all pw_stream;

   type spa_chunk is record
      offset : Unsigned_32;
      size   : Unsigned_32;
      stride : Integer;
      flags  : Integer;
   end record;
   pragma Convention (C, spa_chunk);

   type spa_data is record
      type1     : Unsigned_32;
      flags     : Unsigned_32;
      fd        : Integer_64;
      mapoffset : Unsigned_32;
      maxsize   : Unsigned_32;
      data      : System.Address;
      chunk     : access spa_chunk;
   end record;
   pragma Convention (C, spa_data);

   type spa_meta is record
      type1     : Unsigned_32;
      size      : Unsigned_32;
      data      : System.Address;
   end record;
   pragma Convention (C, spa_meta);

   type spa_buffer is record
      n_metas : Unsigned_32;
      n_datas : Unsigned_32;
      metas   : access spa_meta;
      datas   : access spa_data;
   end record;
   pragma Convention (C, spa_buffer);

   type pw_buffer is record
      buffer    : access spa_buffer;
      user_data : System.Address;
      size      : Unsigned_64;
      requested : Unsigned_64;
      time      : Unsigned_64;
   end record;
   pragma Convention (C, pw_buffer);

   type pw_buffer_access is access all pw_buffer;

   type Sample_Array_Unbounded is array (0 .. 1_000_000) of aliased Interfaces.C.short;

   package Sample_Conv is new System.Address_To_Access_Conversions (Sample_Array_Unbounded);

   function pw_init (argc : access Integer; argv : System.Address) return Integer;
   pragma Import (C, pw_init, "pw_init");

   function pw_main_loop_new (props : System.Address) return pw_main_loop_access;
   pragma Import (C, pw_main_loop_new, "pw_main_loop_new");

   function pw_main_loop_get_loop (ml : pw_main_loop_access)
     return pw_loop_access;
   pragma Import (C, pw_main_loop_get_loop, "pw_main_loop_get_loop");

   function pw_context_new
     (ml    : pw_loop_access;
      props : System.Address;
      size  : Integer) return pw_context_access;
   pragma Import (C, pw_context_new, "pw_context_new");

   type Process_Callback is access procedure (userdata : System.Address);
   pragma Convention (C, Process_Callback);

   type pw_stream_events is record
      version        : Unsigned_32;
      destroy        : System.Address := System.Null_Address;
      state_changed  : System.Address := System.Null_Address;
      control_info   : System.Address := System.Null_Address;
      io_changed     : System.Address := System.Null_Address;
      param_changed  : System.Address := System.Null_Address;
      add_buffer     : System.Address := System.Null_Address;
      remove_buffer  : System.Address := System.Null_Address;
      process        : Process_Callback;
      drained        : System.Address := System.Null_Address;
      command        : System.Address := System.Null_Address;
      trigger_done   : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, pw_stream_events);

   function pw_stream_new_simple
      (pw_lp  : pw_loop_access;
      name   : System.Address;
      props  : System.Address;
      events : access pw_stream_events;
      data   : System.Address)
      return pw_stream_access;
   pragma Import (C, pw_stream_new_simple, "pw_stream_new_simple");

   function pw_stream_connect
      (stream    : pw_stream_access;
      direction : Integer;
      target_id : Integer;
      flags     : Integer;
      params    : System.Address;
      n_params  : Integer) return Integer;
   pragma Import (C, pw_stream_connect, "pw_stream_connect");

   function pw_stream_dequeue_buffer (s : pw_stream_access)
     return pw_buffer_access;
   pragma Import (C, pw_stream_dequeue_buffer, "pw_stream_dequeue_buffer");

   function pw_stream_queue_buffer
      (s : pw_stream_access;
      b : pw_buffer_access) return Integer;
   pragma Import (C, pw_stream_queue_buffer, "pw_stream_queue_buffer");

   function pw_main_loop_run (ml : pw_main_loop_access) return Integer;
   pragma Import (C, pw_main_loop_run, "pw_main_loop_run");

   function Build_Audio_Format return System.Address;
   pragma Import (C, Build_Audio_Format, "build_audio_format");

   Main_Loop : pw_main_loop_access := null;
   Context   : pw_context_access   := null;
   Stream    : pw_stream_access    := null;
   Events : aliased pw_stream_events;
   Format : System.Address;
   PW_VERSION_STREAM_EVENTS : constant := 2;
   Max_Sounds : constant Natural := 16;
   Max : constant Natural := 256;

   task Audio_Thread is
      pragma Priority (System.Priority'Last); -- High priority
      entry Start;
   end Audio_Thread;

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   type Byte_Array_Access is access all Byte_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   type Active_Sound is record
      Audio           : Byte_Array_Access;
      Cursor          : Natural := 1;
      Channels        : Natural := 1;
      Bits_Per_Sample : Natural := 16;
      Looping         : Boolean := False;
      Volume          : Float   := 1.0;
   end record;

   type Sound_Request is record
      Audio           : Byte_Array_Access;
      Channels        : Natural;
      Bits_Per_Sample : Natural;
      Looping         : Boolean;
      Volume          : Float;
   end record;

   type Request_Array is array (Natural range 0 .. Max - 1) of Sound_Request;

   protected Sound_Queue is
      procedure Push (S : Sound_Request);
      procedure Pop (S : out Sound_Request; Success : out Boolean);
   private
      Buffer : Request_Array;
      Head   : Natural := 0;
      Tail   : Natural := 0;
      Count  : Natural := 0;
   end Sound_Queue;

   protected body Sound_Queue is

      procedure Push (S : Sound_Request) is
      begin
         if Count < Max then
            Buffer (Tail) := S;
            Tail := (Tail + 1) mod Max;
            Count := Count + 1;
         end if;
      end Push;

      procedure Pop (S : out Sound_Request; Success : out Boolean) is
      begin
         if Count = 0 then
            Success := False;
            return;
         end if;

         S := Buffer (Head);
         Head := (Head + 1) mod Max;
         Count := Count - 1;
         Success := True;
      end Pop;

   end Sound_Queue;

   package Sound_List is new Ada.Containers.Vectors
      (Index_Type   => Natural,
      Element_Type => Active_Sound);

   Active_Sounds : Sound_List.Vector;

   procedure Mix_Sample
   (Left  : out Interfaces.C.short;
   Right : out Interfaces.C.short) is

   Sample_L : Integer := 0;
   Sample_R : Integer := 0;

   begin
      for I in Active_Sounds.First_Index .. Active_Sounds.Last_Index loop
         declare
            Snd : Active_Sound := Active_Sounds (I);
            Frame_Size : constant Natural :=
               Snd.Channels * (Snd.Bits_Per_Sample / 8);
         begin
            if Snd.Audio /= null then

               if Snd.Cursor + Frame_Size - 1 > Snd.Audio'Length then

                  if Snd.Looping then
                     -- Wrap back to start
                     Snd.Cursor := 1;
                  else
                     Active_Sounds.Delete(I);
                     Left  := Interfaces.C.short (Sample_L);
                     Right := Interfaces.C.short (Sample_R);
                     return;
                  end if;
               end if;

               -- Ensure full frame is available
               if Snd.Cursor + Frame_Size - 1 <= Snd.Audio'Length then

                  if Snd.Bits_Per_Sample = 16 then
                     declare
                        L : Integer;
                        R : Integer;
                     begin
                        -- Left channel
                        L :=
                           Integer (Snd.Audio (Snd.Cursor)) +
                           Integer (Snd.Audio (Snd.Cursor + 1)) * 256;

                        if L > 32767 then
                           L := L - 65536;
                        end if;

                        if Snd.Channels = 2 then
                           -- Right channel
                           R :=
                              Integer (Snd.Audio (Snd.Cursor + 2)) +
                              Integer (Snd.Audio (Snd.Cursor + 3)) * 256;

                           if R > 32767 then
                              R := R - 65536;
                           end if;
                        else
                           -- Duplicate mono source
                           R := L;
                        end if;

                        declare
                           V : constant Float := Snd.Volume;
                        begin
                           Sample_L := Sample_L + Integer (Float (L) * V);
                           Sample_R := Sample_R + Integer (Float (R) * V);
                        end;
                     end;
                  end if;

                  -- Advance a frame
                  Snd.Cursor := Snd.Cursor + Frame_Size;

                  Active_Sounds.Replace_Element (I, Snd);
               end if;
            end if;
         end;
      end loop;

   -- Clamp left
   if Sample_L > 32767 then
      Sample_L := 32767;
   elsif Sample_L < -32768 then
      Sample_L := -32768;
   end if;

   -- Clamp right
   if Sample_R > 32767 then
      Sample_R := 32767;
   elsif Sample_R < -32768 then
      Sample_R := -32768;
   end if;

   Left  := Interfaces.C.short (Sample_L);
   Right := Interfaces.C.short (Sample_R);
   end Mix_Sample;

   procedure On_Process (userdata : System.Address);
   pragma Convention (C, On_Process);

   procedure On_Process (userdata : System.Address) is
      Buf     : pw_buffer_access;
      SpaBuf  : access spa_buffer;
      Data_Ptr : access spa_data;
      Dummy   : Integer;
   begin
      Buf := pw_stream_dequeue_buffer (Stream);
      if Buf = null then return; end if;

      SpaBuf := Buf.buffer;
      if SpaBuf = null or else SpaBuf.n_datas = 0 then
         Dummy := pw_stream_queue_buffer (Stream, Buf);
         return;
      end if;

      Data_Ptr := SpaBuf.datas;
      if Data_Ptr = null or else Data_Ptr.data = System.Null_Address then
         Dummy := pw_stream_queue_buffer (Stream, Buf);
         return;
      end if;

      declare
         -- Use the requested frames, but cap it by the buffer's physical limit
         -- (maxsize / 4) because 1 frame = 2 channels * 2 bytes
         Requested_Frames : constant Natural := Natural (Buf.requested);
         Max_Frames       : constant Natural := Natural (Data_Ptr.maxsize / 4);
         Actual_Frames    : constant Natural := (if Requested_Frames > 0 
                                              then Natural'Min (Requested_Frames, Max_Frames)
                                              else 0);

         Samples : Sample_Conv.Object_Pointer := Sample_Conv.To_Pointer (Data_Ptr.data);
      begin
         declare
            Req     : Sound_Request;
            Success : Boolean;
         begin
            loop
               Sound_Queue.Pop (Req, Success);
               exit when not Success;

               Active_Sounds.Append
               (Active_Sound'(
                  Audio           => Req.Audio,
                  Cursor          => 1,
                  Channels        => Req.Channels,
                  Bits_Per_Sample => Req.Bits_Per_Sample,
                  Looping         => Req.Looping,
                  Volume          => Req.Volume
                  ));
            end loop;
         end;

         if Actual_Frames > 0 then
            for Frame in 0 .. Actual_Frames - 1 loop
               declare
                  L, R : Interfaces.C.short;
               begin
                  Mix_Sample (L, R);
                  Samples.all (Frame * 2)     := L;
                  Samples.all (Frame * 2 + 1) := R;
               end;
            end loop;
         end if;
         
         for I in reverse Active_Sounds.First_Index .. Active_Sounds.Last_Index loop
            if Active_Sounds (I).Audio = null then
               Active_Sounds.Delete (I);
            end if;
         end loop;

         -- Update chunk metadata
         Data_Ptr.chunk.offset := 0;
         Data_Ptr.chunk.size   := Unsigned_32 (Actual_Frames * 4);
         Data_Ptr.chunk.stride := 4;
      end;
      
      Dummy := pw_stream_queue_buffer (Stream, Buf);
   exception
      when E : others =>
         Put_Line ("Exception in On_Process: " & Ada.Exceptions.Exception_Information (E));
   end On_Process;

   procedure Initialize is
      Argc  : aliased Integer := 0;
      Dummy : Integer;

      PW_Loop : pw_loop_access;

   begin
      -- pw_init
      Dummy := pw_init (Argc'Access, System.Null_Address);
      if Dummy /= 0 then
         Put_Line ("[Error] pw_init failed with code: " & Integer'Image (Dummy));
         return;
      end if;

      -- main loop
      Main_Loop := pw_main_loop_new (System.Null_Address);
      if Main_Loop = null then
         Put_Line ("[Error] pw_main_loop_new returned null");
         return;
      end if;

      -- get loop
      PW_Loop := pw_main_loop_get_loop (Main_Loop);
      if PW_Loop = null then
         Put_Line ("[Error] pw_main_loop_get_loop returned null");
         return;
      end if;

      -- context
      Context := pw_context_new (PW_Loop, System.Null_Address, 0);
      if Context = null then
         Put_Line ("[Error] pw_context_new returned null");
         return;
      end if;

      -- setup events
      Events.version := PW_VERSION_STREAM_EVENTS;
      Events.process := On_Process'Access;

      -- stream
      Stream := pw_stream_new_simple
         (PW_Loop,
         System.Null_Address,
         System.Null_Address,
         Events'Access,
         System.Null_Address);

      if Stream = null then
         Put_Line ("[Error] pw_stream_new_simple returned null");
         return;
      end if;

      Format := Build_Audio_Format;

      if Format = System.Null_Address then
         Put_Line ("[Error] Failed to build format");
         return;
      end if;

      -- connect stream
      declare
      Params : aliased array (0 .. 0) of System.Address :=
         (0 => Format);
      begin
         Dummy := pw_stream_connect
            (Stream,
            1,
            -1,
            16#000D#,
            Params'Address,
            1);
      end;

      if Dummy < 0 then
         Put_Line ("[Error] pw_stream_connect failed with code: " & Integer'Image (Dummy));
         return;
      end if;

      Audio_Thread.Start;

   end Initialize;

   task body Audio_Thread is
      Dummy : Integer;
   begin
      accept Start;
      if Main_Loop /= null then
         Dummy := pw_main_loop_run (Main_Loop);
      else
         Put_Line ("[Error] Main_Loop is null in audio thread!");
      end if;
   end Audio_Thread;

   procedure Play_Audio (Filename : String; Looping : Boolean; Volume   : Float := 1.0) is
      File : Ada.Streams.Stream_IO.File_Type;
      Str  : access Ada.Streams.Root_Stream_Type'Class;

      type Chunk_Header is record
         ID   : String (1 .. 4);
         Size : Unsigned_32;
      end record;

      type FMT_Header is record
         FormatTag  : Unsigned_16;
         Channels   : Unsigned_16;
         SampleRate : Unsigned_32;
         ByteRate   : Unsigned_32;
         Align      : Unsigned_16;
         Bits       : Unsigned_16;
      end record;

      type RIFF_Header is record
         ID     : String (1 .. 4);
         Size   : Unsigned_32;
         Format : String (1 .. 4);
      end record;

      RIFF  : RIFF_Header;
      Chunk : Chunk_Header;

      FMT_Found  : Boolean := False;
      DATA_Found : Boolean := False;

      FMT   : FMT_Header;
      Audio : Byte_Array_Access := null;

      procedure Skip_Bytes (Count : Natural) is
         use Ada.Streams;
         Dummy : Stream_Element_Array
           (1 .. Stream_Element_Offset (Count));
         Last  : Stream_Element_Offset;
      begin
         if Count > 0 then
            Read (Str.all, Dummy, Last);
         end if;
      end Skip_Bytes;

      function Align2 (V : Natural) return Natural is
      begin
         if (V mod 2) = 1 then
            return V + 1;
         else
            return V;
         end if;
      end Align2;

   begin
      if Natural (Active_Sounds.Length) >= Max_Sounds then
         return;
      end if;

      Ada.Streams.Stream_IO.Open
        (File, Ada.Streams.Stream_IO.In_File, Filename);

      Str := Ada.Streams.Stream_IO.Stream (File);

      RIFF_Header'Read (Str, RIFF);

      if RIFF.ID /= "RIFF" or else RIFF.Format /= "WAVE" then
         raise Program_Error with "Not a WAV file";
      end if;

      while not (FMT_Found and DATA_Found) loop
         Chunk_Header'Read (Str, Chunk);

         if Chunk.ID = "fmt " then
            FMT_Header'Read (Str, FMT);
            FMT_Found := True;

            if Chunk.Size > 16 then
               Skip_Bytes (Align2 (Integer (Chunk.Size) - 16));
            end if;

         elsif Chunk.ID = "data" then
            DATA_Found := True;
            Audio := new Byte_Array (1 .. Integer (Chunk.Size));
            Byte_Array'Read (Str, Audio.all);

         else
            Skip_Bytes (Align2 (Integer (Chunk.Size)));
         end if;
      end loop;

      Ada.Streams.Stream_IO.Close (File);
      
      if FMT.FormatTag /= 1 then
         raise Program_Error with "Only PCM WAV supported";
      end if;

      declare
         Req : Sound_Request;
      begin
         Req.Audio           := Audio;
         Req.Channels        := Natural (FMT.Channels);
         Req.Bits_Per_Sample := Natural (FMT.Bits);
         Req.Looping         := Looping;
         Req.Volume          := Volume;

         Sound_Queue.Push (Req);
      end;

   end Play_Audio;

   procedure Update is null;

   procedure Finalize is null;

end Audio;