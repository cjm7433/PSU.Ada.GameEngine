with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with System;

package body Audio is

   use Interfaces;
   use Interfaces.C;

   type IXAudio2Vtbl;
   type IXAudio2SourceVtbl;

   -- Basic types
   subtype HRESULT is Long;
   S_OK : constant HRESULT := 0;
   subtype UINT32 is Unsigned_32;
   subtype UINT16 is Unsigned_16;
   subtype UINT64 is Unsigned_64;
   subtype BYTE   is Unsigned_8;

   type Padding_Array is array (1 .. 18) of System.Address;
   pragma Convention (C, Padding_Array);

   -- COM
   function CoInitializeEx
     (Reserved : System.Address;
      Flags    : UINT32) return HRESULT;
   pragma Import (Stdcall, CoInitializeEx, "CoInitializeEx");

   procedure CoUninitialize;
   pragma Import (Stdcall, CoUninitialize, "CoUninitialize");

   COINIT_MULTITHREADED : constant UINT32 := 0;

   -- XAudio2 structs
   type WAVEFORMATEX is record
      wFormatTag      : UINT16;
      nChannels       : UINT16;
      nSamplesPerSec  : UINT32;
      nAvgBytesPerSec : UINT32;
      nBlockAlign      : UINT16;
      wBitsPerSample   : UINT16;
      cbSize          : UINT16;
   end record;
   pragma Convention (C, WAVEFORMATEX);

   type XAUDIO2_BUFFER is record
      Flags      : UINT32;
      AudioBytes : UINT32;
      pAudioData : System.Address;
      PlayBegin  : UINT32 := 0;
      PlayLength : UINT32 := 0;
      LoopBegin  : UINT32 := 0;
      LoopLength : UINT32 := 0;
      LoopCount  : UINT32 := 0;
      pContext   : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, XAUDIO2_BUFFER);

   XAUDIO2_END_OF_STREAM : constant UINT32 := 16#0040#;
   XAUDIO2_DEFAULT_PROCESSOR : constant UINT32 := 1;

   type XAUDIO2_VOICE_STATE is record
      pContext      : System.Address;
      BuffersQueued : UINT32;
      SamplesPlayed : UINT64;
   end record;
   pragma Convention (C, XAUDIO2_VOICE_STATE);

   type XAUDIO2_PERFORMANCE_DATA is record
      AudioCyclesSinceLastQuery  : UINT64;
      TotalCyclesSinceLastQuery  : UINT64;
      MinimumCyclesPerQuantum    : UINT32;
      MaximumCyclesPerQuantum    : UINT32;
      MemoryUsageInBytes         : UINT32;
      CurrentLatencyInSamples    : UINT32;
      GlitchesSinceEngineStarted : UINT32;
      ActiveSourceVoiceCount     : UINT32;
      TotalSourceVoiceCount      : UINT32;
      ActiveSubmixVoiceCount     : UINT32;
      ActiveResamplerCount       : UINT32;
      ActiveMatrixMixCount       : UINT32;
      ActiveXmaSourceVoices      : UINT32;
      ActiveXmaStreams           : UINT32;
   end record;
   pragma Convention (C, XAUDIO2_PERFORMANCE_DATA);

   -- COM interface shells
   type IXAudio2 is limited record
      lpVtbl : access IXAudio2Vtbl;
   end record;
   pragma Convention (C, IXAudio2);

   type IXAudio2Voice is limited null record;
   pragma Convention (C, IXAudio2Voice);

   type IXAudio2SourceVoice is limited record
      lpVtbl : access IXAudio2SourceVtbl;
   end record;
   pragma Convention (C, IXAudio2SourceVoice);

   type IXAudio2_Access       is access all IXAudio2;
   type IXAudio2Voice_Access  is access all IXAudio2Voice;
   type IXAudio2Source_Access is access all IXAudio2SourceVoice;

   -- Vtables
   type IXAudio2Vtbl is record
      -- IUnknown
      QueryInterface : System.Address;
      AddRef         : System.Address;
      Release        : access function (This : IXAudio2_Access) return UINT32;

      -- IXAudio2
      RegisterForCallbacks   : System.Address;
      UnregisterForCallbacks : System.Address;

      CreateSourceVoice :
        access function
          (This        : IXAudio2_Access;
           Voice       : out IXAudio2Source_Access;
           Format      : access constant WAVEFORMATEX;
           Flags       : UINT32;
           MaxFreq     : Float;
           Callback    : System.Address;
           SendList    : System.Address;
           EffectChain : System.Address) return HRESULT;

      CreateSubmixVoice : System.Address;

      CreateMasteringVoice :
         access function
            (This          : IXAudio2_Access;
             Voice         : out IXAudio2Voice_Access;
             Channels      : UINT32;
             SampleRate    : UINT32;
             Flags         : UINT32;
             DeviceId      : System.Address;
             EffectChain   : System.Address;
             StreamCategory: UINT32) return HRESULT;

      StartEngine           : System.Address;
      StopEngine            : System.Address;
      CommitChanges         : System.Address;
      GetPerformanceData :
      access procedure
         (This : IXAudio2_Access;
          Data : out XAUDIO2_PERFORMANCE_DATA);
      SetDebugConfiguration : System.Address;
   end record;
   pragma Convention (C, IXAudio2Vtbl);

   type IXAudio2SourceVtbl is record
   -- IXAudio2Voice
   GetVoiceDetails             : System.Address;
   SetOutputVoices             : System.Address;
   SetEffectChain              : System.Address;
   EnableEffect                : System.Address;
   DisableEffect               : System.Address;
   GetEffectState              : System.Address;
   SetEffectParameters         : System.Address;
   GetEffectParameters         : System.Address;
   SetFilterParameters         : System.Address;
   GetFilterParameters         : System.Address;
   SetOutputFilterParameters   : System.Address;
   GetOutputFilterParameters   : System.Address;
   SetVolume                   : System.Address;

   GetVolume :
     access procedure
       (This   : IXAudio2Source_Access;
        Volume : access FLOAT);

   SetChannelVolumes           : System.Address;
   GetChannelVolumes           : System.Address;
   SetOutputMatrix             : System.Address;
   GetOutputMatrix             : System.Address;

   DestroyVoice :
     access procedure
       (This : IXAudio2Source_Access);

   -- IXAudio2SourceVoice
   Start :
     access function
       (This  : IXAudio2Source_Access;
        Flags : UINT32;
        OpSet : UINT32)
       return HRESULT;

   Stop :
     access function
       (This  : IXAudio2Source_Access;
        Flags : UINT32;
        OpSet : UINT32)
       return HRESULT;

   SubmitSourceBuffer :
     access function
       (This   : IXAudio2Source_Access;
        Buffer : access XAUDIO2_BUFFER;
        WMABuf : System.Address)
       return HRESULT;

   FlushSourceBuffers          : System.Address;
   Discontinuity               : System.Address;
   ExitLoop                    : System.Address;

   GetState :
     access procedure
       (This  : IXAudio2Source_Access;
        State : out XAUDIO2_VOICE_STATE;
        Flags : UINT32);

   SetFrequencyRatio :
     access function
       (This  : IXAudio2Source_Access;
        Ratio : FLOAT;
        OpSet : UINT32)
       return HRESULT;

   GetFrequencyRatio :
     access procedure
       (This  : IXAudio2Source_Access;
        Ratio : access FLOAT);

   SetSourceSampleRate         : System.Address;

end record;

pragma Convention (C, IXAudio2SourceVtbl);

   function XAudio2Create
     (Engine : out IXAudio2_Access;
      Flags  : UINT32;
      Proc   : UINT32) return HRESULT;
   pragma Import (Stdcall, XAudio2Create, "XAudio2Create");

   -- Globals
   Engine : IXAudio2_Access := null;
   Master : IXAudio2Voice_Access := null;

   type Byte_Array is array (Positive range <>) of BYTE;
   type Byte_Array_Access is access all Byte_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   type Active_Sound is record
      Voice : IXAudio2Source_Access;
      Audio : Byte_Array_Access;
   end record;

   package Sound_List is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Active_Sound);

   Active_Sounds : Sound_List.Vector;

   procedure Initialize is
      HR : HRESULT;
   begin
      HR := CoInitializeEx (System.Null_Address, COINIT_MULTITHREADED);
      if HR /= S_OK then
         raise Program_Error with "CoInitializeEx failed";
      end if;

      HR := XAudio2Create (Engine, 0, XAUDIO2_DEFAULT_PROCESSOR);
      if HR /= S_OK then
         raise Program_Error with "XAudio2Create failed";
      end if;

      HR := Engine.lpVtbl.CreateMasteringVoice
         (Engine,
          Master,
          0,
          0,
          0,
          System.Null_Address,
          System.Null_Address,
          0);

      if HR /= S_OK then
         raise Program_Error with "CreateMasteringVoice failed";
      end if;

   end Initialize;

   procedure Play_Audio (Filename : String; Looping : Boolean) is

   File : Ada.Streams.Stream_IO.File_Type;
   Str  : access Ada.Streams.Root_Stream_Type'Class;
   Loop_Count : Natural := 0;

   type Chunk_Header is record
      ID   : String (1 .. 4);
      Size : UINT32;
   end record;

   type FMT_Header is record
      FormatTag  : UINT16;
      Channels   : UINT16;
      SampleRate : UINT32;
      ByteRate   : UINT32;
      Align      : UINT16;
      Bits       : UINT16;
   end record;

   type RIFF_Header is record
      ID     : String (1 .. 4);
      Size   : UINT32;
      Format : String (1 .. 4);
   end record;

   RIFF  : RIFF_Header;
   Chunk : Chunk_Header;

   FMT_Found  : Boolean := False;
   DATA_Found : Boolean := False;

   FMT : FMT_Header;
   Audio : Byte_Array_Access := null;

   Voice  : IXAudio2Source_Access;
   Format : aliased WAVEFORMATEX;
   Buffer : aliased XAUDIO2_BUFFER;
   HR     : HRESULT;

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
   Ada.Streams.Stream_IO.Open
     (File, Ada.Streams.Stream_IO.In_File, Filename);

   Str := Ada.Streams.Stream_IO.Stream (File);

   RIFF_Header'Read (Str, RIFF);

   if RIFF.ID /= "RIFF" or else RIFF.Format /= "WAVE" then
      raise Program_Error with "Not a WAV file";
   end if;

   if Looping then
      Loop_Count  := 255;
   end if;

   -- Scan chunks
   while not (FMT_Found and DATA_Found) loop

      Chunk_Header'Read (Str, Chunk);

      if Chunk.ID = "fmt " then

         FMT_Header'Read
         (Str,
          FMT);
         FMT_Found := True;

         if Chunk.Size > 16 then
            Skip_Bytes (Align2 (Integer (Chunk.Size) - 16));
         end if;

      elsif Chunk.ID = "data" then

         DATA_Found := True;
         Audio := new Byte_Array (1 .. Integer (Chunk.Size));

         Byte_Array'Read (Str, Audio.all);

      else
         -- Skip unknown chunk
         Skip_Bytes (Align2 (Integer (Chunk.Size)));
      end if;

   end loop;

   Ada.Streams.Stream_IO.Close (File);

   if FMT.FormatTag /= 1 then
      raise Program_Error with "Only PCM WAV supported";
   end if;

   Format :=
     (wFormatTag      => FMT.FormatTag,
      nChannels       => FMT.Channels,
      nSamplesPerSec  => FMT.SampleRate,
      nAvgBytesPerSec => FMT.ByteRate,
      nBlockAlign     => FMT.Align,
      wBitsPerSample  => FMT.Bits,
      cbSize          => 0);

   HR := Engine.lpVtbl.CreateSourceVoice
     (Engine,
      Voice,
      Format'Access,
      0,
      1.0,
      System.Null_Address,
      System.Null_Address,
      System.Null_Address);

   if HR /= S_OK then
      raise Program_Error with "CreateSourceVoice failed";
   end if;

   Buffer :=
     (Flags      => XAUDIO2_END_OF_STREAM,
      AudioBytes => UINT32 (Audio'Length),
      pAudioData => Audio.all'Address,
      LoopCount  => UINT32 (Loop_Count),
      others     => <>);

   HR := Voice.lpVtbl.SubmitSourceBuffer
     (Voice, Buffer'Access, System.Null_Address);

   HR := Voice.lpVtbl.Start (Voice, 0, 0);

   Active_Sounds.Append
     (Active_Sound'
        (Voice => Voice,
         Audio => Audio));

end Play_Audio;

   procedure Update is
      State : XAUDIO2_VOICE_STATE;
   begin
      for I in reverse Active_Sounds.First_Index ..
                       Active_Sounds.Last_Index loop
         Active_Sounds (I).Voice.lpVtbl.GetState
           (Active_Sounds (I).Voice, State, 0);

         if State.BuffersQueued = 0 then
            Active_Sounds (I).Voice.lpVtbl.DestroyVoice
              (Active_Sounds (I).Voice);

            Free (Active_Sounds (I).Audio);
            Active_Sounds.Delete (I);
         end if;
      end loop;
   end Update;

   procedure Finalize is
      Dummy : UINT32;
   begin
      if Engine /= null then
         Dummy := Engine.lpVtbl.Release (Engine);
      end if;

      CoUninitialize;
   end Finalize;

end Audio;