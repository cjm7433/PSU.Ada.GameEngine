with Audio_Control_Impl;
with Audio_Data_Impl;
with Audio_API;
with Audio_Wasapi;

package Audio is new Audio_API
   (Device => Audio_Wasapi.Device,
    Control => Audio_Control_Impl,
    Data => Audio_Data_Impl,
    Make_Device => Audio_Wasapi.Make_Device,
    Read_ID => Audio_Wasapi.Get_ID);