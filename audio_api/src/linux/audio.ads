with Audio_Control_Impl;
with Audio_Data_Impl;
with Audio_API;
with Audio_Pipewire;

package Audio is new Audio_API
   (Device => Audio_Pipewire.Device,
    Control => Audio_Control_Impl,
    Data => Audio_Data_Impl,
    Make_Device => Audio_Pipewire.Make_Device,
    Read_ID => Audio_Pipewire.Get_ID);