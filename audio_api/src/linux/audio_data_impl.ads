with Audio_Data;
with Audio_Pipewire;

package Audio_Data_Impl is new Audio_Data 
      (Device => Audio_Pipewire.Device, 
       Low_Level_Write => Audio_Pipewire.Low_Level_Write
      );