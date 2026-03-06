with Audio_Data;
with Audio_Wasapi;

package Audio_Data_Impl is new Audio_Data 
      (Device => Audio_Wasapi.Device, 
       Low_Level_Write => Audio_Wasapi.Low_Level_Write
      );