with Audio_Control;
with Audio_Wasapi;

package Audio_Control_Impl is new Audio_Control
      (Device => Audio_Wasapi.Device, 
       Low_Level_Open => Audio_Wasapi.Low_Level_Open,
       Low_Level_Close => Audio_Wasapi.Low_Level_Close
      );