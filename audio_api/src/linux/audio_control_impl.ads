with Audio_Control;
with Audio_Pipewire;

package Audio_Control_Impl is new Audio_Control
      (Device => Audio_Pipewire.Device, 
       Low_Level_Open => Audio_Pipewire.Low_Level_Open,
       Low_Level_Close => Audio_Pipewire.Low_Level_Close
      );