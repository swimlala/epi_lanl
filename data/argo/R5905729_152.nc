CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-06-25T09:01:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ܐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20220625090112  20220625090112  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @��V&�
1   @��V�l" @(P�`A�7�d~^5?|�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�33B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx  B��B���B���B���B���B���B���B���B���B���B���B�fgB�fgB���B���B���B�  B���BǙ�B���B���B���B�  B���B���B���B���B���B�  B���B�fgB���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC8  C9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�4Dy�D��Dy�D��Dy�D��D	y�D
  D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DX� DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Drs4Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dy  Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A��mA��yA��A��A��A��mA���A׾wA�~�A�n�A�l�A�dZA�dZA�ffA�dZA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�^5A�\)A�^5A�\)A�S�A�K�A�C�A�7LA�1'A�-A�-A�1'A�1'A�/A�(�A�{A��A�/A���AΛ�A�ffA�A���A�`BA��A�bA�%A�5?A���A�VA�$�A���A�oA��A�ƨA��9A�hsA��A��/A��+A�n�AS�AzAtv�Am�
Ad1'Aa&�A\��AX�RAN�/AKl�AK"�AJ��AI�AHĜAH�+AHQ�AEƨAC�FAB�/AA��A@{A?��A?A@ȴA@�\A@jA>ĜA<�DA;C�A:�RA:1A9?}A7`BA5�A3�A0  A.��A.I�A-�^A+�A(�A'��A'7LA&bA%��A%��A$��A#�#A#oA#oA#�A"��A"�A!\)A �DA �\A �9A ��A �A   A;dA1'A��A�AdZA\)AS�AC�A;dA;dA"�A��A��Az�AE�A|�A��A��A�DAJA+A��Ar�A��A  AAE�A�DA�DAn�AI�A�A�A��A��A|�A��A=qAdZAXAS�AC�A%A�RAv�A��A�FA��AoAQ�A��A/A%A�A��Av�AQ�AE�A5?A{A�A��AO�A�+A{A��A\)AoA��AjA1'A�AƨA�PAx�Al�A`BAG�A�A
�!A
A�A	�A	��A	�PA	33A��A�DA{A��A�Ax�AG�Av�AA�TA��AA�hAp�A\)A
=A�A�wA�AdZA/A�A�AoA��A��A��A1'AA  A  A��A��AA�A�TAA��A��A�PA?}@���@�{@�@��@�/@��`@���@�Z@��@��@��R@�n�@�@�hs@�  @�@��@�n�@�`B@��@�Ĝ@�D@�I�@�9X@���@�+@�p�@�Ĝ@�j@�w@���@�~�@�5?@��-@�`B@�%@�u@��m@�dZ@��@���@�~�@���@�G�@�9@���@�+@�ff@�=q@��@��#@噚@�@�@��H@�G�@��/@��u@߮@�dZ@�+@��@�~�@ݺ^@�hs@�/@ۮ@�O�@���@���@�K�@�o@ָR@�@�p�@�&�@��@�r�@Ӿw@�t�@�V@���@�ƨ@�
=@Η�@���@Ͳ-@��@�  @˝�@�\)@��@�V@Ɂ@�%@���@�z�@��@ǝ�@�\)@�C�@�+@�~�@���@ũ�@�`B@��`@�(�@��@öF@�t�@�+@�ȴ@�V@��^@�/@�Ĝ@�Z@��@�dZ@�v�@���@��^@�p�@�7L@���@�Ĝ@�j@�Q�@� �@��@���@�dZ@�"�@���@��y@���@�V@�@�@���@�p�@�7L@��j@�(�@���@��w@��P@�33@���@�n�@�-@��h@���@� �@���@�C�@��@���@��!@��+@�E�@�J@�@�7L@���@�1'@��m@��w@��@�\)@�+@��H@���@�$�@�%@��@�r�@�9X@��@�C�@��H@�^5@��-@�Z@��m@��P@�"�@��@��H@��\@�$�@���@���@��@��D@�1'@� �@� �@��F@���@�~�@��@��h@��@�(�@��@�1@��@��@��;@��
@��;@���@��;@��@��F@��F@�t�@�"�@���@�~�@�@���@��^@�`B@�7L@���@�bN@���@��;@���@�ƨ@���@�\)@�n�@�-@�J@���@�hs@�(�@�S�@�ȴ@�M�@���@��@�O�@�%@��j@��@��m@���@���@�|�@�l�@�l�@�33@�v�@���@�`B@��D@�j@�bN@�1'@� �@��@��m@�t�@�"�@���@���@�ff@�5?@���@�O�@�%@���@�Ĝ@�r�@��;@�ƨ@��w@�l�@�33@�o@��@���@�-@��@���@�?}@���@��j@�r�@�(�@�b@��;@��w@�\)@�@��H@���@���@�n�@�-@���@���@��h@�p�@�`B@�/@��`@��@�r�@�bN@�Z@�I�@�b@�ƨ@�l�@�C�@�33@��@�@���@�M�@��@��@��h@�&�@��`@��9@�9X@�A�@�Z@�9X@l�@~�+@~V@}�-@}p�@|�@|��@|�@{�
@{C�@z�@y��@yG�@x��@xbN@x1'@x  @w��@w�P@wl�@w+@vE�@u��@t�@t�D@s�m@sS�@s"�@r~�@q�@qx�@q%@p�9@pbN@p �@o��@ol�@n��@m��@lj@k��@k33@k"�@j�H@jM�@i�^@i�@hĜ@h�u@hr�@h �@g�@g�w@g��@g+@fȴ@fff@f{@e�h@e/@d�@d�@d9X@c�m@cS�@b�!@bM�@a��@`�u@`Q�@`Q�@`Q�@`Q�@`A�@`A�@`b@_�@^��@]��@]p�@]p�@]p�@]?}@\��@[�F@Z��@Z�\@ZJ@Y�^@Y�7@YG�@Y�@Y%@X�`@X�9@X�@X �@W�;@W|�@WK�@W
=@V��@V�y@V�R@V5?@U�-@U/@T�D@TZ@T9X@T1@Sƨ@S�F@S�@SC�@S"�@R�@R��@R��@RJ@Qhs@P��@Pr�@PA�@Pb@O�@OK�@O;d@O�@N�y@N��@M�@M�@L�@L�/@L�j@L�D@LI�@K��@KdZ@Ko@J�@J��@Jn�@J=q@I��@I�^@Ihs@I7L@I�@H�9@Hr�@G�;@G�P@GK�@G+@F��@F�R@Fff@F5?@F{@E�@E�T@E@E`B@E�@D�/@D�D@D9X@C�m@C��@CC�@C"�@C@B��@Bn�@B�@A�@A�#@A�#@A�^@A��@A�7@A&�@@ �@?�P@?|�@?|�@?K�@?
=@>�@>��@>ff@=��@<��@<j@<Z@<I�@<(�@;ƨ@;�@;C�@;"�@;o@;o@;o@;o@;@:�!@:n�@:-@9��@9G�@9�@8��@8��@8��@8�@81'@7�;@7��@7\)@7\)@7+@6�@6�R@6��@6v�@6ff@6$�@5��@5�@5`B@5?}@5/@4�@4z�@4(�@3��@3�@3C�@3@2�\@1�@1��@1�7@17L@0Ĝ@0A�@0 �@0  @/�@/�P@/;d@.��@.��@.5?@-�@-��@-�h@-O�@-/@-V@,��@,�D@,�D@,�D@,Z@,(�@+�F@+t�@*�@*^5@)�#@)�7@)7L@(��@(��@(�u@( �@(  @'�@'��@'��@'|�@'l�@'\)@'�@&�@&��@&E�@%@%�@$�@$��@$��@$�D@$9X@$�@#��@#�
@#ƨ@#�F@#33@"�!@"n�@"-@!�@!x�@!7L@!%@ �`@ �9@ �@ A�@�@
=@ȴ@ȴ@��@V@�@`B@�/@�j@�@�@�@�D@�m@t�@C�@��@M�@M�@=q@J@��@G�@Ĝ@r�@bN@bN@Q�@b@�w@�@�P@l�@;d@�@��@��@v�@E�@�T@��@�h@?}@�/@��@�@ƨ@�F@�@dZ@C�@o@�!@�\@^5@M�@-@J@�@��@��@x�@G�@%@��@bN@Q�@1'@�;@�P@\)@+@ȴ@ff@V@E�@$�@$�@{@�T@�h@p�@O�@�@��@�@�j@Z@��@�m@�
1111111111111111111111111111111111111111111111111111111441441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��mA��yA��A��A��A��mA���A׾wA�~�A�n�A�l�A�dZA�dZA�ffA�dZA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�^5A�\)A�^5A�\)A�S�A�K�A�C�A�7LA�1'A�-A�-A�1'A�1'A�/A�(�A�{A��A�/A���AΛ�A�ffA�A���A�`BA��A�bA�%A�5?A���A�VA�$�A���A�oA��A�ƨA��9A�hsA��A��/A��+A�n�AS�AzAtv�Am�
Ad1'Aa&�A\��AX�RAN�/AKl�AK"�AJ��AI�AHĜAH�+AHQ�AEƨAC�FAB�/AA��A@{A?��A?A@ȴA@�\A@jA>ĜA<�DA;C�A:�RA:1A9?}A7`BA5�A3�A0  A.��A.I�A-�^A+�A(�A'��A'7LA&bA%��A%��A$��A#�#A#oA#oA#�A"��A"�A!\)A �DA �\A �9A ��A �A   A;dA1'A��A�AdZA\)AS�AC�A;dA;dA"�A��A��Az�AE�A|�A��A��A�DAJA+A��Ar�A��A  AAE�A�DA�DAn�AI�A�A�A��A��A|�A��A=qAdZAXAS�AC�A%A�RAv�A��A�FA��AoAQ�A��A/A%A�A��Av�AQ�AE�A5?A{A�A��AO�A�+A{A��A\)AoA��AjA1'A�AƨA�PAx�Al�A`BAG�A�A
�!A
A�A	�A	��A	�PA	33A��A�DA{A��A�Ax�AG�Av�AA�TA��AA�hAp�A\)A
=A�A�wA�AdZA/A�A�AoA��A��A��A1'AA  A  A��A��AA�A�TAA��A��A�PA?}@���@�{@�@��@�/@��`@���@�Z@��@��@��R@�n�@�@�hs@�  @�@��@�n�@�`B@��@�Ĝ@�D@�I�@�9X@���@�+@�p�@�Ĝ@�j@�w@���@�~�@�5?@��-@�`B@�%@�u@��m@�dZ@��@���@�~�@���@�G�@�9@���@�+@�ff@�=q@��@��#@噚@�@�@��H@�G�@��/@��u@߮@�dZ@�+@��@�~�@ݺ^@�hs@�/@ۮ@�O�@���@���@�K�@�o@ָR@�@�p�@�&�@��@�r�@Ӿw@�t�@�V@���@�ƨ@�
=@Η�@���@Ͳ-@��@�  @˝�@�\)@��@�V@Ɂ@�%@���@�z�@��@ǝ�@�\)@�C�@�+@�~�@���@ũ�@�`B@��`@�(�@��@öF@�t�@�+@�ȴ@�V@��^@�/@�Ĝ@�Z@��@�dZ@�v�@���@��^@�p�@�7L@���@�Ĝ@�j@�Q�@� �@��@���@�dZ@�"�@���@��y@���@�V@�@�@���@�p�@�7L@��j@�(�@���@��w@��P@�33@���@�n�@�-@��h@���@� �@���@�C�@��@���@��!@��+@�E�@�J@�@�7L@���@�1'@��m@��w@��@�\)@�+@��H@���@�$�@�%@��@�r�@�9X@��@�C�@��H@�^5@��-@�Z@��m@��P@�"�@��@��H@��\@�$�@���@���@��@��D@�1'@� �@� �@��F@���@�~�@��@��h@��@�(�@��@�1@��@��@��;@��
@��;@���@��;@��@��F@��F@�t�@�"�@���@�~�@�@���@��^@�`B@�7L@���@�bN@���@��;@���@�ƨ@���@�\)@�n�@�-@�J@���@�hs@�(�@�S�@�ȴ@�M�@���@��@�O�@�%@��j@��@��m@���@���@�|�@�l�@�l�@�33@�v�@���@�`B@��D@�j@�bN@�1'@� �@��@��m@�t�@�"�@���@���@�ff@�5?@���@�O�@�%@���@�Ĝ@�r�@��;@�ƨ@��w@�l�@�33@�o@��@���@�-@��@���@�?}@���@��j@�r�@�(�@�b@��;@��w@�\)@�@��H@���@���@�n�@�-@���@���@��h@�p�@�`B@�/@��`@��@�r�@�bN@�Z@�I�@�b@�ƨ@�l�@�C�@�33@��@�@���@�M�@��@��@��h@�&�@��`@��9@�9X@�A�@�Z@�9X@l�@~�+@~V@}�-@}p�@|�@|��@|�@{�
@{C�@z�@y��@yG�@x��@xbN@x1'@x  @w��@w�P@wl�@w+@vE�@u��@t�@t�D@s�m@sS�@s"�@r~�@q�@qx�@q%@p�9@pbN@p �@o��@ol�@n��@m��@lj@k��@k33@k"�@j�H@jM�@i�^@i�@hĜ@h�u@hr�@h �@g�@g�w@g��@g+@fȴ@fff@f{@e�h@e/@d�@d�@d9X@c�m@cS�@b�!@bM�@a��@`�u@`Q�@`Q�@`Q�@`Q�@`A�@`A�@`b@_�@^��@]��@]p�@]p�@]p�@]?}@\��@[�F@Z��@Z�\@ZJ@Y�^@Y�7@YG�@Y�@Y%@X�`@X�9@X�@X �@W�;@W|�@WK�@W
=@V��@V�y@V�R@V5?@U�-@U/@T�D@TZ@T9X@T1@Sƨ@S�F@S�@SC�@S"�@R�@R��@R��@RJ@Qhs@P��@Pr�@PA�@Pb@O�@OK�@O;d@O�@N�y@N��@M�@M�@L�@L�/@L�j@L�D@LI�@K��@KdZ@Ko@J�@J��@Jn�@J=q@I��@I�^@Ihs@I7L@I�@H�9@Hr�@G�;@G�P@GK�@G+@F��@F�R@Fff@F5?@F{@E�@E�T@E@E`B@E�@D�/@D�D@D9X@C�m@C��@CC�@C"�@C@B��@Bn�@B�@A�@A�#@A�#@A�^@A��@A�7@A&�@@ �@?�P@?|�@?|�@?K�@?
=@>�@>��@>ff@=��@<��@<j@<Z@<I�@<(�@;ƨ@;�@;C�@;"�@;o@;o@;o@;o@;@:�!@:n�@:-@9��@9G�@9�@8��@8��@8��@8�@81'@7�;@7��@7\)@7\)@7+@6�@6�R@6��@6v�@6ff@6$�@5��@5�@5`B@5?}@5/@4�@4z�@4(�@3��@3�@3C�@3@2�\@1�@1��@1�7@17L@0Ĝ@0A�@0 �@0  @/�@/�P@/;d@.��@.��@.5?@-�@-��@-�h@-O�@-/@-V@,��@,�D@,�D@,�D@,Z@,(�@+�F@+t�@*�@*^5@)�#@)�7@)7L@(��@(��@(�u@( �@(  @'�@'��@'��@'|�@'l�@'\)@'�@&�@&��@&E�@%@%�@$�@$��@$��@$�D@$9X@$�@#��@#�
@#ƨ@#�F@#33@"�!@"n�@"-@!�@!x�@!7L@!%@ �`@ �9@ �@ A�@�@
=@ȴ@ȴ@��@V@�@`B@�/@�j@�@�@�@�D@�m@t�@C�@��@M�@M�@=q@J@��@G�@Ĝ@r�@bN@bN@Q�@b@�w@�@�P@l�@;d@�@��@��@v�@E�@�T@��@�h@?}@�/@��@�@ƨ@�F@�@dZ@C�@o@�!@�\@^5@M�@-@J@�@��@��@x�@G�@%@��@bN@Q�@1'@�;@�P@\)@+@ȴ@ff@V@E�@$�@$�@{@�T@�h@p�@O�@�@��@�@�j@Z@��@�m@�
1111111111111111111111111111111111111111111111111111111441441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
$�B
"�B
$�B
$�B
#�B
#�B
"�B
 �B
!�B
�B
)�B
,B
,B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
/B
0!B
/B
.B
-B
,B
,B
,B
-B
.B
/B
1'B
2-B
33B
49B
9XB
9XB
(�B
1'B
;dB
#�B
$�B
�=B
��B
B
~�B
�RB
�TB �B�BVB
�BB
~�B
��B
�uB	�'B	�B
(�B	�B	��B	�+B	bNB	(�B	  BĜB	hB�`B�RBl�B�#B	0!B	)�B	�B	{B	#�B	�B��B	B	%�B	+B	'�B	J�B	]/B	�B	��B	��B	�B	w�B	��B	��B	��B	�\B	y�B	bNB	w�B	aHB	��B	�B	��B	�bB	s�B	��B	��B	ȴB	�;B	�B	�NB	�yB	��B
{B
{B
VB
B
JB
�B
)�B
;dB
D�B
@�B
=qB
>wB
>wB
O�B
ZB
\)B
^5B
]/B
]/B
\)B
[#B
W
B
R�B
N�B
O�B
K�B
C�B
M�B
VB
Q�B
K�B
A�B
R�B
N�B
J�B
n�B
p�B
u�B
�B
~�B
{�B
y�B
w�B
w�B
u�B
r�B
m�B
cTB
]/B
`BB
v�B
v�B
s�B
n�B
k�B
l�B
iyB
iyB
k�B
_;B
[#B
^5B
dZB
k�B
l�B
jB
hsB
k�B
m�B
jB
gmB
dZB
^5B
XB
O�B
Q�B
T�B
VB
W
B
YB
XB
]/B
\)B
^5B
^5B
_;B
`BB
]/B
ZB
VB
O�B
O�B
P�B
T�B
P�B
N�B
O�B
K�B
H�B
L�B
K�B
O�B
J�B
@�B
G�B
Q�B
P�B
Q�B
L�B
K�B
I�B
A�B
8RB
B�B
K�B
H�B
H�B
M�B
N�B
M�B
J�B
H�B
H�B
D�B
K�B
O�B
P�B
O�B
O�B
N�B
K�B
K�B
H�B
G�B
D�B
=qB
49B
�B
9XB
C�B
C�B
C�B
B�B
B�B
@�B
<jB
;dB
@�B
>wB
:^B
6FB
/B
49B
<jB
:^B
49B
=qB
=qB
>wB
=qB
<jB
8RB
.B
.B
5?B
6FB
2-B
0!B
49B
5?B
33B
49B
33B
1'B
/B
0!B
2-B
1'B
0!B
,B
+B
)�B
 �B
-B
1'B
0!B
/B
.B
)�B
!�B
 �B
!�B
�B
%�B
'�B
"�B
'�B
'�B
&�B
#�B
 �B
"�B
�B
uB
DB
�B
�B
�B
"�B
 �B
�B
�B
!�B
 �B
�B
�B
�B
oB
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
�B
!�B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
 �B
�B
�B
�B
!�B
�B
�B
�B
�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
$�B
#�B
"�B
!�B
 �B
�B
�B
�B
"�B
$�B
#�B
#�B
#�B
!�B
!�B
�B
�B
 �B
%�B
#�B
"�B
�B
 �B
�B
�B
�B
#�B
%�B
%�B
(�B
&�B
&�B
%�B
%�B
'�B
$�B
$�B
(�B
)�B
(�B
%�B
�B
%�B
$�B
#�B
"�B
(�B
-B
.B
.B
/B
.B
/B
0!B
/B
.B
.B
,B
,B
+B
(�B
(�B
,B
)�B
,B
.B
+B
,B
,B
(�B
,B
0!B
0!B
/B
-B
+B
%�B
-B
.B
+B
&�B
!�B
'�B
+B
-B
/B
0!B
2-B
1'B
1'B
/B
5?B
6FB
5?B
5?B
6FB
49B
1'B
-B
.B
49B
33B
9XB
;dB
:^B
:^B
:^B
9XB
6FB
8RB
:^B
9XB
:^B
:^B
:^B
7LB
<jB
=qB
>wB
<jB
:^B
>wB
?}B
=qB
=qB
>wB
=qB
<jB
;dB
=qB
=qB
<jB
>wB
>wB
?}B
?}B
A�B
A�B
@�B
@�B
@�B
C�B
C�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
C�B
B�B
B�B
F�B
F�B
F�B
F�B
E�B
D�B
E�B
G�B
H�B
G�B
F�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
J�B
L�B
I�B
F�B
F�B
I�B
H�B
I�B
H�B
I�B
H�B
I�B
H�B
F�B
J�B
L�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
K�B
I�B
K�B
K�B
N�B
M�B
N�B
P�B
O�B
O�B
Q�B
Q�B
R�B
Q�B
R�B
Q�B
P�B
O�B
M�B
M�B
Q�B
T�B
W
B
VB
S�B
T�B
T�B
XB
YB
YB
YB
YB
YB
YB
XB
XB
YB
YB
XB
YB
ZB
ZB
YB
YB
XB
YB
YB
ZB
YB
^5B
`BB
`BB
_;B
_;B
^5B
\)B
XB
\)B
ZB
_;B
`BB
`BB
^5B
[#B
[#B
\)B
`BB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
dZB
dZB
e`B
e`B
cTB
bNB
bNB
bNB
cTB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
dZB
dZB
e`B
ffB
gmB
hsB
gmB
hsB
iyB
iyB
hsB
gmB
ffB
ffB
jB
k�B
jB
jB
iyB
hsB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
jB
k�B
jB
l�B
m�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
q�B
q�B
p�B
o�B
l�B
p�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
o�B
o�B
s�B
u�B
u�B
t�B
s�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
v�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
y�B
y�B
x�B
w�B
x�B
x�B
z�B
y�B
y�B
x�B
w�B
z�B
{�B
z�B
y�B
z�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
}�B
� B
� B
~�B
}�B
}�B
}�B
|�B
|�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�%B
�%B
�B
�B
�+B
�1B
�+B
�%B
�%B
�B
�%B
�7B
�7B
�=B
�7B
�1B
�B
�B
�%B
�B
�+B
�1B
�1B
�+B
�%B
�B
�%B
�1B
�=B
�=B
�=B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�7B
�=B
�=B
�7B
�=B
�DB
�=B
�=B
�DB
�=B
�JB
�VB
�PB
�\B
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�hB
�oB
�oB
�hB
�oB
�uB
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111441441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
$�B
"�B
$�B
$�B
#�B
#�B
"�B
 �B
!�B
�B
)�B
,B
,B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
/B
0!B
/B
.B
-B
,B
,B
,B
-B
.B
/B
1'B
2-B
33B
49B
9XB
9XB
(�B
1'B
;dB
#�B
$�B
�=B
��B
B
~�B
�RB
�TB �B�BVB
�BB
~�B
��B
�uB	�'B	�B
(�B	�B	��B	�+B	bNB	(�B	  BĜB	hB�`B�RBl�B�#B	0!B	)�B	�B	{B	#�B	�B��B	B	%�B	+B	'�B	J�B	]/B	�B	��B	��B	�B	w�B	��B	��B	��B	�\B	y�B	bNB	w�B	aHB	��B	�B	��B	�bB	s�B	��B	��B	ȴB	�;B	�B	�NB	�yB	��B
{B
{B
VB
B
JB
�B
)�B
;dB
D�B
@�B
=qB
>wB
>wB
O�B
ZB
\)B
^5B
]/B
]/B
\)B
[#B
W
B
R�B
N�B
O�B
K�B
C�B
M�B
VB
Q�B
K�B
A�B
R�B
N�B
J�B
n�B
p�B
u�B
�B
~�B
{�B
y�B
w�B
w�B
u�B
r�B
m�B
cTB
]/B
`BB
v�B
v�B
s�B
n�B
k�B
l�B
iyB
iyB
k�B
_;B
[#B
^5B
dZB
k�B
l�B
jB
hsB
k�B
m�B
jB
gmB
dZB
^5B
XB
O�B
Q�B
T�B
VB
W
B
YB
XB
]/B
\)B
^5B
^5B
_;B
`BB
]/B
ZB
VB
O�B
O�B
P�B
T�B
P�B
N�B
O�B
K�B
H�B
L�B
K�B
O�B
J�B
@�B
G�B
Q�B
P�B
Q�B
L�B
K�B
I�B
A�B
8RB
B�B
K�B
H�B
H�B
M�B
N�B
M�B
J�B
H�B
H�B
D�B
K�B
O�B
P�B
O�B
O�B
N�B
K�B
K�B
H�B
G�B
D�B
=qB
49B
�B
9XB
C�B
C�B
C�B
B�B
B�B
@�B
<jB
;dB
@�B
>wB
:^B
6FB
/B
49B
<jB
:^B
49B
=qB
=qB
>wB
=qB
<jB
8RB
.B
.B
5?B
6FB
2-B
0!B
49B
5?B
33B
49B
33B
1'B
/B
0!B
2-B
1'B
0!B
,B
+B
)�B
 �B
-B
1'B
0!B
/B
.B
)�B
!�B
 �B
!�B
�B
%�B
'�B
"�B
'�B
'�B
&�B
#�B
 �B
"�B
�B
uB
DB
�B
�B
�B
"�B
 �B
�B
�B
!�B
 �B
�B
�B
�B
oB
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
�B
!�B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
 �B
�B
�B
�B
!�B
�B
�B
�B
�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
$�B
#�B
"�B
!�B
 �B
�B
�B
�B
"�B
$�B
#�B
#�B
#�B
!�B
!�B
�B
�B
 �B
%�B
#�B
"�B
�B
 �B
�B
�B
�B
#�B
%�B
%�B
(�B
&�B
&�B
%�B
%�B
'�B
$�B
$�B
(�B
)�B
(�B
%�B
�B
%�B
$�B
#�B
"�B
(�B
-B
.B
.B
/B
.B
/B
0!B
/B
.B
.B
,B
,B
+B
(�B
(�B
,B
)�B
,B
.B
+B
,B
,B
(�B
,B
0!B
0!B
/B
-B
+B
%�B
-B
.B
+B
&�B
!�B
'�B
+B
-B
/B
0!B
2-B
1'B
1'B
/B
5?B
6FB
5?B
5?B
6FB
49B
1'B
-B
.B
49B
33B
9XB
;dB
:^B
:^B
:^B
9XB
6FB
8RB
:^B
9XB
:^B
:^B
:^B
7LB
<jB
=qB
>wB
<jB
:^B
>wB
?}B
=qB
=qB
>wB
=qB
<jB
;dB
=qB
=qB
<jB
>wB
>wB
?}B
?}B
A�B
A�B
@�B
@�B
@�B
C�B
C�B
B�B
B�B
A�B
B�B
C�B
C�B
C�B
D�B
C�B
B�B
B�B
F�B
F�B
F�B
F�B
E�B
D�B
E�B
G�B
H�B
G�B
F�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
J�B
L�B
I�B
F�B
F�B
I�B
H�B
I�B
H�B
I�B
H�B
I�B
H�B
F�B
J�B
L�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
K�B
I�B
K�B
K�B
N�B
M�B
N�B
P�B
O�B
O�B
Q�B
Q�B
R�B
Q�B
R�B
Q�B
P�B
O�B
M�B
M�B
Q�B
T�B
W
B
VB
S�B
T�B
T�B
XB
YB
YB
YB
YB
YB
YB
XB
XB
YB
YB
XB
YB
ZB
ZB
YB
YB
XB
YB
YB
ZB
YB
^5B
`BB
`BB
_;B
_;B
^5B
\)B
XB
\)B
ZB
_;B
`BB
`BB
^5B
[#B
[#B
\)B
`BB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
bNB
dZB
dZB
e`B
e`B
cTB
bNB
bNB
bNB
cTB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
dZB
dZB
e`B
ffB
gmB
hsB
gmB
hsB
iyB
iyB
hsB
gmB
ffB
ffB
jB
k�B
jB
jB
iyB
hsB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
jB
k�B
jB
l�B
m�B
n�B
m�B
m�B
m�B
n�B
o�B
o�B
o�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
q�B
q�B
p�B
o�B
l�B
p�B
s�B
s�B
r�B
r�B
r�B
r�B
q�B
o�B
o�B
s�B
u�B
u�B
t�B
s�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
t�B
t�B
t�B
s�B
u�B
v�B
v�B
v�B
v�B
v�B
u�B
v�B
w�B
w�B
x�B
w�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
y�B
y�B
x�B
w�B
x�B
x�B
z�B
y�B
y�B
x�B
w�B
z�B
{�B
z�B
y�B
z�B
|�B
|�B
|�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
}�B
� B
� B
~�B
}�B
}�B
}�B
|�B
|�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�%B
�%B
�B
�B
�+B
�1B
�+B
�%B
�%B
�B
�%B
�7B
�7B
�=B
�7B
�1B
�B
�B
�%B
�B
�+B
�1B
�1B
�+B
�%B
�B
�%B
�1B
�=B
�=B
�=B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�7B
�=B
�=B
�7B
�=B
�DB
�=B
�=B
�DB
�=B
�JB
�VB
�PB
�\B
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�\B
�hB
�oB
�oB
�hB
�oB
�uB
�uB
�uB
�uB
��B
��B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111441441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220625090112                              AO  ARCAADJP                                                                    20220625090112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220625090112  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220625090112  QCF$                G�O�G�O�G�O�4000            