CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:13Z creation;2022-06-04T19:23:14Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192313  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�W�΁��1   @�W�=��@-"M����c�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B���B���B�ffB���B�  B�  B�  B�  B�  Bԙ�Bי�B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @34@s34@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B8  B@  BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B�fgB���B�fgB�fgB���B���B���B�33B�fgB���B���B���B���B���B�fgB�fgB���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC  C  C�fC�fC�fC�fC�fC�fC�fC!��C#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCF  CH  CI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN�4DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dz� Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�� D�� D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̺�A̵�A�ƨA���A���A��HA���A��[A�ӏA��]A��#A���A���A��/A�ݘA��]A�یA��pA���A��A��A��,A��A��A��A��A��A��A��mA��A��A��pA��EA��KA̹$A̩�A̙1A̚7A�+A�I�A�T,A�D�A�یA�
	A��A�A�hA�v+A���A�zxA�e�A���A���A��lA��!A�`�A�E�A�A�%�A�B�A���A�ٴA��TA|�Ax��Au�#Aq�yAmjAj �Aij�AhS&AfqvAcaA`X�A_-�AY��AVCAR4�AN�AJ��AG9�AD*�A@�A>҉A=MA:�A7��A7m]A6�AA54A1�A0hsA.OA+��A*�uA)��A)��A)�5A)�A)@OA(� A(_A'�zA'.�A&?}A%�A%�A$6�A"�A"�`A!�A!(A!K^A!�A v`A %FA�=A�5A{A��A��AA�qAffA��A��AMA�A1�ATaAV�A(�Aj�A
�6A
��A�A�A�=A�&An/A
H�A�8A�ZA
=A�+A^�A3�A�A�A��Ae,Ah
A��A_A \A	�A��A�Ah
AW?A	A�A�A ��A �A 4n@���@���@�}V@�A A b@���@���@��@���@��	@��@�5�@���@�i�@�l�@�L0@��@�A�@�t�@�@���@��@��@�͟@�H@�\)@�x�@��@��@��K@��@�=@�U2@��@��@�'R@�n@�Vm@��f@��@�O�@��@�@�X@��@�U2@���@�$@�E9@��@�k�@���@��@�1@�6z@�4@��@��@�U2@�X@��@��@㹌@�?}@�o@��@�P@���@�M�@ߗ�@�%F@��m@�|�@��@ݚk@�33@�bN@۳�@�B�@��|@ڥz@ډ�@�p;@�Q�@�&�@���@�/@ؾ@ؠ�@�c @��9@�q@ֿ�@ִ9@֏\@�x@��@��&@���@��@Ҟ�@� �@ч�@�J#@�+@��H@ϼ�@ϓ�@�hs@�X@�L�@�?}@�
=@Ϊe@�l�@�6@�خ@ͅ@�$t@��@ʹ�@��@ɦ�@�9�@�&�@ǡ�@ǋ�@ǈf@ǅ@�@�-�@Ŏ"@���@�|@�/@­�@���@� i@�w�@�H�@��+@��@���@��<@��@�1'@�c�@��@��}@��@�J#@�@�@���@��@�ѷ@�ѷ@���@�J�@���@��@�ݘ@�H�@���@��B@�~�@��@��@���@���@�Q@�bN@�Ta@�	@�ԕ@�Q�@�� @�PH@�@�iD@�@���@�Z@�GE@��d@�o�@�/@���@�:*@��D@���@��@��{@�9�@��@��Y@�L0@�,=@�1@��^@�)_@��B@���@���@���@��Y@�~(@�n�@�]d@��@���@���@��~@�a@��@��@�L0@�&�@�4@��3@��"@�RT@�4�@��@��`@��@���@�u%@�J�@�1�@�#:@�7@��*@���@�]d@��@�m]@�?}@��)@���@�9X@��W@���@�8�@��@���@��4@�I�@��@�Vm@�-w@�!�@��@��@���@���@�I�@��@��@��	@�F@�@��R@�S�@�	�@��n@�J�@��@�ѷ@���@�!�@��@��"@�S�@��@��@��F@�)�@��q@�iD@� \@���@�K^@��T@��w@�zx@��@��x@���@�u%@�3�@�e,@��M@���@�(�@���@��h@�#�@��"@��@���@��h@�Q�@�+k@��@�?}@��E@���@��@��I@��\@�|�@�:�@�
�@���@��[@�e�@�[W@�W?@�S&@�;d@�)_@�+@��@��p@���@�h
@�@�@�@���@���@�?}@�*0@��`@��I@�;�@���@��^@���@�_p@��M@��,@���@��z@���@��@�z�@�@�@��@� \@��[@��'@�~(@��@���@�t�@�k�@�a�@�Dg@�>�@�Dg@�L�@�(�@�Ɇ@�@���@�Vm@�	l@��O@��<@�-@��@F�@~�2@~�<@~�+@~B[@}��@|֡@{�+@{]�@{8@z��@y��@yB�@x�p@xXy@w�A@w�:@w@O@wC@v�]@v��@vTa@u��@u�=@uB�@t�`@t��@t%�@sC@r��@r5?@q��@q��@qa�@pA�@oZ�@n\�@m�9@m��@m@l�O@l�@kMj@k�@j�\@jV@j8�@j=q@jO@i(�@h�@g��@g��@g�@gA�@g"�@f�,@f;�@e�@e�@e��@e��@e�"@ek�@eIR@e;@d@c9�@b;�@b	@a��@a��@aX@`�`@`��@`N�@_��@_Z�@^�<@^^5@^u@]�@]��@]@\��@\�Y@\�@[��@[��@[v`@Z�8@Zh
@Z�@Y�d@YDg@X�9@Xj@X!@W��@Wƨ@W�@WW?@W4�@WY@V�@Vp;@Vff@V\�@U�o@T��@T��@Tz�@T]d@T*�@S�F@R��@R�,@R�@R�b@ROv@R�@Ru@Q��@Q�@Qϫ@Q�-@Q�'@Q#�@Pg8@O�Q@O��@Oa@O,�@N�@Nl�@N�@M�@M�9@M�X@MY�@M*0@M�@L��@L�z@L�Y@LN�@L�@KdZ@J��@JQ@I��@IA @I4@H�K@G��@F�@FJ�@E��@EQ�@Dw�@C�&@Cx@C�@B��@B$�@A�@A�7@ADg@A�@@|�@@/�@?�k@>��@=zx@=�@<��@<I�@<*�@;��@:��@:��@:l�@:)�@:	@9�N@9f�@9*0@8��@8��@8�e@8w�@8H@7��@7�	@7�@6�<@6Z�@6J�@66�@6($@6@5��@5-w@4U2@3�&@3�k@3s@3a@3Z�@3P�@3)_@2�8@2�M@2��@2-@1@1\�@0�@0c�@0~@0�@0�@/�W@/ƨ@/��@/�@/l�@/O@/9�@.��@.H�@-�D@-��@-o @,y>@,7@+�@+�	@+\)@+8@+�@*�X@*��@*+k@*e@*4@*�@)��@)��@)*0@)�@(�@(֡@(�)@(�@(��@(�@(D�@(*�@'� @'�k@'=@&�s@&��@&ff@&�@%��@%�h@%:�@$�	@$�/@$��@$�@#�$@#�@"�@"�\@"_@!�>@!�@!�'@!j@!:�@!�@ �p@ ��@ M@ !@خ@�f@A�@/�@
=@��@R�@��@X@@@��@Ɇ@��@�@N�@�r@�:@$t@��@:*@�z@��@w2@J�@�@�@�@��@��@bN@ݘ@�@��@��@A�@�@�@�2@�H@͟@��@�@s�@L0@�@�)@�@��@��@^�@G�@�@��@~@@  @�@��@��@iD@;d@�@�c@�@��@h
@�@�n@x�@ \@�`@֡@ѷ@��@�@��@�Y@Q�@�;@{J@U�@J#@C�@4�@'�@�@�c@��@V@-@�D@ԕ@��@�-@�@J�@4@2a@�@�f@�`@�/@��@��@r�@M@,=@��@�Q@� @��@��@W?@,�@
=@
�m@
��@
v�@
&�@	��@	�9@	��@	�N@	�t@	��@	s�@	Q�@	�@�|@��@�I@l"@<�@7@�@��@��@e�@;d@�@��@u%@?@@@�@��@�@}�@A @=�@<6@#�@�@��@�D@[�@6@@�]@�@��@��@n/@K�@@O@+@�8@�@�@�s@�}@�1@^5@3�@{@�Z@�o@��@�@@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̺�A̵�A�ƨA���A���A��HA���A��[A�ӏA��]A��#A���A���A��/A�ݘA��]A�یA��pA���A��A��A��,A��A��A��A��A��A��A��mA��A��A��pA��EA��KA̹$A̩�A̙1A̚7A�+A�I�A�T,A�D�A�یA�
	A��A�A�hA�v+A���A�zxA�e�A���A���A��lA��!A�`�A�E�A�A�%�A�B�A���A�ٴA��TA|�Ax��Au�#Aq�yAmjAj �Aij�AhS&AfqvAcaA`X�A_-�AY��AVCAR4�AN�AJ��AG9�AD*�A@�A>҉A=MA:�A7��A7m]A6�AA54A1�A0hsA.OA+��A*�uA)��A)��A)�5A)�A)@OA(� A(_A'�zA'.�A&?}A%�A%�A$6�A"�A"�`A!�A!(A!K^A!�A v`A %FA�=A�5A{A��A��AA�qAffA��A��AMA�A1�ATaAV�A(�Aj�A
�6A
��A�A�A�=A�&An/A
H�A�8A�ZA
=A�+A^�A3�A�A�A��Ae,Ah
A��A_A \A	�A��A�Ah
AW?A	A�A�A ��A �A 4n@���@���@�}V@�A A b@���@���@��@���@��	@��@�5�@���@�i�@�l�@�L0@��@�A�@�t�@�@���@��@��@�͟@�H@�\)@�x�@��@��@��K@��@�=@�U2@��@��@�'R@�n@�Vm@��f@��@�O�@��@�@�X@��@�U2@���@�$@�E9@��@�k�@���@��@�1@�6z@�4@��@��@�U2@�X@��@��@㹌@�?}@�o@��@�P@���@�M�@ߗ�@�%F@��m@�|�@��@ݚk@�33@�bN@۳�@�B�@��|@ڥz@ډ�@�p;@�Q�@�&�@���@�/@ؾ@ؠ�@�c @��9@�q@ֿ�@ִ9@֏\@�x@��@��&@���@��@Ҟ�@� �@ч�@�J#@�+@��H@ϼ�@ϓ�@�hs@�X@�L�@�?}@�
=@Ϊe@�l�@�6@�خ@ͅ@�$t@��@ʹ�@��@ɦ�@�9�@�&�@ǡ�@ǋ�@ǈf@ǅ@�@�-�@Ŏ"@���@�|@�/@­�@���@� i@�w�@�H�@��+@��@���@��<@��@�1'@�c�@��@��}@��@�J#@�@�@���@��@�ѷ@�ѷ@���@�J�@���@��@�ݘ@�H�@���@��B@�~�@��@��@���@���@�Q@�bN@�Ta@�	@�ԕ@�Q�@�� @�PH@�@�iD@�@���@�Z@�GE@��d@�o�@�/@���@�:*@��D@���@��@��{@�9�@��@��Y@�L0@�,=@�1@��^@�)_@��B@���@���@���@��Y@�~(@�n�@�]d@��@���@���@��~@�a@��@��@�L0@�&�@�4@��3@��"@�RT@�4�@��@��`@��@���@�u%@�J�@�1�@�#:@�7@��*@���@�]d@��@�m]@�?}@��)@���@�9X@��W@���@�8�@��@���@��4@�I�@��@�Vm@�-w@�!�@��@��@���@���@�I�@��@��@��	@�F@�@��R@�S�@�	�@��n@�J�@��@�ѷ@���@�!�@��@��"@�S�@��@��@��F@�)�@��q@�iD@� \@���@�K^@��T@��w@�zx@��@��x@���@�u%@�3�@�e,@��M@���@�(�@���@��h@�#�@��"@��@���@��h@�Q�@�+k@��@�?}@��E@���@��@��I@��\@�|�@�:�@�
�@���@��[@�e�@�[W@�W?@�S&@�;d@�)_@�+@��@��p@���@�h
@�@�@�@���@���@�?}@�*0@��`@��I@�;�@���@��^@���@�_p@��M@��,@���@��z@���@��@�z�@�@�@��@� \@��[@��'@�~(@��@���@�t�@�k�@�a�@�Dg@�>�@�Dg@�L�@�(�@�Ɇ@�@���@�Vm@�	l@��O@��<@�-@��@F�@~�2@~�<@~�+@~B[@}��@|֡@{�+@{]�@{8@z��@y��@yB�@x�p@xXy@w�A@w�:@w@O@wC@v�]@v��@vTa@u��@u�=@uB�@t�`@t��@t%�@sC@r��@r5?@q��@q��@qa�@pA�@oZ�@n\�@m�9@m��@m@l�O@l�@kMj@k�@j�\@jV@j8�@j=q@jO@i(�@h�@g��@g��@g�@gA�@g"�@f�,@f;�@e�@e�@e��@e��@e�"@ek�@eIR@e;@d@c9�@b;�@b	@a��@a��@aX@`�`@`��@`N�@_��@_Z�@^�<@^^5@^u@]�@]��@]@\��@\�Y@\�@[��@[��@[v`@Z�8@Zh
@Z�@Y�d@YDg@X�9@Xj@X!@W��@Wƨ@W�@WW?@W4�@WY@V�@Vp;@Vff@V\�@U�o@T��@T��@Tz�@T]d@T*�@S�F@R��@R�,@R�@R�b@ROv@R�@Ru@Q��@Q�@Qϫ@Q�-@Q�'@Q#�@Pg8@O�Q@O��@Oa@O,�@N�@Nl�@N�@M�@M�9@M�X@MY�@M*0@M�@L��@L�z@L�Y@LN�@L�@KdZ@J��@JQ@I��@IA @I4@H�K@G��@F�@FJ�@E��@EQ�@Dw�@C�&@Cx@C�@B��@B$�@A�@A�7@ADg@A�@@|�@@/�@?�k@>��@=zx@=�@<��@<I�@<*�@;��@:��@:��@:l�@:)�@:	@9�N@9f�@9*0@8��@8��@8�e@8w�@8H@7��@7�	@7�@6�<@6Z�@6J�@66�@6($@6@5��@5-w@4U2@3�&@3�k@3s@3a@3Z�@3P�@3)_@2�8@2�M@2��@2-@1@1\�@0�@0c�@0~@0�@0�@/�W@/ƨ@/��@/�@/l�@/O@/9�@.��@.H�@-�D@-��@-o @,y>@,7@+�@+�	@+\)@+8@+�@*�X@*��@*+k@*e@*4@*�@)��@)��@)*0@)�@(�@(֡@(�)@(�@(��@(�@(D�@(*�@'� @'�k@'=@&�s@&��@&ff@&�@%��@%�h@%:�@$�	@$�/@$��@$�@#�$@#�@"�@"�\@"_@!�>@!�@!�'@!j@!:�@!�@ �p@ ��@ M@ !@خ@�f@A�@/�@
=@��@R�@��@X@@@��@Ɇ@��@�@N�@�r@�:@$t@��@:*@�z@��@w2@J�@�@�@�@��@��@bN@ݘ@�@��@��@A�@�@�@�2@�H@͟@��@�@s�@L0@�@�)@�@��@��@^�@G�@�@��@~@@  @�@��@��@iD@;d@�@�c@�@��@h
@�@�n@x�@ \@�`@֡@ѷ@��@�@��@�Y@Q�@�;@{J@U�@J#@C�@4�@'�@�@�c@��@V@-@�D@ԕ@��@�-@�@J�@4@2a@�@�f@�`@�/@��@��@r�@M@,=@��@�Q@� @��@��@W?@,�@
=@
�m@
��@
v�@
&�@	��@	�9@	��@	�N@	�t@	��@	s�@	Q�@	�@�|@��@�I@l"@<�@7@�@��@��@e�@;d@�@��@u%@?@@@�@��@�@}�@A @=�@<6@#�@�@��@�D@[�@6@@�]@�@��@��@n/@K�@@O@+@�8@�@�@�s@�}@�1@^5@3�@{@�Z@�o@��@�@@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B��B��BƎB�?B�?BƎB��B�mBżBňBňB�mB��B��B��B�SB��BŢB�B�BżBżB��BżB�mB�SB�9B�9B�9B��BāB��BÖBðB�B��B�QBD�B	Z�B	\B	a�B	�IB	�FB
�B
��B
|6B
e�B
j�B
j0B
mB
OBB
�B
 'B
uB
B
�B	�B
"�B
�B
_B	�B	�B	��B	�B	��B	~�B	h�B	c�B	^�B	WsB	MjB	H�B	DgB	-�B	�B	B�B�BB� BǮB��B��B�B�;B�B��B��B�RB��B��B� BοB�"BۦB�OB�nB�6B	DB	�B	jB	�B	;B	"�B	"�B	'8B	4nB	/5B	8�B	:�B	T,B	c�B	pUB	v�B	z�B	z^B	xB	s�B	h$B	X�B	G�B	9�B	2-B	,�B	$�B	�B	B	�B	$�B	�B	WB	�B	�B	%,B	5?B	E�B	M�B	YB	Z�B	^�B	ZB	R�B	VB	cTB	`BB	c�B	]dB	MjB	Q�B	Z7B	f�B	f�B	d&B	n�B	q[B	t�B	y�B	y>B	{�B	|�B	�uB	�tB	�zB	��B	�B	�BB	��B	�}B	�KB	�|B	�tB	�,B	��B	�&B	��B	�|B	��B	��B	�^B	��B	��B	ňB	�B	ЗB	ΊB	ĜB	�B	��B	�qB	�0B	��B	��B	��B	��B	��B	��B	�UB	�lB	�B	�xB	�B	�vB	�vB	��B	ߊB	�'B	�vB	��B	��B	��B	��B	�nB	�B	�B	�NB	�B	�B	�B	�B	�&B	��B	��B	��B	�ZB	��B	��B	��B	�B	��B	��B	�B	��B	�,B	�B	�LB	�B	�B	�B	��B	�B	�mB	�B	�yB	�B	�B	��B	�B	�B	�0B	�B	�KB	��B	�B	�B	�B	�KB	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�]B	�wB	�B	�]B	�]B	�B	�IB	�B	�}B	�}B	�cB	�}B	� B	�B	�OB	�B	�B	�iB	�iB	�/B	��B	��B	��B	�OB	�B	�B	�B	�B	�!B	�vB	�B	�B	��B	�[B	��B	��B	�B	�-B	��B	��B	��B	�fB	�B	�2B	�B	�B	��B	�FB	��B	�9B	�B	��B	�tB	��B	�+B	��B	��B	�`B	��B	��B	�fB	�B	�RB	��B	��B	��B	�B	�0B	�"B	�HB
 �B
 �B
 B
UB
�B
�B
AB
�B
B
�B
B
�B
?B
tB
YB
YB
YB
tB
KB
	RB
	�B
	�B

XB

rB

�B

�B

�B
DB
�B
�B
�B
dB
dB
dB
JB
dB
dB
�B
B
PB
6B
B
B
�B
B
�B
B
�B
�B
�B
�B
B
vB
[B
�B
�B
�B
�B
�B
�B
�B
FB
aB
�B
�B
{B
2B
B
�B
B
�B
�B
$B
$B
�B
+B
yB
�B
�B
�B
�B
yB
yB
�B
B
B
eB
�B
�B
7B
QB
	B
=B
�B
�B
�B
�B
)B
�B
�B
�B
IB
IB
IB
~B
�B
OB
�B
�B
�B
;B
pB
!B
B
!B
!B
�B
!B
;B
�B
�B
�B
�B
 'B
 'B
 'B
 'B
 'B
 B
 'B
 \B
 \B
 �B
"4B
"�B
#TB
#nB
#nB
#TB
# B
# B
"�B
"�B
"�B
#B
"�B
"�B
"�B
"�B
#B
#B
"�B
#TB
"�B
#�B
#nB
#�B
#�B
$@B
$tB
$tB
$�B
$�B
%�B
&B
&fB
&LB
'B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(>B
)B
)_B
)*B
)�B
)�B
)yB
*B
*0B
*eB
*B
*B
*�B
+B
+B
*�B
*KB
*B
*eB
*KB
+�B
,�B
-]B
,�B
-B
-)B
,�B
,�B
,�B
,�B
-CB
.IB
./B
.cB
/�B
/�B
/�B
0B
0B
0;B
0oB
0�B
0�B
2-B
2aB
2|B
2aB
2�B
3hB
3�B
49B
4nB
5?B
5?B
5tB
5�B
5�B
5?B
6FB
7B
8B
8B
8RB
8�B
8�B
9	B
9rB
9XB
:B
9�B
9�B
9�B
9�B
:�B
:�B
;JB
;B
;B
;�B
;�B
<B
<PB
<�B
<�B
<�B
<�B
=B
=B
=B
=B
=�B
>�B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
AB
A�B
B[B
B�B
C-B
CGB
C�B
C�B
D3B
DgB
EB
EB
EB
EB
D�B
EB
E9B
EmB
E�B
F%B
F?B
F�B
F�B
F�B
F�B
GEB
GEB
GEB
G�B
H1B
HB
G�B
HfB
IlB
I�B
I�B
I�B
I�B
JXB
K)B
K)B
KDB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LJB
L�B
MB
M6B
MjB
MjB
M�B
NB
N"B
N<B
N<B
NVB
N�B
N�B
N�B
OBB
OBB
OvB
O\B
O�B
O�B
O�B
PbB
Q B
Q B
P�B
P�B
R B
SB
S&B
S�B
S�B
TaB
T�B
T�B
UB
U�B
U�B
VB
VB
VmB
VmB
W
B
W?B
W�B
X+B
YKB
YeB
Y�B
ZB
Y�B
ZkB
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
\)B
\]B
\]B
\xB
\�B
\�B
\�B
]IB
]�B
^B
^OB
^jB
^jB
^jB
^�B
^�B
_;B
_�B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
a�B
a�B
bB
bB
b�B
b�B
cB
cB
cB
c:B
c:B
cTB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gmB
gmB
gmB
gmB
g�B
g�B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
iDB
i�B
i�B
i�B
jKB
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
l=B
lqB
l�B
l�B
m]B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
nIB
n}B
n�B
oB
o5B
o5B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q[B
q�B
q�B
raB
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v`B
v`B
v�B
vzB
v�B
v�B
v�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
y$B
y>B
y>B
y�B
y�B
z^B
z�B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{JB
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
B
.B
.B
HB
}B
�B
�B
�B
�B
�4B
�4B
�4B
�OB
��B
��B
��B
��B
�B
� B
�UB
��B
��B
��B
��B
��B
�B
�AB
�[B
��B
��B
��B
��B
�-B
�GB
��B
��B
��B
��B
��B
�gB
��B
��B
�B
��B
��B
��B
��B
�%B
�%B
�YB
�tB
��B
��B
��B
�+B
��B
��B
��B
�KB
�fB
��B
��B
�B
�RB
�lB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
��B
��B
�B
�B
�^B
�^B
�xB
�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B��B��BƎB�?B�?BƎB��B�mBżBňBňB�mB��B��B��B�SB��BŢB�B�BżBżB��BżB�mB�SB�9B�9B�9B��BāB��BÖBðB�B��B�QBD�B	Z�B	\B	a�B	�IB	�FB
�B
��B
|6B
e�B
j�B
j0B
mB
OBB
�B
 'B
uB
B
�B	�B
"�B
�B
_B	�B	�B	��B	�B	��B	~�B	h�B	c�B	^�B	WsB	MjB	H�B	DgB	-�B	�B	B�B�BB� BǮB��B��B�B�;B�B��B��B�RB��B��B� BοB�"BۦB�OB�nB�6B	DB	�B	jB	�B	;B	"�B	"�B	'8B	4nB	/5B	8�B	:�B	T,B	c�B	pUB	v�B	z�B	z^B	xB	s�B	h$B	X�B	G�B	9�B	2-B	,�B	$�B	�B	B	�B	$�B	�B	WB	�B	�B	%,B	5?B	E�B	M�B	YB	Z�B	^�B	ZB	R�B	VB	cTB	`BB	c�B	]dB	MjB	Q�B	Z7B	f�B	f�B	d&B	n�B	q[B	t�B	y�B	y>B	{�B	|�B	�uB	�tB	�zB	��B	�B	�BB	��B	�}B	�KB	�|B	�tB	�,B	��B	�&B	��B	�|B	��B	��B	�^B	��B	��B	ňB	�B	ЗB	ΊB	ĜB	�B	��B	�qB	�0B	��B	��B	��B	��B	��B	��B	�UB	�lB	�B	�xB	�B	�vB	�vB	��B	ߊB	�'B	�vB	��B	��B	��B	��B	�nB	�B	�B	�NB	�B	�B	�B	�B	�&B	��B	��B	��B	�ZB	��B	��B	��B	�B	��B	��B	�B	��B	�,B	�B	�LB	�B	�B	�B	��B	�B	�mB	�B	�yB	�B	�B	��B	�B	�B	�0B	�B	�KB	��B	�B	�B	�B	�KB	��B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�]B	�wB	�B	�]B	�]B	�B	�IB	�B	�}B	�}B	�cB	�}B	� B	�B	�OB	�B	�B	�iB	�iB	�/B	��B	��B	��B	�OB	�B	�B	�B	�B	�!B	�vB	�B	�B	��B	�[B	��B	��B	�B	�-B	��B	��B	��B	�fB	�B	�2B	�B	�B	��B	�FB	��B	�9B	�B	��B	�tB	��B	�+B	��B	��B	�`B	��B	��B	�fB	�B	�RB	��B	��B	��B	�B	�0B	�"B	�HB
 �B
 �B
 B
UB
�B
�B
AB
�B
B
�B
B
�B
?B
tB
YB
YB
YB
tB
KB
	RB
	�B
	�B

XB

rB

�B

�B

�B
DB
�B
�B
�B
dB
dB
dB
JB
dB
dB
�B
B
PB
6B
B
B
�B
B
�B
B
�B
�B
�B
�B
B
vB
[B
�B
�B
�B
�B
�B
�B
�B
FB
aB
�B
�B
{B
2B
B
�B
B
�B
�B
$B
$B
�B
+B
yB
�B
�B
�B
�B
yB
yB
�B
B
B
eB
�B
�B
7B
QB
	B
=B
�B
�B
�B
�B
)B
�B
�B
�B
IB
IB
IB
~B
�B
OB
�B
�B
�B
;B
pB
!B
B
!B
!B
�B
!B
;B
�B
�B
�B
�B
 'B
 'B
 'B
 'B
 'B
 B
 'B
 \B
 \B
 �B
"4B
"�B
#TB
#nB
#nB
#TB
# B
# B
"�B
"�B
"�B
#B
"�B
"�B
"�B
"�B
#B
#B
"�B
#TB
"�B
#�B
#nB
#�B
#�B
$@B
$tB
$tB
$�B
$�B
%�B
&B
&fB
&LB
'B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(>B
)B
)_B
)*B
)�B
)�B
)yB
*B
*0B
*eB
*B
*B
*�B
+B
+B
*�B
*KB
*B
*eB
*KB
+�B
,�B
-]B
,�B
-B
-)B
,�B
,�B
,�B
,�B
-CB
.IB
./B
.cB
/�B
/�B
/�B
0B
0B
0;B
0oB
0�B
0�B
2-B
2aB
2|B
2aB
2�B
3hB
3�B
49B
4nB
5?B
5?B
5tB
5�B
5�B
5?B
6FB
7B
8B
8B
8RB
8�B
8�B
9	B
9rB
9XB
:B
9�B
9�B
9�B
9�B
:�B
:�B
;JB
;B
;B
;�B
;�B
<B
<PB
<�B
<�B
<�B
<�B
=B
=B
=B
=B
=�B
>�B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
@�B
AB
A�B
B[B
B�B
C-B
CGB
C�B
C�B
D3B
DgB
EB
EB
EB
EB
D�B
EB
E9B
EmB
E�B
F%B
F?B
F�B
F�B
F�B
F�B
GEB
GEB
GEB
G�B
H1B
HB
G�B
HfB
IlB
I�B
I�B
I�B
I�B
JXB
K)B
K)B
KDB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LJB
L�B
MB
M6B
MjB
MjB
M�B
NB
N"B
N<B
N<B
NVB
N�B
N�B
N�B
OBB
OBB
OvB
O\B
O�B
O�B
O�B
PbB
Q B
Q B
P�B
P�B
R B
SB
S&B
S�B
S�B
TaB
T�B
T�B
UB
U�B
U�B
VB
VB
VmB
VmB
W
B
W?B
W�B
X+B
YKB
YeB
Y�B
ZB
Y�B
ZkB
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
\)B
\]B
\]B
\xB
\�B
\�B
\�B
]IB
]�B
^B
^OB
^jB
^jB
^jB
^�B
^�B
_;B
_�B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
a�B
a�B
bB
bB
b�B
b�B
cB
cB
cB
c:B
c:B
cTB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
fB
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gmB
gmB
gmB
gmB
g�B
g�B
hXB
hXB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
iDB
i�B
i�B
i�B
jKB
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
l=B
lqB
l�B
l�B
m]B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
nIB
n}B
n�B
oB
o5B
o5B
o�B
o�B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q[B
q�B
q�B
raB
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v`B
v`B
v�B
vzB
v�B
v�B
v�B
w�B
xB
xB
xB
xB
x8B
x�B
x�B
x�B
x�B
y$B
y>B
y>B
y�B
y�B
z^B
z�B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{JB
|B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
~(B
~]B
~]B
~wB
~�B
~�B
~�B
~�B
B
.B
.B
HB
}B
�B
�B
�B
�B
�4B
�4B
�4B
�OB
��B
��B
��B
��B
�B
� B
�UB
��B
��B
��B
��B
��B
�B
�AB
�[B
��B
��B
��B
��B
�-B
�GB
��B
��B
��B
��B
��B
�gB
��B
��B
�B
��B
��B
��B
��B
�%B
�%B
�YB
�tB
��B
��B
��B
�+B
��B
��B
��B
�KB
�fB
��B
��B
�B
�RB
�lB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�XB
��B
��B
�B
�B
�^B
�^B
�xB
�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105242  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192313  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192314  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192314                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042321  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042321  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                