CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:25:16Z creation;2022-06-04T19:25:17Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604192516  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               TA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�x�;*1   @�x��<M^@+3�E����dO�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP��BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�33B���Bә�B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  C   C�fC  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&�C(�C*  C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw�fDxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,��@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BH  BPfgBW��B_��Bg��Bo34Bw��B��B���B���B���B���B���B���B�33B���B���B���B���B���B���B���B���B���B�  B���B�  Bϙ�B�fgB���B���B���B���B���B���B���B���B�33B���B���C��C�fC�fC�fC	�fC�fC�fC��C�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC&  C(  C)�fC+�fC-��C/��C1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCZ  C[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DK� DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dw  Dw� Dx  Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�@ D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�@ D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�YKA�W?A�Y�A�aHA�^A�U2A�Q�A�OBA�K�A�M�A�G�A�C-A�F?A�J�A�.�A��A��A��A��A�MA�7A�"�A�&�A�'A�)�A�$�A�Aͺ�A�ߤA�0UAʠ'A�3hAǇ_A�CA���A�7A�A�AĜA�͟AA�p�A���A�:A�poA�OBA���A�,�A���A�-wA��A�Q�A��A�*0A�SA���A��rA�H�A��\A��A�L0A�$�A��A�jA��A���A�0!A��=A�P�A��KA��A��A�n/A�p;A�J�A��A��
A��A��A�%�A���A���A��jA�=�A�y>A�"4A�AA��zA|a�Av@OAs��As|�ArTaAo#:Aj�EAeS�Ac{A^�AW�xAO=AM�3AL��AI~�AE��AC@OA>�A:�tA9�A9)_A8��A8Q�A7d�A6�[A5�A3ݘA2��A2�A1'�A/(A.�A/� A0�zA1y�A2VmA3��A3��A1�KA.zA,��A*��A*|A*?A)��A)K�A)I�A)!A(�QA(��A(ϫA(zA'�HA'e�A'CA&��A&u�A%�vA%�XA$�A#�"A#�hA#��A#�PA#�A!�mA!n/A �A�kA�)A�CAg8A_A��A2aAiDAc AzA��A��AVA��A \A$A�&A�'A8�A�mA�AA�A�A��A��A�vA%�Au�A($AߤA�A�=AL�A
=A
�6A
�AA
�A	��A	q�A�;A��A�tA�IA��A�A�9A��A�AAYKAH�A��A?}A��A�7Ak�AJ�AC-A�Ap�Ai�Ao�An/Al"AE9A��A�DA	AcAOA$A�NA��A iAS�A]dAxA �'A ��A ��A tTA J�@���@�j@�|�@��0@�<6@�C@��L@�j@�?�@��r@�8�@��@�U�@�p;@�>B@�>B@�2�@��j@�q@�S�@���@�u@�@�H�@��[@�	@�$t@�6@�z@�$@�^@�W?@��@�j�@�xl@�c�@�Z�@��@�҉@褩@脶@�1'@���@��@�\@�@�b�@�&�@�˒@�0@���@��@���@��
@�S�@�?}@��@ޑ @�_@�hs@܃�@�*�@�g�@�Ta@ٺ^@���@؈�@��A@ת�@�g�@�F@�33@��y@�j@Պ�@�<�@�ݘ@�1�@ҸR@�H@���@�n/@�n�@�p�@�@��@ά�@�:�@�u�@�8�@��@�H�@���@�v`@�Ĝ@�>B@��@�n/@��@�r�@�/�@�0U@�%�@�J@��D@���@�\)@�ѷ@�~�@�8�@���@�J#@���@��.@�qv@@��@��@�f�@���@���@�n�@���@���@��@�v�@��
@�"�@��$@�g8@���@�Mj@��5@��o@�.�@���@�Y�@�	l@���@�&�@��7@�Dg@�/�@��@��[@�tT@��@��@��f@��@��6@���@�(�@���@�8@�%F@��@���@�Z@��@��~@�(�@��r@�!�@���@���@�q@�ں@�|�@�!�@�E9@�@�ߤ@��@�<�@��}@���@�0U@�}�@�]�@�U�@�F�@��@��@�Q�@��@��Q@�+�@��@���@���@�`�@���@��t@���@��h@�|�@�:�@���@�W�@��S@�U�@��@���@�4n@�O@��@��z@�G�@�-w@�	l@��)@�bN@�{@�خ@���@���@�c@�o@���@�A�@�/�@��@���@�ϫ@�\�@�8�@��@��I@�p;@��t@�L�@��@��`@�ی@���@���@��O@�� @�V�@�e@��T@��=@�4�@��@�%@��/@��+@�4@��@���@�&�@�Ɇ@��.@�
�@��Q@���@�G�@�-w@��@�tT@��@�	@���@��@���@�g�@�V@��p@�q�@�@��6@���@�rG@��@���@�K^@��@��@�S�@��K@�1�@�J@��r@��@���@��2@��@�PH@�3�@��@���@��t@�dZ@���@�%�@��@��@���@�e�@�+@���@��!@�h�@���@��n@�s@�Z�@�q@��$@�u�@�)�@��)@��t@�x�@�2a@��H@�z�@�N�@���@�iD@�(@�҉@�)�@��6@���@��P@�c@�l�@�IR@�!�@���@���@�q@�\�@�@�@x@~�@}�@}��@}J�@|��@|H@{��@z�M@zl�@zO@y��@y�@x�j@x<�@w�P@wC@v҉@v��@u��@t_@t�@s��@s�q@sU�@r�@rJ@q��@p�@o|�@n��@nq�@n8�@m�o@m�z@m��@m�@mm]@l��@kb�@j�<@j�@i�7@i+�@h�p@hM@hFt@hFt@g�@gj�@gK�@g(@f��@fM�@e��@f�@e�d@dV�@c��@b҉@b�R@b��@a�T@a+�@`��@`m�@`~@_�k@_Mj@_�@_�@^kQ@]�)@]|@\��@\�@\]d@[�w@[33@Z�]@Z��@Z~�@Y`B@X��@X"h@W��@WH�@V�H@VkQ@V;�@V	@U��@U��@UQ�@T��@T��@T"h@S\)@Rxl@Q��@QVm@Poi@P@O�+@O�@O��@O��@O|�@OMj@O
=@Nc @Nu@M�'@M0�@L��@Lq@L>B@L�@K��@K�;@K� @K��@K��@J�@Iԕ@I��@I+�@Hc�@H/�@H�@G�@G��@G\)@G(@F�s@F�L@F@�@E��@Em]@D�P@D�v@D��@DĜ@D��@D_@C�@C�@@Cj�@CP�@CF�@C"�@B� @B?@BO@B{@A�#@A��@A��@A?}@A%F@@�P@@�?@@�I@@D�@?�6@?�f@?E9@?�@?�@>��@>�@>��@>+k@=��@=�M@=�@<�K@<��@<��@<�@<D�@;�&@;��@;Mj@;!-@:�@:��@:��@:L0@9��@9q@9+@9�@8��@8��@8Ĝ@8��@8��@8Xy@8b@7�@7�Q@7�Q@7��@7g�@7(@6�8@6��@6��@63�@5�^@5k�@52a@5�@4��@4�v@4��@4r�@4-�@3�+@3�Q@3��@3n/@3H�@3'�@3S@2��@2��@2u%@2Q@2@�@1�.@1��@1�@0��@0ѷ@0�9@0�D@0M@/��@/&@.��@.ȴ@.�r@.{@-��@-�3@-�7@-c�@,�@,��@,|�@,h�@,e�@,V�@,M@,*�@+��@+��@+X�@+"�@*��@*
�@)��@)�X@)[W@(ی@(��@(A�@(�@'�K@'n/@&��@&ff@&Q@&=q@%�^@%?}@%�@%@@%�@$��@$ѷ@$��@$Ft@#�0@#b�@#6z@#o@"�]@"�b@"�A@"R�@"?@"&�@"{@!��@!�@ j@�&@l�@/�@Y@�2@��@?@#:@�.@��@\�@@�[@z�@_@  @��@��@�	@�4@g�@4�@�@�\@W�@0U@�#@�@!�@��@�@��@��@��@�4@�o@~(@l"@1'@�;@�k@C�@�@��@kQ@;�@�j@��@�@��@u�@5�@q@ѷ@�@�z@��@l"@�0@a@,�@o@�@p;@YK@=q@0U@)�@�@�@�@�@�@�@�z@��@^�@J�@@�@�v@�)@�@�.@<�@	�@�q@o�@;d@&@�@�@��@��@�\@s�@^5@W�@R�@Q@L0@C�@8�@+k@��@�h@�@�@�@Ɇ@�@N�@(�@�;@��@��@{J@j�@]�@H�@6z@4�@4�@�@
��@
� @
~�@
n�@
i�@
ff@
V@
0U@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�YKA�W?A�Y�A�aHA�^A�U2A�Q�A�OBA�K�A�M�A�G�A�C-A�F?A�J�A�.�A��A��A��A��A�MA�7A�"�A�&�A�'A�)�A�$�A�Aͺ�A�ߤA�0UAʠ'A�3hAǇ_A�CA���A�7A�A�AĜA�͟AA�p�A���A�:A�poA�OBA���A�,�A���A�-wA��A�Q�A��A�*0A�SA���A��rA�H�A��\A��A�L0A�$�A��A�jA��A���A�0!A��=A�P�A��KA��A��A�n/A�p;A�J�A��A��
A��A��A�%�A���A���A��jA�=�A�y>A�"4A�AA��zA|a�Av@OAs��As|�ArTaAo#:Aj�EAeS�Ac{A^�AW�xAO=AM�3AL��AI~�AE��AC@OA>�A:�tA9�A9)_A8��A8Q�A7d�A6�[A5�A3ݘA2��A2�A1'�A/(A.�A/� A0�zA1y�A2VmA3��A3��A1�KA.zA,��A*��A*|A*?A)��A)K�A)I�A)!A(�QA(��A(ϫA(zA'�HA'e�A'CA&��A&u�A%�vA%�XA$�A#�"A#�hA#��A#�PA#�A!�mA!n/A �A�kA�)A�CAg8A_A��A2aAiDAc AzA��A��AVA��A \A$A�&A�'A8�A�mA�AA�A�A��A��A�vA%�Au�A($AߤA�A�=AL�A
=A
�6A
�AA
�A	��A	q�A�;A��A�tA�IA��A�A�9A��A�AAYKAH�A��A?}A��A�7Ak�AJ�AC-A�Ap�Ai�Ao�An/Al"AE9A��A�DA	AcAOA$A�NA��A iAS�A]dAxA �'A ��A ��A tTA J�@���@�j@�|�@��0@�<6@�C@��L@�j@�?�@��r@�8�@��@�U�@�p;@�>B@�>B@�2�@��j@�q@�S�@���@�u@�@�H�@��[@�	@�$t@�6@�z@�$@�^@�W?@��@�j�@�xl@�c�@�Z�@��@�҉@褩@脶@�1'@���@��@�\@�@�b�@�&�@�˒@�0@���@��@���@��
@�S�@�?}@��@ޑ @�_@�hs@܃�@�*�@�g�@�Ta@ٺ^@���@؈�@��A@ת�@�g�@�F@�33@��y@�j@Պ�@�<�@�ݘ@�1�@ҸR@�H@���@�n/@�n�@�p�@�@��@ά�@�:�@�u�@�8�@��@�H�@���@�v`@�Ĝ@�>B@��@�n/@��@�r�@�/�@�0U@�%�@�J@��D@���@�\)@�ѷ@�~�@�8�@���@�J#@���@��.@�qv@@��@��@�f�@���@���@�n�@���@���@��@�v�@��
@�"�@��$@�g8@���@�Mj@��5@��o@�.�@���@�Y�@�	l@���@�&�@��7@�Dg@�/�@��@��[@�tT@��@��@��f@��@��6@���@�(�@���@�8@�%F@��@���@�Z@��@��~@�(�@��r@�!�@���@���@�q@�ں@�|�@�!�@�E9@�@�ߤ@��@�<�@��}@���@�0U@�}�@�]�@�U�@�F�@��@��@�Q�@��@��Q@�+�@��@���@���@�`�@���@��t@���@��h@�|�@�:�@���@�W�@��S@�U�@��@���@�4n@�O@��@��z@�G�@�-w@�	l@��)@�bN@�{@�خ@���@���@�c@�o@���@�A�@�/�@��@���@�ϫ@�\�@�8�@��@��I@�p;@��t@�L�@��@��`@�ی@���@���@��O@�� @�V�@�e@��T@��=@�4�@��@�%@��/@��+@�4@��@���@�&�@�Ɇ@��.@�
�@��Q@���@�G�@�-w@��@�tT@��@�	@���@��@���@�g�@�V@��p@�q�@�@��6@���@�rG@��@���@�K^@��@��@�S�@��K@�1�@�J@��r@��@���@��2@��@�PH@�3�@��@���@��t@�dZ@���@�%�@��@��@���@�e�@�+@���@��!@�h�@���@��n@�s@�Z�@�q@��$@�u�@�)�@��)@��t@�x�@�2a@��H@�z�@�N�@���@�iD@�(@�҉@�)�@��6@���@��P@�c@�l�@�IR@�!�@���@���@�q@�\�@�@�@x@~�@}�@}��@}J�@|��@|H@{��@z�M@zl�@zO@y��@y�@x�j@x<�@w�P@wC@v҉@v��@u��@t_@t�@s��@s�q@sU�@r�@rJ@q��@p�@o|�@n��@nq�@n8�@m�o@m�z@m��@m�@mm]@l��@kb�@j�<@j�@i�7@i+�@h�p@hM@hFt@hFt@g�@gj�@gK�@g(@f��@fM�@e��@f�@e�d@dV�@c��@b҉@b�R@b��@a�T@a+�@`��@`m�@`~@_�k@_Mj@_�@_�@^kQ@]�)@]|@\��@\�@\]d@[�w@[33@Z�]@Z��@Z~�@Y`B@X��@X"h@W��@WH�@V�H@VkQ@V;�@V	@U��@U��@UQ�@T��@T��@T"h@S\)@Rxl@Q��@QVm@Poi@P@O�+@O�@O��@O��@O|�@OMj@O
=@Nc @Nu@M�'@M0�@L��@Lq@L>B@L�@K��@K�;@K� @K��@K��@J�@Iԕ@I��@I+�@Hc�@H/�@H�@G�@G��@G\)@G(@F�s@F�L@F@�@E��@Em]@D�P@D�v@D��@DĜ@D��@D_@C�@C�@@Cj�@CP�@CF�@C"�@B� @B?@BO@B{@A�#@A��@A��@A?}@A%F@@�P@@�?@@�I@@D�@?�6@?�f@?E9@?�@?�@>��@>�@>��@>+k@=��@=�M@=�@<�K@<��@<��@<�@<D�@;�&@;��@;Mj@;!-@:�@:��@:��@:L0@9��@9q@9+@9�@8��@8��@8Ĝ@8��@8��@8Xy@8b@7�@7�Q@7�Q@7��@7g�@7(@6�8@6��@6��@63�@5�^@5k�@52a@5�@4��@4�v@4��@4r�@4-�@3�+@3�Q@3��@3n/@3H�@3'�@3S@2��@2��@2u%@2Q@2@�@1�.@1��@1�@0��@0ѷ@0�9@0�D@0M@/��@/&@.��@.ȴ@.�r@.{@-��@-�3@-�7@-c�@,�@,��@,|�@,h�@,e�@,V�@,M@,*�@+��@+��@+X�@+"�@*��@*
�@)��@)�X@)[W@(ی@(��@(A�@(�@'�K@'n/@&��@&ff@&Q@&=q@%�^@%?}@%�@%@@%�@$��@$ѷ@$��@$Ft@#�0@#b�@#6z@#o@"�]@"�b@"�A@"R�@"?@"&�@"{@!��@!�@ j@�&@l�@/�@Y@�2@��@?@#:@�.@��@\�@@�[@z�@_@  @��@��@�	@�4@g�@4�@�@�\@W�@0U@�#@�@!�@��@�@��@��@��@�4@�o@~(@l"@1'@�;@�k@C�@�@��@kQ@;�@�j@��@�@��@u�@5�@q@ѷ@�@�z@��@l"@�0@a@,�@o@�@p;@YK@=q@0U@)�@�@�@�@�@�@�@�z@��@^�@J�@@�@�v@�)@�@�.@<�@	�@�q@o�@;d@&@�@�@��@��@�\@s�@^5@W�@R�@Q@L0@C�@8�@+k@��@�h@�@�@�@Ɇ@�@N�@(�@�;@��@��@{J@j�@]�@H�@6z@4�@4�@�@
��@
� @
~�@
n�@
i�@
ff@
V@
0U@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�`B	�B	�TB	�tB	��B	�DB	�JB	�6B	�B	�qB	��B	��B	��B
B

�B
0B
~B
�B
PB
B
B
�B
�B
�B
!HB
+�B
'�B
&�B
$tB
%FB
,=B
/B
4nB
9>B
F�B
f�B
�lB
��B
�OB
ҽB
�,BNB1�BB�BG�BM6BQ�BT�BW�Br�Bv�BxlB�bB��B��B��B��B��B�#B�=B�6BdB:DB,"BB
rB�B
�RB
��B
�1B
��B
ʦB
ƎB
��B
�B
ՁB
��B
�5B
��B
�DB
tTB
P�B
.�B
)�B
'RB
 vB	��B	ևB	�DB	�B	��B	��B	�B	qB	aHB	Q�B	>]B	B	�B	pB	
	B�>B�[B�
B�B�>B�B��B	B	�B	gB	gB	�B	'B		lB	�B	�B	*KB	b�B	��B	�'B	��B	��B	��B
�B	�B	�B	�oB	�B	��B	�VB
�B

=B
�B
�B
#TB
/5B
;dB
?cB
A�B
B�B
DgB
K�B
J�B
I�B
B�B
>BB
<�B
B�B
E�B
E�B
C�B
B�B
?�B
9�B
6�B
5%B
4B
1�B
0;B
.cB
(XB
7B
�B	�fB	�-B	�nB	�tB	��B	�B	�B	�B	�)B	�B	�B	��B	�QB	�B	�KB	�B	�B	�fB	��B	�B	�B	�B	�B	�nB	��B	�B	��B	�+B	��B	��B	�B	�cB	��B	�IB	�B	��B
	�B
mB
�B
{B
kB
eB
�B
B
�B
�B
�B
�B
)B
�B
 \B
!�B
#:B
#�B
#�B
#�B
 �B
B
�B
�B
uB
�B
�B
B
�B
�B
B
�B
B
_B
�B
VB
B
�B
�B
IB
�B
B
 vB
 vB
VB
�B
WB
+B
gB
�B
gB
�B
�B
B
B
9B
�B
gB
B
�B
[B
�B
 B
}B
�B
�B
�B
BB
�B
�B
�B
^B
	7B
�B
EB
�B
tB
9B
uB	�cB	��B	�"B	�B	�B	��B	�}B
 �B
 �B
 B	��B
 4B
 �B
�B
UB
 �B
 B
 iB
 �B
 B	�}B	��B	��B	��B	��B	�wB	�wB	�BB	�BB	�BB	�B	�}B	��B	�}B	��B	�B	��B	��B	��B	��B	��B	��B	�wB	��B	�BB	�(B	��B	�<B	��B	��B	�jB	��B	�JB	��B	�xB	��B	�^B	�xB	��B	�xB	�xB	�B	��B	��B	��B	�PB	�B	��B	�xB	��B	�0B	�^B	��B	�^B	�B	�B	�xB	�B	�B	�B	��B	��B	��B	�.B	�cB	��B
 �B
 �B
 4B
 4B
 B
 B
B
 �B
;B
�B
uB
�B
�B
�B
�B
B
�B
{B
�B
gB
3B
3B
gB
�B
9B
�B
�B
YB
�B
EB
KB
�B
	RB
	lB
	�B
	�B

=B

	B

�B

�B
�B
�B
�B
�B
dB
�B
�B
"B
�B
�B
�B
�B
�B
vB
�B
�B
�B
HB
�B
}B
bB
bB
}B
�B
�B
}B
bB
}B
}B
�B
�B
NB
�B
:B
B
oB
�B
�B
&B
[B
&B
B
[B
�B
�B
�B
�B
�B
�B
2B
gB
�B
MB
�B
gB
B
�B
9B
mB
9B
�B
?B
sB
sB
YB
?B
$B
YB
?B
�B
B
B
EB
�B
�B
�B
_B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
B
�B
 BB
 BB
 'B
 BB
!B
!|B
!�B
"B
"�B
"�B
#nB
$@B
$B
#�B
#�B
$B
$�B
%FB
%�B
%zB
%�B
%�B
&�B
&�B
'mB
(�B
(�B
)*B
)DB
)yB
)�B
*B
*B
*B
+QB
+�B
+�B
+�B
,"B
,�B
-B
-wB
-�B
-�B
.cB
.�B
/ B
/�B
/�B
0!B
0�B
0�B
0�B
1�B
1�B
1�B
2B
2B
2B
2-B
2-B
2GB
2|B
2aB
2|B
2�B
2�B
3B
3�B
3�B
3�B
49B
4�B
4�B
4�B
5ZB
5�B
6+B
7fB
8B
8RB
8�B
9>B
9�B
9�B
9�B
9�B
:B
:�B
;B
;0B
;�B
<�B
<�B
<�B
<�B
=qB
=�B
>B
>]B
>�B
>�B
>�B
?B
?B
?cB
?�B
@OB
A B
AoB
A�B
BB
B�B
CGB
C�B
D�B
D�B
E�B
E�B
E�B
E�B
FB
FtB
F�B
H1B
HfB
IB
H�B
H�B
IlB
I�B
J	B
J	B
J=B
J�B
J�B
J�B
J�B
K)B
K)B
KxB
K�B
K�B
K�B
LdB
L�B
L�B
L�B
L~B
M�B
M�B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
O�B
PB
P.B
PbB
P�B
P�B
QNB
R:B
R B
R�B
S&B
SuB
S[B
SuB
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T�B
T�B
T�B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
UgB
VB
V�B
V�B
WYB
W�B
XB
X�B
Y�B
Y�B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]IB
]IB
]/B
\�B
\�B
\�B
\�B
\�B
]�B
^5B
^5B
^5B
^�B
^�B
_B
_;B
_VB
_VB
_;B
_VB
_VB
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
bB
b4B
b4B
bhB
b�B
b�B
b�B
c B
c:B
cnB
c�B
cnB
c�B
c�B
dB
c�B
dB
d&B
dZB
d�B
d�B
d�B
d�B
e,B
eB
d�B
d�B
d�B
d�B
d�B
e�B
f�B
f�B
g8B
gRB
gmB
gmB
g�B
g�B
g�B
h
B
g�B
h>B
h�B
h�B
hsB
h�B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
kB
k�B
k6B
kQB
k�B
kkB
k�B
k�B
k�B
lB
lB
k�B
k�B
k�B
lB
l"B
l"B
k�B
k�B
l=B
lWB
l�B
mB
m)B
mB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
mwB
nB
ncB
ncB
ncB
ncB
ncB
n�B
n�B
o B
oiB
o�B
o�B
o�B
p!B
pUB
pUB
p�B
p�B
p�B
p�B
p�B
q�B
r-B
r�B
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
xRB
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
yXB
y�B
y�B
zDB
z^B
zxB
z�B
{JB
{JB
{JB
{JB
{�B
|B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~B
~BB
~B
~(B
~(B
~B
~(B
~(B
~(B
~(B
~(B
~(B
~BB
~wB
~�B
~�B
HB
}B
�B
� B
�B
�B
�4B
�OB
��B
��B
��B
� B
� B
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
��B
��B
��B
�B
�uB
��B
��B
��B
��B
��B
��B
��B
�GB
�aB
�{B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�B
�9B
�9B
�9B
�9B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�`B	�B	�TB	�tB	��B	�DB	�JB	�6B	�B	�qB	��B	��B	��B
B

�B
0B
~B
�B
PB
B
B
�B
�B
�B
!HB
+�B
'�B
&�B
$tB
%FB
,=B
/B
4nB
9>B
F�B
f�B
�lB
��B
�OB
ҽB
�,BNB1�BB�BG�BM6BQ�BT�BW�Br�Bv�BxlB�bB��B��B��B��B��B�#B�=B�6BdB:DB,"BB
rB�B
�RB
��B
�1B
��B
ʦB
ƎB
��B
�B
ՁB
��B
�5B
��B
�DB
tTB
P�B
.�B
)�B
'RB
 vB	��B	ևB	�DB	�B	��B	��B	�B	qB	aHB	Q�B	>]B	B	�B	pB	
	B�>B�[B�
B�B�>B�B��B	B	�B	gB	gB	�B	'B		lB	�B	�B	*KB	b�B	��B	�'B	��B	��B	��B
�B	�B	�B	�oB	�B	��B	�VB
�B

=B
�B
�B
#TB
/5B
;dB
?cB
A�B
B�B
DgB
K�B
J�B
I�B
B�B
>BB
<�B
B�B
E�B
E�B
C�B
B�B
?�B
9�B
6�B
5%B
4B
1�B
0;B
.cB
(XB
7B
�B	�fB	�-B	�nB	�tB	��B	�B	�B	�B	�)B	�B	�B	��B	�QB	�B	�KB	�B	�B	�fB	��B	�B	�B	�B	�B	�nB	��B	�B	��B	�+B	��B	��B	�B	�cB	��B	�IB	�B	��B
	�B
mB
�B
{B
kB
eB
�B
B
�B
�B
�B
�B
)B
�B
 \B
!�B
#:B
#�B
#�B
#�B
 �B
B
�B
�B
uB
�B
�B
B
�B
�B
B
�B
B
_B
�B
VB
B
�B
�B
IB
�B
B
 vB
 vB
VB
�B
WB
+B
gB
�B
gB
�B
�B
B
B
9B
�B
gB
B
�B
[B
�B
 B
}B
�B
�B
�B
BB
�B
�B
�B
^B
	7B
�B
EB
�B
tB
9B
uB	�cB	��B	�"B	�B	�B	��B	�}B
 �B
 �B
 B	��B
 4B
 �B
�B
UB
 �B
 B
 iB
 �B
 B	�}B	��B	��B	��B	��B	�wB	�wB	�BB	�BB	�BB	�B	�}B	��B	�}B	��B	�B	��B	��B	��B	��B	��B	��B	�wB	��B	�BB	�(B	��B	�<B	��B	��B	�jB	��B	�JB	��B	�xB	��B	�^B	�xB	��B	�xB	�xB	�B	��B	��B	��B	�PB	�B	��B	�xB	��B	�0B	�^B	��B	�^B	�B	�B	�xB	�B	�B	�B	��B	��B	��B	�.B	�cB	��B
 �B
 �B
 4B
 4B
 B
 B
B
 �B
;B
�B
uB
�B
�B
�B
�B
B
�B
{B
�B
gB
3B
3B
gB
�B
9B
�B
�B
YB
�B
EB
KB
�B
	RB
	lB
	�B
	�B

=B

	B

�B

�B
�B
�B
�B
�B
dB
�B
�B
"B
�B
�B
�B
�B
�B
vB
�B
�B
�B
HB
�B
}B
bB
bB
}B
�B
�B
}B
bB
}B
}B
�B
�B
NB
�B
:B
B
oB
�B
�B
&B
[B
&B
B
[B
�B
�B
�B
�B
�B
�B
2B
gB
�B
MB
�B
gB
B
�B
9B
mB
9B
�B
?B
sB
sB
YB
?B
$B
YB
?B
�B
B
B
EB
�B
�B
�B
_B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
B
B
�B
 BB
 BB
 'B
 BB
!B
!|B
!�B
"B
"�B
"�B
#nB
$@B
$B
#�B
#�B
$B
$�B
%FB
%�B
%zB
%�B
%�B
&�B
&�B
'mB
(�B
(�B
)*B
)DB
)yB
)�B
*B
*B
*B
+QB
+�B
+�B
+�B
,"B
,�B
-B
-wB
-�B
-�B
.cB
.�B
/ B
/�B
/�B
0!B
0�B
0�B
0�B
1�B
1�B
1�B
2B
2B
2B
2-B
2-B
2GB
2|B
2aB
2|B
2�B
2�B
3B
3�B
3�B
3�B
49B
4�B
4�B
4�B
5ZB
5�B
6+B
7fB
8B
8RB
8�B
9>B
9�B
9�B
9�B
9�B
:B
:�B
;B
;0B
;�B
<�B
<�B
<�B
<�B
=qB
=�B
>B
>]B
>�B
>�B
>�B
?B
?B
?cB
?�B
@OB
A B
AoB
A�B
BB
B�B
CGB
C�B
D�B
D�B
E�B
E�B
E�B
E�B
FB
FtB
F�B
H1B
HfB
IB
H�B
H�B
IlB
I�B
J	B
J	B
J=B
J�B
J�B
J�B
J�B
K)B
K)B
KxB
K�B
K�B
K�B
LdB
L�B
L�B
L�B
L~B
M�B
M�B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
O�B
PB
P.B
PbB
P�B
P�B
QNB
R:B
R B
R�B
S&B
SuB
S[B
SuB
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T�B
T�B
T�B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
UgB
VB
V�B
V�B
WYB
W�B
XB
X�B
Y�B
Y�B
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]IB
]IB
]/B
\�B
\�B
\�B
\�B
\�B
]�B
^5B
^5B
^5B
^�B
^�B
_B
_;B
_VB
_VB
_;B
_VB
_VB
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
bB
b4B
b4B
bhB
b�B
b�B
b�B
c B
c:B
cnB
c�B
cnB
c�B
c�B
dB
c�B
dB
d&B
dZB
d�B
d�B
d�B
d�B
e,B
eB
d�B
d�B
d�B
d�B
d�B
e�B
f�B
f�B
g8B
gRB
gmB
gmB
g�B
g�B
g�B
h
B
g�B
h>B
h�B
h�B
hsB
h�B
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
kB
k�B
k6B
kQB
k�B
kkB
k�B
k�B
k�B
lB
lB
k�B
k�B
k�B
lB
l"B
l"B
k�B
k�B
l=B
lWB
l�B
mB
m)B
mB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
mwB
nB
ncB
ncB
ncB
ncB
ncB
n�B
n�B
o B
oiB
o�B
o�B
o�B
p!B
pUB
pUB
p�B
p�B
p�B
p�B
p�B
q�B
r-B
r�B
shB
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
x8B
xRB
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
yXB
y�B
y�B
zDB
z^B
zxB
z�B
{JB
{JB
{JB
{JB
{�B
|B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~B
~BB
~B
~(B
~(B
~B
~(B
~(B
~(B
~(B
~(B
~(B
~BB
~wB
~�B
~�B
HB
}B
�B
� B
�B
�B
�4B
�OB
��B
��B
��B
� B
� B
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
��B
��B
��B
�B
�uB
��B
��B
��B
��B
��B
��B
��B
�GB
�aB
�{B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�B
�9B
�9B
�9B
�9B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105245  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192517  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192517                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042525  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042525  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                