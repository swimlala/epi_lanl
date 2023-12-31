CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-28T15:43:57Z creation;2023-06-28T15:43:58Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230628154357  20230628162434  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�6_նlx1   @�6`D���@1AG�z��c;�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   AA��AY��A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�CL�C�fC�fC	�fC  C  C  C  C  C  C  C  C  C  C   C"33C$  C%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF  CH�CI�fCK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @34@y��@���A   AffA@  AX  A|��A�ffA�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B�  B�  B�  B���B���B���B���B���B���B���B���B���B���Bϙ�B���B�33B���B���B���B���B���B���B���B���B���B���C  C33C��C��C	��C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC"�C#�fC%��C'��C)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCB  CD  CE�fCH  CI��CK��CM��CO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DN  DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqs4Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D���D�<�D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D׀ D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�@ D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D޼�D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�9�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̻�A��EA��BA�خA���A��}A���A���A��A���A��A��8A��A��A��A��A��A��WA��WA��"A��"A��A���A��oA��A���A��A��MA���A���A��A��fA��TA��GA��TA��ZA���A��A���A��/A��fA��mA̿�A̱�ÂAA���A�7A��dA��A���A���A��A�@�A���A�(�A���A��[A�R�A���A���A�CA���A�A A�7�A�K)A�9$A��fA���A��*A�J�A��A���A�+�A�A�A��A��A�ÖA���A�Q�A�xlA��YA��8A���A�бA��A���A�h�A��fA�՛A�~A��-A�ȀA��A�O�A�TaA}-�A{��Az��A{	A{ \A{�Ax��As)�Al��Ah��Ad_A_7�A\H�AY�AW/AT�>AS�1ARZ�AO�AL�NAJ�HAI>�AF�AD�AA��A>+�A=i�A<+�A9�A8Y�A7M�A4�_A38�A0��A/�A/($A.�OA.I�A,�zA+�xA(�"A&�}A%��A$�A#GEA t�A��AdZAJ�Av�A�A;A��Av`A!AϫAJ�AJ#AH�A��A�AffA�'A��A��A6AĜAv�AOA��A��A
��A	P�A��A��AA�A  A�	A�WAV�A4A/�Ak�A��A|�A-A��A}VA2aAy>A�q@�x@��L@�-@��@@�Ɇ@�H@��A@��^@��@�)_@��"@� \@�^5@�Z�@�S�@�Ft@���@�3�@���@��j@���@�X�@�v�@��@�GE@���@�C-@�4@�:�@��@�0U@�'�@��@�X@��@��@���@�a�@���@��@���@��@�U�@�V@��X@��
@@�0U@���@���@���@�n/@�@@���@��'@��@�7�@��@�;@�_@�$�@�@��@��@��K@艠@�#:@�|�@��@��X@�Y@�~@���@垄@�9�@�@���@�@��P@�W�@�{@�8@�Ĝ@��>@ݾw@�֡@ܚ�@���@�v`@ڍ�@ڀ�@�kQ@���@���@׺^@�K^@�g�@ԣ@�@Ԁ�@��s@�6�@ӍP@҇�@�4@�9X@ϕ�@ρ@�a�@��"@Α�@ψf@�A @Ύ�@���@�X@���@��@�͟@̚@�9X@���@˥@�J�@���@ʲ�@�_@�G�@Ȟ�@�/�@ǝ�@��)@�V@�/�@û0@�J#@���@¦L@�;�@�� @�l�@��I@���@���@���@�|�@�4@�*0@��E@�a|@���@��h@�e,@�A�@���@��u@��@��=@�{J@��/@���@��@�_p@���@�R�@�K^@�(�@���@���@��~@�33@��]@�W�@�&�@�  @���@�u�@�,�@��@�c @�PH@��@��@�Vm@�
=@�ߤ@���@�S�@�?�@���@���@�w2@�S�@�!�@���@�$@���@��}@�C�@��@�l�@�� @��=@�P�@���@��9@�6@�� @�x@�5�@��@���@��@�~�@�($@��~@�8@�ں@��,@��@���@�dZ@��f@��@��U@�!@��3@��@�m�@�Z@��@�p�@��@��o@� i@��E@�w2@��o@���@���@��@��+@��]@��6@���@��"@�C�@���@�`�@�>B@�&�@��X@�[W@�@��/@�͟@��'@���@���@�i�@��@���@���@�O�@��@��@�Q�@��@���@��@��E@��L@�xl@�4n@�ԕ@�8�@�ں@���@���@�r�@�1@��a@�p�@�H�@�@���@�e�@�+k@��@���@���@�j@�~�@�a@�1�@�&@��@���@���@�1'@� �@���@���@�/@��,@�z@�Xy@�$�@���@�|�@�O�@�@O@�
=@���@�R�@�6�@�2�@�2�@���@�qv@�F@�C@��@��y@���@��F@�_�@�!�@���@��@��H@��~@��@�L�@�@��R@��D@�[�@��@��o@��g@��{@�A�@�@@���@���@��@���@�5?@��@�b@��@�j@��B@��M@�Ĝ@���@�u%@��M@���@��@�@�(�@��Z@��@���@��k@�o @�E9@�#�@��@��@�Ɇ@���@�N�@���@��[@��"@���@��A@�_�@���@���@��@�(�@�~@�x@��*@�b�@���@��h@��Z@���@���@�P�@�W�@�(�@�	@&@~�@~�1@~��@~� @~�F@(@g�@�F@t�@�@~��@~��@~~�@~R�@~B[@}�@|ی@|1'@{�6@{�K@{��@{a@{=@{��@{�0@{o�@z�2@ze@y��@yp�@y�@x��@x��@wo�@v~�@vO@t��@tq@sH�@r?@r_@q��@q��@r�@r��@s�@r�@r;�@q�3@q�~@q5�@qe,@o�m@n�@n�@m�@ma�@l��@l�@l�@m:�@l��@l]d@k��@j��@jJ�@i��@i�@h�@h~@g�g@g��@gZ�@f��@f͟@f�1@f3�@e�@e�h@eO�@e \@d�v@d��@d��@e#�@dC-@c��@c;d@b�R@a�>@a�#@a�H@ac@a�@`��@`1'@_�k@^�@^H�@^6�@^6�@^e@^�@]�d@]G�@\�[@\��@\7�@[X�@Z��@Z�@Y�9@Y|@X��@X]d@X"h@W��@W��@W1�@W i@Vں@WE9@W'�@W i@V��@Vp;@V �@UIR@T�@TA�@T7@S��@S��@SK�@R� @Rd�@R?@R�@Q�9@Q�'@Qf�@QDg@Q�@Q�@P��@P4n@O�6@O��@Oo�@OC�@O�@N�}@N��@Nv�@N\�@N�@M�@M��@MG�@L��@K�+@K˒@K�q@KW?@KS@J�@J�B@J�<@JZ�@I��@IQ�@H��@H�I@HC-@G�r@GK�@F�'@FM�@F?@FJ@E�j@E�z@E��@Ea�@E!�@D��@D�E@D��@DD�@C� @C=@Bz@A�N@A��@A�@AO�@A@@�E@@��@@2�@@@?�@?�w@?x@?K�@?�@>�6@>V@=��@=rG@=�@<l"@;�@;�:@;�@:�]@:�@:5?@9�D@9��@9��@9��@9�h@9u�@9^�@9!�@8��@8_@8<�@89X@81'@8%�@8%�@8�@8�@7�&@7�g@7�K@7��@7E9@7
=@6��@6
�@5�X@5\�@5�@4��@4q@4A�@4$@4!@4@3�@3��@3�@2�<@2��@2kQ@2#:@1��@1�d@1�=@1�~@1j@17L@1+@1�@0��@0�U@0�o@0<�@/�@/X�@.��@.��@.��@..�@-�@-�>@-�d@-�M@-%F@,�@,�$@,��@,M@,�@+��@+�6@+��@+!-@*ߤ@*��@*1�@)�9@)f�@)%F@(�5@(�.@'˒@'�	@'X�@'.I@'�@&��@&��@&z@&+k@%��@%��@%j@%!�@%�@$��@$x@#��@#��@#�[@#�[@#��@#�F@#�q@"��@"q�@!��@!��@!-w@ ��@ �.@ "h@ b@ �@�A@�w@�P@@�@��@��@��@��@kQ@��@�H@��@�@@�H@�-@�C@��@�h@\�@[W@S&@7L@@@�v@�@~(@m�@-�@��@�{@6z@�"@�!@c @3�@4@�Z@�@�@��@��@Y�@4@�@��@�@��@�u@e�@S�@�@�{@C@�'@n�@:*@O@{@J@�@�=@c@Y�@*0@�P@�v@��@�@:�@�@�@|�@,�@��@ȴ@��@�!@�F@YK@1�@�@��@��@\�@L�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̻�A��EA��BA�خA���A��}A���A���A��A���A��A��8A��A��A��A��A��A��WA��WA��"A��"A��A���A��oA��A���A��A��MA���A���A��A��fA��TA��GA��TA��ZA���A��A���A��/A��fA��mA̿�A̱�ÂAA���A�7A��dA��A���A���A��A�@�A���A�(�A���A��[A�R�A���A���A�CA���A�A A�7�A�K)A�9$A��fA���A��*A�J�A��A���A�+�A�A�A��A��A�ÖA���A�Q�A�xlA��YA��8A���A�бA��A���A�h�A��fA�՛A�~A��-A�ȀA��A�O�A�TaA}-�A{��Az��A{	A{ \A{�Ax��As)�Al��Ah��Ad_A_7�A\H�AY�AW/AT�>AS�1ARZ�AO�AL�NAJ�HAI>�AF�AD�AA��A>+�A=i�A<+�A9�A8Y�A7M�A4�_A38�A0��A/�A/($A.�OA.I�A,�zA+�xA(�"A&�}A%��A$�A#GEA t�A��AdZAJ�Av�A�A;A��Av`A!AϫAJ�AJ#AH�A��A�AffA�'A��A��A6AĜAv�AOA��A��A
��A	P�A��A��AA�A  A�	A�WAV�A4A/�Ak�A��A|�A-A��A}VA2aAy>A�q@�x@��L@�-@��@@�Ɇ@�H@��A@��^@��@�)_@��"@� \@�^5@�Z�@�S�@�Ft@���@�3�@���@��j@���@�X�@�v�@��@�GE@���@�C-@�4@�:�@��@�0U@�'�@��@�X@��@��@���@�a�@���@��@���@��@�U�@�V@��X@��
@@�0U@���@���@���@�n/@�@@���@��'@��@�7�@��@�;@�_@�$�@�@��@��@��K@艠@�#:@�|�@��@��X@�Y@�~@���@垄@�9�@�@���@�@��P@�W�@�{@�8@�Ĝ@��>@ݾw@�֡@ܚ�@���@�v`@ڍ�@ڀ�@�kQ@���@���@׺^@�K^@�g�@ԣ@�@Ԁ�@��s@�6�@ӍP@҇�@�4@�9X@ϕ�@ρ@�a�@��"@Α�@ψf@�A @Ύ�@���@�X@���@��@�͟@̚@�9X@���@˥@�J�@���@ʲ�@�_@�G�@Ȟ�@�/�@ǝ�@��)@�V@�/�@û0@�J#@���@¦L@�;�@�� @�l�@��I@���@���@���@�|�@�4@�*0@��E@�a|@���@��h@�e,@�A�@���@��u@��@��=@�{J@��/@���@��@�_p@���@�R�@�K^@�(�@���@���@��~@�33@��]@�W�@�&�@�  @���@�u�@�,�@��@�c @�PH@��@��@�Vm@�
=@�ߤ@���@�S�@�?�@���@���@�w2@�S�@�!�@���@�$@���@��}@�C�@��@�l�@�� @��=@�P�@���@��9@�6@�� @�x@�5�@��@���@��@�~�@�($@��~@�8@�ں@��,@��@���@�dZ@��f@��@��U@�!@��3@��@�m�@�Z@��@�p�@��@��o@� i@��E@�w2@��o@���@���@��@��+@��]@��6@���@��"@�C�@���@�`�@�>B@�&�@��X@�[W@�@��/@�͟@��'@���@���@�i�@��@���@���@�O�@��@��@�Q�@��@���@��@��E@��L@�xl@�4n@�ԕ@�8�@�ں@���@���@�r�@�1@��a@�p�@�H�@�@���@�e�@�+k@��@���@���@�j@�~�@�a@�1�@�&@��@���@���@�1'@� �@���@���@�/@��,@�z@�Xy@�$�@���@�|�@�O�@�@O@�
=@���@�R�@�6�@�2�@�2�@���@�qv@�F@�C@��@��y@���@��F@�_�@�!�@���@��@��H@��~@��@�L�@�@��R@��D@�[�@��@��o@��g@��{@�A�@�@@���@���@��@���@�5?@��@�b@��@�j@��B@��M@�Ĝ@���@�u%@��M@���@��@�@�(�@��Z@��@���@��k@�o @�E9@�#�@��@��@�Ɇ@���@�N�@���@��[@��"@���@��A@�_�@���@���@��@�(�@�~@�x@��*@�b�@���@��h@��Z@���@���@�P�@�W�@�(�@�	@&@~�@~�1@~��@~� @~�F@(@g�@�F@t�@�@~��@~��@~~�@~R�@~B[@}�@|ی@|1'@{�6@{�K@{��@{a@{=@{��@{�0@{o�@z�2@ze@y��@yp�@y�@x��@x��@wo�@v~�@vO@t��@tq@sH�@r?@r_@q��@q��@r�@r��@s�@r�@r;�@q�3@q�~@q5�@qe,@o�m@n�@n�@m�@ma�@l��@l�@l�@m:�@l��@l]d@k��@j��@jJ�@i��@i�@h�@h~@g�g@g��@gZ�@f��@f͟@f�1@f3�@e�@e�h@eO�@e \@d�v@d��@d��@e#�@dC-@c��@c;d@b�R@a�>@a�#@a�H@ac@a�@`��@`1'@_�k@^�@^H�@^6�@^6�@^e@^�@]�d@]G�@\�[@\��@\7�@[X�@Z��@Z�@Y�9@Y|@X��@X]d@X"h@W��@W��@W1�@W i@Vں@WE9@W'�@W i@V��@Vp;@V �@UIR@T�@TA�@T7@S��@S��@SK�@R� @Rd�@R?@R�@Q�9@Q�'@Qf�@QDg@Q�@Q�@P��@P4n@O�6@O��@Oo�@OC�@O�@N�}@N��@Nv�@N\�@N�@M�@M��@MG�@L��@K�+@K˒@K�q@KW?@KS@J�@J�B@J�<@JZ�@I��@IQ�@H��@H�I@HC-@G�r@GK�@F�'@FM�@F?@FJ@E�j@E�z@E��@Ea�@E!�@D��@D�E@D��@DD�@C� @C=@Bz@A�N@A��@A�@AO�@A@@�E@@��@@2�@@@?�@?�w@?x@?K�@?�@>�6@>V@=��@=rG@=�@<l"@;�@;�:@;�@:�]@:�@:5?@9�D@9��@9��@9��@9�h@9u�@9^�@9!�@8��@8_@8<�@89X@81'@8%�@8%�@8�@8�@7�&@7�g@7�K@7��@7E9@7
=@6��@6
�@5�X@5\�@5�@4��@4q@4A�@4$@4!@4@3�@3��@3�@2�<@2��@2kQ@2#:@1��@1�d@1�=@1�~@1j@17L@1+@1�@0��@0�U@0�o@0<�@/�@/X�@.��@.��@.��@..�@-�@-�>@-�d@-�M@-%F@,�@,�$@,��@,M@,�@+��@+�6@+��@+!-@*ߤ@*��@*1�@)�9@)f�@)%F@(�5@(�.@'˒@'�	@'X�@'.I@'�@&��@&��@&z@&+k@%��@%��@%j@%!�@%�@$��@$x@#��@#��@#�[@#�[@#��@#�F@#�q@"��@"q�@!��@!��@!-w@ ��@ �.@ "h@ b@ �@�A@�w@�P@@�@��@��@��@��@kQ@��@�H@��@�@@�H@�-@�C@��@�h@\�@[W@S&@7L@@@�v@�@~(@m�@-�@��@�{@6z@�"@�!@c @3�@4@�Z@�@�@��@��@Y�@4@�@��@�@��@�u@e�@S�@�@�{@C@�'@n�@:*@O@{@J@�@�=@c@Y�@*0@�P@�v@��@�@:�@�@�@|�@,�@��@ȴ@��@�!@�F@YK@1�@�@��@��@\�@L�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
"�B
"4B
"NB
"B
"4B
"NB
"4B
"hB
"�B
"hB
"hB
"�B
"hB
"4B
!�B
!bB
!�B
!B
!B
!-B
 �B
!-B
!|B
"B
!�B
 �B
 �B
!B
 �B
!-B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 'B
�B
�B
�B
dB
B
�B

B
�B	�*B	ЗB	��B	�B	��B
RoB
^�B
V�B
��B
�5B
��B
�B
�fB
=B{B�B�B�B�B�B�B
�B
�eB
�[B
՛B
�B
�B
��B
��B
�qB
�.B
�VB
�hB
�B
��B
��B
��B
b�B
@�B
*0B
�B
�B
B	��B	�hB	�\B	�FB	��B	�mB	�!B	��B	��B	��B	��B	�eB	�tB	�rB	g�B	P}B	72B	$tB	&B�B��B�B�B��B��B�B�~BچB�mB�BňB��B��B��B��B�FB�DB�$B�zB�tB�'B��B�OB��B��B�HB��B��B��B��B�FB�mB�1B�B��B��B��B�=B�HB��B�_B��B��B�fB��B�?B�B�CB�B�`B��B�DB��B�B	�B		�B	�B	B	aB�wB�>B�?B��B�B�B�nB�B��B		7B	�B	AB	 �B	UB	aB	�B	KB	�B	�B	�B	FB	QB	!�B	$B	$�B	'mB	(
B	+B	2�B	/OB	0�B	/iB	1B	3B	7�B	;JB	:*B	A�B	S[B	R�B	SB	S�B	V�B	V�B	e�B	pB	t�B	|jB	�B	�6B	��B	�gB	��B	�DB	�|B	��B	�qB	�}B	�CB	��B	��B	��B	�IB	�OB	�iB	��B	��B	�B	�oB	�AB	��B	��B	�fB	�B	��B	��B	��B	�%B	�B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	�uB	�aB	ňB	��B	��B	��B	�zB	�9B	ȴB	��B	�KB	�zB	ƨB	�B	�YB	�GB	�B	�B	��B	�BB	�B	��B	�(B	��B	��B	��B	�B	�-B	�vB	�[B	��B	��B	�B	��B	�2B	�3B	�B	��B	��B	��B	��B	�6B	�{B	ƎB	�B	�B	�zB	�RB	��B	�B	�VB	ЗB	҉B	�gB	�
B	��B	�B	�kB	�eB	خB	�EB	ּB	��B	�:B	�hB	�hB	��B	�B	�oB	ңB	�&B	ӏB	ԕB	��B	�EB	�_B	�_B	׍B	�B	ۦB	�=B	�QB	�qB	�)B	��B	��B	��B	�bB	�B	�B	�@B	��B	�mB	�>B	��B	�B	�_B	�yB	��B	�KB	�B	�B	�B	�cB	��B	�cB	� B	�B	�B	�OB	�B	��B	�;B	�oB	�vB	��B	�vB	�3B	�B	�9B	�B	��B	�zB	��B	��B	�LB	��B	�LB	�B	��B	��B	�B	��B	�B	��B	�VB	��B	��B
B
 �B
 �B
�B
�B
B
-B
�B
�B
�B
{B
9B
SB
B
9B
�B
MB
B
aB
uB
;B
B
oB
'B
'B
 �B	��B	�B	��B
gB
	�B
	�B
B
0B
dB
�B
B
�B
�B
�B
�B
JB
0B
JB
B
bB
bB
bB
B
B
�B
�B
&B
B
�B
�B
B
gB
gB
�B
�B
�B
B
B
mB
$B
�B
�B
�B
�B
�B
�B
�B
eB
B
�B
�B
�B
�B
�B
�B
�B
kB
�B
B
�B
�B
CB
IB
�B
�B
B
OB
OB
�B
;B
�B
�B
�B
 B
 �B
!HB
!�B
!-B
!|B
"4B
"B
!-B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
"hB
# B
$�B
%zB
&B
%�B
%�B
%�B
%B
$�B
$tB
$�B
$�B
$�B
%FB
%�B
%�B
&B
&B
%�B
%�B
&B
&�B
(
B
(�B
*�B
-�B
.�B
.cB
-�B
-]B
+�B
($B
*0B
,�B
3hB
5ZB
5?B
6�B
6�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
8RB
7�B
7�B
6�B
6FB
6`B
6`B
6`B
4�B
7fB
5�B
2|B
2-B
1�B
0�B
1�B
3�B
5tB
6�B
5�B
5?B
3�B
2GB
2|B
1vB
1B
1�B
3�B
3�B
6zB
9$B
;dB
=B
=�B
=�B
="B
>(B
?cB
?}B
?cB
?�B
>�B
=�B
=�B
>(B
>�B
>BB
>]B
?�B
A�B
A�B
AoB
AB
A;B
BB
A�B
B[B
B[B
A�B
@OB
@�B
?�B
?�B
=�B
<�B
<�B
=B
=qB
=�B
@�B
BuB
BB
A�B
BAB
B�B
B�B
C�B
BB
@�B
@�B
@ B
?�B
@OB
@�B
A�B
E�B
F�B
E�B
E9B
D�B
D3B
C�B
D3B
C�B
EB
EB
EB
EB
E�B
FB
F�B
F�B
G_B
GEB
G_B
G�B
G�B
HB
H�B
LJB
LdB
K�B
KDB
KDB
K�B
LB
M�B
N<B
M�B
MPB
L�B
L�B
LdB
L0B
LB
LB
LJB
LdB
L~B
MB
L�B
L~B
K�B
J�B
J�B
KxB
KxB
KxB
K�B
K�B
LB
L0B
L�B
L�B
MB
M�B
PbB
P�B
Q�B
QhB
R:B
R B
Q�B
QNB
P�B
PbB
P}B
PbB
P�B
PHB
P.B
P.B
P}B
P�B
P�B
Q B
P�B
Q B
P�B
Q�B
Q�B
RoB
R:B
RTB
R�B
R�B
S[B
S�B
S�B
S�B
S�B
T,B
TB
T{B
TFB
S[B
R�B
S&B
T,B
TaB
T{B
UgB
VmB
V�B
V�B
V�B
V�B
V�B
W
B
W$B
W�B
W�B
W�B
W�B
XEB
X_B
XyB
X�B
YKB
Z�B
ZQB
Z�B
[	B
[WB
[�B
\CB
\�B
]~B
]~B
]�B
]�B
]�B
^B
^B
^OB
^jB
^jB
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`'B
`BB
`�B
a-B
a�B
b4B
bhB
b�B
cB
c B
cTB
cTB
cnB
c�B
c�B
c�B
c�B
dB
d@B
dZB
d@B
dtB
dtB
d@B
dtB
dtB
dZB
d�B
dZB
d�B
d�B
d�B
e,B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gB
gRB
g�B
h
B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
iB
i*B
i*B
iDB
i_B
iDB
iyB
i�B
i�B
j�B
j�B
j�B
j�B
kQB
kkB
k6B
k�B
k�B
lB
l=B
lWB
lqB
l�B
l�B
l�B
mB
mB
m�B
m�B
nB
ncB
n�B
o B
oB
oOB
o�B
pUB
poB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
r-B
rGB
raB
r�B
sMB
shB
s�B
s�B
s�B
s�B
shB
s3B
t9B
tTB
t�B
uB
u�B
u�B
u�B
v�B
v�B
vzB
v�B
v�B
v�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
x�B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
z^B
zxB
z�B
z�B
{dB
{B
{�B
{�B
|B
{�B
{�B
{�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
~B
~wB
~�B
.B
.B
cB
cB
HB
�B
�B
�B
�B
�4B
�OB
�iB
�iB
��B
�;B
�;B
�oB
��B
�B
�AB
�AB
�[B
�uB
�[B
��B
��B
�B
�aB
�{B
��B
��B
��B
�g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
"�B
"4B
"NB
"B
"4B
"NB
"4B
"hB
"�B
"hB
"hB
"�B
"hB
"4B
!�B
!bB
!�B
!B
!B
!-B
 �B
!-B
!|B
"B
!�B
 �B
 �B
!B
 �B
!-B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 'B
�B
�B
�B
dB
B
�B

B
�B	�*B	ЗB	��B	�B	��B
RoB
^�B
V�B
��B
�5B
��B
�B
�fB
=B{B�B�B�B�B�B�B
�B
�eB
�[B
՛B
�B
�B
��B
��B
�qB
�.B
�VB
�hB
�B
��B
��B
��B
b�B
@�B
*0B
�B
�B
B	��B	�hB	�\B	�FB	��B	�mB	�!B	��B	��B	��B	��B	�eB	�tB	�rB	g�B	P}B	72B	$tB	&B�B��B�B�B��B��B�B�~BچB�mB�BňB��B��B��B��B�FB�DB�$B�zB�tB�'B��B�OB��B��B�HB��B��B��B��B�FB�mB�1B�B��B��B��B�=B�HB��B�_B��B��B�fB��B�?B�B�CB�B�`B��B�DB��B�B	�B		�B	�B	B	aB�wB�>B�?B��B�B�B�nB�B��B		7B	�B	AB	 �B	UB	aB	�B	KB	�B	�B	�B	FB	QB	!�B	$B	$�B	'mB	(
B	+B	2�B	/OB	0�B	/iB	1B	3B	7�B	;JB	:*B	A�B	S[B	R�B	SB	S�B	V�B	V�B	e�B	pB	t�B	|jB	�B	�6B	��B	�gB	��B	�DB	�|B	��B	�qB	�}B	�CB	��B	��B	��B	�IB	�OB	�iB	��B	��B	�B	�oB	�AB	��B	��B	�fB	�B	��B	��B	��B	�%B	�B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	�uB	�aB	ňB	��B	��B	��B	�zB	�9B	ȴB	��B	�KB	�zB	ƨB	�B	�YB	�GB	�B	�B	��B	�BB	�B	��B	�(B	��B	��B	��B	�B	�-B	�vB	�[B	��B	��B	�B	��B	�2B	�3B	�B	��B	��B	��B	��B	�6B	�{B	ƎB	�B	�B	�zB	�RB	��B	�B	�VB	ЗB	҉B	�gB	�
B	��B	�B	�kB	�eB	خB	�EB	ּB	��B	�:B	�hB	�hB	��B	�B	�oB	ңB	�&B	ӏB	ԕB	��B	�EB	�_B	�_B	׍B	�B	ۦB	�=B	�QB	�qB	�)B	��B	��B	��B	�bB	�B	�B	�@B	��B	�mB	�>B	��B	�B	�_B	�yB	��B	�KB	�B	�B	�B	�cB	��B	�cB	� B	�B	�B	�OB	�B	��B	�;B	�oB	�vB	��B	�vB	�3B	�B	�9B	�B	��B	�zB	��B	��B	�LB	��B	�LB	�B	��B	��B	�B	��B	�B	��B	�VB	��B	��B
B
 �B
 �B
�B
�B
B
-B
�B
�B
�B
{B
9B
SB
B
9B
�B
MB
B
aB
uB
;B
B
oB
'B
'B
 �B	��B	�B	��B
gB
	�B
	�B
B
0B
dB
�B
B
�B
�B
�B
�B
JB
0B
JB
B
bB
bB
bB
B
B
�B
�B
&B
B
�B
�B
B
gB
gB
�B
�B
�B
B
B
mB
$B
�B
�B
�B
�B
�B
�B
�B
eB
B
�B
�B
�B
�B
�B
�B
�B
kB
�B
B
�B
�B
CB
IB
�B
�B
B
OB
OB
�B
;B
�B
�B
�B
 B
 �B
!HB
!�B
!-B
!|B
"4B
"B
!-B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
"hB
# B
$�B
%zB
&B
%�B
%�B
%�B
%B
$�B
$tB
$�B
$�B
$�B
%FB
%�B
%�B
&B
&B
%�B
%�B
&B
&�B
(
B
(�B
*�B
-�B
.�B
.cB
-�B
-]B
+�B
($B
*0B
,�B
3hB
5ZB
5?B
6�B
6�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
8RB
7�B
7�B
6�B
6FB
6`B
6`B
6`B
4�B
7fB
5�B
2|B
2-B
1�B
0�B
1�B
3�B
5tB
6�B
5�B
5?B
3�B
2GB
2|B
1vB
1B
1�B
3�B
3�B
6zB
9$B
;dB
=B
=�B
=�B
="B
>(B
?cB
?}B
?cB
?�B
>�B
=�B
=�B
>(B
>�B
>BB
>]B
?�B
A�B
A�B
AoB
AB
A;B
BB
A�B
B[B
B[B
A�B
@OB
@�B
?�B
?�B
=�B
<�B
<�B
=B
=qB
=�B
@�B
BuB
BB
A�B
BAB
B�B
B�B
C�B
BB
@�B
@�B
@ B
?�B
@OB
@�B
A�B
E�B
F�B
E�B
E9B
D�B
D3B
C�B
D3B
C�B
EB
EB
EB
EB
E�B
FB
F�B
F�B
G_B
GEB
G_B
G�B
G�B
HB
H�B
LJB
LdB
K�B
KDB
KDB
K�B
LB
M�B
N<B
M�B
MPB
L�B
L�B
LdB
L0B
LB
LB
LJB
LdB
L~B
MB
L�B
L~B
K�B
J�B
J�B
KxB
KxB
KxB
K�B
K�B
LB
L0B
L�B
L�B
MB
M�B
PbB
P�B
Q�B
QhB
R:B
R B
Q�B
QNB
P�B
PbB
P}B
PbB
P�B
PHB
P.B
P.B
P}B
P�B
P�B
Q B
P�B
Q B
P�B
Q�B
Q�B
RoB
R:B
RTB
R�B
R�B
S[B
S�B
S�B
S�B
S�B
T,B
TB
T{B
TFB
S[B
R�B
S&B
T,B
TaB
T{B
UgB
VmB
V�B
V�B
V�B
V�B
V�B
W
B
W$B
W�B
W�B
W�B
W�B
XEB
X_B
XyB
X�B
YKB
Z�B
ZQB
Z�B
[	B
[WB
[�B
\CB
\�B
]~B
]~B
]�B
]�B
]�B
^B
^B
^OB
^jB
^jB
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`'B
`BB
`�B
a-B
a�B
b4B
bhB
b�B
cB
c B
cTB
cTB
cnB
c�B
c�B
c�B
c�B
dB
d@B
dZB
d@B
dtB
dtB
d@B
dtB
dtB
dZB
d�B
dZB
d�B
d�B
d�B
e,B
e�B
fLB
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gB
gRB
g�B
h
B
h
B
h>B
hsB
h�B
h�B
h�B
h�B
iB
i*B
i*B
iDB
i_B
iDB
iyB
i�B
i�B
j�B
j�B
j�B
j�B
kQB
kkB
k6B
k�B
k�B
lB
l=B
lWB
lqB
l�B
l�B
l�B
mB
mB
m�B
m�B
nB
ncB
n�B
o B
oB
oOB
o�B
pUB
poB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
q�B
r-B
rGB
raB
r�B
sMB
shB
s�B
s�B
s�B
s�B
shB
s3B
t9B
tTB
t�B
uB
u�B
u�B
u�B
v�B
v�B
vzB
v�B
v�B
v�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
x�B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
z^B
zxB
z�B
z�B
{dB
{B
{�B
{�B
|B
{�B
{�B
{�B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
~B
~wB
~�B
.B
.B
cB
cB
HB
�B
�B
�B
�B
�4B
�OB
�iB
�iB
��B
�;B
�;B
�oB
��B
�B
�AB
�AB
�[B
�uB
�[B
��B
��B
�B
�aB
�{B
��B
��B
��B
�g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230628154345  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230628154357  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230628154358  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230628154358                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230628154359  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230628154359  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230628162434                      G�O�G�O�G�O�                