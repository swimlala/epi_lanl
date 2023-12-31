CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:42:06Z creation;2022-06-04T17:42:06Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174206  20220610141504  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٳ�Ӡm1   @ٳ�q��@.�l�C���c>�+J1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BXffB_33Bh  Br  Bx  B~ffB���B���B���B�ffB���B�  B�33B�  B�33B�33B�ffB���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�3D�C3Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@y��@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�  A�  A�33B��B��B��B   B'��B/��B7��B?��BG��BO��BX  B^��Bg��Bq��Bw��B~  B���B���B�fgB�33B���B���B�  B���B�  B�  B�33B�fgB���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC  C�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCJ  CK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Ds4D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dy� Dy��Dzy�Dz��D{y�D{��D|y�D|��D}y�D}��D~y�D~��Dy�D��D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D¼�D���D�<�D�|�Dü�D���D�<�D�|�Dļ�D���D�<�D�|�Dż�D���D�<�D�|�DƼ�D���D�<�D�|�DǼ�D���D�<�D�|�Dȼ�D���D�<�D�|�Dɼ�D���D�<�D�|�Dʼ�D�  D�@ D�|�D˼�D���D�<�D�|�D̼�D���D�<�D�|�Dͼ�D���D�<�D�|�Dμ�D���D�<�D�|�Dϼ�D���D�<�D�|�Dм�D���D�<�D�|�DѼ�D���D�<�D�|�DҼ�D���D�<�D�|�DӼ�D���D�<�D�|�DԼ�D���D�<�D�|�Dռ�D���D�<�D�|�Dּ�D���D�<�D�|�D׼�D���D�<�D�|�Dؼ�D���D�<�D�|�Dټ�D���D�<�D�|�Dڼ�D���D�<�D�|�Dۼ�D���D�<�D�|�Dܼ�D���D�<�D�|�Dݼ�D���D�<�D�|�D�� D���D�<�D�|�D߼�D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D��D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�<�D�|�D���D�  D�<�D�|�D���D���D�<�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-wA�/A�.A�-�A�/�A�.IA�,A�,qA�,�A�,=A�,�A�-wA�.}A�.�A�/�A�/�A�/�A�0�A�2�A�49A�4�A�33A�33A�.�A�.A�/OA�.A�[�A�FtA�*0A���A�LdA��>A�0�A��A�1A��CA��A���A���A��NA��/A�JXA�m)A��A��A��	A�רA���A�N<A���A�}�A�IA� \A��!A�rGA�tA�X�A� �A�!bA~�A{�AvAsM�Ar�wAqL0Ap`�Ao�XAn��Ai�HAf�wAe�AbS�A`ݘA`"�A^�\A\�AZMjAV��AR��AM�!AJ�AH�AF�aAD�AA�A@�A@��A?�A?hsA>)�A:m�A7�A7�A7J#A7kQA7OvA7G�A5�}A2�A,��A%%A#�mA"�hA!�gA �*A��A%�A�A��A%FA��A�AO�Aa�A9XAA�A�A�fA�	Ab�A�Ap;A�A1�A
��A
d�A
c�A
!-A	��ASA
�A
9XA
}VA
ZA
rGA
l�A
iDA
]�A
A	�hA	��A�FA��A�AMAݘAU�A��A>�A6zA��AA�A�TA'�ArGA`BA͟A�BA��A�A��A(�A�A_A/�A�,A�SAS&AĜA�Ae�A;dA"hA�EA�+A'�AȴA��A�	Au�A)�A |�@�}�@�}�@�j@��}@�i�@�Ta@�N�@�
�@��d@���@��M@��@�˒@�{J@��e@�\�@��@��@���@��@�6z@�[�@�K�@��z@�@�@��@�$�@��&@�hs@�/@�`B@�@�e,@��@�_@�� @��"@�}V@��@�@��@�h@�u�@�GE@��@�k�@�z@��@�t�@�(�@�@��@�H�@��H@��@�n�@�?�@�1@�F@��@�tT@���@⤩@�F@�v�@��@�}�@�@�n�@�L�@�͟@މ�@�GE@݌~@�8@܁o@��m@�خ@ۺ^@�k�@ڕ�@�H@��)@ً�@���@�e�@�(�@��+@ח$@�!-@֋D@�G@ջ0@Ն�@�)_@ԫ6@��@ӿH@Ҭ@�,=@���@�n/@�33@���@��E@�PH@�ԕ@�zx@�+@�3�@ͶF@�qv@�j@�g�@�Dg@�l"@��m@�4@ʎ�@��@�s�@�7@�*0@ư�@�:�@ŝ�@� \@�z�@�_@�j�@º�@�@�	@���@�g�@��]@�Xy@���@��a@�W?@�A�@��@��F@�c@�Mj@�+@���@��@�{J@��@�u%@�~@���@�J#@��@���@�A�@��W@���@�e,@���@���@�i�@�?�@��Z@���@���@��@�ߤ@���@�GE@�(�@��Q@��@�t�@�n/@��@�� @�/�@��K@�'�@��@���@�z�@�/�@��d@���@��@�Z�@��;@�1�@���@��h@�[�@���@��'@��@���@�r�@�p;@�M@��+@���@�e,@�@��v@��.@�C�@���@��z@�$�@���@�U�@�%F@���@���@�=q@�  @��9@�^�@�Ɇ@�h�@�,=@�qv@�q@��@���@�c�@�1'@�_@���@���@�hs@�=@�ߤ@��+@�5?@�$@��)@��*@��@�M@��@��@��;@��^@���@���@��f@�(�@��"@��?@���@�=q@���@���@�e�@��8@��"@��s@��?@��o@��@���@���@���@�>�@��@��|@��p@��u@�l�@�?@��d@�e�@��]@�a|@�~@��@���@���@��f@�l�@��@��o@��@��w@���@�g�@�9�@�C@��@���@�{�@�!@��7@��j@���@��_@�z�@�:�@���@���@�Vm@�:�@��@��U@�h
@�)�@�@���@�}�@�<6@�;@��y@�ѷ@���@�h
@�:*@��@��)@��w@�a�@�&�@��@���@���@�;�@��r@��@���@��4@�6z@��@�ѷ@���@�� @�V@�Ft@��@���@��o@���@�a�@�+@��@��!@�H�@�~@���@���@�j�@�=�@��@�z@�M�@�?@�;�@�4n@�-@��@��@���@��q@��7@�rG@�9�@���@�ߤ@�Ĝ@��\@�M@�@��@�@~l�@~{@}�N@}X@}�@|q@|A�@|%�@|  @{�:@z�R@z_@y��@x�)@xu�@xbN@xI�@x�@w�Q@w� @w��@v��@v��@u�o@u��@uO�@u�@t%�@s��@sC�@r��@r{@qT�@p��@p7@o�@o�4@o_p@o8@n��@n��@nC�@m��@mN<@l�@l��@k�]@k��@kMj@j��@j&�@i�#@i@i�'@i�7@iB�@h��@g��@gK�@f�@fV@e��@e�j@e��@e�C@eA @d�p@dM@d*�@d  @c{J@b�M@b͟@bz@a�@am]@aDg@a�@`y>@_�}@_K�@^�X@^�x@^R�@^O@]�>@]�t@]�@]2a@\��@\D�@[o�@Zں@Zz@Z�@Y��@YVm@Xی@XD�@W��@W�[@W\)@V�H@V� @V6�@Uϫ@U�@U \@T�`@T��@TK^@S�g@S�@SS�@S;d@S
=@R�8@R�@R��@Q��@QN<@Q+�@P�f@P�[@P�z@Pg8@P7�@O�@O�[@O�{@O�@N��@N��@N��@N^5@N@�@N)�@M�N@M`B@L��@LFt@K��@KC�@K)_@KY@J��@J��@Ju%@I�d@Ic�@H��@H�j@H�Y@H2�@H�@G�&@G�w@G��@GP�@F�@F�L@FE�@FJ@Ec@Ea�@E+@D�j@D��@DXy@D2�@C��@C�;@C��@C��@CS�@C i@Bi�@A��@Ao @A7L@A@@��@@�?@@��@@	�@?H�@>��@>Z�@>
�@=�z@=w2@=T�@=2a@<�@<�z@<<�@;�g@;��@;{J@;8@:�@:��@:z@:YK@:1�@9��@8�D@8@7��@78@7�@6�y@6͟@6��@68�@6($@6{@5�C@5rG@4�|@4��@4�@4�z@4w�@3��@3n/@333@2��@2��@2�@2Ta@1�@1�-@1�@1F@1 \@1�@0�@0��@0_@/�@/��@/�P@/F�@.�@.�X@.{�@.4@-�@-}�@-�@,PH@+��@+��@+C�@+o@*��@*��@*@�@)��@)�@)�X@)w2@)<6@(��@("h@'��@'�f@'S�@&�@&�h@&n�@&6�@&_@%��@%A @$�@$1@#v`@#O@#�@"ȴ@"Z�@")�@!�@!@!��@!��@!J�@ �@ 9X@ �@��@t�@�"@�!@R�@��@�C@��@�@�@zx@|@m]@L�@7L@;@��@�@�Y@b@l�@�8@��@�m@�@{�@�@c@N<@q@�|@�v@��@u�@!@�@��@�q@�$@a@6z@�M@��@��@a|@1�@�D@�@��@��@�~@=�@�@�@�@��@�@ی@Ɇ@�z@j@M@C-@:�@�+@�@@&@�c@ȴ@�@��@��@d�@�@�Z@��@ϫ@��@0�@��@��@m�@]d@V�@Xy@K^@"h@�6@�@�P@~�@,�@�s@{�@3�@�)@�S@[W@X@G�@%F@%@�@�)@��@��@V�@K^@7�@  @�Q@�@�q@��@j�@S@
�R@
��@
��@
��@
�@
u%@
c @
�@	�j@	�@	T�@	J�@	Dg@	F@	Dg@	:�@	�@�@Q�@?�@'R@��@��@iD@'�@
=@��@�@�'@�1@v�@0U@�@��@�'@}�@IR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�-wA�/A�.A�-�A�/�A�.IA�,A�,qA�,�A�,=A�,�A�-wA�.}A�.�A�/�A�/�A�/�A�0�A�2�A�49A�4�A�33A�33A�.�A�.A�/OA�.A�[�A�FtA�*0A���A�LdA��>A�0�A��A�1A��CA��A���A���A��NA��/A�JXA�m)A��A��A��	A�רA���A�N<A���A�}�A�IA� \A��!A�rGA�tA�X�A� �A�!bA~�A{�AvAsM�Ar�wAqL0Ap`�Ao�XAn��Ai�HAf�wAe�AbS�A`ݘA`"�A^�\A\�AZMjAV��AR��AM�!AJ�AH�AF�aAD�AA�A@�A@��A?�A?hsA>)�A:m�A7�A7�A7J#A7kQA7OvA7G�A5�}A2�A,��A%%A#�mA"�hA!�gA �*A��A%�A�A��A%FA��A�AO�Aa�A9XAA�A�A�fA�	Ab�A�Ap;A�A1�A
��A
d�A
c�A
!-A	��ASA
�A
9XA
}VA
ZA
rGA
l�A
iDA
]�A
A	�hA	��A�FA��A�AMAݘAU�A��A>�A6zA��AA�A�TA'�ArGA`BA͟A�BA��A�A��A(�A�A_A/�A�,A�SAS&AĜA�Ae�A;dA"hA�EA�+A'�AȴA��A�	Au�A)�A |�@�}�@�}�@�j@��}@�i�@�Ta@�N�@�
�@��d@���@��M@��@�˒@�{J@��e@�\�@��@��@���@��@�6z@�[�@�K�@��z@�@�@��@�$�@��&@�hs@�/@�`B@�@�e,@��@�_@�� @��"@�}V@��@�@��@�h@�u�@�GE@��@�k�@�z@��@�t�@�(�@�@��@�H�@��H@��@�n�@�?�@�1@�F@��@�tT@���@⤩@�F@�v�@��@�}�@�@�n�@�L�@�͟@މ�@�GE@݌~@�8@܁o@��m@�خ@ۺ^@�k�@ڕ�@�H@��)@ً�@���@�e�@�(�@��+@ח$@�!-@֋D@�G@ջ0@Ն�@�)_@ԫ6@��@ӿH@Ҭ@�,=@���@�n/@�33@���@��E@�PH@�ԕ@�zx@�+@�3�@ͶF@�qv@�j@�g�@�Dg@�l"@��m@�4@ʎ�@��@�s�@�7@�*0@ư�@�:�@ŝ�@� \@�z�@�_@�j�@º�@�@�	@���@�g�@��]@�Xy@���@��a@�W?@�A�@��@��F@�c@�Mj@�+@���@��@�{J@��@�u%@�~@���@�J#@��@���@�A�@��W@���@�e,@���@���@�i�@�?�@��Z@���@���@��@�ߤ@���@�GE@�(�@��Q@��@�t�@�n/@��@�� @�/�@��K@�'�@��@���@�z�@�/�@��d@���@��@�Z�@��;@�1�@���@��h@�[�@���@��'@��@���@�r�@�p;@�M@��+@���@�e,@�@��v@��.@�C�@���@��z@�$�@���@�U�@�%F@���@���@�=q@�  @��9@�^�@�Ɇ@�h�@�,=@�qv@�q@��@���@�c�@�1'@�_@���@���@�hs@�=@�ߤ@��+@�5?@�$@��)@��*@��@�M@��@��@��;@��^@���@���@��f@�(�@��"@��?@���@�=q@���@���@�e�@��8@��"@��s@��?@��o@��@���@���@���@�>�@��@��|@��p@��u@�l�@�?@��d@�e�@��]@�a|@�~@��@���@���@��f@�l�@��@��o@��@��w@���@�g�@�9�@�C@��@���@�{�@�!@��7@��j@���@��_@�z�@�:�@���@���@�Vm@�:�@��@��U@�h
@�)�@�@���@�}�@�<6@�;@��y@�ѷ@���@�h
@�:*@��@��)@��w@�a�@�&�@��@���@���@�;�@��r@��@���@��4@�6z@��@�ѷ@���@�� @�V@�Ft@��@���@��o@���@�a�@�+@��@��!@�H�@�~@���@���@�j�@�=�@��@�z@�M�@�?@�;�@�4n@�-@��@��@���@��q@��7@�rG@�9�@���@�ߤ@�Ĝ@��\@�M@�@��@�@~l�@~{@}�N@}X@}�@|q@|A�@|%�@|  @{�:@z�R@z_@y��@x�)@xu�@xbN@xI�@x�@w�Q@w� @w��@v��@v��@u�o@u��@uO�@u�@t%�@s��@sC�@r��@r{@qT�@p��@p7@o�@o�4@o_p@o8@n��@n��@nC�@m��@mN<@l�@l��@k�]@k��@kMj@j��@j&�@i�#@i@i�'@i�7@iB�@h��@g��@gK�@f�@fV@e��@e�j@e��@e�C@eA @d�p@dM@d*�@d  @c{J@b�M@b͟@bz@a�@am]@aDg@a�@`y>@_�}@_K�@^�X@^�x@^R�@^O@]�>@]�t@]�@]2a@\��@\D�@[o�@Zں@Zz@Z�@Y��@YVm@Xی@XD�@W��@W�[@W\)@V�H@V� @V6�@Uϫ@U�@U \@T�`@T��@TK^@S�g@S�@SS�@S;d@S
=@R�8@R�@R��@Q��@QN<@Q+�@P�f@P�[@P�z@Pg8@P7�@O�@O�[@O�{@O�@N��@N��@N��@N^5@N@�@N)�@M�N@M`B@L��@LFt@K��@KC�@K)_@KY@J��@J��@Ju%@I�d@Ic�@H��@H�j@H�Y@H2�@H�@G�&@G�w@G��@GP�@F�@F�L@FE�@FJ@Ec@Ea�@E+@D�j@D��@DXy@D2�@C��@C�;@C��@C��@CS�@C i@Bi�@A��@Ao @A7L@A@@��@@�?@@��@@	�@?H�@>��@>Z�@>
�@=�z@=w2@=T�@=2a@<�@<�z@<<�@;�g@;��@;{J@;8@:�@:��@:z@:YK@:1�@9��@8�D@8@7��@78@7�@6�y@6͟@6��@68�@6($@6{@5�C@5rG@4�|@4��@4�@4�z@4w�@3��@3n/@333@2��@2��@2�@2Ta@1�@1�-@1�@1F@1 \@1�@0�@0��@0_@/�@/��@/�P@/F�@.�@.�X@.{�@.4@-�@-}�@-�@,PH@+��@+��@+C�@+o@*��@*��@*@�@)��@)�@)�X@)w2@)<6@(��@("h@'��@'�f@'S�@&�@&�h@&n�@&6�@&_@%��@%A @$�@$1@#v`@#O@#�@"ȴ@"Z�@")�@!�@!@!��@!��@!J�@ �@ 9X@ �@��@t�@�"@�!@R�@��@�C@��@�@�@zx@|@m]@L�@7L@;@��@�@�Y@b@l�@�8@��@�m@�@{�@�@c@N<@q@�|@�v@��@u�@!@�@��@�q@�$@a@6z@�M@��@��@a|@1�@�D@�@��@��@�~@=�@�@�@�@��@�@ی@Ɇ@�z@j@M@C-@:�@�+@�@@&@�c@ȴ@�@��@��@d�@�@�Z@��@ϫ@��@0�@��@��@m�@]d@V�@Xy@K^@"h@�6@�@�P@~�@,�@�s@{�@3�@�)@�S@[W@X@G�@%F@%@�@�)@��@��@V�@K^@7�@  @�Q@�@�q@��@j�@S@
�R@
��@
��@
��@
�@
u%@
c @
�@	�j@	�@	T�@	J�@	Dg@	F@	Dg@	:�@	�@�@Q�@?�@'R@��@��@iD@'�@
=@��@�@�'@�1@v�@0U@�@��@�'@}�@IR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BSuBRoBRoBRoBRoBR�BR�BRoBR�BR�BR�BR�BR�BRoBR�BR�BR�BR�BR�BR�BQ�BR�BR�BR�BR�BR�BR�Bg�BޞB	=�B	S�B	o�B	�QB	�B
.B
FtB
��B
�zB
��B
�hB
BeB{B
�jB
��B
�B
��B
��B
r�B
cnB
VSB
D3B
A;B
>�B
5�B
$B
�B
�B	�mB	՛B	�B	��B	�]B	�)B	�KB	��B	��B	�2B	�gB	~]B	p�B	j�B	jeB	f�B	b�B	Z�B	J�B	>�B	'B	B��BߤB�B��B�B�B�hB��B	�B	VB	'�B	SB	'B��B	-�B	[�B	^jB	^B	^B	\�B	8B�?B��B�&B�|B��B�eB�[B�}BԯB�yBܒB�BڠB�\B͹B��B�#B�YB��B��B�OB�*B��B��B�B��B�BňB�B�1B��B��B��B	�B	pB	�B	YB	�B	�B	�B	B	]B	�B	 B	&B	,�B	7�B	?HB	E�B	R�B	d�B	h
B	qAB	�aB	�~B	�{B	��B	�B	�tB	��B	��B	�.B	�}B	��B	�HB	�B	��B	��B	��B	�;B	��B	ªB	�aB	��B	��B	�lB	̳B	�B	ЗB	� B	��B	��B	�B	ңB	�TB	��B	ԯB	��B	��B	��B	�yB	چB	��B	��B	��B	�B	��B	޸B	�5B	�QB	ٚB	ۦB	߾B	�VB	ݘB	�)B	ٴB	�sB	�?B	��B	��B	�B	�QB	ۦB	ܬB	�|B	�2B	�_B	�B	��B	�eB	�KB	�B	�yB	�KB	�B	�6B	�QB	�B	�B	�B	�B	��B	�B	�eB	�0B	�0B	�WB	��B	�qB	�=B	�"B	�B	�6B	�KB	�B	�6B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�=B	��B	�qB	�wB	�IB	�/B	�}B	�B	�B	��B	�B	�B	��B	�-B	��B	�-B	�|B	�B	�vB	�vB	�B	�B	�-B	�B	��B	�B	��B	�B	�B	��B	��B	�B	�?B	�B	�+B	��B	��B	�B	��B	�B	�B	�B	��B	�RB	��B	��B	��B	��B	��B	�B	�-B	�B	��B	��B	�vB	�[B	�UB	��B	��B	��B	�`B	�B	��B	�fB	��B	��B	��B	�B	�B	�8B	�8B	�lB	�	B	��B	��B	�B	�dB	��B	��B	��B	�]B	��B	�.B	�cB	�HB
 B
 iB
 iB
 OB	��B	�}B	��B	��B	��B	��B
 OB
 �B
 iB
  B
 B
 iB
 �B
 �B
�B
�B
�B
�B
uB
�B
�B
�B
�B
B
�B
aB
�B
B
B
GB
-B
-B
�B
�B
�B
�B
AB
�B
�B
�B
�B
{B
�B
�B
GB
�B
B
�B
AB
 B
 4B	�cB	�B	�.B
 �B
�B
�B
;B
;B
 �B
 B
 �B
[B
[B
�B
{B
�B
�B
�B
�B
mB
�B
�B
	B
	�B

�B
�B
�B
<B
<B
�B
PB
PB
�B
�B
�B
�B
�B
B
B
�B
:B
�B
�B
@B
�B
�B
aB
2B
�B
mB
�B
�B
$B
YB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
]B
xB
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
;B
;B
!B
B
�B
�B
 �B
!|B
!|B
!bB
!bB
!�B
"hB
"hB
"�B
"hB
"�B
"�B
#nB
#�B
#�B
$@B
$ZB
%B
%FB
%`B
%zB
%�B
%�B
&B
&B
&fB
&fB
'B
'RB
'RB
'�B
'�B
(sB
(�B
(�B
(�B
)*B
)�B
*B
)�B
*B
*KB
*�B
*eB
*�B
*�B
*�B
+6B
+6B
+QB
+�B
+�B
,WB
,WB
,=B
,�B
-CB
-�B
/5B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1AB
1[B
1�B
1�B
1vB
1[B
1�B
1�B
2GB
2�B
3�B
4B
4nB
4�B
4�B
5B
4�B
6+B
6B
5�B
5�B
5�B
5�B
5B
4�B
5tB
5�B
5�B
5�B
6�B
7fB
7LB
7fB
8B
8RB
8�B
9>B
9XB
9>B
:B
9�B
:B
:xB
:xB
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
:�B
:�B
;0B
;B
;�B
;�B
<6B
<6B
<PB
=B
=<B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?}B
?�B
@�B
@�B
@�B
A B
@�B
AoB
AoB
A�B
A�B
A�B
BAB
B�B
B�B
B�B
C{B
C�B
C�B
C�B
D3B
D�B
D�B
EmB
EmB
E�B
E�B
FB
FB
E�B
FYB
FtB
F�B
GzB
G�B
G�B
H1B
HfB
H�B
IB
I�B
I�B
I�B
J=B
JXB
J�B
J�B
K)B
KDB
K�B
K�B
K�B
L0B
L~B
L�B
L�B
L�B
MB
MB
L�B
MB
N"B
NB
M�B
N<B
N<B
NVB
NVB
N�B
N�B
N�B
OB
O\B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
PHB
Q B
QB
Q�B
Q�B
QhB
Q�B
Q�B
Q�B
RTB
R�B
SB
S@B
S@B
S�B
S�B
S�B
S�B
S�B
TB
T{B
T�B
T�B
UB
U�B
U�B
U�B
VB
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W
B
WsB
XB
X_B
X�B
X+B
X�B
X�B
X�B
X�B
Y1B
YeB
ZB
ZQB
Z�B
Z�B
Z�B
[	B
[#B
[=B
[�B
[�B
\�B
]/B
\�B
\�B
]B
]~B
]�B
]�B
]�B
^5B
_VB
_!B
_�B
_�B
`BB
`'B
`BB
`BB
`�B
`�B
`�B
a-B
aB
abB
bNB
bB
a�B
b4B
b�B
cB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
dB
d&B
dB
c�B
dB
d@B
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
e�B
f2B
f�B
gB
g8B
gmB
g�B
h
B
h�B
h�B
i*B
iB
iB
iB
iyB
i�B
jKB
j�B
j�B
kB
k6B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n�B
n�B
n�B
oB
oB
n�B
oiB
o�B
p!B
pUB
pUB
p�B
q'B
qAB
q�B
q�B
r-B
rGB
raB
rGB
r|B
rGB
raB
r|B
r�B
r�B
r�B
r�B
sB
shB
tB
tnB
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
xB
xB
x8B
xlB
xlB
xlB
x�B
x�B
y	B
y$B
y$B
y>B
y>B
y$B
yXB
y>B
y�B
y�B
y�B
y�B
y�B
z*B
z^B
{B
{B
{JB
{B
{JB
{JB
{JB
{�B
{�B
{�B
{�B
|B
|�B
}"B
}VB
}qB
}qB
}qB
}qB
}�B
}�B
}�B
}�B
~B
}�B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
B
B
.B
.B
}B
�B
�B
�B
�4B
�B
�4B
�iB
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
�MB
�gB
�gB
��B
��B
��B
�B
�mB
�mB
�mB
��B
��B
��B
��B
�B
�YB
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BSuBRoBRoBRoBRoBR�BR�BRoBR�BR�BR�BR�BR�BRoBR�BR�BR�BR�BR�BR�BQ�BR�BR�BR�BR�BR�BR�Bg�BޞB	=�B	S�B	o�B	�QB	�B
.B
FtB
��B
�zB
��B
�hB
BeB{B
�jB
��B
�B
��B
��B
r�B
cnB
VSB
D3B
A;B
>�B
5�B
$B
�B
�B	�mB	՛B	�B	��B	�]B	�)B	�KB	��B	��B	�2B	�gB	~]B	p�B	j�B	jeB	f�B	b�B	Z�B	J�B	>�B	'B	B��BߤB�B��B�B�B�hB��B	�B	VB	'�B	SB	'B��B	-�B	[�B	^jB	^B	^B	\�B	8B�?B��B�&B�|B��B�eB�[B�}BԯB�yBܒB�BڠB�\B͹B��B�#B�YB��B��B�OB�*B��B��B�B��B�BňB�B�1B��B��B��B	�B	pB	�B	YB	�B	�B	�B	B	]B	�B	 B	&B	,�B	7�B	?HB	E�B	R�B	d�B	h
B	qAB	�aB	�~B	�{B	��B	�B	�tB	��B	��B	�.B	�}B	��B	�HB	�B	��B	��B	��B	�;B	��B	ªB	�aB	��B	��B	�lB	̳B	�B	ЗB	� B	��B	��B	�B	ңB	�TB	��B	ԯB	��B	��B	��B	�yB	چB	��B	��B	��B	�B	��B	޸B	�5B	�QB	ٚB	ۦB	߾B	�VB	ݘB	�)B	ٴB	�sB	�?B	��B	��B	�B	�QB	ۦB	ܬB	�|B	�2B	�_B	�B	��B	�eB	�KB	�B	�yB	�KB	�B	�6B	�QB	�B	�B	�B	�B	��B	�B	�eB	�0B	�0B	�WB	��B	�qB	�=B	�"B	�B	�6B	�KB	�B	�6B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�=B	��B	�qB	�wB	�IB	�/B	�}B	�B	�B	��B	�B	�B	��B	�-B	��B	�-B	�|B	�B	�vB	�vB	�B	�B	�-B	�B	��B	�B	��B	�B	�B	��B	��B	�B	�?B	�B	�+B	��B	��B	�B	��B	�B	�B	�B	��B	�RB	��B	��B	��B	��B	��B	�B	�-B	�B	��B	��B	�vB	�[B	�UB	��B	��B	��B	�`B	�B	��B	�fB	��B	��B	��B	�B	�B	�8B	�8B	�lB	�	B	��B	��B	�B	�dB	��B	��B	��B	�]B	��B	�.B	�cB	�HB
 B
 iB
 iB
 OB	��B	�}B	��B	��B	��B	��B
 OB
 �B
 iB
  B
 B
 iB
 �B
 �B
�B
�B
�B
�B
uB
�B
�B
�B
�B
B
�B
aB
�B
B
B
GB
-B
-B
�B
�B
�B
�B
AB
�B
�B
�B
�B
{B
�B
�B
GB
�B
B
�B
AB
 B
 4B	�cB	�B	�.B
 �B
�B
�B
;B
;B
 �B
 B
 �B
[B
[B
�B
{B
�B
�B
�B
�B
mB
�B
�B
	B
	�B

�B
�B
�B
<B
<B
�B
PB
PB
�B
�B
�B
�B
�B
B
B
�B
:B
�B
�B
@B
�B
�B
aB
2B
�B
mB
�B
�B
$B
YB
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
]B
xB
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
;B
;B
!B
B
�B
�B
 �B
!|B
!|B
!bB
!bB
!�B
"hB
"hB
"�B
"hB
"�B
"�B
#nB
#�B
#�B
$@B
$ZB
%B
%FB
%`B
%zB
%�B
%�B
&B
&B
&fB
&fB
'B
'RB
'RB
'�B
'�B
(sB
(�B
(�B
(�B
)*B
)�B
*B
)�B
*B
*KB
*�B
*eB
*�B
*�B
*�B
+6B
+6B
+QB
+�B
+�B
,WB
,WB
,=B
,�B
-CB
-�B
/5B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1AB
1[B
1�B
1�B
1vB
1[B
1�B
1�B
2GB
2�B
3�B
4B
4nB
4�B
4�B
5B
4�B
6+B
6B
5�B
5�B
5�B
5�B
5B
4�B
5tB
5�B
5�B
5�B
6�B
7fB
7LB
7fB
8B
8RB
8�B
9>B
9XB
9>B
:B
9�B
:B
:xB
:xB
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
:�B
:�B
;0B
;B
;�B
;�B
<6B
<6B
<PB
=B
=<B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?}B
?�B
@�B
@�B
@�B
A B
@�B
AoB
AoB
A�B
A�B
A�B
BAB
B�B
B�B
B�B
C{B
C�B
C�B
C�B
D3B
D�B
D�B
EmB
EmB
E�B
E�B
FB
FB
E�B
FYB
FtB
F�B
GzB
G�B
G�B
H1B
HfB
H�B
IB
I�B
I�B
I�B
J=B
JXB
J�B
J�B
K)B
KDB
K�B
K�B
K�B
L0B
L~B
L�B
L�B
L�B
MB
MB
L�B
MB
N"B
NB
M�B
N<B
N<B
NVB
NVB
N�B
N�B
N�B
OB
O\B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
PHB
Q B
QB
Q�B
Q�B
QhB
Q�B
Q�B
Q�B
RTB
R�B
SB
S@B
S@B
S�B
S�B
S�B
S�B
S�B
TB
T{B
T�B
T�B
UB
U�B
U�B
U�B
VB
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W
B
WsB
XB
X_B
X�B
X+B
X�B
X�B
X�B
X�B
Y1B
YeB
ZB
ZQB
Z�B
Z�B
Z�B
[	B
[#B
[=B
[�B
[�B
\�B
]/B
\�B
\�B
]B
]~B
]�B
]�B
]�B
^5B
_VB
_!B
_�B
_�B
`BB
`'B
`BB
`BB
`�B
`�B
`�B
a-B
aB
abB
bNB
bB
a�B
b4B
b�B
cB
b�B
b�B
b�B
b�B
cTB
c�B
c�B
dB
d&B
dB
c�B
dB
d@B
dtB
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
e�B
e�B
f2B
f�B
gB
g8B
gmB
g�B
h
B
h�B
h�B
i*B
iB
iB
iB
iyB
i�B
jKB
j�B
j�B
kB
k6B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n�B
n�B
n�B
oB
oB
n�B
oiB
o�B
p!B
pUB
pUB
p�B
q'B
qAB
q�B
q�B
r-B
rGB
raB
rGB
r|B
rGB
raB
r|B
r�B
r�B
r�B
r�B
sB
shB
tB
tnB
t�B
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
vzB
v�B
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
xB
xB
x8B
xlB
xlB
xlB
x�B
x�B
y	B
y$B
y$B
y>B
y>B
y$B
yXB
y>B
y�B
y�B
y�B
y�B
y�B
z*B
z^B
{B
{B
{JB
{B
{JB
{JB
{JB
{�B
{�B
{�B
{�B
|B
|�B
}"B
}VB
}qB
}qB
}qB
}qB
}�B
}�B
}�B
}�B
~B
}�B
~]B
~wB
~�B
~�B
~�B
~�B
~�B
B
B
.B
.B
}B
�B
�B
�B
�4B
�B
�4B
�iB
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
�MB
�gB
�gB
��B
��B
��B
�B
�mB
�mB
�mB
��B
��B
��B
��B
�B
�YB
��B
��B
��B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104928  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174206  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174206  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174206                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024214  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024214  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141504                      G�O�G�O�G�O�                