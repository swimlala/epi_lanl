CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-25T00:43:23Z creation;2022-09-25T00:43:24Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220925004323  20220925010427  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��5>F�1   @��5�ʆB@/+I�^�c�KƧ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B�  B�  B�  B�  B�  B�  B�33Bϙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  CL�C�fC�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.33C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`33Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@o\)@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bo\)BwB~��B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��GB��GB�{B�z�B�z�B�z�B�z�B�z�B�z�BˮB�{B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC
>C��C��C	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�C-�C/�qC1��C3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY��C[�qC]�qC_�Ca�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C��C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz�\D{o\D{�\D|o\D|�\D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�4{D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�w�Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǺ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̷�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��]A��A��3Aٶ�A٫A٥�Aّ4A�u%A�QA�YA��fA���A���A���A��]A��KA��mA���A��EA��2A��}A���A�ȀA��#A��A��KAؿHAج�A؄�A�  A׆YA�0�A���A�F�AՒoA�(A���AԹ$A��>AѾ�A��HA�?�A��KAȷ�AƢ�A��fA�%FA���A��}A�d�A��dA���A��A�?�A��pA�	A�3hA���A�A�zDA���A���A���A��A��A��}A�C�A�JA�%zA�bA�<jA�.A��}A��AA�ԕA���A��BA��A��A�A���A�&�A���A�@�A�PHA���A��YA�TA|7Av�.ArqAm�KAl��AiZ�Ag��Ag]�Af�`Ae�=Ad(Aa�A]jAYw2AW�AT�AR$AM�AJ��AG,�AF&AC��A@�7A=�'A=OA;�A7��A4��A3��A2�6A0�6A/g8A.\)A-��A-'�A,f�A+��A*�FA)�=A)�=A)�7A(�]A(m�A(#�A'y�A'  A&�aA&�A&OA%�A%��A%B�A$��A$F�A$>BA$4�A$%�A#��A#}�A"�KA"��A"E�A"3�A" �A"A!�A!�A VA��A|�A)_A��A@A�9AU2A*�A��AHA�KAs�A�AjA�\A}VA_A��A�A4A�nA]dA(AdZA��A�1A�,A)_Af�A��Au�A�6Ac�AB[A
�5A
\�A
)�A	zA	5�A�)A�	A�A�A��A�Aj�A6�A��AeA��Am�A��A��A�rAFtAqA��As�AA7�AYA �A �NA �@��D@�� @���@��D@�y�@��f@�a|@�~@���@�O�@��B@�j@�E�@��F@�0�@�j@���@�33@���@�{@�ݘ@��"@�9�@��c@�J�@��A@��@�C�@���@�	l@��@��@�1�@�4@�O@�\�@�ƨ@��@�i�@���@脶@��@独@�@@�@�C-@�*@���@�oi@�/�@��.@�A @���@�b@��@�@�  @���@ߠ�@ޞ�@�Z�@�ں@�q�@۾w@�S�@��K@�xl@�7@��@�q@׎"@�'�@��@־@֌�@�tT@�M�@�"h@���@�&�@��@�@Ӥ@@�zx@�Z�@�
=@��@Ҥ�@�b@��a@ы�@���@�y>@�L0@��@Ϫ�@Ο�@�;�@��@��r@���@�f�@̹�@ˣn@�hs@ʸR@�x�@�1�@Ț�@�g�@�1@���@ĺ�@ı�@ĳh@�I�@���@�@�{J@�ߤ@�q@�.�@��=@��@���@�"h@��@�ƨ@���@�1@���@�a�@�B�@���@���@��{@�<6@���@�+k@��@��@���@�0�@���@���@�ff@�
�@��K@��V@��"@��M@�A @���@�&�@��a@���@�x�@�RT@�8@��@���@�a|@��@�Mj@��@��@�U2@�4n@���@�`B@��@��r@�kQ@�6�@��@@���@���@�M@�R�@�N�@��@�(�@���@�V�@���@�\)@�5�@�%F@�q@�ȴ@�xl@�:*@�	@��j@���@�ϫ@���@�^�@�T�@�Dg@��@��@���@��@��	@��9@�xl@��@���@�v`@��M@��@���@�PH@�7@���@��z@���@�=�@�33@�*0@�֡@��F@�j@��@��"@���@�:�@��B@���@�h
@�,=@��0@�IR@��@��@��e@�1�@���@�U�@�ی@���@�YK@��@���@�<6@��5@���@�1'@��n@�(�@�@�U2@��m@��@�33@�4@�N<@�Y@�C-@�_@�v`@�&�@��@��z@��@�]�@��@��L@�Ta@�J�@�:�@�-@�
�@���@�hs@�K�@���@�^5@�-@�~@��T@���@��@��@�J�@��P@�m�@�G@���@��=@�{J@�x@�S�@�O�@�rG@�K�@��E@�e@�_@� �@��+@���@���@�v`@�[W@�'�@��f@��@��m@��'@���@��6@�� @��@��L@�<�@�@��@�J�@��@��f@��@���@���@�kQ@�6�@���@�ƨ@��C@��P@�j�@�F�@�:�@�,�@��@��@�
=@��@��@��r@��@��@��[@���@�e,@�.I@�
=@���@��e@�q�@�[�@�Ft@�1'@��@��@���@��-@�y�@�e�@�J#@��@��v@��@���@�9X@��W@��"@�C�@�2a@���@��@�s�@�h
@�^5@�N�@��@_p@33@~��@~��@~��@~Ta@}IR@}�@};@|��@{�Q@{]�@z�@yk�@y&�@x�)@xtT@w�@w��@w4�@v�h@u�D@u��@ux�@t��@t!@s�w@s.I@r�}@r?@q�Z@q+�@p�@p�[@p�Y@p@oA�@n�y@nL0@m�o@m��@mu�@l��@l>B@k��@k�@kv`@kj�@kX�@j�@i��@h��@h~@gs@g�@go@f0U@ec@e�@d��@d_@d*�@c��@c�@c�4@b�@bkQ@b�@a?}@`�@`e�@`	�@_1�@^�]@^{�@]e,@\�K@\��@\�D@\Z@\7�@[�Q@[�:@Z��@ZB[@Y�@Y��@Y��@X��@X�@V�M@V�r@Ve@U��@U��@U�@T�@TXy@TA�@T>B@T4n@S�f@S
=@R�@R�]@R��@R��@R�@Q�@P��@P1'@O�@@N�@N1�@M�X@L��@LI�@K��@K=@Jߤ@J$�@I�@H��@H��@H!@G�W@G�w@G��@G�@@G"�@F��@Fp;@F@�@E�@E|@EB�@E-w@E/@E5�@E�@D4n@C��@C�$@CW?@Co@B�b@B5?@A�)@A��@A��@A�@A�@Af�@A�@A�@A;@@�@@�5@@�j@@�.@@��@@��@@z�@@6@@b@?�Q@?b�@?�@>�@>E�@>_@=�@=�@=x�@=�@=;@<��@<�@<-�@;�@;/�@;�@:�@:�B@:�}@:��@:H�@9�t@98�@9@8�f@8�p@8�e@87@7S�@6��@6M�@6�@5�>@5@57L@4�@42�@3˒@3\)@3o@2�]@2�+@2)�@2J@1�T@1c�@1Q�@1G�@12a@0@.��@.��@.�h@.��@.��@.Q@-��@-Dg@,�@,�D@,Ft@+��@+��@+��@+�4@+t�@++@*�\@)�"@)2a@)�@(PH@'�}@'��@'e�@''�@'$t@'�@&��@&�X@&�@&��@&~�@&Ov@&.�@&�@%�@%�^@%��@%u�@%�@$��@$��@$q@$<�@$@#�@#��@#iD@"��@"GE@")�@!�.@!��@!w2@ �[@ �D@ c�@   @b�@�@��@{�@Ta@��@zx@T�@#�@�@��@�f@��@�@j@-�@�@}V@6�@	@�@��@��@�'@e,@N<@?}@2a@4@�|@�@��@�I@��@��@��@��@z�@l"@c�@`�@Q�@N�@S�@PH@PH@Ft@C-@�@��@�[@��@��@��@a@P�@n�@�@�-@��@-w@��@l"@!@  @��@��@��@��@s@t�@v`@qv@A�@�@�@�h@z@u%@c @GE@{@��@o @X@Q�@G�@Dg@:�@*0@@�@�@��@/�@��@�m@خ@�0@��@��@iD@J#@6z@.I@.I@1�@/�@o@�@��@}V@=q@+k@$�@	@�@@�^@��@s�@N<@8�@*0@%@�@e�@�}@�k@t�@iD@6z@
�@
��@
�x@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��]A��A��3Aٶ�A٫A٥�Aّ4A�u%A�QA�YA��fA���A���A���A��]A��KA��mA���A��EA��2A��}A���A�ȀA��#A��A��KAؿHAج�A؄�A�  A׆YA�0�A���A�F�AՒoA�(A���AԹ$A��>AѾ�A��HA�?�A��KAȷ�AƢ�A��fA�%FA���A��}A�d�A��dA���A��A�?�A��pA�	A�3hA���A�A�zDA���A���A���A��A��A��}A�C�A�JA�%zA�bA�<jA�.A��}A��AA�ԕA���A��BA��A��A�A���A�&�A���A�@�A�PHA���A��YA�TA|7Av�.ArqAm�KAl��AiZ�Ag��Ag]�Af�`Ae�=Ad(Aa�A]jAYw2AW�AT�AR$AM�AJ��AG,�AF&AC��A@�7A=�'A=OA;�A7��A4��A3��A2�6A0�6A/g8A.\)A-��A-'�A,f�A+��A*�FA)�=A)�=A)�7A(�]A(m�A(#�A'y�A'  A&�aA&�A&OA%�A%��A%B�A$��A$F�A$>BA$4�A$%�A#��A#}�A"�KA"��A"E�A"3�A" �A"A!�A!�A VA��A|�A)_A��A@A�9AU2A*�A��AHA�KAs�A�AjA�\A}VA_A��A�A4A�nA]dA(AdZA��A�1A�,A)_Af�A��Au�A�6Ac�AB[A
�5A
\�A
)�A	zA	5�A�)A�	A�A�A��A�Aj�A6�A��AeA��Am�A��A��A�rAFtAqA��As�AA7�AYA �A �NA �@��D@�� @���@��D@�y�@��f@�a|@�~@���@�O�@��B@�j@�E�@��F@�0�@�j@���@�33@���@�{@�ݘ@��"@�9�@��c@�J�@��A@��@�C�@���@�	l@��@��@�1�@�4@�O@�\�@�ƨ@��@�i�@���@脶@��@独@�@@�@�C-@�*@���@�oi@�/�@��.@�A @���@�b@��@�@�  @���@ߠ�@ޞ�@�Z�@�ں@�q�@۾w@�S�@��K@�xl@�7@��@�q@׎"@�'�@��@־@֌�@�tT@�M�@�"h@���@�&�@��@�@Ӥ@@�zx@�Z�@�
=@��@Ҥ�@�b@��a@ы�@���@�y>@�L0@��@Ϫ�@Ο�@�;�@��@��r@���@�f�@̹�@ˣn@�hs@ʸR@�x�@�1�@Ț�@�g�@�1@���@ĺ�@ı�@ĳh@�I�@���@�@�{J@�ߤ@�q@�.�@��=@��@���@�"h@��@�ƨ@���@�1@���@�a�@�B�@���@���@��{@�<6@���@�+k@��@��@���@�0�@���@���@�ff@�
�@��K@��V@��"@��M@�A @���@�&�@��a@���@�x�@�RT@�8@��@���@�a|@��@�Mj@��@��@�U2@�4n@���@�`B@��@��r@�kQ@�6�@��@@���@���@�M@�R�@�N�@��@�(�@���@�V�@���@�\)@�5�@�%F@�q@�ȴ@�xl@�:*@�	@��j@���@�ϫ@���@�^�@�T�@�Dg@��@��@���@��@��	@��9@�xl@��@���@�v`@��M@��@���@�PH@�7@���@��z@���@�=�@�33@�*0@�֡@��F@�j@��@��"@���@�:�@��B@���@�h
@�,=@��0@�IR@��@��@��e@�1�@���@�U�@�ی@���@�YK@��@���@�<6@��5@���@�1'@��n@�(�@�@�U2@��m@��@�33@�4@�N<@�Y@�C-@�_@�v`@�&�@��@��z@��@�]�@��@��L@�Ta@�J�@�:�@�-@�
�@���@�hs@�K�@���@�^5@�-@�~@��T@���@��@��@�J�@��P@�m�@�G@���@��=@�{J@�x@�S�@�O�@�rG@�K�@��E@�e@�_@� �@��+@���@���@�v`@�[W@�'�@��f@��@��m@��'@���@��6@�� @��@��L@�<�@�@��@�J�@��@��f@��@���@���@�kQ@�6�@���@�ƨ@��C@��P@�j�@�F�@�:�@�,�@��@��@�
=@��@��@��r@��@��@��[@���@�e,@�.I@�
=@���@��e@�q�@�[�@�Ft@�1'@��@��@���@��-@�y�@�e�@�J#@��@��v@��@���@�9X@��W@��"@�C�@�2a@���@��@�s�@�h
@�^5@�N�@��@_p@33@~��@~��@~��@~Ta@}IR@}�@};@|��@{�Q@{]�@z�@yk�@y&�@x�)@xtT@w�@w��@w4�@v�h@u�D@u��@ux�@t��@t!@s�w@s.I@r�}@r?@q�Z@q+�@p�@p�[@p�Y@p@oA�@n�y@nL0@m�o@m��@mu�@l��@l>B@k��@k�@kv`@kj�@kX�@j�@i��@h��@h~@gs@g�@go@f0U@ec@e�@d��@d_@d*�@c��@c�@c�4@b�@bkQ@b�@a?}@`�@`e�@`	�@_1�@^�]@^{�@]e,@\�K@\��@\�D@\Z@\7�@[�Q@[�:@Z��@ZB[@Y�@Y��@Y��@X��@X�@V�M@V�r@Ve@U��@U��@U�@T�@TXy@TA�@T>B@T4n@S�f@S
=@R�@R�]@R��@R��@R�@Q�@P��@P1'@O�@@N�@N1�@M�X@L��@LI�@K��@K=@Jߤ@J$�@I�@H��@H��@H!@G�W@G�w@G��@G�@@G"�@F��@Fp;@F@�@E�@E|@EB�@E-w@E/@E5�@E�@D4n@C��@C�$@CW?@Co@B�b@B5?@A�)@A��@A��@A�@A�@Af�@A�@A�@A;@@�@@�5@@�j@@�.@@��@@��@@z�@@6@@b@?�Q@?b�@?�@>�@>E�@>_@=�@=�@=x�@=�@=;@<��@<�@<-�@;�@;/�@;�@:�@:�B@:�}@:��@:H�@9�t@98�@9@8�f@8�p@8�e@87@7S�@6��@6M�@6�@5�>@5@57L@4�@42�@3˒@3\)@3o@2�]@2�+@2)�@2J@1�T@1c�@1Q�@1G�@12a@0@.��@.��@.�h@.��@.��@.Q@-��@-Dg@,�@,�D@,Ft@+��@+��@+��@+�4@+t�@++@*�\@)�"@)2a@)�@(PH@'�}@'��@'e�@''�@'$t@'�@&��@&�X@&�@&��@&~�@&Ov@&.�@&�@%�@%�^@%��@%u�@%�@$��@$��@$q@$<�@$@#�@#��@#iD@"��@"GE@")�@!�.@!��@!w2@ �[@ �D@ c�@   @b�@�@��@{�@Ta@��@zx@T�@#�@�@��@�f@��@�@j@-�@�@}V@6�@	@�@��@��@�'@e,@N<@?}@2a@4@�|@�@��@�I@��@��@��@��@z�@l"@c�@`�@Q�@N�@S�@PH@PH@Ft@C-@�@��@�[@��@��@��@a@P�@n�@�@�-@��@-w@��@l"@!@  @��@��@��@��@s@t�@v`@qv@A�@�@�@�h@z@u%@c @GE@{@��@o @X@Q�@G�@Dg@:�@*0@@�@�@��@/�@��@�m@خ@�0@��@��@iD@J#@6z@.I@.I@1�@/�@o@�@��@}V@=q@+k@$�@	@�@@�^@��@s�@N<@8�@*0@%@�@e�@�}@�k@t�@iD@6z@
�@
��@
�x@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
��B
�oB
�oB
�UB
�UB
�B
�vB
�B
�	B
�jB
�6B
��B
��B
�<B
��B
�B
��B B �B �B �B OB �B�B�B�BSB�B[BkB�B�B"B&�B'�B'�B)_B2BNVB�B�:B�VB̈́B�B8�B7�B'8B vB.IBD�BF�BH�BO(BT{BW�BV�BR�BM�BG�BG�BN�BK)BA�B2�B�B3B �B�WB�B��B�VBh�B[�BN"B,qBuB
�sB
�/B
�\B
��B
�LB
��B
��B
hsB
XEB
6FB
<B	��B	�jB	ðB	��B	��B	�uB	��B	��B	��B	}qB	r�B	d&B	N�B	<�B	0�B	"�B	7B	^B	tB�PB�8B�B�B�B�B� B�=B�B�=B�1B�?B�MB�BɺB�'B�:B�B	 �B	�B	.�B	<6B	Y�B	lB	s3B	�EB	�{B	�B	�B	�XB	��B	�aB	�B	��B	�{B	ǔB	�^B	ѝB	ԯB	�B	��B	�B	�KB	�B	��B	�B	�WB	�GB	�B	��B	�`B	��B	�0B	�DB	�B	��B	��B	�"B	�"B	��B	�B	�B	�B	�VB	�jB	��B	��B	��B	��B	��B	�+B	�B	�GB	�B	��B	�B	�RB	��B	�FB	�&B	�|B	�VB	�5B	�]B	ۦB	ݲB	߾B	�hB	�4B	�B	�B	�yB	�B	�kB	�B	��B	�B	�"B	�B	�B	�B	�B	�B	��B	�B	�2B	�B	��B	�XB	�sB	�XB	�B	�B	�*B	�B	�0B	��B	�B	�B	�/B	�;B	�B	��B	�tB	��B	��B	��B	�ZB	�9B	�+B	�%B	��B	�3B	�B	�|B	��B	�AB	�B	�'B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�;B	�B	�B	��B	�aB	�B	�B	�B	�UB	�B	�B	�cB	��B	�B	�B	�=B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�0B	�KB	��B	�B	�B	��B	�WB	�CB	�]B	��B	�B	�WB	�"B	�B	�B	��B	�]B	�wB	�B	�B	�UB	�aB	�B	�B	�-B	��B	��B	�3B	�?B	�B	�TB	�tB	�`B	�8B	�B	��B	�zB	�+B	��B	��B	�2B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�XB	�DB	��B	�dB	�"B	��B	��B	��B	�.B	��B
�B
�B
�B
B
 �B
 iB
 OB
 B	��B
�B
UB
oB
�B
�B
�B
�B
aB
mB
mB
�B
�B
%B
�B
�B
�B
�B
�B
�B
�B
KB
fB
�B
fB
fB
�B
fB
�B
�B
	B
�B
	�B
	RB
	B
�B
	lB
	B
�B
�B
	�B
^B
	�B
�B
	7B

�B
DB
PB
JB
�B
�B
6B
0B
�B
�B
^B
DB

�B

�B

�B
)B
)B
DB
DB

�B

�B

�B

�B
xB
jB
(B
�B
B
B
�B
�B
�B
B
B
B
B
�B
B
�B
�B
�B
�B
�B
hB
�B
�B
�B
B
oB
uB
�B
�B
�B
�B
@B
{B
�B
{B
FB
�B
�B
{B
�B
[B
&B
B
�B
,B
�B
B
B
�B
2B
�B
9B
SB
�B
sB
�B
B
WB
QB
B
B
�B
7B
B
�B
qB
�B
]B
�B
�B
�B
�B
�B
�B
5B
B
!B
�B
 BB
 BB
!-B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#nB
$B
$�B
%�B
'B
'mB
($B
'�B
&�B
%�B
%�B
%�B
%zB
%`B
%�B
&fB
&�B
&fB
&LB
&�B
&�B
'8B
'�B
'�B
'�B
+�B
-)B
-B
,�B
,=B
+�B
+�B
+�B
+�B
,"B
,�B
-�B
.�B
.IB
.}B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/ B
/5B
/B
/ B
/B
/5B
/5B
0B
0oB
0�B
0�B
0UB
1'B
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2B
1�B
2B
2B
2�B
2�B
3B
2�B
3�B
49B
4�B
5�B
6`B
6�B
6�B
6zB
6FB
6�B
6�B
6�B
7B
72B
7B
7fB
8lB
8RB
8RB
8RB
8�B
8�B
9XB
:DB
:*B
:�B
:�B
:�B
:�B
:�B
;�B
<�B
<PB
<6B
;�B
;JB
;0B
;�B
;�B
;�B
;�B
<PB
<jB
<6B
<6B
<�B
<�B
>B
>wB
?.B
?B
?HB
?�B
?�B
@4B
@iB
@�B
@�B
@�B
@�B
?HB
?cB
?}B
?�B
?�B
@4B
@4B
@B
@iB
@�B
@�B
@�B
A;B
A;B
AUB
A�B
BAB
BuB
C-B
CGB
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
F?B
F%B
FtB
FtB
F�B
GzB
G�B
G�B
G�B
HB
H�B
I�B
I�B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
KDB
KDB
KDB
KDB
KDB
KxB
L�B
N�B
O\B
O�B
PHB
P}B
P.B
P}B
PHB
P�B
PbB
P}B
P}B
P}B
P�B
P}B
Q B
RB
SB
SB
S&B
S@B
S�B
T,B
TFB
S�B
S�B
S�B
S�B
T,B
T{B
T�B
UgB
U�B
VB
VB
V9B
VSB
V�B
V�B
V�B
W$B
W
B
WYB
X�B
X_B
X_B
XEB
X+B
XB
XB
XEB
YeB
Y�B
Z7B
Z�B
[	B
[	B
[WB
[=B
\CB
\�B
\�B
\�B
\�B
]B
]dB
]dB
]IB
]�B
]�B
^B
^jB
^jB
^jB
^jB
^jB
^OB
^�B
_VB
_�B
_�B
_�B
_�B
_�B
`'B
`�B
aB
abB
a|B
a|B
abB
a�B
bB
bNB
b�B
b�B
cB
c B
c:B
cnB
cnB
cnB
c�B
c�B
c�B
cTB
d�B
d�B
eB
eB
eB
eB
e,B
e`B
e`B
e`B
e,B
e,B
eB
d�B
d�B
d�B
dtB
dB
d&B
dZB
d�B
d�B
e�B
f2B
fLB
f�B
gB
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h$B
hXB
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
l"B
lB
l�B
m)B
mCB
mCB
m�B
n}B
n�B
o5B
oOB
oOB
o�B
pB
pB
pUB
p;B
poB
pUB
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
sB
sMB
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
tnB
tTB
tnB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
tnB
tnB
t�B
uB
uB
uB
t�B
uB
u%B
uB
vFB
v�B
v�B
v�B
w�B
w�B
x8B
x�B
x�B
x�B
y$B
y$B
yXB
yXB
yXB
yXB
yXB
y�B
y�B
y�B
z*B
z^B
z^B
z^B
zxB
z�B
z�B
{dB
{B
{B
{�B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~�B
~�B
B
~�B
.B
cB
�B
}B
�B
�B
� B
�B
� B
�B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
��B
�oB
�oB
�UB
�UB
�B
�vB
�B
�	B
�jB
�6B
��B
��B
�<B
��B
�B
��B B �B �B �B OB �B�B�B�BSB�B[BkB�B�B"B&�B'�B'�B)_B2BNVB�B�:B�VB̈́B�B8�B7�B'8B vB.IBD�BF�BH�BO(BT{BW�BV�BR�BM�BG�BG�BN�BK)BA�B2�B�B3B �B�WB�B��B�VBh�B[�BN"B,qBuB
�sB
�/B
�\B
��B
�LB
��B
��B
hsB
XEB
6FB
<B	��B	�jB	ðB	��B	��B	�uB	��B	��B	��B	}qB	r�B	d&B	N�B	<�B	0�B	"�B	7B	^B	tB�PB�8B�B�B�B�B� B�=B�B�=B�1B�?B�MB�BɺB�'B�:B�B	 �B	�B	.�B	<6B	Y�B	lB	s3B	�EB	�{B	�B	�B	�XB	��B	�aB	�B	��B	�{B	ǔB	�^B	ѝB	ԯB	�B	��B	�B	�KB	�B	��B	�B	�WB	�GB	�B	��B	�`B	��B	�0B	�DB	�B	��B	��B	�"B	�"B	��B	�B	�B	�B	�VB	�jB	��B	��B	��B	��B	��B	�+B	�B	�GB	�B	��B	�B	�RB	��B	�FB	�&B	�|B	�VB	�5B	�]B	ۦB	ݲB	߾B	�hB	�4B	�B	�B	�yB	�B	�kB	�B	��B	�B	�"B	�B	�B	�B	�B	�B	��B	�B	�2B	�B	��B	�XB	�sB	�XB	�B	�B	�*B	�B	�0B	��B	�B	�B	�/B	�;B	�B	��B	�tB	��B	��B	��B	�ZB	�9B	�+B	�%B	��B	�3B	�B	�|B	��B	�AB	�B	�'B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�;B	�B	�B	��B	�aB	�B	�B	�B	�UB	�B	�B	�cB	��B	�B	�B	�=B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�0B	�KB	��B	�B	�B	��B	�WB	�CB	�]B	��B	�B	�WB	�"B	�B	�B	��B	�]B	�wB	�B	�B	�UB	�aB	�B	�B	�-B	��B	��B	�3B	�?B	�B	�TB	�tB	�`B	�8B	�B	��B	�zB	�+B	��B	��B	�2B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�XB	�DB	��B	�dB	�"B	��B	��B	��B	�.B	��B
�B
�B
�B
B
 �B
 iB
 OB
 B	��B
�B
UB
oB
�B
�B
�B
�B
aB
mB
mB
�B
�B
%B
�B
�B
�B
�B
�B
�B
�B
KB
fB
�B
fB
fB
�B
fB
�B
�B
	B
�B
	�B
	RB
	B
�B
	lB
	B
�B
�B
	�B
^B
	�B
�B
	7B

�B
DB
PB
JB
�B
�B
6B
0B
�B
�B
^B
DB

�B

�B

�B
)B
)B
DB
DB

�B

�B

�B

�B
xB
jB
(B
�B
B
B
�B
�B
�B
B
B
B
B
�B
B
�B
�B
�B
�B
�B
hB
�B
�B
�B
B
oB
uB
�B
�B
�B
�B
@B
{B
�B
{B
FB
�B
�B
{B
�B
[B
&B
B
�B
,B
�B
B
B
�B
2B
�B
9B
SB
�B
sB
�B
B
WB
QB
B
B
�B
7B
B
�B
qB
�B
]B
�B
�B
�B
�B
�B
�B
5B
B
!B
�B
 BB
 BB
!-B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#nB
$B
$�B
%�B
'B
'mB
($B
'�B
&�B
%�B
%�B
%�B
%zB
%`B
%�B
&fB
&�B
&fB
&LB
&�B
&�B
'8B
'�B
'�B
'�B
+�B
-)B
-B
,�B
,=B
+�B
+�B
+�B
+�B
,"B
,�B
-�B
.�B
.IB
.}B
.}B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/ B
/5B
/B
/ B
/B
/5B
/5B
0B
0oB
0�B
0�B
0UB
1'B
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2B
1�B
2B
2B
2�B
2�B
3B
2�B
3�B
49B
4�B
5�B
6`B
6�B
6�B
6zB
6FB
6�B
6�B
6�B
7B
72B
7B
7fB
8lB
8RB
8RB
8RB
8�B
8�B
9XB
:DB
:*B
:�B
:�B
:�B
:�B
:�B
;�B
<�B
<PB
<6B
;�B
;JB
;0B
;�B
;�B
;�B
;�B
<PB
<jB
<6B
<6B
<�B
<�B
>B
>wB
?.B
?B
?HB
?�B
?�B
@4B
@iB
@�B
@�B
@�B
@�B
?HB
?cB
?}B
?�B
?�B
@4B
@4B
@B
@iB
@�B
@�B
@�B
A;B
A;B
AUB
A�B
BAB
BuB
C-B
CGB
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
FB
F?B
F%B
FtB
FtB
F�B
GzB
G�B
G�B
G�B
HB
H�B
I�B
I�B
I�B
J	B
J#B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
KDB
KDB
KDB
KDB
KDB
KxB
L�B
N�B
O\B
O�B
PHB
P}B
P.B
P}B
PHB
P�B
PbB
P}B
P}B
P}B
P�B
P}B
Q B
RB
SB
SB
S&B
S@B
S�B
T,B
TFB
S�B
S�B
S�B
S�B
T,B
T{B
T�B
UgB
U�B
VB
VB
V9B
VSB
V�B
V�B
V�B
W$B
W
B
WYB
X�B
X_B
X_B
XEB
X+B
XB
XB
XEB
YeB
Y�B
Z7B
Z�B
[	B
[	B
[WB
[=B
\CB
\�B
\�B
\�B
\�B
]B
]dB
]dB
]IB
]�B
]�B
^B
^jB
^jB
^jB
^jB
^jB
^OB
^�B
_VB
_�B
_�B
_�B
_�B
_�B
`'B
`�B
aB
abB
a|B
a|B
abB
a�B
bB
bNB
b�B
b�B
cB
c B
c:B
cnB
cnB
cnB
c�B
c�B
c�B
cTB
d�B
d�B
eB
eB
eB
eB
e,B
e`B
e`B
e`B
e,B
e,B
eB
d�B
d�B
d�B
dtB
dB
d&B
dZB
d�B
d�B
e�B
f2B
fLB
f�B
gB
f�B
f�B
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h$B
hXB
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j�B
k�B
k�B
l"B
lB
l�B
m)B
mCB
mCB
m�B
n}B
n�B
o5B
oOB
oOB
o�B
pB
pB
pUB
p;B
poB
pUB
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
sB
sMB
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
tnB
tTB
tnB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
tnB
tnB
t�B
uB
uB
uB
t�B
uB
u%B
uB
vFB
v�B
v�B
v�B
w�B
w�B
x8B
x�B
x�B
x�B
y$B
y$B
yXB
yXB
yXB
yXB
yXB
y�B
y�B
y�B
z*B
z^B
z^B
z^B
zxB
z�B
z�B
{dB
{B
{B
{�B
{B
{�B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
}B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~�B
~�B
B
~�B
.B
cB
�B
}B
�B
�B
� B
�B
� B
�B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220925004306  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220925004323  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220925004324  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220925004324                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220925094328  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220925094328  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220925010427                      G�O�G�O�G�O�                