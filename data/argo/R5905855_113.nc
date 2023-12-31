CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:30:59Z creation;2022-06-04T19:30:59Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604193059  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��x6͎�1   @��x�<M^@-���
=q�c�&�x��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B˙�B�33Bә�B�  B���B�  B���B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @u@��@��A�
A:=pA[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B��GB�z�B�G�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�{BϮB�{B�z�B�G�B�z�B�G�B�z�B�z�B�z�B�z�B��B��B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�C'��C)��C+��C-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�C[�qC]�qC_�qCa�qCc��Ce�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM��DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyh�Dy�\Dzo\Dz�\D{o\D{�\D|o\D|�\D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�w�Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǷ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̷�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�:�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�T�A�W?A�Z�A�Q�A�TaA�O�A�IRA�K�A�M�A�H�A�HA�J�A�K�A�H�A�I�A�FtA�FA�F�A�F�A�G�A�I�A�I�A�J�A�L�A�MjA�L�A�NA�OA�O�A�:*A��A�R�A�poA˙�Aʠ\A�T,A���A�ҽA��cAȋA�Y�AǪ�A�S[A�J�AĎ�A�%A��A���A��3A�(A���A�r�A��A�N�A�C�A��]A�JXA��A��dA��A�PHA�˒A�C�A���A� 'A�B�A�PHA�d�A� 4A��A�uA��A��|A��A���A��	A�CA�b�A�Q�A�u%A��A��ZA���Ax�Ato AnjAh��Ae�Aa]dA]�-AW��AUAT)_AR�AP��AP4nAO��AO!�AN�VAM�AJe�AG�XAE��AC�&ABoiAA�'A?�:A<��A<$A:��A:�A9�.A8($A1�A/cA/?}A.��A/CA/A/�A.�FA+!A)<�A&0�A$�XA#a|A"�RA"�A"=qA"|A"��A#V�A"ԕA"�}A!	A!Q�A!U2A ��A��A=�Ag8A�KA�RA\)Ag�A��AϫA�A�ACA��A�QA�A�A�0A�A9XA�A�[AoiAc�AL0AݘA;dAm�A��A;A�A?}Ah�A:�A�A0�A�A0�A��AeAS�A�A{�A33A�pAXA�A��A�Aw�A��A��A�-A��A(�A�SAPHA$�A
��A
��A
��A
�jA
�SA
8A	y>A	A�A	+A	�A�A��A4nAĜAV�A!�AیAu%AV�A�A�AϫA��A#�Ah
A�AsA��A�+AL�A�hA;�AA �	A ?}A �@���@�y�@�l�@��P@�M@�}�@�Dg@��`@��U@��@�S�@��7@�ں@���@��@��@�1@�"�@�>�@�8@�)_@�W�@��A@�p�@�S�@�iD@�@�(�@�_@�b�@�j@�8�@��@���@��n@�=@웦@�c�@�~@��@�S�@�|�@��@�Mj@�+k@�V@��@��@��@�w2@�$@�oi@�2�@��@��@��@�$t@��@��Q@�:�@�?@ݠ�@�l"@۱[@�f�@�?}@���@ڌ�@�Xy@ٯ�@�o @��@�ff@�4@��@׌~@�;d@���@։�@�M@�@՗$@��p@�#:@ӯ�@�S�@���@���@���@��@���@у{@�]�@���@�8�@�w2@ΐ.@�Y�@�%@̫6@̓u@̚�@�u%@��@�s�@ʞ@ʇ+@��T@�B�@Ⱦ�@�,=@�e@���@�+@�)�@Ő�@�-w@�Ɇ@���@ÄM@�&�@��@��?@�]d@���@���@�خ@��M@��Y@�($@��W@���@��@��@��h@�)�@��M@� i@��v@���@���@��@���@��\@�p;@�1'@��g@���@�iD@�A�@��@��v@��Y@�0U@��@��s@�j@��r@��3@�L�@��M@�e�@��@���@��@�RT@���@�E�@���@�RT@���@�N�@�1'@��@�u@���@���@��`@��@�i�@�R�@�:*@��@��@��U@�1'@��@�8@���@��W@���@�1�@�͟@���@�bN@�,=@��@�\)@�&�@�ߤ@�;�@��@��@��@��"@�g�@�C@��_@��@��s@�a|@��T@���@��@��9@�Ov@�*�@���@��@��X@���@���@���@��n@�|�@�(@�xl@� �@���@�u�@�a�@�A�@�4@�"�@��y@���@�R�@��g@��:@�%@���@�K^@��@��@���@�=@�q@�;@��v@��x@��.@�}V@�e�@�:�@���@��k@�o�@���@���@��F@�bN@�(�@��Q@��h@�J#@��@���@���@�i�@�.�@��]@��d@�~�@�a�@�#�@���@�C-@��N@��H@��X@�J#@���@�~(@���@��@���@�9�@�Y@��@��y@���@���@�xl@� �@��Q@���@�O@�0�@�#�@��"@���@�PH@�@��@��@���@�Vm@�o@��5@��@��@��U@���@�Q@���@��n@���@�?}@��@��@���@��}@��+@�0U@���@��-@��q@���@���@�e,@�0�@���@�U2@�($@�1@��>@�خ@���@�o @�-w@��@�ی@�l"@��@��)@���@���@�a�@�IR@�0�@�$t@��@��@���@�l"@�($@��T@��@���@�iD@��@��@�ی@��'@��F@�oi@�E�@�'R@��@˒@x@C�@(@~�h@~�\@~xl@~i�@~d�@~W�@}c�@};@|`�@{�@{E9@y�@y?}@x�@x@w�@v��@u�)@u��@uS&@t�I@tA�@s�&@s��@s"�@r^5@r4@q�)@q�j@q��@q@p�5@p��@p�?@p�@p?�@p1@o�@n�"@n)�@l�@lh�@k��@kn/@k+@j��@i�)@i��@i+�@hɆ@h��@h�@f�"@f:*@e��@e�@d��@c�r@cqv@c
=@b��@b�1@bE�@`�$@`>B@`*�@_خ@_E9@^�'@^
�@]!�@\�j@\q@\�@[�q@[$t@Z��@Y�@XɆ@X1'@X�@X7@X�@W�w@V�@Vff@U��@U/@T��@T�D@TD�@T�@S��@S\)@S(@R��@R�A@RJ�@R{@Q�T@Qm]@Q%F@Q�@P�I@O��@O@O@N��@N�X@N�<@N�@N��@N}V@NkQ@NJ�@N�@M��@MA @LC-@K�[@K�:@Kl�@K=@J�@J��@Jh
@I�j@I}�@H�f@H��@H��@HD�@G�@G��@Gg�@F��@F�,@F�!@F�\@F5?@E��@E�@Ezx@E@@D�E@D��@DI�@D7@C�@C1�@B��@B��@A�-@@�@@�e@@~(@@bN@@(�@?�k@>ȴ@>E�@>�@=�)@=��@=a�@<�U@<��@<Ĝ@<]d@;��@;�$@;_p@;9�@:��@:�@:͟@:�@:.�@9�)@9��@9	l@8�@8H@8%�@7�@7'�@6��@6��@6E�@5�T@5��@5[W@5�@4�?@4��@4w�@4j@44n@3�g@3�@3j�@3/�@2�y@2�'@2��@2�@2d�@1��@1w2@15�@1�@1;@0��@0�z@0z�@0Xy@0~@/�@/˒@/��@/�@/�:@/x@/A�@.��@.�@-�~@-f�@-0�@,�f@,��@,"h@+ƨ@+qv@+b�@+W?@+�@*�@*��@*L0@*
�@)��@)�S@)c@)+�@(֡@(�j@(�u@(  @'�$@'K�@'�@&��@&�}@&��@&R�@%�)@%�-@%�"@%u�@%o @%�@$�.@$C-@#��@#��@#iD@#C�@"��@"�R@"��@"Z�@!�@![W@!�@ �$@ ��@ 7@��@��@�P@y�@qv@\)@8@+@'�@!-@�c@��@{@��@��@�j@�@��@�"@��@��@��@��@e,@#�@��@C-@�@˒@�$@Z�@͟@�@�1@{�@C�@J@ϫ@��@j@+�@�)@�4@u�@]d@C-@�@�@��@iD@O@�@@��@��@҉@�@�h@��@�A@^5@�@�@�3@�-@�'@�~@s�@+�@��@~(@c�@<�@�@��@��@��@�P@�f@x@_p@6z@�@
=@�+@h
@J�@($@�Z@�#@@�t@�7@:�@��@�@��@q@(�@�@�@�@x@��@e�@4�@)_@��@��@��@xl@H�@+k@
�@�-@c@rG@[W@IR@@�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�T�A�W?A�Z�A�Q�A�TaA�O�A�IRA�K�A�M�A�H�A�HA�J�A�K�A�H�A�I�A�FtA�FA�F�A�F�A�G�A�I�A�I�A�J�A�L�A�MjA�L�A�NA�OA�O�A�:*A��A�R�A�poA˙�Aʠ\A�T,A���A�ҽA��cAȋA�Y�AǪ�A�S[A�J�AĎ�A�%A��A���A��3A�(A���A�r�A��A�N�A�C�A��]A�JXA��A��dA��A�PHA�˒A�C�A���A� 'A�B�A�PHA�d�A� 4A��A�uA��A��|A��A���A��	A�CA�b�A�Q�A�u%A��A��ZA���Ax�Ato AnjAh��Ae�Aa]dA]�-AW��AUAT)_AR�AP��AP4nAO��AO!�AN�VAM�AJe�AG�XAE��AC�&ABoiAA�'A?�:A<��A<$A:��A:�A9�.A8($A1�A/cA/?}A.��A/CA/A/�A.�FA+!A)<�A&0�A$�XA#a|A"�RA"�A"=qA"|A"��A#V�A"ԕA"�}A!	A!Q�A!U2A ��A��A=�Ag8A�KA�RA\)Ag�A��AϫA�A�ACA��A�QA�A�A�0A�A9XA�A�[AoiAc�AL0AݘA;dAm�A��A;A�A?}Ah�A:�A�A0�A�A0�A��AeAS�A�A{�A33A�pAXA�A��A�Aw�A��A��A�-A��A(�A�SAPHA$�A
��A
��A
��A
�jA
�SA
8A	y>A	A�A	+A	�A�A��A4nAĜAV�A!�AیAu%AV�A�A�AϫA��A#�Ah
A�AsA��A�+AL�A�hA;�AA �	A ?}A �@���@�y�@�l�@��P@�M@�}�@�Dg@��`@��U@��@�S�@��7@�ں@���@��@��@�1@�"�@�>�@�8@�)_@�W�@��A@�p�@�S�@�iD@�@�(�@�_@�b�@�j@�8�@��@���@��n@�=@웦@�c�@�~@��@�S�@�|�@��@�Mj@�+k@�V@��@��@��@�w2@�$@�oi@�2�@��@��@��@�$t@��@��Q@�:�@�?@ݠ�@�l"@۱[@�f�@�?}@���@ڌ�@�Xy@ٯ�@�o @��@�ff@�4@��@׌~@�;d@���@։�@�M@�@՗$@��p@�#:@ӯ�@�S�@���@���@���@��@���@у{@�]�@���@�8�@�w2@ΐ.@�Y�@�%@̫6@̓u@̚�@�u%@��@�s�@ʞ@ʇ+@��T@�B�@Ⱦ�@�,=@�e@���@�+@�)�@Ő�@�-w@�Ɇ@���@ÄM@�&�@��@��?@�]d@���@���@�خ@��M@��Y@�($@��W@���@��@��@��h@�)�@��M@� i@��v@���@���@��@���@��\@�p;@�1'@��g@���@�iD@�A�@��@��v@��Y@�0U@��@��s@�j@��r@��3@�L�@��M@�e�@��@���@��@�RT@���@�E�@���@�RT@���@�N�@�1'@��@�u@���@���@��`@��@�i�@�R�@�:*@��@��@��U@�1'@��@�8@���@��W@���@�1�@�͟@���@�bN@�,=@��@�\)@�&�@�ߤ@�;�@��@��@��@��"@�g�@�C@��_@��@��s@�a|@��T@���@��@��9@�Ov@�*�@���@��@��X@���@���@���@��n@�|�@�(@�xl@� �@���@�u�@�a�@�A�@�4@�"�@��y@���@�R�@��g@��:@�%@���@�K^@��@��@���@�=@�q@�;@��v@��x@��.@�}V@�e�@�:�@���@��k@�o�@���@���@��F@�bN@�(�@��Q@��h@�J#@��@���@���@�i�@�.�@��]@��d@�~�@�a�@�#�@���@�C-@��N@��H@��X@�J#@���@�~(@���@��@���@�9�@�Y@��@��y@���@���@�xl@� �@��Q@���@�O@�0�@�#�@��"@���@�PH@�@��@��@���@�Vm@�o@��5@��@��@��U@���@�Q@���@��n@���@�?}@��@��@���@��}@��+@�0U@���@��-@��q@���@���@�e,@�0�@���@�U2@�($@�1@��>@�خ@���@�o @�-w@��@�ی@�l"@��@��)@���@���@�a�@�IR@�0�@�$t@��@��@���@�l"@�($@��T@��@���@�iD@��@��@�ی@��'@��F@�oi@�E�@�'R@��@˒@x@C�@(@~�h@~�\@~xl@~i�@~d�@~W�@}c�@};@|`�@{�@{E9@y�@y?}@x�@x@w�@v��@u�)@u��@uS&@t�I@tA�@s�&@s��@s"�@r^5@r4@q�)@q�j@q��@q@p�5@p��@p�?@p�@p?�@p1@o�@n�"@n)�@l�@lh�@k��@kn/@k+@j��@i�)@i��@i+�@hɆ@h��@h�@f�"@f:*@e��@e�@d��@c�r@cqv@c
=@b��@b�1@bE�@`�$@`>B@`*�@_خ@_E9@^�'@^
�@]!�@\�j@\q@\�@[�q@[$t@Z��@Y�@XɆ@X1'@X�@X7@X�@W�w@V�@Vff@U��@U/@T��@T�D@TD�@T�@S��@S\)@S(@R��@R�A@RJ�@R{@Q�T@Qm]@Q%F@Q�@P�I@O��@O@O@N��@N�X@N�<@N�@N��@N}V@NkQ@NJ�@N�@M��@MA @LC-@K�[@K�:@Kl�@K=@J�@J��@Jh
@I�j@I}�@H�f@H��@H��@HD�@G�@G��@Gg�@F��@F�,@F�!@F�\@F5?@E��@E�@Ezx@E@@D�E@D��@DI�@D7@C�@C1�@B��@B��@A�-@@�@@�e@@~(@@bN@@(�@?�k@>ȴ@>E�@>�@=�)@=��@=a�@<�U@<��@<Ĝ@<]d@;��@;�$@;_p@;9�@:��@:�@:͟@:�@:.�@9�)@9��@9	l@8�@8H@8%�@7�@7'�@6��@6��@6E�@5�T@5��@5[W@5�@4�?@4��@4w�@4j@44n@3�g@3�@3j�@3/�@2�y@2�'@2��@2�@2d�@1��@1w2@15�@1�@1;@0��@0�z@0z�@0Xy@0~@/�@/˒@/��@/�@/�:@/x@/A�@.��@.�@-�~@-f�@-0�@,�f@,��@,"h@+ƨ@+qv@+b�@+W?@+�@*�@*��@*L0@*
�@)��@)�S@)c@)+�@(֡@(�j@(�u@(  @'�$@'K�@'�@&��@&�}@&��@&R�@%�)@%�-@%�"@%u�@%o @%�@$�.@$C-@#��@#��@#iD@#C�@"��@"�R@"��@"Z�@!�@![W@!�@ �$@ ��@ 7@��@��@�P@y�@qv@\)@8@+@'�@!-@�c@��@{@��@��@�j@�@��@�"@��@��@��@��@e,@#�@��@C-@�@˒@�$@Z�@͟@�@�1@{�@C�@J@ϫ@��@j@+�@�)@�4@u�@]d@C-@�@�@��@iD@O@�@@��@��@҉@�@�h@��@�A@^5@�@�@�3@�-@�'@�~@s�@+�@��@~(@c�@<�@�@��@��@��@�P@�f@x@_p@6z@�@
=@�+@h
@J�@($@�Z@�#@@�t@�7@:�@��@�@��@q@(�@�@�@�@x@��@e�@4�@)_@��@��@��@xl@H�@+k@
�@�-@c@rG@[W@IR@@�@��@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�vB	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�vB	�vB	�AB	�[B	�'B	�AB	�AB	�'B	�AB	�AB	�'B	�B	�'B	�B	��B	�B	��B
�B
�BKB �B
��B
�wB�B<6BT{Bn/B��B�hB��B��B�,B�B��B�rB�%B�B�MB	7B�B&B(�B{BmB�!B�B�B��B�lB�;B�oB�	B~(Br�Bd@BX�BYB=�B,qBB
��B
�B
��B
B
�hB
�B
|B
dB
+�B	��B	ңB	�B	�B	��B	n�B	cTB	Q B	G_B	D�B	A�B	FYB	K�B	OBB	O�B	M�B	MjB	J#B	I�B	H�B	IB	JXB	J�B	L�B	L�B	MB	QhB	T�B	R�B	IB	'RB	�B	�B	 B	3B	YB	q�B	p�B	OB	=VB	!�B	�B	5B	+kB	=�B	abB	|B	��B	�B	��B	��B	�fB	��B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�4B	�hB	�HB	�7B	�HB	��B	��B	�B	��B	��B	��B	��B	�mB	�;B	�OB	�B	�B	�vB	�LB	��B	��B	��B
�B
pB
%,B
'�B
'�B
$�B
&�B
 vB
B
�B
#B
$�B
"4B
!-B
!�B
# B
"�B
$&B
$&B
 �B
#�B
&fB
(�B
)�B
)�B
'B
%�B
$tB
#�B
"�B
"NB
&�B
'�B
$tB
!-B
 �B
"4B
$�B
$�B
#�B
"�B
$ZB
$�B
$B
#�B
"�B
"hB
"hB
VB
�B
#�B
 �B
�B
+B
�B
B
 vB
!HB
VB
B
)B
�B
B
=B
 �B
!�B
"�B
"B
"�B
!|B
!�B
!|B
!HB
 �B
 �B
 BB
VB
�B
�B
B
�B
kB
B
�B
B
�B
~B
/B
qB
7B
EB
�B
�B
oB
�B
,B
aB
�B
+B
B
�B
+B
�B
EB
+B
�B
+B
�B
�B
mB
mB
�B
�B
gB
�B
�B
�B
�B
B
�B
B
�B
�B
vB
jB
�B

#B
�B
B
1B
fB
�B
�B
1B
�B
�B
zB
�B
�B
%B
�B
B
�B
�B
B
�B
�B
MB
�B
 B	��B
  B
 iB
 �B
B
 �B
  B
 �B	��B	�qB	��B	��B	�6B	�<B	�B	��B
�B
aB
-B
�B
�B
�B
;B
 �B
�B
�B
	RB

	B

XB

#B
	�B
	RB
	B
�B
�B
�B
�B
�B
�B
	B
�B
1B
�B
EB
�B
�B
�B
�B
�B
�B
KB
	�B
	�B
	�B
	�B
	�B

	B

	B

#B

#B

=B

XB

XB

XB

XB

rB

�B

�B
DB
JB
�B
�B
�B
VB
�B
vB
\B
\B
�B
\B
�B
�B
HB
B
�B
�B
�B
�B
�B
�B
4B
�B
�B
�B
�B
�B
�B
 B
TB
�B
B
[B
uB
�B
�B
,B
�B
�B
2B
B
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
YB
�B
�B
�B
1B
eB
�B
�B
7B
7B
kB
�B
�B
�B
�B
�B
kB
kB
�B
qB
�B
CB
B
B
B
B
B
]B
]B
�B
�B
/B
5B
�B
�B
VB
�B
�B
 BB
 BB
 \B
 vB
 �B
 vB
 �B
 �B
 �B
 �B
 �B
 'B
 BB
�B
 B
 �B
!-B
!�B
!�B
!�B
"4B
"�B
"�B
"�B
# B
#:B
#:B
#�B
$@B
%B
%`B
$�B
$@B
#�B
#�B
#�B
#�B
$B
#�B
$@B
$@B
$�B
%�B
&B
&�B
'B
'B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'B
'RB
'�B
'�B
(
B
($B
(sB
(�B
(�B
)B
)�B
*KB
*B
+QB
*�B
*B
+�B
*�B
*�B
+B
*�B
*�B
*�B
+QB
+�B
,B
,"B
,WB
,�B
.IB
/�B
0!B
0B
0;B
0UB
0UB
0�B
1�B
2-B
2B
1�B
1�B
2|B
2�B
2�B
2�B
3�B
3�B
4B
49B
49B
4nB
4TB
4�B
5B
5ZB
6+B
6FB
6zB
6`B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9XB
9�B
9rB
9�B
:^B
:^B
:�B
:�B
:�B
;dB
;�B
;�B
<6B
<6B
<�B
=�B
=�B
>�B
?.B
?.B
?.B
>�B
?cB
?}B
?}B
?}B
?}B
?�B
?�B
?�B
?�B
@4B
@iB
A�B
A�B
BuB
B�B
B�B
CB
CGB
CaB
C-B
CB
B�B
B�B
CGB
C�B
DB
D�B
DgB
D�B
D�B
D�B
D�B
D�B
EB
GEB
G+B
GB
GB
G�B
HKB
H�B
I7B
I�B
I�B
JrB
J�B
KB
KB
K�B
LB
LB
LB
K�B
K�B
K�B
LdB
L0B
LJB
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
M�B
N<B
NVB
NpB
N�B
N�B
OB
OB
OBB
O�B
P.B
PbB
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
QB
QB
R B
RoB
R�B
R�B
R�B
R�B
S&B
S@B
S�B
S�B
T{B
T�B
T�B
T�B
UB
U2B
UgB
U�B
U�B
U�B
U�B
VB
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
W�B
W�B
W�B
X�B
Y1B
Y1B
YeB
YeB
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
ZkB
Z�B
[�B
[qB
[WB
[�B
\]B
\�B
]B
]B
]/B
\�B
\�B
]B
]IB
]dB
]�B
^5B
^�B
^�B
^�B
^�B
`B
`B
`'B
`'B
`vB
`vB
`�B
`�B
aB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
bNB
bhB
bhB
b�B
b�B
cTB
c�B
c�B
dB
dB
dZB
dtB
d�B
d�B
d�B
d�B
d�B
e,B
eFB
eFB
e`B
e`B
e�B
fLB
f�B
f�B
ffB
fLB
f�B
g8B
g�B
g�B
h
B
g�B
h$B
hsB
h�B
i*B
iyB
i�B
i�B
i�B
jeB
j�B
j�B
j�B
kkB
k�B
l"B
lWB
lWB
l�B
l�B
l�B
m)B
mCB
mCB
mCB
mB
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
oB
o B
oOB
poB
p�B
p�B
qAB
q[B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
tB
tB
tB
tB
tB
t9B
tnB
tnB
tnB
tTB
tTB
tnB
t�B
t�B
utB
u�B
u�B
vB
v+B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
x8B
xRB
x�B
x�B
y	B
y$B
y	B
y>B
y�B
y�B
y�B
y�B
zDB
zDB
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{dB
{dB
{B
{B
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
}<B
}"B
}"B
}VB
}<B
}VB
}qB
}�B
}�B
}�B
~BB
~]B
~]B
~�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�iB
��B
��B
��B
�;B
�oB
��B
��B
�oB
�UB
�;B
�;B
�;B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B	�B	�vB	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�vB	�vB	�AB	�[B	�'B	�AB	�AB	�'B	�AB	�AB	�'B	�B	�'B	�B	��B	�B	��B
�B
�BKB �B
��B
�wB�B<6BT{Bn/B��B�hB��B��B�,B�B��B�rB�%B�B�MB	7B�B&B(�B{BmB�!B�B�B��B�lB�;B�oB�	B~(Br�Bd@BX�BYB=�B,qBB
��B
�B
��B
B
�hB
�B
|B
dB
+�B	��B	ңB	�B	�B	��B	n�B	cTB	Q B	G_B	D�B	A�B	FYB	K�B	OBB	O�B	M�B	MjB	J#B	I�B	H�B	IB	JXB	J�B	L�B	L�B	MB	QhB	T�B	R�B	IB	'RB	�B	�B	 B	3B	YB	q�B	p�B	OB	=VB	!�B	�B	5B	+kB	=�B	abB	|B	��B	�B	��B	��B	�fB	��B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�4B	�hB	�HB	�7B	�HB	��B	��B	�B	��B	��B	��B	��B	�mB	�;B	�OB	�B	�B	�vB	�LB	��B	��B	��B
�B
pB
%,B
'�B
'�B
$�B
&�B
 vB
B
�B
#B
$�B
"4B
!-B
!�B
# B
"�B
$&B
$&B
 �B
#�B
&fB
(�B
)�B
)�B
'B
%�B
$tB
#�B
"�B
"NB
&�B
'�B
$tB
!-B
 �B
"4B
$�B
$�B
#�B
"�B
$ZB
$�B
$B
#�B
"�B
"hB
"hB
VB
�B
#�B
 �B
�B
+B
�B
B
 vB
!HB
VB
B
)B
�B
B
=B
 �B
!�B
"�B
"B
"�B
!|B
!�B
!|B
!HB
 �B
 �B
 BB
VB
�B
�B
B
�B
kB
B
�B
B
�B
~B
/B
qB
7B
EB
�B
�B
oB
�B
,B
aB
�B
+B
B
�B
+B
�B
EB
+B
�B
+B
�B
�B
mB
mB
�B
�B
gB
�B
�B
�B
�B
B
�B
B
�B
�B
vB
jB
�B

#B
�B
B
1B
fB
�B
�B
1B
�B
�B
zB
�B
�B
%B
�B
B
�B
�B
B
�B
�B
MB
�B
 B	��B
  B
 iB
 �B
B
 �B
  B
 �B	��B	�qB	��B	��B	�6B	�<B	�B	��B
�B
aB
-B
�B
�B
�B
;B
 �B
�B
�B
	RB

	B

XB

#B
	�B
	RB
	B
�B
�B
�B
�B
�B
�B
	B
�B
1B
�B
EB
�B
�B
�B
�B
�B
�B
KB
	�B
	�B
	�B
	�B
	�B

	B

	B

#B

#B

=B

XB

XB

XB

XB

rB

�B

�B
DB
JB
�B
�B
�B
VB
�B
vB
\B
\B
�B
\B
�B
�B
HB
B
�B
�B
�B
�B
�B
�B
4B
�B
�B
�B
�B
�B
�B
 B
TB
�B
B
[B
uB
�B
�B
,B
�B
�B
2B
B
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
YB
�B
�B
�B
1B
eB
�B
�B
7B
7B
kB
�B
�B
�B
�B
�B
kB
kB
�B
qB
�B
CB
B
B
B
B
B
]B
]B
�B
�B
/B
5B
�B
�B
VB
�B
�B
 BB
 BB
 \B
 vB
 �B
 vB
 �B
 �B
 �B
 �B
 �B
 'B
 BB
�B
 B
 �B
!-B
!�B
!�B
!�B
"4B
"�B
"�B
"�B
# B
#:B
#:B
#�B
$@B
%B
%`B
$�B
$@B
#�B
#�B
#�B
#�B
$B
#�B
$@B
$@B
$�B
%�B
&B
&�B
'B
'B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'B
'RB
'�B
'�B
(
B
($B
(sB
(�B
(�B
)B
)�B
*KB
*B
+QB
*�B
*B
+�B
*�B
*�B
+B
*�B
*�B
*�B
+QB
+�B
,B
,"B
,WB
,�B
.IB
/�B
0!B
0B
0;B
0UB
0UB
0�B
1�B
2-B
2B
1�B
1�B
2|B
2�B
2�B
2�B
3�B
3�B
4B
49B
49B
4nB
4TB
4�B
5B
5ZB
6+B
6FB
6zB
6`B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8RB
8lB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9XB
9�B
9rB
9�B
:^B
:^B
:�B
:�B
:�B
;dB
;�B
;�B
<6B
<6B
<�B
=�B
=�B
>�B
?.B
?.B
?.B
>�B
?cB
?}B
?}B
?}B
?}B
?�B
?�B
?�B
?�B
@4B
@iB
A�B
A�B
BuB
B�B
B�B
CB
CGB
CaB
C-B
CB
B�B
B�B
CGB
C�B
DB
D�B
DgB
D�B
D�B
D�B
D�B
D�B
EB
GEB
G+B
GB
GB
G�B
HKB
H�B
I7B
I�B
I�B
JrB
J�B
KB
KB
K�B
LB
LB
LB
K�B
K�B
K�B
LdB
L0B
LJB
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
M�B
N<B
NVB
NpB
N�B
N�B
OB
OB
OBB
O�B
P.B
PbB
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
QB
QB
R B
RoB
R�B
R�B
R�B
R�B
S&B
S@B
S�B
S�B
T{B
T�B
T�B
T�B
UB
U2B
UgB
U�B
U�B
U�B
U�B
VB
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
W�B
W�B
W�B
X�B
Y1B
Y1B
YeB
YeB
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
ZkB
Z�B
[�B
[qB
[WB
[�B
\]B
\�B
]B
]B
]/B
\�B
\�B
]B
]IB
]dB
]�B
^5B
^�B
^�B
^�B
^�B
`B
`B
`'B
`'B
`vB
`vB
`�B
`�B
aB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
bNB
bhB
bhB
b�B
b�B
cTB
c�B
c�B
dB
dB
dZB
dtB
d�B
d�B
d�B
d�B
d�B
e,B
eFB
eFB
e`B
e`B
e�B
fLB
f�B
f�B
ffB
fLB
f�B
g8B
g�B
g�B
h
B
g�B
h$B
hsB
h�B
i*B
iyB
i�B
i�B
i�B
jeB
j�B
j�B
j�B
kkB
k�B
l"B
lWB
lWB
l�B
l�B
l�B
m)B
mCB
mCB
mCB
mB
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
oB
o B
oOB
poB
p�B
p�B
qAB
q[B
q�B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
tB
tB
tB
tB
tB
t9B
tnB
tnB
tnB
tTB
tTB
tnB
t�B
t�B
utB
u�B
u�B
vB
v+B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
x8B
xRB
x�B
x�B
y	B
y$B
y	B
y>B
y�B
y�B
y�B
y�B
zDB
zDB
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{dB
{dB
{B
{B
{B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
}<B
}"B
}"B
}VB
}<B
}VB
}qB
}�B
}�B
}�B
~BB
~]B
~]B
~�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�iB
��B
��B
��B
�;B
�oB
��B
��B
�oB
�UB
�;B
�;B
�;B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193059  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193059  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193059                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043107  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043107  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                