CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:24:23Z creation;2022-06-04T19:24:24Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192423  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               OA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�lL�A<1   @�l��7@-�
=p���c�n��O�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C L�C�fC  C  C  C
  C  C  C  C  C33C��C�fC  C  C  C�fC"  C$  C&  C(  C*  C,�3C-� C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CPL�CQ�fCT  CU�fCX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  CrL�Cs��Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܃3D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@o\)@��@��A�
A;�
AZ=pA{�
A��A��A��A��RA��A��A��A��B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�C 
>C��C�qC�qC�qC	�qC�qC�qC�qC�qC�C�>C��C�qC�qC�qC��C!�qC#�qC%�qC'�qC)�qC,p�C-=qC/��C1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCP
>CQ��CS�qCU��CW�qCY�qC[�qC]�qC_�qCa�qCc�Ce�qCg�qCi�qCk�qCm�qCo�qCr
>Cs�>Cu��Cw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz��D{o\D{�\D|o\D|�\D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�:�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�w�Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǷ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̷�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�z�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ӏA��A��QA��jA���Aǌ~A�P�A�2�A�A��iA��A���A��EA���A��?A��[AƹXAƲ�AƮ}AƩ�Aƣ�AƖ�AƓ@AƑ�AƉ7A�}�A�v�A�ncA�c�A�_;A�U2A�F�A�8�A�'�A�A�	7A���A���AŬ�AŁ�A�]dA�@�A�)�A�@A���A��&AįOAĒ�A�x�A�O�A� 'A��A�$A�4�A�2aA�@�A�M�A�d�A�l�A�q�A��A��AĜ�AÃ�A�{A��A�z�A���A��LA�T,A��BA�|A��A�,=A�6A�xA���A�a�A���A�n/A��A��\A���A���A��VA��`A���A�;�A�l�A���A�8RA���A�0!A�2aA�I�A��^A��&A���A���A�A��A��]A��A�A�dZA�c�A�L�A�$AdZAz�As�7Ao&Am�Af�A^��AW��AS�_AQ+kAN?}AMGEALn�AJ�AAH��AHG�AG�mAG+AE�@ABzxA?(�A=�<A:�A9iDA7|A6!�A533A4e�A3
=A2�)A2��A2��A1Q�A/n�A.CA.�A.Q�A.�SA."�A-��A-<�A-�A,�A,A+�YA*��A*	A*.IA*u%A*s�A*(�A)�'A(�A(j�A'�'A&�A$�MA$�A$��A$�:A$dZA$7A#�A#��A#B[A#;A"J#A!I�A!�A �^A ��A �VA X�A $A��Ao�ACAE�A@OA�AGEA�$A
=A��A6A��A�hA�AMjA�QAu%AߤAO�A�A~A��A�mA�"A~A�}A/A �A��AIRAԕA��A@A�wAzA%�A�MA4AOvA��A-wA
��A
�PA
�A
8�A
�dAqAW�A}�A��A�7A�7A>BA
�DA
��A
�A
W?A
�A
A	�}A��AJ�AZ�A�[A@�A6�A33A+kA�A��A��A!-A��A�mA�MA<6A��A�CA�VA�	A8�A �A �@A �0A �SA %@��@��@�E�@�%�@��@��C@�IR@�c @���@��E@���@��_@�q@� �@�V@�I�@��]@���@�͟@���@���@�Q@�V@��|@���@��A@��;@�@�9�@�(@��p@�z@��@킪@��@�k@�$t@��?@�Xy@�u@�t@���@�@��@��@�M�@�H@�:�@��g@��P@��?@�)�@�hs@�j@�B[@�7@�/�@��f@�S�@�_p@�;@�<�@�j�@ܿ�@��@ۼ@�@O@��[@ڬ�@�q�@�u@ٖS@���@�~@�S�@�R�@���@��@�{J@�/@��@��@��@�خ@��@Ϋ6@�0U@��]@͊	@�@@���@�Z�@˸�@˩*@ˋ�@�E9@�&@ʖ�@���@�s�@�K�@�!�@�[�@ǜ�@ǁ@�[W@�J#@��@ƕ@��K@��,@�	�@ê�@�Y�@�#�@��@Y@�
�@��@�j�@�/@���@�_�@��r@�s@��@�ff@��@���@�l�@��@���@��@�`�@�1�@�خ@��[@���@�
=@�� @�H�@��@���@�=q@�@��r@���@���@��"@�=@��Y@��S@���@��@��@���@��M@�S@���@�n�@��}@�l�@�\)@�X�@�X�@�U�@�O�@��	@�h�@�f�@��@��@���@��@�~�@��[@��@�_@���@�4@�;@��u@�J�@��A@�]�@� i@��@�D�@���@��@��@��@��@�m]@�W?@�9�@��@�q�@��@���@��@���@���@�%@�6�@��@��-@���@��S@���@��"@��~@��	@���@�~�@�x�@��@��@�ں@���@���@��@�w�@���@��@�x�@��@���@�:*@���@��w@�n/@�ߤ@�l�@�#:@�ϫ@�l�@��c@�h�@�M@�-@��m@��^@���@�L�@���@��@�}V@�{@��@@�hs@��/@�y>@�H�@�M@��g@���@�P�@�@O@�/�@�+@��@��9@��@��@�oi@�5?@��N@�w2@�7L@�@@��M@�ߤ@��h@���@�C-@�!�@��@�	�@��@���@��D@�@��-@��@��@�!-@���@�z@�8�@�˒@�s�@�(�@��@��@��B@��I@��.@��@�  @��3@���@�8�@��@��@���@�J�@�&�@��9@��n@�a�@�@@��y@���@�?@���@��z@���@�v`@�b�@�RT@�6z@��@��@��@���@���@�m�@�B[@�O@��r@�@�\�@�#�@���@���@�_@�E�@�4n@��+@��=@�b�@�P�@�:�@�)_@�+@��"@��p@��@���@�_�@�Ov@�"h@��@���@��>@��9@��P@��@�m�@�]@�g@��@qv@&@~�b@~�@}��@|��@|�@|oi@{��@{>�@z��@zp;@y��@y��@ys�@x��@x��@xQ�@w��@w��@w�:@w$t@v��@vh
@uԕ@u�7@uT�@u \@t��@s�:@r�@r��@r�1@rc @r0U@q��@qX@p�@pM@oiD@n�@n#:@m��@m:�@l�@lb@k��@kH�@k�@j�<@j��@jkQ@iw2@i+@h�`@h~(@g��@g��@g��@g"�@fff@fB[@f($@e��@ex�@d��@dFt@d�@c�m@c�@@c|�@c,�@b�F@b�@a+�@`Ft@_��@_��@_S�@_6z@^�@^�@^($@]�@]�@]��@]��@]%F@\Ɇ@\�D@\@[�a@[s@Zߤ@ZW�@Z-@Z&�@Y�X@YVm@X��@X1@W�w@W��@W��@WJ#@V��@VOv@U�@U��@U�~@UY�@U!�@U	l@T��@T�@S��@S=@S�@R��@R\�@Q�N@Q�=@Q`B@Pѷ@P�u@Pe�@O�@OZ�@N��@Ni�@M�@M��@M�@L��@L�@K�q@KiD@K33@J�@JYK@Iϫ@IrG@IX@I4@H�@H�@HPH@G]�@F��@F�}@FL0@E�D@Ea�@D�@Dm�@D�@C�m@C��@C��@Cy�@C\)@C$t@B͟@Bh
@BGE@B$�@B �@A�@A��@A=�@@�?@@	�@?�q@?v`@?=@?(@>�<@>?@=V@<�|@<�D@;�@;�
@;��@;>�@:��@::*@9�M@8�@8��@8��@8r�@8$@8  @7�@7n/@7@6H�@5�H@5<6@4�f@4�j@4��@4>B@3�q@3F�@2��@2i�@1�#@0�@0Q�@0�@/��@/��@/&@.��@.~�@.M�@.)�@. �@-�"@,�f@,�$@,�.@,$@+��@+K�@*�y@*�h@*	@)u�@(�@(z�@'�&@'��@'��@']�@';d@&�8@&�@&��@&n�@&#:@%��@%�n@%F@$�f@$Ĝ@$�@$U2@$�@#��@#�4@#;d@#�@"�M@"҉@"�@"h
@"B[@"4@!�X@!|@!8�@!@@ ��@ D�@ 6@ /�@ ,=@ "h@   @��@��@E9@�M@�@#:@�@�@�H@p�@Y�@Dg@4@&�@%@��@S�@C-@,=@��@��@{J@@O@)_@�@�b@��@n�@Ov@)�@
�@�@�t@hs@IR@2a@�@�@�@�z@��@tT@  @�;@��@�4@U�@&@�@(@��@�6@Ov@4@�9@��@�C@��@e,@@�@��@ѷ@��@��@1'@��@˒@ƨ@�0@��@W?@4�@�@��@͟@�@�@��@�A@M�@;�@1�@J@�D@��@c@^�@5�@Ĝ@��@w�@M@9X@"h@�]@�6@��@�$@|�@+@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A�ӏA��A��QA��jA���Aǌ~A�P�A�2�A�A��iA��A���A��EA���A��?A��[AƹXAƲ�AƮ}AƩ�Aƣ�AƖ�AƓ@AƑ�AƉ7A�}�A�v�A�ncA�c�A�_;A�U2A�F�A�8�A�'�A�A�	7A���A���AŬ�AŁ�A�]dA�@�A�)�A�@A���A��&AįOAĒ�A�x�A�O�A� 'A��A�$A�4�A�2aA�@�A�M�A�d�A�l�A�q�A��A��AĜ�AÃ�A�{A��A�z�A���A��LA�T,A��BA�|A��A�,=A�6A�xA���A�a�A���A�n/A��A��\A���A���A��VA��`A���A�;�A�l�A���A�8RA���A�0!A�2aA�I�A��^A��&A���A���A�A��A��]A��A�A�dZA�c�A�L�A�$AdZAz�As�7Ao&Am�Af�A^��AW��AS�_AQ+kAN?}AMGEALn�AJ�AAH��AHG�AG�mAG+AE�@ABzxA?(�A=�<A:�A9iDA7|A6!�A533A4e�A3
=A2�)A2��A2��A1Q�A/n�A.CA.�A.Q�A.�SA."�A-��A-<�A-�A,�A,A+�YA*��A*	A*.IA*u%A*s�A*(�A)�'A(�A(j�A'�'A&�A$�MA$�A$��A$�:A$dZA$7A#�A#��A#B[A#;A"J#A!I�A!�A �^A ��A �VA X�A $A��Ao�ACAE�A@OA�AGEA�$A
=A��A6A��A�hA�AMjA�QAu%AߤAO�A�A~A��A�mA�"A~A�}A/A �A��AIRAԕA��A@A�wAzA%�A�MA4AOvA��A-wA
��A
�PA
�A
8�A
�dAqAW�A}�A��A�7A�7A>BA
�DA
��A
�A
W?A
�A
A	�}A��AJ�AZ�A�[A@�A6�A33A+kA�A��A��A!-A��A�mA�MA<6A��A�CA�VA�	A8�A �A �@A �0A �SA %@��@��@�E�@�%�@��@��C@�IR@�c @���@��E@���@��_@�q@� �@�V@�I�@��]@���@�͟@���@���@�Q@�V@��|@���@��A@��;@�@�9�@�(@��p@�z@��@킪@��@�k@�$t@��?@�Xy@�u@�t@���@�@��@��@�M�@�H@�:�@��g@��P@��?@�)�@�hs@�j@�B[@�7@�/�@��f@�S�@�_p@�;@�<�@�j�@ܿ�@��@ۼ@�@O@��[@ڬ�@�q�@�u@ٖS@���@�~@�S�@�R�@���@��@�{J@�/@��@��@��@�خ@��@Ϋ6@�0U@��]@͊	@�@@���@�Z�@˸�@˩*@ˋ�@�E9@�&@ʖ�@���@�s�@�K�@�!�@�[�@ǜ�@ǁ@�[W@�J#@��@ƕ@��K@��,@�	�@ê�@�Y�@�#�@��@Y@�
�@��@�j�@�/@���@�_�@��r@�s@��@�ff@��@���@�l�@��@���@��@�`�@�1�@�خ@��[@���@�
=@�� @�H�@��@���@�=q@�@��r@���@���@��"@�=@��Y@��S@���@��@��@���@��M@�S@���@�n�@��}@�l�@�\)@�X�@�X�@�U�@�O�@��	@�h�@�f�@��@��@���@��@�~�@��[@��@�_@���@�4@�;@��u@�J�@��A@�]�@� i@��@�D�@���@��@��@��@��@�m]@�W?@�9�@��@�q�@��@���@��@���@���@�%@�6�@��@��-@���@��S@���@��"@��~@��	@���@�~�@�x�@��@��@�ں@���@���@��@�w�@���@��@�x�@��@���@�:*@���@��w@�n/@�ߤ@�l�@�#:@�ϫ@�l�@��c@�h�@�M@�-@��m@��^@���@�L�@���@��@�}V@�{@��@@�hs@��/@�y>@�H�@�M@��g@���@�P�@�@O@�/�@�+@��@��9@��@��@�oi@�5?@��N@�w2@�7L@�@@��M@�ߤ@��h@���@�C-@�!�@��@�	�@��@���@��D@�@��-@��@��@�!-@���@�z@�8�@�˒@�s�@�(�@��@��@��B@��I@��.@��@�  @��3@���@�8�@��@��@���@�J�@�&�@��9@��n@�a�@�@@��y@���@�?@���@��z@���@�v`@�b�@�RT@�6z@��@��@��@���@���@�m�@�B[@�O@��r@�@�\�@�#�@���@���@�_@�E�@�4n@��+@��=@�b�@�P�@�:�@�)_@�+@��"@��p@��@���@�_�@�Ov@�"h@��@���@��>@��9@��P@��@�m�@�]@�g@��@qv@&@~�b@~�@}��@|��@|�@|oi@{��@{>�@z��@zp;@y��@y��@ys�@x��@x��@xQ�@w��@w��@w�:@w$t@v��@vh
@uԕ@u�7@uT�@u \@t��@s�:@r�@r��@r�1@rc @r0U@q��@qX@p�@pM@oiD@n�@n#:@m��@m:�@l�@lb@k��@kH�@k�@j�<@j��@jkQ@iw2@i+@h�`@h~(@g��@g��@g��@g"�@fff@fB[@f($@e��@ex�@d��@dFt@d�@c�m@c�@@c|�@c,�@b�F@b�@a+�@`Ft@_��@_��@_S�@_6z@^�@^�@^($@]�@]�@]��@]��@]%F@\Ɇ@\�D@\@[�a@[s@Zߤ@ZW�@Z-@Z&�@Y�X@YVm@X��@X1@W�w@W��@W��@WJ#@V��@VOv@U�@U��@U�~@UY�@U!�@U	l@T��@T�@S��@S=@S�@R��@R\�@Q�N@Q�=@Q`B@Pѷ@P�u@Pe�@O�@OZ�@N��@Ni�@M�@M��@M�@L��@L�@K�q@KiD@K33@J�@JYK@Iϫ@IrG@IX@I4@H�@H�@HPH@G]�@F��@F�}@FL0@E�D@Ea�@D�@Dm�@D�@C�m@C��@C��@Cy�@C\)@C$t@B͟@Bh
@BGE@B$�@B �@A�@A��@A=�@@�?@@	�@?�q@?v`@?=@?(@>�<@>?@=V@<�|@<�D@;�@;�
@;��@;>�@:��@::*@9�M@8�@8��@8��@8r�@8$@8  @7�@7n/@7@6H�@5�H@5<6@4�f@4�j@4��@4>B@3�q@3F�@2��@2i�@1�#@0�@0Q�@0�@/��@/��@/&@.��@.~�@.M�@.)�@. �@-�"@,�f@,�$@,�.@,$@+��@+K�@*�y@*�h@*	@)u�@(�@(z�@'�&@'��@'��@']�@';d@&�8@&�@&��@&n�@&#:@%��@%�n@%F@$�f@$Ĝ@$�@$U2@$�@#��@#�4@#;d@#�@"�M@"҉@"�@"h
@"B[@"4@!�X@!|@!8�@!@@ ��@ D�@ 6@ /�@ ,=@ "h@   @��@��@E9@�M@�@#:@�@�@�H@p�@Y�@Dg@4@&�@%@��@S�@C-@,=@��@��@{J@@O@)_@�@�b@��@n�@Ov@)�@
�@�@�t@hs@IR@2a@�@�@�@�z@��@tT@  @�;@��@�4@U�@&@�@(@��@�6@Ov@4@�9@��@�C@��@e,@@�@��@ѷ@��@��@1'@��@˒@ƨ@�0@��@W?@4�@�@��@͟@�@�@��@�A@M�@;�@1�@J@�D@��@c@^�@5�@Ĝ@��@w�@M@9X@"h@�]@�6@��@�$@|�@+@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
%zB
$�B
$ZB
$B
$B
$@B
#TB
"�B
 �B
�B
�B
�B
�B
]B
�B
#B
�B
7B
B
�B
1B
�B
B
�B
mB
B
gB
FB
�B
�B
�B
B
HB
B
�B
�B

=B
fB
�B
 B	��B	�rB	�tB	�B	�'B	�;B	�B	�WB	�B	�sB	��B	��B	�B	�tB	��B	��B	�WB	�B	�B	��B	�B	��B
M�B
��B
�=B
�RB
ںB
��B�B�B�B,"BB[B_�Bi�BxB�1B��B��B��B�eB��B�B�'B�}B��B�WB�B�!B��B��B�kB��B�CB�B��B��B�OBdBUBYKBgRBV9B(�B
��B
��B
c�B
K�B
&�B
?B
.B	�9B	ǔB	��B	��B	m�B	E9B	$�B	IB	SB	[B	FB	�B	%FB	6�B	:�B	=�B	?.B	BuB	LB	a�B	nB	�kB	��B	�9B	��B	�B	��B
�B
B
#�B
2�B
3�B
1�B
'�B
-CB
5�B
=<B
B'B
F�B
JXB
O(B
R�B
TB
UgB
RoB
LJB
X_B
b�B
c�B
c�B
k�B
iB
fLB
c�B
_�B
ZB
Z�B
[	B
Z�B
Z�B
Y�B
X�B
WsB
W�B
ZQB
X�B
S@B
UMB
TFB
U�B
UMB
S�B
RB
PB
M�B
J�B
DgB
=B
88B
1B
.}B
/�B
"�B
�B
0!B
/OB
%,B
�B
�B
�B
hB
�B
�B
�B
�B
JB
�B
�B
�B
�B
3B
aB
B	�cB	�B	��B	�xB	��B	��B	��B	�HB
�B
YB
�B
_B
6B
"B
{B
#�B
-�B
3�B
7LB
7�B
8B
7�B
7LB
6FB
7LB
7�B
7�B
7�B
7LB
6�B
0�B
"�B
�B
�B
�B
/B
B
OB
�B
B
�B
sB
�B
�B
{B
SB
_B
�B
�B
�B
�B
�B
�B
B
 �B
 �B
pB
jB
�B
IB
�B
�B
�B
#B
�B
qB
�B
�B
jB
B
5B
�B
�B
�B
�B
�B
qB
B
KB
_B
�B
�B
�B
aB
,B
�B
�B
uB
�B
@B
�B
�B
 B
�B
HB
�B
�B
(B
VB
�B
�B
�B
�B
�B
�B
�B
jB
6B
dB
�B

�B

�B
	�B
	RB
�B
B
�B
�B
�B
�B
UB
 �B
 �B
 iB
 B	��B	��B	��B
 iB	��B	�.B	��B	�jB	��B	�*B	��B	�	B	�lB	�fB	�FB	�B	�9B	�hB	�3B	�B	�B	��B	�nB	�+B	�B	�	B	��B	�$B	�rB	��B	��B	��B	��B	�B	��B	�(B	�wB	��B	��B	�]B	�dB	�B	�6B	�qB	�B	�B	�wB	��B	�}B	��B	��B	��B
 iB
 �B
 B
�B
�B
[B
[B
'B
�B
�B
�B
�B
uB
uB
�B
[B
�B
�B
[B
�B
�B
B
�B
�B
�B
�B
{B
�B
�B
�B
B
YB
B
�B
�B
B
zB
�B
_B
�B
fB
fB
fB
KB
1B
�B
1B
fB
	�B

	B
	�B

#B

�B
DB
�B
�B
�B
�B
�B
�B
6B
jB
�B
B
<B
�B
�B
(B
vB
.B
B
B
.B
�B
�B
 B
B
�B
�B
.B
�B
bB
�B
.B
hB
�B
oB
�B
uB
�B
�B
B
FB
aB
{B
2B
�B
B
MB
gB
�B
�B
mB
�B
SB
�B
�B
�B
�B
�B
?B
+B
B
�B
sB
�B
_B
�B
�B
B
�B
�B
�B
WB
�B
�B
�B
xB
~B
�B
B
5B
�B
�B
;B
pB
pB
pB
VB
pB
�B
�B
�B
�B
�B
�B
 \B
 �B
 �B
!HB
!bB
!|B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#nB
#TB
#:B
#TB
#�B
#�B
$@B
$@B
$�B
%B
%FB
%FB
%`B
%�B
%�B
%�B
%zB
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
($B
(sB
(�B
(�B
)�B
)�B
)�B
*KB
*�B
+B
+QB
+kB
+QB
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,"B
,=B
,WB
,WB
,qB
,�B
-B
,�B
,�B
,�B
,�B
,�B
-)B
-)B
-CB
-]B
-)B
-)B
-CB
-)B
-]B
-�B
-�B
-�B
-�B
./B
.}B
.�B
/B
/�B
0!B
0oB
0�B
0�B
0�B
0�B
0�B
0oB
0UB
0�B
0�B
1vB
1�B
1�B
2-B
2�B
2�B
33B
4B
4B
49B
4�B
4�B
5%B
5tB
5tB
5�B
6+B
6�B
6�B
7�B
7�B
7�B
7�B
8lB
9rB
9�B
9�B
9�B
9�B
9�B
:B
:xB
:�B
;�B
<�B
="B
=�B
=�B
>]B
>]B
>�B
>�B
?cB
?cB
?�B
?�B
?�B
@�B
@�B
@�B
A B
A B
A B
AoB
A�B
B�B
BuB
BuB
B[B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
EmB
E�B
FB
FtB
F�B
FtB
F�B
F�B
GzB
G_B
G_B
G_B
G�B
G�B
G�B
HB
H�B
H�B
H�B
IB
IlB
I�B
IlB
I�B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
KDB
K�B
L0B
LJB
LJB
LdB
L~B
LdB
L�B
MPB
MB
M�B
MjB
M�B
N<B
N�B
N�B
N�B
OB
O(B
OB
O\B
O�B
P.B
PbB
P�B
P}B
Q4B
QB
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
SB
S�B
TFB
T,B
TFB
TaB
T�B
UB
U�B
VB
V9B
VSB
VmB
V�B
V�B
V�B
V�B
WsB
XB
XB
X+B
X+B
XyB
XyB
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
[qB
[	B
[qB
[�B
[�B
[�B
\B
\�B
\�B
]�B
^B
^B
^5B
^5B
^jB
^jB
^�B
^�B
^�B
_VB
_�B
`B
`'B
`\B
`vB
`�B
`�B
aHB
a-B
a�B
a�B
b�B
c B
cnB
c�B
c�B
c�B
dZB
dtB
d�B
d�B
d�B
e,B
ezB
ezB
ezB
e�B
fLB
f�B
f�B
f�B
gmB
g�B
g�B
h>B
h�B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
j0B
jKB
jB
j�B
kB
k6B
k�B
k�B
k�B
k�B
l"B
lWB
l=B
lqB
l�B
l�B
l�B
l�B
mB
mwB
m�B
m�B
m�B
n/B
n}B
n}B
n}B
n}B
n}B
n�B
n�B
o B
oB
oOB
o�B
p!B
p;B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
qvB
qvB
q�B
q�B
rB
raB
raB
r�B
r�B
r�B
r�B
sB
s3B
s3B
sMB
s�B
s�B
s�B
tB
tB
tTB
tTB
tnB
tnB
t�B
u?B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
wLB
wfB
w�B
w�B
xB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y>B
yrB
yrB
yrB
y�B
y�B
y�B
zB
zDB
zDB
z�B
z�B
{B
{0B
{0B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
%zB
$�B
$ZB
$B
$B
$@B
#TB
"�B
 �B
�B
�B
�B
�B
]B
�B
#B
�B
7B
B
�B
1B
�B
B
�B
mB
B
gB
FB
�B
�B
�B
B
HB
B
�B
�B

=B
fB
�B
 B	��B	�rB	�tB	�B	�'B	�;B	�B	�WB	�B	�sB	��B	��B	�B	�tB	��B	��B	�WB	�B	�B	��B	�B	��B
M�B
��B
�=B
�RB
ںB
��B�B�B�B,"BB[B_�Bi�BxB�1B��B��B��B�eB��B�B�'B�}B��B�WB�B�!B��B��B�kB��B�CB�B��B��B�OBdBUBYKBgRBV9B(�B
��B
��B
c�B
K�B
&�B
?B
.B	�9B	ǔB	��B	��B	m�B	E9B	$�B	IB	SB	[B	FB	�B	%FB	6�B	:�B	=�B	?.B	BuB	LB	a�B	nB	�kB	��B	�9B	��B	�B	��B
�B
B
#�B
2�B
3�B
1�B
'�B
-CB
5�B
=<B
B'B
F�B
JXB
O(B
R�B
TB
UgB
RoB
LJB
X_B
b�B
c�B
c�B
k�B
iB
fLB
c�B
_�B
ZB
Z�B
[	B
Z�B
Z�B
Y�B
X�B
WsB
W�B
ZQB
X�B
S@B
UMB
TFB
U�B
UMB
S�B
RB
PB
M�B
J�B
DgB
=B
88B
1B
.}B
/�B
"�B
�B
0!B
/OB
%,B
�B
�B
�B
hB
�B
�B
�B
�B
JB
�B
�B
�B
�B
3B
aB
B	�cB	�B	��B	�xB	��B	��B	��B	�HB
�B
YB
�B
_B
6B
"B
{B
#�B
-�B
3�B
7LB
7�B
8B
7�B
7LB
6FB
7LB
7�B
7�B
7�B
7LB
6�B
0�B
"�B
�B
�B
�B
/B
B
OB
�B
B
�B
sB
�B
�B
{B
SB
_B
�B
�B
�B
�B
�B
�B
B
 �B
 �B
pB
jB
�B
IB
�B
�B
�B
#B
�B
qB
�B
�B
jB
B
5B
�B
�B
�B
�B
�B
qB
B
KB
_B
�B
�B
�B
aB
,B
�B
�B
uB
�B
@B
�B
�B
 B
�B
HB
�B
�B
(B
VB
�B
�B
�B
�B
�B
�B
�B
jB
6B
dB
�B

�B

�B
	�B
	RB
�B
B
�B
�B
�B
�B
UB
 �B
 �B
 iB
 B	��B	��B	��B
 iB	��B	�.B	��B	�jB	��B	�*B	��B	�	B	�lB	�fB	�FB	�B	�9B	�hB	�3B	�B	�B	��B	�nB	�+B	�B	�	B	��B	�$B	�rB	��B	��B	��B	��B	�B	��B	�(B	�wB	��B	��B	�]B	�dB	�B	�6B	�qB	�B	�B	�wB	��B	�}B	��B	��B	��B
 iB
 �B
 B
�B
�B
[B
[B
'B
�B
�B
�B
�B
uB
uB
�B
[B
�B
�B
[B
�B
�B
B
�B
�B
�B
�B
{B
�B
�B
�B
B
YB
B
�B
�B
B
zB
�B
_B
�B
fB
fB
fB
KB
1B
�B
1B
fB
	�B

	B
	�B

#B

�B
DB
�B
�B
�B
�B
�B
�B
6B
jB
�B
B
<B
�B
�B
(B
vB
.B
B
B
.B
�B
�B
 B
B
�B
�B
.B
�B
bB
�B
.B
hB
�B
oB
�B
uB
�B
�B
B
FB
aB
{B
2B
�B
B
MB
gB
�B
�B
mB
�B
SB
�B
�B
�B
�B
�B
?B
+B
B
�B
sB
�B
_B
�B
�B
B
�B
�B
�B
WB
�B
�B
�B
xB
~B
�B
B
5B
�B
�B
;B
pB
pB
pB
VB
pB
�B
�B
�B
�B
�B
�B
 \B
 �B
 �B
!HB
!bB
!|B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#nB
#TB
#:B
#TB
#�B
#�B
$@B
$@B
$�B
%B
%FB
%FB
%`B
%�B
%�B
%�B
%zB
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
($B
(sB
(�B
(�B
)�B
)�B
)�B
*KB
*�B
+B
+QB
+kB
+QB
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,"B
,=B
,WB
,WB
,qB
,�B
-B
,�B
,�B
,�B
,�B
,�B
-)B
-)B
-CB
-]B
-)B
-)B
-CB
-)B
-]B
-�B
-�B
-�B
-�B
./B
.}B
.�B
/B
/�B
0!B
0oB
0�B
0�B
0�B
0�B
0�B
0oB
0UB
0�B
0�B
1vB
1�B
1�B
2-B
2�B
2�B
33B
4B
4B
49B
4�B
4�B
5%B
5tB
5tB
5�B
6+B
6�B
6�B
7�B
7�B
7�B
7�B
8lB
9rB
9�B
9�B
9�B
9�B
9�B
:B
:xB
:�B
;�B
<�B
="B
=�B
=�B
>]B
>]B
>�B
>�B
?cB
?cB
?�B
?�B
?�B
@�B
@�B
@�B
A B
A B
A B
AoB
A�B
B�B
BuB
BuB
B[B
B�B
C-B
C{B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
EmB
E�B
FB
FtB
F�B
FtB
F�B
F�B
GzB
G_B
G_B
G_B
G�B
G�B
G�B
HB
H�B
H�B
H�B
IB
IlB
I�B
IlB
I�B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
KDB
K�B
L0B
LJB
LJB
LdB
L~B
LdB
L�B
MPB
MB
M�B
MjB
M�B
N<B
N�B
N�B
N�B
OB
O(B
OB
O\B
O�B
P.B
PbB
P�B
P}B
Q4B
QB
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
RoB
R�B
R�B
R�B
SB
S�B
TFB
T,B
TFB
TaB
T�B
UB
U�B
VB
V9B
VSB
VmB
V�B
V�B
V�B
V�B
WsB
XB
XB
X+B
X+B
XyB
XyB
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
[qB
[	B
[qB
[�B
[�B
[�B
\B
\�B
\�B
]�B
^B
^B
^5B
^5B
^jB
^jB
^�B
^�B
^�B
_VB
_�B
`B
`'B
`\B
`vB
`�B
`�B
aHB
a-B
a�B
a�B
b�B
c B
cnB
c�B
c�B
c�B
dZB
dtB
d�B
d�B
d�B
e,B
ezB
ezB
ezB
e�B
fLB
f�B
f�B
f�B
gmB
g�B
g�B
h>B
h�B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
j0B
jKB
jB
j�B
kB
k6B
k�B
k�B
k�B
k�B
l"B
lWB
l=B
lqB
l�B
l�B
l�B
l�B
mB
mwB
m�B
m�B
m�B
n/B
n}B
n}B
n}B
n}B
n}B
n�B
n�B
o B
oB
oOB
o�B
p!B
p;B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
qvB
qvB
q�B
q�B
rB
raB
raB
r�B
r�B
r�B
r�B
sB
s3B
s3B
sMB
s�B
s�B
s�B
tB
tB
tTB
tTB
tnB
tnB
t�B
u?B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
wLB
wfB
w�B
w�B
xB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y>B
yrB
yrB
yrB
y�B
y�B
y�B
zB
zDB
zDB
z�B
z�B
{B
{0B
{0B
{JB
{�B
{�B
{�B
{�B
|B
|6B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105244  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192423  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192424  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192424                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042431  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042431  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                