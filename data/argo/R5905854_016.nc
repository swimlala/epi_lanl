CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:47:13Z creation;2022-06-04T17:47:14Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174713  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�̥ffff1   @�̥��X^@,��
=q�c�KƧ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A~ffA�33A�33A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BF��BPffBX  B_��Bh  Bp  Bx  B�33B���B�ffB���B���B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB���B㙚B�  B�  B�  B�  B�  B�  C   C  C33C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0�C2�C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb33Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ DɃ3D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @7�@�=q@�=qA�A!�AA�Aa�A�A�A�A��\A��\AЏ\A��\A��\B G�BG�B�BG�B G�B(G�B0G�B8G�B@G�BG{BP�BXG�B_�HBhG�BpG�BxG�B�W
B��qB��=B��qB��B�#�B�#�B�#�B�#�B�#�B�#�B��=B�W
B�#�B�#�B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B܊=B��B�qB�#�B�#�B�#�B�#�B�#�B�#�C �C�CEC�RC�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C%�RC(�C*�C,�C.�C0+�C2+�C4�C5�RC8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`+�CbECc�RCf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cu�RCx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D��D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Df�Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Ds�Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D�
D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�pD�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=DɅpD��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D�
D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�A�bA��A�(A�$A�xA��A�IA�&A�'�A�(�A�($A�)*A�*0A�%zA�'RA� �A�A�$tA�+A�0UA��A��A��Aڨ�A�#:A�F�A��AҜ�AҒoA�VmA�8�A��A�qAA���A�!bA��.A�7A�,Aŏ\A��	A�,qAA��A���A��A��A�+A�J�A��A���A�I�A�C-A�R�A�qA�[#A�?�A���A�($A���A�'RA��pA��LA�FA��A�EA���A�YKA��A�[WA��A���A��5A�0�A�FA�C�A�>wA�B'A�`BA��A���A�I�A��A��A�F?A�l"A}tTA{��Ay��Ax�BAu(�Am��Al�HAk�gAh��Ab�vA_o A^l�A\�0AY�HAX�AW�8AU�5AQ��AO�eANAKAF��AB\�A?�A;�A8�,A7� A6�AA4҉A4%�A3VA1�"A1�A0�QA0��A0��A/;�A,��A,\�A+��A*��A)��A(�/A&�A%�1A$�9A$A�A#��A$�A$��A$�5A#��A qA ��A qvA=A�_A��A1�A�A�2A��A��A�A�}A)_A�A��A�TA�<A>�A�xA��A��AO�AA��A�A�hA��A4AT�A	lA��Ax�A�LA�+A� A��A`�AA�A�A�pAf�A@�A
�[A	�/A	r�A	�A��AT�A��A\�A҉A�|AffA�A��A�AT�AxA�)AxA�>A�\A�aA��A|�As�Ag8AHA�A �VA PHA �@�-w@�h
@���@�Q�@�RT@�v�@��t@��@��m@���@��u@�c�@�'R@�=@�!�@�RT@���@�q�@��}@��@�@��@�q�@�S&@�h�@���@�/�@��m@���@��@�*0@�L@��@�@��@�l�@��A@癚@�8�@�F�@�~�@�0@��@�^�@�b@�j�@��@�@��H@�M�@���@�X@���@�N�@���@߁�@�@ާ�@�g8@���@݉7@�+@ܧ@�@ژ_@��@ى7@�~�@��@�e�@�o@֌@�x@Յ@�8�@��U@ӏ�@��@��[@�e�@���@�g�@�	l@Ї�@�1�@Ϡ'@ϊ	@��v@�kQ@���@�RT@��5@̂A@�!�@��@ʉ�@�
�@�@�?�@Ǭq@�8�@��@žw@�2a@��5@�!�@��@ñ[@Â�@���@¥z@�_�@��@�y�@���@��@�,=@��@��@���@��@�_@�o�@��[@���@�@�@�
�@���@���@���@���@�q�@�R�@�I�@�6�@���@�v`@�'�@��c@��@�c�@��@��@���@�c�@�U�@�=@�kQ@���@��P@�]�@��@���@���@�z�@�Q@���@��M@�\)@�!�@���@�h
@�2�@���@�;d@��	@��9@�PH@�e@��@��h@�j@��@���@��.@�w2@��R@�5?@���@�IR@���@�#:@�G�@��)@���@�kQ@�=q@��r@��-@�^�@��@�Q�@���@���@�=@��K@���@�j@��)@��K@��@���@��@���@���@�Ta@�($@��@���@�RT@�'�@���@�֡@���@�e�@��@��@���@�T�@�1�@��@��z@�[�@�4n@��o@���@�Q�@��@��@��r@�1'@��;@���@�%@��!@�tT@�:�@�{@���@���@�>�@��p@�:*@�خ@���@��P@�e,@���@���@�v�@�&�@��)@���@�a�@�@�ߤ@���@��@���@��9@��@�>B@��@��@�X�@�G�@�1�@���@�҉@���@�O@��@��@���@��@��m@�c @�$�@��@��o@�� @���@���@��S@��4@�[W@�!�@��@��1@�H�@��@��K@���@�y�@���@�?@��]@���@�zx@�4@��@���@��v@���@���@�$�@��@���@�b�@�C@��@�S@���@�h
@�e�@�Q@�)�@�@��=@�*0@��@��@���@�~�@�GE@��@���@���@��q@���@��	@�Dg@�;@���@���@��u@�^5@�@�@�'R@�@�ƨ@��q@���@�8�@��h@���@�;�@���@���@�4�@���@��[@��<@�s�@�7�@�@�@6z@~{�@}�T@}�@}F@|��@|_@|-�@|x@{�f@{
=@z�F@z&�@y�)@y�3@y[W@x��@xr�@x-�@w�A@w��@wg�@wO@v��@vL0@u��@u��@t�[@tw�@tx@s~�@sH�@sY@r��@r�<@rC�@q�>@q�@q�@qx�@p�|@pN�@ox@oMj@n�@nv�@n	@mm]@l��@l�@l[�@k�W@kJ#@j��@j!�@i�@i�S@i-w@h��@hQ�@h"h@h@hb@g�g@gF�@f�@f��@f^5@f�@e�C@e�@eA @d�@d4n@cv`@cH�@b�L@b_@a}�@`�U@`A�@_�@_˒@_iD@_>�@_33@_�@^�'@^_�@^Ta@^R�@^Ta@]�@]�~@]2a@\��@\�@\��@\Ft@\-�@\'R@\@[x@[;d@Z�,@Z�A@Ze@Y��@YL�@Y0�@X�5@X��@X�@W�*@Wn/@W�@V�}@V-@U�#@Uc�@T�?@S�@S�{@SH�@R�@R{�@RGE@Q�>@Q`B@Pی@P��@PbN@O�}@O>�@N��@Ni�@N_�@N^5@N.�@M�d@Mw2@M�@L��@L_@K��@Kqv@K"�@Jں@Ju@IY�@H�|@H�O@He�@H1'@G��@G�@@Gn/@G.I@F��@F}V@F4@E�h@D�P@D��@D/�@C��@C�@@B�X@Bh
@B�@Aԕ@A�9@A�3@ArG@AS&@A-w@@�@@Ĝ@@j@?��@?�f@?g�@?;d@?.I@>҉@>}V@>$�@=�>@=��@=zx@=T�@=;@<h�@<  @;�+@;�@;�q@;RT@;+@;$t@;Y@:�@:�h@:u%@9ԕ@9x�@8�P@8��@8�$@8��@8��@8y>@8g8@7�@7��@7��@7�@7��@7W?@6�"@6�r@6u@5��@58�@5�@4�v@4�O@4oi@4N�@3خ@3P�@2��@2i�@2Ta@2E�@2�@1�@1=�@0�$@0oi@07�@/��@/�V@/�@/>�@.�"@.�b@.�r@.?@-�H@-�'@-w2@-@,��@,l"@+�]@+��@+~�@+a@+C�@+@*�b@)�@)zx@)c�@)^�@(��@(��@(tT@(:�@'�+@'�f@'RT@'�@&�}@&p;@&M�@&5?@&)�@&�@%�@%`B@%8�@%0�@%=�@%J�@%%@$�@$�@$z�@#��@#�4@#l�@#�@"�@"��@"C�@"�@!��@!��@!e,@!:�@!q@ �@ Ɇ@ �)@ �Y@ Z@�@��@U�@RT@@�@�,@�'@��@�+@J�@#:@_@��@�3@�M@?}@�u@V�@7�@I�@-�@�@�@�@�@�Q@�
@�@�V@n/@�@6�@R�@B[@&�@!�@�j@}�@&�@+@7L@(�@��@	�@��@�	@�4@��@��@e�@"�@�@S@�y@��@}V@a|@W�@	@ �@��@�9@��@5�@�@�@�@y>@A�@"h@�]@�@@Z�@�@��@��@�!@�@~�@}V@kQ@u%@}V@{�@ff@�@�@��@e,@V@��@Z@(�@�@�r@dZ@E9@4�@.I@�@��@s�@�@_@�N@�z@�z@�@��@�X@��@rG@*0@�@��@֡@�@|�@%�@�]@�Q@�V@��@x@_p@K�@>�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�A�bA��A�(A�$A�xA��A�IA�&A�'�A�(�A�($A�)*A�*0A�%zA�'RA� �A�A�$tA�+A�0UA��A��A��Aڨ�A�#:A�F�A��AҜ�AҒoA�VmA�8�A��A�qAA���A�!bA��.A�7A�,Aŏ\A��	A�,qAA��A���A��A��A�+A�J�A��A���A�I�A�C-A�R�A�qA�[#A�?�A���A�($A���A�'RA��pA��LA�FA��A�EA���A�YKA��A�[WA��A���A��5A�0�A�FA�C�A�>wA�B'A�`BA��A���A�I�A��A��A�F?A�l"A}tTA{��Ay��Ax�BAu(�Am��Al�HAk�gAh��Ab�vA_o A^l�A\�0AY�HAX�AW�8AU�5AQ��AO�eANAKAF��AB\�A?�A;�A8�,A7� A6�AA4҉A4%�A3VA1�"A1�A0�QA0��A0��A/;�A,��A,\�A+��A*��A)��A(�/A&�A%�1A$�9A$A�A#��A$�A$��A$�5A#��A qA ��A qvA=A�_A��A1�A�A�2A��A��A�A�}A)_A�A��A�TA�<A>�A�xA��A��AO�AA��A�A�hA��A4AT�A	lA��Ax�A�LA�+A� A��A`�AA�A�A�pAf�A@�A
�[A	�/A	r�A	�A��AT�A��A\�A҉A�|AffA�A��A�AT�AxA�)AxA�>A�\A�aA��A|�As�Ag8AHA�A �VA PHA �@�-w@�h
@���@�Q�@�RT@�v�@��t@��@��m@���@��u@�c�@�'R@�=@�!�@�RT@���@�q�@��}@��@�@��@�q�@�S&@�h�@���@�/�@��m@���@��@�*0@�L@��@�@��@�l�@��A@癚@�8�@�F�@�~�@�0@��@�^�@�b@�j�@��@�@��H@�M�@���@�X@���@�N�@���@߁�@�@ާ�@�g8@���@݉7@�+@ܧ@�@ژ_@��@ى7@�~�@��@�e�@�o@֌@�x@Յ@�8�@��U@ӏ�@��@��[@�e�@���@�g�@�	l@Ї�@�1�@Ϡ'@ϊ	@��v@�kQ@���@�RT@��5@̂A@�!�@��@ʉ�@�
�@�@�?�@Ǭq@�8�@��@žw@�2a@��5@�!�@��@ñ[@Â�@���@¥z@�_�@��@�y�@���@��@�,=@��@��@���@��@�_@�o�@��[@���@�@�@�
�@���@���@���@���@�q�@�R�@�I�@�6�@���@�v`@�'�@��c@��@�c�@��@��@���@�c�@�U�@�=@�kQ@���@��P@�]�@��@���@���@�z�@�Q@���@��M@�\)@�!�@���@�h
@�2�@���@�;d@��	@��9@�PH@�e@��@��h@�j@��@���@��.@�w2@��R@�5?@���@�IR@���@�#:@�G�@��)@���@�kQ@�=q@��r@��-@�^�@��@�Q�@���@���@�=@��K@���@�j@��)@��K@��@���@��@���@���@�Ta@�($@��@���@�RT@�'�@���@�֡@���@�e�@��@��@���@�T�@�1�@��@��z@�[�@�4n@��o@���@�Q�@��@��@��r@�1'@��;@���@�%@��!@�tT@�:�@�{@���@���@�>�@��p@�:*@�خ@���@��P@�e,@���@���@�v�@�&�@��)@���@�a�@�@�ߤ@���@��@���@��9@��@�>B@��@��@�X�@�G�@�1�@���@�҉@���@�O@��@��@���@��@��m@�c @�$�@��@��o@�� @���@���@��S@��4@�[W@�!�@��@��1@�H�@��@��K@���@�y�@���@�?@��]@���@�zx@�4@��@���@��v@���@���@�$�@��@���@�b�@�C@��@�S@���@�h
@�e�@�Q@�)�@�@��=@�*0@��@��@���@�~�@�GE@��@���@���@��q@���@��	@�Dg@�;@���@���@��u@�^5@�@�@�'R@�@�ƨ@��q@���@�8�@��h@���@�;�@���@���@�4�@���@��[@��<@�s�@�7�@�@�@6z@~{�@}�T@}�@}F@|��@|_@|-�@|x@{�f@{
=@z�F@z&�@y�)@y�3@y[W@x��@xr�@x-�@w�A@w��@wg�@wO@v��@vL0@u��@u��@t�[@tw�@tx@s~�@sH�@sY@r��@r�<@rC�@q�>@q�@q�@qx�@p�|@pN�@ox@oMj@n�@nv�@n	@mm]@l��@l�@l[�@k�W@kJ#@j��@j!�@i�@i�S@i-w@h��@hQ�@h"h@h@hb@g�g@gF�@f�@f��@f^5@f�@e�C@e�@eA @d�@d4n@cv`@cH�@b�L@b_@a}�@`�U@`A�@_�@_˒@_iD@_>�@_33@_�@^�'@^_�@^Ta@^R�@^Ta@]�@]�~@]2a@\��@\�@\��@\Ft@\-�@\'R@\@[x@[;d@Z�,@Z�A@Ze@Y��@YL�@Y0�@X�5@X��@X�@W�*@Wn/@W�@V�}@V-@U�#@Uc�@T�?@S�@S�{@SH�@R�@R{�@RGE@Q�>@Q`B@Pی@P��@PbN@O�}@O>�@N��@Ni�@N_�@N^5@N.�@M�d@Mw2@M�@L��@L_@K��@Kqv@K"�@Jں@Ju@IY�@H�|@H�O@He�@H1'@G��@G�@@Gn/@G.I@F��@F}V@F4@E�h@D�P@D��@D/�@C��@C�@@B�X@Bh
@B�@Aԕ@A�9@A�3@ArG@AS&@A-w@@�@@Ĝ@@j@?��@?�f@?g�@?;d@?.I@>҉@>}V@>$�@=�>@=��@=zx@=T�@=;@<h�@<  @;�+@;�@;�q@;RT@;+@;$t@;Y@:�@:�h@:u%@9ԕ@9x�@8�P@8��@8�$@8��@8��@8y>@8g8@7�@7��@7��@7�@7��@7W?@6�"@6�r@6u@5��@58�@5�@4�v@4�O@4oi@4N�@3خ@3P�@2��@2i�@2Ta@2E�@2�@1�@1=�@0�$@0oi@07�@/��@/�V@/�@/>�@.�"@.�b@.�r@.?@-�H@-�'@-w2@-@,��@,l"@+�]@+��@+~�@+a@+C�@+@*�b@)�@)zx@)c�@)^�@(��@(��@(tT@(:�@'�+@'�f@'RT@'�@&�}@&p;@&M�@&5?@&)�@&�@%�@%`B@%8�@%0�@%=�@%J�@%%@$�@$�@$z�@#��@#�4@#l�@#�@"�@"��@"C�@"�@!��@!��@!e,@!:�@!q@ �@ Ɇ@ �)@ �Y@ Z@�@��@U�@RT@@�@�,@�'@��@�+@J�@#:@_@��@�3@�M@?}@�u@V�@7�@I�@-�@�@�@�@�@�Q@�
@�@�V@n/@�@6�@R�@B[@&�@!�@�j@}�@&�@+@7L@(�@��@	�@��@�	@�4@��@��@e�@"�@�@S@�y@��@}V@a|@W�@	@ �@��@�9@��@5�@�@�@�@y>@A�@"h@�]@�@@Z�@�@��@��@�!@�@~�@}V@kQ@u%@}V@{�@ff@�@�@��@e,@V@��@Z@(�@�@�r@dZ@E9@4�@.I@�@��@s�@�@_@�N@�z@�z@�@��@�X@��@rG@*0@�@��@֡@�@|�@%�@�]@�Q@�V@��@x@_p@K�@>�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	d&B	c�B	d&B	c�B	c�B	c�B	cnB	b�B	b4B	cB	c�B	c�B	^�B	Q�B	F%B	@B	L�B	�B	�#B	�=B	�cB	�sB	�xB	�BB	�+B	��B	�?B	�yB	��B
�B
'8B
;dB
�B
�FB
ȀB
уB
�B
�cB�B~B4�B;dBG�BR BW�BQ�BVmBg8BgBf�B`�B\�Ba�BncBv+B`BB3�B�B*�B$B;B�BxB
�qB
��B
�B
��B
��B
wB
]�B
.}B
�B
JB
�B
�B	��B
^B
 �B	��B	�ZB	��B	ňB	�eB	��B	�B	|PB	c�B	QNB	JXB	FtB	>�B	7�B	2�B	)DB	yB	�B		�B��B�UB��B�B�=B�\B�|B�B��B�tB�RB�WB�B�CB��B	^B	�B		�B	�B	�B	�B	oB	oB	�B	HB	�B	#�B	#TB	7�B	W
B	h�B	l=B	H�B	_VB	a-B	^jB	h�B	n�B	�B	�DB	��B	��B	��B	�CB	��B	��B	�TB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�GB	��B	�%B	��B	�B	�1B	�B	�iB	��B	��B	�RB	�<B	͹B	��B	̘B	�B	�	B	ʦB	�6B	�B	�0B	�^B	�XB	ʌB	��B	�<B	ΊB	�(B	�BB	�pB	͹B	�jB	�"B	��B	�VB	�B	��B	�pB	�B	��B	�B	�pB	�\B	��B	бB	�4B	��B	��B	�B	̳B	�JB	̘B	��B	�dB	�~B	̳B	�bB	уB	��B	��B	�VB	�(B	��B	�jB	�jB	̳B	�dB	�B	ˬB	�B	��B	͟B	�B	͹B	οB	ΥB	�jB	�<B	��B	�"B	ϑB	�BB	ϫB	ϑB	ϑB	�}B	��B	�B	�$B	�)B	یB	�7B	�SB	�uB	� B	��B	�B	ѝB	ҽB	�@B	�uB	�B	��B	��B	�,B	ԕB	՛B	�gB	��B	՛B	ՁB	��B	ևB	�
B	�?B	��B	רB	ؓB	��B	�B	�eB	�B	�KB	�yB	�B	�_B	�B	�KB	�1B	�B	�B	چB	�	B	ܒB	ܒB	�B	��B	�pB	��B	��B	�B	�B	ߤB	�;B	�BB	�'B	�'B	�pB	�5B	��B	ߊB	�VB	�B	�pB	��B	��B	�B	��B	��B	�|B	�NB	��B	� B	�ZB	�RB	�B	��B	�`B	��B	�fB	�RB	��B	�XB	�$B	��B	�B	�yB	��B	�B	�B	��B	��B	�KB	��B	�B	�QB	�B	��B	��B	�qB	��B	�B	��B	��B	�/B	�cB	��B	� B	�B	��B	�!B	�;B	�UB	�AB	�B	�B	��B	�|B	�B	��B	�3B	�B	�B	�B	�9B	�9B	�nB	�%B	�ZB	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	�0B	��B	��B	�B	�6B	��B	��B	�VB	��B	�B	�BB	�B	�.B	�}B
  B
 �B
 �B
 �B
B
�B
�B
�B
B
�B
�B
3B
�B
�B
�B
�B
�B
B
�B
9B
?B
�B
�B
+B
�B
B
KB
�B
	�B
	lB
	�B

XB

�B

�B
DB
�B
JB
dB
dB
dB
dB
0B
dB
0B
�B
jB
�B
�B
B
vB
bB
HB
bB
�B
�B
 B
oB
TB
:B
�B
�B
uB
,B
B
�B
�B
B
9B
�B
�B
MB
�B
�B
B
�B
�B
,B
�B
�B
MB
�B
9B
�B
$B
�B
B
EB
yB
�B
�B
KB
�B
7B
QB
�B
�B
�B
�B
QB
�B
�B
CB
B
�B
�B
�B
OB
�B
�B
�B
�B
 'B
 'B
 vB
 �B
"hB
!�B
"�B
"�B
#:B
#�B
#�B
"�B
# B
#nB
#TB
#nB
#�B
#�B
$�B
%B
%FB
%,B
%�B
%zB
&2B
'B
&�B
&�B
&�B
(sB
(�B
)�B
)DB
)�B
)�B
)�B
*KB
+�B
,�B
,�B
,�B
,�B
,B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-]B
-�B
./B
.IB
/5B
/�B
0!B
0�B
2-B
2�B
3MB
3�B
3�B
4B
4nB
4�B
5?B
5?B
5tB
5�B
5�B
5�B
6FB
6+B
6`B
6�B
7�B
7�B
8B
8�B
8�B
9$B
9>B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
9�B
9�B
:*B
:^B
:*B
9�B
9�B
9�B
:B
:DB
:DB
:�B
:�B
:�B
:�B
;0B
;B
;B
;B
;dB
;�B
;�B
<B
<B
<6B
<�B
=qB
=<B
=qB
=�B
>BB
>�B
>wB
>�B
>�B
?.B
?�B
?�B
@ B
@ B
@iB
@�B
@�B
A;B
AoB
A�B
A�B
A�B
A�B
B�B
DgB
E9B
ESB
ESB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
GEB
G_B
HB
HfB
H�B
HfB
H�B
H�B
IB
IRB
I�B
I�B
J	B
J=B
J=B
JXB
J�B
J�B
JXB
J�B
KB
K�B
K�B
K�B
KxB
K�B
LdB
L0B
L�B
MB
MjB
M�B
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
O(B
OBB
OBB
O(B
O�B
PB
P.B
PbB
P�B
Q4B
Q4B
Q�B
Q�B
R B
R�B
RTB
R:B
RTB
R�B
R�B
R�B
R�B
S[B
S[B
S�B
S�B
TFB
T�B
UMB
UgB
U2B
U2B
UB
UB
U�B
U�B
U�B
U�B
VB
U�B
VB
VSB
VmB
V�B
V�B
V�B
V�B
V�B
WYB
W?B
W$B
W?B
W?B
W�B
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
Z�B
Z�B
Z�B
Z�B
Z�B
ZkB
ZQB
Z�B
[�B
\xB
\�B
]B
]dB
]�B
]dB
\�B
\�B
\�B
\xB
\�B
\�B
\�B
\�B
\�B
]dB
]�B
^B
^B
^B
^OB
^jB
^�B
_B
_VB
_pB
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
a|B
a|B
a|B
a�B
b4B
bhB
b�B
cB
c:B
cTB
cTB
cTB
c�B
d�B
d�B
d�B
dtB
d�B
d�B
eB
e,B
ezB
f2B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
i�B
j�B
kB
jB
jB
k6B
kkB
k6B
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
mB
m]B
m�B
mCB
l�B
l�B
m)B
m�B
nIB
n}B
n�B
o B
pB
p�B
p�B
p�B
p�B
p�B
q[B
qvB
q[B
q'B
qvB
rGB
r|B
r�B
shB
s�B
tB
tB
tnB
t�B
uB
uB
t�B
tB
t�B
uZB
u?B
u%B
t�B
t�B
t�B
t�B
u�B
u�B
vB
u�B
uB
uB
uB
u�B
u�B
vB
vzB
w2B
w2B
w2B
wLB
wB
wB
wLB
w2B
w2B
w�B
w�B
x8B
x�B
y$B
y�B
y�B
y>B
yrB
yrB
y�B
y�B
y�B
y�B
zB
zxB
{0B
{B
{B
{�B
{�B
|B
|6B
|6B
|�B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}qB
}�B
}�B
~wB
~�B
B
.B
B
HB
.B
.B
HB
HB
HB
HB
cB
cB
HB
HB
}B
�B
�B
� B
�B
�iB
��B
��B
��B
�B
�;B
� B
�;B
�oB
�oB
�UB
�U11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	c�B	d&B	c�B	d&B	c�B	c�B	c�B	cnB	b�B	b4B	cB	c�B	c�B	^�B	Q�B	F%B	@B	L�B	�B	�#B	�=B	�cB	�sB	�xB	�BB	�+B	��B	�?B	�yB	��B
�B
'8B
;dB
�B
�FB
ȀB
уB
�B
�cB�B~B4�B;dBG�BR BW�BQ�BVmBg8BgBf�B`�B\�Ba�BncBv+B`BB3�B�B*�B$B;B�BxB
�qB
��B
�B
��B
��B
wB
]�B
.}B
�B
JB
�B
�B	��B
^B
 �B	��B	�ZB	��B	ňB	�eB	��B	�B	|PB	c�B	QNB	JXB	FtB	>�B	7�B	2�B	)DB	yB	�B		�B��B�UB��B�B�=B�\B�|B�B��B�tB�RB�WB�B�CB��B	^B	�B		�B	�B	�B	�B	oB	oB	�B	HB	�B	#�B	#TB	7�B	W
B	h�B	l=B	H�B	_VB	a-B	^jB	h�B	n�B	�B	�DB	��B	��B	��B	�CB	��B	��B	�TB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�GB	��B	�%B	��B	�B	�1B	�B	�iB	��B	��B	�RB	�<B	͹B	��B	̘B	�B	�	B	ʦB	�6B	�B	�0B	�^B	�XB	ʌB	��B	�<B	ΊB	�(B	�BB	�pB	͹B	�jB	�"B	��B	�VB	�B	��B	�pB	�B	��B	�B	�pB	�\B	��B	бB	�4B	��B	��B	�B	̳B	�JB	̘B	��B	�dB	�~B	̳B	�bB	уB	��B	��B	�VB	�(B	��B	�jB	�jB	̳B	�dB	�B	ˬB	�B	��B	͟B	�B	͹B	οB	ΥB	�jB	�<B	��B	�"B	ϑB	�BB	ϫB	ϑB	ϑB	�}B	��B	�B	�$B	�)B	یB	�7B	�SB	�uB	� B	��B	�B	ѝB	ҽB	�@B	�uB	�B	��B	��B	�,B	ԕB	՛B	�gB	��B	՛B	ՁB	��B	ևB	�
B	�?B	��B	רB	ؓB	��B	�B	�eB	�B	�KB	�yB	�B	�_B	�B	�KB	�1B	�B	�B	چB	�	B	ܒB	ܒB	�B	��B	�pB	��B	��B	�B	�B	ߤB	�;B	�BB	�'B	�'B	�pB	�5B	��B	ߊB	�VB	�B	�pB	��B	��B	�B	��B	��B	�|B	�NB	��B	� B	�ZB	�RB	�B	��B	�`B	��B	�fB	�RB	��B	�XB	�$B	��B	�B	�yB	��B	�B	�B	��B	��B	�KB	��B	�B	�QB	�B	��B	��B	�qB	��B	�B	��B	��B	�/B	�cB	��B	� B	�B	��B	�!B	�;B	�UB	�AB	�B	�B	��B	�|B	�B	��B	�3B	�B	�B	�B	�9B	�9B	�nB	�%B	�ZB	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	�0B	��B	��B	�B	�6B	��B	��B	�VB	��B	�B	�BB	�B	�.B	�}B
  B
 �B
 �B
 �B
B
�B
�B
�B
B
�B
�B
3B
�B
�B
�B
�B
�B
B
�B
9B
?B
�B
�B
+B
�B
B
KB
�B
	�B
	lB
	�B

XB

�B

�B
DB
�B
JB
dB
dB
dB
dB
0B
dB
0B
�B
jB
�B
�B
B
vB
bB
HB
bB
�B
�B
 B
oB
TB
:B
�B
�B
uB
,B
B
�B
�B
B
9B
�B
�B
MB
�B
�B
B
�B
�B
,B
�B
�B
MB
�B
9B
�B
$B
�B
B
EB
yB
�B
�B
KB
�B
7B
QB
�B
�B
�B
�B
QB
�B
�B
CB
B
�B
�B
�B
OB
�B
�B
�B
�B
 'B
 'B
 vB
 �B
"hB
!�B
"�B
"�B
#:B
#�B
#�B
"�B
# B
#nB
#TB
#nB
#�B
#�B
$�B
%B
%FB
%,B
%�B
%zB
&2B
'B
&�B
&�B
&�B
(sB
(�B
)�B
)DB
)�B
)�B
)�B
*KB
+�B
,�B
,�B
,�B
,�B
,B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
-]B
-�B
./B
.IB
/5B
/�B
0!B
0�B
2-B
2�B
3MB
3�B
3�B
4B
4nB
4�B
5?B
5?B
5tB
5�B
5�B
5�B
6FB
6+B
6`B
6�B
7�B
7�B
8B
8�B
8�B
9$B
9>B
9XB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
9�B
9�B
:*B
:^B
:*B
9�B
9�B
9�B
:B
:DB
:DB
:�B
:�B
:�B
:�B
;0B
;B
;B
;B
;dB
;�B
;�B
<B
<B
<6B
<�B
=qB
=<B
=qB
=�B
>BB
>�B
>wB
>�B
>�B
?.B
?�B
?�B
@ B
@ B
@iB
@�B
@�B
A;B
AoB
A�B
A�B
A�B
A�B
B�B
DgB
E9B
ESB
ESB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
GEB
G_B
HB
HfB
H�B
HfB
H�B
H�B
IB
IRB
I�B
I�B
J	B
J=B
J=B
JXB
J�B
J�B
JXB
J�B
KB
K�B
K�B
K�B
KxB
K�B
LdB
L0B
L�B
MB
MjB
M�B
M�B
M�B
M�B
N"B
NVB
N�B
N�B
N�B
O(B
OBB
OBB
O(B
O�B
PB
P.B
PbB
P�B
Q4B
Q4B
Q�B
Q�B
R B
R�B
RTB
R:B
RTB
R�B
R�B
R�B
R�B
S[B
S[B
S�B
S�B
TFB
T�B
UMB
UgB
U2B
U2B
UB
UB
U�B
U�B
U�B
U�B
VB
U�B
VB
VSB
VmB
V�B
V�B
V�B
V�B
V�B
WYB
W?B
W$B
W?B
W?B
W�B
W�B
W�B
W�B
W�B
X+B
X_B
X�B
X�B
Z�B
Z�B
Z�B
Z�B
Z�B
ZkB
ZQB
Z�B
[�B
\xB
\�B
]B
]dB
]�B
]dB
\�B
\�B
\�B
\xB
\�B
\�B
\�B
\�B
\�B
]dB
]�B
^B
^B
^B
^OB
^jB
^�B
_B
_VB
_pB
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
a|B
a|B
a|B
a�B
b4B
bhB
b�B
cB
c:B
cTB
cTB
cTB
c�B
d�B
d�B
d�B
dtB
d�B
d�B
eB
e,B
ezB
f2B
f�B
f�B
g8B
gmB
g�B
g�B
g�B
g�B
h
B
h�B
h�B
h�B
i�B
j�B
kB
jB
jB
k6B
kkB
k6B
kB
kQB
kkB
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
mB
m]B
m�B
mCB
l�B
l�B
m)B
m�B
nIB
n}B
n�B
o B
pB
p�B
p�B
p�B
p�B
p�B
q[B
qvB
q[B
q'B
qvB
rGB
r|B
r�B
shB
s�B
tB
tB
tnB
t�B
uB
uB
t�B
tB
t�B
uZB
u?B
u%B
t�B
t�B
t�B
t�B
u�B
u�B
vB
u�B
uB
uB
uB
u�B
u�B
vB
vzB
w2B
w2B
w2B
wLB
wB
wB
wLB
w2B
w2B
w�B
w�B
x8B
x�B
y$B
y�B
y�B
y>B
yrB
yrB
y�B
y�B
y�B
y�B
zB
zxB
{0B
{B
{B
{�B
{�B
|B
|6B
|6B
|�B
}"B
}"B
}VB
}�B
}�B
}�B
}�B
}qB
}�B
}�B
~wB
~�B
B
.B
B
HB
.B
.B
HB
HB
HB
HB
cB
cB
HB
HB
}B
�B
�B
� B
�B
�iB
��B
��B
��B
�B
�;B
� B
�;B
�oB
�oB
�UB
�U11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104941  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174713  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174714  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174714                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024722  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024722  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                