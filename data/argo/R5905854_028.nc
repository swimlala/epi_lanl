CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:49:28Z creation;2022-06-04T17:49:29Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174928  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��ײ��1   @��ج/5@.��j~���c9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BG��BP  BX  B`  Bj  Bo33Bx  B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C��C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&33C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @{@~{@�
>A�A!�AA�Aa�A��\A�\)A�\)A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B?�HBG�HBPG�BXG�B`G�BjG�Boz�BxG�B�#�B�#�B�#�B�#�B��=B�W
B��B�#�B�#�B�#�B�#�B��B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C EC��C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&EC(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\+�C^+�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cm�RCp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��)C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D~D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�pD�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�?
Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�EpD�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��xA���A���A���A���A��$A���A���A�'A�:A��A૟A�qA��A�IA�}A��A�*A���A��uA���A�Y�A�XyA�M6A��A�AUA�R�A�w�A�pA�U2A�q�A���A��BA��A�K)A�;dA��A��A��A��A���A�V9A�#�A�*�A�YA���A�S�A�%FA��A��1A��A�*�A��A�/A�8�A��A��A��A��A��A�y�A�AUA�U2A���A��A�ٴA���A�FA��oA���A�c A�XA�iyA��A���A��A�֡A���A�2aA�@�A�|Ay��Aut�AttTAsN<Ao��Al�Ai�rAf�UAb�A_�AZ^�AU_pAR�AP~(AN|AJ-AHr�AF��ACX�A?tTA;�A8�|A7�TA6PHA3r�A1w�A/�A-��A+XyA)��A(^�A'�+A&�A&A A%�rA%I�A$"hA#[WA#6�A#	A"�.A"��A"�A!?�A dZA E�A B[A Y�A 0UA��A�A��A�'A��AںA��A��A�:A6�A^�A��A ��A �PA �A�[A�XA��A�A%�A��A]dA;dA/�A�tAkQAffA)�A��AVmA%�A�A\�A��AHA��A@Al�AiDA&A��A�Ay>A0UA�"A �A��A�A�Ag8A
�A
��A
!-A
bA	��A	�^A	�A_A�2A�:AZ�A�}A7LA��A��AsAA�RA�zA�.AE9AݘA��A^�A=�A�A�A�AiDAFA��A�AN<A@A �nA m�A 'R@�� @��@���@��@��@���@���@���@�0�@�֡@�J@��@���@��@�ں@��1@�{J@��h@�x@���@���@�@���@�	�@@��@�@�!-@���@�<@��@���@�+@�o�@�@�]d@�ݘ@�~@��`@�g8@�(�@�n/@�B[@��@�+@�\@�u�@�1'@�X@�.I@��H@��,@��+@�j�@�U�@�ѷ@ާ@ޚ�@�C-@ݑh@܋D@܁o@��@۷@ۢ�@�S�@ڻ�@�Xy@�2�@ْ:@�C�@�A @�S&@���@��@�j@�!-@��@�%F@֭�@֜x@��m@�@�
=@���@�x@�/@Բ�@�?�@Ӟ�@�.I@��@ұ�@�(�@ќ�@�Y@�+@���@�h
@ώ"@�w2@��@Ψ�@�Q@�7@�v`@��p@�  @�j�@ʅ�@��m@�S�@Ȋr@�H@��o@��@��p@Ƽj@ƋD@�W�@��@���@�S&@���@ė�@��@Ü@�L�@��@x@�%�@¦L@��]@�z@��:@��@��4@�`�@�@�/�@��I@���@���@�+@� �@���@���@�q@���@�:�@��@�e@���@��@�X�@�.I@��@���@���@��@@�l�@�?}@�ی@��@��@��@��@�m�@�+k@�x@��:@��@��@���@��@�8�@��@�6z@��H@��c@�ߤ@���@��r@�@O@�Q�@���@��@�8@��'@��@�y>@�C�@��@��)@��@��C@��	@�2a@�ߤ@��\@�"h@�y�@�Y@���@��h@���@�$@��@�n/@�7L@���@��B@��@�Q@�)�@���@��M@�RT@�@���@���@�q@�H�@��@��'@�@��@�?@���@�j@��@��@��$@�xl@�Q�@�@���@�v`@�V@��'@��A@�Z�@�?�@��@���@�#�@��@��u@�g8@�@�@���@�Vm@�6z@��2@�w�@�{@��T@���@���@�J#@��@�c�@��@�ƨ@���@�a�@�C@�Ɇ@�xl@�V�@�H@�7@��@��)@��F@�4@��M@��p@���@���@�YK@�G@�Z�@���@��b@�_@�e@��@��$@�`B@�%F@��@��@���@�_@��@��w@�X@��5@�Ɇ@��}@�~(@�B[@� �@���@�x@�+@���@���@�oi@�$�@��@���@�l�@�C�@��@��@�bN@�0U@�1@��;@���@���@���@�W?@�Y@�҉@���@�j@�;�@���@��F@�}�@�/�@��E@�s�@�($@�b@���@��'@�S�@�.I@�;@��h@��<@�3�@��z@���@�dZ@��5@�h�@�;�@��@���@��h@�dZ@�Mj@��@���@���@���@��!@�z@�R�@��@�6@g�@�@
=@�@~�@~�R@~z@}�@}�@}^�@|�Y@{�;@{��@{�*@{�*@{\)@{�@z��@z_@y�3@yx�@x�v@xH@x�@w�@wJ#@w�@v��@vZ�@v_@u�h@u \@t�@t�@t'R@s�a@sdZ@r�@r8�@qY�@pK^@o�6@o\)@o=@n�,@n��@np;@m��@m��@m%F@ll"@l7@k�W@ky�@k(@jxl@j\�@i�@i�-@ix�@i�@h`�@g�@@gy�@f��@f;�@eVm@d�@dr�@c��@c\)@c�@b�h@b=q@a�z@a|@`�v@`��@` �@_��@_�P@_o�@_=@^�]@^E�@]�Z@]�@]?}@\��@\�@\?�@\G@[��@Z�c@Z�@ZC�@Y�@YY�@Y#�@X�@XM@W�@W�@WP�@W(@V�c@V��@V��@V��@V\�@V@�@U@U�@T�j@Tm�@T"h@S�a@S��@S��@Se�@S�@R�@R�@R��@R�6@R�b@R��@R�@Q�d@Q�C@Qhs@Q-w@PĜ@P�Y@P4n@O�@O,�@N��@N8�@N�@Mw2@M \@M@L��@L�K@Lی@Lz�@K�@K��@K/�@J��@J��@J0U@I��@I�'@IV@H�@H	�@G��@G/�@F�L@E��@E��@E�@D�v@D�z@Dm�@C�r@Ca@C�@Bl�@A��@Azx@A7L@@�@@�j@@�I@@�@@U2@@@?y�@?�@>kQ@=�o@=�7@=`B@=:�@<�@<�@<�9@<e�@;�r@;��@;v`@;W?@;�@:_�@:
�@9�^@9��@9c�@9IR@9%F@8��@8��@8U2@7خ@78@6�@6u%@6 �@5��@5Vm@4��@4]d@4�@3�K@3�	@3g�@3F�@3>�@3"�@3�@2�M@2��@2Ta@2C�@1�@1m]@1!�@0�@0g8@0�@/33@.�@.z@.6�@-��@-��@-�H@-�h@-Vm@-�@,�@,�o@,q@,`�@,H@+�r@+�}@+�@+A�@+�@*ں@*� @*^5@)�D@)�t@)zx@)X@)A @)2a@(�P@(�@(_@(Q�@(Ft@(/�@'�@'�@'>�@&�]@&��@&Q@&$�@%�^@%^�@%J�@%Dg@%�@$��@$r�@$%�@#�;@#s@#�@"��@"��@"~�@"kQ@"\�@";�@"+k@"$�@"_@!�N@!��@!zx@!-w@ �@ ��@ �@ _@ �@ݘ@�@�@@�P@J#@�M@��@�@��@@��@rG@f�@O�@B�@*0@!�@�@�@��@C-@:�@'R@b@��@�k@qv@9�@o@��@Q@�@��@0�@�|@�9@��@w�@`�@Q�@"h@�A@�@�@@v`@g�@J#@&@�@ȴ@}V@=q@�@�~@w2@[W@@	l@��@�I@r�@S�@>B@!@��@��@��@RT@�@(@�y@�m@�h@^5@E�@&�@��@��@�#@�M@c�@\�@Dg@�@�?@��@~(@6@�@G@�@��@�@@��@X�@Y@�@@�"@��@�b@��@Ta@	@��@��@�~@G�@�P@��@�$@�@m�@*�@�m@�}@��@�@l�@W?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��xA���A���A���A���A��$A���A���A�'A�:A��A૟A�qA��A�IA�}A��A�*A���A��uA���A�Y�A�XyA�M6A��A�AUA�R�A�w�A�pA�U2A�q�A���A��BA��A�K)A�;dA��A��A��A��A���A�V9A�#�A�*�A�YA���A�S�A�%FA��A��1A��A�*�A��A�/A�8�A��A��A��A��A��A�y�A�AUA�U2A���A��A�ٴA���A�FA��oA���A�c A�XA�iyA��A���A��A�֡A���A�2aA�@�A�|Ay��Aut�AttTAsN<Ao��Al�Ai�rAf�UAb�A_�AZ^�AU_pAR�AP~(AN|AJ-AHr�AF��ACX�A?tTA;�A8�|A7�TA6PHA3r�A1w�A/�A-��A+XyA)��A(^�A'�+A&�A&A A%�rA%I�A$"hA#[WA#6�A#	A"�.A"��A"�A!?�A dZA E�A B[A Y�A 0UA��A�A��A�'A��AںA��A��A�:A6�A^�A��A ��A �PA �A�[A�XA��A�A%�A��A]dA;dA/�A�tAkQAffA)�A��AVmA%�A�A\�A��AHA��A@Al�AiDA&A��A�Ay>A0UA�"A �A��A�A�Ag8A
�A
��A
!-A
bA	��A	�^A	�A_A�2A�:AZ�A�}A7LA��A��AsAA�RA�zA�.AE9AݘA��A^�A=�A�A�A�AiDAFA��A�AN<A@A �nA m�A 'R@�� @��@���@��@��@���@���@���@�0�@�֡@�J@��@���@��@�ں@��1@�{J@��h@�x@���@���@�@���@�	�@@��@�@�!-@���@�<@��@���@�+@�o�@�@�]d@�ݘ@�~@��`@�g8@�(�@�n/@�B[@��@�+@�\@�u�@�1'@�X@�.I@��H@��,@��+@�j�@�U�@�ѷ@ާ@ޚ�@�C-@ݑh@܋D@܁o@��@۷@ۢ�@�S�@ڻ�@�Xy@�2�@ْ:@�C�@�A @�S&@���@��@�j@�!-@��@�%F@֭�@֜x@��m@�@�
=@���@�x@�/@Բ�@�?�@Ӟ�@�.I@��@ұ�@�(�@ќ�@�Y@�+@���@�h
@ώ"@�w2@��@Ψ�@�Q@�7@�v`@��p@�  @�j�@ʅ�@��m@�S�@Ȋr@�H@��o@��@��p@Ƽj@ƋD@�W�@��@���@�S&@���@ė�@��@Ü@�L�@��@x@�%�@¦L@��]@�z@��:@��@��4@�`�@�@�/�@��I@���@���@�+@� �@���@���@�q@���@�:�@��@�e@���@��@�X�@�.I@��@���@���@��@@�l�@�?}@�ی@��@��@��@��@�m�@�+k@�x@��:@��@��@���@��@�8�@��@�6z@��H@��c@�ߤ@���@��r@�@O@�Q�@���@��@�8@��'@��@�y>@�C�@��@��)@��@��C@��	@�2a@�ߤ@��\@�"h@�y�@�Y@���@��h@���@�$@��@�n/@�7L@���@��B@��@�Q@�)�@���@��M@�RT@�@���@���@�q@�H�@��@��'@�@��@�?@���@�j@��@��@��$@�xl@�Q�@�@���@�v`@�V@��'@��A@�Z�@�?�@��@���@�#�@��@��u@�g8@�@�@���@�Vm@�6z@��2@�w�@�{@��T@���@���@�J#@��@�c�@��@�ƨ@���@�a�@�C@�Ɇ@�xl@�V�@�H@�7@��@��)@��F@�4@��M@��p@���@���@�YK@�G@�Z�@���@��b@�_@�e@��@��$@�`B@�%F@��@��@���@�_@��@��w@�X@��5@�Ɇ@��}@�~(@�B[@� �@���@�x@�+@���@���@�oi@�$�@��@���@�l�@�C�@��@��@�bN@�0U@�1@��;@���@���@���@�W?@�Y@�҉@���@�j@�;�@���@��F@�}�@�/�@��E@�s�@�($@�b@���@��'@�S�@�.I@�;@��h@��<@�3�@��z@���@�dZ@��5@�h�@�;�@��@���@��h@�dZ@�Mj@��@���@���@���@��!@�z@�R�@��@�6@g�@�@
=@�@~�@~�R@~z@}�@}�@}^�@|�Y@{�;@{��@{�*@{�*@{\)@{�@z��@z_@y�3@yx�@x�v@xH@x�@w�@wJ#@w�@v��@vZ�@v_@u�h@u \@t�@t�@t'R@s�a@sdZ@r�@r8�@qY�@pK^@o�6@o\)@o=@n�,@n��@np;@m��@m��@m%F@ll"@l7@k�W@ky�@k(@jxl@j\�@i�@i�-@ix�@i�@h`�@g�@@gy�@f��@f;�@eVm@d�@dr�@c��@c\)@c�@b�h@b=q@a�z@a|@`�v@`��@` �@_��@_�P@_o�@_=@^�]@^E�@]�Z@]�@]?}@\��@\�@\?�@\G@[��@Z�c@Z�@ZC�@Y�@YY�@Y#�@X�@XM@W�@W�@WP�@W(@V�c@V��@V��@V��@V\�@V@�@U@U�@T�j@Tm�@T"h@S�a@S��@S��@Se�@S�@R�@R�@R��@R�6@R�b@R��@R�@Q�d@Q�C@Qhs@Q-w@PĜ@P�Y@P4n@O�@O,�@N��@N8�@N�@Mw2@M \@M@L��@L�K@Lی@Lz�@K�@K��@K/�@J��@J��@J0U@I��@I�'@IV@H�@H	�@G��@G/�@F�L@E��@E��@E�@D�v@D�z@Dm�@C�r@Ca@C�@Bl�@A��@Azx@A7L@@�@@�j@@�I@@�@@U2@@@?y�@?�@>kQ@=�o@=�7@=`B@=:�@<�@<�@<�9@<e�@;�r@;��@;v`@;W?@;�@:_�@:
�@9�^@9��@9c�@9IR@9%F@8��@8��@8U2@7خ@78@6�@6u%@6 �@5��@5Vm@4��@4]d@4�@3�K@3�	@3g�@3F�@3>�@3"�@3�@2�M@2��@2Ta@2C�@1�@1m]@1!�@0�@0g8@0�@/33@.�@.z@.6�@-��@-��@-�H@-�h@-Vm@-�@,�@,�o@,q@,`�@,H@+�r@+�}@+�@+A�@+�@*ں@*� @*^5@)�D@)�t@)zx@)X@)A @)2a@(�P@(�@(_@(Q�@(Ft@(/�@'�@'�@'>�@&�]@&��@&Q@&$�@%�^@%^�@%J�@%Dg@%�@$��@$r�@$%�@#�;@#s@#�@"��@"��@"~�@"kQ@"\�@";�@"+k@"$�@"_@!�N@!��@!zx@!-w@ �@ ��@ �@ _@ �@ݘ@�@�@@�P@J#@�M@��@�@��@@��@rG@f�@O�@B�@*0@!�@�@�@��@C-@:�@'R@b@��@�k@qv@9�@o@��@Q@�@��@0�@�|@�9@��@w�@`�@Q�@"h@�A@�@�@@v`@g�@J#@&@�@ȴ@}V@=q@�@�~@w2@[W@@	l@��@�I@r�@S�@>B@!@��@��@��@RT@�@(@�y@�m@�h@^5@E�@&�@��@��@�#@�M@c�@\�@Dg@�@�?@��@~(@6@�@G@�@��@�@@��@X�@Y@�@@�"@��@�b@��@Ta@	@��@��@�~@G�@�P@��@�$@�@m�@*�@�m@�}@��@�@l�@W?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BIBH�BI7BH�BH�BIBIBIBIBIBIBIRBIRBI�BJ�BK�BKDBM�BP�BQNBQBf�B�B	�B	�bB	��B	�)B	�B	�B	ȀB	��B	�&B	��B
B
9�B
Y�B
r�B
�B
�`B
�-B
ɠB
�TB
��BFB_�BjeB�FB��B�}B�B��B��B}�BZ�BCaB33B-)B�B�B0B
��B
�tB
�KB
��B
��B
�B
��B
�B
��B
�$B
�B
�qB
�B
�B
z�B
y>B
[WB
CaB
"�B	�wB	��B	�B	��B	��B	�B	uB	jeB	e�B	mB	a-B	I�B	,"B	�B	B�DB�"B�MB̈́B�KB�cB��B�oB�B�*B��B��B��B��B��B��B�]B�B��B�oB��BȀB҉B�B�B�%B�8B�JB	%B	�B	�B	)B	!B	,�B	3�B	8�B	>B	C�B	K�B	X�B	g�B	v�B	yrB	yrB	yXB	zxB	��B	��B	��B	��B	�9B	�tB	�aB	��B	��B	��B	�2B	��B	��B	�B	��B	��B	�B	�XB	�eB	��B	�sB	�eB	��B	�;B	��B	�oB	��B	�vB	��B	�B	�B	��B	�|B	��B	�B	��B	�qB	��B	��B	�4B	��B	�aB	ŢB	ŢB	�B	�%B	�+B	ƨB	�%B	ƨB	ǔB	��B	�RB	�RB	ɺB	ɺB	�=B	�XB	ʦB	��B	�)B	��B	�6B	̳B	�PB	̈́B	��B	οB	�BB	�(B	ϑB	� B	ЗB	�4B	҉B	�TB	�FB	�B	�MB	��B	ԕB	��B	ԕB	�mB	�+B	ٚB	��B	��B	��B	�5B	ߤB	�HB	�8B	�zB	�B	�6B	��B	�mB	�@B	� B	�LB	�>B	�B	�B	�zB	�fB	�LB	�B	��B	��B	��B	��B	��B	� B	�FB	�B	��B	�B	�mB	�B	�B	�B	��B	��B	�B	�HB	�\B	�vB	��B	��B	ߤB	�B	��B	�\B	�B	�vB	�B	��B	�:B	�&B	�B	�2B	�B	�`B	��B	��B	��B	�zB	��B	�fB	��B	�zB	�B	�B	�B	�B	��B	�KB	�=B	�5B	�B	�B	�B	�B	��B	��B	�B	�B	�'B	��B	�B	�B	�nB	��B	��B	��B	�fB	�RB	��B	�>B	��B	�^B	�*B	�0B	�B	��B	��B	�$B	�rB	��B	��B	��B	�$B	�lB	�$B	��B	�"B	�<B	��B	��B	�XB	��B	��B	��B	�RB	��B	��B	�	B	��B	��B	�}B	��B	�.B	��B	�.B	��B	��B
 �B	�cB	�(B	��B	�B	��B	��B	�*B	�xB	��B	��B	��B	�BB	��B	��B	��B	�VB	��B	��B	��B	�qB	�VB	�<B	�<B	�VB	��B	�(B	��B	�.B	�B	��B	�HB
 iB
oB
3B
�B
�B
oB
�B
�B
9B
B
�B
�B
uB
'B
�B
�B
B
 �B
 �B
'B
AB
'B
B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
AB
-B
�B
GB
�B
�B
�B
3B
B
�B
�B
�B
MB
MB
�B
�B
�B
B
B
B
�B
�B
�B
B
?B
?B
�B
�B
�B
1B
fB
�B
�B
�B
	�B
	�B

rB

XB
)B
B
DB
0B
dB
~B
�B
VB
�B
�B
�B
�B
�B
�B
�B
}B
}B
}B
 B
NB
�B
�B
�B
�B
�B
&B
�B
FB
aB
aB
{B
�B
2B
SB
�B
mB
mB
�B
�B
�B
�B
�B
sB
�B
�B
?B
$B
YB
�B
EB
�B
B
B
1B
B
B
�B
	B
�B
	B
�B
qB
�B
CB
]B
]B
B
�B
B
;B
�B
�B
�B
 'B
 BB
 BB
 vB
 �B
!HB
!�B
"B
"hB
"�B
#�B
$B
#�B
$ZB
$tB
%FB
%zB
&LB
(XB
($B
'�B
'RB
(>B
)yB
*B
*�B
*0B
*KB
*�B
+B
+�B
+�B
+�B
,WB
,�B
,�B
,�B
-CB
-CB
-]B
-�B
-�B
.B
.cB
.�B
/B
/�B
/�B
0!B
1B
1vB
1�B
2B
2GB
2B
1�B
2GB
2�B
33B
3�B
49B
4TB
4B
49B
4�B
4�B
4�B
5?B
5?B
5B
5ZB
5tB
5�B
5�B
5�B
5�B
6+B
6zB
6�B
6�B
7LB
7�B
8B
8�B
9>B
9�B
9�B
9�B
9rB
9�B
9�B
9�B
:�B
:�B
:^B
:�B
;�B
<B
<jB
=VB
=�B
=�B
>(B
=�B
=�B
?HB
?}B
@ B
@B
@B
@�B
A;B
BB
B'B
BuB
B�B
B�B
B�B
CB
CGB
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
EB
EmB
E�B
E�B
F%B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
I7B
I�B
I�B
I�B
I�B
J	B
J#B
JXB
JXB
J=B
J=B
J=B
J�B
KB
K)B
KxB
K�B
K�B
K�B
L0B
L~B
L�B
MB
MPB
M6B
M�B
N"B
N"B
N"B
NB
NB
N<B
NVB
N�B
N�B
N�B
OBB
OvB
O�B
O�B
P.B
P}B
P�B
P�B
QB
Q�B
RB
R:B
R�B
R�B
S&B
S[B
S�B
T,B
S�B
T�B
T�B
UB
UB
UgB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
W
B
WYB
W�B
W�B
W�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[qB
[qB
[�B
\]B
\xB
]B
]dB
]�B
]�B
^B
^B
^5B
^jB
^OB
^jB
^�B
^�B
_B
_VB
_;B
_�B
`vB
`�B
aHB
aHB
a|B
bNB
b�B
b�B
cB
c B
c�B
c�B
c�B
c�B
c�B
dB
dtB
dtB
d�B
d�B
eB
eFB
e�B
e�B
f2B
ffB
f�B
f�B
g8B
g8B
gmB
gmB
g�B
gmB
g�B
h$B
hsB
hsB
hsB
hsB
h�B
h�B
h�B
iDB
iDB
h�B
iB
i�B
i�B
jB
jB
j�B
kB
k6B
k�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m]B
mwB
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
o B
o B
oB
oB
oOB
o B
o B
oB
o5B
oOB
oiB
oiB
oiB
oiB
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
qAB
q[B
qvB
qvB
q�B
q�B
q�B
rB
rGB
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
shB
sMB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
u%B
uZB
utB
utB
u�B
u�B
u�B
v+B
v`B
v`B
v`B
v�B
v�B
v�B
wB
w�B
w�B
w�B
xB
x8B
xB
xRB
x8B
xRB
x�B
x�B
xlB
y	B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
z*B
zDB
zDB
zxB
z�B
z�B
z�B
z�B
{0B
{0B
{0B
{0B
{�B
{�B
{B
|B
|6B
|PB
|jB
|�B
|�B
}<B
}VB
}VB
}�B
}�B
~B
~(B
~BB
~�B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BIBH�BI7BH�BH�BIBIBIBIBIBIBIRBIRBI�BJ�BK�BKDBM�BP�BQNBQBf�B�B	�B	�bB	��B	�)B	�B	�B	ȀB	��B	�&B	��B
B
9�B
Y�B
r�B
�B
�`B
�-B
ɠB
�TB
��BFB_�BjeB�FB��B�}B�B��B��B}�BZ�BCaB33B-)B�B�B0B
��B
�tB
�KB
��B
��B
�B
��B
�B
��B
�$B
�B
�qB
�B
�B
z�B
y>B
[WB
CaB
"�B	�wB	��B	�B	��B	��B	�B	uB	jeB	e�B	mB	a-B	I�B	,"B	�B	B�DB�"B�MB̈́B�KB�cB��B�oB�B�*B��B��B��B��B��B��B�]B�B��B�oB��BȀB҉B�B�B�%B�8B�JB	%B	�B	�B	)B	!B	,�B	3�B	8�B	>B	C�B	K�B	X�B	g�B	v�B	yrB	yrB	yXB	zxB	��B	��B	��B	��B	�9B	�tB	�aB	��B	��B	��B	�2B	��B	��B	�B	��B	��B	�B	�XB	�eB	��B	�sB	�eB	��B	�;B	��B	�oB	��B	�vB	��B	�B	�B	��B	�|B	��B	�B	��B	�qB	��B	��B	�4B	��B	�aB	ŢB	ŢB	�B	�%B	�+B	ƨB	�%B	ƨB	ǔB	��B	�RB	�RB	ɺB	ɺB	�=B	�XB	ʦB	��B	�)B	��B	�6B	̳B	�PB	̈́B	��B	οB	�BB	�(B	ϑB	� B	ЗB	�4B	҉B	�TB	�FB	�B	�MB	��B	ԕB	��B	ԕB	�mB	�+B	ٚB	��B	��B	��B	�5B	ߤB	�HB	�8B	�zB	�B	�6B	��B	�mB	�@B	� B	�LB	�>B	�B	�B	�zB	�fB	�LB	�B	��B	��B	��B	��B	��B	� B	�FB	�B	��B	�B	�mB	�B	�B	�B	��B	��B	�B	�HB	�\B	�vB	��B	��B	ߤB	�B	��B	�\B	�B	�vB	�B	��B	�:B	�&B	�B	�2B	�B	�`B	��B	��B	��B	�zB	��B	�fB	��B	�zB	�B	�B	�B	�B	��B	�KB	�=B	�5B	�B	�B	�B	�B	��B	��B	�B	�B	�'B	��B	�B	�B	�nB	��B	��B	��B	�fB	�RB	��B	�>B	��B	�^B	�*B	�0B	�B	��B	��B	�$B	�rB	��B	��B	��B	�$B	�lB	�$B	��B	�"B	�<B	��B	��B	�XB	��B	��B	��B	�RB	��B	��B	�	B	��B	��B	�}B	��B	�.B	��B	�.B	��B	��B
 �B	�cB	�(B	��B	�B	��B	��B	�*B	�xB	��B	��B	��B	�BB	��B	��B	��B	�VB	��B	��B	��B	�qB	�VB	�<B	�<B	�VB	��B	�(B	��B	�.B	�B	��B	�HB
 iB
oB
3B
�B
�B
oB
�B
�B
9B
B
�B
�B
uB
'B
�B
�B
B
 �B
 �B
'B
AB
'B
B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
AB
-B
�B
GB
�B
�B
�B
3B
B
�B
�B
�B
MB
MB
�B
�B
�B
B
B
B
�B
�B
�B
B
?B
?B
�B
�B
�B
1B
fB
�B
�B
�B
	�B
	�B

rB

XB
)B
B
DB
0B
dB
~B
�B
VB
�B
�B
�B
�B
�B
�B
�B
}B
}B
}B
 B
NB
�B
�B
�B
�B
�B
&B
�B
FB
aB
aB
{B
�B
2B
SB
�B
mB
mB
�B
�B
�B
�B
�B
sB
�B
�B
?B
$B
YB
�B
EB
�B
B
B
1B
B
B
�B
	B
�B
	B
�B
qB
�B
CB
]B
]B
B
�B
B
;B
�B
�B
�B
 'B
 BB
 BB
 vB
 �B
!HB
!�B
"B
"hB
"�B
#�B
$B
#�B
$ZB
$tB
%FB
%zB
&LB
(XB
($B
'�B
'RB
(>B
)yB
*B
*�B
*0B
*KB
*�B
+B
+�B
+�B
+�B
,WB
,�B
,�B
,�B
-CB
-CB
-]B
-�B
-�B
.B
.cB
.�B
/B
/�B
/�B
0!B
1B
1vB
1�B
2B
2GB
2B
1�B
2GB
2�B
33B
3�B
49B
4TB
4B
49B
4�B
4�B
4�B
5?B
5?B
5B
5ZB
5tB
5�B
5�B
5�B
5�B
6+B
6zB
6�B
6�B
7LB
7�B
8B
8�B
9>B
9�B
9�B
9�B
9rB
9�B
9�B
9�B
:�B
:�B
:^B
:�B
;�B
<B
<jB
=VB
=�B
=�B
>(B
=�B
=�B
?HB
?}B
@ B
@B
@B
@�B
A;B
BB
B'B
BuB
B�B
B�B
B�B
CB
CGB
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
EB
EmB
E�B
E�B
F%B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
GB
G_B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
HB
H�B
H�B
H�B
I7B
I�B
I�B
I�B
I�B
J	B
J#B
JXB
JXB
J=B
J=B
J=B
J�B
KB
K)B
KxB
K�B
K�B
K�B
L0B
L~B
L�B
MB
MPB
M6B
M�B
N"B
N"B
N"B
NB
NB
N<B
NVB
N�B
N�B
N�B
OBB
OvB
O�B
O�B
P.B
P}B
P�B
P�B
QB
Q�B
RB
R:B
R�B
R�B
S&B
S[B
S�B
T,B
S�B
T�B
T�B
UB
UB
UgB
U�B
U�B
U�B
U�B
U�B
V9B
V�B
W
B
WYB
W�B
W�B
W�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
X�B
Y�B
Y�B
ZB
Z7B
Z7B
Z7B
ZQB
ZkB
Z�B
Z�B
Z�B
[qB
[qB
[�B
\]B
\xB
]B
]dB
]�B
]�B
^B
^B
^5B
^jB
^OB
^jB
^�B
^�B
_B
_VB
_;B
_�B
`vB
`�B
aHB
aHB
a|B
bNB
b�B
b�B
cB
c B
c�B
c�B
c�B
c�B
c�B
dB
dtB
dtB
d�B
d�B
eB
eFB
e�B
e�B
f2B
ffB
f�B
f�B
g8B
g8B
gmB
gmB
g�B
gmB
g�B
h$B
hsB
hsB
hsB
hsB
h�B
h�B
h�B
iDB
iDB
h�B
iB
i�B
i�B
jB
jB
j�B
kB
k6B
k�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
mCB
m]B
mwB
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
o B
o B
oB
oB
oOB
o B
o B
oB
o5B
oOB
oiB
oiB
oiB
oiB
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
qAB
q[B
qvB
qvB
q�B
q�B
q�B
rB
rGB
r�B
r�B
r�B
r�B
r�B
sB
s3B
sMB
shB
sMB
s�B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
u%B
uZB
utB
utB
u�B
u�B
u�B
v+B
v`B
v`B
v`B
v�B
v�B
v�B
wB
w�B
w�B
w�B
xB
x8B
xB
xRB
x8B
xRB
x�B
x�B
xlB
y	B
x�B
x�B
y$B
yXB
y�B
y�B
y�B
z*B
zDB
zDB
zxB
z�B
z�B
z�B
z�B
{0B
{0B
{0B
{0B
{�B
{�B
{B
|B
|6B
|PB
|jB
|�B
|�B
}<B
}VB
}VB
}�B
}�B
~B
~(B
~BB
~�B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104946  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174928  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174928  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174929                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024936  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024936  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                