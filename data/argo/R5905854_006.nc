CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:25Z creation;2022-06-04T17:45:25Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174525  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @س�X�%�1   @س����@-��n���dD�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C33C  C  C  C�fC�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A�\)B G�BG�BG�B�HB G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B��B�#�B��B��B��B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�#�B�#�B�#�B�#�B�#�B�=B�W
B��B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�CEC�C�C�C�RC�RC�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6+�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd+�Cf�Ch�Cj�Cl�Cn�Cp�Cr+�Ct+�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D��
D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�EpD��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�pD�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�B�A�A�A�-wA�.}A�/OA��A��A��cA���A���AϭAϕ�Aτ�A�{JA�j�A�ZA�>�A���AΝIA�ٴA͉A�Q�A�6�A��A���A�רA̍�A�49A��)A˩�A�e�A�_A��DA��QAʩ_A�gA�/�A�{�A�6zA�6zA�xA�یA���A��A�8�A�)�A�M�A�4Aȩ�A� �Aƕ�A�ŢA��ASA���A�R�A���A���A�o�A��A�OA���A�
�A��7A�^�A��JA��RA�o�A��A��NA���A�_A���A�j�A�EA���A�=A��A��A�_A�[�A�خA���A��A�.�A�-�A�W?A��A��fAy�rAp��AnOAk%�Ah-wAe��AeqvAeI�Ac�IAb�Aa��A_�1A]2aA[}�AYkQAU*0AQFAN$tAL+kAK�=AJA AF�DAE�AC��AC)_AB�!AA+kA=<6A;+A8�hA6P�A6,=A4�/A3�A1� A1$�A0��A/�4A*�~A)�A(��A'�vA&�vA&\�A&!A%�9A$	A#?�A#�A"�@A!��A!�AA!9XA ��A ,�AE�A�$A�hAL�A��A�'A��A��A�A��A0�A��A��A6A{�A��A�AخA�VAcAe�A�[Aa�A��A�XA�A��A�Au�Ar�Aj�A��Aw�A)�A�DADgAA��Al"A(�AqvAbAu%A��A�nA+kAt�A�A*�Az�AxA
��AoiA��AA�&A�jAo�APHA�A
�-A
m�A
�A	�[A	W�A	�A��A4A��AA�A֡A��AtTA=AU2A��AC�A�TAU�AC�A)�A�;A�ADgAA �A qvA �@���@��x@��@�\�@���@�33@�k�@�{@�+k@��@��@�7L@�Q�@��T@�+�@��s@�h�@�.I@���@�c�@��@�p;@�4�@��@�l"@�@�@O@�~�@�c�@�i�@�@��c@�/�@��@�q@�4@⭬@ᧇ@��@��@ޝI@�($@ݪ�@��@�,=@���@�˒@ہ�@���@�/�@�u�@��@�҉@ײ-@��@�hs@Ԓ�@��N@ӆ�@ҽ<@�K^@ї�@��@�|�@π4@ͬq@�W?@̕�@�ԕ@˅@�v`@�@O@��@��K@ȋD@��@ǣn@�zx@�\�@�[W@�O�@���@�i�@��@�b�@ċD@�C-@�,=@ãn@��,@´9@�z@�&�@��@�ȴ@�!@�j�@�ߤ@��L@� �@���@�e�@�@��m@���@��6@�E9@�.I@��@�� @�Ta@��@�� @�~�@�#�@�g8@��+@�IR@�(@��`@��j@��I@�}V@�a|@�8�@��@���@�}�@��8@��@�M@�4n@�"h@�M@��@���@� \@���@��T@�=�@�}V@��@��>@�n/@�%F@��@��@��m@��$@�>�@���@�~(@��@�q@�ݘ@�,�@���@�8�@��@��h@��4@�g�@�4@� i@��@�Ov@���@��@���@�Vm@�ѷ@�*�@��)@�k�@�!�@���@��)@�ff@�1@���@�A @��[@��+@��@��6@�;d@��@���@��K@���@���@�5?@�b@��T@��C@���@�+@���@�_@�@��t@�u�@�a�@�IR@�5�@��@���@��@���@�'R@�o @�9�@��@���@�*�@��@�
�@���@�\�@�T�@���@�kQ@�	@���@��@�3�@��
@��4@�Z�@�<6@�"�@��@�h
@�R�@�
�@��V@�'�@���@�z@�H�@���@��@��'@���@�Vm@���@��O@���@�0U@�@���@�qv@�=@�S@�ں@�i�@��@���@��H@��7@�A @��@�ȴ@�q�@�Q�@�:*@�O@�@��@��]@���@�`B@�=�@�@���@�p;@�Ft@�%�@�b@��j@�j@�
=@��'@�~�@�H@�-@��A@���@�`B@�Y�@�T�@�=@�-w@��@��@�Ĝ@�d�@�@�@�($@��@��m@��z@��@�a�@�%@��'@��1@�$�@��t@�S&@�C@�ߤ@���@�y>@��@���@���@�qv@�G�@��,@�n�@�5?@��@�g@t�@�@~	@}<6@|��@|�[@|��@|C-@{��@{�@{n/@{�@z�b@zOv@z{@y�o@y��@y�^@y��@y`B@x��@xK^@w�@w�	@wY@v��@v�!@v��@v1�@u�@u��@u��@u�~@t�`@t�o@t:�@s�+@s.I@s�@r�y@rL0@r4@ru@qVm@p�	@pĜ@p�D@ptT@pU2@p�@o�}@n��@n:*@n�@m�@mu�@m<6@m%@l�I@ltT@l�@k��@kiD@j��@j��@jL0@i��@ip�@i(�@h��@hu�@hQ�@g��@g8@g@f�}@fp;@f\�@f�@e��@eL�@d�	@d�?@d�@d`�@d2�@c�6@c�@@ct�@bȴ@bOv@b3�@b#:@a�j@af�@aF@a4@a�@`ی@`�e@`j@_��@_;d@^�@^��@^��@^��@^L0@]�@]��@]�@]�@]��@\��@\��@\y>@\A�@[�@[� @[�0@[�@[O@Z�H@Z�b@Z �@Y��@Y�"@Y�@X��@XXy@X�@W��@WdZ@WY@V��@V_�@V@U��@U�@UG�@T�D@S��@S i@R�1@Rv�@Rs�@Rl�@Q�@Q�@P��@Pl"@P7@O�$@O;d@N�B@N\�@M�z@M[W@L�v@L�u@L|�@Lz�@Lh�@L<�@L�@K�*@K�@J��@J8�@I�@I��@IIR@H�I@HV�@G�a@F�!@FZ�@F1�@E��@E�'@D�|@Doi@D�@Cy�@CA�@C
=@B{�@A�@Ap�@@�@@�@@<�@?�&@?�f@>�X@>c @>?@>8�@>$�@=�C@=A @=@<��@<�z@<r�@<?�@<	�@;��@;��@;�f@;o�@;;d@:͟@:�+@:O@9�@9O�@8��@9;@8�K@8��@8��@8�4@8�@8c�@8[�@8$@7�g@7��@7j�@7.I@6ں@6��@6u%@6d�@6M�@5�#@5�"@5m]@5+�@4ی@4~(@4'R@3�@3��@3��@3�w@3�q@3�$@3~�@3]�@3E9@3+@3@2��@2R�@1ϫ@1��@1o @1Y�@1(�@0��@0֡@0��@0�9@0j@0�@/��@/v`@/.I@/'�@/&@/�@/�@.��@.�+@.u@-�d@-�t@-u�@--w@,�|@,ی@,��@,��@,,=@+��@+A�@+�@*��@*�@*Q@)c@)0�@)�@(��@(Ĝ@(��@(oi@(-�@(�@'��@'��@'�@'�V@'J#@'o@&��@&��@&�@&h
@%�@%@%�@$��@$��@$�I@$g8@$�@#��@#s@#X�@#6z@#&@#o@"�B@"h
@":*@".�@!�T@!�@!!�@!�@ �[@ ��@ N�@ -�@ $@ �@�@�@S�@�@�1@J�@�@��@o @IR@�@ѷ@�@q@A�@!@�@�$@J#@�@�R@�@�\@C�@�@@�@c�@��@�/@��@�O@z�@[�@D�@��@�P@j�@(@�y@�]@��@��@n�@H�@4@�@�-@u�@X@��@��@q@b@��@;d@@�'@�\@�@��@�@��@�@��@�'@��@�S@��@a�@-w@;@��@ѷ@�@q@]d@K^@!@ݘ@��@�V@��@dZ@a@Y@��@͟@�r@L0@:*@;�@�@��@|@`B@=�@2a@�@��@ѷ@�Y@S�@�]@�$@O@9�@'�@(@
��@
��@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�B�A�A�A�-wA�.}A�/OA��A��A��cA���A���AϭAϕ�Aτ�A�{JA�j�A�ZA�>�A���AΝIA�ٴA͉A�Q�A�6�A��A���A�רA̍�A�49A��)A˩�A�e�A�_A��DA��QAʩ_A�gA�/�A�{�A�6zA�6zA�xA�یA���A��A�8�A�)�A�M�A�4Aȩ�A� �Aƕ�A�ŢA��ASA���A�R�A���A���A�o�A��A�OA���A�
�A��7A�^�A��JA��RA�o�A��A��NA���A�_A���A�j�A�EA���A�=A��A��A�_A�[�A�خA���A��A�.�A�-�A�W?A��A��fAy�rAp��AnOAk%�Ah-wAe��AeqvAeI�Ac�IAb�Aa��A_�1A]2aA[}�AYkQAU*0AQFAN$tAL+kAK�=AJA AF�DAE�AC��AC)_AB�!AA+kA=<6A;+A8�hA6P�A6,=A4�/A3�A1� A1$�A0��A/�4A*�~A)�A(��A'�vA&�vA&\�A&!A%�9A$	A#?�A#�A"�@A!��A!�AA!9XA ��A ,�AE�A�$A�hAL�A��A�'A��A��A�A��A0�A��A��A6A{�A��A�AخA�VAcAe�A�[Aa�A��A�XA�A��A�Au�Ar�Aj�A��Aw�A)�A�DADgAA��Al"A(�AqvAbAu%A��A�nA+kAt�A�A*�Az�AxA
��AoiA��AA�&A�jAo�APHA�A
�-A
m�A
�A	�[A	W�A	�A��A4A��AA�A֡A��AtTA=AU2A��AC�A�TAU�AC�A)�A�;A�ADgAA �A qvA �@���@��x@��@�\�@���@�33@�k�@�{@�+k@��@��@�7L@�Q�@��T@�+�@��s@�h�@�.I@���@�c�@��@�p;@�4�@��@�l"@�@�@O@�~�@�c�@�i�@�@��c@�/�@��@�q@�4@⭬@ᧇ@��@��@ޝI@�($@ݪ�@��@�,=@���@�˒@ہ�@���@�/�@�u�@��@�҉@ײ-@��@�hs@Ԓ�@��N@ӆ�@ҽ<@�K^@ї�@��@�|�@π4@ͬq@�W?@̕�@�ԕ@˅@�v`@�@O@��@��K@ȋD@��@ǣn@�zx@�\�@�[W@�O�@���@�i�@��@�b�@ċD@�C-@�,=@ãn@��,@´9@�z@�&�@��@�ȴ@�!@�j�@�ߤ@��L@� �@���@�e�@�@��m@���@��6@�E9@�.I@��@�� @�Ta@��@�� @�~�@�#�@�g8@��+@�IR@�(@��`@��j@��I@�}V@�a|@�8�@��@���@�}�@��8@��@�M@�4n@�"h@�M@��@���@� \@���@��T@�=�@�}V@��@��>@�n/@�%F@��@��@��m@��$@�>�@���@�~(@��@�q@�ݘ@�,�@���@�8�@��@��h@��4@�g�@�4@� i@��@�Ov@���@��@���@�Vm@�ѷ@�*�@��)@�k�@�!�@���@��)@�ff@�1@���@�A @��[@��+@��@��6@�;d@��@���@��K@���@���@�5?@�b@��T@��C@���@�+@���@�_@�@��t@�u�@�a�@�IR@�5�@��@���@��@���@�'R@�o @�9�@��@���@�*�@��@�
�@���@�\�@�T�@���@�kQ@�	@���@��@�3�@��
@��4@�Z�@�<6@�"�@��@�h
@�R�@�
�@��V@�'�@���@�z@�H�@���@��@��'@���@�Vm@���@��O@���@�0U@�@���@�qv@�=@�S@�ں@�i�@��@���@��H@��7@�A @��@�ȴ@�q�@�Q�@�:*@�O@�@��@��]@���@�`B@�=�@�@���@�p;@�Ft@�%�@�b@��j@�j@�
=@��'@�~�@�H@�-@��A@���@�`B@�Y�@�T�@�=@�-w@��@��@�Ĝ@�d�@�@�@�($@��@��m@��z@��@�a�@�%@��'@��1@�$�@��t@�S&@�C@�ߤ@���@�y>@��@���@���@�qv@�G�@��,@�n�@�5?@��@�g@t�@�@~	@}<6@|��@|�[@|��@|C-@{��@{�@{n/@{�@z�b@zOv@z{@y�o@y��@y�^@y��@y`B@x��@xK^@w�@w�	@wY@v��@v�!@v��@v1�@u�@u��@u��@u�~@t�`@t�o@t:�@s�+@s.I@s�@r�y@rL0@r4@ru@qVm@p�	@pĜ@p�D@ptT@pU2@p�@o�}@n��@n:*@n�@m�@mu�@m<6@m%@l�I@ltT@l�@k��@kiD@j��@j��@jL0@i��@ip�@i(�@h��@hu�@hQ�@g��@g8@g@f�}@fp;@f\�@f�@e��@eL�@d�	@d�?@d�@d`�@d2�@c�6@c�@@ct�@bȴ@bOv@b3�@b#:@a�j@af�@aF@a4@a�@`ی@`�e@`j@_��@_;d@^�@^��@^��@^��@^L0@]�@]��@]�@]�@]��@\��@\��@\y>@\A�@[�@[� @[�0@[�@[O@Z�H@Z�b@Z �@Y��@Y�"@Y�@X��@XXy@X�@W��@WdZ@WY@V��@V_�@V@U��@U�@UG�@T�D@S��@S i@R�1@Rv�@Rs�@Rl�@Q�@Q�@P��@Pl"@P7@O�$@O;d@N�B@N\�@M�z@M[W@L�v@L�u@L|�@Lz�@Lh�@L<�@L�@K�*@K�@J��@J8�@I�@I��@IIR@H�I@HV�@G�a@F�!@FZ�@F1�@E��@E�'@D�|@Doi@D�@Cy�@CA�@C
=@B{�@A�@Ap�@@�@@�@@<�@?�&@?�f@>�X@>c @>?@>8�@>$�@=�C@=A @=@<��@<�z@<r�@<?�@<	�@;��@;��@;�f@;o�@;;d@:͟@:�+@:O@9�@9O�@8��@9;@8�K@8��@8��@8�4@8�@8c�@8[�@8$@7�g@7��@7j�@7.I@6ں@6��@6u%@6d�@6M�@5�#@5�"@5m]@5+�@4ی@4~(@4'R@3�@3��@3��@3�w@3�q@3�$@3~�@3]�@3E9@3+@3@2��@2R�@1ϫ@1��@1o @1Y�@1(�@0��@0֡@0��@0�9@0j@0�@/��@/v`@/.I@/'�@/&@/�@/�@.��@.�+@.u@-�d@-�t@-u�@--w@,�|@,ی@,��@,��@,,=@+��@+A�@+�@*��@*�@*Q@)c@)0�@)�@(��@(Ĝ@(��@(oi@(-�@(�@'��@'��@'�@'�V@'J#@'o@&��@&��@&�@&h
@%�@%@%�@$��@$��@$�I@$g8@$�@#��@#s@#X�@#6z@#&@#o@"�B@"h
@":*@".�@!�T@!�@!!�@!�@ �[@ ��@ N�@ -�@ $@ �@�@�@S�@�@�1@J�@�@��@o @IR@�@ѷ@�@q@A�@!@�@�$@J#@�@�R@�@�\@C�@�@@�@c�@��@�/@��@�O@z�@[�@D�@��@�P@j�@(@�y@�]@��@��@n�@H�@4@�@�-@u�@X@��@��@q@b@��@;d@@�'@�\@�@��@�@��@�@��@�'@��@�S@��@a�@-w@;@��@ѷ@�@q@]d@K^@!@ݘ@��@�V@��@dZ@a@Y@��@͟@�r@L0@:*@;�@�@��@|@`B@=�@2a@�@��@ѷ@�Y@S�@�]@�$@O@9�@'�@(@
��@
��@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B�rB�>B�	B�	B��B�	B��B�^B�jB�}B�B�B vBBBK�BNpBN�BRoBV�B^�Bn�B��B�B��B�.B��B��B�YB�BӏB��B��B	}B	@B	/�B	EmB	MPB	i�B	�?B	�^B
�B
X�B
��B[	BoB�/B��BƨB��B�B�MB�YB@�BX+BC�Bf2B�[B�B��B�`B�B�B�YB� B��B�{B��B�B��Bz�B`'B./B
�dB
��B
�oB
��B
u?B
i�B
_VB
F�B
%`B
.B
:B	�B	��B	��B	|�B	mB	_pB	]IB	[qB	SuB	M�B	H�B	?.B	1[B	)�B	�B	B�^B��B�mB�nB�VB��B�B��BбB�6B�0B˒B�B�B�DB�	BȴB�zB�KB��B�BāB�B�"BңB�SB��B޸B�4B��B�RB�<B	3B	0B	&B	�B	#�B	2B	=<B	B�B	BuB	K�B	UMB	cTB	k�B	q�B	q�B	p�B	p�B	s3B	{JB	}"B	|PB	}B	��B	��B	�KB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	�GB	��B	�UB	��B	��B	�SB	�rB	��B	��B	��B	� B	ңB	��B	ɠB	��B	�MB	�&B	�B	ޞB	�B	�B	�"B	�B	�qB	��B	�B	��B	��B	�B	�"B	�WB	�0B	��B	��B	��B	�$B	�>B	�B	��B	��B	�0B	�B	�B	�KB	�0B	��B	�"B	��B	�B	�6B	�KB	�$B	�
B	�B	��B	��B	��B	�BB	��B	�B	�XB	��B	�nB	�&B	�fB	�RB	��B	��B	�B	��B	�B	�,B	��B	��B	�,B	�tB	�&B	�B	�B	��B	��B	�dB	�=B	ٚB	�7B	�#B	�#B	�B	ּB	�B	ӏB	ӏB	��B	��B	�oB	�oB	�B	��B	��B	�:B	��B	�&B	��B	ՁB	��B	�gB	�TB	�:B	҉B	��B	ѷB	�oB	ӏB	��B	�yB	�B	ڠB	�1B	�B	רB	�EB	��B	רB	׍B	�
B	�B	��B	��B	��B	ٴB	ٴB	ٚB	ٚB	�kB	�)B	�B	��B	�'B	�'B	��B	�BB	��B	�\B	�vB	��B	�bB	��B	�B	�NB	�B	�hB	��B	�TB	�B	�ZB	�@B	�tB	��B	�B	�B	�B	�RB	�RB	�
B	�>B	�B	��B	�KB	�B	�B	�B	��B	�B	�=B	�=B	�qB	�B	�B	��B	�CB	�/B	�cB	��B	��B	��B	�B	��B	�OB	��B	��B	��B	��B	�|B	��B	��B	�hB	�hB	�hB	�hB	�B	�B	�B	�ZB	��B	�tB	��B	�B	��B	�lB	��B	��B	��B	�$B	�>B	�rB	��B	��B	�B	�^B	�^B	�^B	�xB	�B	��B	�"B	��B	�(B	�]B	�BB	�]B	��B	��B	�BB	�wB	�wB	�B	�HB
 �B
 �B
 �B
 �B
UB
�B
�B
B
[B
�B
�B
aB
�B
B
�B
B
�B
�B
�B
%B
%B
YB
�B
�B
	B
	B
B
�B
�B
�B
�B

rB
B

�B
�B
B
xB
)B

�B
�B
B
�B
B
B
6B
B
B
"B
<B
�B
B
�B
bB
�B
�B
4B
NB
hB
�B
B
�B
�B
�B
�B
�B
,B
�B
�B
�B
�B
9B
YB
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
B
B
�B
	B
�B
B
�B
�B
/B
B
�B
�B
�B
�B
;B
 'B
!�B
"NB
"4B
"4B
"�B
"�B
"�B
# B
#:B
$B
$&B
$tB
$�B
$�B
%FB
%�B
&�B
'�B
(>B
(XB
)B
)yB
)�B
*B
*KB
*KB
*B
+B
+kB
+�B
+�B
+�B
,qB
,�B
-)B
-CB
-wB
-�B
-�B
.�B
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1[B
1�B
2GB
2�B
33B
3MB
3hB
3�B
3�B
3�B
4nB
4�B
5B
5?B
5�B
5�B
5�B
5�B
5�B
6+B
6FB
6+B
6+B
6�B
6�B
6�B
7B
7�B
7�B
7�B
8B
8B
8B
8�B
9$B
9>B
9rB
9rB
9rB
9�B
9�B
:xB
:�B
:�B
:�B
;JB
;JB
;B
;�B
;�B
<B
<PB
<�B
<�B
=<B
=qB
=�B
=�B
=�B
>]B
>]B
>wB
?.B
?HB
?cB
?�B
?�B
?�B
@ B
@iB
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
A�B
BAB
BuB
B�B
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
CB
C{B
C�B
C�B
C�B
C�B
C�B
DB
DgB
D�B
D�B
D�B
D�B
ESB
EmB
EmB
E�B
F?B
F�B
F�B
GB
F�B
FtB
FYB
F�B
F�B
F�B
GzB
G_B
GB
GEB
G�B
HKB
H�B
I�B
JXB
JrB
JrB
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
K�B
K�B
LB
L�B
MB
MB
M�B
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
P.B
PHB
PbB
P�B
QB
QB
Q�B
R�B
SB
S&B
S[B
SuB
S�B
TFB
TaB
T�B
T�B
UB
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
WYB
W?B
WYB
W?B
W$B
W�B
W�B
XB
XEB
XyB
X�B
X�B
YB
YB
YKB
YKB
YKB
YeB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZQB
[	B
[	B
[#B
[	B
[#B
[=B
[WB
[WB
[�B
\B
\)B
\)B
\xB
\�B
\�B
\�B
\�B
]B
]/B
]/B
]IB
]~B
]�B
]�B
^B
^B
^B
^B
^B
^B
^B
^5B
^OB
^jB
^OB
^jB
^�B
_!B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
a-B
aHB
aHB
a-B
aHB
a-B
a|B
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
b�B
b�B
cnB
c�B
c�B
c�B
c�B
d&B
d�B
eB
eFB
e`B
e`B
ezB
e�B
e�B
e�B
fB
fB
e�B
fLB
f�B
f�B
f�B
f�B
gB
f�B
gmB
g�B
h
B
h$B
h
B
h
B
h>B
h�B
h�B
iB
i*B
iDB
i*B
iDB
i�B
jB
j0B
jB
jKB
j�B
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
nIB
ncB
n}B
n�B
n�B
o B
oB
oOB
oiB
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
q�B
q�B
q�B
r-B
raB
r|B
r|B
sB
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
uZB
u?B
u�B
u�B
u�B
vB
v�B
wB
wLB
w�B
w�B
x8B
x8B
xRB
xRB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y$B
y	B
yrB
yrB
yrB
y�B
y�B
zB
z*B
zDB
z^B
zxB
z^B
z�B
z�B
{B
{JB
{�B
{�B
{B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
~(B
~wB
~wB
~�B
~�B
~�B
B
.11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B�rB�>B�	B�	B��B�	B��B�^B�jB�}B�B�B vBBBK�BNpBN�BRoBV�B^�Bn�B��B�B��B�.B��B��B�YB�BӏB��B��B	}B	@B	/�B	EmB	MPB	i�B	�?B	�^B
�B
X�B
��B[	BoB�/B��BƨB��B�B�MB�YB@�BX+BC�Bf2B�[B�B��B�`B�B�B�YB� B��B�{B��B�B��Bz�B`'B./B
�dB
��B
�oB
��B
u?B
i�B
_VB
F�B
%`B
.B
:B	�B	��B	��B	|�B	mB	_pB	]IB	[qB	SuB	M�B	H�B	?.B	1[B	)�B	�B	B�^B��B�mB�nB�VB��B�B��BбB�6B�0B˒B�B�B�DB�	BȴB�zB�KB��B�BāB�B�"BңB�SB��B޸B�4B��B�RB�<B	3B	0B	&B	�B	#�B	2B	=<B	B�B	BuB	K�B	UMB	cTB	k�B	q�B	q�B	p�B	p�B	s3B	{JB	}"B	|PB	}B	��B	��B	�KB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	�GB	��B	�UB	��B	��B	�SB	�rB	��B	��B	��B	� B	ңB	��B	ɠB	��B	�MB	�&B	�B	ޞB	�B	�B	�"B	�B	�qB	��B	�B	��B	��B	�B	�"B	�WB	�0B	��B	��B	��B	�$B	�>B	�B	��B	��B	�0B	�B	�B	�KB	�0B	��B	�"B	��B	�B	�6B	�KB	�$B	�
B	�B	��B	��B	��B	�BB	��B	�B	�XB	��B	�nB	�&B	�fB	�RB	��B	��B	�B	��B	�B	�,B	��B	��B	�,B	�tB	�&B	�B	�B	��B	��B	�dB	�=B	ٚB	�7B	�#B	�#B	�B	ּB	�B	ӏB	ӏB	��B	��B	�oB	�oB	�B	��B	��B	�:B	��B	�&B	��B	ՁB	��B	�gB	�TB	�:B	҉B	��B	ѷB	�oB	ӏB	��B	�yB	�B	ڠB	�1B	�B	רB	�EB	��B	רB	׍B	�
B	�B	��B	��B	��B	ٴB	ٴB	ٚB	ٚB	�kB	�)B	�B	��B	�'B	�'B	��B	�BB	��B	�\B	�vB	��B	�bB	��B	�B	�NB	�B	�hB	��B	�TB	�B	�ZB	�@B	�tB	��B	�B	�B	�B	�RB	�RB	�
B	�>B	�B	��B	�KB	�B	�B	�B	��B	�B	�=B	�=B	�qB	�B	�B	��B	�CB	�/B	�cB	��B	��B	��B	�B	��B	�OB	��B	��B	��B	��B	�|B	��B	��B	�hB	�hB	�hB	�hB	�B	�B	�B	�ZB	��B	�tB	��B	�B	��B	�lB	��B	��B	��B	�$B	�>B	�rB	��B	��B	�B	�^B	�^B	�^B	�xB	�B	��B	�"B	��B	�(B	�]B	�BB	�]B	��B	��B	�BB	�wB	�wB	�B	�HB
 �B
 �B
 �B
 �B
UB
�B
�B
B
[B
�B
�B
aB
�B
B
�B
B
�B
�B
�B
%B
%B
YB
�B
�B
	B
	B
B
�B
�B
�B
�B

rB
B

�B
�B
B
xB
)B

�B
�B
B
�B
B
B
6B
B
B
"B
<B
�B
B
�B
bB
�B
�B
4B
NB
hB
�B
B
�B
�B
�B
�B
�B
,B
�B
�B
�B
�B
9B
YB
�B
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
B
B
�B
	B
�B
B
�B
�B
/B
B
�B
�B
�B
�B
;B
 'B
!�B
"NB
"4B
"4B
"�B
"�B
"�B
# B
#:B
$B
$&B
$tB
$�B
$�B
%FB
%�B
&�B
'�B
(>B
(XB
)B
)yB
)�B
*B
*KB
*KB
*B
+B
+kB
+�B
+�B
+�B
,qB
,�B
-)B
-CB
-wB
-�B
-�B
.�B
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1[B
1�B
2GB
2�B
33B
3MB
3hB
3�B
3�B
3�B
4nB
4�B
5B
5?B
5�B
5�B
5�B
5�B
5�B
6+B
6FB
6+B
6+B
6�B
6�B
6�B
7B
7�B
7�B
7�B
8B
8B
8B
8�B
9$B
9>B
9rB
9rB
9rB
9�B
9�B
:xB
:�B
:�B
:�B
;JB
;JB
;B
;�B
;�B
<B
<PB
<�B
<�B
=<B
=qB
=�B
=�B
=�B
>]B
>]B
>wB
?.B
?HB
?cB
?�B
?�B
?�B
@ B
@iB
@�B
@�B
A B
AoB
A�B
A�B
A�B
A�B
A�B
BAB
BuB
B�B
BuB
B�B
B�B
B�B
B�B
B�B
B�B
CB
CB
C{B
C�B
C�B
C�B
C�B
C�B
DB
DgB
D�B
D�B
D�B
D�B
ESB
EmB
EmB
E�B
F?B
F�B
F�B
GB
F�B
FtB
FYB
F�B
F�B
F�B
GzB
G_B
GB
GEB
G�B
HKB
H�B
I�B
JXB
JrB
JrB
JXB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
K�B
K�B
LB
L�B
MB
MB
M�B
M�B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
P.B
PHB
PbB
P�B
QB
QB
Q�B
R�B
SB
S&B
S[B
SuB
S�B
TFB
TaB
T�B
T�B
UB
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
WYB
W?B
WYB
W?B
W$B
W�B
W�B
XB
XEB
XyB
X�B
X�B
YB
YB
YKB
YKB
YKB
YeB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZQB
[	B
[	B
[#B
[	B
[#B
[=B
[WB
[WB
[�B
\B
\)B
\)B
\xB
\�B
\�B
\�B
\�B
]B
]/B
]/B
]IB
]~B
]�B
]�B
^B
^B
^B
^B
^B
^B
^B
^5B
^OB
^jB
^OB
^jB
^�B
_!B
_VB
_pB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`�B
`�B
a-B
aHB
aHB
a-B
aHB
a-B
a|B
abB
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
b�B
b�B
cnB
c�B
c�B
c�B
c�B
d&B
d�B
eB
eFB
e`B
e`B
ezB
e�B
e�B
e�B
fB
fB
e�B
fLB
f�B
f�B
f�B
f�B
gB
f�B
gmB
g�B
h
B
h$B
h
B
h
B
h>B
h�B
h�B
iB
i*B
iDB
i*B
iDB
i�B
jB
j0B
jB
jKB
j�B
j�B
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
nIB
ncB
n}B
n�B
n�B
o B
oB
oOB
oiB
o�B
o�B
o�B
p!B
pUB
p�B
p�B
p�B
q�B
q�B
q�B
r-B
raB
r|B
r|B
sB
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
uZB
u?B
u�B
u�B
u�B
vB
v�B
wB
wLB
w�B
w�B
x8B
x8B
xRB
xRB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y$B
y	B
yrB
yrB
yrB
y�B
y�B
zB
z*B
zDB
z^B
zxB
z^B
z�B
z�B
{B
{JB
{�B
{�B
{B
|B
|6B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
~(B
~wB
~wB
~�B
~�B
~�B
B
.11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104936  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174525  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174525                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024533  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024533  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                