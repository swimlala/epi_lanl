CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:13Z creation;2022-06-04T17:52:14Z conversion to V3.1      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604175213  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @������1   @��^З�@1���l��c\(�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @@  @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�33B�33B���B���B�  B�  B���B���B�  C 33C�3C  C��C�fC
  C  C  C  C  C  C  C  C  C�C33C   C!�fC$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF33CG��CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�3D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Dz�@�=q@�=qA�A!�AA�Aa�A��\A�A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�W
B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
BԊ=B�#�B�W
B�W
B��B��B�#�B�#�B��B��B�#�C EC�C�C޹C�RC
�C�C�C�C�C�C�C�C�C+�CEC �C!�RC$�C&�C'�RC*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@+�CB+�CD�CFECG��CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C]�RC`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp+�Cr�Ct�Cv�Cx�Cz�C|�C}�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-
�D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD~DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��pD��pD�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�EpD��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D�=D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�pD�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΂=D��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�B=DՂ=D��=D�=D�B=Dւ=D��=D�=D�B=Dׂ=D��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�
D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D�up111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ߤA��aAɹ$A��A��A��xA��A��A���A�>�AȷAȅ�A�:�A��Aǟ�A�G�A�%zA���AƃA�X�A�_A��A�%FA�=A��A�ߤA���AŷLA�ncA�I�A�A�MA��A�eA�I�A�qvAŏ\A�g8A�3hA��A��DA���A���A��AħA�-wA��mA���AätA�U�A©�A�l�A��A��QA��A�,A���A��A���A�A�A��jA���A��fA���A�H�A���A��)A��zA��7A��RA�d�A�C�A��A���A���A��A�p�A��A��jA���A�r�A��A�/�A�/�A�OA�%�A�5tA��"A��zA��A�D3A�xA���A��A�/�A�{�A�NpA���A�w2A��A{E�AyVAt��Ak�Ab͟A[ɆAYS&AVSAN�dAL�1AKe,AJ��AIQ�AH�[AG��AEYKADZACF�AA��AA�A?��A>�A=�_A<-wA; \A:c�A:MA8Q�A74A6cA4�IA4�hA40UA2�PA2�A1��A0��A/��A/DgA.�*A.�|A/c A/��A/��A/�A/��A/A.��A.ZA.bA-��A-!-A,�jA+ĜA+"�A*	lA'�jA'MA'�A'4nA%�A#kQA!�:A!(�A �A +AMA�XA��A1�A��A�A�A1�AE9A^5AںA��A�"A�ZA��AhsAOA�Ag�AiDAVmA�A�}A7A�A)_AƨA�A�A~�AYA�hA��A�A��Ah�As�A�]AW�A�Au%A��AR�A��AzA
��A
N�A	y>A	�A�DA��AN�A4A�mA��A�7A/�AeAݘAw2AY�AZ�A[WA[WAW?AS&AMA@�A"hAHA��A�	A�AxAHAYA	lA�A��A}�AXADgAqA�A�Af�A �x@��>@��@���@���@�3�@��[@��@�L�@�V�@���@��[@�\@�@��@�S�@�C@�͟@�l"@���@��@�q@�@�@�~@�
�@�=@��@���@�H�@�5?@�y�@�v`@��@�0U@��@縻@�4@�?}@��@�ѷ@�Ɇ@�D@�s�@�<�@�ԕ@�'�@�U2@��@㕁@�33@�"�@��@�I@�h�@��@��@�(�@ߔ�@�8@��@���@�&�@ݿH@�N<@��@ܺ�@�[�@۸�@��@�*�@ټ@�!�@�;�@�o @���@ֆY@���@մ�@�+�@��@Ӏ4@��@Ҿ@�@�e,@ІY@�x�@�C@��@μj@�_�@�@ͦ�@̹$@˃{@�Z@�$@�u@� �@���@�v`@�@��H@Ȳ�@�!@�s@Ǻ^@�S&@�
=@�e�@��r@���@�X@��p@ķ�@ĕ@�W�@��@íC@�#�@°�@�+k@�>�@�@���@��[@�Ft@��d@��@�r�@�)�@��)@�ϫ@���@�Vm@�4�@��@��@���@���@��@�p�@��O@���@���@�v�@�.�@���@���@���@��@�}�@�S�@���@��c@��p@�z@�u@���@�g�@�33@� i@�K^@���@�IR@��@��4@�l"@�]d@�GE@���@��@��@���@�~(@� �@��@��S@�S&@�C�@��@���@�M�@���@���@�|�@��@���@�@��@�N<@��@���@��x@�C-@��:@�<6@�*0@��@��@��U@�!�@��.@�@���@���@�'�@��@��@� i@��@��@�7@��d@�]�@���@���@��@�tT@�-@��D@���@�p�@� \@��?@���@��@���@���@�ƨ@��a@���@�&@��E@�_�@�'R@�J@��@�`B@��2@��X@���@��Y@�m�@�\�@�N�@��@���@�.I@��@� i@��@�]d@���@���@�Y@���@��u@�N�@��@���@���@�m]@��@�l�@�!�@��>@��;@�l�@�4@�-w@��@��@�>B@�e@��@�f�@�5�@�/@�!-@�+@�@��2@�{�@�!@��@���@��d@��M@�6z@���@��@���@��b@�Ɇ@��@� i@�C@��@��@��@���@�i�@�*�@���@���@�C�@��@���@�ff@�-@�  @��*@�f�@�Y@���@��Y@��@��@��6@��@@�S�@��@���@��@��X@���@��E@��u@�p;@�~@�zx@�6z@��@��@��E@��O@�8�@��@��T@�ԕ@���@��M@�|@�f�@� \@�͟@�ff@�Z@�:�@� �@� �@��@�J@� �@���@���@���@�c@�o�@�A @��@���@��\@�H@�u@��@W?@�@~!�@}�@}7L@}@|�5@|�_@|c�@{�@z�\@z@y��@y�C@y��@y��@y�@x�$@xj@x9X@w�&@w��@w!-@v
�@u�@t��@t[�@s~�@s)_@rs�@q��@q}�@q5�@p��@o�@o��@o�@nȴ@ni�@nTa@nTa@nOv@nE�@n#:@n �@m��@m�@l�D@lc�@l,=@l�@ks@j��@jq�@j@i�~@iDg@h��@h��@hw�@h7�@g�r@g��@gZ�@g(@f��@f��@f��@f@�@eϫ@e�X@eX@e*0@d�@d��@dc�@dH@d	�@cv`@c8@b�@b��@bOv@b@�@b�@a��@a7L@`�|@`��@`z�@_��@_qv@_,�@^�@^�}@^{�@]�@]|@]q@\�.@[�@[�F@[_p@[
=@Z��@ZOv@Z�@Yx�@YF@Y4@X�v@X��@W�a@We�@W"�@Vȴ@V�r@Va|@V)�@U�D@U��@U=�@T�5@T>B@S�:@S_p@S'�@S�@R;�@Q��@Q@P��@P��@Py>@P$@O�Q@O��@N҉@M��@M�@M@L�)@LM@Kt�@K@J�1@JkQ@J$�@I��@Ic@I5�@H�)@HA�@H@Gݘ@G��@G�@Go�@F�H@F_�@E��@ET�@E�@E�@D��@D��@D��@D��@Dz�@Dl"@De�@D2�@D'R@C�@C��@C�@B#:@AG�@A�@A;@@�P@@��@@�_@@j@@6@@7@?��@?9�@?�@>�"@>�c@>�y@>�@>��@>p;@>)�@>4@>J@=��@=p�@<��@<��@<bN@<K^@<$@;�]@;��@;S�@:�B@:��@:l�@:H�@:�@9�t@9�"@9hs@9�@8�@8��@8��@8Xy@8*�@7ݘ@7˒@7�@@7�$@7C@6��@6z@6-@5�9@5�C@5N<@4�	@4�[@4��@47�@3��@2�2@2_�@25?@2�@2u@1�@1�#@1N<@0��@/ݘ@.s�@.R�@.5?@.($@.$�@.�@.�@. �@-�@-��@-�h@-rG@-#�@,ѷ@,u�@,�@+��@+�6@+�F@+��@+y�@+Mj@+=@++@++@+$t@+"�@+�@+�@*�M@*?@)j@(ѷ@(y>@(q@(A�@'�@'W?@''�@''�@'C@'�@&�H@&��@&=q@&@%=�@$j@$7�@$M@#�0@#"�@"� @"Q@"�@!�D@!�@!�z@!N<@!@@ ��@ ��@ �@ ��@ [�@��@6z@�6@\�@�@�z@Vm@�@��@�?@�e@��@A�@M@�@��@S�@(@�R@� @�#@��@��@��@v`@W?@S�@U�@U�@RT@E9@8@'�@�@S@��@�H@�m@Ov@��@�@�"@}�@J�@��@*�@��@�w@��@t�@P�@.I@$t@�@�!@z@)�@�@�@_@ �@�D@�Z@��@�#@��@�@�'@|@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ߤA��aAɹ$A��A��A��xA��A��A���A�>�AȷAȅ�A�:�A��Aǟ�A�G�A�%zA���AƃA�X�A�_A��A�%FA�=A��A�ߤA���AŷLA�ncA�I�A�A�MA��A�eA�I�A�qvAŏ\A�g8A�3hA��A��DA���A���A��AħA�-wA��mA���AätA�U�A©�A�l�A��A��QA��A�,A���A��A���A�A�A��jA���A��fA���A�H�A���A��)A��zA��7A��RA�d�A�C�A��A���A���A��A�p�A��A��jA���A�r�A��A�/�A�/�A�OA�%�A�5tA��"A��zA��A�D3A�xA���A��A�/�A�{�A�NpA���A�w2A��A{E�AyVAt��Ak�Ab͟A[ɆAYS&AVSAN�dAL�1AKe,AJ��AIQ�AH�[AG��AEYKADZACF�AA��AA�A?��A>�A=�_A<-wA; \A:c�A:MA8Q�A74A6cA4�IA4�hA40UA2�PA2�A1��A0��A/��A/DgA.�*A.�|A/c A/��A/��A/�A/��A/A.��A.ZA.bA-��A-!-A,�jA+ĜA+"�A*	lA'�jA'MA'�A'4nA%�A#kQA!�:A!(�A �A +AMA�XA��A1�A��A�A�A1�AE9A^5AںA��A�"A�ZA��AhsAOA�Ag�AiDAVmA�A�}A7A�A)_AƨA�A�A~�AYA�hA��A�A��Ah�As�A�]AW�A�Au%A��AR�A��AzA
��A
N�A	y>A	�A�DA��AN�A4A�mA��A�7A/�AeAݘAw2AY�AZ�A[WA[WAW?AS&AMA@�A"hAHA��A�	A�AxAHAYA	lA�A��A}�AXADgAqA�A�Af�A �x@��>@��@���@���@�3�@��[@��@�L�@�V�@���@��[@�\@�@��@�S�@�C@�͟@�l"@���@��@�q@�@�@�~@�
�@�=@��@���@�H�@�5?@�y�@�v`@��@�0U@��@縻@�4@�?}@��@�ѷ@�Ɇ@�D@�s�@�<�@�ԕ@�'�@�U2@��@㕁@�33@�"�@��@�I@�h�@��@��@�(�@ߔ�@�8@��@���@�&�@ݿH@�N<@��@ܺ�@�[�@۸�@��@�*�@ټ@�!�@�;�@�o @���@ֆY@���@մ�@�+�@��@Ӏ4@��@Ҿ@�@�e,@ІY@�x�@�C@��@μj@�_�@�@ͦ�@̹$@˃{@�Z@�$@�u@� �@���@�v`@�@��H@Ȳ�@�!@�s@Ǻ^@�S&@�
=@�e�@��r@���@�X@��p@ķ�@ĕ@�W�@��@íC@�#�@°�@�+k@�>�@�@���@��[@�Ft@��d@��@�r�@�)�@��)@�ϫ@���@�Vm@�4�@��@��@���@���@��@�p�@��O@���@���@�v�@�.�@���@���@���@��@�}�@�S�@���@��c@��p@�z@�u@���@�g�@�33@� i@�K^@���@�IR@��@��4@�l"@�]d@�GE@���@��@��@���@�~(@� �@��@��S@�S&@�C�@��@���@�M�@���@���@�|�@��@���@�@��@�N<@��@���@��x@�C-@��:@�<6@�*0@��@��@��U@�!�@��.@�@���@���@�'�@��@��@� i@��@��@�7@��d@�]�@���@���@��@�tT@�-@��D@���@�p�@� \@��?@���@��@���@���@�ƨ@��a@���@�&@��E@�_�@�'R@�J@��@�`B@��2@��X@���@��Y@�m�@�\�@�N�@��@���@�.I@��@� i@��@�]d@���@���@�Y@���@��u@�N�@��@���@���@�m]@��@�l�@�!�@��>@��;@�l�@�4@�-w@��@��@�>B@�e@��@�f�@�5�@�/@�!-@�+@�@��2@�{�@�!@��@���@��d@��M@�6z@���@��@���@��b@�Ɇ@��@� i@�C@��@��@��@���@�i�@�*�@���@���@�C�@��@���@�ff@�-@�  @��*@�f�@�Y@���@��Y@��@��@��6@��@@�S�@��@���@��@��X@���@��E@��u@�p;@�~@�zx@�6z@��@��@��E@��O@�8�@��@��T@�ԕ@���@��M@�|@�f�@� \@�͟@�ff@�Z@�:�@� �@� �@��@�J@� �@���@���@���@�c@�o�@�A @��@���@��\@�H@�u@��@W?@�@~!�@}�@}7L@}@|�5@|�_@|c�@{�@z�\@z@y��@y�C@y��@y��@y�@x�$@xj@x9X@w�&@w��@w!-@v
�@u�@t��@t[�@s~�@s)_@rs�@q��@q}�@q5�@p��@o�@o��@o�@nȴ@ni�@nTa@nTa@nOv@nE�@n#:@n �@m��@m�@l�D@lc�@l,=@l�@ks@j��@jq�@j@i�~@iDg@h��@h��@hw�@h7�@g�r@g��@gZ�@g(@f��@f��@f��@f@�@eϫ@e�X@eX@e*0@d�@d��@dc�@dH@d	�@cv`@c8@b�@b��@bOv@b@�@b�@a��@a7L@`�|@`��@`z�@_��@_qv@_,�@^�@^�}@^{�@]�@]|@]q@\�.@[�@[�F@[_p@[
=@Z��@ZOv@Z�@Yx�@YF@Y4@X�v@X��@W�a@We�@W"�@Vȴ@V�r@Va|@V)�@U�D@U��@U=�@T�5@T>B@S�:@S_p@S'�@S�@R;�@Q��@Q@P��@P��@Py>@P$@O�Q@O��@N҉@M��@M�@M@L�)@LM@Kt�@K@J�1@JkQ@J$�@I��@Ic@I5�@H�)@HA�@H@Gݘ@G��@G�@Go�@F�H@F_�@E��@ET�@E�@E�@D��@D��@D��@D��@Dz�@Dl"@De�@D2�@D'R@C�@C��@C�@B#:@AG�@A�@A;@@�P@@��@@�_@@j@@6@@7@?��@?9�@?�@>�"@>�c@>�y@>�@>��@>p;@>)�@>4@>J@=��@=p�@<��@<��@<bN@<K^@<$@;�]@;��@;S�@:�B@:��@:l�@:H�@:�@9�t@9�"@9hs@9�@8�@8��@8��@8Xy@8*�@7ݘ@7˒@7�@@7�$@7C@6��@6z@6-@5�9@5�C@5N<@4�	@4�[@4��@47�@3��@2�2@2_�@25?@2�@2u@1�@1�#@1N<@0��@/ݘ@.s�@.R�@.5?@.($@.$�@.�@.�@. �@-�@-��@-�h@-rG@-#�@,ѷ@,u�@,�@+��@+�6@+�F@+��@+y�@+Mj@+=@++@++@+$t@+"�@+�@+�@*�M@*?@)j@(ѷ@(y>@(q@(A�@'�@'W?@''�@''�@'C@'�@&�H@&��@&=q@&@%=�@$j@$7�@$M@#�0@#"�@"� @"Q@"�@!�D@!�@!�z@!N<@!@@ ��@ ��@ �@ ��@ [�@��@6z@�6@\�@�@�z@Vm@�@��@�?@�e@��@A�@M@�@��@S�@(@�R@� @�#@��@��@��@v`@W?@S�@U�@U�@RT@E9@8@'�@�@S@��@�H@�m@Ov@��@�@�"@}�@J�@��@*�@��@�w@��@t�@P�@.I@$t@�@�!@z@)�@�@�@_@ �@�D@�Z@��@�#@��@�@�'@|@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�IB�<BfBEB/ B?�BNpBR BYB�B��B�CB�BB�(B�RB	3B	�B��B�B�=B�B��B�+B��B�B�B��B�MB�/B�B�B��B�>B	B	B	"�B	2�B	9rB	=�B	E9B	M�B	W�B	\�B	ezB	vB	��B	�DB	��B	�7B	��B
4�B
h�B
��B
�;B
z*B
raB
�B
�B
��B
�.B
�7B
�vB�B�B/�BL~BT{BfBv`B��B��B��B�-B��B��B��B��B�bB�LB�B��B��B��B��B��B�bB��Bg�BaBP}B8lBmB
�yB
�B
��B
��B
rGB
e`B
R B
,�B	��B	��B	��B	r�B	<�B	�B	�B	JB�(B�nB�hB�B�B�B�fB��B�B�B��B�%B��B��B�B	�B	6B	�B	'mB	GEB	S&B	b4B	f�B	pB	}�B	|�B	vB	p�B	iDB	b4B	_;B	k�B	r�B	�DB	��B	��B	�uB	�B	�rB	ʌB	��B	�2B	��B	��B	�CB	��B	�QB	�WB	��B	��B	��B	�\B	�.B	ŢB	�;B	��B	�=B	οB	��B	��B	�NB	��B	�B	�:B	ңB	��B	ɆB	�7B	ʦB	ѝB	��B	οB	�B	�B	�B	бB	�B	��B	�mB	�sB	��B	��B	ݘB	��B	��B	�LB	�LB	�]B	�B	�B	�LB	�B	�@B	�&B	�B	�B	�+B	�B	�B	�6B	�FB	�BB	�VB	�VB	��B	�|B	��B	��B	�B	�B	��B	�pB	�;B	�'B	�B	�|B	�B	��B	��B	��B	�B	�B	�ZB	�&B	��B	��B	��B	�;B	��B	�HB	�4B	��B	��B	�NB	�B	�B	�sB	�eB	�B	�B	�B	��B	��B	�B	�XB	��B	��B	��B	��B	�DB	�DB	�B	�B	��B	�B	�B	߾B	�pB	�hB	�-B	��B	�\B	��B	ߤB	߾B	ߊB	�VB	�VB	߾B	�HB	�B	�'B	�VB	��B	޸B	�B	�!B	��B	�5B	�B	ޞB	�B	�pB	�BB	��B	�B	�-B	�HB	�B	��B	�'B	ߊB	ߊB	�!B	�!B	�B	ߊB	��B	�;B	��B	��B	�OB	��B	�~B	�~B	�xB	�B	�dB	�5B	ޞB	��B	��B	�;B	ޞB	�OB	ބB	��B	߾B	�VB	�!B	޸B	�VB	ߤB	߾B	�VB	�;B	ߤB	�;B	޸B	�!B	ߤB	ߊB	߾B	�bB	�hB	�4B	��B	�tB	�B	�B	�hB	�B	�B	�hB	�B	�B	�nB	�nB	��B	�B	�CB	��B	�B	�B	�B	��B	�aB	�MB	�B	�MB	�B	��B	�3B	��B	��B	�B	��B	�ZB	�ZB	�%B	��B	��B	��B	��B	��B	�fB	�fB	�2B	�fB	�LB	��B	�8B	�8B	��B	��B	�B	��B	�B	�B	�aB	�-B	�aB	�B	��B	�B	�B	�nB	�LB	��B	�B	�"B	��B	�6B	��B	�<B	�B	�"B	��B	�VB	�qB	��B	��B	�PB	�6B	�PB	�JB	�B	�0B	�JB	�0B	��B	�DB	�^B	�DB	�DB	��B	�XB	�XB	��B	�rB	�rB	�*B	��B	�B	�PB	��B	�jB	��B	�B	�VB	��B	��B	��B	�B	��B	��B	�wB	�B	�.B	��B	��B	��B
  B	��B	��B	��B	�.B	��B	�wB	��B	��B	��B	��B	�}B
AB
9B
9B
mB
+B
�B
�B
�B
1B
fB
�B

	B

�B
B
~B
PB
�B
�B
�B
�B
�B
�B
�B
�B
B
:B
@B
�B
�B
,B
,B
B
aB
�B
�B
�B
 B
TB
:B
TB
:B
�B
B
HB
B
�B
�B
�B
�B
�B
 B
[B
B
,B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
2B
B
2B
B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
!B
�B
�B
�B
�B
 �B
 �B
!bB
!�B
!�B
"B
"4B
"hB
"�B
# B
"�B
#B
#B
#TB
#�B
#�B
$�B
%B
%FB
%�B
%�B
%�B
&�B
'8B
'�B
(
B
(>B
(
B
(XB
)B
)�B
)yB
)�B
*B
+�B
,"B
+�B
*�B
*�B
+�B
+�B
,B
,�B
,�B
,�B
-�B
-�B
.�B
/ B
/B
/B
/ B
/B
/5B
/iB
/�B
/�B
0B
0UB
0�B
0�B
1[B
1�B
1�B
1�B
2B
2GB
2-B
2-B
2�B
2�B
2�B
3B
3B
3hB
3�B
3�B
3�B
4B
3�B
3�B
4�B
6�B
7fB
7LB
7�B
7�B
7�B
88B
8B
8�B
8lB
8�B
8lB
8lB
8RB
9rB
9XB
9�B
9�B
9�B
9�B
9�B
:*B
:�B
;0B
;JB
;JB
;JB
;0B
;�B
<B
<jB
<�B
=B
=<B
=qB
=�B
=�B
=�B
>B
=�B
>wB
>wB
>�B
>�B
>�B
>�B
?cB
?HB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@B
@OB
@iB
@�B
@�B
@�B
@�B
@�B
AUB
AoB
A�B
A�B
B'B
B[B
BuB
B�B
B�B
B�B
CGB
CaB
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
ESB
E�B
E�B
E�B
FB
FYB
GB
G_B
GzB
G�B
G�B
G�B
G�B
H1B
HKB
H�B
H�B
I�B
J#B
J#B
JrB
J=B
KB
KDB
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
MB
MPB
M�B
M�B
N<B
N�B
N�B
O(B
OB
OBB
O�B
O�B
O�B
O�B
PbB
PHB
P}B
P�B
P}B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
RB
RB
R B
R:B
R:B
R:B
R:B
RTB
R:B
RTB
R�B
R�B
S�B
TFB
TFB
TFB
TFB
T,B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
VSB
V9B
VSB
V�B
W
B
W?B
WYB
WsB
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
[�B
\)B
\CB
\�B
\�B
\�B
]/B
]dB
]~B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
_B
_;B
`BB
abB
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
bhB
b�B
b�B
cTB
c�B
c�B
c�B
c�B
dB
dB
d&B
d&B
d@B
d@B
d@B
d@B
d@B
d&B
c�B
d�B
eB
ezB
e�B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
fLB
g8B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
iB
iB
iB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
j�B
kQB
k�B
lB
lWB
l�B
mB
mCB
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
o5B
o�B
oiB
pUB
qvB
rB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
tB
t�B
t�B
t�B
t�B
u�B
vFB
v`B
v�B
v�B
v�B
w2B
w2B
w2B
wfB
w�B
w�B
xRB
xRB
xRB
xRB
xRB
xlB
xlB
xlB
x�B
xlB
x�B
x�B
x�B
y>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�IB�<BfBEB/ B?�BNpBR BYB�B��B�CB�BB�(B�RB	3B	�B��B�B�=B�B��B�+B��B�B�B��B�MB�/B�B�B��B�>B	B	B	"�B	2�B	9rB	=�B	E9B	M�B	W�B	\�B	ezB	vB	��B	�DB	��B	�7B	��B
4�B
h�B
��B
�;B
z*B
raB
�B
�B
��B
�.B
�7B
�vB�B�B/�BL~BT{BfBv`B��B��B��B�-B��B��B��B��B�bB�LB�B��B��B��B��B��B�bB��Bg�BaBP}B8lBmB
�yB
�B
��B
��B
rGB
e`B
R B
,�B	��B	��B	��B	r�B	<�B	�B	�B	JB�(B�nB�hB�B�B�B�fB��B�B�B��B�%B��B��B�B	�B	6B	�B	'mB	GEB	S&B	b4B	f�B	pB	}�B	|�B	vB	p�B	iDB	b4B	_;B	k�B	r�B	�DB	��B	��B	�uB	�B	�rB	ʌB	��B	�2B	��B	��B	�CB	��B	�QB	�WB	��B	��B	��B	�\B	�.B	ŢB	�;B	��B	�=B	οB	��B	��B	�NB	��B	�B	�:B	ңB	��B	ɆB	�7B	ʦB	ѝB	��B	οB	�B	�B	�B	бB	�B	��B	�mB	�sB	��B	��B	ݘB	��B	��B	�LB	�LB	�]B	�B	�B	�LB	�B	�@B	�&B	�B	�B	�+B	�B	�B	�6B	�FB	�BB	�VB	�VB	��B	�|B	��B	��B	�B	�B	��B	�pB	�;B	�'B	�B	�|B	�B	��B	��B	��B	�B	�B	�ZB	�&B	��B	��B	��B	�;B	��B	�HB	�4B	��B	��B	�NB	�B	�B	�sB	�eB	�B	�B	�B	��B	��B	�B	�XB	��B	��B	��B	��B	�DB	�DB	�B	�B	��B	�B	�B	߾B	�pB	�hB	�-B	��B	�\B	��B	ߤB	߾B	ߊB	�VB	�VB	߾B	�HB	�B	�'B	�VB	��B	޸B	�B	�!B	��B	�5B	�B	ޞB	�B	�pB	�BB	��B	�B	�-B	�HB	�B	��B	�'B	ߊB	ߊB	�!B	�!B	�B	ߊB	��B	�;B	��B	��B	�OB	��B	�~B	�~B	�xB	�B	�dB	�5B	ޞB	��B	��B	�;B	ޞB	�OB	ބB	��B	߾B	�VB	�!B	޸B	�VB	ߤB	߾B	�VB	�;B	ߤB	�;B	޸B	�!B	ߤB	ߊB	߾B	�bB	�hB	�4B	��B	�tB	�B	�B	�hB	�B	�B	�hB	�B	�B	�nB	�nB	��B	�B	�CB	��B	�B	�B	�B	��B	�aB	�MB	�B	�MB	�B	��B	�3B	��B	��B	�B	��B	�ZB	�ZB	�%B	��B	��B	��B	��B	��B	�fB	�fB	�2B	�fB	�LB	��B	�8B	�8B	��B	��B	�B	��B	�B	�B	�aB	�-B	�aB	�B	��B	�B	�B	�nB	�LB	��B	�B	�"B	��B	�6B	��B	�<B	�B	�"B	��B	�VB	�qB	��B	��B	�PB	�6B	�PB	�JB	�B	�0B	�JB	�0B	��B	�DB	�^B	�DB	�DB	��B	�XB	�XB	��B	�rB	�rB	�*B	��B	�B	�PB	��B	�jB	��B	�B	�VB	��B	��B	��B	�B	��B	��B	�wB	�B	�.B	��B	��B	��B
  B	��B	��B	��B	�.B	��B	�wB	��B	��B	��B	��B	�}B
AB
9B
9B
mB
+B
�B
�B
�B
1B
fB
�B

	B

�B
B
~B
PB
�B
�B
�B
�B
�B
�B
�B
�B
B
:B
@B
�B
�B
,B
,B
B
aB
�B
�B
�B
 B
TB
:B
TB
:B
�B
B
HB
B
�B
�B
�B
�B
�B
 B
[B
B
,B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
2B
B
2B
B
�B
,B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
!B
�B
�B
�B
�B
 �B
 �B
!bB
!�B
!�B
"B
"4B
"hB
"�B
# B
"�B
#B
#B
#TB
#�B
#�B
$�B
%B
%FB
%�B
%�B
%�B
&�B
'8B
'�B
(
B
(>B
(
B
(XB
)B
)�B
)yB
)�B
*B
+�B
,"B
+�B
*�B
*�B
+�B
+�B
,B
,�B
,�B
,�B
-�B
-�B
.�B
/ B
/B
/B
/ B
/B
/5B
/iB
/�B
/�B
0B
0UB
0�B
0�B
1[B
1�B
1�B
1�B
2B
2GB
2-B
2-B
2�B
2�B
2�B
3B
3B
3hB
3�B
3�B
3�B
4B
3�B
3�B
4�B
6�B
7fB
7LB
7�B
7�B
7�B
88B
8B
8�B
8lB
8�B
8lB
8lB
8RB
9rB
9XB
9�B
9�B
9�B
9�B
9�B
:*B
:�B
;0B
;JB
;JB
;JB
;0B
;�B
<B
<jB
<�B
=B
=<B
=qB
=�B
=�B
=�B
>B
=�B
>wB
>wB
>�B
>�B
>�B
>�B
?cB
?HB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@B
@OB
@iB
@�B
@�B
@�B
@�B
@�B
AUB
AoB
A�B
A�B
B'B
B[B
BuB
B�B
B�B
B�B
CGB
CaB
C�B
C�B
D3B
DgB
D�B
D�B
D�B
EB
ESB
E�B
E�B
E�B
FB
FYB
GB
G_B
GzB
G�B
G�B
G�B
G�B
H1B
HKB
H�B
H�B
I�B
J#B
J#B
JrB
J=B
KB
KDB
K�B
K�B
K�B
K�B
K�B
K�B
LB
L�B
MB
MPB
M�B
M�B
N<B
N�B
N�B
O(B
OB
OBB
O�B
O�B
O�B
O�B
PbB
PHB
P}B
P�B
P}B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
RB
RB
R B
R:B
R:B
R:B
R:B
RTB
R:B
RTB
R�B
R�B
S�B
TFB
TFB
TFB
TFB
T,B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
VSB
V9B
VSB
V�B
W
B
W?B
WYB
WsB
W�B
W�B
W�B
XB
X�B
X�B
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[qB
[�B
[�B
\)B
\CB
\�B
\�B
\�B
]/B
]dB
]~B
^OB
^�B
^�B
^�B
^�B
^�B
^�B
_B
_;B
`BB
abB
abB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
bhB
b�B
b�B
cTB
c�B
c�B
c�B
c�B
dB
dB
d&B
d&B
d@B
d@B
d@B
d@B
d@B
d&B
c�B
d�B
eB
ezB
e�B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
ffB
fLB
g8B
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
iB
iB
iB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
j�B
kQB
k�B
lB
lWB
l�B
mB
mCB
m�B
m�B
m�B
m�B
nIB
ncB
ncB
n�B
n�B
o5B
o�B
oiB
pUB
qvB
rB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
tB
t�B
t�B
t�B
t�B
u�B
vFB
v`B
v�B
v�B
v�B
w2B
w2B
w2B
wfB
w�B
w�B
xRB
xRB
xRB
xRB
xRB
xlB
xlB
xlB
x�B
xlB
x�B
x�B
x�B
y>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104952  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175213  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175214  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175214                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025221  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025221  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                