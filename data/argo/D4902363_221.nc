CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-21T00:35:21Z creation;2018-03-21T00:35:25Z conversion to V3.1;2019-12-19T07:46:42Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180321003521  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_221                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�T��-�1   @�T�O���@:#g��	l�dh��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D���D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Dz�@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�BpG�BxG�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C7�RC:�C<�C>�C@�CB+�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw{Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{�{D|{D|�{D}{D}�{D~{D~�{D{D�{D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��qD��=D�=D�?
D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D=D��=D�=D�B=DÂ=D��=D�=D�B=DĂ=D��=D�=D�B=Dł=D��=D�=D�B=DƂ=D��=D�=D�B=Dǂ=D��=D��
D�B=DȂ=D��=D�=D�B=Dɂ=D��=D�=D�B=Dʂ=D��=D�=D�B=D˂=D��=D�=D�B=D̂=D��=D�=D�B=D͂=D��=D�=D�B=D΅qD��=D�=D�B=Dς=D��=D�=D�B=DЂ=D��=D�=D�B=Dт=D��=D�=D�B=D҂=D��=D�=D�B=Dӂ=D��=D�=D�B=DԂ=D��=D�=D�?
D�
D��=D�=D�B=Dւ=D��=D�=D�EqDׅqD��=D�=D�B=D؂=D��=D�=D�B=Dق=D��=D�=D�B=Dڂ=D��=D�=D�B=Dۂ=D��=D�=D�B=D܂=D��=D�=D�B=D݂=D��=D�=D�B=Dނ=D��=D�=D�B=D߂=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D�=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�=D�B=D��=D��=D�qD�EqD��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�`BA�`BA�bNA�bNA�`BA�bNA�bNA�bNA�bNA�^5A�dZA�\)A�\)A�^5A�^5A�`BA�bNA�`BA�`BA�\)A�\)A�^5A�`BA�`BA�XA�M�A�K�A�E�A�E�A�E�A�E�A�A�A�;dA��
A�dZA���A�%A���A�VA�+A��A��;A���A�n�A�{A�^5A���A��jA��!A���A�`BA��A���A��uA���A���A�+A��
A���A���A��A�`BA�7LA��jA��A��A���A�jA�  A�1A���A��A�5?A��7A���A�%A��A��yA�1'A�-A�dZA��/A�VA�`BA��A�ZA��jA���A�+A��A���A�^5A�K�A��A�oA�ƨAt�A~VA|��A|=qA{|�Ay�mAw�AvJAuS�AtM�As�As�AsArĜAp��An$�Am7LAl=qAk��AkXAkoAj1'Ah�+Ag
=AeXAe&�Ae�AeVAd�Ad��Ac�Ab��Aa�A_A_+A^�+A\r�AZ�!AY;dAW33AW
=AV��AV�9AUt�AT��AS�#AS&�AQ��AP�9AP1'AP�AO|�AO"�AN�uAN �AMVAKƨAJ�RAJ�AI�AH�DAG�
AF�!AEK�AD��ADZAC�mAC�PAA�AAG�A@�uA>�+A=&�A<�uA;��A:�A9�A8I�A7t�A5G�A4bNA3��A1t�A0��A0�A/��A.ĜA,�!A*��A)�A(  A'
=A&��A&��A&A%\)A$�HA#A"�A!�TA!33A �yA��A�;A�7A
=A�A�mAG�A;dA�A�`A��A��A$�A|�A��An�A�FAE�A�/A��A/A��A5?A�A��A1AG�A(�A�A1A�`A��A
�A�`A �A��AJAVAn�A�AVA9XA��A ��A 5?@��!@�r�@���@�%@�j@�b@��7@�S�@�v�@��@�p�@�z�@�1@�@�Ĝ@��y@�V@�h@蛦@��@�@��@�j@���@�V@�?}@�o@��@���@�p�@ܣ�@۝�@�M�@�&�@�E�@� �@��y@�E�@�@�&�@Ь@�dZ@�-@��@̴9@�b@˾w@�dZ@��@���@�r�@�1'@���@�|�@�\)@�+@�@ƸR@�{@�z�@�t�@°!@�V@�;d@���@�r�@�A�@�(�@���@��H@��+@�-@���@��7@�x�@�G�@��9@��@�V@�hs@��@���@���@�ȴ@��\@���@�z�@���@�n�@�=q@��-@�S�@��@��R@��+@�%@��@�K�@��y@��@�A�@���@�M�@�$�@�J@�@��@��@���@�+@��!@�E�@��@��@���@�dZ@��@�=q@���@�7L@��;@��@��\@�n�@�V@���@�`B@�7L@�/@��`@�Q�@�b@���@��
@�"�@�@�ȴ@���@�^5@�E�@�$�@��#@��7@�z�@�33@�M�@�x�@�?}@�&�@�V@��@���@��9@�I�@� �@�b@��@��
@�C�@���@��\@�V@�E�@�-@��-@���@��`@��u@���@�(�@��F@�\)@�@��!@�v�@�=q@�J@��#@��7@�p�@��@��@��u@���@���@���@��D@�r�@��@���@��P@�K�@��@��\@�ff@�5?@���@�@��-@�G�@���@���@�Ĝ@���@��@�j@�I�@�1@\)@+@~�R@~�+@~ff@}�@|(�@{ƨ@{��@{�@{dZ@{o@z�H@z�\@zM�@y��@y��@yhs@y7L@y7L@y�@x��@xr�@xQ�@x  @w�@w��@wl�@w+@vȴ@v��@v@u@u��@u�@up�@u?}@t��@t�@s�
@s��@sS�@r��@r-@q�^@qG�@p��@p�9@p1'@o�@ol�@oK�@o;d@n��@n�R@nE�@m@m/@l�@lz�@l9X@k��@k�F@k�F@k�F@k�F@k��@k��@k�@kS�@j�@j�\@jn�@j=q@j�@j=q@jJ@i�^@i�7@i�@h1'@g��@g�P@g
=@f�y@f�R@fv�@f5?@e��@e�@e`B@e/@d��@d�/@d�j@dz�@c��@c�F@cS�@cS�@cC�@c"�@b�!@b=q@a��@a%@`�9@`r�@_�P@^�y@^v�@]�@]�@]/@\�@\�/@\�j@\9X@[�m@[ƨ@[��@[dZ@["�@Z��@Z=q@Y�#@Y�^@Y�@X�9@Xr�@X1'@W�;@W\)@V�R@Vff@VV@VE�@U�T@UV@T��@T�@St�@SdZ@St�@St�@SS�@So@R��@R��@R~�@RM�@Q��@Q��@Q��@Qx�@QX@P��@PA�@O�w@N��@M@M��@M�h@M�@M`B@M?}@L��@Lz�@L(�@K�@K@J�H@J�!@J~�@J^5@I�@I��@I�7@Ihs@IG�@I7L@H��@H��@H1'@G�@G�w@G�P@Gl�@G;d@F�+@FV@F5?@E��@E��@E`B@E/@EV@D��@D��@D��@D�@D�@D��@D��@D�D@Dj@C��@C�F@Ct�@Ct�@Ct�@CdZ@B��@Bn�@Bn�@B~�@B�\@B~�@B~�@Bn�@Bn�@Bn�@A��@A��@Ax�@A7L@@��@@Ĝ@@bN@?
=@>��@>�+@>ff@>5?@=��@=V@<�@<�D@<z�@<I�@<(�@;��@:�@:^5@9��@9�#@9�^@9G�@8Q�@8 �@8b@7��@6�y@6ff@65?@65?@65?@6$�@5@5p�@5?}@4��@4�j@4z�@41@3��@3�m@3�F@3��@3�@3dZ@3C�@3@2��@2�\@2n�@2M�@1��@1�^@1�7@0��@0��@0Q�@0  @/�@/�;@/��@/�P@/|�@/l�@/\)@/;d@/+@/
=@.��@.�y@.�y@.v�@.{@-�T@-��@-��@-�-@-��@-�@-?}@,�@,��@,�j@,j@,1@+��@+t�@+33@*��@*n�@*J@)��@)�@(Ĝ@(�@(Q�@(1'@'�;@'+@&��@&@%�-@%��@%?}@$��@$�@$�/@$�/@$�/@$��@$��@$�j@$�@$�D@$j@$(�@$1@#�m@#�@#o@"��@"�\@"-@!x�@ �`@ �@  �@   @�P@
=@�R@E�@{@�T@p�@V@�@��@�j@�@��@I�@1@��@S�@33@��@��@��@�\@~�@~�@^5@J@��@��@X@��@Ĝ@�@1'@�;@|�@;d@��@��@v�@ff@ff@V@$�@�@�@V@�/@�j@�@��@�D@z�@z�@z�@j@j@9X@1@�m@ƨ@��@�@dZ@33@"�@o@@�@��@��@�!@~�@M�@-@��@��@x�@G�@�@��@Ĝ@�9@�u@Q�@b@�;@��@�@�P@l�@;d@�@��@�y@��@5?@{@�T@@�h@O�@/@�@�j@�@��@�D@Z@I�@9X@�@1@1@�m@��@��@�@�@t�@C�@o@@
�@
��@
��@
�\@
~�@
n�@
n�@
^5@
-@
�@
J@
J@	��@	�@	�@	�#@	�^@	�^@	��@	��@	��@	x�@	hs@	%@	%@	%@	%@�`@��@�u@Q�@A�@  @��@�w@�@\)@+@��@�y@��@��@��@�+@$�@{@{@�@�@��@��@@��@�@`B@`B@O�@?}@?}@?}@?}@?}@?}@?}@?}@/@/@/@�@�/@�j@��@��@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�`BA�`BA�bNA�bNA�`BA�bNA�bNA�bNA�bNA�^5A�dZA�\)A�\)A�^5A�^5A�`BA�bNA�`BA�`BA�\)A�\)A�^5A�`BA�`BA�XA�M�A�K�A�E�A�E�A�E�A�E�A�A�A�;dA��
A�dZA���A�%A���A�VA�+A��A��;A���A�n�A�{A�^5A���A��jA��!A���A�`BA��A���A��uA���A���A�+A��
A���A���A��A�`BA�7LA��jA��A��A���A�jA�  A�1A���A��A�5?A��7A���A�%A��A��yA�1'A�-A�dZA��/A�VA�`BA��A�ZA��jA���A�+A��A���A�^5A�K�A��A�oA�ƨAt�A~VA|��A|=qA{|�Ay�mAw�AvJAuS�AtM�As�As�AsArĜAp��An$�Am7LAl=qAk��AkXAkoAj1'Ah�+Ag
=AeXAe&�Ae�AeVAd�Ad��Ac�Ab��Aa�A_A_+A^�+A\r�AZ�!AY;dAW33AW
=AV��AV�9AUt�AT��AS�#AS&�AQ��AP�9AP1'AP�AO|�AO"�AN�uAN �AMVAKƨAJ�RAJ�AI�AH�DAG�
AF�!AEK�AD��ADZAC�mAC�PAA�AAG�A@�uA>�+A=&�A<�uA;��A:�A9�A8I�A7t�A5G�A4bNA3��A1t�A0��A0�A/��A.ĜA,�!A*��A)�A(  A'
=A&��A&��A&A%\)A$�HA#A"�A!�TA!33A �yA��A�;A�7A
=A�A�mAG�A;dA�A�`A��A��A$�A|�A��An�A�FAE�A�/A��A/A��A5?A�A��A1AG�A(�A�A1A�`A��A
�A�`A �A��AJAVAn�A�AVA9XA��A ��A 5?@��!@�r�@���@�%@�j@�b@��7@�S�@�v�@��@�p�@�z�@�1@�@�Ĝ@��y@�V@�h@蛦@��@�@��@�j@���@�V@�?}@�o@��@���@�p�@ܣ�@۝�@�M�@�&�@�E�@� �@��y@�E�@�@�&�@Ь@�dZ@�-@��@̴9@�b@˾w@�dZ@��@���@�r�@�1'@���@�|�@�\)@�+@�@ƸR@�{@�z�@�t�@°!@�V@�;d@���@�r�@�A�@�(�@���@��H@��+@�-@���@��7@�x�@�G�@��9@��@�V@�hs@��@���@���@�ȴ@��\@���@�z�@���@�n�@�=q@��-@�S�@��@��R@��+@�%@��@�K�@��y@��@�A�@���@�M�@�$�@�J@�@��@��@���@�+@��!@�E�@��@��@���@�dZ@��@�=q@���@�7L@��;@��@��\@�n�@�V@���@�`B@�7L@�/@��`@�Q�@�b@���@��
@�"�@�@�ȴ@���@�^5@�E�@�$�@��#@��7@�z�@�33@�M�@�x�@�?}@�&�@�V@��@���@��9@�I�@� �@�b@��@��
@�C�@���@��\@�V@�E�@�-@��-@���@��`@��u@���@�(�@��F@�\)@�@��!@�v�@�=q@�J@��#@��7@�p�@��@��@��u@���@���@���@��D@�r�@��@���@��P@�K�@��@��\@�ff@�5?@���@�@��-@�G�@���@���@�Ĝ@���@��@�j@�I�@�1@\)@+@~�R@~�+@~ff@}�@|(�@{ƨ@{��@{�@{dZ@{o@z�H@z�\@zM�@y��@y��@yhs@y7L@y7L@y�@x��@xr�@xQ�@x  @w�@w��@wl�@w+@vȴ@v��@v@u@u��@u�@up�@u?}@t��@t�@s�
@s��@sS�@r��@r-@q�^@qG�@p��@p�9@p1'@o�@ol�@oK�@o;d@n��@n�R@nE�@m@m/@l�@lz�@l9X@k��@k�F@k�F@k�F@k�F@k��@k��@k�@kS�@j�@j�\@jn�@j=q@j�@j=q@jJ@i�^@i�7@i�@h1'@g��@g�P@g
=@f�y@f�R@fv�@f5?@e��@e�@e`B@e/@d��@d�/@d�j@dz�@c��@c�F@cS�@cS�@cC�@c"�@b�!@b=q@a��@a%@`�9@`r�@_�P@^�y@^v�@]�@]�@]/@\�@\�/@\�j@\9X@[�m@[ƨ@[��@[dZ@["�@Z��@Z=q@Y�#@Y�^@Y�@X�9@Xr�@X1'@W�;@W\)@V�R@Vff@VV@VE�@U�T@UV@T��@T�@St�@SdZ@St�@St�@SS�@So@R��@R��@R~�@RM�@Q��@Q��@Q��@Qx�@QX@P��@PA�@O�w@N��@M@M��@M�h@M�@M`B@M?}@L��@Lz�@L(�@K�@K@J�H@J�!@J~�@J^5@I�@I��@I�7@Ihs@IG�@I7L@H��@H��@H1'@G�@G�w@G�P@Gl�@G;d@F�+@FV@F5?@E��@E��@E`B@E/@EV@D��@D��@D��@D�@D�@D��@D��@D�D@Dj@C��@C�F@Ct�@Ct�@Ct�@CdZ@B��@Bn�@Bn�@B~�@B�\@B~�@B~�@Bn�@Bn�@Bn�@A��@A��@Ax�@A7L@@��@@Ĝ@@bN@?
=@>��@>�+@>ff@>5?@=��@=V@<�@<�D@<z�@<I�@<(�@;��@:�@:^5@9��@9�#@9�^@9G�@8Q�@8 �@8b@7��@6�y@6ff@65?@65?@65?@6$�@5@5p�@5?}@4��@4�j@4z�@41@3��@3�m@3�F@3��@3�@3dZ@3C�@3@2��@2�\@2n�@2M�@1��@1�^@1�7@0��@0��@0Q�@0  @/�@/�;@/��@/�P@/|�@/l�@/\)@/;d@/+@/
=@.��@.�y@.�y@.v�@.{@-�T@-��@-��@-�-@-��@-�@-?}@,�@,��@,�j@,j@,1@+��@+t�@+33@*��@*n�@*J@)��@)�@(Ĝ@(�@(Q�@(1'@'�;@'+@&��@&@%�-@%��@%?}@$��@$�@$�/@$�/@$�/@$��@$��@$�j@$�@$�D@$j@$(�@$1@#�m@#�@#o@"��@"�\@"-@!x�@ �`@ �@  �@   @�P@
=@�R@E�@{@�T@p�@V@�@��@�j@�@��@I�@1@��@S�@33@��@��@��@�\@~�@~�@^5@J@��@��@X@��@Ĝ@�@1'@�;@|�@;d@��@��@v�@ff@ff@V@$�@�@�@V@�/@�j@�@��@�D@z�@z�@z�@j@j@9X@1@�m@ƨ@��@�@dZ@33@"�@o@@�@��@��@�!@~�@M�@-@��@��@x�@G�@�@��@Ĝ@�9@�u@Q�@b@�;@��@�@�P@l�@;d@�@��@�y@��@5?@{@�T@@�h@O�@/@�@�j@�@��@�D@Z@I�@9X@�@1@1@�m@��@��@�@�@t�@C�@o@@
�@
��@
��@
�\@
~�@
n�@
n�@
^5@
-@
�@
J@
J@	��@	�@	�@	�#@	�^@	�^@	��@	��@	��@	x�@	hs@	%@	%@	%@	%@�`@��@�u@Q�@A�@  @��@�w@�@\)@+@��@�y@��@��@��@�+@$�@{@{@�@�@��@��@@��@�@`B@`B@O�@?}@?}@?}@?}@?}@?}@?}@?}@/@/@/@�@�/@�j@��@��@�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�JB�DB�DB�JB�DB�JB�JB�DB�DB�DB�DB�DB�DB�DB�=B�7B�=B�=B�JB�PB�PB�PB�DB�B�7B�7B�Br�B}�B�B�+B�B� B~�Bv�Bz�B{�B�JB�VB�DB�%B~�B~�Bq�BdZB_;BYBO�BR�BN�BI�B=qB5?B�B  B��B�B�/B�RB�B��B��B�Bs�Bn�BaHBR�BYB@�B�B
��BDB
��B
��B
�RB
�LB
�wB
�!B
�-B
�9B
�B
��B
��B
�oB
~�B
iyB
bNB
dZB
aHB
XB
Q�B
;dB
(�B
,B
0!B
$�B
!�B
!�B
 �B
�B	��B	�B
  B	��B	��B
  B	��B	�B	�B	�B	��B	�5B	�;B	�5B	�B	��B	ǮB	�FB	�!B	��B	�B	��B	�%B	m�B	m�B	]/B	w�B	w�B	n�B	cTB	`BB	[#B	XB	J�B	G�B	L�B	T�B	J�B	H�B	C�B	?}B	/B	$�B	(�B	&�B	 �B	$�B	�B	{B	bB	{B	�B	oB	
=B��B��B��B�5B�NB�B�TB�#B�/B�/B��BǮBŢBƨB�9B��BŢB�RB��B��B�bB��B��B��B��B��B��B�bB�\B�B�B� B�B�+B|�B�1B�B}�B�Bu�Bu�B�B�B}�B|�By�Bq�Bk�BjBe`B^5BP�BG�BP�BXBZBS�BN�BJ�BG�BE�B>wB6FB:^B33B0!B2-B&�B49B+B/B-B1'B33B+B'�B-B)�B&�B �B�B�B!�B'�B'�B�B�B#�B%�B#�B �B!�B�BhB�B�B�B�BhB�B�B�BbB{BoB1B�B�B�B�BuBJBPB  B	7B\B�B�B�B�B{BuB�B�B�B�B�B�B�B&�B'�B'�B(�B+B)�B(�B%�B �B�B �B �B �B\B�B%�B1'B33B0!B/B33B49B5?B7LB9XB6FB33B-B6FB7LB;dB8RB;dBC�BB�B=qB;dB>wBG�BG�BC�B8RBJ�BM�BK�BC�BC�BXBVBP�BM�BYBhsBn�Bn�Bn�Bk�Be`BcTBw�By�B{�B|�B|�B|�B}�B�=B�JB�\B�\B�JB�hB��B��B��B��B��B�B�B��B��B�'B�3B�3B�'B�dB�qB�}B�wB��B�}B�qB�dB�LB�FB��BȴB��B�B�B�
B�B�B�B�#B�/B�/B�/B�)B�HB�`B�sB�B�yB�mB�mB�B�B��B��B��B��B��B	  B	B	B	+B	JB	uB	�B	�B	�B	 �B	"�B	"�B	&�B	'�B	'�B	'�B	)�B	,B	-B	/B	2-B	5?B	6FB	6FB	8RB	:^B	=qB	B�B	G�B	J�B	J�B	M�B	P�B	O�B	O�B	Q�B	VB	T�B	XB	W
B	S�B	W
B	`BB	dZB	e`B	e`B	e`B	hsB	iyB	k�B	l�B	o�B	p�B	p�B	r�B	r�B	q�B	t�B	v�B	v�B	x�B	y�B	{�B	|�B	|�B	}�B	~�B	�B	�B	�%B	�%B	�%B	�%B	�=B	�PB	�VB	�PB	�PB	�\B	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�'B	�-B	�3B	�3B	�3B	�3B	�3B	�3B	�FB	�XB	�^B	�jB	�}B	�}B	��B	B	B	B	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	��B	��B	�B	�B	�B	�#B	�B	�)B	�5B	�;B	�HB	�NB	�ZB	�`B	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�fB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB

=B

=B
DB
JB
VB
VB
VB
VB
JB
bB
bB
\B
bB
hB
oB
uB
uB
uB
{B
uB
uB
uB
uB
uB
uB
oB
uB
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
"�B
!�B
&�B
'�B
%�B
$�B
'�B
)�B
+B
+B
+B
)�B
)�B
+B
+B
+B
,B
+B
.B
.B
.B
.B
/B
/B
.B
/B
.B
/B
0!B
0!B
/B
0!B
0!B
/B
1'B
2-B
2-B
49B
49B
49B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
49B
49B
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
8RB
7LB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
;dB
;dB
<jB
<jB
;dB
:^B
;dB
=qB
?}B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
C�B
B�B
A�B
A�B
B�B
C�B
B�B
@�B
B�B
C�B
E�B
F�B
E�B
E�B
F�B
F�B
H�B
G�B
G�B
H�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
I�B
K�B
L�B
K�B
M�B
N�B
N�B
N�B
M�B
M�B
L�B
M�B
N�B
M�B
M�B
O�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
S�B
S�B
S�B
R�B
R�B
Q�B
R�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
bNB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
dZB
ffB
ffB
gmB
ffB
hsB
hsB
gmB
ffB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
iyB
jB
jB
k�B
k�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�JB�JB�JB�JB�JB�JB�JB�JB�JB�0B�JB�^B�DB�JB�DB�0B�0B�DB�)B�DB�)B�DB�DB�DB�=B�RB�=B�XB�JB�PB�PB�jB��B�9B�XB��B�-Bu?B~�B�mB�_B��B��B�Bw�B|PB|�B�dB�VB�xB��B�B}Br�Be�B`BZBP�BSBO(BI�B=�B5�B;B�B�EB��BބB�B�B��B�1B�3ButBp;Bc:BT�BY�BB�BB
��B~B
�*B
��B
�PB
��B
��B
�|B
�3B
��B
��B
�yB
�ZB
��B
� B
l�B
d�B
e�B
b�B
Y1B
S&B
=�B
+�B
-]B
0�B
&B
"�B
"NB
!B
SB	��B	�ZB
B
  B	��B
 �B	�xB	��B	�CB	ٴB	ϑB	�5B	�VB	�5B	�QB	҉B	��B	�B	��B	�yB	��B	��B	��B	o�B	oOB	_�B	w�B	xB	o5B	d�B	aHB	\CB	Y1B	LJB	I7B	MjB	UMB	KxB	I7B	DgB	@OB	0�B	&�B	*0B	'�B	!�B	%�B	�B	B	B	MB	
B	&B	B��B��B�B��B��B�B��B�BބB�jBևB�=B��B��B�zB�'B�B��B�B�dB�&B�KB��B��B��B�!B�yB�hB�HB��B�MB�;B��B��B~(B�fB��B~�B�oBv�Bv�B�-B�UB~BB}<BzDBraBlqBkQBfLB_�BR�BI�BR:BX�BZ�BT�BO�BK�BH�BF�B@B8B;�B4�B1�B3�B(�B5tB,�B0;B.cB1�B4B,qB)*B-�B*�B($B"BB �B"�B(XB(�B7B�B$ZB&LB$@B!bB"hB�B�B�BVBdB7B�BBBB�B�B[B	�BB�B�BB,B6BVB�B
rBBBBBB�B,BKBB;B 'B;B�BxB'B(>B($B)DB+6B*0B)DB&LB!bB�B!bB!bB!|BhB�B&�B1AB3MB0�B/�B3�B4�B5tB7�B9�B6�B3�B.IB6�B7�B;�B9$B<BC�BB�B>BB<jB?cBHBG�BDMB9�BKBNBL0BD�BD�BX_BV�BQ�BO(BY�Bh�Bn�Bn�Bn�Bk�BfBdtBw�Bz*B|PB}VB}VB}�B~�B��B��B��B��B�6B� B��B��B�B�:B�B�B�B�KB�_B�[B�MB�MB��B�B��B��B��B��B��B��B��B�8B�LB�B�7B� B�B�B�$B�EB�KB�_B�=B�IB�dB�IBܬB�B�B�B�B�B�B��B�B��B��B�B�$B�B�<B	 OB	MB	SB	_B	dB	�B	�B	�B	�B	 �B	"�B	"�B	&�B	'�B	(
B	($B	*KB	,"B	-]B	/iB	2aB	5tB	6zB	6`B	8lB	:xB	=�B	B�B	G�B	J�B	J�B	M�B	Q B	O�B	P.B	R:B	VB	U2B	XB	W?B	TFB	WsB	`\B	dtB	e`B	ezB	e�B	h�B	i�B	k�B	l�B	o�B	p�B	p�B	r�B	r�B	q�B	t�B	v�B	v�B	x�B	y�B	|B	|�B	}"B	~B	B	�B	�9B	�?B	�?B	�?B	�tB	�XB	�jB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�0B	�!B	�B	�B	�B	�MB	�MB	�3B	�MB	�hB	�`B	�rB	�^B	�jB	�}B	��B	��B	ªB	��B	��B	ƨB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	� B	�B	�B	�B	��B	�$B	�B	�B	�B	�SB	�EB	�7B	�=B	�eB	�]B	�OB	�pB	�bB	�hB	�tB	�zB	�tB	�B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�*B	�B	�DB	�BB
�B
B
B
-B
B
B
AB
-B
GB
SB
1B
1B
1B
1B
KB
	RB

=B
DB
DB
^B

=B

rB
xB
dB
pB
pB
pB
pB
~B
}B
}B
vB
bB
�B
oB
�B
uB
[B
{B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
#B
!�B
&�B
(
B
&B
%,B
($B
*B
+B
*�B
+B
*0B
*B
+B
+B
+B
,"B
+6B
-�B
./B
./B
./B
/B
/B
./B
/5B
./B
/5B
0;B
0;B
/B
0!B
0!B
/5B
1'B
2-B
2GB
49B
4B
49B
33B
49B
5?B
5%B
5ZB
5?B
5?B
5?B
5?B
5ZB
4nB
49B
6`B
7LB
72B
7fB
7LB
7fB
6`B
6`B
8lB
7LB
6zB
6`B
7LB
8RB
8lB
8�B
8�B
9�B
9�B
9rB
;dB
;B
<�B
<�B
;B
:�B
;�B
=�B
?}B
@�B
@�B
A�B
B�B
C�B
C{B
C�B
C�B
C{B
C�B
C{B
B�B
B�B
B�B
C�B
B�B
A�B
A�B
B�B
C�B
B�B
@�B
B�B
C�B
E�B
F�B
E�B
E�B
F�B
F�B
H�B
G�B
G�B
H�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
I�B
K�B
L�B
K�B
M�B
N�B
N�B
N�B
M�B
M�B
L�B
M�B
N�B
M�B
NB
O�B
N�B
O�B
O�B
O�B
Q B
Q�B
RB
SB
S�B
S�B
S�B
SB
SB
R B
S&B
UB
U�B
VB
VB
VB
W
B
W
B
W
B
W
B
VB
VB
VB
VB
W
B
W$B
W$B
W$B
W
B
XB
XB
W�B
XB
XB
XB
X+B
X+B
X+B
X+B
X+B
X+B
X+B
Y1B
Y1B
Z7B
YB
ZB
Z7B
Z7B
Z7B
[#B
\)B
\CB
\CB
\)B
\)B
\)B
\CB
]IB
\CB
\]B
^OB
^OB
^OB
^5B
^5B
_VB
_VB
_VB
`BB
`'B
`BB
`BB
a-B
aHB
aHB
aHB
a-B
aHB
aHB
bNB
bNB
b4B
bNB
abB
abB
bNB
b4B
bhB
bNB
cTB
cTB
c:B
cTB
cTB
cTB
cTB
dZB
d@B
cTB
dZB
dZB
dZB
dZB
d@B
d@B
dZB
dZB
cnB
cnB
b�B
eFB
eFB
eFB
dZB
dtB
dZB
dtB
e`B
e`B
ezB
ffB
ffB
dtB
f�B
f�B
gmB
ffB
hsB
hXB
gmB
f�B
iyB
iyB
i�B
iyB
iyB
i_B
i_B
i�B
iyB
i�B
jB
jB
jeB
jB
jB
jB
jB
jB
jB
jB
jB
jeB
jeB
jeB
iyB
j�B
jB
k�B
k�B
lq111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803250034132018032500341320180325003413201806221239192018062212391920180622123919201804050436292018040504362920180405043629  JA  ARFMdecpA19c                                                                20180321093520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180321003521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180321003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180321003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180321003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180321003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180321003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180321003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180321003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180321003525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180321005520                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180321153701  CV  JULD            G�O�G�O�F§�                JM  ARCAJMQC2.0                                                                 20180324153413  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180324153413  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193629  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033919  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                