CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:46:18Z creation;2022-06-04T17:46:18Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604174618  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @���.E1   @��.z�@-ffffff�d$r� Ĝ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�ff@���AffA@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B ��B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C3�fC6  C833C:�C;�fC=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƃ3D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @6ff@���@�  @�34A33A@��A`��A�ffA�33A�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B!  B(33B033B833B@33BH33BP33BX33B_��Bh33Bp33Bx33B��B��B��B��4B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B�L�B��gB��gB��B��B��B��B��C �C�C�C�C�C
&gC�C�C�C�C�C�C�C�C�C�C �C"�C$�C%�3C(�C*�C,�C.�C0�C2�C3�3C6�C8@ C:&gC;�3C=�3C?�3CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch&gCj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�3C�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[��D\	�D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƄ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�D�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�J�A�J�A�IA�C�A�K)A�V9A�I�A�<�A�I�A�MA�?�A�)*A�.A��A��A���A�˒A��zAԾ�AԽAԺ�AԳ�Aԩ�Aԗ�AԄMA�ncA�<�A��A�y�A���AБ4A�E9A���A�pA̎�A�(A�W�AʣA�� A�͟A���A�5�A��A�h�A���A�O�A���Aª0A��(A�T,A�ԕA�S�A�#A�>�A���A���A�A�?�A�}�A�ٴA���A��A���A���A���A���A���A�)�A�bA���A�˒A�gA�EmA�'�A���A�m)A�DA�CaA�8�A�]�A�B'A�\�A�FA���A��@A�.A��{A�yrA�>wA�_A���A��A�u�A{z�As�dAn
=Ag�oAb��A\��AYZAW�]AV�AT�wAR0�AN�AM��AKX�AF�jAB0UA@�]A>��A>L�A>MA=�|A<5?A9��A8~A7��A8	�A7o A5L�A3sA0��A/�^A/YA.�NA.��A.y�A.��A-?}A,GA+��A+�A*!�A(�5A(3�A'j�A&��A$��A#(�A"�9A!��A �A�A>BA5�A��A!�Ah�A�KA�A�An/A1�A�`Ae,A�'A�wAv`A~(A!�A�AXA��A7�ATaAT�A�uA�cAn�AK�A��A}VAaA�A�A��A�hAj�A��A��AW�A!-A�A��A��AHA
��A	�2A	��A	o A�oA�AW?Ar�A/Ap;AGA�$A��A}�A0�A�AT�A��AĜA�$Ae,A�A�A�Ac�A �8A [�A 8�@��0@�x@��8@�P�@���@���@�g�@��@���@�@�{J@�w2@���@�/�@��j@�"�@��@��O@���@��D@�5?@��S@�o@���@�Ta@��T@���@��\@�?�@���@��'@�o�@�bN@�J�@��@��@�!@�&�@�b@�(�@�"�@�
=@���@�_@�'R@��}@�$@���@�]�@�2�@�A�@�0U@��@甯@��@�M@�^�@�=@��@��@�Q�@�0U@�@��@�!@��@�_�@��@�Y�@�^5@��r@ߊ	@޸R@��?@�w�@��A@ݝ�@���@�_@۠'@�A�@�V@گO@�oi@�J�@ٳ�@��"@�s�@�ϫ@���@�� @׍P@�&�@���@���@֎�@�C-@�  @�hs@��,@�
�@�ѷ@ҁo@�/�@���@��a@ь~@�<6@П�@��#@�(@Γu@���@��j@̈́M@� \@̫6@�$@˳�@���@�~@ɋ�@�!-@��|@ȶ�@ȃ�@�"h@ǲ-@�L�@ƌ�@�~@��@��&@�IR@���@�z�@�4@Ò:@��@�u�@��@��@@�^�@�F�@�a�@�hs@��|@��_@�[�@�;�@��@���@��f@�H@��o@��-@�|@��@�tT@�A�@� �@���@���@�a@��]@�RT@�V@���@���@���@��@��@�c@��@�l�@��@�a@��@���@�xl@�C-@���@�^�@�@��@�J@���@�X�@�
=@���@�u%@��]@���@�e,@��@��?@�y>@� �@��j@��"@�4�@��p@��F@�Z@�9X@���@�?}@�%F@��@���@��g@�o @�A @��@�@��M@���@���@��@�@�Z�@�S@���@�{�@�4n@��)@�Mj@�!-@��P@�V�@��S@�<6@��@���@���@�@�@�ԕ@���@���@�s�@���@�v�@�	�@�=�@��@���@�A�@�˒@���@�hs@�K�@��m@���@�GE@���@���@�O�@��@��@���@��e@�~�@�:�@���@��k@�IR@��@���@�Ov@��g@���@��S@�Dg@�(@��@��	@���@�M@��@�x@��@�� @���@�5�@���@�)�@��@��)@���@�@@���@���@�@�@�S&@���@���@�O@�u@���@���@�|@��@���@�q@�V�@�*�@���@�ƨ@��X@�`B@�8�@��@���@��Y@�7�@�+k@�	@��@�خ@���@��n@��M@�K�@��@��P@���@���@�Ov@�$@���@���@��:@�?}@� i@��@��j@���@�r�@�]d@�?�@�1@���@��P@�{J@�iD@�Y�@�O�@�A�@�33@�,�@�Y@��@���@���@�YK@��@�ݘ@��k@�a�@�Dg@�*0@��@��@��E@�&�@��:@�[W@�J#@�>�@� \@�ѷ@�u�@�	�@��)@�@���@�P�@���@�y>@�(�@�_@�@��@�@~�\@~a|@~#:@~�@}�@}2a@}�@|�/@|e�@|*�@{�W@{�g@{��@{,�@z��@z8�@y�o@y�@x9X@xb@w�W@w��@w6z@wo@v�@vd�@v+k@u��@t�p@t~(@t�@s��@s_p@sS@r�'@rOv@ru@q�^@q(�@p�@p-�@o�Q@o��@o!-@n҉@n�A@m�z@mN<@l��@loi@k�;@kj�@k.I@j�]@j^5@j!�@i�@i��@i%F@h��@hy>@g�}@f�y@fq�@f($@e��@d�P@d��@d`�@cݘ@c�k@c)_@b�R@bE�@a�@arG@aN<@a�@`�o@_�W@_qv@_�@^s�@]�@]A @\��@\c�@[��@[��@[(@Z��@Y��@Y[W@Y8�@Y2a@X��@X,=@X�@W�@W��@W/�@V��@VQ@V{@U�@U�t@U^�@UJ�@T�@Tw�@TM@S��@S(@S�@S i@Rz@Q�3@QO�@P�E@PI�@O��@OP�@O�@N�<@NTa@M�D@M��@Mhs@MV@L�O@L��@L`�@L@K�@KP�@J��@Ja|@J!�@I�3@I�=@IVm@I�@H��@HI�@H7�@H4n@G˒@G>�@F�c@FGE@E��@E-w@E�@D�@D�@C�
@C�$@C)_@B҉@Bi�@B:*@A��@A�h@AB�@@�@@|�@@7�@@'R@@~@@@?�F@?6z@?Y@>��@>�A@=�D@=p�@=;@<��@<��@<@;��@;&@:�m@:l�@:3�@:�@9�z@9��@9��@9�X@9�'@9rG@9!�@8�@8!@7��@7�@6YK@5�>@5a�@5�@4�@4��@3��@3O@2��@2�r@2E�@2+k@1�@1��@1`B@0�K@0oi@0N�@0A�@/��@/��@/��@/)_@.��@.�R@.^5@-�^@-��@-4@,��@,��@,9X@+�Q@+��@+�f@+\)@+Y@+�@*�@*҉@*�A@)�9@)2a@)�@)+@)�@(�v@(�?@(�_@(oi@(K^@(M@'��@'ƨ@'�[@'F�@'�@'&@'$t@&�@&c @%�@%A @$�U@$��@$[�@#��@#J#@#1�@#o@"��@"�<@"��@"R�@"&�@"{@!�j@!u�@!�@ ��@ �@ ی@ ��@ <�@�@@�@��@g�@@?@��@��@�^@�M@+�@��@��@�@�u@j@�&@�P@'�@�8@�@�r@YK@R�@?@0U@_@��@ϫ@@��@��@w2@F@@��@�@]d@N�@>B@�@�]@�@l�@S@�X@��@��@�+@ff@Z�@L0@)�@_@�h@G�@@@��@�z@w�@Ft@1@�@�@  @�+@�*@\)@E9@�@�@�<@�r@_�@6�@�@�@�z@��@�X@rG@A @�@�@U2@*�@�m@��@dZ@>�@��@��@�}@��@GE@�@�@O@�T@}�@f�@4@#�@Ɇ@m�@N�@A�@<�@7�@�@��@�f@_p@K�@9�@�@
�@
�@
��@
�\@
_�@
:*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�J�A�J�A�IA�C�A�K)A�V9A�I�A�<�A�I�A�MA�?�A�)*A�.A��A��A���A�˒A��zAԾ�AԽAԺ�AԳ�Aԩ�Aԗ�AԄMA�ncA�<�A��A�y�A���AБ4A�E9A���A�pA̎�A�(A�W�AʣA�� A�͟A���A�5�A��A�h�A���A�O�A���Aª0A��(A�T,A�ԕA�S�A�#A�>�A���A���A�A�?�A�}�A�ٴA���A��A���A���A���A���A���A�)�A�bA���A�˒A�gA�EmA�'�A���A�m)A�DA�CaA�8�A�]�A�B'A�\�A�FA���A��@A�.A��{A�yrA�>wA�_A���A��A�u�A{z�As�dAn
=Ag�oAb��A\��AYZAW�]AV�AT�wAR0�AN�AM��AKX�AF�jAB0UA@�]A>��A>L�A>MA=�|A<5?A9��A8~A7��A8	�A7o A5L�A3sA0��A/�^A/YA.�NA.��A.y�A.��A-?}A,GA+��A+�A*!�A(�5A(3�A'j�A&��A$��A#(�A"�9A!��A �A�A>BA5�A��A!�Ah�A�KA�A�An/A1�A�`Ae,A�'A�wAv`A~(A!�A�AXA��A7�ATaAT�A�uA�cAn�AK�A��A}VAaA�A�A��A�hAj�A��A��AW�A!-A�A��A��AHA
��A	�2A	��A	o A�oA�AW?Ar�A/Ap;AGA�$A��A}�A0�A�AT�A��AĜA�$Ae,A�A�A�Ac�A �8A [�A 8�@��0@�x@��8@�P�@���@���@�g�@��@���@�@�{J@�w2@���@�/�@��j@�"�@��@��O@���@��D@�5?@��S@�o@���@�Ta@��T@���@��\@�?�@���@��'@�o�@�bN@�J�@��@��@�!@�&�@�b@�(�@�"�@�
=@���@�_@�'R@��}@�$@���@�]�@�2�@�A�@�0U@��@甯@��@�M@�^�@�=@��@��@�Q�@�0U@�@��@�!@��@�_�@��@�Y�@�^5@��r@ߊ	@޸R@��?@�w�@��A@ݝ�@���@�_@۠'@�A�@�V@گO@�oi@�J�@ٳ�@��"@�s�@�ϫ@���@�� @׍P@�&�@���@���@֎�@�C-@�  @�hs@��,@�
�@�ѷ@ҁo@�/�@���@��a@ь~@�<6@П�@��#@�(@Γu@���@��j@̈́M@� \@̫6@�$@˳�@���@�~@ɋ�@�!-@��|@ȶ�@ȃ�@�"h@ǲ-@�L�@ƌ�@�~@��@��&@�IR@���@�z�@�4@Ò:@��@�u�@��@��@@�^�@�F�@�a�@�hs@��|@��_@�[�@�;�@��@���@��f@�H@��o@��-@�|@��@�tT@�A�@� �@���@���@�a@��]@�RT@�V@���@���@���@��@��@�c@��@�l�@��@�a@��@���@�xl@�C-@���@�^�@�@��@�J@���@�X�@�
=@���@�u%@��]@���@�e,@��@��?@�y>@� �@��j@��"@�4�@��p@��F@�Z@�9X@���@�?}@�%F@��@���@��g@�o @�A @��@�@��M@���@���@��@�@�Z�@�S@���@�{�@�4n@��)@�Mj@�!-@��P@�V�@��S@�<6@��@���@���@�@�@�ԕ@���@���@�s�@���@�v�@�	�@�=�@��@���@�A�@�˒@���@�hs@�K�@��m@���@�GE@���@���@�O�@��@��@���@��e@�~�@�:�@���@��k@�IR@��@���@�Ov@��g@���@��S@�Dg@�(@��@��	@���@�M@��@�x@��@�� @���@�5�@���@�)�@��@��)@���@�@@���@���@�@�@�S&@���@���@�O@�u@���@���@�|@��@���@�q@�V�@�*�@���@�ƨ@��X@�`B@�8�@��@���@��Y@�7�@�+k@�	@��@�خ@���@��n@��M@�K�@��@��P@���@���@�Ov@�$@���@���@��:@�?}@� i@��@��j@���@�r�@�]d@�?�@�1@���@��P@�{J@�iD@�Y�@�O�@�A�@�33@�,�@�Y@��@���@���@�YK@��@�ݘ@��k@�a�@�Dg@�*0@��@��@��E@�&�@��:@�[W@�J#@�>�@� \@�ѷ@�u�@�	�@��)@�@���@�P�@���@�y>@�(�@�_@�@��@�@~�\@~a|@~#:@~�@}�@}2a@}�@|�/@|e�@|*�@{�W@{�g@{��@{,�@z��@z8�@y�o@y�@x9X@xb@w�W@w��@w6z@wo@v�@vd�@v+k@u��@t�p@t~(@t�@s��@s_p@sS@r�'@rOv@ru@q�^@q(�@p�@p-�@o�Q@o��@o!-@n҉@n�A@m�z@mN<@l��@loi@k�;@kj�@k.I@j�]@j^5@j!�@i�@i��@i%F@h��@hy>@g�}@f�y@fq�@f($@e��@d�P@d��@d`�@cݘ@c�k@c)_@b�R@bE�@a�@arG@aN<@a�@`�o@_�W@_qv@_�@^s�@]�@]A @\��@\c�@[��@[��@[(@Z��@Y��@Y[W@Y8�@Y2a@X��@X,=@X�@W�@W��@W/�@V��@VQ@V{@U�@U�t@U^�@UJ�@T�@Tw�@TM@S��@S(@S�@S i@Rz@Q�3@QO�@P�E@PI�@O��@OP�@O�@N�<@NTa@M�D@M��@Mhs@MV@L�O@L��@L`�@L@K�@KP�@J��@Ja|@J!�@I�3@I�=@IVm@I�@H��@HI�@H7�@H4n@G˒@G>�@F�c@FGE@E��@E-w@E�@D�@D�@C�
@C�$@C)_@B҉@Bi�@B:*@A��@A�h@AB�@@�@@|�@@7�@@'R@@~@@@?�F@?6z@?Y@>��@>�A@=�D@=p�@=;@<��@<��@<@;��@;&@:�m@:l�@:3�@:�@9�z@9��@9��@9�X@9�'@9rG@9!�@8�@8!@7��@7�@6YK@5�>@5a�@5�@4�@4��@3��@3O@2��@2�r@2E�@2+k@1�@1��@1`B@0�K@0oi@0N�@0A�@/��@/��@/��@/)_@.��@.�R@.^5@-�^@-��@-4@,��@,��@,9X@+�Q@+��@+�f@+\)@+Y@+�@*�@*҉@*�A@)�9@)2a@)�@)+@)�@(�v@(�?@(�_@(oi@(K^@(M@'��@'ƨ@'�[@'F�@'�@'&@'$t@&�@&c @%�@%A @$�U@$��@$[�@#��@#J#@#1�@#o@"��@"�<@"��@"R�@"&�@"{@!�j@!u�@!�@ ��@ �@ ی@ ��@ <�@�@@�@��@g�@@?@��@��@�^@�M@+�@��@��@�@�u@j@�&@�P@'�@�8@�@�r@YK@R�@?@0U@_@��@ϫ@@��@��@w2@F@@��@�@]d@N�@>B@�@�]@�@l�@S@�X@��@��@�+@ff@Z�@L0@)�@_@�h@G�@@@��@�z@w�@Ft@1@�@�@  @�+@�*@\)@E9@�@�@�<@�r@_�@6�@�@�@�z@��@�X@rG@A @�@�@U2@*�@�m@��@dZ@>�@��@��@�}@��@GE@�@�@O@�T@}�@f�@4@#�@Ɇ@m�@N�@A�@<�@7�@�@��@�f@_p@K�@9�@�@
�@
�@
��@
�\@
_�@
:*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�uBªBªBªB�uB�uB�[B�'B�[BB�AB�uB��B�AB��B�uBB�[B�'B��B��B� B��B�B�.B�B�JB��B��B��B��B�B�B	<B	�B	�tB	�aB	�B
+B
YB
u?B
�gB
�hB
��B
�fB�B�B3�B<�B\Bh$B��B� B�QB�fB�B��B��B�B��B��B�B� B��B��B�pB��B�MBlWBiDBV�BQ�BQ�BN�BB�B7�B)*BNB
�B
�HB
�^B
ȀB
�BB
��B
��B
� B
}�B
l�B
aHB
ZB
^�B
8�B
�B	�B	�B	��B	tnB	U�B	7LB	-�B	.�B	*�B	#�B	!B	!�B	5B	~B	�B	WB	0B	2�B	0�B	<PB	@�B	Q�B	IlB	J�B	O�B	Z�B	s3B	sB	l�B	bNB	^jB	c�B	g8B	i�B	v�B	��B	�B	��B	��B	��B	��B	�gB	�+B	�	B	��B	�qB	�QB	��B	�B	��B	��B	�B	�}B	��B	��B	�B	�B	�"B	��B	�B	�hB	�B	��B	��B	�fB	�UB	�B	�/B	�dB	��B	�8B	��B	��B	��B	�B	�2B	��B	�LB	��B	��B	��B	��B	�gB	��B	��B	�!B	�+B	�B	޸B	�VB	�-B	��B	�B	�B	�B	��B	�BB	�BB	��B	��B	��B	�B	�SB	�B	�eB	�yB	�FB	�`B	��B	�B	�tB	��B	��B	�kB	��B	�}B	��B	�hB	�-B	�B	�]B	�iB	�B	�vB	��B	��B	��B
'B
uB
'B
oB
 iB
  B
�B
9B
B
�B
'B
[B
B
�B
�B
�B
�B
B
B
�B
tB
mB
mB
�B
�B
MB
�B
'B	�^B	��B	��B	��B	�nB	��B	�xB	��B	��B	�B	�dB	��B	�0B	�>B	��B	��B	��B	�TB	�B	�TB	��B	��B	��B	��B	��B	��B	��B	��B	�nB	��B	��B	��B	�B	��B	��B	�GB	�B	�|B	�GB	�B	��B	��B	��B	�zB	�B	�B	��B	��B	��B	�?B	��B	��B	��B	��B	�B	��B	�ZB	�LB	��B	��B	�RB	��B	��B	��B	�B	��B	��B	�B	�%B	��B	�`B	�FB	�B	��B	��B	��B	�B	�'B	��B	��B	� B	�!B	�'B	�vB	�B	�B	�=B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�yB	�*B	��B	��B	�B	�B	��B	��B	�B	�LB	�B	�2B	�LB	�B	�B	��B	��B	�B	�)B	�CB	�/B	�B	�WB	�/B	��B	�'B	��B	�B	�B	�[B	�B	�AB	�!B	�B	�WB	�B	��B	�B	��B	��B	�vB	�B	�'B	��B	�oB	��B	�B	�B	�;B	��B	�B	��B	�GB	��B	�B	��B	�B	�nB	�nB	�B	�%B	�?B	��B	�`B	��B	�B	��B	��B	��B	�$B	��B	�*B	�^B	��B	��B	��B	��B	�B	��B	�qB	�]B	��B	��B	��B	��B	��B	�.B	��B
 OB
 �B
 B
UB
�B
�B
�B
�B
uB
uB
-B
3B
�B
�B
B
tB
�B
B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
	RB
	�B

	B
	�B

�B

�B

�B

�B

�B
�B
xB
�B
�B
dB
�B
jB
6B
�B
PB
B
�B
B
<B
�B
VB
�B
(B
.B
NB
�B
�B
:B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
MB
aB
B
B
+B
EB
�B
YB
�B
�B
�B
�B
�B
?B
�B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
B
IB
dB
dB
B
5B
jB
�B
B
VB
 'B
!B
 �B
 �B
!HB
!|B
!�B
!|B
!�B
#:B
#nB
#�B
$B
$B
$&B
$ZB
$tB
$tB
$�B
%�B
'B
'�B
(
B
(sB
(�B
)*B
)yB
)_B
)�B
)�B
)_B
)DB
*B
+B
+B
*�B
*�B
*�B
+B
+6B
,B
+�B
,WB
,qB
,�B
-�B
.�B
.�B
.�B
.�B
/ B
/OB
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1B
1[B
1vB
1[B
1AB
1'B
1�B
2GB
2�B
2�B
3MB
3MB
3hB
3�B
3�B
4B
3�B
3�B
4TB
4B
3�B
4B
49B
5�B
5tB
5�B
5�B
5�B
72B
7fB
7�B
88B
8RB
88B
8B
8B
7�B
7�B
7�B
7�B
8B
8RB
8lB
8�B
9XB
9�B
9�B
:^B
:�B
:�B
:�B
;JB
;�B
;�B
<jB
=<B
=qB
=B
=VB
=�B
>BB
>BB
>wB
>�B
>�B
?B
?cB
?�B
@ B
@B
@4B
@�B
@�B
A B
A;B
A�B
A�B
A�B
A�B
BAB
B[B
B[B
B�B
B�B
CaB
C�B
C�B
C�B
D�B
D�B
EB
EB
EmB
E�B
EmB
E�B
E�B
E�B
F?B
F�B
F�B
GEB
GEB
G_B
HB
H1B
H1B
HB
HfB
H�B
IB
IRB
I�B
J#B
JrB
J�B
J�B
K^B
LB
LdB
L�B
M6B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O(B
O�B
OvB
O�B
O�B
P.B
PbB
PbB
P.B
P�B
P�B
Q B
QNB
RB
R:B
R:B
R B
RTB
R�B
R�B
S&B
S[B
S�B
S�B
TB
T,B
T{B
U2B
UMB
UMB
UMB
U2B
UB
U�B
VB
U�B
VB
VSB
V�B
W$B
WYB
W?B
W�B
W�B
X+B
XyB
X�B
X�B
X�B
YKB
YeB
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
ZkB
[=B
[�B
[�B
\]B
\�B
\�B
\�B
]IB
]~B
^B
^5B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`B
_�B
`�B
`�B
`�B
aB
a-B
aB
aHB
a�B
a�B
a�B
bB
bB
b�B
b�B
cB
c�B
c�B
d&B
d&B
d@B
d@B
dZB
dZB
dZB
dtB
dtB
dtB
d�B
d�B
d�B
eB
e,B
e`B
e�B
e�B
ezB
e�B
fB
fB
e�B
f2B
f�B
f�B
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
iB
i*B
i_B
iyB
i�B
j0B
j0B
jKB
jKB
jKB
jKB
j�B
k�B
k�B
lB
lWB
l�B
mCB
mwB
m�B
mwB
m�B
nB
n}B
n}B
n}B
ncB
n}B
o B
oiB
p!B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
qB
q'B
qAB
q'B
q'B
qB
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
r�B
r�B
sB
sB
sB
s3B
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
u%B
u%B
uB
uB
uB
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
wB
wB
v�B
wLB
w�B
w�B
xRB
xlB
x�B
x�B
y	B
y>B
yXB
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
|B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
~B
~]B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�uBªBªBªB�uB�uB�[B�'B�[BB�AB�uB��B�AB��B�uBB�[B�'B��B��B� B��B�B�.B�B�JB��B��B��B��B�B�B	<B	�B	�tB	�aB	�B
+B
YB
u?B
�gB
�hB
��B
�fB�B�B3�B<�B\Bh$B��B� B�QB�fB�B��B��B�B��B��B�B� B��B��B�pB��B�MBlWBiDBV�BQ�BQ�BN�BB�B7�B)*BNB
�B
�HB
�^B
ȀB
�BB
��B
��B
� B
}�B
l�B
aHB
ZB
^�B
8�B
�B	�B	�B	��B	tnB	U�B	7LB	-�B	.�B	*�B	#�B	!B	!�B	5B	~B	�B	WB	0B	2�B	0�B	<PB	@�B	Q�B	IlB	J�B	O�B	Z�B	s3B	sB	l�B	bNB	^jB	c�B	g8B	i�B	v�B	��B	�B	��B	��B	��B	��B	�gB	�+B	�	B	��B	�qB	�QB	��B	�B	��B	��B	�B	�}B	��B	��B	�B	�B	�"B	��B	�B	�hB	�B	��B	��B	�fB	�UB	�B	�/B	�dB	��B	�8B	��B	��B	��B	�B	�2B	��B	�LB	��B	��B	��B	��B	�gB	��B	��B	�!B	�+B	�B	޸B	�VB	�-B	��B	�B	�B	�B	��B	�BB	�BB	��B	��B	��B	�B	�SB	�B	�eB	�yB	�FB	�`B	��B	�B	�tB	��B	��B	�kB	��B	�}B	��B	�hB	�-B	�B	�]B	�iB	�B	�vB	��B	��B	��B
'B
uB
'B
oB
 iB
  B
�B
9B
B
�B
'B
[B
B
�B
�B
�B
�B
B
B
�B
tB
mB
mB
�B
�B
MB
�B
'B	�^B	��B	��B	��B	�nB	��B	�xB	��B	��B	�B	�dB	��B	�0B	�>B	��B	��B	��B	�TB	�B	�TB	��B	��B	��B	��B	��B	��B	��B	��B	�nB	��B	��B	��B	�B	��B	��B	�GB	�B	�|B	�GB	�B	��B	��B	��B	�zB	�B	�B	��B	��B	��B	�?B	��B	��B	��B	��B	�B	��B	�ZB	�LB	��B	��B	�RB	��B	��B	��B	�B	��B	��B	�B	�%B	��B	�`B	�FB	�B	��B	��B	��B	�B	�'B	��B	��B	� B	�!B	�'B	�vB	�B	�B	�=B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�yB	�*B	��B	��B	�B	�B	��B	��B	�B	�LB	�B	�2B	�LB	�B	�B	��B	��B	�B	�)B	�CB	�/B	�B	�WB	�/B	��B	�'B	��B	�B	�B	�[B	�B	�AB	�!B	�B	�WB	�B	��B	�B	��B	��B	�vB	�B	�'B	��B	�oB	��B	�B	�B	�;B	��B	�B	��B	�GB	��B	�B	��B	�B	�nB	�nB	�B	�%B	�?B	��B	�`B	��B	�B	��B	��B	��B	�$B	��B	�*B	�^B	��B	��B	��B	��B	�B	��B	�qB	�]B	��B	��B	��B	��B	��B	�.B	��B
 OB
 �B
 B
UB
�B
�B
�B
�B
uB
uB
-B
3B
�B
�B
B
tB
�B
B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
	RB
	�B

	B
	�B

�B

�B

�B

�B

�B
�B
xB
�B
�B
dB
�B
jB
6B
�B
PB
B
�B
B
<B
�B
VB
�B
(B
.B
NB
�B
�B
:B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
MB
aB
B
B
+B
EB
�B
YB
�B
�B
�B
�B
�B
?B
�B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
B
IB
dB
dB
B
5B
jB
�B
B
VB
 'B
!B
 �B
 �B
!HB
!|B
!�B
!|B
!�B
#:B
#nB
#�B
$B
$B
$&B
$ZB
$tB
$tB
$�B
%�B
'B
'�B
(
B
(sB
(�B
)*B
)yB
)_B
)�B
)�B
)_B
)DB
*B
+B
+B
*�B
*�B
*�B
+B
+6B
,B
+�B
,WB
,qB
,�B
-�B
.�B
.�B
.�B
.�B
/ B
/OB
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1B
1[B
1vB
1[B
1AB
1'B
1�B
2GB
2�B
2�B
3MB
3MB
3hB
3�B
3�B
4B
3�B
3�B
4TB
4B
3�B
4B
49B
5�B
5tB
5�B
5�B
5�B
72B
7fB
7�B
88B
8RB
88B
8B
8B
7�B
7�B
7�B
7�B
8B
8RB
8lB
8�B
9XB
9�B
9�B
:^B
:�B
:�B
:�B
;JB
;�B
;�B
<jB
=<B
=qB
=B
=VB
=�B
>BB
>BB
>wB
>�B
>�B
?B
?cB
?�B
@ B
@B
@4B
@�B
@�B
A B
A;B
A�B
A�B
A�B
A�B
BAB
B[B
B[B
B�B
B�B
CaB
C�B
C�B
C�B
D�B
D�B
EB
EB
EmB
E�B
EmB
E�B
E�B
E�B
F?B
F�B
F�B
GEB
GEB
G_B
HB
H1B
H1B
HB
HfB
H�B
IB
IRB
I�B
J#B
JrB
J�B
J�B
K^B
LB
LdB
L�B
M6B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O(B
O�B
OvB
O�B
O�B
P.B
PbB
PbB
P.B
P�B
P�B
Q B
QNB
RB
R:B
R:B
R B
RTB
R�B
R�B
S&B
S[B
S�B
S�B
TB
T,B
T{B
U2B
UMB
UMB
UMB
U2B
UB
U�B
VB
U�B
VB
VSB
V�B
W$B
WYB
W?B
W�B
W�B
X+B
XyB
X�B
X�B
X�B
YKB
YeB
YB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
ZkB
[=B
[�B
[�B
\]B
\�B
\�B
\�B
]IB
]~B
^B
^5B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`B
_�B
`�B
`�B
`�B
aB
a-B
aB
aHB
a�B
a�B
a�B
bB
bB
b�B
b�B
cB
c�B
c�B
d&B
d&B
d@B
d@B
dZB
dZB
dZB
dtB
dtB
dtB
d�B
d�B
d�B
eB
e,B
e`B
e�B
e�B
ezB
e�B
fB
fB
e�B
f2B
f�B
f�B
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
iB
i*B
i_B
iyB
i�B
j0B
j0B
jKB
jKB
jKB
jKB
j�B
k�B
k�B
lB
lWB
l�B
mCB
mwB
m�B
mwB
m�B
nB
n}B
n}B
n}B
ncB
n}B
o B
oiB
p!B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
qB
qB
qB
q'B
qAB
q'B
q'B
qB
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
rGB
raB
r�B
r�B
r�B
r�B
sB
sB
sB
s3B
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
u%B
u%B
uB
uB
uB
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
wB
wB
v�B
wLB
w�B
w�B
xRB
xlB
x�B
x�B
y	B
y>B
yXB
y�B
y�B
zB
z�B
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
|B
|�B
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}qB
}�B
}�B
}�B
~B
~]B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104939  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174618  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174618  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174618                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024625  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024625  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                