CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:38Z creation;2022-06-04T17:45:38Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174538  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ض13�1   @ض���u@.��vȴ�dDbM��1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@y��@�  A   A   A@  A`  A~ffA�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B���B���B���B���B�  B�  B�  B�ffB�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�3D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@)��@|��@���A ��A ��A@��A`��A33A�ffA���A�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(��B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B�� B��4B��gB��4B��gB��gB��gB��B��B��B̀ B��BԀ B��gB��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C&gC�C�3C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn&gCo�3Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"|�D#3D#��D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw	�Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dل�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�@A�8�A�:^A�)�A�*eA�.IA�5A�.�A�Aм6AЛqAЎ�AІ%A�{JA�v�A�q�A�rA�rA�p;A�poA�o�A�n/A�h�A�g�A�g�A�f�A�e�A�`�A�TaA�QNA�OA�K^A�7A��A�cA��2A�ںA͹$A̸A��A˕�A�`�Aǚ7AƦ�A�TaA���A��A��UA��A�C-A��-A���A���A��A�[�A��A�U�A�]�A��fA���A���A�HA�{A���A���A��A��A��A�ɺA�7�A�8RA���A�G�A�3hA�i�A���A���A��QA�tA��A�qA�ʌA���A��2A���A�+A��(A�x�A��+A��A7�A{4nAy�FAx��Ax[WAxqAw�+Aq)_Am�@Ama�AlFtAj6Ag�Ac]dA`��A]bNA[qvAY�ATZAP��AM9�AJ��AIu�AFQ�AC�hAAqvA>��A=��A;sA8��A7�A5d�A4+A2�+A2��A2�A1��A0�ZA/�{A-H�A+�A+QA*��A)�	A(CA'A&�}A&J�A%��A%��A$��A#��A"��A!/�A ��A �A͟A�A�{A��A�*A�!A�$A�"A�7AB�A�WA��A��A�Av`A%�A��AjA4A��AS�AqA�A�A�A�MA��A}�A!�A��A��AVA��A��Al�A��A�$A�?AĜA�A8�Ay>A4nAn�AP�A��A�rAn/A��A�RA�YA^5A��A
�9A	�MA��A��A3�A�A�;A�A��A5�A��A�XAiDAW�AP�AIRA&�AAx�A�A�tAZ�A4nA�A�A�A{JA �fA Y�@���@��K@���@��"@���@�($@��a@�q@�3�@��@@��8@��@�@��@�9�@�J�@�o@��	@�"�@�p;@��@�@��)@��@���@쟾@�*�@�y�@ꗍ@�S�@���@� i@��@�zx@�7L@���@�Ĝ@�l"@�4@�@�@��W@�Mj@��@�e@��@��@�_p@ව@�#:@ߩ*@�/@�ی@޸R@��@�g�@�W?@��@��T@ۆ�@�#�@�~�@�A�@��)@٢�@�Z�@�A @�:�@�/�@��@؃@�F@�kQ@�H�@���@Ք�@Լj@�3�@�!-@��@�L0@��9@п�@�,=@��@���@���@��@�&�@�l�@��@��>@ͯ�@ͅ@��@��@̯O@̌�@�C�@�rG@�ff@���@Ʌ�@�<6@��@���@�Q�@���@Ƿ�@� \@�~�@��@Œ:@�\�@���@�u%@��K@�dZ@���@�@��^@�o@�u�@�{@���@�S&@�%@��E@�?�@���@���@�J�@��A@���@�_p@���@�@���@�"�@� i@��s@���@�Ta@�ݘ@��4@�2a@��@���@�q�@���@��@�ѷ@���@�"h@�o@�B[@�&�@��@��@���@�v`@� \@���@��@���@��v@��1@�0U@��	@�\�@��@�U2@��a@�O�@��`@��m@��R@��h@���@���@�5?@��r@���@��o@�!@��@���@�˒@��@@�t�@��@�{�@��T@���@��f@���@�}�@�2a@��@��D@�&�@���@���@�U�@���@��@�R�@��@���@�;d@�
=@��z@�E�@��r@���@��P@�|�@�W?@�@O@�33@��@���@�2�@��Z@���@�j�@�U�@�B�@�.I@��@��B@���@�?@��A@�J#@���@���@�xl@��@�u�@�IR@��@��@�@�@�b@�c@�K�@�!-@�ѷ@���@��\@�0U@��a@��@��f@��x@�J�@���@���@�zx@�+@���@�c�@��j@�l�@�0�@��5@�;�@���@�o @�4�@��e@�:�@��]@��S@�S�@�IR@�1�@��@��m@��\@�D�@��@��j@�t�@�8@��]@���@��@���@�j@��@���@��r@���@���@��n@�J�@��@��9@�� @�~�@�-@���@��W@���@���@��:@�W?@�
=@��@��@���@�c�@�E�@�"h@�	�@���@��t@��@���@�RT@�Dg@�7L@��@��P@��/@��B@���@���@��e@���@��D@�3�@���@��@�[W@��@��M@��@�ی@��@�~(@�1@��$@�8�@�#�@�!�@��@��c@�ی@��?@�~(@�e�@�PH@�I�@�1�@�@��@�	@l�@9�@~�]@~Ov@}�@}w2@|��@|��@|��@|z�@|M@|1@{�V@{8@zz@zJ@yhs@x��@x��@x~@w��@w�@vv�@v:*@u��@u5�@t�I@sƨ@sj�@s�@r��@r=q@q�@qY�@p�/@p��@p`�@o��@oW?@n�@n�}@n�x@n_�@m@k�@k�	@kt�@kJ#@k i@j͟@j��@jl�@j@iw2@h��@h�4@h]d@h �@h�@g��@g��@gn/@g�@f�x@f)�@ex�@dѷ@dx@c��@cx@c>�@b��@b_�@a�@`��@`M@`/�@`�@_�W@_�6@_~�@^��@^Ov@^5?@]�9@]�C@]e,@](�@]�@\��@[��@[��@[_p@[>�@Z��@ZOv@Y��@Y�@Y\�@YQ�@YDg@X�5@X1@V�h@U�#@U%@Tl"@S�
@S�k@S@O@R��@R�@Rz@Q��@Q��@Qw2@Q�@P�@P'R@O�Q@OX�@N�@NB[@N�@M��@M0�@L��@L�u@Le�@K��@K�@J�@J�F@J-@I��@I7L@I&�@I�@H�@H��@H`�@H@G�:@GK�@F��@F҉@F��@F� @Fu%@FR�@F�@E�@E��@E��@E�@D��@Du�@D/�@CU�@B�B@A�@A��@A�C@A�@A�"@A��@Ax�@@�|@@~(@?��@?�4@?@>�r@=��@=��@=��@=^�@=�@<q@<x@;��@;'�@:�}@:�1@:��@:W�@:?@:�@9�@9^�@94@8֡@8��@7�@733@6��@6��@6��@6�r@6h
@6$�@6_@5��@5��@5c�@5�@4w�@4?�@3�W@3��@3��@3~�@3Mj@2ff@25?@1�#@1�@1Q�@1�@0��@0�_@0`�@0!@/�g@/v`@/K�@/�@.�1@.a|@.($@-�@-p�@-F@-@@,��@,*�@+��@+�q@+K�@*�@*�x@*W�@*)�@)�~@)Y�@)7L@(��@(��@(<�@'�*@'y�@'s@']�@'W?@'RT@&�8@&�'@&�+@&GE@%�C@%`B@%<6@%@$֡@$tT@$<�@$	�@$�@#��@#�Q@#�a@#�q@#|�@#!-@# i@"��@"��@"�@"�@"3�@!��@!��@!A @!�@ �@ g8@ K^@ C-@�g@��@RT@)_@�@�X@�}@@�~@@@��@��@�@~(@y>@w�@u�@m�@S�@�@�g@�w@�@|�@6z@��@͟@�h@��@Ov@($@_@��@�H@�@��@��@u�@G�@*0@ \@�@�P@�@%�@��@��@�@o@�c@͟@�h@�b@h
@;�@J@��@�3@�n@��@m]@^�@#�@�p@��@Xy@PH@K^@%�@�+@�;@��@��@�k@t�@O@=@�@�@�@�@v�@c @R�@3�@�@�@@�>@�@@}�@o @Q�@!�@��@�@oi@bN@Q�@/�@�$@�@l�@K�@�8@�,@��@�b@3�@$�@$�@{@�@��@�z@�@��@s�@Q�@�@�j@��@S�@"h@7@	�@�@˒@��@�*111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�=qA�@A�8�A�:^A�)�A�*eA�.IA�5A�.�A�Aм6AЛqAЎ�AІ%A�{JA�v�A�q�A�rA�rA�p;A�poA�o�A�n/A�h�A�g�A�g�A�f�A�e�A�`�A�TaA�QNA�OA�K^A�7A��A�cA��2A�ںA͹$A̸A��A˕�A�`�Aǚ7AƦ�A�TaA���A��A��UA��A�C-A��-A���A���A��A�[�A��A�U�A�]�A��fA���A���A�HA�{A���A���A��A��A��A�ɺA�7�A�8RA���A�G�A�3hA�i�A���A���A��QA�tA��A�qA�ʌA���A��2A���A�+A��(A�x�A��+A��A7�A{4nAy�FAx��Ax[WAxqAw�+Aq)_Am�@Ama�AlFtAj6Ag�Ac]dA`��A]bNA[qvAY�ATZAP��AM9�AJ��AIu�AFQ�AC�hAAqvA>��A=��A;sA8��A7�A5d�A4+A2�+A2��A2�A1��A0�ZA/�{A-H�A+�A+QA*��A)�	A(CA'A&�}A&J�A%��A%��A$��A#��A"��A!/�A ��A �A͟A�A�{A��A�*A�!A�$A�"A�7AB�A�WA��A��A�Av`A%�A��AjA4A��AS�AqA�A�A�A�MA��A}�A!�A��A��AVA��A��Al�A��A�$A�?AĜA�A8�Ay>A4nAn�AP�A��A�rAn/A��A�RA�YA^5A��A
�9A	�MA��A��A3�A�A�;A�A��A5�A��A�XAiDAW�AP�AIRA&�AAx�A�A�tAZ�A4nA�A�A�A{JA �fA Y�@���@��K@���@��"@���@�($@��a@�q@�3�@��@@��8@��@�@��@�9�@�J�@�o@��	@�"�@�p;@��@�@��)@��@���@쟾@�*�@�y�@ꗍ@�S�@���@� i@��@�zx@�7L@���@�Ĝ@�l"@�4@�@�@��W@�Mj@��@�e@��@��@�_p@ව@�#:@ߩ*@�/@�ی@޸R@��@�g�@�W?@��@��T@ۆ�@�#�@�~�@�A�@��)@٢�@�Z�@�A @�:�@�/�@��@؃@�F@�kQ@�H�@���@Ք�@Լj@�3�@�!-@��@�L0@��9@п�@�,=@��@���@���@��@�&�@�l�@��@��>@ͯ�@ͅ@��@��@̯O@̌�@�C�@�rG@�ff@���@Ʌ�@�<6@��@���@�Q�@���@Ƿ�@� \@�~�@��@Œ:@�\�@���@�u%@��K@�dZ@���@�@��^@�o@�u�@�{@���@�S&@�%@��E@�?�@���@���@�J�@��A@���@�_p@���@�@���@�"�@� i@��s@���@�Ta@�ݘ@��4@�2a@��@���@�q�@���@��@�ѷ@���@�"h@�o@�B[@�&�@��@��@���@�v`@� \@���@��@���@��v@��1@�0U@��	@�\�@��@�U2@��a@�O�@��`@��m@��R@��h@���@���@�5?@��r@���@��o@�!@��@���@�˒@��@@�t�@��@�{�@��T@���@��f@���@�}�@�2a@��@��D@�&�@���@���@�U�@���@��@�R�@��@���@�;d@�
=@��z@�E�@��r@���@��P@�|�@�W?@�@O@�33@��@���@�2�@��Z@���@�j�@�U�@�B�@�.I@��@��B@���@�?@��A@�J#@���@���@�xl@��@�u�@�IR@��@��@�@�@�b@�c@�K�@�!-@�ѷ@���@��\@�0U@��a@��@��f@��x@�J�@���@���@�zx@�+@���@�c�@��j@�l�@�0�@��5@�;�@���@�o @�4�@��e@�:�@��]@��S@�S�@�IR@�1�@��@��m@��\@�D�@��@��j@�t�@�8@��]@���@��@���@�j@��@���@��r@���@���@��n@�J�@��@��9@�� @�~�@�-@���@��W@���@���@��:@�W?@�
=@��@��@���@�c�@�E�@�"h@�	�@���@��t@��@���@�RT@�Dg@�7L@��@��P@��/@��B@���@���@��e@���@��D@�3�@���@��@�[W@��@��M@��@�ی@��@�~(@�1@��$@�8�@�#�@�!�@��@��c@�ی@��?@�~(@�e�@�PH@�I�@�1�@�@��@�	@l�@9�@~�]@~Ov@}�@}w2@|��@|��@|��@|z�@|M@|1@{�V@{8@zz@zJ@yhs@x��@x��@x~@w��@w�@vv�@v:*@u��@u5�@t�I@sƨ@sj�@s�@r��@r=q@q�@qY�@p�/@p��@p`�@o��@oW?@n�@n�}@n�x@n_�@m@k�@k�	@kt�@kJ#@k i@j͟@j��@jl�@j@iw2@h��@h�4@h]d@h �@h�@g��@g��@gn/@g�@f�x@f)�@ex�@dѷ@dx@c��@cx@c>�@b��@b_�@a�@`��@`M@`/�@`�@_�W@_�6@_~�@^��@^Ov@^5?@]�9@]�C@]e,@](�@]�@\��@[��@[��@[_p@[>�@Z��@ZOv@Y��@Y�@Y\�@YQ�@YDg@X�5@X1@V�h@U�#@U%@Tl"@S�
@S�k@S@O@R��@R�@Rz@Q��@Q��@Qw2@Q�@P�@P'R@O�Q@OX�@N�@NB[@N�@M��@M0�@L��@L�u@Le�@K��@K�@J�@J�F@J-@I��@I7L@I&�@I�@H�@H��@H`�@H@G�:@GK�@F��@F҉@F��@F� @Fu%@FR�@F�@E�@E��@E��@E�@D��@Du�@D/�@CU�@B�B@A�@A��@A�C@A�@A�"@A��@Ax�@@�|@@~(@?��@?�4@?@>�r@=��@=��@=��@=^�@=�@<q@<x@;��@;'�@:�}@:�1@:��@:W�@:?@:�@9�@9^�@94@8֡@8��@7�@733@6��@6��@6��@6�r@6h
@6$�@6_@5��@5��@5c�@5�@4w�@4?�@3�W@3��@3��@3~�@3Mj@2ff@25?@1�#@1�@1Q�@1�@0��@0�_@0`�@0!@/�g@/v`@/K�@/�@.�1@.a|@.($@-�@-p�@-F@-@@,��@,*�@+��@+�q@+K�@*�@*�x@*W�@*)�@)�~@)Y�@)7L@(��@(��@(<�@'�*@'y�@'s@']�@'W?@'RT@&�8@&�'@&�+@&GE@%�C@%`B@%<6@%@$֡@$tT@$<�@$	�@$�@#��@#�Q@#�a@#�q@#|�@#!-@# i@"��@"��@"�@"�@"3�@!��@!��@!A @!�@ �@ g8@ K^@ C-@�g@��@RT@)_@�@�X@�}@@�~@@@��@��@�@~(@y>@w�@u�@m�@S�@�@�g@�w@�@|�@6z@��@͟@�h@��@Ov@($@_@��@�H@�@��@��@u�@G�@*0@ \@�@�P@�@%�@��@��@�@o@�c@͟@�h@�b@h
@;�@J@��@�3@�n@��@m]@^�@#�@�p@��@Xy@PH@K^@%�@�+@�;@��@��@�k@t�@O@=@�@�@�@�@v�@c @R�@3�@�@�@@�>@�@@}�@o @Q�@!�@��@�@oi@bN@Q�@/�@�$@�@l�@K�@�8@�,@��@�b@3�@$�@$�@{@�@��@�z@�@��@s�@Q�@�@�j@��@S�@"h@7@	�@�@˒@��@�*111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B)�B)�B)�B)�B)�B)�B)yB)�B)yB)_B($B'�B'�B'�B'�B(
B'�B'�B(
B(
B'�B($B($B($B($B(>B(>B(XB(�B(�B(�B(�B(�B(�B2B7�B>�BBABI�B��B�BуB	B!B1vBq[B��B�nB� B��B�B"BYB�B�B�BgBdB$ZB'B.�B2�B1'B/5B)�B!�B�B�B��B��BևB��B��B��B��B�0B�B|jBqBhsBN�B1�B&fB
�B
��B
v�B
^�B
NpB
8�B
$�B
�B	��B	��B	ҽB	��B	�	B	ǮB	�-B	�B	�{B	�}B	�fB	{dB	m]B	X�B	JrB	88B	,�B	VB	�B��B�ZB�B�KB��B�BܒB�bB��B��B�dB��B�=B��BɺB�RBɆB�lB�B��B�B�EB�sB֡BרB�B�bB�B�2B�B�5B��B�B��B�B��B��B��B	9B	�B	�B	�B	B	.IB	6�B	A B	J#B	S[B	_�B	l=B	raB	p�B	o5B	m�B	g8B	n}B	v+B	t�B	s�B	z�B	u�B	r�B	r|B	n�B	utB	u?B	t�B	t�B	yrB	��B	�bB	�|B	�7B	�=B	��B	��B	�B	��B	��B	ƨB	��B	�jB	�DB	��B	�0B	��B	�fB	�}B	�B	�{B	�B	��B	ɠB	ȀB	�fB	�B	�RB	�lB	�)B	�1B	��B	ƨB	�?B	��B	żB	�mB	��B	��B	��B	��B	˒B	ˬB	�0B	��B	�jB	�"B	͹B	�B	�B	ɺB	�7B	��B	��B	ǔB	�_B	��B	�B	��B	�YB	�?B	ɆB	�rB	�XB	�#B	�xB	�dB	�VB	҉B	��B	��B	ɺB	ɆB	˒B	�^B	�DB	�=B	�=B	ɠB	ɺB	�RB	ȴB	ȚB	�B	�RB	ɆB	�)B	�xB	�^B	�)B	ˬB	�~B	͟B	�VB	οB	�B	�.B	��B	οB	�PB	͹B	��B	͟B	��B	�"B	�VB	�B	�}B	�.B	��B	�vB	бB	уB	�B	�oB	�oB	�TB	�:B	��B	��B	� B	өB	�B	ӏB	�B	՛B	ּB	ևB	�YB	�$B	�YB	�sB	�+B	��B	��B	��B	��B	׍B	ںB	چB	�B	��B	�#B	�qB	�CB	��B	�xB	ݲB	�B	�OB	ݲB	�OB	�!B	ߊB	߾B	�B	�\B	�BB	�BB	�HB	��B	��B	��B	�B	��B	�B	�:B	�TB	�&B	�&B	��B	�B	�>B	�XB	�XB	�sB	�>B	�>B	�XB	��B	��B	�KB	�B	�B	��B	�B	�"B	�WB	�=B	�=B	�B	�B	�)B	�B	�B	��B	�B	�cB	�B	�B	�UB	�B	�OB	�B	�B	��B	�B	�MB	�B	�B	�nB	�TB	��B	�B	��B	�+B	�tB	��B	��B	�ZB	�`B	�B	��B	�B	�B	�2B	�B	��B	��B	��B	�2B	�B	��B	�	B	�>B	�>B	��B	��B	��B	�*B	��B	��B	��B	�6B	�PB	�PB	��B	�qB	��B	�B	�wB	��B	�wB	��B	�B
 B
 OB
 �B
 B
�B
�B
uB
uB
�B
-B
-B
-B
{B
�B
�B
�B
B
�B
%B
�B
B
B
B
%B
�B
	�B
0B
6B
dB
B
�B
JB
JB
B
�B
�B
B
�B
VB
<B
�B
�B
�B
�B
B
�B
�B
�B
vB
�B
�B
vB
�B
�B
�B
:B
TB
�B
@B
B
�B
oB
�B
{B
�B
2B
SB
�B
$B
�B
�B
�B
�B
�B
_B
�B
�B
B
�B
�B
7B
�B
	B
�B
�B
#B
�B
�B
�B
dB
�B
�B
B
B
�B
�B
�B
�B
 B
 'B
 vB
 �B
!HB
!�B
"�B
# B
#nB
#�B
$&B
$tB
$�B
$�B
%`B
%�B
&B
&LB
&fB
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'B
(
B
(>B
(
B
)_B
)�B
)�B
*B
*KB
*0B
*0B
*�B
+�B
,=B
,qB
,qB
,�B
,�B
,�B
-]B
.�B
.�B
.�B
/ B
/OB
0B
0UB
0oB
0�B
0�B
1AB
1[B
1'B
1�B
2aB
2aB
2aB
2�B
2�B
3B
3MB
3B
3�B
3�B
4B
4nB
4�B
4�B
4�B
5ZB
5�B
5�B
5�B
6+B
6`B
6�B
6�B
7B
7�B
7�B
7�B
7�B
8lB
8�B
9$B
:B
9�B
:^B
:�B
:xB
:^B
;dB
<B
<B
<B
<B
<6B
<jB
<jB
<�B
<�B
=VB
=�B
=�B
>(B
>BB
>BB
>]B
>�B
>�B
?B
?.B
?�B
@4B
@OB
@�B
@�B
AB
A;B
AUB
@�B
@ B
?�B
@ B
@4B
@OB
@iB
@�B
@�B
AUB
A;B
AUB
A�B
A�B
BAB
B[B
BuB
B�B
CGB
C{B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
E�B
FYB
G_B
G�B
HB
H�B
IB
IRB
J�B
J�B
J�B
K^B
K)B
KB
J�B
JrB
JrB
J�B
K)B
KxB
K^B
K�B
L~B
L~B
L�B
L�B
MB
M�B
NB
OB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
R B
R�B
R�B
SB
R�B
S&B
SB
SB
SuB
S�B
S�B
S�B
S�B
S�B
TB
T,B
T�B
TB
S�B
S�B
TFB
T�B
T�B
UB
T�B
VB
U�B
U�B
UgB
UgB
U�B
VB
V�B
V�B
V�B
V�B
W
B
W?B
W�B
X+B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
Z�B
Z�B
[#B
[=B
\)B
\]B
\xB
\]B
\�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
_!B
_pB
_�B
`B
`'B
`BB
`vB
`vB
`�B
`�B
`�B
aB
a|B
a�B
a�B
bB
b�B
b�B
b�B
b�B
c B
c:B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
cnB
cTB
c�B
dtB
dZB
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fB
gB
g�B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kB
kQB
kkB
k�B
lB
lWB
lqB
lWB
l�B
mB
m)B
mB
mCB
m�B
mwB
m�B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
oOB
oiB
oiB
o�B
o�B
o�B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
u�B
vB
vFB
v`B
v`B
v�B
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y�B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
{0B
{B
{0B
{JB
{dB
{JB
{B
{�B
{dB
{B
{B
{JB
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
}"B
}"111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B)�B)�B)�B)�B)�B)�B)yB)�B)yB)_B($B'�B'�B'�B'�B(
B'�B'�B(
B(
B'�B($B($B($B($B(>B(>B(XB(�B(�B(�B(�B(�B(�B2B7�B>�BBABI�B��B�BуB	B!B1vBq[B��B�nB� B��B�B"BYB�B�B�BgBdB$ZB'B.�B2�B1'B/5B)�B!�B�B�B��B��BևB��B��B��B��B�0B�B|jBqBhsBN�B1�B&fB
�B
��B
v�B
^�B
NpB
8�B
$�B
�B	��B	��B	ҽB	��B	�	B	ǮB	�-B	�B	�{B	�}B	�fB	{dB	m]B	X�B	JrB	88B	,�B	VB	�B��B�ZB�B�KB��B�BܒB�bB��B��B�dB��B�=B��BɺB�RBɆB�lB�B��B�B�EB�sB֡BרB�B�bB�B�2B�B�5B��B�B��B�B��B��B��B	9B	�B	�B	�B	B	.IB	6�B	A B	J#B	S[B	_�B	l=B	raB	p�B	o5B	m�B	g8B	n}B	v+B	t�B	s�B	z�B	u�B	r�B	r|B	n�B	utB	u?B	t�B	t�B	yrB	��B	�bB	�|B	�7B	�=B	��B	��B	�B	��B	��B	ƨB	��B	�jB	�DB	��B	�0B	��B	�fB	�}B	�B	�{B	�B	��B	ɠB	ȀB	�fB	�B	�RB	�lB	�)B	�1B	��B	ƨB	�?B	��B	żB	�mB	��B	��B	��B	��B	˒B	ˬB	�0B	��B	�jB	�"B	͹B	�B	�B	ɺB	�7B	��B	��B	ǔB	�_B	��B	�B	��B	�YB	�?B	ɆB	�rB	�XB	�#B	�xB	�dB	�VB	҉B	��B	��B	ɺB	ɆB	˒B	�^B	�DB	�=B	�=B	ɠB	ɺB	�RB	ȴB	ȚB	�B	�RB	ɆB	�)B	�xB	�^B	�)B	ˬB	�~B	͟B	�VB	οB	�B	�.B	��B	οB	�PB	͹B	��B	͟B	��B	�"B	�VB	�B	�}B	�.B	��B	�vB	бB	уB	�B	�oB	�oB	�TB	�:B	��B	��B	� B	өB	�B	ӏB	�B	՛B	ּB	ևB	�YB	�$B	�YB	�sB	�+B	��B	��B	��B	��B	׍B	ںB	چB	�B	��B	�#B	�qB	�CB	��B	�xB	ݲB	�B	�OB	ݲB	�OB	�!B	ߊB	߾B	�B	�\B	�BB	�BB	�HB	��B	��B	��B	�B	��B	�B	�:B	�TB	�&B	�&B	��B	�B	�>B	�XB	�XB	�sB	�>B	�>B	�XB	��B	��B	�KB	�B	�B	��B	�B	�"B	�WB	�=B	�=B	�B	�B	�)B	�B	�B	��B	�B	�cB	�B	�B	�UB	�B	�OB	�B	�B	��B	�B	�MB	�B	�B	�nB	�TB	��B	�B	��B	�+B	�tB	��B	��B	�ZB	�`B	�B	��B	�B	�B	�2B	�B	��B	��B	��B	�2B	�B	��B	�	B	�>B	�>B	��B	��B	��B	�*B	��B	��B	��B	�6B	�PB	�PB	��B	�qB	��B	�B	�wB	��B	�wB	��B	�B
 B
 OB
 �B
 B
�B
�B
uB
uB
�B
-B
-B
-B
{B
�B
�B
�B
B
�B
%B
�B
B
B
B
%B
�B
	�B
0B
6B
dB
B
�B
JB
JB
B
�B
�B
B
�B
VB
<B
�B
�B
�B
�B
B
�B
�B
�B
vB
�B
�B
vB
�B
�B
�B
:B
TB
�B
@B
B
�B
oB
�B
{B
�B
2B
SB
�B
$B
�B
�B
�B
�B
�B
_B
�B
�B
B
�B
�B
7B
�B
	B
�B
�B
#B
�B
�B
�B
dB
�B
�B
B
B
�B
�B
�B
�B
 B
 'B
 vB
 �B
!HB
!�B
"�B
# B
#nB
#�B
$&B
$tB
$�B
$�B
%`B
%�B
&B
&LB
&fB
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'B
(
B
(>B
(
B
)_B
)�B
)�B
*B
*KB
*0B
*0B
*�B
+�B
,=B
,qB
,qB
,�B
,�B
,�B
-]B
.�B
.�B
.�B
/ B
/OB
0B
0UB
0oB
0�B
0�B
1AB
1[B
1'B
1�B
2aB
2aB
2aB
2�B
2�B
3B
3MB
3B
3�B
3�B
4B
4nB
4�B
4�B
4�B
5ZB
5�B
5�B
5�B
6+B
6`B
6�B
6�B
7B
7�B
7�B
7�B
7�B
8lB
8�B
9$B
:B
9�B
:^B
:�B
:xB
:^B
;dB
<B
<B
<B
<B
<6B
<jB
<jB
<�B
<�B
=VB
=�B
=�B
>(B
>BB
>BB
>]B
>�B
>�B
?B
?.B
?�B
@4B
@OB
@�B
@�B
AB
A;B
AUB
@�B
@ B
?�B
@ B
@4B
@OB
@iB
@�B
@�B
AUB
A;B
AUB
A�B
A�B
BAB
B[B
BuB
B�B
CGB
C{B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
EB
E�B
FYB
G_B
G�B
HB
H�B
IB
IRB
J�B
J�B
J�B
K^B
K)B
KB
J�B
JrB
JrB
J�B
K)B
KxB
K^B
K�B
L~B
L~B
L�B
L�B
MB
M�B
NB
OB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
R B
R�B
R�B
SB
R�B
S&B
SB
SB
SuB
S�B
S�B
S�B
S�B
S�B
TB
T,B
T�B
TB
S�B
S�B
TFB
T�B
T�B
UB
T�B
VB
U�B
U�B
UgB
UgB
U�B
VB
V�B
V�B
V�B
V�B
W
B
W?B
W�B
X+B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
Z�B
Z�B
[#B
[=B
\)B
\]B
\xB
\]B
\�B
\xB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
_!B
_pB
_�B
`B
`'B
`BB
`vB
`vB
`�B
`�B
`�B
aB
a|B
a�B
a�B
bB
b�B
b�B
b�B
b�B
c B
c:B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
cnB
cTB
c�B
dtB
dZB
dZB
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fB
gB
g�B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
i*B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
kB
kQB
kkB
k�B
lB
lWB
lqB
lWB
l�B
mB
m)B
mB
mCB
m�B
mwB
m�B
m�B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
oOB
oiB
oiB
o�B
o�B
o�B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
u�B
vB
vFB
v`B
v`B
v�B
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y�B
yrB
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
{0B
{B
{0B
{JB
{dB
{JB
{B
{�B
{dB
{B
{B
{JB
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
}"B
}"111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104937  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174538  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174538  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174538                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024545  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024545  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                