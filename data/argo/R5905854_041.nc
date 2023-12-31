CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:51:51Z creation;2022-06-04T17:51:51Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20220604175151  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               )A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��%�	{1   @���z�H@0������ch�\)1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���Bә�B���B�  B�33B���B�  B�ffBB���B�  B�  B���C  C  C  C  C
  C  C33C�fC�fC�fC  C  C  C  C�C 33C!��C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF33CH�CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffA���B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��gB��gBӳ4B��gB��B�L�B��gB��B� B�4B��gB��B��B��gC�C�C�C�C
�C�C@ C�3C�3C�3C�C�C�C�C&gC @ C!ٚC#�3C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD&gCF@ CH&gCI�3CK�3CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx&gCy�3C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#|�D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM	�DM��DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��gD�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��gD�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�D��D���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AƚAƜAƉ�A�.�AřeA�O�A�>wA�:�A�0�A�'�A�!bA� �A�~A�xA�+A�A� A�
	A�
=A��A�A���A��8A���A���A�u�A�S�A�U�A�X�A�;�A�/A�/A�!-A� �A���A��iA��A���A��|Aá�AÆYA�O�A��A���A�ʌA¾�A¯�A¨�AA�l�A��aA��kA�iA�]/A�k�A�5A���A��AA�4�A��xA�w�A���A��rA��<A��A�=A���A���A�"�A��,A��|A��A��A���A�ȀA�� A���A�SA�K�A��A��YA��.A��A���A�2�A�{�A��<A�~�A�r�A�u�A���A���A�K�A��AA��ZA�0�A�pA%A~p;Ax�rAs"�Am�"Ah�Ag-�AfzAec�AcGEA`�XA]IRAVh
AT\)AP��AN��AK�HAI�AD�NAC�AB�A@�A=*0A;�.A:}VA7|�A4�A3IRA2��A2��A1��A0�A.?�A-MA,��A+�aA,��A,�KA,��A-�+A-�bA-F�A,y>A*OvA'�uA&8A&7LA%��A&�XA%�jA%?}A$�^A$PHA#��A#��A#F�A"�dA"_pA"�=A"�A"p;A!�rA!ѷA �FA خA!�A j�A��A��Al"AVA�xAA]dA��A�PA�YA��A|�A�Ay>A�<A}�A�A�Aa�A$�A��A�A iAA=�AخA�A[�AGAi�AA�)A�CA��A��A�A�XA1�ARTA�A^5A�A@A��A9�A
�A
�wA
^�A
7A	�xA	1A�A��A6zA��A��Av�A�A  A��A�VA�_AݘA�hA�A6zA�A`BA ��A �A 9�@��@�>B@�g�@�\�@�_@��@�-@�/�@���@�8�@���@��m@��@��z@���@�*0@���@��
@��P@�p;@�+k@��@���@�<�@���@�7@��@���@�j@��r@�	�@�-@�zx@��@�\)@�I@��@�A @腈@��@�A @��@�e�@䭬@�q@�J@�C�@�u�@�h@�Z@�:*@ߔ�@޲�@�ԕ@ݠ�@�l�@��@�?@�� @�>�@�H�@پw@�L�@ة�@ד�@�d�@�K�@�$t@Ի�@ԔF@�tT@��@ӹ�@ӫ�@Ӣ�@�~�@�0�@Ҵ9@���@�Dg@���@��D@���@ϟV@�j�@�U�@���@�Z�@͊	@���@�-@˶F@�&�@�h�@ɦ�@ȝI@��@��@�k�@��H@�V�@�	@�ԕ@ņ�@�1�@��c@Ĭ@�$@��@��@´9@�?@�dZ@���@���@�l�@�!-@��/@�1�@�c�@��@���@�n/@�Mj@� i@�;@���@���@�X@���@��@�1�@��P@���@��	@�V@���@��@�ѷ@���@�PH@��d@�s�@�hs@�	@�  @��@�^�@�C�@�@O@�4@���@�ff@��@��N@���@���@�l"@�/�@�O@��L@���@�B[@��@�ƨ@���@��@�_p@��@�E�@���@��P@�O@��@���@�R�@�!@��@��K@��F@��@�O�@��E@���@�A�@��@���@�Vm@��\@�@��@���@�o�@��"@��@�L�@�+@��j@�R�@�@���@���@�/@�͟@�e@���@��"@��	@�҉@���@�6�@���@�Dg@��@��@��)@��4@��x@���@���@���@�{�@�7@��@��@���@��6@���@�:�@�1�@��@��/@�Ĝ@���@���@��+@�q@�j@�]d@�$@�S�@���@��@�g8@� �@���@�,�@���@���@���@��@��F@���@�y>@�_�@��@���@���@�&�@�ѷ@�`�@��w@��4@� \@��@��@���@�V@��@���@�O�@�!�@��@��@�ی@���@���@�q@�J�@� �@��@�F�@�@���@�/�@��@��@�@�Y�@��@��j@�GE@���@���@�\�@�C�@�C@�֡@��L@��@�v�@�V�@� �@��'@�T�@��@���@�s�@�?@��&@���@�n/@�A @�<6@�6z@�͟@��x@���@�J�@��m@��h@�e,@�F@�-w@���@��@��.@�\�@���@���@���@��@��@���@���@���@�~(@�xl@�W�@�:*@�'R@�{@�u@��o@��T@��Q@���@�7L@���@��@�ѷ@�͟@���@���@���@�`�@��@��@��Q@���@���@�[W@��@��@��$@�i�@�:�@�	�@��@
=@~��@~V@}��@}�j@}��@}��@}��@}@|m�@{�+@{��@{.I@zxl@yu�@xN�@w�a@w��@wo�@w1�@v��@v��@vM�@v$�@v�@u�9@uj@u!�@t�@t�_@t|�@t`�@t7@s�@s��@ri�@q@q�@p�@pC-@oo@n-@m��@m�@l�U@lD�@k��@k i@j��@j_@i��@i+�@h�@h�@hq@h!@g�0@gx@g\)@g8@g�@f�R@fkQ@e�C@d��@d�_@dbN@cخ@cx@c;d@b�@b��@b��@b�@a;@`�@`��@`~(@`/�@_�A@_��@_X�@^�1@^J�@]�@]!�@\�?@\m�@\e�@\[�@\Ft@\~@[��@ZZ�@Z	@Y��@YG�@YV@X�/@X��@Xoi@XD�@W�r@W��@Ws@V�y@V��@VJ@U��@U7L@T��@T�@Th�@T?�@T�@S�]@S�@S�{@SW?@SY@R�@R��@RW�@Q�T@Qk�@QF@Q@@P��@P�@O=@N�B@N;�@M��@MY�@M�@L��@L��@Lc�@LC-@K�;@K�P@K\)@KW?@KK�@K�@Jz@J5?@J�@J �@I��@IG�@I�@H��@H��@HN�@G�A@G�$@G�@F�<@F�@F� @F^5@F
�@E�@E�@Eϫ@Ew2@Ec�@E!�@D�/@D�e@D��@D  @C�k@CC�@C i@B�@Bq�@BL0@A��@Au�@A#�@@��@@_@@]d@@V�@@ �@?�@?�@?~�@?H�@?�@>�!@=�>@=s�@=u�@=�M@=(�@<�@<Ĝ@<PH@;��@:��@:�1@:u@9��@9��@9+@8�j@8��@8$@7�@74�@6��@6p;@6Ta@6 �@5�9@5��@5G�@5&�@4�j@4tT@4u�@4��@4��@4j@4M@4x@3�;@3�g@3��@3v`@3�@2s�@2GE@2!�@1�@1�M@1-w@1%@0�|@0��@0u�@0V�@0!@0�@/��@/1�@.��@.��@.��@.�r@.Ov@.0U@-�o@-��@-�^@-�^@-m]@-@@,�E@,�D@,U2@,�@+�]@+��@+�@+��@+33@+@*�H@*��@*\�@)�o@)�z@)�@)�@)a�@)X@)IR@)8�@)@)%@(�9@(9X@'�;@'��@'a@'RT@'H�@'E9@'8@'"�@'@&��@&$�@&@%��@%��@%?}@%�@$�/@$��@$��@$�Y@$h�@$bN@$7@#��@#b�@#"�@"�@"��@"��@".�@!�@!�^@!�@!&�@!%@ �@ u�@ �@�@��@�@@8@�y@kQ@=q@$�@_@�@��@:�@%F@�@ѷ@�z@c�@9X@�@��@��@g�@9�@"�@�!@)�@@�@�h@`B@+�@�v@��@h�@Q�@4n@�@�A@��@�@��@kQ@H�@6�@#:@��@�M@F@�@�j@?�@��@C�@@�@�m@�@v�@-@�t@}�@O�@2a@�@�/@��@u�@7@��@��@j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AƚAƜAƉ�A�.�AřeA�O�A�>wA�:�A�0�A�'�A�!bA� �A�~A�xA�+A�A� A�
	A�
=A��A�A���A��8A���A���A�u�A�S�A�U�A�X�A�;�A�/A�/A�!-A� �A���A��iA��A���A��|Aá�AÆYA�O�A��A���A�ʌA¾�A¯�A¨�AA�l�A��aA��kA�iA�]/A�k�A�5A���A��AA�4�A��xA�w�A���A��rA��<A��A�=A���A���A�"�A��,A��|A��A��A���A�ȀA�� A���A�SA�K�A��A��YA��.A��A���A�2�A�{�A��<A�~�A�r�A�u�A���A���A�K�A��AA��ZA�0�A�pA%A~p;Ax�rAs"�Am�"Ah�Ag-�AfzAec�AcGEA`�XA]IRAVh
AT\)AP��AN��AK�HAI�AD�NAC�AB�A@�A=*0A;�.A:}VA7|�A4�A3IRA2��A2��A1��A0�A.?�A-MA,��A+�aA,��A,�KA,��A-�+A-�bA-F�A,y>A*OvA'�uA&8A&7LA%��A&�XA%�jA%?}A$�^A$PHA#��A#��A#F�A"�dA"_pA"�=A"�A"p;A!�rA!ѷA �FA خA!�A j�A��A��Al"AVA�xAA]dA��A�PA�YA��A|�A�Ay>A�<A}�A�A�Aa�A$�A��A�A iAA=�AخA�A[�AGAi�AA�)A�CA��A��A�A�XA1�ARTA�A^5A�A@A��A9�A
�A
�wA
^�A
7A	�xA	1A�A��A6zA��A��Av�A�A  A��A�VA�_AݘA�hA�A6zA�A`BA ��A �A 9�@��@�>B@�g�@�\�@�_@��@�-@�/�@���@�8�@���@��m@��@��z@���@�*0@���@��
@��P@�p;@�+k@��@���@�<�@���@�7@��@���@�j@��r@�	�@�-@�zx@��@�\)@�I@��@�A @腈@��@�A @��@�e�@䭬@�q@�J@�C�@�u�@�h@�Z@�:*@ߔ�@޲�@�ԕ@ݠ�@�l�@��@�?@�� @�>�@�H�@پw@�L�@ة�@ד�@�d�@�K�@�$t@Ի�@ԔF@�tT@��@ӹ�@ӫ�@Ӣ�@�~�@�0�@Ҵ9@���@�Dg@���@��D@���@ϟV@�j�@�U�@���@�Z�@͊	@���@�-@˶F@�&�@�h�@ɦ�@ȝI@��@��@�k�@��H@�V�@�	@�ԕ@ņ�@�1�@��c@Ĭ@�$@��@��@´9@�?@�dZ@���@���@�l�@�!-@��/@�1�@�c�@��@���@�n/@�Mj@� i@�;@���@���@�X@���@��@�1�@��P@���@��	@�V@���@��@�ѷ@���@�PH@��d@�s�@�hs@�	@�  @��@�^�@�C�@�@O@�4@���@�ff@��@��N@���@���@�l"@�/�@�O@��L@���@�B[@��@�ƨ@���@��@�_p@��@�E�@���@��P@�O@��@���@�R�@�!@��@��K@��F@��@�O�@��E@���@�A�@��@���@�Vm@��\@�@��@���@�o�@��"@��@�L�@�+@��j@�R�@�@���@���@�/@�͟@�e@���@��"@��	@�҉@���@�6�@���@�Dg@��@��@��)@��4@��x@���@���@���@�{�@�7@��@��@���@��6@���@�:�@�1�@��@��/@�Ĝ@���@���@��+@�q@�j@�]d@�$@�S�@���@��@�g8@� �@���@�,�@���@���@���@��@��F@���@�y>@�_�@��@���@���@�&�@�ѷ@�`�@��w@��4@� \@��@��@���@�V@��@���@�O�@�!�@��@��@�ی@���@���@�q@�J�@� �@��@�F�@�@���@�/�@��@��@�@�Y�@��@��j@�GE@���@���@�\�@�C�@�C@�֡@��L@��@�v�@�V�@� �@��'@�T�@��@���@�s�@�?@��&@���@�n/@�A @�<6@�6z@�͟@��x@���@�J�@��m@��h@�e,@�F@�-w@���@��@��.@�\�@���@���@���@��@��@���@���@���@�~(@�xl@�W�@�:*@�'R@�{@�u@��o@��T@��Q@���@�7L@���@��@�ѷ@�͟@���@���@���@�`�@��@��@��Q@���@���@�[W@��@��@��$@�i�@�:�@�	�@��@
=@~��@~V@}��@}�j@}��@}��@}��@}@|m�@{�+@{��@{.I@zxl@yu�@xN�@w�a@w��@wo�@w1�@v��@v��@vM�@v$�@v�@u�9@uj@u!�@t�@t�_@t|�@t`�@t7@s�@s��@ri�@q@q�@p�@pC-@oo@n-@m��@m�@l�U@lD�@k��@k i@j��@j_@i��@i+�@h�@h�@hq@h!@g�0@gx@g\)@g8@g�@f�R@fkQ@e�C@d��@d�_@dbN@cخ@cx@c;d@b�@b��@b��@b�@a;@`�@`��@`~(@`/�@_�A@_��@_X�@^�1@^J�@]�@]!�@\�?@\m�@\e�@\[�@\Ft@\~@[��@ZZ�@Z	@Y��@YG�@YV@X�/@X��@Xoi@XD�@W�r@W��@Ws@V�y@V��@VJ@U��@U7L@T��@T�@Th�@T?�@T�@S�]@S�@S�{@SW?@SY@R�@R��@RW�@Q�T@Qk�@QF@Q@@P��@P�@O=@N�B@N;�@M��@MY�@M�@L��@L��@Lc�@LC-@K�;@K�P@K\)@KW?@KK�@K�@Jz@J5?@J�@J �@I��@IG�@I�@H��@H��@HN�@G�A@G�$@G�@F�<@F�@F� @F^5@F
�@E�@E�@Eϫ@Ew2@Ec�@E!�@D�/@D�e@D��@D  @C�k@CC�@C i@B�@Bq�@BL0@A��@Au�@A#�@@��@@_@@]d@@V�@@ �@?�@?�@?~�@?H�@?�@>�!@=�>@=s�@=u�@=�M@=(�@<�@<Ĝ@<PH@;��@:��@:�1@:u@9��@9��@9+@8�j@8��@8$@7�@74�@6��@6p;@6Ta@6 �@5�9@5��@5G�@5&�@4�j@4tT@4u�@4��@4��@4j@4M@4x@3�;@3�g@3��@3v`@3�@2s�@2GE@2!�@1�@1�M@1-w@1%@0�|@0��@0u�@0V�@0!@0�@/��@/1�@.��@.��@.��@.�r@.Ov@.0U@-�o@-��@-�^@-�^@-m]@-@@,�E@,�D@,U2@,�@+�]@+��@+�@+��@+33@+@*�H@*��@*\�@)�o@)�z@)�@)�@)a�@)X@)IR@)8�@)@)%@(�9@(9X@'�;@'��@'a@'RT@'H�@'E9@'8@'"�@'@&��@&$�@&@%��@%��@%?}@%�@$�/@$��@$��@$�Y@$h�@$bN@$7@#��@#b�@#"�@"�@"��@"��@".�@!�@!�^@!�@!&�@!%@ �@ u�@ �@�@��@�@@8@�y@kQ@=q@$�@_@�@��@:�@%F@�@ѷ@�z@c�@9X@�@��@��@g�@9�@"�@�!@)�@@�@�h@`B@+�@�v@��@h�@Q�@4n@�@�A@��@�@��@kQ@H�@6�@#:@��@�M@F@�@�j@?�@��@C�@@�@�m@�@v�@-@�t@}�@O�@2a@�@�/@��@u�@7@��@��@j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	2�B	2�B	2-B	3�B	8�B	:�B	="B	=�B	>�B	?�B	@�B	A�B	D3B	E�B	F%B	G�B	H�B	KDB	NB	Q�B	U�B	\)B	cnB	mwB	xB	��B	��B	��B	�B	��B	��B	��B	�dB	��B	�wB	�ZB	ǮB	ǮB	ȀB	��B	��B	�fB
 B
�B
�B
#B
&B
&�B
*�B
3B
R:B
~�B
��B
�BO(BTBW�BxB��B��B~�B~�B~�B�~B�}B�B�B�xB�BwBq�B��B��B��B�zB�B��B��B��B��Bm�B<6BoB
�B
�-B
�B
�)B
�rB
��B
��B
�B
��B
|6B
f�B
K�B
'�B	��B	�}B	�B	�{B	��B	y�B	]/B	R:B	N�B	G_B	@ B	4nB	"�B	mB��B��B�B��B��B�[B�"B��B��B��B��B��B��B�eB�$B��B��B�9B��B�]B��B�wB�BB�QB	�B	.�B	W�B	o�B	l�B	e,B	]~B	jB	q�B	��B	��B	͹B	��B	��B	�B	��B	ѝB	�kB	�1B	�uB	�B	��B	��B	�hB	�nB	�
B	�bB	�B	�B	�-B	�&B	��B	��B	��B	�KB	�+B	ٚB	یB	��B	�vB	�HB	�;B	߾B	��B	�VB	�OB	�B	޸B	�B	�B	�TB	��B	��B	�\B	��B	��B	�)B	��B	�=B	�	B	�#B	�)B	�B	�vB	��B	�XB	�mB	�B	��B	�B	��B	�B	ߊB	��B	�pB	�HB	��B	��B	��B	�tB	�B	�2B	�LB	��B	�B	�ZB	�TB	�:B	� B	�B	�B	�NB	�bB	��B	�'B	�'B	ߤB	��B	��B	��B	�B	�	B	�B	׍B	��B	خB	��B	ևB	��B	�uB	�TB	уB	�B	ϫB	�BB	ΥB	�<B	��B	�XB	ɠB	ɺB	͹B	�B	��B	��B	�B	�B	�xB	��B	��B	��B	ՁB	�sB	��B	��B	�~B	ݲB	��B	�jB	�B	�dB	�xB	یB	��B	�B	�IB	ܒB	�	B	ٴB	��B	�B	�=B	�=B	�]B	ݘB	�B	�VB	�!B	ߊB	��B	��B	߾B	�pB	ߊB	ߊB	߾B	�VB	�/B	ܬB	�]B	��B	یB	��B	یB	�qB	�=B	�WB	��B	��B	�CB	�]B	��B	��B	�~B	ݲB	�~B	�IB	�IB	�B	��B	�B	ޞB	޸B	ߊB	ߊB	�pB	�BB	�\B	�HB	�B	�B	�:B	� B	�nB	�@B	�B	�ZB	�ZB	�ZB	��B	�`B	�zB	�B	�B	�B	�>B	�$B	�
B	��B	�sB	�B	�mB	�B	�B	�RB	�8B	��B	��B	�RB	�B	��B	�sB	��B	��B	�0B	��B	��B	�eB	�B	�B	�B	�]B	�iB	��B	��B	�$B	��B	��B	�xB	�^B	��B	�B	�0B	��B	�*B	��B	�rB	�$B	��B	�B	��B	��B	��B	��B	��B	�	B	��B	�$B	��B	�$B	�*B	�xB	�B	�*B	��B	�JB	�B	��B	�VB
  B
aB
%B
�B
fB
�B
�B
�B
fB
fB
�B
zB
B
fB
�B
	�B

rB

XB

�B

XB

�B

rB

�B

�B
	7B
�B
	RB
	�B
B
�B
PB
B
�B
�B
0B
B
�B
6B
B
�B
�B
vB
�B
�B
�B
B
B
�B
�B
B
&B
&B
@B
�B
B
�B
&B
�B
{B
�B
gB
2B
�B
�B
�B
�B
B
uB
B
uB
@B
B
B
B
�B
�B
�B
�B
�B
:B
&B
&B
�B
�B
�B
�B
�B
2B
gB
B
�B

B
?B
�B
�B
�B
�B
�B
�B
�B

B
YB
�B
B
B
�B
1B
eB
B
�B
B
kB
�B
�B
B
B
�B
B
�B
�B
 B
�B
 \B
 �B
 �B
!�B
!�B
"NB
"�B
#:B
#:B
$ZB
$�B
$�B
$�B
$�B
$@B
%,B
$�B
%B
%�B
&fB
&�B
'B
'8B
'�B
(>B
(XB
(�B
(�B
)DB
)_B
)_B
)_B
+6B
+kB
+kB
+�B
+�B
+�B
,B
,WB
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
-]B
./B
.�B
.}B
.cB
.}B
.�B
/ B
/iB
/�B
0B
0�B
0�B
1[B
1�B
2�B
2aB
2�B
2�B
2�B
2�B
2�B
2|B
2�B
33B
3MB
3�B
3�B
49B
4�B
5�B
5�B
6B
6FB
6zB
6zB
7B
72B
7LB
7LB
7fB
7�B
7�B
8B
8lB
8lB
8�B
8�B
8�B
8lB
9�B
:*B
:B
:�B
:�B
;dB
;dB
;�B
<B
<B
<�B
="B
="B
="B
=�B
>BB
>]B
>BB
>]B
>]B
>�B
>�B
?.B
?cB
?}B
?�B
?�B
?}B
?�B
@�B
@�B
AB
A�B
A�B
A�B
BB
A�B
A�B
B[B
CB
B�B
CB
C-B
CB
B�B
B�B
B�B
CB
B�B
C�B
C�B
C�B
D�B
D�B
EB
D�B
D�B
D�B
EB
EB
ESB
E�B
E�B
E�B
E�B
F?B
F?B
F�B
F�B
GEB
G�B
HB
H�B
H�B
H�B
IB
IB
IB
IlB
IRB
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J=B
J#B
J	B
J#B
JXB
K^B
K^B
K�B
LB
LdB
L�B
MB
MPB
M�B
MjB
MjB
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O(B
O(B
OBB
OvB
O�B
O�B
O�B
PB
PbB
P}B
P}B
P}B
P�B
P�B
P�B
P�B
P�B
PbB
PHB
P.B
P.B
PHB
P�B
Q�B
R B
R�B
R�B
R�B
R�B
SB
SuB
S�B
S�B
T,B
T,B
T,B
TB
TB
S�B
S�B
TB
TB
T,B
T{B
TFB
T�B
U�B
V9B
W$B
WsB
WYB
XB
X�B
YKB
YeB
YeB
Y1B
X�B
YeB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
[�B
\)B
\�B
\�B
]�B
^B
]�B
]�B
^B
^B
^B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^OB
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
_�B
`vB
`�B
`�B
`�B
`�B
aHB
aHB
a|B
a�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
b�B
cB
c B
cTB
c�B
c�B
c�B
d&B
dtB
d�B
d�B
d�B
d�B
eB
eB
e,B
e,B
e`B
e`B
e�B
fB
ffB
f�B
f�B
f�B
gB
f�B
f�B
gB
gB
gB
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
h�B
h�B
iB
h�B
i*B
iyB
i_B
i�B
i�B
i�B
j0B
j0B
jeB
jB
j�B
kB
k6B
kQB
lB
lWB
lWB
l=B
lqB
l�B
mB
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n}B
n�B
n�B
n�B
o5B
oOB
oiB
oiB
o�B
pB
p!B
pB
p�B
qB
q'B
q[B
q�B
q�B
q�B
rB
raB
r|B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
tB
t9B
t9B
t9B
t9B
t�B
t�B
uB
u?B
u�B
vFB
v�B
v�B
v�B
w2B
wLB
wLB
wfB
xB
xB
xRB
xlB
x�B
x�B
x�B
y	B
yXB
yrB
y�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	2�B	2�B	2-B	3�B	8�B	:�B	="B	=�B	>�B	?�B	@�B	A�B	D3B	E�B	F%B	G�B	H�B	KDB	NB	Q�B	U�B	\)B	cnB	mwB	xB	��B	��B	��B	�B	��B	��B	��B	�dB	��B	�wB	�ZB	ǮB	ǮB	ȀB	��B	��B	�fB
 B
�B
�B
#B
&B
&�B
*�B
3B
R:B
~�B
��B
�BO(BTBW�BxB��B��B~�B~�B~�B�~B�}B�B�B�xB�BwBq�B��B��B��B�zB�B��B��B��B��Bm�B<6BoB
�B
�-B
�B
�)B
�rB
��B
��B
�B
��B
|6B
f�B
K�B
'�B	��B	�}B	�B	�{B	��B	y�B	]/B	R:B	N�B	G_B	@ B	4nB	"�B	mB��B��B�B��B��B�[B�"B��B��B��B��B��B��B�eB�$B��B��B�9B��B�]B��B�wB�BB�QB	�B	.�B	W�B	o�B	l�B	e,B	]~B	jB	q�B	��B	��B	͹B	��B	��B	�B	��B	ѝB	�kB	�1B	�uB	�B	��B	��B	�hB	�nB	�
B	�bB	�B	�B	�-B	�&B	��B	��B	��B	�KB	�+B	ٚB	یB	��B	�vB	�HB	�;B	߾B	��B	�VB	�OB	�B	޸B	�B	�B	�TB	��B	��B	�\B	��B	��B	�)B	��B	�=B	�	B	�#B	�)B	�B	�vB	��B	�XB	�mB	�B	��B	�B	��B	�B	ߊB	��B	�pB	�HB	��B	��B	��B	�tB	�B	�2B	�LB	��B	�B	�ZB	�TB	�:B	� B	�B	�B	�NB	�bB	��B	�'B	�'B	ߤB	��B	��B	��B	�B	�	B	�B	׍B	��B	خB	��B	ևB	��B	�uB	�TB	уB	�B	ϫB	�BB	ΥB	�<B	��B	�XB	ɠB	ɺB	͹B	�B	��B	��B	�B	�B	�xB	��B	��B	��B	ՁB	�sB	��B	��B	�~B	ݲB	��B	�jB	�B	�dB	�xB	یB	��B	�B	�IB	ܒB	�	B	ٴB	��B	�B	�=B	�=B	�]B	ݘB	�B	�VB	�!B	ߊB	��B	��B	߾B	�pB	ߊB	ߊB	߾B	�VB	�/B	ܬB	�]B	��B	یB	��B	یB	�qB	�=B	�WB	��B	��B	�CB	�]B	��B	��B	�~B	ݲB	�~B	�IB	�IB	�B	��B	�B	ޞB	޸B	ߊB	ߊB	�pB	�BB	�\B	�HB	�B	�B	�:B	� B	�nB	�@B	�B	�ZB	�ZB	�ZB	��B	�`B	�zB	�B	�B	�B	�>B	�$B	�
B	��B	�sB	�B	�mB	�B	�B	�RB	�8B	��B	��B	�RB	�B	��B	�sB	��B	��B	�0B	��B	��B	�eB	�B	�B	�B	�]B	�iB	��B	��B	�$B	��B	��B	�xB	�^B	��B	�B	�0B	��B	�*B	��B	�rB	�$B	��B	�B	��B	��B	��B	��B	��B	�	B	��B	�$B	��B	�$B	�*B	�xB	�B	�*B	��B	�JB	�B	��B	�VB
  B
aB
%B
�B
fB
�B
�B
�B
fB
fB
�B
zB
B
fB
�B
	�B

rB

XB

�B

XB

�B

rB

�B

�B
	7B
�B
	RB
	�B
B
�B
PB
B
�B
�B
0B
B
�B
6B
B
�B
�B
vB
�B
�B
�B
B
B
�B
�B
B
&B
&B
@B
�B
B
�B
&B
�B
{B
�B
gB
2B
�B
�B
�B
�B
B
uB
B
uB
@B
B
B
B
�B
�B
�B
�B
�B
:B
&B
&B
�B
�B
�B
�B
�B
2B
gB
B
�B

B
?B
�B
�B
�B
�B
�B
�B
�B

B
YB
�B
B
B
�B
1B
eB
B
�B
B
kB
�B
�B
B
B
�B
B
�B
�B
 B
�B
 \B
 �B
 �B
!�B
!�B
"NB
"�B
#:B
#:B
$ZB
$�B
$�B
$�B
$�B
$@B
%,B
$�B
%B
%�B
&fB
&�B
'B
'8B
'�B
(>B
(XB
(�B
(�B
)DB
)_B
)_B
)_B
+6B
+kB
+kB
+�B
+�B
+�B
,B
,WB
,qB
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
-]B
./B
.�B
.}B
.cB
.}B
.�B
/ B
/iB
/�B
0B
0�B
0�B
1[B
1�B
2�B
2aB
2�B
2�B
2�B
2�B
2�B
2|B
2�B
33B
3MB
3�B
3�B
49B
4�B
5�B
5�B
6B
6FB
6zB
6zB
7B
72B
7LB
7LB
7fB
7�B
7�B
8B
8lB
8lB
8�B
8�B
8�B
8lB
9�B
:*B
:B
:�B
:�B
;dB
;dB
;�B
<B
<B
<�B
="B
="B
="B
=�B
>BB
>]B
>BB
>]B
>]B
>�B
>�B
?.B
?cB
?}B
?�B
?�B
?}B
?�B
@�B
@�B
AB
A�B
A�B
A�B
BB
A�B
A�B
B[B
CB
B�B
CB
C-B
CB
B�B
B�B
B�B
CB
B�B
C�B
C�B
C�B
D�B
D�B
EB
D�B
D�B
D�B
EB
EB
ESB
E�B
E�B
E�B
E�B
F?B
F?B
F�B
F�B
GEB
G�B
HB
H�B
H�B
H�B
IB
IB
IB
IlB
IRB
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J=B
J#B
J	B
J#B
JXB
K^B
K^B
K�B
LB
LdB
L�B
MB
MPB
M�B
MjB
MjB
M�B
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O(B
O(B
OBB
OvB
O�B
O�B
O�B
PB
PbB
P}B
P}B
P}B
P�B
P�B
P�B
P�B
P�B
PbB
PHB
P.B
P.B
PHB
P�B
Q�B
R B
R�B
R�B
R�B
R�B
SB
SuB
S�B
S�B
T,B
T,B
T,B
TB
TB
S�B
S�B
TB
TB
T,B
T{B
TFB
T�B
U�B
V9B
W$B
WsB
WYB
XB
X�B
YKB
YeB
YeB
Y1B
X�B
YeB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
[�B
\)B
\�B
\�B
]�B
^B
]�B
]�B
^B
^B
^B
^B
^OB
^jB
^�B
^�B
^�B
^�B
^OB
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
_�B
`vB
`�B
`�B
`�B
`�B
aHB
aHB
a|B
a�B
a�B
a�B
a�B
bB
bhB
b�B
b�B
b�B
b�B
cB
c B
cTB
c�B
c�B
c�B
d&B
dtB
d�B
d�B
d�B
d�B
eB
eB
e,B
e,B
e`B
e`B
e�B
fB
ffB
f�B
f�B
f�B
gB
f�B
f�B
gB
gB
gB
g�B
g�B
g�B
h
B
h�B
h�B
h�B
h�B
h�B
h�B
iB
h�B
i*B
iyB
i_B
i�B
i�B
i�B
j0B
j0B
jeB
jB
j�B
kB
k6B
kQB
lB
lWB
lWB
l=B
lqB
l�B
mB
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n}B
n�B
n�B
n�B
o5B
oOB
oiB
oiB
o�B
pB
p!B
pB
p�B
qB
q'B
q[B
q�B
q�B
q�B
rB
raB
r|B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
tB
t9B
t9B
t9B
t9B
t�B
t�B
uB
u?B
u�B
vFB
v�B
v�B
v�B
w2B
wLB
wLB
wfB
xB
xB
xRB
xlB
x�B
x�B
x�B
y	B
yXB
yrB
y�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104951  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175151  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175151  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175151                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025158  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025158  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                