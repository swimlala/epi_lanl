CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-29T21:41:47Z creation;2022-12-29T21:41:48Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221229214147  20221230012902  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�	*��$�1   @�	+OW�@//��w�c�����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A33A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B���B�33B�ffB�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(L�C)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @ff@���@���A  A ��A?33A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp��Bx��B��B��B��B��B��B��B��B��gB�L�B�� B��B��B��B�L�B�L�B��B��B��gB��B��B��B��B��B��B��B��B��B��B�L�B� B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&&gC(Y�C)�3C+�3C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT&gCV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX	�DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]	�D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{	�D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�MA��A�A��A�uA��A��A�!bA�7A�H�A�]/A�{AφYA�|PA�h�A�_�A�d�A�P}A�0�A�%�A�($A�%�A�"�A�'�A��A�
�A���A��A��<A��aA΂AA�u�A�O�A�K)A�3�A͈�A�^�A˹$A��A�<�A�R�A�MA��}A���A���A�n�A��FA�v�A��A�-A��}A��ZA���A���A�+kA��aA��DA��nA��<A���A��-A���A�R�A��A�ܒA�H�A�бA��'A�یA�.IA�PA�&�A�@A��A�7�A���A�l�A�e�A�-CA��rA�B�A�!�A���A}�VAvqvAoV�Ag�AdzAc�Acc�AbƨA`�A^��AZ�vAW��AS \ANbAI�aAE�AD��AD�AB�AA($A?&�A:y�A9�A7�A7��A8A8@OA8�[A8�A6OA5~(A3�A1N<A//�A.��A.�"A.{A-��A-MA,jA,<�A+�A+�XA+-wA*��A*.�A*3�A*9�A)��A)J�A(�fA(��A(��A(.�A'dZA'@OA'�A&sA&/�A%��A%��A%�XA%#�A$��A#��A#zA#X�A"�A!��A!�9A!ĜA"%�A!��A n�A��A��A�A�HA�pAj�A\�A�}A��A��A(�A�HAM�AjA��A��A��Ax�A�A��AC�A��A�A�A�A�9A�!AW�A@OA��A-A��A��A�A!�AخA��A/A-�A�}AFA�A�XA8�A�'AB[A
�NA
d�A	��A	Z�A	M�A��A�0A�cA�A��A�"A�A��AS�AhsA�A�qAF�A{�AVA ��A q@�\�A ,=A z�@���@��:@�;�@��@��.@���@��@�~@��L@��@�=@�ߤ@�N�@�خ@��x@�C@�:*@��@@��	@�v�@��>@�}�@��'@�Ov@�Y�@��|@�b@�n�@�W�@�)�@���@�B�@�K�@�
=@�I@�{@�rG@��B@���@��@�҉@�{@��@�U�@��@�ff@���@��@߂�@ޠ�@ބ�@�tT@ޛ�@ޯO@ݗ�@��@ܷ�@�U2@ۮ�@�!�@��@�N�@���@ـ4@��@؇�@؞@��[@ؠ�@�!�@��j@�\)@�kQ@��9@ՠ�@�6z@�s�@��@Ұ!@���@�
�@�I�@�o�@з�@��@�dZ@���@���@���@͹�@�}�@��P@�J@˪�@�%F@ʍ�@�_@�	@�S�@�	l@Ȟ�@�G@ǋ�@�8�@��E@ƚ@��@Ň�@ę1@Ď�@�خ@��@u@�,=@�	�@��3@�x@�@�ں@�v�@��0@�?}@���@�e�@�V�@�4n@��@��
@��@�W�@��w@�C�@�	l@���@�D�@���@�V@���@� �@��~@�_p@��|@�p;@���@�G�@�+�@��@���@�J@�O@�.I@�@��@��6@�i�@�b@���@�e�@�+@���@��Y@�*�@��T@�@�~�@�=�@�4@���@���@���@��@�G�@��@��@��)@�M@��t@��@�e,@�4�@�@���@���@��@��h@�l�@�C@���@�q�@�?�@��r@���@��~@���@���@�`�@�K^@�'R@��9@��w@���@�W?@�$t@��@�E�@�� @�8�@��j@�C-@��9@��*@�A @���@�~�@�\�@�<�@��@��9@���@�#�@�+@�S@���@�_�@�خ@�o�@�Y@���@�[�@��m@��3@�G@���@�A�@��v@��@�Xy@�7�@���@���@�m]@���@���@�h�@��j@���@�iD@�&@��h@�?�@��@���@�N<@�@��@���@��@��@���@�9�@�S@��!@�y>@�!@���@�8@��u@�$�@��@�u�@��8@���@���@���@�R�@�@��#@��^@���@�Z�@�C@���@�� @�V@�#:@��}@��k@�y�@��@��p@���@��X@��z@�Z@��@��-@��"@�Q�@�o@�@��c@��O@�K^@�+k@�@��)@��K@��@���@��5@���@���@�Ta@�!@��@���@�e,@�A @�S@���@�c�@��@���@���@��=@�a�@�K�@��@��|@���@�j@��@��@��@��@�m]@�&@���@��z@�ff@�r@�f@�@~�B@~��@~v�@~}V@~��@~xl@}��@}�@}��@}G�@|�`@|C-@{{J@{@O@{S@z��@z#:@y�'@y�@yϫ@y�o@y�@yQ�@x��@x�@w��@v�c@uzx@s��@s$t@r�,@rOv@q�D@q��@q��@qzx@p��@p�U@pj@o�@@o'�@n�@n��@n!�@m�S@mw2@mL�@m�@l��@l�$@lV�@l�@k�w@kiD@j��@jn�@j�@i��@i��@i��@i�j@i�t@i��@i@h�@g�+@g�
@g�@g��@g@fxl@e��@e@@dN�@c�F@c�@b�b@bM�@a�@a+@`q@`4n@_"�@^V@]�N@]G�@\��@\/�@[��@[)_@Z�@Z�+@Z;�@Z1�@Z0U@Y��@Y�@X��@X@W��@W��@W~�@W8@Vu%@U�@T�P@T/�@S��@S�w@S��@R��@R4@Q�o@Q�-@Qw2@Q2a@P��@O�m@O_p@N�8@N�@N.�@M�@M[W@M�@M�@L�)@L�O@Lj@K�]@K��@Ks@Kn/@KdZ@KF�@K(@J�@J��@Jn�@JV@J6�@I�@Ix�@I(�@H��@HFt@GX�@F��@F��@F�b@F&�@E�)@F�@F@E@EF@E	l@D�@D�z@D�o@Dy>@DD�@D	�@C��@C�F@C��@CS�@CE9@B�@B@A�@A��@@��@?��@?�
@?�0@?dZ@?33@?(@>�X@>�A@>c @=��@=��@=j@=@@<��@<,=@;|�@;F�@;9�@:�@:v�@9��@9�@9=�@9*0@8�@8��@8u�@84n@8@7��@7n/@6��@6��@6d�@6M�@68�@6	@5��@5�C@5��@5k�@5N<@5!�@4�z@4~@3��@3o�@3;d@3(@2�,@2�b@2i�@28�@1��@1�n@1+�@0�	@0Ɇ@0�@0`�@0"h@/�0@/e�@/@O@/.I@/!-@/�@.�y@.�@.Ov@-�Z@-�^@-��@-�@-�@-��@-%F@,��@,z�@,g8@,"h@+�K@+�[@+�V@+a@++@+�@*��@*�X@*H�@*e@)�@)@)B�@)@@(��@(r�@(-�@'��@'Mj@'1�@'�@&v�@&:*@&#:@&u@%�d@%zx@%F@%�@$Ɇ@$��@$�@$��@$h�@$A�@$	�@#�@#o�@#\)@#P�@#�@"�m@"�1@"z@"@�@"�@!�@!��@!��@!��@!j@!IR@ �5@ �z@ M@ �@��@��@{J@33@�c@҉@�m@��@h
@($@�o@ԕ@�N@�H@x�@5�@0�@0�@&�@�j@j@-�@�@�@��@�K@��@��@,�@�@��@}V@Ta@+k@��@��@��@�z@c�@7L@�@�@oi@'R@�@s@)_@��@�R@p;@#:@��@�j@�@�@��@x�@G�@<6@�@Ĝ@�@�Q@l�@9�@S@�@��@��@Q@($@�@@��@�C@�'@��@��@:�@�@�I@tT@bN@$@�@�@��@X�@�@�B@��@c @4@�)@�@�n@��@��@s�@IR@*0@;@ѷ@�@9X@x@��@��@��@�:@�4@P�@@
�s@
��@
�r@
W�@
@
�@	�@	�^@	��@	��@	�'@	�S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�MA��A�A��A�uA��A��A�!bA�7A�H�A�]/A�{AφYA�|PA�h�A�_�A�d�A�P}A�0�A�%�A�($A�%�A�"�A�'�A��A�
�A���A��A��<A��aA΂AA�u�A�O�A�K)A�3�A͈�A�^�A˹$A��A�<�A�R�A�MA��}A���A���A�n�A��FA�v�A��A�-A��}A��ZA���A���A�+kA��aA��DA��nA��<A���A��-A���A�R�A��A�ܒA�H�A�бA��'A�یA�.IA�PA�&�A�@A��A�7�A���A�l�A�e�A�-CA��rA�B�A�!�A���A}�VAvqvAoV�Ag�AdzAc�Acc�AbƨA`�A^��AZ�vAW��AS \ANbAI�aAE�AD��AD�AB�AA($A?&�A:y�A9�A7�A7��A8A8@OA8�[A8�A6OA5~(A3�A1N<A//�A.��A.�"A.{A-��A-MA,jA,<�A+�A+�XA+-wA*��A*.�A*3�A*9�A)��A)J�A(�fA(��A(��A(.�A'dZA'@OA'�A&sA&/�A%��A%��A%�XA%#�A$��A#��A#zA#X�A"�A!��A!�9A!ĜA"%�A!��A n�A��A��A�A�HA�pAj�A\�A�}A��A��A(�A�HAM�AjA��A��A��Ax�A�A��AC�A��A�A�A�A�9A�!AW�A@OA��A-A��A��A�A!�AخA��A/A-�A�}AFA�A�XA8�A�'AB[A
�NA
d�A	��A	Z�A	M�A��A�0A�cA�A��A�"A�A��AS�AhsA�A�qAF�A{�AVA ��A q@�\�A ,=A z�@���@��:@�;�@��@��.@���@��@�~@��L@��@�=@�ߤ@�N�@�خ@��x@�C@�:*@��@@��	@�v�@��>@�}�@��'@�Ov@�Y�@��|@�b@�n�@�W�@�)�@���@�B�@�K�@�
=@�I@�{@�rG@��B@���@��@�҉@�{@��@�U�@��@�ff@���@��@߂�@ޠ�@ބ�@�tT@ޛ�@ޯO@ݗ�@��@ܷ�@�U2@ۮ�@�!�@��@�N�@���@ـ4@��@؇�@؞@��[@ؠ�@�!�@��j@�\)@�kQ@��9@ՠ�@�6z@�s�@��@Ұ!@���@�
�@�I�@�o�@з�@��@�dZ@���@���@���@͹�@�}�@��P@�J@˪�@�%F@ʍ�@�_@�	@�S�@�	l@Ȟ�@�G@ǋ�@�8�@��E@ƚ@��@Ň�@ę1@Ď�@�خ@��@u@�,=@�	�@��3@�x@�@�ں@�v�@��0@�?}@���@�e�@�V�@�4n@��@��
@��@�W�@��w@�C�@�	l@���@�D�@���@�V@���@� �@��~@�_p@��|@�p;@���@�G�@�+�@��@���@�J@�O@�.I@�@��@��6@�i�@�b@���@�e�@�+@���@��Y@�*�@��T@�@�~�@�=�@�4@���@���@���@��@�G�@��@��@��)@�M@��t@��@�e,@�4�@�@���@���@��@��h@�l�@�C@���@�q�@�?�@��r@���@��~@���@���@�`�@�K^@�'R@��9@��w@���@�W?@�$t@��@�E�@�� @�8�@��j@�C-@��9@��*@�A @���@�~�@�\�@�<�@��@��9@���@�#�@�+@�S@���@�_�@�خ@�o�@�Y@���@�[�@��m@��3@�G@���@�A�@��v@��@�Xy@�7�@���@���@�m]@���@���@�h�@��j@���@�iD@�&@��h@�?�@��@���@�N<@�@��@���@��@��@���@�9�@�S@��!@�y>@�!@���@�8@��u@�$�@��@�u�@��8@���@���@���@�R�@�@��#@��^@���@�Z�@�C@���@�� @�V@�#:@��}@��k@�y�@��@��p@���@��X@��z@�Z@��@��-@��"@�Q�@�o@�@��c@��O@�K^@�+k@�@��)@��K@��@���@��5@���@���@�Ta@�!@��@���@�e,@�A @�S@���@�c�@��@���@���@��=@�a�@�K�@��@��|@���@�j@��@��@��@��@�m]@�&@���@��z@�ff@�r@�f@�@~�B@~��@~v�@~}V@~��@~xl@}��@}�@}��@}G�@|�`@|C-@{{J@{@O@{S@z��@z#:@y�'@y�@yϫ@y�o@y�@yQ�@x��@x�@w��@v�c@uzx@s��@s$t@r�,@rOv@q�D@q��@q��@qzx@p��@p�U@pj@o�@@o'�@n�@n��@n!�@m�S@mw2@mL�@m�@l��@l�$@lV�@l�@k�w@kiD@j��@jn�@j�@i��@i��@i��@i�j@i�t@i��@i@h�@g�+@g�
@g�@g��@g@fxl@e��@e@@dN�@c�F@c�@b�b@bM�@a�@a+@`q@`4n@_"�@^V@]�N@]G�@\��@\/�@[��@[)_@Z�@Z�+@Z;�@Z1�@Z0U@Y��@Y�@X��@X@W��@W��@W~�@W8@Vu%@U�@T�P@T/�@S��@S�w@S��@R��@R4@Q�o@Q�-@Qw2@Q2a@P��@O�m@O_p@N�8@N�@N.�@M�@M[W@M�@M�@L�)@L�O@Lj@K�]@K��@Ks@Kn/@KdZ@KF�@K(@J�@J��@Jn�@JV@J6�@I�@Ix�@I(�@H��@HFt@GX�@F��@F��@F�b@F&�@E�)@F�@F@E@EF@E	l@D�@D�z@D�o@Dy>@DD�@D	�@C��@C�F@C��@CS�@CE9@B�@B@A�@A��@@��@?��@?�
@?�0@?dZ@?33@?(@>�X@>�A@>c @=��@=��@=j@=@@<��@<,=@;|�@;F�@;9�@:�@:v�@9��@9�@9=�@9*0@8�@8��@8u�@84n@8@7��@7n/@6��@6��@6d�@6M�@68�@6	@5��@5�C@5��@5k�@5N<@5!�@4�z@4~@3��@3o�@3;d@3(@2�,@2�b@2i�@28�@1��@1�n@1+�@0�	@0Ɇ@0�@0`�@0"h@/�0@/e�@/@O@/.I@/!-@/�@.�y@.�@.Ov@-�Z@-�^@-��@-�@-�@-��@-%F@,��@,z�@,g8@,"h@+�K@+�[@+�V@+a@++@+�@*��@*�X@*H�@*e@)�@)@)B�@)@@(��@(r�@(-�@'��@'Mj@'1�@'�@&v�@&:*@&#:@&u@%�d@%zx@%F@%�@$Ɇ@$��@$�@$��@$h�@$A�@$	�@#�@#o�@#\)@#P�@#�@"�m@"�1@"z@"@�@"�@!�@!��@!��@!��@!j@!IR@ �5@ �z@ M@ �@��@��@{J@33@�c@҉@�m@��@h
@($@�o@ԕ@�N@�H@x�@5�@0�@0�@&�@�j@j@-�@�@�@��@�K@��@��@,�@�@��@}V@Ta@+k@��@��@��@�z@c�@7L@�@�@oi@'R@�@s@)_@��@�R@p;@#:@��@�j@�@�@��@x�@G�@<6@�@Ĝ@�@�Q@l�@9�@S@�@��@��@Q@($@�@@��@�C@�'@��@��@:�@�@�I@tT@bN@$@�@�@��@X�@�@�B@��@c @4@�)@�@�n@��@��@s�@IR@*0@;@ѷ@�@9X@x@��@��@��@�:@�4@P�@@
�s@
��@
�r@
W�@
@
�@	�@	�^@	��@	��@	�'@	�S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	.�B	.}B	./B	.B	.cB	.�B	/�B	0�B	8�B	D�B	Y�B	}�B	�FB	ĜB	�B	�
B	�B	��B	�hB	��B	��B
oB

�B
�B
�B
�B
!|B
'�B
?�B
P�B
VB
W?B
f�B
}�B
��B
�WB
�yB
�rB
s�B
m)B
�rB
уB
�B
�dB	B�BkB2GBf�BN�B2B9$B%`B
��B
��B
�B
��B
�kB
�`BB7B$B%�B9$B%�BYB
�4B
�?B
�rB
x8B
��B
��B
�TB
��B
��B
L~B
7�B
J�B
Y�B
V9B
MjB
BB
�B	�B	��B	�:B	[qB	J	B	GB	D�B	A�B	@iB	C{B	:�B	=�B	!�B�VB��B�VB��B��B�}B�B�B�B�bB�OB	;B	�B	9�B	U�B	h�B	wLB	�gB	��B	��B	��B	��B	� B	��B	�SB	��B	��B	�QB	�'B	�rB	�.B	�'B	��B	бB	��B	�!B	�B	�B	�GB	��B	�B	�JB	��B	��B
9B
~B
�B
�B
�B
�B
"B
+B
,qB
-�B
-�B
.�B
4nB
:B
FYB
FB
="B
:*B
:�B
B�B
DMB
B�B
B�B
FB
D�B
H1B
PB
Q4B
=�B
OvB
U�B
Z�B
]dB
_;B
d�B
dZB
`�B
_!B
\�B
Z�B
U�B
MjB
DgB
=�B
;�B
5?B
)yB
-�B
;JB
:^B
49B
 BB
!B
%B
$B
($B
'�B
%FB
#�B
#B
"�B
&B
$�B
"hB
�B
)B
�B
&fB
%FB
%�B
)�B
)�B
$�B
(
B
)*B
!�B
�B
�B
�B
	B
B
 B	�	B	�lB	�B	��B	��B
B
�B
;B	�cB	��B	�xB	��B	��B	�B	�nB	��B	��B	��B	�0B	��B	��B	��B	�2B	�B	�B	�-B	�B	��B	�B	�B	��B	�B	��B	�B	�aB	�GB	�|B	��B	�AB	��B	�zB	�TB	��B	�HB	�bB	�B	�B	��B	�B	�2B	�zB	�zB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�_B	�B	��B	�B	��B	�B	�wB	��B	��B	�B	�B	�B	�)B	��B	�B	�B	�B	�iB	�IB	��B	�]B	�]B	�B	�]B	�*B	�B	��B	�B	�QB	��B	�qB	��B	�IB	�/B	�B	��B	�UB	�UB	�B	�'B	�'B	�'B	�[B	��B	�;B	�B	�OB	�/B	�wB	��B	�!B	�B	�B	��B	�3B	�hB	�B	��B	�B	��B	��B	��B	�B	�B	�nB	��B	�B	�ZB	��B	�+B	��B	��B	�ZB	�B	�B	��B	�ZB	��B	��B	��B	��B	�2B	��B	��B	��B	��B	�*B	�xB	��B	�B	�B	��B	��B	�qB	��B	�B	�B	�BB	�(B	�]B	��B	�B	�.B	��B	��B
 4B
 OB
 �B
 4B
 �B
 �B
 �B
 �B
B
�B
�B
AB
�B
�B
�B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
YB
�B
�B
B
�B
EB
�B
�B
_B
EB
zB
�B
1B
1B
KB
KB
_B
�B
zB
zB
�B
�B
EB
�B
�B
	RB

=B
B
~B
PB
�B
B
�B
B
�B
B
�B
�B
�B
(B
�B
B
4B
oB
SB

B
?B
$B
�B
�B
�B
EB
�B
�B
�B
�B
_B
?B
KB
�B
�B
B
B
dB
OB
�B
�B
jB
�B
;B
;B
�B
 \B
 \B
 vB
 vB
 �B
 �B
!-B
!�B
"hB
"�B
# B
#TB
#TB
#�B
#�B
#�B
$@B
$ZB
$tB
$�B
$�B
%B
%FB
%�B
%�B
%�B
%`B
$�B
%zB
$tB
$B
$tB
&B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(sB
(XB
($B
)�B
)�B
)�B
)�B
*KB
*B
*B
+�B
+�B
+�B
,�B
,�B
-]B
-�B
-�B
.B
.}B
.�B
/5B
/B
/OB
/�B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1vB
1vB
1�B
1vB
1�B
2|B
3�B
3�B
3�B
49B
49B
4�B
4nB
4�B
4�B
4�B
4�B
5%B
6FB
6B
6+B
6`B
6`B
6`B
4�B
49B
3�B
3hB
3�B
4�B
6FB
6`B
7LB
8�B
8�B
8�B
8�B
7B
5�B
3�B
3�B
2�B
2�B
2�B
3B
3�B
5�B
6�B
6`B
6+B
5�B
5B
4�B
5B
4�B
5?B
5�B
5�B
6`B
6�B
72B
7�B
8lB
8lB
9�B
:*B
:xB
:�B
;dB
<B
<�B
<�B
="B
=�B
>(B
>�B
>�B
>]B
>�B
>�B
?HB
?cB
?}B
?cB
?�B
@�B
@�B
@�B
@�B
@�B
A B
@�B
@�B
@�B
B'B
A�B
A�B
A�B
B'B
BAB
B�B
B[B
B�B
B�B
B�B
B�B
B�B
C{B
C{B
D3B
D�B
E�B
FYB
GzB
G�B
HKB
H�B
I�B
JXB
JXB
JrB
JrB
K�B
L�B
L�B
MB
MB
MPB
M�B
N"B
N�B
N�B
N�B
O�B
P}B
P�B
Q B
Q B
QNB
QhB
Q�B
RoB
R�B
S[B
TB
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
UgB
V9B
U�B
U�B
U�B
U�B
VmB
W
B
W�B
W�B
W�B
XB
X+B
XB
W�B
W�B
XyB
X�B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
ZkB
Z�B
ZQB
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
]�B
^B
^B
^B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
_�B
`\B
aHB
abB
a|B
a�B
a�B
a�B
a�B
b4B
bNB
bNB
bNB
bhB
c B
c�B
c�B
dB
d&B
dZB
dtB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
fB
ffB
ffB
f�B
gB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
iB
iB
i*B
iB
i�B
jB
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
j�B
k6B
kB
k�B
k�B
lB
lB
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p!B
p!B
p�B
p�B
p�B
qB
q[B
qvB
qvB
q�B
q�B
rB
r-B
raB
rGB
r|B
r|B
r�B
sB
s�B
s�B
tB
tTB
t�B
t�B
u%B
u?B
u?B
u?B
u�B
u�B
vFB
vFB
v`B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
xB
xB
w�B
x8B
xRB
xRB
x�B
x�B
y$B
y>B
y�B
y�B
y�B
zB
y�B
y�B
z^B
z^B
zxB
z�B
{B
{B
{�B
|B
|PB
|jB
|�B
|�B
}VB
}VB
}qB
}qB
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~�B
B
�B
�B
�B
�B
�B
�4B
��B
��B
��B
� B
� B
� B
� B
� B
�B
�UB
��B
��B
��B
�B
�AB
�[B
�AB
��B
��B
�GB
�{B
��B
��B
�MB
�MB
�gB
��B
��B
��B
��B
��B
��B
�B
�9B
��B
��B
�B
�%B
�YB
�tB
�YB
�tB
��B
��B
�+B
�EB
�_B
��B
��B
��B
�1B
�1B
�KB
�fB
�fB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	.�B	.}B	./B	.B	.cB	.�B	/�B	0�B	8�B	D�B	Y�B	}�B	�FB	ĜB	�B	�
B	�B	��B	�hB	��B	��B
oB

�B
�B
�B
�B
!|B
'�B
?�B
P�B
VB
W?B
f�B
}�B
��B
�WB
�yB
�rB
s�B
m)B
�rB
уB
�B
�dB	B�BkB2GBf�BN�B2B9$B%`B
��B
��B
�B
��B
�kB
�`BB7B$B%�B9$B%�BYB
�4B
�?B
�rB
x8B
��B
��B
�TB
��B
��B
L~B
7�B
J�B
Y�B
V9B
MjB
BB
�B	�B	��B	�:B	[qB	J	B	GB	D�B	A�B	@iB	C{B	:�B	=�B	!�B�VB��B�VB��B��B�}B�B�B�B�bB�OB	;B	�B	9�B	U�B	h�B	wLB	�gB	��B	��B	��B	��B	� B	��B	�SB	��B	��B	�QB	�'B	�rB	�.B	�'B	��B	бB	��B	�!B	�B	�B	�GB	��B	�B	�JB	��B	��B
9B
~B
�B
�B
�B
�B
"B
+B
,qB
-�B
-�B
.�B
4nB
:B
FYB
FB
="B
:*B
:�B
B�B
DMB
B�B
B�B
FB
D�B
H1B
PB
Q4B
=�B
OvB
U�B
Z�B
]dB
_;B
d�B
dZB
`�B
_!B
\�B
Z�B
U�B
MjB
DgB
=�B
;�B
5?B
)yB
-�B
;JB
:^B
49B
 BB
!B
%B
$B
($B
'�B
%FB
#�B
#B
"�B
&B
$�B
"hB
�B
)B
�B
&fB
%FB
%�B
)�B
)�B
$�B
(
B
)*B
!�B
�B
�B
�B
	B
B
 B	�	B	�lB	�B	��B	��B
B
�B
;B	�cB	��B	�xB	��B	��B	�B	�nB	��B	��B	��B	�0B	��B	��B	��B	�2B	�B	�B	�-B	�B	��B	�B	�B	��B	�B	��B	�B	�aB	�GB	�|B	��B	�AB	��B	�zB	�TB	��B	�HB	�bB	�B	�B	��B	�B	�2B	�zB	�zB	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�_B	�B	��B	�B	��B	�B	�wB	��B	��B	�B	�B	�B	�)B	��B	�B	�B	�B	�iB	�IB	��B	�]B	�]B	�B	�]B	�*B	�B	��B	�B	�QB	��B	�qB	��B	�IB	�/B	�B	��B	�UB	�UB	�B	�'B	�'B	�'B	�[B	��B	�;B	�B	�OB	�/B	�wB	��B	�!B	�B	�B	��B	�3B	�hB	�B	��B	�B	��B	��B	��B	�B	�B	�nB	��B	�B	�ZB	��B	�+B	��B	��B	�ZB	�B	�B	��B	�ZB	��B	��B	��B	��B	�2B	��B	��B	��B	��B	�*B	�xB	��B	�B	�B	��B	��B	�qB	��B	�B	�B	�BB	�(B	�]B	��B	�B	�.B	��B	��B
 4B
 OB
 �B
 4B
 �B
 �B
 �B
 �B
B
�B
�B
AB
�B
�B
�B
-B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
YB
�B
�B
B
�B
EB
�B
�B
_B
EB
zB
�B
1B
1B
KB
KB
_B
�B
zB
zB
�B
�B
EB
�B
�B
	RB

=B
B
~B
PB
�B
B
�B
B
�B
B
�B
�B
�B
(B
�B
B
4B
oB
SB

B
?B
$B
�B
�B
�B
EB
�B
�B
�B
�B
_B
?B
KB
�B
�B
B
B
dB
OB
�B
�B
jB
�B
;B
;B
�B
 \B
 \B
 vB
 vB
 �B
 �B
!-B
!�B
"hB
"�B
# B
#TB
#TB
#�B
#�B
#�B
$@B
$ZB
$tB
$�B
$�B
%B
%FB
%�B
%�B
%�B
%`B
$�B
%zB
$tB
$B
$tB
&B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(sB
(XB
($B
)�B
)�B
)�B
)�B
*KB
*B
*B
+�B
+�B
+�B
,�B
,�B
-]B
-�B
-�B
.B
.}B
.�B
/5B
/B
/OB
/�B
/�B
/�B
0!B
0UB
0�B
0�B
1AB
1vB
1vB
1�B
1vB
1�B
2|B
3�B
3�B
3�B
49B
49B
4�B
4nB
4�B
4�B
4�B
4�B
5%B
6FB
6B
6+B
6`B
6`B
6`B
4�B
49B
3�B
3hB
3�B
4�B
6FB
6`B
7LB
8�B
8�B
8�B
8�B
7B
5�B
3�B
3�B
2�B
2�B
2�B
3B
3�B
5�B
6�B
6`B
6+B
5�B
5B
4�B
5B
4�B
5?B
5�B
5�B
6`B
6�B
72B
7�B
8lB
8lB
9�B
:*B
:xB
:�B
;dB
<B
<�B
<�B
="B
=�B
>(B
>�B
>�B
>]B
>�B
>�B
?HB
?cB
?}B
?cB
?�B
@�B
@�B
@�B
@�B
@�B
A B
@�B
@�B
@�B
B'B
A�B
A�B
A�B
B'B
BAB
B�B
B[B
B�B
B�B
B�B
B�B
B�B
C{B
C{B
D3B
D�B
E�B
FYB
GzB
G�B
HKB
H�B
I�B
JXB
JXB
JrB
JrB
K�B
L�B
L�B
MB
MB
MPB
M�B
N"B
N�B
N�B
N�B
O�B
P}B
P�B
Q B
Q B
QNB
QhB
Q�B
RoB
R�B
S[B
TB
TFB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
UgB
V9B
U�B
U�B
U�B
U�B
VmB
W
B
W�B
W�B
W�B
XB
X+B
XB
W�B
W�B
XyB
X�B
XyB
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
ZkB
Z�B
ZQB
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[�B
[�B
\B
\)B
\xB
\�B
\�B
\�B
]�B
^B
^B
^B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
_�B
`\B
aHB
abB
a|B
a�B
a�B
a�B
a�B
b4B
bNB
bNB
bNB
bhB
c B
c�B
c�B
dB
d&B
dZB
dtB
d�B
d�B
d�B
e,B
e`B
e�B
e�B
fB
ffB
ffB
f�B
gB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
iB
iB
i*B
iB
i�B
jB
i�B
jB
jKB
jB
j�B
j�B
j�B
j�B
j�B
k6B
kB
k�B
k�B
lB
lB
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p!B
p!B
p�B
p�B
p�B
qB
q[B
qvB
qvB
q�B
q�B
rB
r-B
raB
rGB
r|B
r|B
r�B
sB
s�B
s�B
tB
tTB
t�B
t�B
u%B
u?B
u?B
u?B
u�B
u�B
vFB
vFB
v`B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
xB
xB
w�B
x8B
xRB
xRB
x�B
x�B
y$B
y>B
y�B
y�B
y�B
zB
y�B
y�B
z^B
z^B
zxB
z�B
{B
{B
{�B
|B
|PB
|jB
|�B
|�B
}VB
}VB
}qB
}qB
}�B
}�B
}�B
}�B
}�B
~(B
~BB
~�B
B
�B
�B
�B
�B
�B
�4B
��B
��B
��B
� B
� B
� B
� B
� B
�B
�UB
��B
��B
��B
�B
�AB
�[B
�AB
��B
��B
�GB
�{B
��B
��B
�MB
�MB
�gB
��B
��B
��B
��B
��B
��B
�B
�9B
��B
��B
�B
�%B
�YB
�tB
�YB
�tB
��B
��B
�+B
�EB
�_B
��B
��B
��B
�1B
�1B
�KB
�fB
�fB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221229214145  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221229214147  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221229214148  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221229214148                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221229214149  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221229214149  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221230012902                      G�O�G�O�G�O�                