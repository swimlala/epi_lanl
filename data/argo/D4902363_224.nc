CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-30T00:35:20Z creation;2018-03-30T00:35:26Z conversion to V3.1;2019-12-19T07:46:02Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180330003520  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_224                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�W5�W�1   @�W6�8�@:�u�dk�_p1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@<��@���@�ffA ��A ��A@��A`��A�ffA�ffA�ffA�ffA���A�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D		�D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D|��D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��fD�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>fD���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D;fD��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�>fD�~fD���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D�њD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��FA��9A���A��DA�O�A�9XA�oA��A�jA�+A�7LA��A�^5A�?}A��FA� �A�A��\A�t�A�;dA��A���A��hA��PA��A���A��jA��!A��A�bNA�S�A�VA�M�A�1'A� �A��A���A��^A�t�A�O�A�1'A��A�  A���A�$�A�1A�JA���A�$�A�~�A��PA���A�bA���A�  A�ffA� �A��A��A���A�;dA��A�7LA�  A�ȴA�x�A�%A��FA�~�A�A��A��-A��9A��A�1'A�5?A���A���A��^A�33A�
=A��HA�v�A��A��A{%Ax�9Ax  Aw|�Av�Av�\Av^5Au;dAt��AtZAtJAs�As�Ar��Aq�-An��AnVAn�Am��Am|�AmO�Al�\Al�Ai��Ahn�Ag�Af�Ad�Aa33A`�\A`VA`�A_��A]K�A[+AW��AWO�AVȴAV�DAVffAUC�AT$�AS��ASp�AR�`AQ��APjAMƨAL�!AL9XAL-AK�
AI�AF�/AE
=ACO�A@~�A>�A>�A=�-A=p�A=/A<�A<��A<A�A;�mA;�A8�RA8r�A7+A5�A4ȴA4^5A4bA3�^A333A2��A2��A2VA25?A2$�A2�A2  A1�wA1VA0A/O�A.��A.�RA.��A.�\A.r�A.I�A-x�A,5?A+��A+?}A)��A)XA)�A(�A'�-A'"�A'VA&A%
=A$5?A#��A#hsA"��A"I�A v�A�9AAXAz�AJA��A`BAoA�AM�A9XA�AbA1At�A�A��A�+Ar�A(�AƨAhsA^5AĜA{A��AXA  AZA�jA��AdZA�+A�-A
�uA	�#A	
=AJAXA��A��A-A�7A��A~�A�7@�|�@�@��`@��@��@�@�Z@���@�~�@�O�@�$�@�`B@㝲@���@���@��
@��H@�-@ݲ-@�O�@�j@�+@�$�@؃@׮@ם�@�|�@�o@���@և+@�V@�@�?}@���@�~�@�bN@ϕ�@�\)@��@���@Ο�@Ο�@�^5@��/@˝�@˝�@�l�@�\)@�\)@�\)@�^5@�hs@���@Ǿw@�p�@Ý�@�~�@�Q�@��@�@�/@�A�@���@�ȴ@�-@��T@�X@��@�z�@�\)@��`@���@��7@��m@��^@���@�j@�(�@�  @�  @��@��@�S�@���@�`B@��@��@��@���@�-@���@��@���@�bN@�  @��w@�K�@���@�~�@�-@��T@��@�%@��u@��@���@���@�v�@�M�@�=q@�-@���@�p�@�X@��@�V@���@�Ĝ@�z�@��m@���@�V@��@���@�?}@���@��@��@�j@�l�@�v�@�/@�r�@��@�b@��@��@���@�dZ@�33@��y@���@�{@��#@���@�?}@�%@��@�bN@� �@��@�l�@�@��@��R@��@�/@��/@��@��@�j@�9X@���@�l�@�@���@�ȴ@���@�V@�J@�x�@�Ĝ@�z�@�I�@��m@��@�l�@�;d@��@��y@���@�v�@�n�@�^5@�$�@��@�@�&�@��@�Ĝ@��j@��j@��j@��9@���@���@��@�bN@�I�@��@���@�dZ@�;d@�
=@���@�~�@�E�@��@��^@�x�@�p�@��/@��j@��@�9X@�;@~��@~5?@}��@}?}@}�@|�/@|Z@{��@{dZ@{33@{@z��@z~�@z-@zJ@y�#@yhs@x��@x�u@xr�@xbN@x �@w�;@w��@w+@v�@v�+@v$�@u�@u�T@u��@u��@u��@u�-@u�h@u�@t(�@s��@s��@sC�@s"�@r�@r��@r=q@q��@qX@qG�@qG�@qG�@q7L@q&�@p��@pĜ@p��@p�@pbN@o�@nȴ@nȴ@nff@n$�@m@m`B@m�@l�j@lI�@k��@k�m@k��@kƨ@k��@kt�@k33@j�\@jM�@jM�@i�@h�@hQ�@h1'@g�@g�@f��@e�-@d��@d�@c�m@c�F@c��@c@b��@b��@b-@a��@ahs@a%@`��@`Q�@_�@_;d@^ȴ@^ff@^$�@]��@\��@\j@\(�@\�@\1@[�m@[ƨ@[t�@Z��@ZM�@Z�@Y�@Y��@Y�^@Y��@Y��@Y7L@Y%@X�`@X��@XQ�@W�@V�y@V5?@U�@U��@U�-@U?}@T�/@T��@Tj@T9X@T�@T1@S�F@S"�@R��@Rn�@Q��@Q��@Qx�@Qhs@Qhs@QX@QG�@Q%@P��@PA�@Pb@O�@O��@O+@N�+@N{@M�h@M?}@MV@L�@L�j@Lz�@LI�@LI�@L�@K�m@K��@KdZ@KC�@KC�@Ko@J��@JM�@I�@I7L@I%@HĜ@H�@G�w@G�@F�y@Fȴ@F��@F{@E�T@E�-@E�h@E`B@D�@D�j@D��@Dz�@DZ@D(�@C�m@C��@CdZ@B�@BM�@A�@A��@Ax�@Ahs@AX@A7L@A%@@��@@��@@Ĝ@@��@@r�@@  @?�@?l�@?;d@>�@>v�@>ff@>V@>$�@=�@=�@=O�@=?}@=/@<��@<��@;ƨ@;33@:��@:�!@:�\@:~�@:~�@:�@9��@9�@9��@9�^@9�7@9hs@9X@97L@9&�@9%@8Ĝ@8�9@8�u@8b@7�P@7+@6��@6ȴ@6��@6v�@6ff@6ff@6V@6@5��@5@5@5@5�-@5�@5V@4��@4��@3�m@3�
@3��@3dZ@3S�@333@3@2�H@2��@2^5@2�@1��@/�@/\)@/;d@/�@.�R@.v�@.E�@-�@-�-@-��@-�h@-�@-�@-`B@-`B@-O�@-?}@-�@,��@,j@,(�@+�
@+ƨ@+��@+�@+33@*��@*��@*M�@)�@)x�@)�@(��@(Ĝ@(�@(�@(bN@( �@'l�@'�@&�y@&�@&�@&�R@&v�@&$�@&@%�-@%�@%p�@%`B@%�@$��@$Z@#�m@#��@#S�@#"�@"��@"~�@"^5@"M�@"M�@"=q@"-@"-@"�@"J@!�#@!x�@!7L@!%@ ��@ ��@ �u@ 1'@   @�@�@�y@�R@�+@�+@v�@�T@��@�h@p�@?}@V@�@��@�D@Z@(�@1@�m@�@"�@~�@�#@G�@&�@Ĝ@��@r�@bN@bN@Q�@Q�@  @��@��@|�@;d@�@
=@
=@��@�y@ȴ@��@v�@5?@{@�T@@�-@��@�@�@O�@?}@?}@?}@�@�j@I�@��@�
@�F@��@��@dZ@o@�@�H@~�@�@J@��@�@�#@��@X@7L@�@%@Ĝ@�@bN@b@�w@��@|�@|�@l�@K�@ȴ@E�@@�h@O�@/@�@��@��@I�@��@�F@��@��@dZ@C�@C�@C�@33@o@
��@
�\@
^5@
M�@	��@	��@	G�@�`@�9@�u@r�@bN@Q�@1'@  @�w@��@��@|�@;d@;d@;d@;d@�@�y@�R@��@V@$�@@�T@@�h@O�@/@�@�/@�j@�@Z@1@�
@��@dZ@"�@o@��@��@~�@^5@�@��@��@�7@hs@X@G�@G�@�@ ��@ �`@ �`11111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��FA��9A���A��DA�O�A�9XA�oA��A�jA�+A�7LA��A�^5A�?}A��FA� �A�A��\A�t�A�;dA��A���A��hA��PA��A���A��jA��!A��A�bNA�S�A�VA�M�A�1'A� �A��A���A��^A�t�A�O�A�1'A��A�  A���A�$�A�1A�JA���A�$�A�~�A��PA���A�bA���A�  A�ffA� �A��A��A���A�;dA��A�7LA�  A�ȴA�x�A�%A��FA�~�G�O�G�O�A��-A��9A��A�1'A�5?A���A���A��^A�33A�
=A��HA�v�A��A��A{%Ax�9Ax  Aw|�Av�Av�\Av^5Au;dAt��AtZAtJAs�As�Ar��Aq�-An��AnVAn�Am��Am|�AmO�Al�\Al�Ai��Ahn�Ag�Af�Ad�Aa33A`�\A`VA`�A_��A]K�A[+AW��AWO�AVȴAV�DAVffAUC�AT$�AS��ASp�AR�`AQ��APjAMƨAL�!AL9XAL-AK�
AI�AF�/AE
=ACO�A@~�A>�A>�A=�-A=p�A=/A<�A<��A<A�A;�mA;�A8�RA8r�A7+A5�A4ȴA4^5A4bA3�^A333A2��A2��A2VA25?A2$�A2�A2  A1�wA1VA0A/O�A.��A.�RA.��A.�\A.r�A.I�A-x�A,5?A+��A+?}A)��A)XA)�A(�A'�-A'"�A'VA&A%
=A$5?A#��A#hsA"��A"I�A v�A�9AAXAz�AJA��A`BAoA�AM�A9XA�AbA1At�A�A��A�+Ar�A(�AƨAhsA^5AĜA{A��AXA  AZA�jA��AdZA�+A�-A
�uA	�#A	
=AJAXA��A��A-A�7A��A~�A�7@�|�@�@��`@��@��@�@�Z@���@�~�@�O�@�$�@�`B@㝲@���@���@��
@��H@�-@ݲ-@�O�@�j@�+@�$�@؃@׮@ם�@�|�@�o@���@և+@�V@�@�?}@���@�~�@�bN@ϕ�@�\)@��@���@Ο�@Ο�@�^5@��/@˝�@˝�@�l�@�\)@�\)@�\)@�^5@�hs@���@Ǿw@�p�@Ý�@�~�@�Q�@��@�@�/@�A�@���@�ȴ@�-@��T@�X@��@�z�@�\)@��`@���@��7@��m@��^@���@�j@�(�@�  @�  @��@��@�S�@���@�`B@��@��@��@���@�-@���@��@���@�bN@�  @��w@�K�@���@�~�@�-@��T@��@�%@��u@��@���@���@�v�@�M�@�=q@�-@���@�p�@�X@��@�V@���@�Ĝ@�z�@��m@���@�V@��@���@�?}@���@��@��@�j@�l�@�v�@�/@�r�@��@�b@��@��@���@�dZ@�33@��y@���@�{@��#@���@�?}@�%@��@�bN@� �@��@�l�@�@��@��R@��@�/@��/@��@��@�j@�9X@���@�l�@�@���@�ȴ@���@�V@�J@�x�@�Ĝ@�z�@�I�@��m@��@�l�@�;d@��@��y@���@�v�@�n�@�^5@�$�@��@�@�&�@��@�Ĝ@��j@��j@��j@��9@���@���@��@�bN@�I�@��@���@�dZ@�;d@�
=@���@�~�@�E�@��@��^@�x�@�p�@��/@��j@��@�9X@�;@~��@~5?@}��@}?}@}�@|�/@|Z@{��@{dZ@{33@{@z��@z~�@z-@zJ@y�#@yhs@x��@x�u@xr�@xbN@x �@w�;@w��@w+@v�@v�+@v$�@u�@u�T@u��@u��@u��@u�-@u�h@u�@t(�@s��@s��@sC�@s"�@r�@r��@r=q@q��@qX@qG�@qG�@qG�@q7L@q&�@p��@pĜ@p��@p�@pbN@o�@nȴ@nȴ@nff@n$�@m@m`B@m�@l�j@lI�@k��@k�m@k��@kƨ@k��@kt�@k33@j�\@jM�@jM�@i�@h�@hQ�@h1'@g�@g�@f��@e�-@d��@d�@c�m@c�F@c��@c@b��@b��@b-@a��@ahs@a%@`��@`Q�@_�@_;d@^ȴ@^ff@^$�@]��@\��@\j@\(�@\�@\1@[�m@[ƨ@[t�@Z��@ZM�@Z�@Y�@Y��@Y�^@Y��@Y��@Y7L@Y%@X�`@X��@XQ�@W�@V�y@V5?@U�@U��@U�-@U?}@T�/@T��@Tj@T9X@T�@T1@S�F@S"�@R��@Rn�@Q��@Q��@Qx�@Qhs@Qhs@QX@QG�@Q%@P��@PA�@Pb@O�@O��@O+@N�+@N{@M�h@M?}@MV@L�@L�j@Lz�@LI�@LI�@L�@K�m@K��@KdZ@KC�@KC�@Ko@J��@JM�@I�@I7L@I%@HĜ@H�@G�w@G�@F�y@Fȴ@F��@F{@E�T@E�-@E�h@E`B@D�@D�j@D��@Dz�@DZ@D(�@C�m@C��@CdZ@B�@BM�@A�@A��@Ax�@Ahs@AX@A7L@A%@@��@@��@@Ĝ@@��@@r�@@  @?�@?l�@?;d@>�@>v�@>ff@>V@>$�@=�@=�@=O�@=?}@=/@<��@<��@;ƨ@;33@:��@:�!@:�\@:~�@:~�@:�@9��@9�@9��@9�^@9�7@9hs@9X@97L@9&�@9%@8Ĝ@8�9@8�u@8b@7�P@7+@6��@6ȴ@6��@6v�@6ff@6ff@6V@6@5��@5@5@5@5�-@5�@5V@4��@4��@3�m@3�
@3��@3dZ@3S�@333@3@2�H@2��@2^5@2�@1��@/�@/\)@/;d@/�@.�R@.v�@.E�@-�@-�-@-��@-�h@-�@-�@-`B@-`B@-O�@-?}@-�@,��@,j@,(�@+�
@+ƨ@+��@+�@+33@*��@*��@*M�@)�@)x�@)�@(��@(Ĝ@(�@(�@(bN@( �@'l�@'�@&�y@&�@&�@&�R@&v�@&$�@&@%�-@%�@%p�@%`B@%�@$��@$Z@#�m@#��@#S�@#"�@"��@"~�@"^5@"M�@"M�@"=q@"-@"-@"�@"J@!�#@!x�@!7L@!%@ ��@ ��@ �u@ 1'@   @�@�@�y@�R@�+@�+@v�@�T@��@�h@p�@?}@V@�@��@�D@Z@(�@1@�m@�@"�@~�@�#@G�@&�@Ĝ@��@r�@bN@bN@Q�@Q�@  @��@��@|�@;d@�@
=@
=@��@�y@ȴ@��@v�@5?@{@�T@@�-@��@�@�@O�@?}@?}@?}@�@�j@I�@��@�
@�F@��@��@dZ@o@�@�H@~�@�@J@��@�@�#@��@X@7L@�@%@Ĝ@�@bN@b@�w@��@|�@|�@l�@K�@ȴ@E�@@�h@O�@/@�@��@��@I�@��@�F@��@��@dZ@C�@C�@C�@33@o@
��@
�\@
^5@
M�@	��@	��@	G�@�`@�9@�u@r�@bN@Q�@1'@  @�w@��@��@|�@;d@;d@;d@;d@�@�y@�R@��@V@$�@@�T@@�h@O�@/@�@�/@�j@�@Z@1@�
@��@dZ@"�@o@��@��@~�@^5@�@��@��@�7@hs@X@G�@G�@�@ ��@ �`@ �`11111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��B�}B�qB�RB�^B�9B�B��B�oB��B�B�XB�RB�B�B�B�?B�?B�'B�B�B�3B�3B�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�\B|�BffBz�B� Bx�BgmBK�BA�B:^B9XB.B�B"�B�B�BuBB�fB��B�/B��BĜB��B�BE�BJ�B/B
��B
�qB
�-B
ɺB
�dB
�9B
��B
��B
��B
��B
��B
�B
T�B
K�B
�B
)�B
49B
9XB
5?B
33B
49B
(�B
,B
/B
,B
&�B
#�B
�B
JB	�B
bB
oB
VB

=B
%B	��B	�B	�B	��B	�B	ǮB	�B	��B	�3B	�RB	�!B	��B	z�B	v�B	]/B	~�B	|�B	� B	z�B	m�B	cTB	m�B	hsB	_;B	R�B	D�B	49B	9XB	E�B	F�B	=qB	&�B	�B	{B	
=B��B��B��B	  B	B��B��B��B��B�B�ZB��B�;B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB��B�^B�qB�qBÖB��B��B�wB�jB�9B�B�B�B��B��B��B��B��B��B��B�uB�PB�hB�VB�oB�=B�%Bx�Bq�B{�By�Bx�B{�B{�B{�B{�Bx�Bw�B{�Bz�Bz�Bx�Bs�Bq�Bu�Bv�Bt�Bq�Bm�BiyBaHBXB^5B_;B[#BM�BA�BE�BG�BM�BF�BD�B?}BB�B?}B<jB;dB49B33B<jB:^B8RB5?B.B#�B1'B33B0!B/B"�B�B�B�B\BhB!�B�B�B�B�B�B!�B!�B!�B�B�B�B�B�B#�B#�B!�B!�B!�B �B�B�B�B�B�B�B#�B#�B#�B#�B#�B �B�B�B&�B&�B&�B%�B#�B�B�B�B�B{B�B�B�B�B,B.B,B0!B0!B2-B49B49B49B2-B-B&�B+B6FB6FB6FBD�BI�BL�BN�BN�BM�BL�BK�BI�BG�BH�BVB]/B\)B[#B\)B_;BaHBdZBdZBffBffBgmBl�Bl�Bm�Bl�Bm�Bn�Bl�Bs�By�B{�B}�B~�B~�B}�B}�B�B�B�B�B�B�B�B�B�1B�PB�uB��B��B��B��B��B��B��B��B�B�-B�3B�3B�3B�-B�!B�3B�3B�9B�9B�RB�^B�^B�wB�}B��BĜBƨBŢBȴB��B��BɺB��B��B�B�B�/B�5B�5B�fB�yB�B�B�B�B�B�B��B��B��B��B	B	%B	+B		7B	
=B	JB	\B	hB	hB	hB	uB	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	%�B	%�B	%�B	$�B	&�B	)�B	+B	.B	0!B	1'B	33B	5?B	8RB	:^B	?}B	A�B	F�B	D�B	K�B	K�B	L�B	N�B	P�B	VB	ZB	\)B	^5B	_;B	_;B	aHB	e`B	ffB	gmB	hsB	iyB	l�B	m�B	n�B	n�B	o�B	r�B	s�B	s�B	s�B	t�B	u�B	t�B	w�B	y�B	{�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�%B	�JB	�PB	�VB	�hB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�9B	�?B	�?B	�?B	�LB	�jB	�dB	�XB	�qB	�wB	�wB	�}B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�5B	�;B	�BB	�HB	�HB	�HB	�TB	�fB	�mB	�mB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
1B
1B
+B
+B
+B
1B
	7B
DB
JB
DB
DB
JB
VB
\B
\B
VB
bB
hB
hB
hB
hB
uB
uB
uB
uB
{B
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
&�B
'�B
'�B
&�B
'�B
(�B
+B
+B
+B
,B
,B
,B
,B
+B
,B
.B
.B
.B
-B
-B
,B
-B
-B
,B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
/B
-B
1'B
49B
49B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
9XB
:^B
:^B
<jB
<jB
<jB
=qB
<jB
<jB
;dB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
F�B
I�B
I�B
I�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
N�B
P�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
T�B
VB
T�B
VB
VB
VB
VB
T�B
T�B
VB
W
B
XB
XB
XB
XB
W
B
XB
YB
XB
XB
ZB
ZB
ZB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
\)B
[#B
\)B
]/B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
cTB
bNB
bNB
bNB
cTB
dZB
e`B
e`B
e`B
ffB
ffB
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
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
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��B��B��B��B��B��B��B�hB�2B��B�CB��B��B�IB�CB��B��B��B��B��B��B�3B�MB�iB��B�fB�5B�qB�WB�)B�B�)B�0B�*B�B�LB�bB�HB�B�B�B�B�.B~�BiB|�B�BzBiBM�BCGB<B:DB/�BB#TB;B1BBmB�*BևBݲBӏBňB��B��BJ�BNVG�O�G�O�B
�'B
�FB
��B
��B
��B
�B
��B
��B
�!B
�SB
��B
YKB
NVB
"�B
,"B
5B
9�B
5�B
3�B
4�B
*0B
,�B
/iB
,�B
'�B
$tB
jB
"B	�B
}B
�B
�B

�B
�B	��B	�B	ںB	ӏB	��B	ɆB	�'B	��B	��B	��B	��B	��B	}�B	yrB	`�B	�B	}�B	�iB	{dB	o B	d�B	nB	iB	`BB	T�B	F�B	72B	:�B	F%B	GB	>�B	*KB	yB	�B	�B�B��B��B	 �B	oB�cB�qB�DB�`B�iB�BյB��B��B��BбBՁB�{B�oBΊB�BB�.B�.B�B�B�B�B�KBB��B�]B�BðB��B��B��B��B�tB��B��B��B��B�RB�sB��B��B�xB�B��B��B�oB�(B��B�)B�_B{JBs�B|�Bz�By�B|�B|jB|PB|jByrBxRB|B{Bz�By$BtnBrGBvBv�BuBrBnIBjKBb�BZB_!B_�B[�BO�BC�BG�BH�BN�BG�BE�B@�BC�B@�B=�B<�B6FB4�B="B;JB9>B6FB/�B&B2B4B1[B0oB%B!�B�B�BoB@B"hBB�BpB �BpB"NB"4B"4B�B�B~B�B;B#�B$B"4B"B!�B!B!BqB�B�B�B!B#�B$B$B#�B#�B!HB�B�B'B'B&�B%�B$&BpB~B BB�BB�B�BBB,qB.�B,�B0�B0�B2�B4�B4�B4�B2�B.B(�B,�B72B7�B7�BEBJ	BL�BOBN�BNBMBL0BJXBH�BI�BVmB]/B\]B[�B\�B_�Ba�Bd�Bd�Bf�Bf�Bg�Bl�Bl�Bm�Bl�Bm�Bo BmCBtBy�B|B~(BB.B~(B~BB� B� B�B�SB�mB�mB��B��B��B��B��B��B��B��B��B��B�sB�~B��B�cB�GB�3B�MB�3B�GB�oB�hB��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�XB�dB�2B�1B�QB�dBބBޞB�B�B��B�B��B��B��B�-B�?B�B�B�BB	UB	?B	_B		lB	
XB	dB	\B	�B	hB	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	%�B	%�B	%�B	%B	'B	*KB	+QB	.IB	0UB	1[B	3hB	5�B	8�B	:�B	?�B	A�B	F�B	EB	K�B	K�B	MB	O(B	Q4B	VSB	ZQB	\CB	^OB	_pB	_pB	a|B	e`B	f�B	gmB	h�B	i�B	l�B	m�B	n�B	n�B	o�B	r�B	s�B	s�B	s�B	t�B	u�B	uB	w�B	zB	|B	~B	B	�B	� B	�B	�B	�-B	�aB	�tB	�JB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�0B	�B	�CB	�OB	�'B	�3B	�9B	�9B	�9B	�ZB	�tB	�tB	�fB	�jB	��B	��B	�qB	��B	��B	��B	��B	��B	�	B	�B	��B	� B	� B	�B	�B	�$B	�+B	�1B	�+B	�KB	�#B	�kB	�qB	�OB	�pB	�vB	�bB	�bB	�bB	�B	�fB	�mB	�B	�mB	�mB	�B	�B	�B	�B	�B	�B	�qB	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�"B	�B	�B	��B	�<B	�.B
;B
'B
-B
3B
B
9B
B
?B
%B
%B
?B
?B
+B
KB
1B
EB
_B
_B
fB
	lB
DB
dB
xB
�B
~B
pB
\B
vB
�B
}B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
B
 �B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'B
'�B
&�B
'B
(
B
(
B
'B
(
B
)B
+B
+B
+B
,"B
,B
,B
,B
+B
,"B
-�B
.B
.B
-)B
-)B
,=B
-)B
-)B
,=B
/B
/B
/5B
0!B
0;B
0!B
0!B
0;B
0;B
0UB
/iB
-�B
1AB
4TB
4TB
4TB
5ZB
5ZB
5ZB
6`B
72B
7LB
72B
7LB
7LB
7LB
72B
7fB
7LB
6zB
7fB
8lB
8lB
9XB
9XB
9XB
9rB
9�B
:xB
:xB
9�B
:�B
:xB
<�B
<jB
<jB
=qB
<�B
<�B
;�B
=qB
>wB
?}B
?}B
?}B
?�B
?}B
?�B
?�B
@�B
A�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
F�B
F�B
G�B
G�B
G�B
F�B
I�B
I�B
I�B
I�B
H�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
LB
L�B
OB
Q B
O�B
P�B
Q B
Q�B
Q�B
Q�B
Q�B
P�B
RB
R�B
R�B
SB
TB
S�B
S�B
S�B
S�B
TB
TB
TB
S�B
TB
TB
T�B
T�B
VB
T�B
U�B
T�B
U�B
VB
VB
VB
U2B
U2B
VB
W$B
XB
W�B
X+B
XB
W$B
X+B
Y1B
XEB
X+B
ZB
ZB
ZB
ZB
Y1B
Z7B
[=B
[#B
[=B
[#B
[=B
[=B
[=B
[#B
\CB
]IB
]/B
]/B
\CB
[=B
\]B
]dB
_VB
_VB
_VB
_VB
`\B
`\B
`\B
`BB
abB
aHB
bNB
bNB
bNB
bNB
bNB
bhB
bhB
abB
bhB
cnB
bhB
bhB
b�B
cnB
dtB
ezB
ezB
e`B
ffB
fLB
ezB
f�B
f�B
f�B
gmB
g�B
gmB
gmB
hsB
hsB
g�B
g�B
g�B
h�B
hsB
h�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
jB
jB
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804030042012018040300420120180403004201201806221239452018062212394520180622123945201804271405012018042714050120180427140501  JA  ARFMdecpA19c                                                                20180330093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180330003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180330003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180330003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180330003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180330003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180330003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180330003525  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180330003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180330003525  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180330003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180330003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20180330010229                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180330153330  CV  JULD            G�O�G�O�F¹�                JM  ARCAJMQC2.0                                                                 20180402154201  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180402154201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180427050501  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033945  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                