CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-05T00:35:15Z creation;2018-10-05T00:35:20Z conversion to V3.1;2019-12-19T07:31:11Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
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
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005003515  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_287                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؆vbj�1   @؆wUUU�@9�\�����d:�q�i�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BXffB_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A?33A`��A�ffA�ffA�ffA�ffA�ffA�33A�ffA�ffB 33B33B33B33B 33B(33B033B8��B@��BH33BP33BX��B_��Bg��Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D+��D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D3	�D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw��Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~fD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�>fD�~fD���D��D�D�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D�fD��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D��fD��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AӃA�~�A�|�A�|�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�|�A�z�A�x�A�x�A�x�A�z�A�z�A�x�A�jA�hsA�VA�E�A�%A�n�A�dZA��AþwA��A��+A�7LA�A�A���A��HA�t�A���A��A�;dA�+A�C�A�O�A�K�A��A�ffA���A�E�A��jA�l�A�1A��DA�bNA��A��#A��-A��`A���A��RA�VA�ZA�M�A� �A�33A���A�=qA��A�\)A�JA���A�oA��
A�A�A��A��PA�+A���A�dZA���A���A���A�hsA�=qA���A�hsA�ȴA���A��!A�`BA��;A�ĜA��A�A�+A��hA��A�A���A�33A��A~~�A|$�Ay��Ay7LAyAx��AxffAx-AwƨAu��ArE�Aq�;Aq"�AnbAl�9Aj��Ai
=Ag��Af�+Ad�HAdAb�Ab1'Aa��A`�jA_K�A^�A]�;A]�hA]
=A\E�A\$�A[��AZ��AZbAY|�AX�AX~�AXM�AW�7AW%AVr�AV$�AU�-AU�AS�AS+AR��AQ�#APz�AOS�ANZAL�AL1AK��AK��AJ�RAJ(�AIAG�7AFI�AE��AD1AChsAC/AB�/AA�A@�A@�uA?\)A>��A>VA=%A;��A:�jA:�DA:1'A9�FA9\)A8��A8�A7�-A6�RA6bA5��A5�PA5t�A5�A4�A3K�A2(�A0z�A.�!A,{A*A�A)K�A'��A&��A%�A%VA%A$�HA$�RA#�;A"^5A"5?A"bA!�A!�^A!+A �\A�A��AK�A{A�9A�A�RA�AS�A�RA��AƨA�/A��AA�/AZA�A\)A
�A	��A��A��A��A(�AS�Az�A�FA�A�;A��AA�A -@�ȴ@�M�@�$�@��@��@���@�K�@��h@�"�@�/@��H@�r�@��@�@�V@陚@�/@���@�D@���@��y@�X@�j@�(�@��@�w@�ȴ@޸R@�x�@�Ĝ@�"�@أ�@���@Ձ@�7L@�j@ӶF@ҸR@с@�;d@͙�@�r�@�|�@��T@�p�@��/@ȼj@ȣ�@Ǯ@Ų-@ēu@î@�ȴ@��T@�X@�C�@���@�@��h@�%@�Q�@���@�o@���@��@��j@��@�bN@�  @�t�@�V@��@�Z@��@��@�n�@�5?@��-@�hs@���@��;@�+@���@�^5@��T@���@�O�@�r�@���@�
=@�-@�x�@�bN@��w@�K�@��!@��@�1'@�|�@��!@�=q@���@�X@��/@�r�@�1'@���@�M�@���@�p�@��@���@��@�|�@��@��@�^5@���@���@�p�@��D@�;d@��!@�E�@���@��h@�`B@�/@���@���@���@��@�bN@�Z@�b@��y@�p�@���@��F@���@�V@�$�@�@�`B@��@�bN@��F@���@�v�@�M�@�$�@��@��#@�`B@�V@���@��u@�j@�I�@��@��;@���@���@�l�@�K�@��@���@���@��+@�$�@��-@�`B@�?}@�/@���@�j@�j@�z�@��@�z�@��@�j@���@�K�@�"�@�C�@�\)@�\)@�33@�;d@���@��H@�n�@��@�@��-@��T@��@��@��T@�$�@�v�@���@�^5@��+@�E�@�Ĝ@�w@~�+@~5?@}@}�@;d@�1@}@|9X@{�
@{dZ@z=q@z=q@y�@x��@x�9@x�@w�w@vv�@v{@v{@u��@uO�@t��@t��@t�D@t��@uV@tz�@st�@so@rM�@q�^@q��@q��@q�7@qx�@qx�@qX@q&�@q&�@p�`@p��@pbN@pQ�@q&�@q��@q�7@p�u@pĜ@p�u@p  @o;d@n5?@m�T@mO�@mV@l��@l�@l��@l��@lz�@lZ@l9X@k��@kC�@ko@j�@j��@j^5@i��@i��@i��@ihs@iG�@i&�@h��@h�@hbN@hbN@hQ�@h  @g�P@g+@f�+@fff@f5?@f{@e�h@ep�@e`B@eO�@e/@d�/@d�/@d�@d�@d��@d9X@d1@dI�@d�@c�m@cƨ@c�F@c��@c�@cS�@b�H@b-@a�^@a�7@`�9@_�@^v�@^E�@^E�@^5?@^@\Z@[�F@[�
@\(�@[dZ@Z�\@ZM�@Y��@Y��@Y��@Yx�@YG�@Y�@X��@XQ�@W�w@W+@Vȴ@Vff@V5?@U�@U�-@U/@T�/@T�@T��@Tz�@T(�@SS�@R^5@Q�#@QX@P�`@PbN@PbN@Q%@Qx�@Q�7@Q%@P1'@P  @O+@Nff@M�T@M�@Nv�@N�R@N�R@Nff@L�D@L1@Kƨ@KdZ@K33@Ko@K@J��@J��@J�\@JM�@I��@I��@I��@I��@I�^@I��@Ihs@I&�@H�`@H  @G�w@G�@Gl�@Gl�@Gl�@Gl�@Gl�@G\)@G;d@G�@F��@F�+@E��@D��@D�/@D�/@D�j@Dj@D(�@Ct�@CC�@B��@B�!@B^5@B�@A�@A�@A�#@A��@Ax�@AG�@A7L@@��@@��@@�@@Q�@@ �@@b@@  @?�;@?�@?l�@?K�@>�y@>�+@>$�@=��@=�h@=`B@=V@<�/@<�j@<j@<�@;��@;��@;��@;�m@;��@:�H@:�!@:-@:�@:J@:J@:J@:J@:J@:J@:J@9��@8bN@7�w@7;d@6�y@6��@6��@6��@6ȴ@6ȴ@6�R@6��@6��@6��@6�+@6E�@5@5p�@5`B@5p�@5p�@5p�@5�@5�@5�@5�h@5�h@5�h@5p�@4�/@4��@49X@3�m@3��@3dZ@3S�@3C�@3"�@2�H@2�@1��@1x�@0�`@0�u@0bN@0b@0  @/�@/�w@/|�@.��@.ff@-�T@-��@-�@-`B@-/@-V@,�@,�j@+��@+�
@+dZ@*�@*��@*��@*=q@)��@)�^@)�^@)�^@)x�@)7L@(�`@(Ĝ@(bN@(  @'�;@'|�@';d@&�@&ff@&{@%�T@%�-@%�h@%O�@$��@$Z@$(�@$�@#�m@#��@#��@#t�@#dZ@#C�@#@"��@"�!@"�\@"n�@"M�@"�@!��@!��@!��@!�7@!�7@!�7@!X@!7L@!&�@!%@ �u@ ��@ �9@ ��@ �@ A�@�w@\)@;d@
=@��@V@@�@O�@�/@�@��@Z@�
@�F@�@dZ@C�@33@"�@�@��@�!@�@x�@G�@7L@��@�`@�9@�@r�@Q�@A�@A�@A�@1'@b@�@��@|�@|�@l�@l�@�@�@ȴ@��@v�@�+@$�@{@�@�-@?}@V@��@�/@�@j@1@�
@��@S�@�H@�!@�!@�!@�!@�!@�!@~�@~�@n�@=q@��@�^@hs@&�@�@�@��@Ĝ@�@Q�@A�@1'@b@  @�@�w@l�@\)@\)@\)@K�@�y@ȴ@��@E�@5?@{@@�T@@��@�@O�@�j@��@��@z�@j@(�@�m@ƨ@��@dZ@"�@
��@
~�@
^5@
M�@
=q@
�@
J@	�@	��@	�^@	�^@	��@	��@	��@	�7@	7L@	&�@	�@��@�`@Ĝ@Ĝ@Ĝ@�9@�@A�@ �@  @�w@|�@;d@�R@5?@$�@�T@�@/@V@�@�@�D@j@I�@(�@��@�m@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AӃA�~�A�|�A�|�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�|�A�~�A�|�A�z�A�x�A�x�A�x�A�z�A�z�A�x�A�jA�hsA�VA�E�A�%A�n�A�dZA��AþwA��A��+A�7LA�A�A���A��HA�t�A���A��A�;dA�+A�C�A�O�A�K�A��A�ffA���A�E�A��jA�l�A�1A��DA�bNA��A��#A��-A��`A���A��RA�VA�ZA�M�A� �A�33A���A�=qA��A�\)A�JA���A�oA��
A�A�A��A��PA�+A���A�dZA���A���A���A�hsA�=qA���A�hsA�ȴA���A��!A�`BA��;A�ĜA��A�A�+A��hA��A�A���A�33A��A~~�A|$�Ay��Ay7LAyAx��AxffAx-AwƨAu��ArE�Aq�;Aq"�AnbAl�9Aj��Ai
=Ag��Af�+Ad�HAdAb�Ab1'Aa��A`�jA_K�A^�A]�;A]�hA]
=A\E�A\$�A[��AZ��AZbAY|�AX�AX~�AXM�AW�7AW%AVr�AV$�AU�-AU�AS�AS+AR��AQ�#APz�AOS�ANZAL�AL1AK��AK��AJ�RAJ(�AIAG�7AFI�AE��AD1AChsAC/AB�/AA�A@�A@�uA?\)A>��A>VA=%A;��A:�jA:�DA:1'A9�FA9\)A8��A8�A7�-A6�RA6bA5��A5�PA5t�A5�A4�A3K�A2(�A0z�A.�!A,{A*A�A)K�A'��A&��A%�A%VA%A$�HA$�RA#�;A"^5A"5?A"bA!�A!�^A!+A �\A�A��AK�A{A�9A�A�RA�AS�A�RA��AƨA�/A��AA�/AZA�A\)A
�A	��A��A��A��A(�AS�Az�A�FA�A�;A��AA�A -@�ȴ@�M�@�$�@��@��@���@�K�@��h@�"�@�/@��H@�r�@��@�@�V@陚@�/@���@�D@���@��y@�X@�j@�(�@��@�w@�ȴ@޸R@�x�@�Ĝ@�"�@أ�@���@Ձ@�7L@�j@ӶF@ҸR@с@�;d@͙�@�r�@�|�@��T@�p�@��/@ȼj@ȣ�@Ǯ@Ų-@ēu@î@�ȴ@��T@�X@�C�@���@�@��h@�%@�Q�@���@�o@���@��@��j@��@�bN@�  @�t�@�V@��@�Z@��@��@�n�@�5?@��-@�hs@���@��;@�+@���@�^5@��T@���@�O�@�r�@���@�
=@�-@�x�@�bN@��w@�K�@��!@��@�1'@�|�@��!@�=q@���@�X@��/@�r�@�1'@���@�M�@���@�p�@��@���@��@�|�@��@��@�^5@���@���@�p�@��D@�;d@��!@�E�@���@��h@�`B@�/@���@���@���@��@�bN@�Z@�b@��y@�p�@���@��F@���@�V@�$�@�@�`B@��@�bN@��F@���@�v�@�M�@�$�@��@��#@�`B@�V@���@��u@�j@�I�@��@��;@���@���@�l�@�K�@��@���@���@��+@�$�@��-@�`B@�?}@�/@���@�j@�j@�z�@��@�z�@��@�j@���@�K�@�"�@�C�@�\)@�\)@�33@�;d@���@��H@�n�@��@�@��-@��T@��@��@��T@�$�@�v�@���@�^5@��+@�E�@�Ĝ@�w@~�+@~5?@}@}�@;d@�1@}@|9X@{�
@{dZ@z=q@z=q@y�@x��@x�9@x�@w�w@vv�@v{@v{@u��@uO�@t��@t��@t�D@t��@uV@tz�@st�@so@rM�@q�^@q��@q��@q�7@qx�@qx�@qX@q&�@q&�@p�`@p��@pbN@pQ�@q&�@q��@q�7@p�u@pĜ@p�u@p  @o;d@n5?@m�T@mO�@mV@l��@l�@l��@l��@lz�@lZ@l9X@k��@kC�@ko@j�@j��@j^5@i��@i��@i��@ihs@iG�@i&�@h��@h�@hbN@hbN@hQ�@h  @g�P@g+@f�+@fff@f5?@f{@e�h@ep�@e`B@eO�@e/@d�/@d�/@d�@d�@d��@d9X@d1@dI�@d�@c�m@cƨ@c�F@c��@c�@cS�@b�H@b-@a�^@a�7@`�9@_�@^v�@^E�@^E�@^5?@^@\Z@[�F@[�
@\(�@[dZ@Z�\@ZM�@Y��@Y��@Y��@Yx�@YG�@Y�@X��@XQ�@W�w@W+@Vȴ@Vff@V5?@U�@U�-@U/@T�/@T�@T��@Tz�@T(�@SS�@R^5@Q�#@QX@P�`@PbN@PbN@Q%@Qx�@Q�7@Q%@P1'@P  @O+@Nff@M�T@M�@Nv�@N�R@N�R@Nff@L�D@L1@Kƨ@KdZ@K33@Ko@K@J��@J��@J�\@JM�@I��@I��@I��@I��@I�^@I��@Ihs@I&�@H�`@H  @G�w@G�@Gl�@Gl�@Gl�@Gl�@Gl�@G\)@G;d@G�@F��@F�+@E��@D��@D�/@D�/@D�j@Dj@D(�@Ct�@CC�@B��@B�!@B^5@B�@A�@A�@A�#@A��@Ax�@AG�@A7L@@��@@��@@�@@Q�@@ �@@b@@  @?�;@?�@?l�@?K�@>�y@>�+@>$�@=��@=�h@=`B@=V@<�/@<�j@<j@<�@;��@;��@;��@;�m@;��@:�H@:�!@:-@:�@:J@:J@:J@:J@:J@:J@:J@9��@8bN@7�w@7;d@6�y@6��@6��@6��@6ȴ@6ȴ@6�R@6��@6��@6��@6�+@6E�@5@5p�@5`B@5p�@5p�@5p�@5�@5�@5�@5�h@5�h@5�h@5p�@4�/@4��@49X@3�m@3��@3dZ@3S�@3C�@3"�@2�H@2�@1��@1x�@0�`@0�u@0bN@0b@0  @/�@/�w@/|�@.��@.ff@-�T@-��@-�@-`B@-/@-V@,�@,�j@+��@+�
@+dZ@*�@*��@*��@*=q@)��@)�^@)�^@)�^@)x�@)7L@(�`@(Ĝ@(bN@(  @'�;@'|�@';d@&�@&ff@&{@%�T@%�-@%�h@%O�@$��@$Z@$(�@$�@#�m@#��@#��@#t�@#dZ@#C�@#@"��@"�!@"�\@"n�@"M�@"�@!��@!��@!��@!�7@!�7@!�7@!X@!7L@!&�@!%@ �u@ ��@ �9@ ��@ �@ A�@�w@\)@;d@
=@��@V@@�@O�@�/@�@��@Z@�
@�F@�@dZ@C�@33@"�@�@��@�!@�@x�@G�@7L@��@�`@�9@�@r�@Q�@A�@A�@A�@1'@b@�@��@|�@|�@l�@l�@�@�@ȴ@��@v�@�+@$�@{@�@�-@?}@V@��@�/@�@j@1@�
@��@S�@�H@�!@�!@�!@�!@�!@�!@~�@~�@n�@=q@��@�^@hs@&�@�@�@��@Ĝ@�@Q�@A�@1'@b@  @�@�w@l�@\)@\)@\)@K�@�y@ȴ@��@E�@5?@{@@�T@@��@�@O�@�j@��@��@z�@j@(�@�m@ƨ@��@dZ@"�@
��@
~�@
^5@
M�@
=q@
�@
J@	�@	��@	�^@	�^@	��@	��@	��@	�7@	7L@	&�@	�@��@�`@Ĝ@Ĝ@Ĝ@�9@�@A�@ �@  @�w@|�@;d@�R@5?@$�@�T@�@/@V@�@�@�D@j@I�@(�@��@�m@��@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bo�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Bs�Bs�Br�Br�Br�Bv�Bu�By�Bx�Bx�BC�B�B+B\)B\)B�=By�Bn�B��Bm�B��B�oB��B��B��B��B�VB}�B~�Bw�Bq�BjBiyBbNB\)BYBJ�B1'B+BhBB�B�)BB��B�}B��BB�qB�dB�B��B�\B�VB�{B�oB�B�%B�B�Bt�BffB^5BF�B?}BG�BD�B33B�B	7B�BVB
��B
�fB
��B
��B
ɺB
ÖB
�XB
�9B
��B
v�B
q�B
q�B
aHB
T�B
_;B
aHB
^5B
YB
S�B
H�B
2-B
hB
%�B
�B	��B	��B	�B	�;B	�TB	�)B	�
B	��B	��B	ɺB	ǮB	�}B	�FB	�3B	�^B	�LB	�-B	�B	�!B	�B	��B	��B	��B	��B	��B	��B	�hB	�\B	�JB	�DB	�+B	�B	x�B	t�B	v�B	p�B	e`B	_;B	ZB	P�B	M�B	K�B	K�B	?}B	5?B	+B	�B	�B	�B	VB	\B	oB	\B	B��B��B�B�B�B�BB�5B�5B�mB�TB�;B�BB�)B�B��B��B��B��B��B��BɺB�wB�'B��B��B�VB�PB�DB��B�oB��B��B��B��B��B��B�bB�B��B��B�uB�VB�1B�B}�Bo�Bk�BdZBdZBgmBaHBffB`BBZBR�B@�BI�BC�B8RBB�BB�B9XB)�B6FB?}B@�BD�BB�B;dB49B1'B1'B/B&�B#�B.B(�B'�B-B49B49B,B#�B#�B$�B �B�B�B�B�B(�B&�B'�B'�B)�B)�B(�B%�B �B�B"�B'�B%�B!�B�BB�B�B�BVB�B�B$�B �B�B�B�BuB�B�B"�B�B)�B(�B,B)�B"�B�B!�B$�B&�B%�B'�B �B/B1'B33B33B2-B6FB49B49B9XB>wB?}B?}B=qB;dB8RB9XB?}BA�BD�BG�BJ�BJ�BK�BK�BG�BI�BL�BO�BN�BP�BN�BK�BN�BQ�BP�BT�BT�B[#B^5B_;B[#B`BBgmBgmBm�Bo�Bm�Bp�Br�Bs�Br�Bo�B~�B�B�B�B�B�1B�JB�PB�JB�VB�uB�uB�bB�hB��B��B��B��B��B�B�B�B�B�B�B�B�B��B��B�FB�dBB��B��B��B��B�
B�/B�/B�;B�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B	+B	JB	bB	hB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	'�B	)�B	)�B	(�B	+B	,B	-B	,B	/B	7LB	:^B	=qB	A�B	A�B	A�B	D�B	G�B	J�B	M�B	Q�B	W
B	P�B	R�B	T�B	ZB	]/B	aHB	e`B	ffB	^5B	^5B	e`B	e`B	dZB	k�B	r�B	o�B	s�B	r�B	q�B	p�B	u�B	y�B	|�B	}�B	�B	�B	�%B	�DB	�PB	�=B	�7B	�JB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�B	�B	�-B	�9B	�?B	�FB	�LB	�^B	�dB	�jB	�dB	�wB	��B	B	B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ɺB	ɺB	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�/B	�5B	�5B	�5B	�/B	�5B	�NB	�TB	�TB	�`B	�`B	�`B	�`B	�`B	�ZB	�ZB	�fB	�mB	�ZB	�HB	�mB	�B	�B	�B	�yB	�fB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
  B	��B
B
B
B
B
	7B
JB
\B
JB
DB
DB
VB
PB
DB
PB
hB
uB
{B
uB
hB
JB
bB
oB
hB
oB
oB
oB
oB
oB
uB
oB
hB
uB
uB
{B
�B
�B
{B
{B
{B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
 �B
�B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
+B
+B
+B
,B
,B
-B
,B
-B
.B
/B
/B
.B
,B
+B
/B
.B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
/B
-B
(�B
-B
/B
0!B
0!B
33B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
49B
7LB
9XB
:^B
:^B
9XB
9XB
9XB
9XB
9XB
9XB
8RB
7LB
5?B
7LB
7LB
8RB
8RB
9XB
:^B
:^B
9XB
8RB
6FB
9XB
9XB
8RB
:^B
<jB
<jB
=qB
=qB
<jB
;dB
;dB
;dB
=qB
>wB
@�B
@�B
@�B
@�B
@�B
?}B
>wB
B�B
B�B
B�B
D�B
D�B
B�B
B�B
E�B
F�B
F�B
D�B
D�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
J�B
L�B
K�B
K�B
L�B
L�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
L�B
O�B
O�B
N�B
M�B
M�B
K�B
M�B
N�B
N�B
M�B
M�B
L�B
N�B
O�B
N�B
P�B
P�B
O�B
O�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
P�B
O�B
P�B
R�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
T�B
T�B
T�B
VB
VB
VB
VB
T�B
VB
W
B
W
B
W
B
XB
T�B
W
B
W
B
VB
VB
XB
ZB
ZB
YB
YB
YB
\)B
[#B
[#B
[#B
^5B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
]/B
]/B
]/B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
cTB
e`B
dZB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bo�Bp�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Bs�Bs�Br�Br�Br�Bv�BvBzxBz�B}�BS@B5B1�B`BB`vB�~B��BvB��Bt�B�zB��B�eB��B��B��B��B��B�4By$Br�Bk�Bj0Bc:B]/BY�BLB4TB^B[BzB��B��B�YB�&B�AB�{B��B�]B�B��B�B�:B��B�MB�B�_B�_B��B��Bv+BhsB`\BIlBA�BHKBE�B5B�B)BB\B �B
�*B
ӏB
ԕB
�DB
�B
��B
�%B
� B
z�B
t�B
s�B
dB
WsB
_�B
a�B
^�B
Y�B
T{B
I�B
4�B
2B
&�B
B
UB	��B	��B	�B	��B	ݲB	��B	�B	�B	��B	ȚB	��B	�B	��B	��B	��B	��B	��B	�UB	��B	��B	��B	�dB	�WB	�1B	��B	�oB	�B	�B	��B	��B	��B	z*B	u�B	w�B	q�B	gB	`�B	[qB	RoB	N�B	LJB	K�B	@�B	6+B	,�B	qB	#B	�B	B	B	�B	�B	�B	  B��B�?B�B�oB��B��B�VB�B��B��B��B��B��B�4B��BѝB�oB�,B�4B�XB�}B��B��B�B�B�bB��B�B�{B��B��B�sB��B��B�B��B��B��B��B��B��B�B�BBqvBmCBf2Bf2Bh�Bb�Bg8BabB[=BT�BB�BKBEmB:�BC�BC{B;0B,qB7�B@OBA;BD�BCB<PB5ZB2GB2GB0;B(�B%`B/ B*0B)*B.B4�B4�B-)B%`B%FB&B"B/B�BB#B)_B'�B(XB(sB*eB*KB)DB&fB!�B �B#nB($B&B"4B�BzB+BdB�B�B�B�B%,B!|B \B�B�B�B�B �B#�B�B*eB)_B,"B*KB#�B	B"�B%zB'�B&�B(sB"NB/�B1�B3�B3�B2�B6�B4�B5%B9�B>�B?�B?�B=�B;�B9>B:*B@BB'BEBHBKBK)BK�BLJBH�BJ=BM6BP.BOBBQBO\BL~BO\BRoBQ�BU�BU�B[�B^�B_�B\)B`�Bg�Bh
Bm�Bo�BnBqBsBs�Bs3BpUBHB�aB�aB�mB��B��B��B��B��B��B��B��B�4B�:B�B� B�FB�$B�B�B�=B�"B�CB�/B�IB�/B�kB��B�B��B�B�GB��B�,B�2B�{B׍B�~B��B��B�B�B��B�B��B��B��B��B��B�B�	B�B�B�B�B�.B	 B	;B	GB	-B	aB	gB	zB	~B	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	5B	B	#�B	'�B	)�B	)�B	(�B	+B	,=B	-)B	,qB	/�B	7fB	:^B	=qB	A�B	A�B	A�B	D�B	GzB	J�B	NB	RB	WYB	Q�B	SuB	UgB	Z7B	]IB	a-B	eB	fLB	_B	^�B	e�B	e�B	d�B	k�B	r�B	o�B	s�B	r�B	q�B	qB	u�B	y�B	}"B	~(B	� B	�B	�?B	�DB	�6B	��B	��B	�~B	�~B	��B	�gB	�sB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�)B	�QB	��B	�!B	�iB	�cB	�WB	�5B	�cB	�GB	�TB	�ZB	�FB	�fB	�xB	�dB	��B	��B	��B	��B	B	ªB	��B	ÖB	ĜB	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	� B	�B	�B	�B	�B	�
B	�$B	�B	�/B	�5B	�OB	�OB	�dB	�OB	�NB	�nB	�nB	�zB	�`B	�zB	�zB	�zB	�tB	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B
 B
 B
'B
-B
3B
3B
-B
AB
 4B	�]B
AB
AB
GB
B
	7B
�B
BB
dB
^B
�B
pB
�B
�B
�B
4B
@B
aB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
[B
�B
�B
�B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
 �B
B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
%�B
(
B
(
B
(
B
(�B
)B
)B
)B
)B
)B
)*B
)B
)B
+B
+B
+B
,"B
,B
-)B
,"B
-)B
./B
/B
/ B
.B
,"B
+QB
/5B
.IB
1'B
1'B
1'B
1'B
1B
1'B
0!B
/5B
-CB
)_B
-CB
/OB
0!B
0;B
3B
49B
5%B
6+B
6FB
6FB
6FB
6+B
6`B
5ZB
4nB
7LB
9>B
:^B
:DB
9XB
9XB
9XB
9XB
9>B
9>B
88B
7LB
5tB
7fB
7�B
8lB
8lB
9rB
:^B
:^B
9XB
8lB
6�B
9rB
9�B
8�B
:xB
<�B
<�B
=qB
=�B
<jB
;�B
;B
;�B
=�B
>wB
@�B
@�B
@�B
@�B
@�B
?�B
>�B
B�B
B�B
B�B
D�B
D�B
B�B
B�B
E�B
F�B
F�B
D�B
D�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
J�B
J�B
I�B
I�B
I�B
J�B
L�B
K�B
K�B
L�B
L�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
M�B
L�B
O�B
O�B
N�B
M�B
M�B
K�B
M�B
N�B
N�B
NB
M�B
MB
N�B
O�B
N�B
Q B
Q B
O�B
O�B
Q�B
RB
RB
SB
R�B
R�B
Q�B
Q�B
Q B
PB
QB
SB
T�B
S�B
T�B
UB
UB
T�B
VB
VB
VB
U�B
VB
UB
UB
T�B
VB
VB
VB
U�B
T�B
VB
W
B
W$B
W$B
XB
U2B
V�B
W$B
VB
V9B
X+B
ZB
Z7B
YB
YB
YKB
\CB
[=B
[=B
[WB
^OB
_!B
_!B
_;B
_;B
_;B
^OB
^5B
^5B
]IB
]IB
]/B
^OB
_VB
`BB
aHB
abB
aHB
aHB
bhB
cTB
dZB
cTB
dZB
dtB
dtB
dtB
eFB
e`B
e`B
e`B
c�B
ezB
dtB
dZB
fLB
f�B
ffB
f�B
f�B
ffB
f�B
f�B
ezB
gRB
hXB
hsB
hsB
gmB
h�B
h�B
h�B
hsB
h�B
hsB
i�B
kkB
k�B
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
mwB
mwB
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<z��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810090031362018100900313620181009003136201810090200172018100902001720181009020017201810100021132018101000211320181010002113  JA  ARFMdecpA19c                                                                20181005093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181005003515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181005003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181005003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181005003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181005003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181005003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181005003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181005003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181005003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20181005005741                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181005153626  CV  JULD            G�O�G�O�F�3�                JM  ARGQJMQC2.0                                                                 20181005153626  CV  JULD_LOCATION   G�O�G�O�F�3�                JM  ARGQJMQC2.0                                                                 20181005153626  CV  LONGITUDE       G�O�G�O��!Ӷ                JM  ARCAJMQC2.0                                                                 20181008153136  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181008153136  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181008170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181009152113  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                