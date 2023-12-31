CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:25Z creation;2022-06-04T17:52:25Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175225  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��{B_1   @��]L;@1������cr$�/�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C�fC�fC  C  C
  C  C  C�fC�fC  C�C  C  C  C�C 33C!�3C#�fC&  C(  C*  C,  C.  C0  C2�C4  C6  C8�C:  C<�C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ff@���@���A ��A ��A@��AbfgA�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`��Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��gB��B��B��B��B��B��B��B��B��B��B��gB��gC�3C�3C�C�C
�C�C�C�3C�3C�C&gC�C�C�C&gC @ C!� C#�3C&�C(�C*�C,�C.�C0�C2&gC4�C6�C8&gC:�C<&gC=�3C?�3CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch&gCj&gCl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D	�D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De	�De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�;4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�%FA�#�A��A�uA�AA�A��A��9A�A˘�A�~]A�~A��A��A���Aʩ�A��A�0�A�+6A�1�A�[�A��AǽqA��AƘ�Aƒ:Aƀ4A�T�A� �A�bA�MA��]A�ҽAŰUAŌ�A�~�A�<�A���A�ʌA�~A�_;A�PA���A��vA�m�A���A�"4A�N<A�U�A��OA�k�A��A��&A�u�A�{�A��A��
A��A��mA�-wA�x�A�/OA��+A���A�
rA��A�zDA��A�O�A�5�A��$A�=A�W
A��dA��@A���A�jA���A�$�A�خA�MjA�_A���A��XA�^�A���A���A�@�A�K^A��IA�B�A� A�x�A��AyA�As��ApH�Aj�AfA AeC�Ab�FA]�AVϫATߤAS�`ARo�AQ1�AM��AJ�hAG�tAC�`AB!�AAB�A@bA>�HA=��A=�A;MjA9��A99XA8�pA6ѷA5p;A5�A4��A2��A2{�A1&A/H�A.X�A/MA0��A0D�A0JA/:�A._�A-��A-�A,f�A+��A+uA*H�A)I�A(jA'�KA'~�A'J�A&�MA&��A&��A%�A#�A#1A"�#A"o A"��A";�A!�eA!S�A �6AɆAo�AoiA�UA �A �A ��A �9A �A �LA �At�A�A�qAw�As�A�6A�A��A��A�8A�MAѷAoA��Am�A4A��A�>A�2A�A�kA_pAYA<6A�zAv�AJ�A �AJ�A��AT�A��A?�A�+A�rA�dA��A�oAl"A��Ag�A"hA�nAX�A<6A�A�A{�AMA0�A�A�A��A��A��A��A<6A
ȴA	�*A�A��APHA�6A^5A'�A�A
=A�A͟AxA�$A��A�^A��A�MA[�A��A�;A�:AT�A-�A ��A r�A `BA Q�A 5�A (�A �@���@���@�oi@��|@��@�g8@�#:@��@�[W@��@�b�@�@�b@�a�@�}V@��P@�I�@�1@�h@�=�@���@�ff@��@�1@�F@��A@�f�@�?}@�n@�O@�A @�R@�R�@��@�%F@��F@�(�@��@߀4@��"@���@�
=@�c�@��a@�u�@�5�@�ߤ@ڌ@�Ov@���@�?}@ؖ�@�D�@�<�@�($@��z@�X�@�&�@��B@��@Ԩ�@�"h@ӥ�@��@�W�@�*0@�xl@Ϣ�@ξ@�$�@ͷ�@͢�@͕�@�l�@�L�@�0�@�'�@��@�ff@��6@ˀ4@�q@ʾ@�@ɇ�@�iD@�>�@ȉ�@�$�@���@���@ǧ�@��@ƃ�@�6@Ň�@Ļ�@�d�@��@Þ�@�<6@��8@¤�@�l"@�Ta@�PH@�e�@�H�@��@��
@��'@�[W@��@���@�z�@�(�@���@�|@�j�@�Q�@�@@��@���@���@���@��@���@�j@�Ft@� �@��@��.@�H@���@�0�@��h@�=q@��.@��@@�ȴ@��C@�>�@��_@���@���@���@�ff@�>B@�8�@�c�@��@��I@�E�@��D@�)_@��@��@��;@���@��q@��"@�a@�5�@��@���@��z@�[�@�*�@�
�@��M@��@��P@�q�@�m]@��c@��@�bN@��>@�X@�o@��H@���@��@��@��P@�RT@�5�@��@��@��@��o@��>@�Mj@��D@��$@�S@�i�@�)�@��)@�U�@��@��B@���@��b@��1@���@�s�@�8�@���@���@��S@���@�qv@�n/@�]�@�C�@�@���@�YK@�!@�	@�{@�@��@��W@���@���@�W?@�=@�&@��6@�.�@��@��}@��t@���@��@@���@���@��@�Mj@��@��X@�W�@���@�@�o@��@�V@�6�@�*�@��&@��P@���@���@�	�@��/@���@�bN@�PH@�L0@�GE@�8�@��@��#@���@�zx@���@��j@���@�i�@���@�ƨ@���@��F@��[@��[@��@�7@�X�@��8@��H@���@���@�z�@�tT@�d�@�Ov@�@�4n@��C@�!-@��@���@���@�a|@�6@�e@���@���@�l�@�'�@��s@���@�]d@�,=@��@���@�~�@��@���@�Xy@�)�@�b�@�(@���@��E@���@��}@���@���@�i�@�`�@�Ov@�:*@�u@�@���@��@��@���@���@�R�@�$�@���@���@���@�^�@��@��@���@��e@��A@�s�@�h
@�J�@�
�@��}@���@�`B@�(�@���@���@�oi@�kQ@�V@��@iD@@~�\@~$�@}�n@}%F@|9X@{�q@{Z�@{1�@{.I@{C@{
=@{�@z�@zں@z��@z��@yIR@x�O@x_@x@w
=@v��@v��@vOv@v�@uhs@t��@ty>@s�@s��@sS�@s�@s�@sY@s�@s(@r�@r{�@r$�@q��@q?}@p�@pg8@pG@o�@n��@nkQ@nJ�@n5?@nO@m�@l��@lV�@k�V@ko�@k,�@j�s@j��@j��@j3�@i�t@iq@g�r@gx@f�A@f�@e�^@e��@e�S@eJ�@e	l@d�@c�@c4�@b_�@b-@b	@a��@a�"@`��@`c�@_�@^��@^��@^h
@]�@]^�@\�U@\~(@[��@[o�@[]�@[H�@[8@[�@Z��@Y�@Y \@X�?@X[�@W��@W�@Ws@W_p@W6z@W i@V��@VkQ@V3�@U��@U��@U�@U@@T�`@T��@T9X@S��@S�[@S��@Sl�@S
=@R�y@R͟@R�\@R}V@R+k@Q�o@Q@Q2a@P�)@P��@Py>@P]d@P/�@O�A@O�F@OU�@Oo@N�@N� @N=q@N!�@M�@M/@L�@L��@LQ�@K��@K�f@Kg�@K=@J͟@JYK@J�@I��@I��@I��@If�@I4@I�@H��@H4n@G�@G��@G.I@F�H@F��@Fl�@E��@E+@Dѷ@D~@C�4@CiD@CS�@CJ#@C)_@B��@A�@A�@A��@A:�@@�@@�@@PH@?U�@?Y@?(@>�y@>�F@>xl@>M�@=��@<��@<1@;��@;x@;l�@;W?@;J#@;�@; i@:�y@:ȴ@:��@:�h@:�F@:c @:3�@:{@9��@9��@9\�@9#�@9�@8�`@8��@8e�@8�@7��@7�f@7�{@7Z�@7"�@6��@6z@5�@50�@4Ɇ@4��@4u�@4h�@4U2@44n@3�	@3!-@3�@3�@2��@2@1IR@0�@0`�@0*�@/�4@/RT@.�8@.q�@-�D@-�'@-s�@-X@-J�@-G�@-Dg@-+�@,�P@,�O@,�@+��@+��@+W?@+"�@+�@+�@+@*�"@*�@*��@*ں@*�s@*҉@*�}@*{�@*YK@)�o@)F@(�v@(��@'�W@'o@&�@&��@&�A@&;�@%�@%��@%�@%��@%�@$��@$��@$U2@$�@#�@#��@#�k@#�k@#�@#qv@#�@"��@"�\@"�@!��@!s�@!a�@![W@!<6@ �P@ ��@ �E@ oi@ 1'@�r@��@�V@�V@��@�:@�:@��@Mj@�@��@�L@�+@ff@GE@=q@.�@�@�)@�@��@�M@[W@J�@#�@�@��@I�@�g@�@@�4@W?@,�@�@��@��@�3@�M@Dg@�@֡@Ĝ@�z@oi@/�@�@��@|�@@O@��@��@��@�r@�r@{�@Ta@ �@rG@:�@(�@�@��@�o@?�@�@��@�@��@y�@�@�,@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�%FA�#�A��A�uA�AA�A��A��9A�A˘�A�~]A�~A��A��A���Aʩ�A��A�0�A�+6A�1�A�[�A��AǽqA��AƘ�Aƒ:Aƀ4A�T�A� �A�bA�MA��]A�ҽAŰUAŌ�A�~�A�<�A���A�ʌA�~A�_;A�PA���A��vA�m�A���A�"4A�N<A�U�A��OA�k�A��A��&A�u�A�{�A��A��
A��A��mA�-wA�x�A�/OA��+A���A�
rA��A�zDA��A�O�A�5�A��$A�=A�W
A��dA��@A���A�jA���A�$�A�خA�MjA�_A���A��XA�^�A���A���A�@�A�K^A��IA�B�A� A�x�A��AyA�As��ApH�Aj�AfA AeC�Ab�FA]�AVϫATߤAS�`ARo�AQ1�AM��AJ�hAG�tAC�`AB!�AAB�A@bA>�HA=��A=�A;MjA9��A99XA8�pA6ѷA5p;A5�A4��A2��A2{�A1&A/H�A.X�A/MA0��A0D�A0JA/:�A._�A-��A-�A,f�A+��A+uA*H�A)I�A(jA'�KA'~�A'J�A&�MA&��A&��A%�A#�A#1A"�#A"o A"��A";�A!�eA!S�A �6AɆAo�AoiA�UA �A �A ��A �9A �A �LA �At�A�A�qAw�As�A�6A�A��A��A�8A�MAѷAoA��Am�A4A��A�>A�2A�A�kA_pAYA<6A�zAv�AJ�A �AJ�A��AT�A��A?�A�+A�rA�dA��A�oAl"A��Ag�A"hA�nAX�A<6A�A�A{�AMA0�A�A�A��A��A��A��A<6A
ȴA	�*A�A��APHA�6A^5A'�A�A
=A�A͟AxA�$A��A�^A��A�MA[�A��A�;A�:AT�A-�A ��A r�A `BA Q�A 5�A (�A �@���@���@�oi@��|@��@�g8@�#:@��@�[W@��@�b�@�@�b@�a�@�}V@��P@�I�@�1@�h@�=�@���@�ff@��@�1@�F@��A@�f�@�?}@�n@�O@�A @�R@�R�@��@�%F@��F@�(�@��@߀4@��"@���@�
=@�c�@��a@�u�@�5�@�ߤ@ڌ@�Ov@���@�?}@ؖ�@�D�@�<�@�($@��z@�X�@�&�@��B@��@Ԩ�@�"h@ӥ�@��@�W�@�*0@�xl@Ϣ�@ξ@�$�@ͷ�@͢�@͕�@�l�@�L�@�0�@�'�@��@�ff@��6@ˀ4@�q@ʾ@�@ɇ�@�iD@�>�@ȉ�@�$�@���@���@ǧ�@��@ƃ�@�6@Ň�@Ļ�@�d�@��@Þ�@�<6@��8@¤�@�l"@�Ta@�PH@�e�@�H�@��@��
@��'@�[W@��@���@�z�@�(�@���@�|@�j�@�Q�@�@@��@���@���@���@��@���@�j@�Ft@� �@��@��.@�H@���@�0�@��h@�=q@��.@��@@�ȴ@��C@�>�@��_@���@���@���@�ff@�>B@�8�@�c�@��@��I@�E�@��D@�)_@��@��@��;@���@��q@��"@�a@�5�@��@���@��z@�[�@�*�@�
�@��M@��@��P@�q�@�m]@��c@��@�bN@��>@�X@�o@��H@���@��@��@��P@�RT@�5�@��@��@��@��o@��>@�Mj@��D@��$@�S@�i�@�)�@��)@�U�@��@��B@���@��b@��1@���@�s�@�8�@���@���@��S@���@�qv@�n/@�]�@�C�@�@���@�YK@�!@�	@�{@�@��@��W@���@���@�W?@�=@�&@��6@�.�@��@��}@��t@���@��@@���@���@��@�Mj@��@��X@�W�@���@�@�o@��@�V@�6�@�*�@��&@��P@���@���@�	�@��/@���@�bN@�PH@�L0@�GE@�8�@��@��#@���@�zx@���@��j@���@�i�@���@�ƨ@���@��F@��[@��[@��@�7@�X�@��8@��H@���@���@�z�@�tT@�d�@�Ov@�@�4n@��C@�!-@��@���@���@�a|@�6@�e@���@���@�l�@�'�@��s@���@�]d@�,=@��@���@�~�@��@���@�Xy@�)�@�b�@�(@���@��E@���@��}@���@���@�i�@�`�@�Ov@�:*@�u@�@���@��@��@���@���@�R�@�$�@���@���@���@�^�@��@��@���@��e@��A@�s�@�h
@�J�@�
�@��}@���@�`B@�(�@���@���@�oi@�kQ@�V@��@iD@@~�\@~$�@}�n@}%F@|9X@{�q@{Z�@{1�@{.I@{C@{
=@{�@z�@zں@z��@z��@yIR@x�O@x_@x@w
=@v��@v��@vOv@v�@uhs@t��@ty>@s�@s��@sS�@s�@s�@sY@s�@s(@r�@r{�@r$�@q��@q?}@p�@pg8@pG@o�@n��@nkQ@nJ�@n5?@nO@m�@l��@lV�@k�V@ko�@k,�@j�s@j��@j��@j3�@i�t@iq@g�r@gx@f�A@f�@e�^@e��@e�S@eJ�@e	l@d�@c�@c4�@b_�@b-@b	@a��@a�"@`��@`c�@_�@^��@^��@^h
@]�@]^�@\�U@\~(@[��@[o�@[]�@[H�@[8@[�@Z��@Y�@Y \@X�?@X[�@W��@W�@Ws@W_p@W6z@W i@V��@VkQ@V3�@U��@U��@U�@U@@T�`@T��@T9X@S��@S�[@S��@Sl�@S
=@R�y@R͟@R�\@R}V@R+k@Q�o@Q@Q2a@P�)@P��@Py>@P]d@P/�@O�A@O�F@OU�@Oo@N�@N� @N=q@N!�@M�@M/@L�@L��@LQ�@K��@K�f@Kg�@K=@J͟@JYK@J�@I��@I��@I��@If�@I4@I�@H��@H4n@G�@G��@G.I@F�H@F��@Fl�@E��@E+@Dѷ@D~@C�4@CiD@CS�@CJ#@C)_@B��@A�@A�@A��@A:�@@�@@�@@PH@?U�@?Y@?(@>�y@>�F@>xl@>M�@=��@<��@<1@;��@;x@;l�@;W?@;J#@;�@; i@:�y@:ȴ@:��@:�h@:�F@:c @:3�@:{@9��@9��@9\�@9#�@9�@8�`@8��@8e�@8�@7��@7�f@7�{@7Z�@7"�@6��@6z@5�@50�@4Ɇ@4��@4u�@4h�@4U2@44n@3�	@3!-@3�@3�@2��@2@1IR@0�@0`�@0*�@/�4@/RT@.�8@.q�@-�D@-�'@-s�@-X@-J�@-G�@-Dg@-+�@,�P@,�O@,�@+��@+��@+W?@+"�@+�@+�@+@*�"@*�@*��@*ں@*�s@*҉@*�}@*{�@*YK@)�o@)F@(�v@(��@'�W@'o@&�@&��@&�A@&;�@%�@%��@%�@%��@%�@$��@$��@$U2@$�@#�@#��@#�k@#�k@#�@#qv@#�@"��@"�\@"�@!��@!s�@!a�@![W@!<6@ �P@ ��@ �E@ oi@ 1'@�r@��@�V@�V@��@�:@�:@��@Mj@�@��@�L@�+@ff@GE@=q@.�@�@�)@�@��@�M@[W@J�@#�@�@��@I�@�g@�@@�4@W?@,�@�@��@��@�3@�M@Dg@�@֡@Ĝ@�z@oi@/�@�@��@|�@@O@��@��@��@�r@�r@{�@Ta@ �@rG@:�@(�@�@��@�o@?�@�@��@�@��@y�@�@�,@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BUgBU2BU�BW�BYBY�BY�BZ�B_VBw�B�sB��B��B�qB�OB��B	�B�^B�B�iB��B	�B	B	 �B	)*B	*B	+�B	0B	/�B	0oB	1vB	2-B	3�B	5�B	88B	>�B	A;B	N<B	X�B	abB	��B	ƨB	�CB
�B
>�B
@OB
9rB
I�B
g�B
��B
��B
��B
�UB
�;B�B/OB0oB.�B3�B5%B:DB@�BZ7BsMB��B�jB�B��B�RB�UB�OB��B�B��B�RB�QB��B�B�mBg�B0UB
��B
�B
˒B
ĶB
�;B
�B
kkB
Y�B
L�B
H�B
FB
:xB
$�B	�/B	��B	��B	�4B	m]B	UgB	N�B	?cB	&2B		B	 �B��B��B�MB�MB�eB��B�B��B��B�jB	 �B	�B	B		B	HB	B	�B	$�B	4�B	7B	:�B	U�B	`�B	l�B	q'B	�:B	�.B	��B	�B	�cB	�B	��B	�qB	�wB	�}B
UB	��B	��B	��B	��B	��B	�]B	�<B	�BB
 iB
�B
�B	�B	�3B	�B	��B
 �B
�B
�B
SB
�B
B
6B
BB
sB
+�B
9�B
<�B
?B
B[B
I�B
N�B
L�B
KB
MB
M�B
P�B
S[B
S@B
R�B
R�B
OvB
L~B
KDB
K^B
K�B
L~B
LJB
M6B
O�B
QNB
R�B
R�B
T,B
T�B
T�B
R�B
R:B
QB
P�B
PHB
O\B
N�B
L~B
JXB
K)B
K�B
K^B
KB
KB
J�B
K)B
IB
GzB
D�B
A�B
@�B
?}B
=�B
9�B
9$B
@�B
B[B
BuB
BB
AoB
BB
E�B
E�B
EB
>BB
9XB
6�B
49B
0�B
0B
.�B
-�B
-�B
'B
 'B
�B
B
�B
�B
�B
gB
SB
�B
FB
B
�B
�B
�B
�B
�B
mB
�B
�B
�B
 B
�B
KB
�B	�wB	�B	��B	��B	��B	�XB	�$B	��B	��B	�3B	�!B	�/B	�)B	��B	��B	�B	��B	�FB	�TB	�B	�B	�=B	�B	�B	��B	ބB	�HB	�\B	�;B	�5B	ܒB	�WB	ںB	�7B	��B	�KB	�B	��B	ٴB	�QB	�QB	�kB	��B	چB	�kB	ںB	�WB	��B	�CB	ܬB	�dB	�jB	ޞB	�jB	޸B	��B	�pB	ߤB	ߊB	�B	�'B	��B	�vB	��B	��B	�B	�HB	�|B	��B	��B	�B	�nB	��B	�@B	�tB	�B	��B	��B	�LB	�LB	�`B	�B	�
B	�B	�XB	�eB	�"B	�B	��B	�B	��B	�B	�B	�*B	��B	��B	��B	�KB	�B	��B	��B	��B	��B	��B	��B	��B	�nB	��B	��B	�B	��B	��B	��B	��B	�?B	�tB	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�?B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�FB	��B	�LB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	�(B	��B	�.B	�HB	�HB	�.B	�B	�.B	�.B	�.B	��B	��B	�wB	��B	�BB	�(B	�BB	�qB	�VB	�qB	�qB	��B	��B	�qB	�<B	��B	��B	��B	�"B	��B	��B	��B	�BB	�wB	�cB
 iB
 iB
;B
[B
�B
B
[B
[B
'B
'B
'B
[B
AB
'B
'B
[B
{B
YB
�B
�B
	7B
	�B
	lB
	�B

�B

�B
DB
jB
PB
jB
�B
�B
�B
"B
�B
vB
bB
�B
�B
hB
B
�B
4B
�B
TB
�B
&B
&B
[B
@B
oB
 B
oB
B
�B
�B
�B
�B
TB
 B
:B
 B
:B
B
[B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
FB
FB
gB
�B
B
[B
�B
�B
�B
B

B
�B
yB
1B
�B
�B
VB
�B
�B
 B
 B
 B
 \B
 vB
!|B
!�B
!�B
"�B
"�B
"�B
"�B
#:B
#TB
#nB
#�B
$�B
$�B
%,B
%B
&�B
'RB
'�B
'�B
'�B
(
B
(>B
(sB
(�B
(�B
(�B
(�B
(sB
(XB
)B
)�B
)�B
)�B
*B
*�B
+QB
+�B
+kB
+QB
+B
,B
,"B
,qB
,�B
,�B
-B
-)B
-�B
-�B
-�B
-�B
.�B
.IB
.�B
.�B
/B
/iB
/�B
0�B
0�B
0�B
1'B
1[B
1�B
1�B
2B
2-B
2aB
2|B
2aB
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3�B
49B
5?B
5�B
5tB
5�B
5�B
6B
6FB
6�B
6�B
6�B
6�B
7B
7B
6�B
7B
6�B
6�B
7B
7LB
7�B
7�B
8B
88B
8lB
8�B
9rB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;dB
;dB
;B
;�B
;�B
<B
<B
<�B
<�B
=�B
=qB
>]B
>]B
>wB
>wB
>�B
>�B
>�B
>�B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
A B
AB
A�B
B�B
B[B
B�B
B�B
CaB
C{B
C�B
DMB
DgB
DgB
D�B
D�B
D�B
EB
FB
F?B
FYB
FtB
GB
GB
F�B
F�B
G+B
G_B
G�B
G�B
G�B
G�B
HB
HB
HfB
HfB
HfB
H�B
I7B
IRB
I7B
I7B
IRB
I7B
IRB
IRB
IRB
IlB
IlB
IRB
I�B
I�B
J#B
J	B
J#B
J#B
JXB
J�B
KB
KDB
K)B
KxB
K�B
K�B
LdB
LdB
L�B
L�B
MB
MPB
M�B
M�B
M�B
NB
N�B
N�B
N�B
N�B
OB
OB
O(B
OBB
O�B
PB
P.B
PbB
P�B
P�B
P�B
P�B
Q�B
RB
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
U�B
U�B
U�B
U�B
VB
VB
VB
V�B
W�B
W�B
X+B
X_B
X_B
X_B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y1B
Y1B
YB
Y�B
Y�B
ZB
ZB
ZB
Z7B
ZkB
Z�B
[#B
[#B
[	B
[=B
[WB
[�B
[�B
\)B
\�B
]/B
]IB
]IB
]dB
]IB
]/B
]�B
]�B
]�B
]�B
]~B
]�B
]�B
]�B
^jB
^�B
_�B
`\B
_�B
_!B
^�B
^�B
_B
_B
_B
_!B
_B
_!B
_VB
_pB
`BB
`\B
`\B
`�B
aB
aB
a-B
a-B
a-B
a-B
aHB
abB
aHB
aHB
abB
a�B
a|B
a�B
b�B
c:B
cTB
c�B
e,B
eFB
e`B
e�B
e�B
fLB
ffB
ffB
f�B
f�B
gB
gmB
g�B
g�B
h$B
h>B
h>B
h>B
h>B
h$B
hXB
hsB
h�B
i*B
i�B
i�B
i�B
i�B
i�B
jKB
jKB
jKB
j�B
j�B
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
l�B
mB
mB
mCB
m]B
m�B
m�B
m�B
m�B
m�B
nB
ncB
nIB
o B
o5B
oOB
o�B
o�B
o�B
o�B
p;B
p�B
poB
p�B
p�B
q'B
q'B
qAB
qAB
qvB
q�B
q�B
q�B
r-B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
tnB
t�B
uB
u%B
u%B
u%B
u?B
v`B
vB
v+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BUgBU2BU�BW�BYBY�BY�BZ�B_VBw�B�sB��B��B�qB�OB��B	�B�^B�B�iB��B	�B	B	 �B	)*B	*B	+�B	0B	/�B	0oB	1vB	2-B	3�B	5�B	88B	>�B	A;B	N<B	X�B	abB	��B	ƨB	�CB
�B
>�B
@OB
9rB
I�B
g�B
��B
��B
��B
�UB
�;B�B/OB0oB.�B3�B5%B:DB@�BZ7BsMB��B�jB�B��B�RB�UB�OB��B�B��B�RB�QB��B�B�mBg�B0UB
��B
�B
˒B
ĶB
�;B
�B
kkB
Y�B
L�B
H�B
FB
:xB
$�B	�/B	��B	��B	�4B	m]B	UgB	N�B	?cB	&2B		B	 �B��B��B�MB�MB�eB��B�B��B��B�jB	 �B	�B	B		B	HB	B	�B	$�B	4�B	7B	:�B	U�B	`�B	l�B	q'B	�:B	�.B	��B	�B	�cB	�B	��B	�qB	�wB	�}B
UB	��B	��B	��B	��B	��B	�]B	�<B	�BB
 iB
�B
�B	�B	�3B	�B	��B
 �B
�B
�B
SB
�B
B
6B
BB
sB
+�B
9�B
<�B
?B
B[B
I�B
N�B
L�B
KB
MB
M�B
P�B
S[B
S@B
R�B
R�B
OvB
L~B
KDB
K^B
K�B
L~B
LJB
M6B
O�B
QNB
R�B
R�B
T,B
T�B
T�B
R�B
R:B
QB
P�B
PHB
O\B
N�B
L~B
JXB
K)B
K�B
K^B
KB
KB
J�B
K)B
IB
GzB
D�B
A�B
@�B
?}B
=�B
9�B
9$B
@�B
B[B
BuB
BB
AoB
BB
E�B
E�B
EB
>BB
9XB
6�B
49B
0�B
0B
.�B
-�B
-�B
'B
 'B
�B
B
�B
�B
�B
gB
SB
�B
FB
B
�B
�B
�B
�B
�B
mB
�B
�B
�B
 B
�B
KB
�B	�wB	�B	��B	��B	��B	�XB	�$B	��B	��B	�3B	�!B	�/B	�)B	��B	��B	�B	��B	�FB	�TB	�B	�B	�=B	�B	�B	��B	ބB	�HB	�\B	�;B	�5B	ܒB	�WB	ںB	�7B	��B	�KB	�B	��B	ٴB	�QB	�QB	�kB	��B	چB	�kB	ںB	�WB	��B	�CB	ܬB	�dB	�jB	ޞB	�jB	޸B	��B	�pB	ߤB	ߊB	�B	�'B	��B	�vB	��B	��B	�B	�HB	�|B	��B	��B	�B	�nB	��B	�@B	�tB	�B	��B	��B	�LB	�LB	�`B	�B	�
B	�B	�XB	�eB	�"B	�B	��B	�B	��B	�B	�B	�*B	��B	��B	��B	�KB	�B	��B	��B	��B	��B	��B	��B	��B	�nB	��B	��B	�B	��B	��B	��B	��B	�?B	�tB	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�?B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�FB	��B	�LB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	�(B	��B	�.B	�HB	�HB	�.B	�B	�.B	�.B	�.B	��B	��B	�wB	��B	�BB	�(B	�BB	�qB	�VB	�qB	�qB	��B	��B	�qB	�<B	��B	��B	��B	�"B	��B	��B	��B	�BB	�wB	�cB
 iB
 iB
;B
[B
�B
B
[B
[B
'B
'B
'B
[B
AB
'B
'B
[B
{B
YB
�B
�B
	7B
	�B
	lB
	�B

�B

�B
DB
jB
PB
jB
�B
�B
�B
"B
�B
vB
bB
�B
�B
hB
B
�B
4B
�B
TB
�B
&B
&B
[B
@B
oB
 B
oB
B
�B
�B
�B
�B
TB
 B
:B
 B
:B
B
[B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
FB
FB
gB
�B
B
[B
�B
�B
�B
B

B
�B
yB
1B
�B
�B
VB
�B
�B
 B
 B
 B
 \B
 vB
!|B
!�B
!�B
"�B
"�B
"�B
"�B
#:B
#TB
#nB
#�B
$�B
$�B
%,B
%B
&�B
'RB
'�B
'�B
'�B
(
B
(>B
(sB
(�B
(�B
(�B
(�B
(sB
(XB
)B
)�B
)�B
)�B
*B
*�B
+QB
+�B
+kB
+QB
+B
,B
,"B
,qB
,�B
,�B
-B
-)B
-�B
-�B
-�B
-�B
.�B
.IB
.�B
.�B
/B
/iB
/�B
0�B
0�B
0�B
1'B
1[B
1�B
1�B
2B
2-B
2aB
2|B
2aB
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
3�B
49B
5?B
5�B
5tB
5�B
5�B
6B
6FB
6�B
6�B
6�B
6�B
7B
7B
6�B
7B
6�B
6�B
7B
7LB
7�B
7�B
8B
88B
8lB
8�B
9rB
9�B
9�B
9�B
9�B
:*B
:�B
:�B
;dB
;dB
;B
;�B
;�B
<B
<B
<�B
<�B
=�B
=qB
>]B
>]B
>wB
>wB
>�B
>�B
>�B
>�B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
A B
AB
A�B
B�B
B[B
B�B
B�B
CaB
C{B
C�B
DMB
DgB
DgB
D�B
D�B
D�B
EB
FB
F?B
FYB
FtB
GB
GB
F�B
F�B
G+B
G_B
G�B
G�B
G�B
G�B
HB
HB
HfB
HfB
HfB
H�B
I7B
IRB
I7B
I7B
IRB
I7B
IRB
IRB
IRB
IlB
IlB
IRB
I�B
I�B
J#B
J	B
J#B
J#B
JXB
J�B
KB
KDB
K)B
KxB
K�B
K�B
LdB
LdB
L�B
L�B
MB
MPB
M�B
M�B
M�B
NB
N�B
N�B
N�B
N�B
OB
OB
O(B
OBB
O�B
PB
P.B
PbB
P�B
P�B
P�B
P�B
Q�B
RB
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
TFB
T�B
T�B
T�B
U�B
U�B
U�B
U�B
VB
VB
VB
V�B
W�B
W�B
X+B
X_B
X_B
X_B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y1B
Y1B
YB
Y�B
Y�B
ZB
ZB
ZB
Z7B
ZkB
Z�B
[#B
[#B
[	B
[=B
[WB
[�B
[�B
\)B
\�B
]/B
]IB
]IB
]dB
]IB
]/B
]�B
]�B
]�B
]�B
]~B
]�B
]�B
]�B
^jB
^�B
_�B
`\B
_�B
_!B
^�B
^�B
_B
_B
_B
_!B
_B
_!B
_VB
_pB
`BB
`\B
`\B
`�B
aB
aB
a-B
a-B
a-B
a-B
aHB
abB
aHB
aHB
abB
a�B
a|B
a�B
b�B
c:B
cTB
c�B
e,B
eFB
e`B
e�B
e�B
fLB
ffB
ffB
f�B
f�B
gB
gmB
g�B
g�B
h$B
h>B
h>B
h>B
h>B
h$B
hXB
hsB
h�B
i*B
i�B
i�B
i�B
i�B
i�B
jKB
jKB
jKB
j�B
j�B
k6B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
l�B
mB
mB
mCB
m]B
m�B
m�B
m�B
m�B
m�B
nB
ncB
nIB
o B
o5B
oOB
o�B
o�B
o�B
o�B
p;B
p�B
poB
p�B
p�B
q'B
q'B
qAB
qAB
qvB
q�B
q�B
q�B
r-B
r-B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
s�B
s�B
tnB
t�B
uB
u%B
u%B
u%B
u?B
v`B
vB
v+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104953  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175225  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175225  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175225                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025232  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025232  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                