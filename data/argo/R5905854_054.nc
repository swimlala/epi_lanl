CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:12Z creation;2022-06-04T17:54:13Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175412  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               6A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�,,����1   @�,-OW�@/׍O�;d�c����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @���@���AffA@  A`  A�  A�  A�  A�  A�33A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B�  B�  B�  B�  B���B�ffB�B�  B�  C   C�fC  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.L�C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@���@�fg@�fgA33A@��A`��A�ffA�ffA�ffA�ffA���A�ffA�33A�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B̀ B�L�B��B��B��B��B��B��B��gB� B�4B��B��C �C�3C�C�C&gC
&gC�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(&gC*�C,�C.Y�C/�3C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf&gCh�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr	�Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�DǾgD��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D�~gD���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�;4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%�A�$@A�%�A�%FA�&�A�'�A�*�A�*eA�+6A�6FA�0UA�,A�/A�6A�5?A�5�A�=<A�>�A�>BA�=qA�?A�?�A�@�A�B�A�>A�<�A�2-A�.IA�#:A�1A���A���A�.Aџ�Aђ�A�}�A�e`A�ҽA��A��+A�jA�jAˉ�A�~�AȅA�.IAŉ�AĽ�A�C�A���A���A��]A�K)A��A�<A�c A��A���A��A��A��nA���A�9XA��A�d�A��
A��A���A���A�$�A�~�A�� A�p�A�O�A�X�A�7A�A��A���A��
A�m�A��YA��UA��A��?A��Azi�Av��Aq��AoB�Ai.IAg�FAc�=A_��A\9�AX�yAV�SAU \AS�AR"�AQCAP�AL�AK�AG�AE�AD_�AB%FA@)�A>��A=��A<�KA;�PA9�3A8�xA8*0A7�A8OA7[�A7�A5�A3�RA1��A/�	A.�A-�qA,��A+�	A+��A+�A+�A+I�A)�zA(��A(HA%�A#B�A!��A�gAF�A�.AN�A2�A˒A��A�4A�6A1�A��AR�AxlAl�A(�A��A��A�A�3A�dA�&AzA�>Ac AoA��A+�AW�AdZAdZA�AoiA^�A*0A�A�?A��A�|A^5A��A�)A_Aw2A��A�MA��AZ�A�A�A[�AhsAM�A�=A�uA7LA
��A
($A
Y�A
�{A	�aA	��A	��A	��A
'RA
2aA	�]A	��A	)�A�5A�2A}VA� AI�A@A�A\�A�#AiDA�A��A�UAeA ��A ƨAYA/�A �A ��A T�A *�A �A 1�A :*@��D@��@�^�@�)�@���@�B[@�~@�O@�A�@��Z@��*@�/�@�7�@���@�S@�`�@��m@�@O@�o@�~�@���@�^@��@��@��@�zx@���@�6�@�M@웦@� �@��@�f�@�(@�2�@�@��@��A@�$@�u@�6@�P@�:�@�!-@�V�@��"@���@�Z�@�'�@ව@߷@��p@��@݅@ܷ�@�2�@�L�@�%�@�A�@ؠ�@� �@���@�l�@�xl@չ�@՗�@�:�@��@�ی@Ա�@�_�@�	@Ӵ�@��@Ҙ_@�v�@љ�@��@��B@Ќ�@�W�@�@��6@�j�@�C�@�ی@γh@��@��@�s@��"@̰�@�v�@�4n@��@˧�@�X@ʬ�@�@�@���@�s�@�8�@��@�4@�zx@�@��[@�~(@�C-@���@ŷ@�e�@��B@�v�@�Xy@�˒@Ë�@��P@F@�Q�@��@�hs@���@�	@���@��A@�X@�ѷ@���@���@���@���@���@��,@��e@��z@�,�@���@��@���@��n@��q@�dZ@�!�@���@�M@���@���@�#:@�_@��@�J@�hs@�%F@��@���@���@��W@�F�@���@��D@���@�X�@�@��@���@���@�4@�$t@���@��@�Q@�&�@��@��m@��s@���@�#:@���@�.I@�kQ@��}@��@�Ta@��X@��|@��_@���@�%F@��f@��@���@���@�O@��m@�y>@��@��A@���@��@��s@���@�Xy@�%�@��]@��6@���@�)_@���@�p;@��@���@��M@�N<@���@���@�-�@��-@�j@�RT@�(@�w�@���@��@�s�@�>�@��@��<@�{�@�6@��>@���@���@�x�@�Vm@�Dg@��@��@�r�@�-@�خ@��t@�qv@�q@��K@��@�Ov@�,=@��@�G@��-@�L�@�+@�@��@�ی@��@���@�M@�� @��[@���@�O@��@��@��s@���@�7�@��@���@���@���@��4@�J#@���@��O@��@�^5@�%�@�x@�ݘ@��@���@��	@�H�@�&@��"@�ѷ@���@��o@�L0@�b@���@��@@�a�@�4�@��@��j@��@�4n@�	@�خ@��w@���@��X@���@�L�@�@�ی@���@��\@�h
@�@�خ@�m]@�=�@�0�@�%F@�@@���@���@�G@�`B@��K@�^5@�-�@�?@���@���@���@�v`@�Q�@��@���@���@�i�@�4@�1@��K@�w2@�e,@�{J@�X�@�5�@��@��@���@���@�C�@��)@���@�@��!@���@�-@��@Z�@~҉@~)�@}��@}�C@}s�@}5�@|��@{|�@z�@z:*@y��@x��@xK^@w�@w��@w�@v��@u�@t��@t9X@t1@sS@ri�@q��@q�@p�@p9X@p7@o�@o��@n��@nff@n+k@m�@m��@m�n@m��@mw2@mp�@m=�@l�@lc�@k��@k��@k6z@j��@j�2@jȴ@jTa@i�@i��@iVm@h�E@h-�@g�A@g�K@g��@g��@g�f@gdZ@gC@fȴ@f�F@fu%@fW�@e��@e��@e��@e�~@e+@d��@d�@d�u@dXy@d7@dG@c��@cS@b�r@b)�@a��@ak�@`�f@`I�@_�P@_C�@^�@^�@^:*@]�d@]x�@\�K@\_@[�W@[�:@[s@[4�@Z��@Z�@Zq�@Zd�@Y��@YB�@Y0�@X�P@X�I@W�g@W{J@W=@W6z@WS@VC�@U��@U�@T��@T6@S��@S=@R�h@Rxl@RH�@Q��@Q��@Q%F@P�	@Pѷ@P/�@O�
@O��@O1�@N)�@M�)@M�@M2a@L�@L�@L��@LV�@L@K��@K�@J�1@J��@J�F@JkQ@I�>@Iϫ@I�@I�H@I��@I��@If�@IIR@I@H��@H�$@HbN@H�@G�$@G$t@F�"@F��@F�6@F�x@F��@F�}@F��@F�1@F3�@E�h@EIR@E�@D��@D�@DtT@D$@C�6@Cƨ@C�q@Cg�@C'�@B�]@BTa@A��@A��@Ao @A8�@A%@@�)@@~(@@D�@?�@@?b�@?8@?�@>�R@>8�@=��@=m]@=<6@=V@<�v@<Ĝ@<��@<I�@;��@;U�@:�!@:L0@:3�@9��@9o @9X@9(�@8�p@8�@7��@7/�@6��@6�6@6~�@6ff@6($@5��@5�@4ی@4�)@4�_@4e�@3��@2ں@2q�@2C�@2-@2J@1�@1}�@0��@0%�@/�@/��@/e�@/=@.�@.��@.p;@.GE@-��@-@-��@-?}@,�@,��@,|�@,M@+��@+��@+�4@+8@+�@*�]@*��@*��@*q�@*Z�@*1�@)�@)��@)��@)u�@)Y�@)8�@)+�@(�P@(�j@(K^@(�@'�g@'�k@'\)@'$t@'@&�H@&V@%�3@%�=@%��@%�"@%j@%!�@$��@$�D@$l"@#�]@#��@#�F@#~�@#b�@#A�@"��@"�r@"ff@"=q@"O@" �@!�@!�X@!��@!o @! \@ ��@ �@ m�@ C-@ !@ !@ "h@ G@خ@��@Mj@�@�s@��@�\@+k@�T@�S@(�@�f@��@�z@_@K^@<�@�@�K@�k@��@_p@E9@=@+@�@��@��@YK@�@�D@�9@��@��@m]@Q�@@��@y>@j@H@,=@*�@(�@"h@�r@��@o�@9�@/�@�8@҉@�m@��@�R@�x@��@l�@3�@&�@#:@J@��@zx@w2@N<@	l@�@ѷ@�@�z@�@Z@A�@�@�;@ƨ@��@�4@Z�@E9@"�@o@�M@��@�+@E�@O@�@��@��@�X@�"@|@f�@J�@@�@�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%�A�$@A�%�A�%FA�&�A�'�A�*�A�*eA�+6A�6FA�0UA�,A�/A�6A�5?A�5�A�=<A�>�A�>BA�=qA�?A�?�A�@�A�B�A�>A�<�A�2-A�.IA�#:A�1A���A���A�.Aџ�Aђ�A�}�A�e`A�ҽA��A��+A�jA�jAˉ�A�~�AȅA�.IAŉ�AĽ�A�C�A���A���A��]A�K)A��A�<A�c A��A���A��A��A��nA���A�9XA��A�d�A��
A��A���A���A�$�A�~�A�� A�p�A�O�A�X�A�7A�A��A���A��
A�m�A��YA��UA��A��?A��Azi�Av��Aq��AoB�Ai.IAg�FAc�=A_��A\9�AX�yAV�SAU \AS�AR"�AQCAP�AL�AK�AG�AE�AD_�AB%FA@)�A>��A=��A<�KA;�PA9�3A8�xA8*0A7�A8OA7[�A7�A5�A3�RA1��A/�	A.�A-�qA,��A+�	A+��A+�A+�A+I�A)�zA(��A(HA%�A#B�A!��A�gAF�A�.AN�A2�A˒A��A�4A�6A1�A��AR�AxlAl�A(�A��A��A�A�3A�dA�&AzA�>Ac AoA��A+�AW�AdZAdZA�AoiA^�A*0A�A�?A��A�|A^5A��A�)A_Aw2A��A�MA��AZ�A�A�A[�AhsAM�A�=A�uA7LA
��A
($A
Y�A
�{A	�aA	��A	��A	��A
'RA
2aA	�]A	��A	)�A�5A�2A}VA� AI�A@A�A\�A�#AiDA�A��A�UAeA ��A ƨAYA/�A �A ��A T�A *�A �A 1�A :*@��D@��@�^�@�)�@���@�B[@�~@�O@�A�@��Z@��*@�/�@�7�@���@�S@�`�@��m@�@O@�o@�~�@���@�^@��@��@��@�zx@���@�6�@�M@웦@� �@��@�f�@�(@�2�@�@��@��A@�$@�u@�6@�P@�:�@�!-@�V�@��"@���@�Z�@�'�@ව@߷@��p@��@݅@ܷ�@�2�@�L�@�%�@�A�@ؠ�@� �@���@�l�@�xl@չ�@՗�@�:�@��@�ی@Ա�@�_�@�	@Ӵ�@��@Ҙ_@�v�@љ�@��@��B@Ќ�@�W�@�@��6@�j�@�C�@�ی@γh@��@��@�s@��"@̰�@�v�@�4n@��@˧�@�X@ʬ�@�@�@���@�s�@�8�@��@�4@�zx@�@��[@�~(@�C-@���@ŷ@�e�@��B@�v�@�Xy@�˒@Ë�@��P@F@�Q�@��@�hs@���@�	@���@��A@�X@�ѷ@���@���@���@���@���@��,@��e@��z@�,�@���@��@���@��n@��q@�dZ@�!�@���@�M@���@���@�#:@�_@��@�J@�hs@�%F@��@���@���@��W@�F�@���@��D@���@�X�@�@��@���@���@�4@�$t@���@��@�Q@�&�@��@��m@��s@���@�#:@���@�.I@�kQ@��}@��@�Ta@��X@��|@��_@���@�%F@��f@��@���@���@�O@��m@�y>@��@��A@���@��@��s@���@�Xy@�%�@��]@��6@���@�)_@���@�p;@��@���@��M@�N<@���@���@�-�@��-@�j@�RT@�(@�w�@���@��@�s�@�>�@��@��<@�{�@�6@��>@���@���@�x�@�Vm@�Dg@��@��@�r�@�-@�خ@��t@�qv@�q@��K@��@�Ov@�,=@��@�G@��-@�L�@�+@�@��@�ی@��@���@�M@�� @��[@���@�O@��@��@��s@���@�7�@��@���@���@���@��4@�J#@���@��O@��@�^5@�%�@�x@�ݘ@��@���@��	@�H�@�&@��"@�ѷ@���@��o@�L0@�b@���@��@@�a�@�4�@��@��j@��@�4n@�	@�خ@��w@���@��X@���@�L�@�@�ی@���@��\@�h
@�@�خ@�m]@�=�@�0�@�%F@�@@���@���@�G@�`B@��K@�^5@�-�@�?@���@���@���@�v`@�Q�@��@���@���@�i�@�4@�1@��K@�w2@�e,@�{J@�X�@�5�@��@��@���@���@�C�@��)@���@�@��!@���@�-@��@Z�@~҉@~)�@}��@}�C@}s�@}5�@|��@{|�@z�@z:*@y��@x��@xK^@w�@w��@w�@v��@u�@t��@t9X@t1@sS@ri�@q��@q�@p�@p9X@p7@o�@o��@n��@nff@n+k@m�@m��@m�n@m��@mw2@mp�@m=�@l�@lc�@k��@k��@k6z@j��@j�2@jȴ@jTa@i�@i��@iVm@h�E@h-�@g�A@g�K@g��@g��@g�f@gdZ@gC@fȴ@f�F@fu%@fW�@e��@e��@e��@e�~@e+@d��@d�@d�u@dXy@d7@dG@c��@cS@b�r@b)�@a��@ak�@`�f@`I�@_�P@_C�@^�@^�@^:*@]�d@]x�@\�K@\_@[�W@[�:@[s@[4�@Z��@Z�@Zq�@Zd�@Y��@YB�@Y0�@X�P@X�I@W�g@W{J@W=@W6z@WS@VC�@U��@U�@T��@T6@S��@S=@R�h@Rxl@RH�@Q��@Q��@Q%F@P�	@Pѷ@P/�@O�
@O��@O1�@N)�@M�)@M�@M2a@L�@L�@L��@LV�@L@K��@K�@J�1@J��@J�F@JkQ@I�>@Iϫ@I�@I�H@I��@I��@If�@IIR@I@H��@H�$@HbN@H�@G�$@G$t@F�"@F��@F�6@F�x@F��@F�}@F��@F�1@F3�@E�h@EIR@E�@D��@D�@DtT@D$@C�6@Cƨ@C�q@Cg�@C'�@B�]@BTa@A��@A��@Ao @A8�@A%@@�)@@~(@@D�@?�@@?b�@?8@?�@>�R@>8�@=��@=m]@=<6@=V@<�v@<Ĝ@<��@<I�@;��@;U�@:�!@:L0@:3�@9��@9o @9X@9(�@8�p@8�@7��@7/�@6��@6�6@6~�@6ff@6($@5��@5�@4ی@4�)@4�_@4e�@3��@2ں@2q�@2C�@2-@2J@1�@1}�@0��@0%�@/�@/��@/e�@/=@.�@.��@.p;@.GE@-��@-@-��@-?}@,�@,��@,|�@,M@+��@+��@+�4@+8@+�@*�]@*��@*��@*q�@*Z�@*1�@)�@)��@)��@)u�@)Y�@)8�@)+�@(�P@(�j@(K^@(�@'�g@'�k@'\)@'$t@'@&�H@&V@%�3@%�=@%��@%�"@%j@%!�@$��@$�D@$l"@#�]@#��@#�F@#~�@#b�@#A�@"��@"�r@"ff@"=q@"O@" �@!�@!�X@!��@!o @! \@ ��@ �@ m�@ C-@ !@ !@ "h@ G@خ@��@Mj@�@�s@��@�\@+k@�T@�S@(�@�f@��@�z@_@K^@<�@�@�K@�k@��@_p@E9@=@+@�@��@��@YK@�@�D@�9@��@��@m]@Q�@@��@y>@j@H@,=@*�@(�@"h@�r@��@o�@9�@/�@�8@҉@�m@��@�R@�x@��@l�@3�@&�@#:@J@��@zx@w2@N<@	l@�@ѷ@�@�z@�@Z@A�@�@�;@ƨ@��@�4@Z�@E9@"�@o@�M@��@�+@E�@O@�@��@��@�X@�"@|@f�@J�@@�@�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	o B	n�B	n�B	n�B	n�B	n�B	o B	oB	n�B	o5B	oB	n�B	n�B	n�B	oiB	oOB	oB	o5B	o5B	o B	o5B	o5B	o5B	o5B	n�B	n�B	m�B	mB	l"B	j�B	i�B	eFB	^�B	Z�B	\xB	^�B	]�B	QNB	@�B	:B	6+B	0;B	!HB	pB	B	DB	�B	aB	-�B	M�B	�B	�B
7�B
>(B
PbB
p;B
ðBB
��B
=B`BB`B{�B��BlB��B� B�JB��B��B�OB�CB��Bd�BE�BIB
�B
��B
�kB
^5B
/B	��B	��B	�QB	�\B	��B	��B	�qB	�B	t�B	Y�B	N�B	;B	%B	�B	�B	 �B�B�`B��B��B��BݲB��B�oB��BƎB�B��B��B�zB��B��B��B��B��B��BΥB�!B��B	�B�>B�IB��B�6B��B�B��B��B�XB	PB	�B	
B	FB	�B	�B	B�B��B�-B��B�dB��B�9B�B��B�>B	MB	�B	'�B	1�B	;0B	?�B	@OB	J#B	]/B	gmB	i�B	o�B	t�B	w�B	|�B	}�B	w�B	v�B	�UB	��B	��B	�mB	�gB	��B	�gB	��B	��B	�VB	��B	��B	��B	�B	��B	�%B	�qB	��B	�B	�aB	��B	��B	�eB	�%B	��B	�hB	�WB	�B	��B	��B	�GB	�RB	�+B	�B	��B	уB	�)B	߾B	�|B	�\B	��B	�9B	�eB	��B	�SB	��B	յB	�B	��B	ʌB	��B	��B	��B	��B	��B	�B	�-B	��B	��B	��B	ӏB	ѝB	��B	��B	�_B	ܬB	��B	�B	ݲB	�B	�B	�B	�=B	ۦB	��B	چB	��B	�B	ٴB	خB	�+B	׍B	�?B	ևB	յB	�mB	�sB	�EB	ٚB	چB	��B	چB	�#B	��B	�7B	�kB	�kB	ںB	�	B	�#B	�B	یB	��B	�WB	�#B	��B	�/B	�CB	��B	ۦB	�xB	�5B	߾B	��B	��B	�-B	�HB	�B	�-B	�HB	�-B	�B	�B	�-B	�BB	��B	�vB	�B	�\B	��B	��B	��B	�HB	�B	�B	��B	��B	�B	�-B	�vB	�vB	�B	�B	�B	�B	�hB	�NB	�NB	�B	�:B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�LB	�B	��B	�B	�mB	�sB	�B	��B	�B	��B	�XB	�$B	�sB	�B	�_B	��B	�B	�6B	�)B	��B	�B	�wB	��B	�aB	�B	�B	�MB	��B	�B	�AB	�B	�AB	�|B	�hB	�B	�%B	��B	�B	�B	�B	��B	�LB	��B	��B	��B	�<B	��B	�"B	��B	�B	��B	��B	�%B	��B	��B	��B	��B	��B	�	B	�*B	��B	�^B	��B	��B	�`B	��B	��B	��B	�*B	�cB
�B
�B
�B
�B
mB
�B
gB
�B
[B
B
�B
B
3B
�B
�B
�B
�B
 B	��B	�<B	�jB	��B	�DB	��B	�VB	�VB	�B	��B	��B	��B	�6B	�<B	��B
 OB
 B
�B
�B
�B
�B
�B
�B
SB
mB
�B
%B
YB
tB
YB
�B
zB
�B
B
1B
B
�B
	RB
	�B

#B

#B

	B
	�B

=B

�B

�B
�B
�B
�B
�B
B
6B
jB
�B
"B
pB
BB
B
BB
�B
�B
�B
�B
�B
}B
�B
4B
�B
�B
B
:B
:B
 B
�B
�B
&B
[B
@B
�B
FB
FB
FB
�B
�B
2B
�B
�B
�B
�B
B
�B
$B
sB
�B
B
EB
�B
�B
�B
KB
�B
�B
B
�B
#B
WB
�B
CB
xB
�B
�B
B
IB
�B
B
B
�B
B
pB
�B
�B
 B
 'B
 BB
 �B
 �B
 �B
 �B
 \B
 �B
 vB
!|B
!�B
"B
"NB
"4B
!�B
!HB
!-B
 �B
 �B
!HB
!�B
!�B
!�B
"�B
$B
$�B
%B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'B
(�B
)*B
)*B
)*B
)�B
)�B
*B
*B
*B
*eB
(�B
'�B
(�B
'�B
'�B
(XB
(XB
(sB
(�B
(�B
)DB
(�B
)_B
*eB
+6B
+B
+6B
+�B
,B
,WB
,�B
,�B
-B
-]B
.cB
.}B
.}B
/5B
/OB
/iB
0B
0�B
0�B
0�B
1[B
1[B
1�B
1�B
1�B
2GB
2GB
2aB
2�B
2|B
2|B
2|B
2GB
2GB
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4B
4�B
5?B
5�B
6B
6+B
6`B
6�B
7B
7B
72B
72B
7LB
72B
7LB
7�B
7�B
8B
9	B
9�B
9$B
8�B
8�B
9>B
9�B
9�B
9�B
:*B
9�B
:^B
:�B
;B
;�B
;�B
;�B
;�B
<6B
<�B
<�B
="B
=<B
=�B
=�B
>BB
>]B
>�B
>�B
>�B
?HB
?�B
?�B
?�B
@�B
AB
A B
AUB
A�B
B'B
A�B
A�B
BB
BAB
CB
B�B
CaB
C�B
C�B
C�B
D3B
DgB
D�B
E9B
F?B
F�B
G_B
GEB
G�B
HB
G�B
H1B
H�B
IB
H�B
IB
IRB
I�B
I�B
I�B
J#B
JXB
J�B
K�B
K�B
K�B
K�B
K�B
LB
LB
K�B
LB
L0B
L0B
LJB
L~B
L�B
L�B
MB
L�B
M6B
M�B
M�B
NpB
N�B
O(B
OvB
PB
P�B
P�B
Q4B
Q4B
Q�B
RB
RTB
R�B
R�B
RoB
RTB
RoB
R�B
SB
S&B
S@B
SB
S[B
S[B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T�B
T�B
T�B
UB
UB
U�B
U�B
VB
VB
V9B
VSB
V9B
V�B
V�B
V�B
W?B
XB
XEB
XEB
X�B
Y1B
Y1B
YB
YeB
YKB
Y�B
YeB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[	B
[#B
[WB
[�B
\�B
\�B
\�B
\�B
]B
]B
]IB
]�B
]�B
]�B
^B
^OB
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_pB
_�B
`B
`BB
`BB
`�B
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
c:B
c�B
c�B
c�B
c�B
dB
d@B
d@B
dtB
d�B
d�B
eB
e,B
ezB
e�B
e�B
fLB
ffB
f�B
f�B
f�B
f�B
gmB
g�B
h
B
h
B
hsB
hXB
hsB
h�B
h�B
iB
i�B
i�B
i�B
jB
jB
j0B
jKB
j0B
j0B
j0B
jKB
j0B
jB
jB
jB
jKB
j0B
j0B
jeB
jKB
jB
j�B
j�B
kB
kB
kQB
k�B
k�B
l"B
l�B
l�B
l�B
mB
m]B
m]B
mwB
mwB
m�B
m�B
m�B
nB
nIB
nIB
nIB
ncB
ncB
n�B
n�B
o5B
oiB
o�B
o�B
poB
poB
p�B
p�B
qAB
q'B
qAB
q�B
q�B
q�B
q�B
qvB
q�B
q�B
q�B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sB
s3B
s�B
s�B
s�B
s�B
tB
tB
tTB
tnB
t�B
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v`B
v`B
v�B
v�B
w2B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	o B	n�B	n�B	n�B	n�B	n�B	o B	oB	n�B	o5B	oB	n�B	n�B	n�B	oiB	oOB	oB	o5B	o5B	o B	o5B	o5B	o5B	o5B	n�B	n�B	m�B	mB	l"B	j�B	i�B	eFB	^�B	Z�B	\xB	^�B	]�B	QNB	@�B	:B	6+B	0;B	!HB	pB	B	DB	�B	aB	-�B	M�B	�B	�B
7�B
>(B
PbB
p;B
ðBB
��B
=B`BB`B{�B��BlB��B� B�JB��B��B�OB�CB��Bd�BE�BIB
�B
��B
�kB
^5B
/B	��B	��B	�QB	�\B	��B	��B	�qB	�B	t�B	Y�B	N�B	;B	%B	�B	�B	 �B�B�`B��B��B��BݲB��B�oB��BƎB�B��B��B�zB��B��B��B��B��B��BΥB�!B��B	�B�>B�IB��B�6B��B�B��B��B�XB	PB	�B	
B	FB	�B	�B	B�B��B�-B��B�dB��B�9B�B��B�>B	MB	�B	'�B	1�B	;0B	?�B	@OB	J#B	]/B	gmB	i�B	o�B	t�B	w�B	|�B	}�B	w�B	v�B	�UB	��B	��B	�mB	�gB	��B	�gB	��B	��B	�VB	��B	��B	��B	�B	��B	�%B	�qB	��B	�B	�aB	��B	��B	�eB	�%B	��B	�hB	�WB	�B	��B	��B	�GB	�RB	�+B	�B	��B	уB	�)B	߾B	�|B	�\B	��B	�9B	�eB	��B	�SB	��B	յB	�B	��B	ʌB	��B	��B	��B	��B	��B	�B	�-B	��B	��B	��B	ӏB	ѝB	��B	��B	�_B	ܬB	��B	�B	ݲB	�B	�B	�B	�=B	ۦB	��B	چB	��B	�B	ٴB	خB	�+B	׍B	�?B	ևB	յB	�mB	�sB	�EB	ٚB	چB	��B	چB	�#B	��B	�7B	�kB	�kB	ںB	�	B	�#B	�B	یB	��B	�WB	�#B	��B	�/B	�CB	��B	ۦB	�xB	�5B	߾B	��B	��B	�-B	�HB	�B	�-B	�HB	�-B	�B	�B	�-B	�BB	��B	�vB	�B	�\B	��B	��B	��B	�HB	�B	�B	��B	��B	�B	�-B	�vB	�vB	�B	�B	�B	�B	�hB	�NB	�NB	�B	�:B	��B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�LB	�B	��B	�B	�mB	�sB	�B	��B	�B	��B	�XB	�$B	�sB	�B	�_B	��B	�B	�6B	�)B	��B	�B	�wB	��B	�aB	�B	�B	�MB	��B	�B	�AB	�B	�AB	�|B	�hB	�B	�%B	��B	�B	�B	�B	��B	�LB	��B	��B	��B	�<B	��B	�"B	��B	�B	��B	��B	�%B	��B	��B	��B	��B	��B	�	B	�*B	��B	�^B	��B	��B	�`B	��B	��B	��B	�*B	�cB
�B
�B
�B
�B
mB
�B
gB
�B
[B
B
�B
B
3B
�B
�B
�B
�B
 B	��B	�<B	�jB	��B	�DB	��B	�VB	�VB	�B	��B	��B	��B	�6B	�<B	��B
 OB
 B
�B
�B
�B
�B
�B
�B
SB
mB
�B
%B
YB
tB
YB
�B
zB
�B
B
1B
B
�B
	RB
	�B

#B

#B

	B
	�B

=B

�B

�B
�B
�B
�B
�B
B
6B
jB
�B
"B
pB
BB
B
BB
�B
�B
�B
�B
�B
}B
�B
4B
�B
�B
B
:B
:B
 B
�B
�B
&B
[B
@B
�B
FB
FB
FB
�B
�B
2B
�B
�B
�B
�B
B
�B
$B
sB
�B
B
EB
�B
�B
�B
KB
�B
�B
B
�B
#B
WB
�B
CB
xB
�B
�B
B
IB
�B
B
B
�B
B
pB
�B
�B
 B
 'B
 BB
 �B
 �B
 �B
 �B
 \B
 �B
 vB
!|B
!�B
"B
"NB
"4B
!�B
!HB
!-B
 �B
 �B
!HB
!�B
!�B
!�B
"�B
$B
$�B
%B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'B
(�B
)*B
)*B
)*B
)�B
)�B
*B
*B
*B
*eB
(�B
'�B
(�B
'�B
'�B
(XB
(XB
(sB
(�B
(�B
)DB
(�B
)_B
*eB
+6B
+B
+6B
+�B
,B
,WB
,�B
,�B
-B
-]B
.cB
.}B
.}B
/5B
/OB
/iB
0B
0�B
0�B
0�B
1[B
1[B
1�B
1�B
1�B
2GB
2GB
2aB
2�B
2|B
2|B
2|B
2GB
2GB
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4B
4�B
5?B
5�B
6B
6+B
6`B
6�B
7B
7B
72B
72B
7LB
72B
7LB
7�B
7�B
8B
9	B
9�B
9$B
8�B
8�B
9>B
9�B
9�B
9�B
:*B
9�B
:^B
:�B
;B
;�B
;�B
;�B
;�B
<6B
<�B
<�B
="B
=<B
=�B
=�B
>BB
>]B
>�B
>�B
>�B
?HB
?�B
?�B
?�B
@�B
AB
A B
AUB
A�B
B'B
A�B
A�B
BB
BAB
CB
B�B
CaB
C�B
C�B
C�B
D3B
DgB
D�B
E9B
F?B
F�B
G_B
GEB
G�B
HB
G�B
H1B
H�B
IB
H�B
IB
IRB
I�B
I�B
I�B
J#B
JXB
J�B
K�B
K�B
K�B
K�B
K�B
LB
LB
K�B
LB
L0B
L0B
LJB
L~B
L�B
L�B
MB
L�B
M6B
M�B
M�B
NpB
N�B
O(B
OvB
PB
P�B
P�B
Q4B
Q4B
Q�B
RB
RTB
R�B
R�B
RoB
RTB
RoB
R�B
SB
S&B
S@B
SB
S[B
S[B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T�B
T�B
T�B
UB
UB
U�B
U�B
VB
VB
V9B
VSB
V9B
V�B
V�B
V�B
W?B
XB
XEB
XEB
X�B
Y1B
Y1B
YB
YeB
YKB
Y�B
YeB
YB
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[	B
[#B
[WB
[�B
\�B
\�B
\�B
\�B
]B
]B
]IB
]�B
]�B
]�B
^B
^OB
^jB
^�B
^�B
^�B
^�B
_;B
_;B
_pB
_�B
`B
`BB
`BB
`�B
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
c:B
c�B
c�B
c�B
c�B
dB
d@B
d@B
dtB
d�B
d�B
eB
e,B
ezB
e�B
e�B
fLB
ffB
f�B
f�B
f�B
f�B
gmB
g�B
h
B
h
B
hsB
hXB
hsB
h�B
h�B
iB
i�B
i�B
i�B
jB
jB
j0B
jKB
j0B
j0B
j0B
jKB
j0B
jB
jB
jB
jKB
j0B
j0B
jeB
jKB
jB
j�B
j�B
kB
kB
kQB
k�B
k�B
l"B
l�B
l�B
l�B
mB
m]B
m]B
mwB
mwB
m�B
m�B
m�B
nB
nIB
nIB
nIB
ncB
ncB
n�B
n�B
o5B
oiB
o�B
o�B
poB
poB
p�B
p�B
qAB
q'B
qAB
q�B
q�B
q�B
q�B
qvB
q�B
q�B
q�B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sB
s3B
s�B
s�B
s�B
s�B
tB
tB
tTB
tnB
t�B
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
u�B
u�B
vFB
v`B
v`B
v�B
v�B
w2B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104957  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175412  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175413  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175413                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025420  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025420  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                