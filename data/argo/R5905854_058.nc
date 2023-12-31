CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:58Z creation;2022-06-04T17:54:58Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175458  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               :A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�6;'qf1   @�6;{�?@-ܬ1&��c�S���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH��BPffBW��Ba33Bg33Bo��Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�33B�33Bϙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C�fC  C  C  C
�C�C�fC  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���AfgA ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B��B(33B033B833B@33BI  BP��BW��BaffBgffBo��Bx33B��B��B��gB��B��B��B��B��B��B��B��B��B��B��B��B�L�B�� B��B�L�B�L�Bϳ4B��B��B��B��B��B��B��B��B��B��B�L�C &gC�3C�C�C�C
&gC&gC�3C�C�C�C�C�C�C�C�C �C"�C$&gC&�C(�C*�C,�C-�3C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�3C�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}��D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�D�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�ng111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�4�A�0�A�&�A�!-A�7A�$�A�~A��A��A��"A��,A��ZA���A���A���A��;A�ߤA���A�jKAӣ�A�_�Aѝ�A�}�A��sA��A��A̶zA�\)A��&A��]A�?A��kA���A�ĜA��hA��sA���A�K�A��CA�$@A�g�A�lWA���A���A�H�A��A���A�[�A��sA��~A�iDA��)A��9A�oA�ffA�7�A���A���A�j�A�8�A���A�}�A�A�A� �A���A��]A�C�A�r�A�JXA���A�49A�h>A���A�/�A�^5A�GzA��0A���A���AYKAz�At
�An:*AkqAic�Ah�Ab~�A]l"AX�AS\)AP�ZAL)_AG"hAF�AC��AA�6A@X�A>�6A=kQA<�A;v`A:i�A8��A6��A4��A3�A2Z�A1v`A0ƨA/�A-�_A-&�A,{JA+�A)A A(�TA'�zA'�SA' �A&�BA%�TA%�dA%u%A$�AA#e�A"��A"kQA!��A �jA��AϫAhsA��A��A�zA�A�^As�AȴA  A�]A�+A�#A|A�A��A��A��A��A�Ag8A�gA�A7LA6A
��A
��A
��A$�A
)�A	��A��AV�A��A��A�FAیA,�A ��Am�AuA�A��A�A�Ac Ae�Al�A 4@�ԕ@�8@�n/@�l�@��8@���@���@�:�@�\�@��@��@�8�@��@��m@�F@��@���@��W@���@���@�@�iD@�6@�t�@�͟@�_�@�?}@��@���@���@�c @���@��@�A@��@�%@��@�!�@��@�?@�@��c@�h�@��@��+@���@�"h@�w@�7@�Y�@��@�D�@���@߷@���@߇�@�ȴ@��N@�|�@�ȴ@��@ۉ7@�"�@ڎ�@�M@��a@�9�@ػ�@��@�y�@���@�%�@�Dg@��m@�Ov@���@�%@Ҫe@�:�@�($@�!�@��@��@���@�K�@�g8@�?}@��"@Ε@�0U@�W�@́�@��@�;�@˕�@�K�@ʾ�@��Z@��@��#@Ǹ�@ǟV@ǹ�@��@ǅ@��@��`@�~(@�`�@�h�@�Ov@�J@��K@Èf@�"�@�~@�o @�y�@��g@�?@�!�@���@��'@�?@���@�ߤ@��e@�~�@��@�G�@��@��B@��1@���@�5�@�Ĝ@�=q@��@�Dg@�!�@��@��b@�~(@�}V@�r�@�6@�ݘ@���@�$t@��1@�z@��@��Z@��f@��O@�N�@���@�~�@�f�@��@���@�\�@�:�@�x@���@�;d@��@���@��F@��@�f�@�;@���@�-�@���@�K�@�>�@��E@���@�4n@��r@��@�v`@��p@��?@���@�B[@���@��K@�}�@�%F@��'@�m�@���@�Q�@��@��B@���@��@�YK@���@��"@�Y�@��@��]@�q@�D�@�  @���@��4@��@���@�l�@�C�@�#:@��@�'�@���@�r�@�&�@���@�O�@�:�@�	l@��r@�Ft@�M@���@���@���@�T�@�(@��@���@�Q@��@��h@�/�@��5@���@�~@���@���@�o @�1�@��X@�H@��.@��N@�rG@�T�@�.I@��@��m@�{�@�e@�v`@�Vm@���@�0U@��;@��d@���@��M@�Y�@�"�@�S�@���@��@�b�@��"@���@��@�kQ@��@��T@���@�1�@���@��@��u@�q�@�7�@�{@�u@��@�g�@�Dg@�f�@�U�@��@�ѷ@�|�@�6�@��o@�J#@�-w@�(�@�@�ȴ@��D@�]d@�1'@�@���@�|�@��@�ȴ@���@�h
@�Q�@�?@�	@��)@��@�1�@�Ɇ@�w�@�Ft@��T@��d@��X@���@�^�@�!�@� i@��E@�w�@�S�@�M�@�N�@��@��K@�|@��@��s@��Y@�*�@��@��@�@���@�Ov@�e@��@��@�@��@�q@J#@~��@~xl@~)�@|�v@|]d@{�}@{�f@{,�@z�@z��@y�z@ya�@y�@x�@x  @ws@wC@w i@vd�@u�)@u�h@uQ�@u+�@u�@t�@s1�@r�}@r^5@r!�@q}�@q�@p�P@p��@pI�@oqv@n�c@n�1@n:*@m<6@l��@l@kY@j��@j�@jc @i�-@i�@hoi@g�6@gy�@gC�@g@f�R@f5?@e�C@e%F@d�@d@c��@cK�@b�M@b��@b;�@a��@a�S@ahs@a8�@a;@`��@`��@`|�@`7�@`	�@_�+@_�;@_��@^�X@^��@^kQ@^�@]�-@]��@]��@]G�@\��@\Z@[��@[�f@[{J@[K�@[@Zߤ@Z��@Z&�@Z@Y�n@Y@@XĜ@X~(@W�W@W�@W=@V��@V��@V{@U��@Uu�@U�@T��@T9X@S�@R��@Rff@R3�@Q�@Qo @P�@PD�@O�&@O�@Ov`@O4�@N^5@N �@M�z@M��@M`B@Mq@L�_@K�@K"�@J��@Jv�@JOv@J+k@J	@I��@I�7@I<6@Hѷ@H�@H�@G4�@Fߤ@F��@F�@F_@E��@EG�@D��@DQ�@C�&@C�$@C�@CC�@C(@B��@BkQ@B:*@A��@A�~@A�@@��@@�@@oi@@`�@@/�@?�:@?9�@?�@>��@>� @>�@=��@=��@=��@=hs@=:�@<��@<�@<m�@;�+@;��@;
=@:�h@:=q@:�@9��@9�C@9�7@9p�@9/@8�E@8��@8N�@8	�@7|�@7U�@6�@6�h@6�@6��@61�@6{@5��@5�d@5��@5V@4��@4�`@4�u@4U2@4�@3��@3{J@3O@3,�@2��@2��@2�@1�d@1x�@0��@0�O@0PH@0@/�@/��@/��@/O@/!-@.�@.�L@.\�@.1�@-�@-��@-o @-4@- \@,��@,��@,z�@,Z@,  @+��@+|�@+F�@*�8@*�R@*h
@*;�@*!�@*@)�@)��@)c�@(��@(��@(�@(H@(�@'�@']�@&�@&��@&��@&xl@&c @&Q@& �@%��@%�@%u�@%G�@%/@%q@$�@$�[@$�9@$�.@$M@$x@#��@#�f@#_p@#9�@#@"�@"q�@"!�@!�N@!��@!B�@ ��@ C-@ *�@�@��@�@��@y�@dZ@F�@"�@�@�@^5@!�@��@�"@j@q@�?@��@e�@Z@H@M@�&@ƨ@n/@A�@@O@)_@��@�H@��@��@_�@�@F@�@�K@��@N�@N�@2�@�@�m@�K@��@qv@>�@'�@�8@��@��@�+@kQ@.�@�>@��@[W@+@��@�.@w�@PH@2�@�+@��@U�@@O@"�@�@��@�2@�'@�!@�1@Ta@{@��@�9@��@p�@IR@�@�@��@j@Z@Q�@C-@ �@�+@��@��@�w@�k@U�@9�@�@��@��@�@H�@	@�@��@zx@F@@@�@�@�	@��@�I@bN@Q�@H@'R@  @�g@��@�@��@v`@]�@;d@
�@
��@
l�@
GE@
{@
	@	��@	��@	��@	��@	��@	�=@	��@	�@��@�E@�)@�9@��@�@�o@l"@1'@$@�@�@�;@�6@�a@��@�*@��@g�@,�@�M@�@�<@��@�\@}V@Z�@:*@�@��@��@��@^�@8�@(�@��@�@��@�@u�@Xy111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�4�A�0�A�&�A�!-A�7A�$�A�~A��A��A��"A��,A��ZA���A���A���A��;A�ߤA���A�jKAӣ�A�_�Aѝ�A�}�A��sA��A��A̶zA�\)A��&A��]A�?A��kA���A�ĜA��hA��sA���A�K�A��CA�$@A�g�A�lWA���A���A�H�A��A���A�[�A��sA��~A�iDA��)A��9A�oA�ffA�7�A���A���A�j�A�8�A���A�}�A�A�A� �A���A��]A�C�A�r�A�JXA���A�49A�h>A���A�/�A�^5A�GzA��0A���A���AYKAz�At
�An:*AkqAic�Ah�Ab~�A]l"AX�AS\)AP�ZAL)_AG"hAF�AC��AA�6A@X�A>�6A=kQA<�A;v`A:i�A8��A6��A4��A3�A2Z�A1v`A0ƨA/�A-�_A-&�A,{JA+�A)A A(�TA'�zA'�SA' �A&�BA%�TA%�dA%u%A$�AA#e�A"��A"kQA!��A �jA��AϫAhsA��A��A�zA�A�^As�AȴA  A�]A�+A�#A|A�A��A��A��A��A�Ag8A�gA�A7LA6A
��A
��A
��A$�A
)�A	��A��AV�A��A��A�FAیA,�A ��Am�AuA�A��A�A�Ac Ae�Al�A 4@�ԕ@�8@�n/@�l�@��8@���@���@�:�@�\�@��@��@�8�@��@��m@�F@��@���@��W@���@���@�@�iD@�6@�t�@�͟@�_�@�?}@��@���@���@�c @���@��@�A@��@�%@��@�!�@��@�?@�@��c@�h�@��@��+@���@�"h@�w@�7@�Y�@��@�D�@���@߷@���@߇�@�ȴ@��N@�|�@�ȴ@��@ۉ7@�"�@ڎ�@�M@��a@�9�@ػ�@��@�y�@���@�%�@�Dg@��m@�Ov@���@�%@Ҫe@�:�@�($@�!�@��@��@���@�K�@�g8@�?}@��"@Ε@�0U@�W�@́�@��@�;�@˕�@�K�@ʾ�@��Z@��@��#@Ǹ�@ǟV@ǹ�@��@ǅ@��@��`@�~(@�`�@�h�@�Ov@�J@��K@Èf@�"�@�~@�o @�y�@��g@�?@�!�@���@��'@�?@���@�ߤ@��e@�~�@��@�G�@��@��B@��1@���@�5�@�Ĝ@�=q@��@�Dg@�!�@��@��b@�~(@�}V@�r�@�6@�ݘ@���@�$t@��1@�z@��@��Z@��f@��O@�N�@���@�~�@�f�@��@���@�\�@�:�@�x@���@�;d@��@���@��F@��@�f�@�;@���@�-�@���@�K�@�>�@��E@���@�4n@��r@��@�v`@��p@��?@���@�B[@���@��K@�}�@�%F@��'@�m�@���@�Q�@��@��B@���@��@�YK@���@��"@�Y�@��@��]@�q@�D�@�  @���@��4@��@���@�l�@�C�@�#:@��@�'�@���@�r�@�&�@���@�O�@�:�@�	l@��r@�Ft@�M@���@���@���@�T�@�(@��@���@�Q@��@��h@�/�@��5@���@�~@���@���@�o @�1�@��X@�H@��.@��N@�rG@�T�@�.I@��@��m@�{�@�e@�v`@�Vm@���@�0U@��;@��d@���@��M@�Y�@�"�@�S�@���@��@�b�@��"@���@��@�kQ@��@��T@���@�1�@���@��@��u@�q�@�7�@�{@�u@��@�g�@�Dg@�f�@�U�@��@�ѷ@�|�@�6�@��o@�J#@�-w@�(�@�@�ȴ@��D@�]d@�1'@�@���@�|�@��@�ȴ@���@�h
@�Q�@�?@�	@��)@��@�1�@�Ɇ@�w�@�Ft@��T@��d@��X@���@�^�@�!�@� i@��E@�w�@�S�@�M�@�N�@��@��K@�|@��@��s@��Y@�*�@��@��@�@���@�Ov@�e@��@��@�@��@�q@J#@~��@~xl@~)�@|�v@|]d@{�}@{�f@{,�@z�@z��@y�z@ya�@y�@x�@x  @ws@wC@w i@vd�@u�)@u�h@uQ�@u+�@u�@t�@s1�@r�}@r^5@r!�@q}�@q�@p�P@p��@pI�@oqv@n�c@n�1@n:*@m<6@l��@l@kY@j��@j�@jc @i�-@i�@hoi@g�6@gy�@gC�@g@f�R@f5?@e�C@e%F@d�@d@c��@cK�@b�M@b��@b;�@a��@a�S@ahs@a8�@a;@`��@`��@`|�@`7�@`	�@_�+@_�;@_��@^�X@^��@^kQ@^�@]�-@]��@]��@]G�@\��@\Z@[��@[�f@[{J@[K�@[@Zߤ@Z��@Z&�@Z@Y�n@Y@@XĜ@X~(@W�W@W�@W=@V��@V��@V{@U��@Uu�@U�@T��@T9X@S�@R��@Rff@R3�@Q�@Qo @P�@PD�@O�&@O�@Ov`@O4�@N^5@N �@M�z@M��@M`B@Mq@L�_@K�@K"�@J��@Jv�@JOv@J+k@J	@I��@I�7@I<6@Hѷ@H�@H�@G4�@Fߤ@F��@F�@F_@E��@EG�@D��@DQ�@C�&@C�$@C�@CC�@C(@B��@BkQ@B:*@A��@A�~@A�@@��@@�@@oi@@`�@@/�@?�:@?9�@?�@>��@>� @>�@=��@=��@=��@=hs@=:�@<��@<�@<m�@;�+@;��@;
=@:�h@:=q@:�@9��@9�C@9�7@9p�@9/@8�E@8��@8N�@8	�@7|�@7U�@6�@6�h@6�@6��@61�@6{@5��@5�d@5��@5V@4��@4�`@4�u@4U2@4�@3��@3{J@3O@3,�@2��@2��@2�@1�d@1x�@0��@0�O@0PH@0@/�@/��@/��@/O@/!-@.�@.�L@.\�@.1�@-�@-��@-o @-4@- \@,��@,��@,z�@,Z@,  @+��@+|�@+F�@*�8@*�R@*h
@*;�@*!�@*@)�@)��@)c�@(��@(��@(�@(H@(�@'�@']�@&�@&��@&��@&xl@&c @&Q@& �@%��@%�@%u�@%G�@%/@%q@$�@$�[@$�9@$�.@$M@$x@#��@#�f@#_p@#9�@#@"�@"q�@"!�@!�N@!��@!B�@ ��@ C-@ *�@�@��@�@��@y�@dZ@F�@"�@�@�@^5@!�@��@�"@j@q@�?@��@e�@Z@H@M@�&@ƨ@n/@A�@@O@)_@��@�H@��@��@_�@�@F@�@�K@��@N�@N�@2�@�@�m@�K@��@qv@>�@'�@�8@��@��@�+@kQ@.�@�>@��@[W@+@��@�.@w�@PH@2�@�+@��@U�@@O@"�@�@��@�2@�'@�!@�1@Ta@{@��@�9@��@p�@IR@�@�@��@j@Z@Q�@C-@ �@�+@��@��@�w@�k@U�@9�@�@��@��@�@H�@	@�@��@zx@F@@@�@�@�	@��@�I@bN@Q�@H@'R@  @�g@��@�@��@v`@]�@;d@
�@
��@
l�@
GE@
{@
	@	��@	��@	��@	��@	��@	�=@	��@	�@��@�E@�)@�9@��@�@�o@l"@1'@$@�@�@�;@�6@�a@��@�*@��@g�@,�@�M@�@�<@��@�\@}V@Z�@:*@�@��@��@��@^�@8�@(�@��@�@��@�@u�@Xy111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	�`B	�zB	�zB	�zB	�zB	�zB	�zB	�zB	�FB	�ZB	�AB	ܬB	��B	یB	��B	�
B	�PB	ɺB	�#B	ǔB	�B	ԯB	�RB	��B
�B
�B
TaB
YKB
l�B
iyB
x�B
��B
��B
ðB
ބB
�B
�cBuB'�B6zBM�BW$B[�Bi�B~�B~�B��B��B��B��B��B�LBB�B�xB��B��B��B�\B��Bo�BG_BB  B
�B
ՁB
�B
�B
Q�B
0�B
�B	��B	�B	�?B	cTB	S�B	H�B	>(B	#�B	�B�BݘBЗB��B�8B��B�B��B�B��B�B�TB�VB�B� B��B�B�0B�B	 iB	�B	?B	hB	�B	"�B	0�B	1vB	AUB	K^B	U�B	r|B	y�B	v�B	x�B	�B	��B	��B	HB	}"B	yrB	t�B	kQB	e�B	`'B	fLB	o�B	xB	�B	��B	� B	��B	��B	�6B	�(B	��B	ƨB	��B	�oB	�kB	��B	��B	�jB	��B	�FB	�yB	��B	yrB	i�B	lB	poB	v+B	s�B	r�B	\)B	OB	Y�B	[�B	PbB	@�B	=�B	>�B	O\B	cTB	c�B	v+B	��B	�aB	�OB	��B	�9B	�OB	~(B	B	��B	��B	��B	�B	��B	�.B	��B	�lB	��B	�BB	��B	��B	��B	��B	�iB	�B	��B	�B	��B	��B	�B	�*B	��B	�qB	�IB	��B	��B	� B	�iB	�UB	��B	��B	�3B	�B	��B	�?B	�%B	�?B	�tB	�tB	��B	�8B	�.B	�HB	�B	�1B	ƨB	�MB	żB	ǔB	ȴB	ȀB	�B	̘B	�dB	ʦB	͹B	�B	��B	�7B	ʌB	�XB	�^B	��B	�JB	�PB	�B	�0B	�"B	��B	��B	��B	�DB	�dB	�DB	��B	ʌB	˒B	�jB	ΊB	�vB	��B	�bB	�(B	��B	̘B	̳B	�dB	уB	ӏB	�uB	��B	�@B	��B	��B	�EB	�YB	�B	ּB	��B	�B	ޞB	޸B	ބB	ޞB	�pB	��B	�B	��B	��B	�vB	��B	�IB	�kB	�B	��B	��B	��B	�tB	�zB	�B	�sB	�sB	�B	�B	�eB	�B	��B	�B	�B	�B	�B	�B	�qB	�)B	�]B	��B	��B	�B	�B	�vB	�B	�B	�B	�vB	�B	�B	�'B	��B	�B	�|B	�-B	�[B	�B	�GB	��B	��B	�B	�B	�-B	�GB	��B	��B	�B	�B	�B	��B	��B	��B	�9B	�B	��B	�+B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	�B	��B	�jB	��B	��B	��B	�VB	��B	�(B	��B	��B	�B	�HB	�cB	��B	��B
 iB
 �B
 �B
 �B
 �B
 B
B
;B
;B
oB
'B
[B
�B
�B
�B
GB
�B
�B
B
gB
B
B
mB
�B
YB
?B
?B
YB
�B
�B
�B
tB
YB
�B
zB
_B
�B
	B
	�B

=B

�B
0B
dB
�B
jB
<B
�B
�B
�B
�B
�B
�B
�B
�B
.B
 B
B
.B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
B
B
9B
9B
�B
�B
9B
B
�B
�B
�B

B
?B

B
�B
+B
�B
�B
�B
�B
�B
�B
�B
~B
IB
dB
�B
�B
�B
�B
B
jB
�B
VB
 B
 �B
 �B
!B
 �B
 �B
!|B
!�B
"4B
"�B
"�B
"�B
$B
$@B
$ZB
%`B
%FB
&B
&LB
&fB
&LB
&�B
&�B
'B
'�B
'�B
(
B
'�B
(>B
(�B
(sB
(�B
)_B
)*B
)*B
)B
)*B
'�B
'�B
(XB
(�B
(�B
)*B
)�B
+B
+�B
+B
+B
+B
*�B
)yB
)�B
*KB
*�B
*�B
+6B
+�B
+�B
+�B
,WB
,�B
,�B
-�B
-�B
-�B
.�B
/5B
0UB
0�B
0�B
0�B
1vB
0!B
0!B
0UB
0�B
1vB
2�B
4nB
4nB
4�B
5�B
5�B
5�B
5tB
5tB
5%B
5ZB
6B
6B
5�B
5�B
6FB
6`B
6�B
7B
72B
7�B
7�B
8B
8lB
8�B
8�B
8�B
8�B
9rB
9�B
:DB
:*B
:xB
:�B
;�B
<B
<�B
<�B
=B
="B
=VB
=�B
=�B
>B
>]B
>�B
?�B
?�B
?�B
@ B
@ B
@�B
AoB
A�B
BuB
B�B
B�B
BuB
B�B
C{B
C{B
C�B
C�B
C�B
C�B
DB
D3B
DgB
D�B
EB
EB
EB
E�B
E�B
F�B
F�B
F�B
GEB
G_B
GzB
G�B
HfB
H�B
H�B
I7B
I7B
I�B
I�B
I�B
I�B
J#B
J�B
K�B
K�B
L0B
LB
K�B
K�B
L0B
L~B
MB
M�B
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
OB
O�B
PHB
PbB
P}B
P�B
P�B
P�B
QB
Q4B
Q4B
Q�B
QB
Q�B
Q�B
Q�B
RB
RoB
R�B
R�B
R�B
S[B
S[B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
UB
UB
U2B
UMB
U�B
UgB
U�B
VB
VSB
V�B
V�B
W$B
W$B
WYB
W�B
XEB
XEB
X�B
YB
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\CB
\�B
\]B
\�B
\�B
]B
]dB
]�B
]�B
^B
^�B
_B
_B
_;B
_�B
`'B
`'B
`�B
`�B
aB
a-B
a|B
a�B
bB
b4B
bB
b4B
bNB
b�B
cB
c:B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dZB
dZB
dZB
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
f2B
f�B
gB
g8B
g8B
g8B
gRB
g8B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
h$B
h>B
h>B
hsB
h�B
h�B
iB
iDB
i*B
i*B
iB
iyB
i�B
i�B
i�B
jeB
kB
k6B
kQB
k�B
k�B
l"B
l"B
l"B
lWB
lqB
l�B
mB
mwB
m�B
m�B
nB
nB
nB
ncB
n}B
n�B
oB
o B
oB
oB
o B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p�B
qB
q'B
q�B
rB
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
shB
shB
shB
s�B
s�B
s�B
tB
tnB
t�B
uB
uZB
uZB
utB
utB
u�B
u�B
vB
v+B
vFB
v�B
v�B
wB
wB
w2B
w2B
w2B
w�B
w�B
w�B
w�B
xB
x8B
xRB
x�B
x�B
y	B
y$B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
|B
|jB
|jB
|jB
|jB
|�B
|�B
}B
}"B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
.B
HB
HB
cB
}B
}B
�B
�B
}B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
� B
�B
� B
�UB
�;B
�UB
�UB
��B
��B
�B
�B
�[B
��B
��B
��B
��B
�B
�B
�GB
�{B
�{B
��B
��B
�3B
�3B
�gB
��B
��B
��B
�B
�SB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	�`B	�zB	�zB	�zB	�zB	�zB	�zB	�zB	�FB	�ZB	�AB	ܬB	��B	یB	��B	�
B	�PB	ɺB	�#B	ǔB	�B	ԯB	�RB	��B
�B
�B
TaB
YKB
l�B
iyB
x�B
��B
��B
ðB
ބB
�B
�cBuB'�B6zBM�BW$B[�Bi�B~�B~�B��B��B��B��B��B�LBB�B�xB��B��B��B�\B��Bo�BG_BB  B
�B
ՁB
�B
�B
Q�B
0�B
�B	��B	�B	�?B	cTB	S�B	H�B	>(B	#�B	�B�BݘBЗB��B�8B��B�B��B�B��B�B�TB�VB�B� B��B�B�0B�B	 iB	�B	?B	hB	�B	"�B	0�B	1vB	AUB	K^B	U�B	r|B	y�B	v�B	x�B	�B	��B	��B	HB	}"B	yrB	t�B	kQB	e�B	`'B	fLB	o�B	xB	�B	��B	� B	��B	��B	�6B	�(B	��B	ƨB	��B	�oB	�kB	��B	��B	�jB	��B	�FB	�yB	��B	yrB	i�B	lB	poB	v+B	s�B	r�B	\)B	OB	Y�B	[�B	PbB	@�B	=�B	>�B	O\B	cTB	c�B	v+B	��B	�aB	�OB	��B	�9B	�OB	~(B	B	��B	��B	��B	�B	��B	�.B	��B	�lB	��B	�BB	��B	��B	��B	��B	�iB	�B	��B	�B	��B	��B	�B	�*B	��B	�qB	�IB	��B	��B	� B	�iB	�UB	��B	��B	�3B	�B	��B	�?B	�%B	�?B	�tB	�tB	��B	�8B	�.B	�HB	�B	�1B	ƨB	�MB	żB	ǔB	ȴB	ȀB	�B	̘B	�dB	ʦB	͹B	�B	��B	�7B	ʌB	�XB	�^B	��B	�JB	�PB	�B	�0B	�"B	��B	��B	��B	�DB	�dB	�DB	��B	ʌB	˒B	�jB	ΊB	�vB	��B	�bB	�(B	��B	̘B	̳B	�dB	уB	ӏB	�uB	��B	�@B	��B	��B	�EB	�YB	�B	ּB	��B	�B	ޞB	޸B	ބB	ޞB	�pB	��B	�B	��B	��B	�vB	��B	�IB	�kB	�B	��B	��B	��B	�tB	�zB	�B	�sB	�sB	�B	�B	�eB	�B	��B	�B	�B	�B	�B	�B	�qB	�)B	�]B	��B	��B	�B	�B	�vB	�B	�B	�B	�vB	�B	�B	�'B	��B	�B	�|B	�-B	�[B	�B	�GB	��B	��B	�B	�B	�-B	�GB	��B	��B	�B	�B	�B	��B	��B	��B	�9B	�B	��B	�+B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	�B	��B	�jB	��B	��B	��B	�VB	��B	�(B	��B	��B	�B	�HB	�cB	��B	��B
 iB
 �B
 �B
 �B
 �B
 B
B
;B
;B
oB
'B
[B
�B
�B
�B
GB
�B
�B
B
gB
B
B
mB
�B
YB
?B
?B
YB
�B
�B
�B
tB
YB
�B
zB
_B
�B
	B
	�B

=B

�B
0B
dB
�B
jB
<B
�B
�B
�B
�B
�B
�B
�B
�B
.B
 B
B
.B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
B
B
9B
9B
�B
�B
9B
B
�B
�B
�B

B
?B

B
�B
+B
�B
�B
�B
�B
�B
�B
�B
~B
IB
dB
�B
�B
�B
�B
B
jB
�B
VB
 B
 �B
 �B
!B
 �B
 �B
!|B
!�B
"4B
"�B
"�B
"�B
$B
$@B
$ZB
%`B
%FB
&B
&LB
&fB
&LB
&�B
&�B
'B
'�B
'�B
(
B
'�B
(>B
(�B
(sB
(�B
)_B
)*B
)*B
)B
)*B
'�B
'�B
(XB
(�B
(�B
)*B
)�B
+B
+�B
+B
+B
+B
*�B
)yB
)�B
*KB
*�B
*�B
+6B
+�B
+�B
+�B
,WB
,�B
,�B
-�B
-�B
-�B
.�B
/5B
0UB
0�B
0�B
0�B
1vB
0!B
0!B
0UB
0�B
1vB
2�B
4nB
4nB
4�B
5�B
5�B
5�B
5tB
5tB
5%B
5ZB
6B
6B
5�B
5�B
6FB
6`B
6�B
7B
72B
7�B
7�B
8B
8lB
8�B
8�B
8�B
8�B
9rB
9�B
:DB
:*B
:xB
:�B
;�B
<B
<�B
<�B
=B
="B
=VB
=�B
=�B
>B
>]B
>�B
?�B
?�B
?�B
@ B
@ B
@�B
AoB
A�B
BuB
B�B
B�B
BuB
B�B
C{B
C{B
C�B
C�B
C�B
C�B
DB
D3B
DgB
D�B
EB
EB
EB
E�B
E�B
F�B
F�B
F�B
GEB
G_B
GzB
G�B
HfB
H�B
H�B
I7B
I7B
I�B
I�B
I�B
I�B
J#B
J�B
K�B
K�B
L0B
LB
K�B
K�B
L0B
L~B
MB
M�B
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
OB
O�B
PHB
PbB
P}B
P�B
P�B
P�B
QB
Q4B
Q4B
Q�B
QB
Q�B
Q�B
Q�B
RB
RoB
R�B
R�B
R�B
S[B
S[B
S�B
S�B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
UB
UB
U2B
UMB
U�B
UgB
U�B
VB
VSB
V�B
V�B
W$B
W$B
WYB
W�B
XEB
XEB
X�B
YB
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\CB
\�B
\]B
\�B
\�B
]B
]dB
]�B
]�B
^B
^�B
_B
_B
_;B
_�B
`'B
`'B
`�B
`�B
aB
a-B
a|B
a�B
bB
b4B
bB
b4B
bNB
b�B
cB
c:B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dZB
dZB
dZB
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
f2B
f�B
gB
g8B
g8B
g8B
gRB
g8B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
h$B
h>B
h>B
hsB
h�B
h�B
iB
iDB
i*B
i*B
iB
iyB
i�B
i�B
i�B
jeB
kB
k6B
kQB
k�B
k�B
l"B
l"B
l"B
lWB
lqB
l�B
mB
mwB
m�B
m�B
nB
nB
nB
ncB
n}B
n�B
oB
o B
oB
oB
o B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p�B
qB
q'B
q�B
rB
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
r�B
sB
sMB
shB
shB
shB
s�B
s�B
s�B
tB
tnB
t�B
uB
uZB
uZB
utB
utB
u�B
u�B
vB
v+B
vFB
v�B
v�B
wB
wB
w2B
w2B
w2B
w�B
w�B
w�B
w�B
xB
x8B
xRB
x�B
x�B
y	B
y$B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
|B
|jB
|jB
|jB
|jB
|�B
|�B
}B
}"B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
.B
HB
HB
cB
}B
}B
�B
�B
}B
�B
�OB
�iB
��B
��B
��B
��B
��B
��B
� B
�B
� B
�UB
�;B
�UB
�UB
��B
��B
�B
�B
�[B
��B
��B
��B
��B
�B
�B
�GB
�{B
�{B
��B
��B
�3B
�3B
�gB
��B
��B
��B
�B
�SB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104958  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175458  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175458  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175458                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025505  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025505  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                