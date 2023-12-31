CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:59Z creation;2022-06-04T17:46:00Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174559  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ػ�ZD1   @ػ��s�@.��n���d?+I�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B���B���B�  B�  B�  B�  C �3CffC�fC  C  C
  C  C  C  C  C  C�C�C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<33C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(��B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B�L�B��B��4B��gB��B��B��B��B��B��B��B��B��B��gB��B��gB��gB��gB��gB��B��B��B��C � Cs3C�3C�C�C
�C�C�C�C�C�C&gC&gC�C�3C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:&gC<@ C=�3C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj&gCl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO	�DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D��gD��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�k41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�A�oA���A��dAѺ�AѴ�AѲ�Aѹ$AѵA�u%A�^�A�GA�2�A�2�A�1�A�.�A�*�A�(�A�($A�'RA�&�A�$�A�#�A�#A��A�OA��A��A��A��A�kA��A��A�A��A�Y�A��A��A�u�A�<�A��A�MA��A�$A�VA�m)A�:*A�Aʮ}A�~AɦLA���A��A�c�A�ĜA���AĪ�AõA°�A�l�A��;A���A��+A��KA�'�A�?}A�+6A�IRA�|�A���A�/�A���A�*�A�A�A���A��xA��A�d&A�
rA���A���A�VA��A�ԕA�ѷA�iA��IA���A���A���A�ΥA��hA���A���A��9A�r|A%A{�Ay˒Aw��As/Ap�4Al��AjoiAfϫAa5�A\h�AW��AT�HARp�AP.�AL�AIC�AHJAF�AAE(�AC��AA�zA>�A>�SA>@OA=�hA<MjA:��A8�A7��A5`�A48A0֡A/��A-��A-�*A-��A-��A,�aA+�A*IRA)��A(�UA(qA'�/A&�A%@OA$�A#֡A#u%A#$�A"��A!��A!��A!d�A!F�A!%�A!�A ��A�?A�A��A��A��A��A��A�xA�BA$�A��A_pA�AcA��AMA�A�vA"�A�mA�eAg�A�fA�[A��Ae�A<�A�'APHA�jA�kA�A�QA��A0�A
�}A	��A	ffAYA�sAu%A�<A�A�2A�HA��AY�A�A�^Ay�A��A@�A�2A=�AbA&A �vA p�A Q�A /A @��@�S@��@�T�@��Z@�o@�~�@���@��]@�_p@��@�F�@��@�ϫ@���@��9@��f@��@�1'@��@��@�@�V@�_p@� \@��U@�i�@��@��@�6@�@�L@�n�@�e@���@�L�@��@�~(@���@�3�@�~�@��2@耝@�^5@�L0@�+k@��@��@��
@���@��@�y�@�F�@��@�u�@�=�@��m@��@�@�0�@�j�@�!@��@ᝲ@��@��A@�H@��@��@��@�V@��M@��@��@�9X@ڣ@ڋD@�|�@�g�@��@�c�@�N�@�(�@��@նF@�\)@��@�H�@��m@�B�@��@�A�@�S@Ѝ�@��@ϼ�@��@�M@�\)@̤�@˝�@ʬ�@�V@�J�@�@�O�@��@���@��B@Ȼ�@ȏ\@Ƿ@�o@�h�@�8�@��W@��@Ş�@�X�@�@��,@�kQ@��@�b�@�>�@�@��@�@���@��@�bN@���@��@���@�?@�ԕ@�K�@��@�_�@��m@��{@�+�@��@���@��!@�r�@��@��@@��	@�>�@��@�~(@�9X@��@���@���@�U2@�tT@�xl@�v�@��z@��I@���@��P@�^5@��M@��O@�a|@�bN@�M@��@���@��F@��:@�k�@�Mj@��K@�� @���@���@��h@��A@��@��V@�\)@�@@���@�H�@���@�/�@�@�
=@���@���@�d�@�_@�خ@��H@��-@��"@�Q�@�V@��@���@��x@�>B@�	�@�o�@���@��@�Z�@��@�m�@�:�@�	l@���@��@��@��5@�\�@��o@�g�@��5@�r�@�.�@���@��@��"@��4@�t�@�e�@��8@�~(@�Ft@��T@���@��M@��@��@��@��@�/@�2a@�IR@�^�@�%F@��h@��{@�C�@�+@�q@���@�~(@�E�@�7@��j@��{@���@��@�?@���@���@���@��=@�W?@�'�@���@���@�|�@�7@���@�/�@��L@�!@���@��/@���@��1@��h@��u@�!�@�ϫ@�k�@�8@��@���@���@�q�@�Q�@�9X@��@��@���@�A�@���@���@��@�=q@�خ@�U�@���@��p@��@��o@�Ov@��@���@�N<@�5�@�@��!@���@�D�@�"h@��@��;@�s�@�E9@�o@��M@��X@��e@��@���@��~@�}�@�X@�/@��5@���@�s�@�H@���@��0@��{@�:�@���@��v@���@��A@�"h@��X@�g�@�O�@�9�@��F@�(�@�'R@�M@��@�ԕ@��$@�t�@�2a@���@���@���@�C-@��@_p@~�X@~}V@}�T@}�@}f�@}S&@} \@|�e@{�]@{��@{|�@{)_@z��@z��@zYK@y�@x��@xy>@xg8@xN�@xFt@x2�@x1@w�@vYK@uԕ@t�|@t~(@s��@s��@r��@r�B@r\�@q��@q�@p��@p?�@p�@o��@odZ@n��@n#:@m�#@m�@m*0@l��@lw�@l:�@k�@kn/@j��@j{�@j@if�@h�@h�@g��@g�w@g��@g��@gX�@fߤ@fO@e��@ec�@eJ�@e \@d��@d�@dw�@de�@dZ@d$@c��@c�@c|�@cy�@c@O@c$t@b�@be@a�@au�@a�@`��@`�@_��@_J#@^�@^ff@^Z�@^5?@^ �@]�@]��@]\�@]�@\��@\`�@\,=@[��@[t�@["�@Z�"@Z�!@Z6�@Y��@Y�h@Y�@X��@X��@Xu�@W�@W�k@W=@V�'@V��@VYK@Ve@U�@U:�@T�@T�D@T_@S�@S1�@R�@RL0@RB[@Q��@Q��@Qj@Q0�@P��@P�U@O�@O9�@O(@N�@N)�@M�j@M|@M=�@L�@L��@L/�@Kqv@J�@J}V@J@�@J.�@J#:@I�@H�@H��@HtT@HXy@H6@H�@G��@G�w@G�V@G9�@G�@F�@Fq�@E��@E�C@Ej@D��@D֡@D�@C�W@Cg�@B�@B��@B}V@B-@A�H@A�"@Ac�@A(�@A�@@�5@@�j@@�9@@|�@?�g@?�	@?_p@?!-@>��@>҉@>xl@=�>@=�3@=�-@=j@=/@<�K@<��@<��@<��@<�D@<I�@;��@;'�@:�B@:� @:YK@:�@9��@9L�@9%@8�U@8h�@8"h@7�W@7RT@6�@6��@6\�@5��@5�j@5�@5��@5u�@5`B@5+@4�e@4U2@4 �@3��@3��@3P�@3�@2�1@2l�@2Ov@2 �@1�n@1x�@1X@1%F@0�@0�_@0%�@/�*@/dZ@/�@.��@.��@.�B@.��@.u%@.c @.?@-�Z@-m]@-&�@,��@,�@,Xy@,@+��@+��@+{J@+E9@+33@*�"@*�!@*.�@)ԕ@)��@)f�@(�@(�$@(��@(w�@(K^@(@'�@'�Q@'��@'��@'qv@'$t@'�@'�@&��@&��@&-@%e,@$�@$��@$�/@$�E@$��@$�@$Ft@$~@$x@$  @#�]@#�:@#�@"��@"Q@"J@!��@!�h@!=�@ �P@ �O@ H@�;@�k@$t@
=@͟@3�@�@�X@�h@x�@5�@@�@Ĝ@��@r�@�+@�	@33@�]@�@��@�6@s�@Ta@�@��@��@hs@?}@�K@��@7�@  @�}@�@�4@S�@�@@@�H@ȴ@��@��@c @O@�o@��@�H@��@�-@}�@&�@�@�U@�@C-@�@��@�@�A@�;@�F@��@j�@,�@�c@��@��@z@?@+k@J@��@�#@��@�=@rG@J�@#�@��@��@��@r�@V�@�@� @�K@��@b�@C�@�@�@}V@J�@e@�Z@��@��@��@�7@Q�@!�@�|@�@�`@�4@w�@_@M@"h@�@�W1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�A�oA���A��dAѺ�AѴ�AѲ�Aѹ$AѵA�u%A�^�A�GA�2�A�2�A�1�A�.�A�*�A�(�A�($A�'RA�&�A�$�A�#�A�#A��A�OA��A��A��A��A�kA��A��A�A��A�Y�A��A��A�u�A�<�A��A�MA��A�$A�VA�m)A�:*A�Aʮ}A�~AɦLA���A��A�c�A�ĜA���AĪ�AõA°�A�l�A��;A���A��+A��KA�'�A�?}A�+6A�IRA�|�A���A�/�A���A�*�A�A�A���A��xA��A�d&A�
rA���A���A�VA��A�ԕA�ѷA�iA��IA���A���A���A�ΥA��hA���A���A��9A�r|A%A{�Ay˒Aw��As/Ap�4Al��AjoiAfϫAa5�A\h�AW��AT�HARp�AP.�AL�AIC�AHJAF�AAE(�AC��AA�zA>�A>�SA>@OA=�hA<MjA:��A8�A7��A5`�A48A0֡A/��A-��A-�*A-��A-��A,�aA+�A*IRA)��A(�UA(qA'�/A&�A%@OA$�A#֡A#u%A#$�A"��A!��A!��A!d�A!F�A!%�A!�A ��A�?A�A��A��A��A��A��A�xA�BA$�A��A_pA�AcA��AMA�A�vA"�A�mA�eAg�A�fA�[A��Ae�A<�A�'APHA�jA�kA�A�QA��A0�A
�}A	��A	ffAYA�sAu%A�<A�A�2A�HA��AY�A�A�^Ay�A��A@�A�2A=�AbA&A �vA p�A Q�A /A @��@�S@��@�T�@��Z@�o@�~�@���@��]@�_p@��@�F�@��@�ϫ@���@��9@��f@��@�1'@��@��@�@�V@�_p@� \@��U@�i�@��@��@�6@�@�L@�n�@�e@���@�L�@��@�~(@���@�3�@�~�@��2@耝@�^5@�L0@�+k@��@��@��
@���@��@�y�@�F�@��@�u�@�=�@��m@��@�@�0�@�j�@�!@��@ᝲ@��@��A@�H@��@��@��@�V@��M@��@��@�9X@ڣ@ڋD@�|�@�g�@��@�c�@�N�@�(�@��@նF@�\)@��@�H�@��m@�B�@��@�A�@�S@Ѝ�@��@ϼ�@��@�M@�\)@̤�@˝�@ʬ�@�V@�J�@�@�O�@��@���@��B@Ȼ�@ȏ\@Ƿ@�o@�h�@�8�@��W@��@Ş�@�X�@�@��,@�kQ@��@�b�@�>�@�@��@�@���@��@�bN@���@��@���@�?@�ԕ@�K�@��@�_�@��m@��{@�+�@��@���@��!@�r�@��@��@@��	@�>�@��@�~(@�9X@��@���@���@�U2@�tT@�xl@�v�@��z@��I@���@��P@�^5@��M@��O@�a|@�bN@�M@��@���@��F@��:@�k�@�Mj@��K@�� @���@���@��h@��A@��@��V@�\)@�@@���@�H�@���@�/�@�@�
=@���@���@�d�@�_@�خ@��H@��-@��"@�Q�@�V@��@���@��x@�>B@�	�@�o�@���@��@�Z�@��@�m�@�:�@�	l@���@��@��@��5@�\�@��o@�g�@��5@�r�@�.�@���@��@��"@��4@�t�@�e�@��8@�~(@�Ft@��T@���@��M@��@��@��@��@�/@�2a@�IR@�^�@�%F@��h@��{@�C�@�+@�q@���@�~(@�E�@�7@��j@��{@���@��@�?@���@���@���@��=@�W?@�'�@���@���@�|�@�7@���@�/�@��L@�!@���@��/@���@��1@��h@��u@�!�@�ϫ@�k�@�8@��@���@���@�q�@�Q�@�9X@��@��@���@�A�@���@���@��@�=q@�خ@�U�@���@��p@��@��o@�Ov@��@���@�N<@�5�@�@��!@���@�D�@�"h@��@��;@�s�@�E9@�o@��M@��X@��e@��@���@��~@�}�@�X@�/@��5@���@�s�@�H@���@��0@��{@�:�@���@��v@���@��A@�"h@��X@�g�@�O�@�9�@��F@�(�@�'R@�M@��@�ԕ@��$@�t�@�2a@���@���@���@�C-@��@_p@~�X@~}V@}�T@}�@}f�@}S&@} \@|�e@{�]@{��@{|�@{)_@z��@z��@zYK@y�@x��@xy>@xg8@xN�@xFt@x2�@x1@w�@vYK@uԕ@t�|@t~(@s��@s��@r��@r�B@r\�@q��@q�@p��@p?�@p�@o��@odZ@n��@n#:@m�#@m�@m*0@l��@lw�@l:�@k�@kn/@j��@j{�@j@if�@h�@h�@g��@g�w@g��@g��@gX�@fߤ@fO@e��@ec�@eJ�@e \@d��@d�@dw�@de�@dZ@d$@c��@c�@c|�@cy�@c@O@c$t@b�@be@a�@au�@a�@`��@`�@_��@_J#@^�@^ff@^Z�@^5?@^ �@]�@]��@]\�@]�@\��@\`�@\,=@[��@[t�@["�@Z�"@Z�!@Z6�@Y��@Y�h@Y�@X��@X��@Xu�@W�@W�k@W=@V�'@V��@VYK@Ve@U�@U:�@T�@T�D@T_@S�@S1�@R�@RL0@RB[@Q��@Q��@Qj@Q0�@P��@P�U@O�@O9�@O(@N�@N)�@M�j@M|@M=�@L�@L��@L/�@Kqv@J�@J}V@J@�@J.�@J#:@I�@H�@H��@HtT@HXy@H6@H�@G��@G�w@G�V@G9�@G�@F�@Fq�@E��@E�C@Ej@D��@D֡@D�@C�W@Cg�@B�@B��@B}V@B-@A�H@A�"@Ac�@A(�@A�@@�5@@�j@@�9@@|�@?�g@?�	@?_p@?!-@>��@>҉@>xl@=�>@=�3@=�-@=j@=/@<�K@<��@<��@<��@<�D@<I�@;��@;'�@:�B@:� @:YK@:�@9��@9L�@9%@8�U@8h�@8"h@7�W@7RT@6�@6��@6\�@5��@5�j@5�@5��@5u�@5`B@5+@4�e@4U2@4 �@3��@3��@3P�@3�@2�1@2l�@2Ov@2 �@1�n@1x�@1X@1%F@0�@0�_@0%�@/�*@/dZ@/�@.��@.��@.�B@.��@.u%@.c @.?@-�Z@-m]@-&�@,��@,�@,Xy@,@+��@+��@+{J@+E9@+33@*�"@*�!@*.�@)ԕ@)��@)f�@(�@(�$@(��@(w�@(K^@(@'�@'�Q@'��@'��@'qv@'$t@'�@'�@&��@&��@&-@%e,@$�@$��@$�/@$�E@$��@$�@$Ft@$~@$x@$  @#�]@#�:@#�@"��@"Q@"J@!��@!�h@!=�@ �P@ �O@ H@�;@�k@$t@
=@͟@3�@�@�X@�h@x�@5�@@�@Ĝ@��@r�@�+@�	@33@�]@�@��@�6@s�@Ta@�@��@��@hs@?}@�K@��@7�@  @�}@�@�4@S�@�@@@�H@ȴ@��@��@c @O@�o@��@�H@��@�-@}�@&�@�@�U@�@C-@�@��@�@�A@�;@�F@��@j�@,�@�c@��@��@z@?@+k@J@��@�#@��@�=@rG@J�@#�@��@��@��@r�@V�@�@� @�K@��@b�@C�@�@�@}V@J�@e@�Z@��@��@��@�7@Q�@!�@�|@�@�`@�4@w�@_@M@"h@�@�W1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Br�Br�Bq�Bq'BoOBn�BncBn}Bn}Bn}Bl�Bk�Bk�Bj�Bj�Bj�Bj�BjBjBjeBjKBjeBjBi�Bi�Bi�Bi�Bi�Bi�BjBjBi�Bi�BjBjKBj�B~�B��B��B�|B	dB	<�B	:DB	bB	��B	�EB	�B
�B
 �B
l�B
��B
��B
��B
��B
��B�BA BS�BX�Bg�B�oB�B~(B��B�:B�4B{0B�MB�BB��B�-B�B�B�B�XB�uB��B�B�"B_�B<BHB
�B
�B
�_B
�qB
��B
�=B
�_B
�B
�B
�6B
y�B
h�B
`\B
H1B
*B
B
mB	�B	�RB	�	B	�B	��B	��B	kQB	P�B	CGB	9�B	,�B	"4B	kB	mB	�B	�B	�B	�B	�B	
�B	�B	EB	�B	mB	B	B	xB	�B	�B	VB	B	0�B	B�B	V�B	bhB	{B	�7B	��B	��B	�B	� B	��B	�aB	��B	��B	�DB	�?B	�<B	��B	ĶB	��B	�3B	�%B	��B	��B	�dB	��B	�7B	��B	��B	��B	�+B	��B	��B	�)B	�"B	�"B	��B	��B	��B	� B	��B	�B	�>B	�DB	��B	��B	��B	�B	��B	��B	��B	�6B	�]B	��B	�(B	�cB	�;B	��B	�9B	ŢB	ȴB	�)B	͟B	�jB	ʌB	ǮB	��B	�KB	�B	ȴB	�^B	�B	��B	��B	�.B	�TB	՛B	�mB	ӏB	ӏB	�MB	�eB	ٴB	�+B	�_B	�1B	ٴB	��B	ܬB	��B	خB	��B	��B	҉B	�FB	�
B	՛B	�FB	�B	��B	�VB	��B	߾B	�5B	�B	ߊB	�pB	�5B	��B	ߊB	��B	ߊB	�B	� B	��B	��B	�B	�FB	��B	�tB	�B	�8B	�yB	�yB	�B	�B	�B	�_B	��B	��B	�sB	�B	�B	�B	��B	��B	��B	��B	�sB	�$B	�RB	�-B	��B	�KB	�qB	یB	�IB	�IB	ܬB	��B	�dB	ޞB	�B	�B	�FB	�B	�B	��B	��B	�DB	�$B	�B	�B	��B	�ZB	�:B	�B	��B	�B	�B	�5B	�'B	�B	��B	ߤB	�B	�B	��B	߾B	�vB	�B	�B	�B	�zB	�mB	�8B	��B	��B	�B	�XB	�XB	��B	�B	��B	�B	�DB	�B	�B	�B	�>B	�B	�=B	��B	�B	�B	��B	�)B	��B	�CB	��B	�]B	�CB	��B	�B	�B	�=B	�B	�
B	�RB	�B	�LB	�fB	�B	��B	�B	��B	��B	�B	��B	�6B	�QB	�cB	��B	� B	�B	�iB	�oB	��B	�aB	�GB	�aB	�B	��B	�B	�B	��B	��B	�*B	�B	��B	��B	�	B	��B	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	�XB	�lB	�8B	��B	�fB	�+B	�tB	��B	��B	�fB	�B	��B	��B	��B	�8B	�$B	��B	�^B	�xB	�xB	��B	�dB	�dB	�dB	��B	��B	�B	��B	��B	��B	��B	��B
B
oB
 B
 �B
 B
;B
�B
UB
 iB
 �B
 �B
UB
�B
�B
B
�B
�B
B
B
gB
�B
�B
�B
tB
�B
+B
1B
�B
	B
	lB

	B

XB

�B
�B
�B
�B
�B
VB
pB
<B
pB
�B
�B
�B
�B
�B
�B
�B
hB
B
�B
�B
�B
NB
�B
 B
oB
oB
�B
�B
B
�B
�B
�B
TB
�B
�B
�B
B
�B
B
aB
�B
uB
�B
�B
B
aB
{B
B
SB
B
B
9B
SB
�B
$B
sB
B
�B
B
KB
�B
B
kB
=B
�B
�B
)B
�B
�B
/B
dB
�B
B
jB
�B
�B
�B
!B
�B
!-B
!�B
!�B
!�B
"NB
"hB
"�B
#�B
#�B
#�B
$�B
%,B
%�B
&B
&�B
&�B
'B
'8B
(
B
(�B
)*B
)DB
)yB
*�B
+�B
+�B
+�B
+�B
,=B
-B
-wB
-�B
.�B
.�B
/5B
/�B
0!B
0UB
0;B
0;B
0�B
0�B
0�B
0�B
1'B
1�B
2�B
2�B
2�B
3B
3B
3B
2�B
3MB
3MB
33B
33B
33B
2�B
2�B
2�B
3hB
3hB
3�B
4B
4B
4TB
4TB
4�B
4�B
4�B
5ZB
6zB
6zB
6�B
6�B
72B
7�B
8B
8B
8B
7�B
7�B
8B
8�B
8�B
9	B
9�B
:^B
:B
9�B
:�B
<jB
=B
>(B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
@ B
@ B
@ B
@ B
@4B
AB
A�B
A�B
B'B
BuB
B�B
B�B
B�B
CB
CB
C�B
C�B
D3B
DMB
D�B
D�B
D�B
ESB
E�B
FB
F?B
F%B
FYB
FtB
F�B
F�B
F�B
F�B
G+B
GzB
GzB
G�B
G�B
H1B
H1B
HfB
H�B
H�B
IB
IRB
IlB
I7B
I�B
I�B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
K^B
KxB
K�B
K�B
K�B
K�B
L~B
L~B
LJB
L�B
L�B
L�B
MB
L�B
L�B
MjB
M6B
M6B
M�B
M�B
M�B
M�B
M�B
N<B
N<B
NpB
N�B
N�B
O(B
OBB
O(B
OB
OB
O�B
PB
PB
PB
PB
P.B
PHB
P.B
PHB
P�B
P�B
P}B
Q B
QB
P�B
Q4B
Q�B
QNB
Q�B
Q�B
RTB
R�B
R�B
SuB
S�B
T,B
TaB
T�B
T�B
T�B
T�B
T�B
T{B
T�B
T�B
T�B
UB
UB
UB
T�B
U�B
U�B
U�B
U�B
U�B
VB
VB
V9B
VSB
V9B
VB
VmB
V�B
W$B
WsB
W�B
W�B
W�B
XEB
XEB
X�B
X�B
X�B
YB
Y1B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[=B
[=B
[�B
[�B
\B
\CB
\CB
\�B
\�B
\�B
]dB
]dB
]~B
]�B
^B
^OB
^�B
^�B
^�B
_!B
_pB
_�B
_�B
`'B
`BB
`BB
`\B
`�B
`�B
`�B
`�B
`�B
aHB
abB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
b�B
cB
c�B
dB
d&B
d@B
d�B
d�B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
e�B
e�B
fB
e�B
e�B
fLB
fLB
ffB
f�B
g�B
gmB
gmB
gmB
g8B
g�B
g�B
g�B
h
B
h
B
g�B
hsB
h�B
h�B
iDB
i_B
i�B
i�B
j0B
jB
j�B
kB
kkB
k�B
lB
k�B
l"B
l�B
l�B
mB
mB
mB
m]B
mwB
m�B
m�B
m�B
m�B
ncB
n�B
o B
o5B
o5B
o5B
o5B
o�B
o�B
o�B
pB
p;B
p;B
pUB
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
rB
rB
rB
rGB
rGB
rGB
raB
r|B
r�B
r�B
r�B
sB
sB
r�B
s3B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
v`B
vFB
vzB
vzB
v�B
v�B
v�B
v�B
wB
w2B
w�B
wfB
wfB
w�B
w�B
w�B
x8B
xlB
x�B
x�B
y	B
y	B
y>B
y$B
y>B
yXB
y�B
y�B
zB
zB
zB
zDB
z^B
zxB
z�B
{0B
{dB
{d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Br�Br�Bq�Bq'BoOBn�BncBn}Bn}Bn}Bl�Bk�Bk�Bj�Bj�Bj�Bj�BjBjBjeBjKBjeBjBi�Bi�Bi�Bi�Bi�Bi�BjBjBi�Bi�BjBjKBj�B~�B��B��B�|B	dB	<�B	:DB	bB	��B	�EB	�B
�B
 �B
l�B
��B
��B
��B
��B
��B�BA BS�BX�Bg�B�oB�B~(B��B�:B�4B{0B�MB�BB��B�-B�B�B�B�XB�uB��B�B�"B_�B<BHB
�B
�B
�_B
�qB
��B
�=B
�_B
�B
�B
�6B
y�B
h�B
`\B
H1B
*B
B
mB	�B	�RB	�	B	�B	��B	��B	kQB	P�B	CGB	9�B	,�B	"4B	kB	mB	�B	�B	�B	�B	�B	
�B	�B	EB	�B	mB	B	B	xB	�B	�B	VB	B	0�B	B�B	V�B	bhB	{B	�7B	��B	��B	�B	� B	��B	�aB	��B	��B	�DB	�?B	�<B	��B	ĶB	��B	�3B	�%B	��B	��B	�dB	��B	�7B	��B	��B	��B	�+B	��B	��B	�)B	�"B	�"B	��B	��B	��B	� B	��B	�B	�>B	�DB	��B	��B	��B	�B	��B	��B	��B	�6B	�]B	��B	�(B	�cB	�;B	��B	�9B	ŢB	ȴB	�)B	͟B	�jB	ʌB	ǮB	��B	�KB	�B	ȴB	�^B	�B	��B	��B	�.B	�TB	՛B	�mB	ӏB	ӏB	�MB	�eB	ٴB	�+B	�_B	�1B	ٴB	��B	ܬB	��B	خB	��B	��B	҉B	�FB	�
B	՛B	�FB	�B	��B	�VB	��B	߾B	�5B	�B	ߊB	�pB	�5B	��B	ߊB	��B	ߊB	�B	� B	��B	��B	�B	�FB	��B	�tB	�B	�8B	�yB	�yB	�B	�B	�B	�_B	��B	��B	�sB	�B	�B	�B	��B	��B	��B	��B	�sB	�$B	�RB	�-B	��B	�KB	�qB	یB	�IB	�IB	ܬB	��B	�dB	ޞB	�B	�B	�FB	�B	�B	��B	��B	�DB	�$B	�B	�B	��B	�ZB	�:B	�B	��B	�B	�B	�5B	�'B	�B	��B	ߤB	�B	�B	��B	߾B	�vB	�B	�B	�B	�zB	�mB	�8B	��B	��B	�B	�XB	�XB	��B	�B	��B	�B	�DB	�B	�B	�B	�>B	�B	�=B	��B	�B	�B	��B	�)B	��B	�CB	��B	�]B	�CB	��B	�B	�B	�=B	�B	�
B	�RB	�B	�LB	�fB	�B	��B	�B	��B	��B	�B	��B	�6B	�QB	�cB	��B	� B	�B	�iB	�oB	��B	�aB	�GB	�aB	�B	��B	�B	�B	��B	��B	�*B	�B	��B	��B	�	B	��B	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	�XB	�lB	�8B	��B	�fB	�+B	�tB	��B	��B	�fB	�B	��B	��B	��B	�8B	�$B	��B	�^B	�xB	�xB	��B	�dB	�dB	�dB	��B	��B	�B	��B	��B	��B	��B	��B
B
oB
 B
 �B
 B
;B
�B
UB
 iB
 �B
 �B
UB
�B
�B
B
�B
�B
B
B
gB
�B
�B
�B
tB
�B
+B
1B
�B
	B
	lB

	B

XB

�B
�B
�B
�B
�B
VB
pB
<B
pB
�B
�B
�B
�B
�B
�B
�B
hB
B
�B
�B
�B
NB
�B
 B
oB
oB
�B
�B
B
�B
�B
�B
TB
�B
�B
�B
B
�B
B
aB
�B
uB
�B
�B
B
aB
{B
B
SB
B
B
9B
SB
�B
$B
sB
B
�B
B
KB
�B
B
kB
=B
�B
�B
)B
�B
�B
/B
dB
�B
B
jB
�B
�B
�B
!B
�B
!-B
!�B
!�B
!�B
"NB
"hB
"�B
#�B
#�B
#�B
$�B
%,B
%�B
&B
&�B
&�B
'B
'8B
(
B
(�B
)*B
)DB
)yB
*�B
+�B
+�B
+�B
+�B
,=B
-B
-wB
-�B
.�B
.�B
/5B
/�B
0!B
0UB
0;B
0;B
0�B
0�B
0�B
0�B
1'B
1�B
2�B
2�B
2�B
3B
3B
3B
2�B
3MB
3MB
33B
33B
33B
2�B
2�B
2�B
3hB
3hB
3�B
4B
4B
4TB
4TB
4�B
4�B
4�B
5ZB
6zB
6zB
6�B
6�B
72B
7�B
8B
8B
8B
7�B
7�B
8B
8�B
8�B
9	B
9�B
:^B
:B
9�B
:�B
<jB
=B
>(B
>�B
?.B
?}B
?�B
?�B
?�B
?�B
@ B
@ B
@ B
@ B
@4B
AB
A�B
A�B
B'B
BuB
B�B
B�B
B�B
CB
CB
C�B
C�B
D3B
DMB
D�B
D�B
D�B
ESB
E�B
FB
F?B
F%B
FYB
FtB
F�B
F�B
F�B
F�B
G+B
GzB
GzB
G�B
G�B
H1B
H1B
HfB
H�B
H�B
IB
IRB
IlB
I7B
I�B
I�B
J	B
J=B
J�B
J�B
J�B
J�B
J�B
K^B
KxB
K�B
K�B
K�B
K�B
L~B
L~B
LJB
L�B
L�B
L�B
MB
L�B
L�B
MjB
M6B
M6B
M�B
M�B
M�B
M�B
M�B
N<B
N<B
NpB
N�B
N�B
O(B
OBB
O(B
OB
OB
O�B
PB
PB
PB
PB
P.B
PHB
P.B
PHB
P�B
P�B
P}B
Q B
QB
P�B
Q4B
Q�B
QNB
Q�B
Q�B
RTB
R�B
R�B
SuB
S�B
T,B
TaB
T�B
T�B
T�B
T�B
T�B
T{B
T�B
T�B
T�B
UB
UB
UB
T�B
U�B
U�B
U�B
U�B
U�B
VB
VB
V9B
VSB
V9B
VB
VmB
V�B
W$B
WsB
W�B
W�B
W�B
XEB
XEB
X�B
X�B
X�B
YB
Y1B
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[=B
[=B
[�B
[�B
\B
\CB
\CB
\�B
\�B
\�B
]dB
]dB
]~B
]�B
^B
^OB
^�B
^�B
^�B
_!B
_pB
_�B
_�B
`'B
`BB
`BB
`\B
`�B
`�B
`�B
`�B
`�B
aHB
abB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
b�B
cB
c�B
dB
d&B
d@B
d�B
d�B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
e�B
e�B
fB
e�B
e�B
fLB
fLB
ffB
f�B
g�B
gmB
gmB
gmB
g8B
g�B
g�B
g�B
h
B
h
B
g�B
hsB
h�B
h�B
iDB
i_B
i�B
i�B
j0B
jB
j�B
kB
kkB
k�B
lB
k�B
l"B
l�B
l�B
mB
mB
mB
m]B
mwB
m�B
m�B
m�B
m�B
ncB
n�B
o B
o5B
o5B
o5B
o5B
o�B
o�B
o�B
pB
p;B
p;B
pUB
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
rB
rB
rB
rGB
rGB
rGB
raB
r|B
r�B
r�B
r�B
sB
sB
r�B
s3B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
tnB
t�B
t�B
t�B
t�B
u%B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
v`B
vFB
vzB
vzB
v�B
v�B
v�B
v�B
wB
w2B
w�B
wfB
wfB
w�B
w�B
w�B
x8B
xlB
x�B
x�B
y	B
y	B
y>B
y$B
y>B
yXB
y�B
y�B
zB
zB
zB
zDB
z^B
zxB
z�B
{0B
{dB
{d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104938  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174559  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174600  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174600                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024607  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024607  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                