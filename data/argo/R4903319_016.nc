CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-09-16T11:05:31Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200916110531  20200916110531  4903319 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @�8�Dt��1   @�8��b��@8�z�H�c�Z�11   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�fg@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp��Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	�D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~gD���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��gD�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�K4D�k411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�
=A�+A���A�VA�"�A�JA�A�JA�"�A�=qA�ZA�I�A�+A�bA�{A�A���A��A�ƨAץ�A׏\A�|�A�jA�ZA�=qA�A�AϾwA�(�A���A�ȴAÃA��FA��A���A�v�A�r�A�Q�A���A���A���A�O�A�bNA�(�A��+A��A�G�A�l�A�\)A���A�E�A�ȴA�oA�G�A�ƨA���A�A��wA�1A�O�A�S�A�K�A��+A�E�A�&�A�JA��TA�p�A�O�A���A�"�A���A���A���A�-A���A�XA�~�A�  A�A�A�-A���A�M�A��DA��RA�5?A��-A�{A�ZA���A�r�A�E�A�5?A�/A��RA��DA���A��A��A��\A�5?A���A�9XA�VA�C�A�5?A�&�A�A{�Aw��AvQ�Au�^As�Aq�
Ap��Ao%Al�AkdZAip�Ag��Ae+Ac+Aa��A^�/A]l�A\��A[�PAZJAW��AUAT�/ASO�AQt�AN�9AL��ALJAK`BAJȴAI�^AG�AG;dAF��AE��AD�AC?}AB �A@n�A?�^A>��A=t�A;��A;oA9��A8�yA7�
A7�A6bNA5�mA5S�A4bNA3l�A2�DA1�A1��A0��A/|�A/XA.��A-��A,jA+%A*~�A)�TA)\)A(��A'7LA%��A$�A$ffA$9XA$  A#ƨA#l�A#+A"n�A!l�A �DAAC�AVA�/A�uA��AI�A�
AƨA�7A�/A��A�PA�yA��A��A �A��Az�A�Av�AȴAl�A�RA^5A�A�wA�AVA
ZA	l�A	
=AE�A�hAK�A�\Al�AbNA�A&�A�+A�mAx�AS�A ��@���@��\@�1@�E�@�dZ@�`B@�P@�p�@�l�@���@��@�v�@�`B@睲@��@�\)@�;d@��y@���@�Ĝ@ߍP@���@���@�%@��
@�X@��
@�o@�ff@�@�7L@ԃ@�|�@��y@ѡ�@�33@��@́@̣�@��@���@ʧ�@��@ɡ�@�?}@�9X@��@Ɨ�@���@ļj@��;@���@���@�S�@�M�@�hs@�Q�@�\)@���@�~�@��@�7L@��9@�z�@�Z@��@�bN@��@�n�@��h@���@�z�@�1'@��P@���@���@�/@��@��/@�Z@�dZ@��H@�t�@���@�ȴ@��@���@�A�@��w@�C�@��R@�{@�x�@��9@�9X@��F@��P@��@��!@��#@�x�@���@�bN@��w@�33@���@���@��@�&�@���@��`@���@�I�@��w@���@��@���@�J@��T@���@��@�`B@��@��9@��u@�A�@�ƨ@�t�@�;d@�ȴ@��+@�~�@�M�@�$�@�J@��@��T@��T@��^@�p�@���@��u@�j@�1@��@�+@���@��H@��\@��@���@��h@�G�@��/@�z�@�I�@��m@�C�@��H@��\@�=q@��T@�x�@�/@���@��@�9X@��P@���@��@�/@�A�@�ƨ@��H@�-@��#@���@��@�G�@��/@��D@���@��F@�dZ@��@��@�~�@�$�@��T@��-@�G�@��@��@���@�bN@�1@�ƨ@���@�t�@�dZ@�+@��y@���@�ff@�E�@�$�@�J@�@��T@���@���@��7@�hs@�?}@��@��`@��@� �@�b@��@�w@��@~ff@}`B@|�@{ƨ@{t�@{t�@{C�@{o@z�@z��@z�!@z~�@z=q@y��@yX@x��@xr�@x �@w�;@wl�@w\)@w+@v�@v��@vV@v5?@v{@u�T@u�T@u��@uO�@uO�@u?}@u?}@u/@s�F@st�@sS�@r��@rM�@rJ@rJ@rJ@rJ@rJ@q��@q��@q��@q&�@p��@p��@p�@pQ�@p1'@p �@o��@o�P@oK�@o+@o
=@nȴ@n�R@nE�@m�T@m�@m?}@l��@l��@l�@l�@l��@l��@l��@lz�@lI�@l�@k�m@kt�@k@j�H@j~�@jn�@i�@i��@ix�@i&�@hĜ@hA�@g\)@f�R@e��@e/@eV@eV@d�j@dI�@cC�@b��@b�!@b�!@b�@aG�@a%@`��@`�`@`��@`1'@_�@_�w@_�@_�P@_\)@^��@]��@]�h@]O�@\��@\�j@\I�@[�m@[o@Z�\@Z^5@ZJ@Y��@Y�7@XbN@Xb@W�w@Wl�@W;d@W�@V��@V��@Vff@V5?@U@U�-@U�@U`B@U/@UV@UV@T��@Tz�@S�m@S��@S�@St�@St�@St�@St�@St�@SdZ@S33@R�H@R^5@RM�@R=q@R=q@R-@Q�@QX@PĜ@PA�@O��@O;d@O
=@N��@N@M/@L��@Lz�@Kƨ@K�F@K�F@K��@KS�@J�\@J^5@JM�@J-@JJ@I��@I��@I7L@H��@G��@G�P@Gl�@Gl�@Gl�@G;d@F�y@F��@F$�@E�@E�T@E�-@E�h@E/@D�D@D1@Cƨ@C�@CC�@C33@Co@C@B��@B��@B~�@B^5@A�#@Ax�@A�@A%@@�9@@�@@A�@@A�@@1'@@ �@?�@?�P@?l�@?l�@>��@>��@>v�@>V@>@=�-@=�@=`B@=V@<Z@;�F@;o@:��@:�!@:�\@:=q@9�#@9�7@9�@8�`@8��@81'@7�;@7��@7��@7��@7�w@7�w@7�w@7�w@7��@7�P@7;d@7
=@6ȴ@6v�@65?@6{@5@4��@4�j@4�j@4�j@4��@49X@3�
@3��@333@2�!@2=q@2-@2�@2�@1��@1�#@17L@0��@0�9@0bN@0b@/�@/�w@/K�@.v�@-�-@-/@-V@,��@,�@,�@,�@,�@,�/@,��@,�@,j@,�@,1@,1@+�m@+��@+S�@+33@+@*��@*M�@*-@*-@)�#@)�7@)�@(Ĝ@(bN@(A�@(  @'��@'+@&��@&ȴ@&�R@&��@&��@&�+@&{@%`B@%/@%V@$��@$�j@$�D@$(�@#ƨ@#C�@"��@"��@"n�@"M�@!�@!�^@!�7@!x�@!hs@!X@!X@!X@!X@!7L@!�@ ��@ ��@ bN@�@�@�P@\)@K�@+@��@ff@�h@/@�j@��@�D@z�@I�@9X@I�@9X@�
@�F@�F@�F@��@�@dZ@C�@33@"�@"�@o@o@@@�H@��@�\@n�@-@J@�@�^@��@x�@�@�9@b@��@�@\)@+@�@�y@��@�+@V@@�T@��@@�-@�-@�-@�h@p�@`B@/@V@V@��@��@�/@��@�j@��@z�@j@9X@1@ƨ@�@33@o@��@�!@�\@~�@^5@^5@M�@M�@-@�@��@�7@x�@&�@��@�`@Ĝ@�@Q�@b@�@��@��@|�@|�@K�@
=@�y@�R@��@ff@ff@V@V@5?@$�@$�@$�@$�@{@�h@`B@/@��@z�@9X@�@�m@�
@ƨ@�F@�F@�F@�F@��@dZ@C�@33@
�@
�H@
�H@
��@
n�@
-@	�#@	��@	��@	�7@	x�@	&�@	%@�`@Ĝ@��@�u@�@bN@Q�@ �@�@�@�P@l�@+@
=@��@��@ȴ@��@�+@V@@@��@�h@/@��@�j@j@9X@(�@�@1@��@�m@��@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�oA�
=A�+A���A�VA�"�A�JA�A�JA�"�A�=qA�ZA�I�A�+A�bA�{A�A���A��A�ƨAץ�A׏\A�|�A�jA�ZA�=qA�A�AϾwA�(�A���A�ȴAÃA��FA��A���A�v�A�r�A�Q�A���A���A���A�O�A�bNA�(�A��+A��A�G�A�l�A�\)A���A�E�A�ȴA�oA�G�A�ƨA���A�A��wA�1A�O�A�S�A�K�A��+A�E�A�&�A�JA��TA�p�A�O�A���A�"�A���A���A���A�-A���A�XA�~�A�  A�A�A�-A���A�M�A��DA��RA�5?A��-A�{A�ZA���A�r�A�E�A�5?A�/A��RA��DA���A��A��A��\A�5?A���A�9XA�VA�C�A�5?A�&�A�A{�Aw��AvQ�Au�^As�Aq�
Ap��Ao%Al�AkdZAip�Ag��Ae+Ac+Aa��A^�/A]l�A\��A[�PAZJAW��AUAT�/ASO�AQt�AN�9AL��ALJAK`BAJȴAI�^AG�AG;dAF��AE��AD�AC?}AB �A@n�A?�^A>��A=t�A;��A;oA9��A8�yA7�
A7�A6bNA5�mA5S�A4bNA3l�A2�DA1�A1��A0��A/|�A/XA.��A-��A,jA+%A*~�A)�TA)\)A(��A'7LA%��A$�A$ffA$9XA$  A#ƨA#l�A#+A"n�A!l�A �DAAC�AVA�/A�uA��AI�A�
AƨA�7A�/A��A�PA�yA��A��A �A��Az�A�Av�AȴAl�A�RA^5A�A�wA�AVA
ZA	l�A	
=AE�A�hAK�A�\Al�AbNA�A&�A�+A�mAx�AS�A ��@���@��\@�1@�E�@�dZ@�`B@�P@�p�@�l�@���@��@�v�@�`B@睲@��@�\)@�;d@��y@���@�Ĝ@ߍP@���@���@�%@��
@�X@��
@�o@�ff@�@�7L@ԃ@�|�@��y@ѡ�@�33@��@́@̣�@��@���@ʧ�@��@ɡ�@�?}@�9X@��@Ɨ�@���@ļj@��;@���@���@�S�@�M�@�hs@�Q�@�\)@���@�~�@��@�7L@��9@�z�@�Z@��@�bN@��@�n�@��h@���@�z�@�1'@��P@���@���@�/@��@��/@�Z@�dZ@��H@�t�@���@�ȴ@��@���@�A�@��w@�C�@��R@�{@�x�@��9@�9X@��F@��P@��@��!@��#@�x�@���@�bN@��w@�33@���@���@��@�&�@���@��`@���@�I�@��w@���@��@���@�J@��T@���@��@�`B@��@��9@��u@�A�@�ƨ@�t�@�;d@�ȴ@��+@�~�@�M�@�$�@�J@��@��T@��T@��^@�p�@���@��u@�j@�1@��@�+@���@��H@��\@��@���@��h@�G�@��/@�z�@�I�@��m@�C�@��H@��\@�=q@��T@�x�@�/@���@��@�9X@��P@���@��@�/@�A�@�ƨ@��H@�-@��#@���@��@�G�@��/@��D@���@��F@�dZ@��@��@�~�@�$�@��T@��-@�G�@��@��@���@�bN@�1@�ƨ@���@�t�@�dZ@�+@��y@���@�ff@�E�@�$�@�J@�@��T@���@���@��7@�hs@�?}@��@��`@��@� �@�b@��@�w@��@~ff@}`B@|�@{ƨ@{t�@{t�@{C�@{o@z�@z��@z�!@z~�@z=q@y��@yX@x��@xr�@x �@w�;@wl�@w\)@w+@v�@v��@vV@v5?@v{@u�T@u�T@u��@uO�@uO�@u?}@u?}@u/@s�F@st�@sS�@r��@rM�@rJ@rJ@rJ@rJ@rJ@q��@q��@q��@q&�@p��@p��@p�@pQ�@p1'@p �@o��@o�P@oK�@o+@o
=@nȴ@n�R@nE�@m�T@m�@m?}@l��@l��@l�@l�@l��@l��@l��@lz�@lI�@l�@k�m@kt�@k@j�H@j~�@jn�@i�@i��@ix�@i&�@hĜ@hA�@g\)@f�R@e��@e/@eV@eV@d�j@dI�@cC�@b��@b�!@b�!@b�@aG�@a%@`��@`�`@`��@`1'@_�@_�w@_�@_�P@_\)@^��@]��@]�h@]O�@\��@\�j@\I�@[�m@[o@Z�\@Z^5@ZJ@Y��@Y�7@XbN@Xb@W�w@Wl�@W;d@W�@V��@V��@Vff@V5?@U@U�-@U�@U`B@U/@UV@UV@T��@Tz�@S�m@S��@S�@St�@St�@St�@St�@St�@SdZ@S33@R�H@R^5@RM�@R=q@R=q@R-@Q�@QX@PĜ@PA�@O��@O;d@O
=@N��@N@M/@L��@Lz�@Kƨ@K�F@K�F@K��@KS�@J�\@J^5@JM�@J-@JJ@I��@I��@I7L@H��@G��@G�P@Gl�@Gl�@Gl�@G;d@F�y@F��@F$�@E�@E�T@E�-@E�h@E/@D�D@D1@Cƨ@C�@CC�@C33@Co@C@B��@B��@B~�@B^5@A�#@Ax�@A�@A%@@�9@@�@@A�@@A�@@1'@@ �@?�@?�P@?l�@?l�@>��@>��@>v�@>V@>@=�-@=�@=`B@=V@<Z@;�F@;o@:��@:�!@:�\@:=q@9�#@9�7@9�@8�`@8��@81'@7�;@7��@7��@7��@7�w@7�w@7�w@7�w@7��@7�P@7;d@7
=@6ȴ@6v�@65?@6{@5@4��@4�j@4�j@4�j@4��@49X@3�
@3��@333@2�!@2=q@2-@2�@2�@1��@1�#@17L@0��@0�9@0bN@0b@/�@/�w@/K�@.v�@-�-@-/@-V@,��@,�@,�@,�@,�@,�/@,��@,�@,j@,�@,1@,1@+�m@+��@+S�@+33@+@*��@*M�@*-@*-@)�#@)�7@)�@(Ĝ@(bN@(A�@(  @'��@'+@&��@&ȴ@&�R@&��@&��@&�+@&{@%`B@%/@%V@$��@$�j@$�D@$(�@#ƨ@#C�@"��@"��@"n�@"M�@!�@!�^@!�7@!x�@!hs@!X@!X@!X@!X@!7L@!�@ ��@ ��@ bN@�@�@�P@\)@K�@+@��@ff@�h@/@�j@��@�D@z�@I�@9X@I�@9X@�
@�F@�F@�F@��@�@dZ@C�@33@"�@"�@o@o@@@�H@��@�\@n�@-@J@�@�^@��@x�@�@�9@b@��@�@\)@+@�@�y@��@�+@V@@�T@��@@�-@�-@�-@�h@p�@`B@/@V@V@��@��@�/@��@�j@��@z�@j@9X@1@ƨ@�@33@o@��@�!@�\@~�@^5@^5@M�@M�@-@�@��@�7@x�@&�@��@�`@Ĝ@�@Q�@b@�@��@��@|�@|�@K�@
=@�y@�R@��@ff@ff@V@V@5?@$�@$�@$�@$�@{@�h@`B@/@��@z�@9X@�@�m@�
@ƨ@�F@�F@�F@�F@��@dZ@C�@33@
�@
�H@
�H@
��@
n�@
-@	�#@	��@	��@	�7@	x�@	&�@	%@�`@Ĝ@��@�u@�@bN@Q�@ �@�@�@�P@l�@+@
=@��@��@ȴ@��@�+@V@@@��@�h@/@��@�j@j@9X@(�@�@1@��@�m@��@S�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BS�BR�BK�B=qB-B&�B)�B-B2-B9XB@�BK�BN�BM�BJ�BL�BK�BK�BK�BI�BG�BG�BE�BC�B@�B;dB+B%�B]/BgmBm�Bp�Bq�Bq�B|�Bx�Bv�Bv�Bx�B~�B�%B�{B�oB�=B�B� B~�B}�B{�Bw�Bp�Bn�Bk�BffBcTB`BB\)BYBT�BP�BL�BF�B?}B=qB<jB;dB9XB6FB33B-B(�B"�B�B1B��B��B�B�sB�HB�B��B�qB�9B��B��B�uB�=B�By�BhsB_;B[#BM�B=qB,B�BB
�B
�yB
�)B
ǮB
�'B
��B
��B
�DB
y�B
iyB
R�B
8RB
�B
JB
%B	��B	�sB	�;B	��B	ȴB	�qB	�'B	��B	��B	�VB	�+B	|�B	s�B	o�B	iyB	aHB	VB	J�B	F�B	?}B	6FB	)�B	�B	�B	�B	�B	hB	
=B	%B	B	  B��B�B�B�sB�ZB�BB�)B��B��B��BɺBĜB��B�}B�qB�dB�RB�9B�!B�B�B��B��B��B��B��B��B�oB�bB�VB�DB�1B�B� B|�Bz�By�By�Bx�Bv�Bu�Br�Bo�Bl�BjBiyBhsBgmBe`BdZBdZBbNBaHB`BB_;B\)BVBT�BT�BS�BR�BP�BN�BI�BF�BD�B@�B>wB<jB;dB9XB8RB49B2-B0!B/B-B+B)�B'�B&�B$�B"�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B%�B%�B$�B$�B)�B.B0!B1'B1'B2-B2-B2-B49B33B5?B7LB6FB6FB8RB8RB8RB;dB<jB<jB=qB>wB@�BA�BE�BI�BJ�BO�BP�BP�BP�BQ�BT�BT�BT�BVBVBT�BT�BVBW
BYB]/BbNBiyBl�Bm�Bl�Bk�BjBjBn�Bo�Bq�Bq�Br�Bt�Bu�B~�B�B�JB�\B�hB�oB�uB��B��B��B��B��B��B�B�B�B�B�'B�3B�?B�LB�RB�^B�dB�dB�qB��B��B��BBÖBǮBȴBȴB��B��B��B��B�B�B�B�)B�5B�BB�TB�fB�sB�B�B�B��B	B	B	B	+B	1B	PB	bB	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	(�B	)�B	,B	-B	.B	0!B	33B	49B	49B	7LB	9XB	;dB	<jB	=qB	>wB	>wB	?}B	?}B	>wB	=qB	?}B	B�B	D�B	D�B	C�B	D�B	D�B	E�B	F�B	H�B	L�B	M�B	Q�B	S�B	T�B	W
B	ZB	\)B	]/B	`BB	bNB	cTB	ffB	hsB	jB	l�B	m�B	n�B	o�B	p�B	s�B	u�B	w�B	x�B	y�B	z�B	z�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�B	�%B	�1B	�1B	�7B	�7B	�7B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
bB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BS�BR�BK�B=qB-B&�B)�B-B2-B9XB@�BK�BN�BM�BJ�BL�BK�BK�BK�BI�BG�BG�BE�BC�B@�B;dB+B%�B]/BgmBm�Bp�Bq�Bq�B|�Bx�Bv�Bv�Bx�B~�B�%B�{B�oB�=B�B� B~�B}�B{�Bw�Bp�Bn�Bk�BffBcTB`BB\)BYBT�BP�BL�BF�B?}B=qB<jB;dB9XB6FB33B-B(�B"�B�B1B��B��B�B�sB�HB�B��B�qB�9B��B��B�uB�=B�By�BhsB_;B[#BM�B=qB,B�BB
�B
�yB
�)B
ǮB
�'B
��B
��B
�DB
y�B
iyB
R�B
8RB
�B
JB
%B	��B	�sB	�;B	��B	ȴB	�qB	�'B	��B	��B	�VB	�+B	|�B	s�B	o�B	iyB	aHB	VB	J�B	F�B	?}B	6FB	)�B	�B	�B	�B	�B	hB	
=B	%B	B	  B��B�B�B�sB�ZB�BB�)B��B��B��BɺBĜB��B�}B�qB�dB�RB�9B�!B�B�B��B��B��B��B��B��B�oB�bB�VB�DB�1B�B� B|�Bz�By�By�Bx�Bv�Bu�Br�Bo�Bl�BjBiyBhsBgmBe`BdZBdZBbNBaHB`BB_;B\)BVBT�BT�BS�BR�BP�BN�BI�BF�BD�B@�B>wB<jB;dB9XB8RB49B2-B0!B/B-B+B)�B'�B&�B$�B"�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B%�B%�B$�B$�B)�B.B0!B1'B1'B2-B2-B2-B49B33B5?B7LB6FB6FB8RB8RB8RB;dB<jB<jB=qB>wB@�BA�BE�BI�BJ�BO�BP�BP�BP�BQ�BT�BT�BT�BVBVBT�BT�BVBW
BYB]/BbNBiyBl�Bm�Bl�Bk�BjBjBn�Bo�Bq�Bq�Br�Bt�Bu�B~�B�B�JB�\B�hB�oB�uB��B��B��B��B��B��B�B�B�B�B�'B�3B�?B�LB�RB�^B�dB�dB�qB��B��B��BBÖBǮBȴBȴB��B��B��B��B�B�B�B�)B�5B�BB�TB�fB�sB�B�B�B��B	B	B	B	+B	1B	PB	bB	{B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	(�B	(�B	)�B	,B	-B	.B	0!B	33B	49B	49B	7LB	9XB	;dB	<jB	=qB	>wB	>wB	?}B	?}B	>wB	=qB	?}B	B�B	D�B	D�B	C�B	D�B	D�B	E�B	F�B	H�B	L�B	M�B	Q�B	S�B	T�B	W
B	ZB	\)B	]/B	`BB	bNB	cTB	ffB	hsB	jB	l�B	m�B	n�B	o�B	p�B	s�B	u�B	w�B	x�B	y�B	z�B	z�B	{�B	{�B	|�B	}�B	~�B	� B	�B	�B	�%B	�1B	�1B	�7B	�7B	�7B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	B	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
bB
hB
hB
hB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200916110531                              AO  ARCAADJP                                                                    20200916110531    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200916110531  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200916110531  QCF$                G�O�G�O�G�O�0               