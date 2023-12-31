CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-29T00:35:27Z creation;2018-01-29T00:35:34Z conversion to V3.1;2019-12-19T07:50:40Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180129003527  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_204                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�H5�� 1   @�H6UUU�@:�V�Ϫ��dX�*�11   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�3D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr&fCt�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;|�D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�>fDׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��fD�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��;A��/A��/A��A��
A��
A�ȴA��-A��A���A���A��A�`BA�;dA��A�A��FA�;dA��A���A�7LA��#A���A�|�A�I�A� �A��A���A��!A���A���A���A��uA��hA��+A�\)A�I�A�E�A�9XA���A�A���A��!A��FA���A�jA� �A��A��A�  A��-A�$�A�ffA�{A�XA��\A��FA��A��A�ĜA�&�A��HA��^A��A�?}A���A��hA�t�A�33A��A�jA���A�VA�&�A�A��^A�A��DA�ZA���A�S�A�(�A�p�A���A��A�?}A�;dA�  A�^5A�p�A��RA�t�A�=qA�{A&�A}A|~�A{�PAz �Ax��Ax$�Aw�7Av�9Av$�Au�^Au�At9XAr�uAn��AlȴAk��Aj�Aj�Ai��Ah��Ag��Af�+AedZAeK�Ae%Ab��Ab^5AbbAa`BA_A^1'A]��A]/A]"�A]oA\��A[O�AZ~�AYhsAX1AU�AS��AQƨAQ&�APz�AP{AO��AOl�AO&�AN�jAN�ANE�AM�FAMK�AL�uAJ��AI7LAH�DAF-AC��ACAB�9AB=qAA��AA+A@A�A>�A=?}A<�9A<ZA;%A9��A7t�A6�A6ffA5�FA4�A3��A2�yA2^5A1�mA0�A0n�A/�hA.�A-?}A,�A+`BA*�A*ĜA*��A*ZA*$�A*  A)�TA)��A)�A)S�A)oA(�HA(1A&�+A%S�A$ȴA$�\A$M�A$�A$1A#��A#�PA#t�A#`BA"�A!�A�-A�uA��A��An�AK�AffAp�AQ�A��A�wA�A�yA��A�DAr�AE�A{A�mAO�A�9A5?AA�AA�;A
ȴA	S�A�\A��A�A�AI�A{A�#A��A�Al�AK�A
=A�A33A�AS�A33A �yA �DA E�@��F@��+@�b@���@�$�@��@�G�@�Ĝ@�9X@�F@�&�@��@�ff@�`B@�bN@�t�@�
=@�M�@�r�@ߝ�@އ+@�I�@��@��y@ف@�A�@��@���@ղ-@Ձ@�hs@���@�bN@�1'@��@�J@�&�@�bN@���@ʰ!@��#@ȼj@Ǯ@��H@�x�@Ĭ@��@���@�{@���@��9@��;@�\)@�C�@�;d@�C�@�K�@�\)@�\)@�\)@��y@�-@��`@�\)@�=q@�bN@�  @�-@�Q�@�33@���@��@�V@�I�@�o@�=q@��7@�`B@�?}@��@���@��@��9@�j@�A�@�  @��;@��@��@�ff@��@��^@�/@�j@� �@�t�@�l�@�K�@��!@��@��@�z�@� �@�dZ@��@���@�n�@��@���@�G�@�&�@��@�%@��@�ƨ@�S�@�$�@��@��T@���@��^@�@�%@�I�@�9X@�1'@�I�@�1@�ƨ@���@�C�@���@�=q@��h@�V@�b@��@��y@��@���@��R@���@���@���@�M�@���@��-@��9@�C�@���@�@���@�-@���@���@�p�@�G�@�%@���@�j@�b@��w@��@�dZ@�+@��@�
=@��H@�v�@�{@��@��
@��@��@�\)@�33@���@��H@���@�v�@��-@�(�@��
@��@�C�@�@�n�@�J@��@��@��@�Z@�A�@|�@��@|�@;d@�@~��@~��@~ff@~E�@}�@}�@}V@|��@|��@|��@|�D@|9X@{ƨ@{��@{o@z�!@z-@y7L@x��@x�@xr�@x1'@w�@w
=@vE�@v@u�h@u/@u�@t��@t��@t�/@t�@t�D@tj@tZ@t(�@s��@s��@s33@r��@r��@r�!@r�\@rM�@rJ@p��@pr�@p1'@pb@pb@p �@pb@o��@oK�@nV@n@m�@m�@m��@m?}@l�@lj@k��@k�
@k�m@k�m@kt�@k@j�H@j�!@j~�@j^5@jJ@i��@i��@i��@i��@i�7@ix�@iG�@iG�@h��@h�u@hbN@hbN@hb@g�@gK�@g;d@fȴ@fff@e�@d�/@dI�@c��@b��@ax�@`��@`��@`��@`�9@`�u@`Q�@` �@`  @_�@_�@^��@^v�@^$�@]�h@]�h@]�@]p�@]O�@\�/@\z�@\9X@\1@[�F@[t�@[S�@["�@Z��@Zn�@Z�@Y�^@Yx�@Y&�@Y%@X��@Xr�@X  @W��@W|�@W\)@W;d@W;d@W
=@VV@U�T@U��@U��@T��@Tj@T(�@S��@S�F@SdZ@So@R�@R��@Q��@Q%@P��@P �@O��@N�@Nff@NE�@M�@N@M�T@M@M��@M�@L�@L�D@L�@K��@KdZ@Ko@J�!@J��@J~�@JJ@Ihs@H��@Hr�@HbN@H1'@G�;@Gl�@G+@F�y@F�R@F5?@F$�@E@E`B@E�@D�@D�j@D��@D�D@Dj@DI�@DI�@DZ@DZ@D(�@Ct�@B��@B-@A�#@Ax�@A7L@@��@@bN@?��@?l�@>�@>$�@=��@=�h@=�@<�D@<Z@<(�@;��@:��@:~�@:-@:J@:�@:J@9�@9�7@8��@8�`@8��@8�`@8Ĝ@81'@7�@7|�@7+@7
=@6�y@6ȴ@6ff@6{@5��@5�-@5/@4j@4(�@3ƨ@3��@3t�@3C�@2��@2��@2n�@2^5@2M�@2-@1�@1�^@0��@0�u@0�u@0�@0�@0�@0r�@0bN@01'@0b@/�@/��@/K�@.�y@.ȴ@.��@.��@.�+@.��@.��@.�+@.V@.V@.5?@.{@-��@-�@-`B@-�@,��@,��@+�
@+�@+"�@*�H@*��@*��@*�H@+o@+33@*�@*�H@*�!@*^5@*J@)��@)x�@)X@)&�@(�9@(��@(�9@(�9@(��@(�u@'�;@'+@&�R@&V@&5?@&{@%�T@%��@%�@%O�@%O�@%�@$�@$�@$�@$�/@$�j@$Z@$(�@$1@#�m@#ƨ@#�F@#��@#�@#"�@"�H@"��@"^5@"-@"�@"J@!�@!��@!x�@ ��@ �u@ bN@ b@|�@l�@�@�y@��@��@��@ff@5?@��@/@�@�@�D@Z@�@��@��@�F@dZ@"�@�H@�!@~�@M�@��@��@�7@7L@%@�9@bN@ �@l�@+@�R@v�@V@{@�@�T@��@�h@`B@O�@?}@�@��@�j@z�@j@j@I�@�@�F@dZ@C�@C�@33@@�@��@~�@=q@�@X@7L@�@��@Ĝ@bN@ �@b@  @�@�w@�@��@��@|�@;d@�@��@�@ȴ@ȴ@��@ff@E�@�@�-@�h@p�@?}@�j@I�@(�@�@�@1@�m@ƨ@��@��@��@t�@t�@dZ@S�@C�@o@
n�@
�@
J@
J@	��@	��@	�@	�#@	�^@	�7@	&�@��@��@�@1'@�;@�w@�w@�w@�w@�w@�@��@�P@�P@�P@�P@|�@\)@K�@;d@;d@;d@;d@;d@;d@+@+@��@�y@�@�@ȴ@ȴ@��@�+@v�@v�@V@V@V@V@$�@{@�@��@V@z�@1@�m@��@��@t�@C�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��;A��/A��/A��A��
A��
A�ȴA��-A��A���A���A��A�`BA�;dA��A�A��FA�;dA��A���A�7LA��#A���A�|�A�I�A� �A��A���A��!A���A���A���A��uA��hA��+A�\)A�I�A�E�A�9XA���A�A���A��!A��FA���A�jA� �A��A��A�  A��-A�$�A�ffA�{A�XA��\A��FA��A��A�ĜA�&�A��HA��^A��A�?}A���A��hA�t�A�33A��A�jA���A�VA�&�A�A��^A�A��DA�ZA���A�S�A�(�A�p�A���A��A�?}A�;dA�  A�^5A�p�A��RA�t�A�=qA�{A&�A}A|~�A{�PAz �Ax��Ax$�Aw�7Av�9Av$�Au�^Au�At9XAr�uAn��AlȴAk��Aj�Aj�Ai��Ah��Ag��Af�+AedZAeK�Ae%Ab��Ab^5AbbAa`BA_A^1'A]��A]/A]"�A]oA\��A[O�AZ~�AYhsAX1AU�AS��AQƨAQ&�APz�AP{AO��AOl�AO&�AN�jAN�ANE�AM�FAMK�AL�uAJ��AI7LAH�DAF-AC��ACAB�9AB=qAA��AA+A@A�A>�A=?}A<�9A<ZA;%A9��A7t�A6�A6ffA5�FA4�A3��A2�yA2^5A1�mA0�A0n�A/�hA.�A-?}A,�A+`BA*�A*ĜA*��A*ZA*$�A*  A)�TA)��A)�A)S�A)oA(�HA(1A&�+A%S�A$ȴA$�\A$M�A$�A$1A#��A#�PA#t�A#`BA"�A!�A�-A�uA��A��An�AK�AffAp�AQ�A��A�wA�A�yA��A�DAr�AE�A{A�mAO�A�9A5?AA�AA�;A
ȴA	S�A�\A��A�A�AI�A{A�#A��A�Al�AK�A
=A�A33A�AS�A33A �yA �DA E�@��F@��+@�b@���@�$�@��@�G�@�Ĝ@�9X@�F@�&�@��@�ff@�`B@�bN@�t�@�
=@�M�@�r�@ߝ�@އ+@�I�@��@��y@ف@�A�@��@���@ղ-@Ձ@�hs@���@�bN@�1'@��@�J@�&�@�bN@���@ʰ!@��#@ȼj@Ǯ@��H@�x�@Ĭ@��@���@�{@���@��9@��;@�\)@�C�@�;d@�C�@�K�@�\)@�\)@�\)@��y@�-@��`@�\)@�=q@�bN@�  @�-@�Q�@�33@���@��@�V@�I�@�o@�=q@��7@�`B@�?}@��@���@��@��9@�j@�A�@�  @��;@��@��@�ff@��@��^@�/@�j@� �@�t�@�l�@�K�@��!@��@��@�z�@� �@�dZ@��@���@�n�@��@���@�G�@�&�@��@�%@��@�ƨ@�S�@�$�@��@��T@���@��^@�@�%@�I�@�9X@�1'@�I�@�1@�ƨ@���@�C�@���@�=q@��h@�V@�b@��@��y@��@���@��R@���@���@���@�M�@���@��-@��9@�C�@���@�@���@�-@���@���@�p�@�G�@�%@���@�j@�b@��w@��@�dZ@�+@��@�
=@��H@�v�@�{@��@��
@��@��@�\)@�33@���@��H@���@�v�@��-@�(�@��
@��@�C�@�@�n�@�J@��@��@��@�Z@�A�@|�@��@|�@;d@�@~��@~��@~ff@~E�@}�@}�@}V@|��@|��@|��@|�D@|9X@{ƨ@{��@{o@z�!@z-@y7L@x��@x�@xr�@x1'@w�@w
=@vE�@v@u�h@u/@u�@t��@t��@t�/@t�@t�D@tj@tZ@t(�@s��@s��@s33@r��@r��@r�!@r�\@rM�@rJ@p��@pr�@p1'@pb@pb@p �@pb@o��@oK�@nV@n@m�@m�@m��@m?}@l�@lj@k��@k�
@k�m@k�m@kt�@k@j�H@j�!@j~�@j^5@jJ@i��@i��@i��@i��@i�7@ix�@iG�@iG�@h��@h�u@hbN@hbN@hb@g�@gK�@g;d@fȴ@fff@e�@d�/@dI�@c��@b��@ax�@`��@`��@`��@`�9@`�u@`Q�@` �@`  @_�@_�@^��@^v�@^$�@]�h@]�h@]�@]p�@]O�@\�/@\z�@\9X@\1@[�F@[t�@[S�@["�@Z��@Zn�@Z�@Y�^@Yx�@Y&�@Y%@X��@Xr�@X  @W��@W|�@W\)@W;d@W;d@W
=@VV@U�T@U��@U��@T��@Tj@T(�@S��@S�F@SdZ@So@R�@R��@Q��@Q%@P��@P �@O��@N�@Nff@NE�@M�@N@M�T@M@M��@M�@L�@L�D@L�@K��@KdZ@Ko@J�!@J��@J~�@JJ@Ihs@H��@Hr�@HbN@H1'@G�;@Gl�@G+@F�y@F�R@F5?@F$�@E@E`B@E�@D�@D�j@D��@D�D@Dj@DI�@DI�@DZ@DZ@D(�@Ct�@B��@B-@A�#@Ax�@A7L@@��@@bN@?��@?l�@>�@>$�@=��@=�h@=�@<�D@<Z@<(�@;��@:��@:~�@:-@:J@:�@:J@9�@9�7@8��@8�`@8��@8�`@8Ĝ@81'@7�@7|�@7+@7
=@6�y@6ȴ@6ff@6{@5��@5�-@5/@4j@4(�@3ƨ@3��@3t�@3C�@2��@2��@2n�@2^5@2M�@2-@1�@1�^@0��@0�u@0�u@0�@0�@0�@0r�@0bN@01'@0b@/�@/��@/K�@.�y@.ȴ@.��@.��@.�+@.��@.��@.�+@.V@.V@.5?@.{@-��@-�@-`B@-�@,��@,��@+�
@+�@+"�@*�H@*��@*��@*�H@+o@+33@*�@*�H@*�!@*^5@*J@)��@)x�@)X@)&�@(�9@(��@(�9@(�9@(��@(�u@'�;@'+@&�R@&V@&5?@&{@%�T@%��@%�@%O�@%O�@%�@$�@$�@$�@$�/@$�j@$Z@$(�@$1@#�m@#ƨ@#�F@#��@#�@#"�@"�H@"��@"^5@"-@"�@"J@!�@!��@!x�@ ��@ �u@ bN@ b@|�@l�@�@�y@��@��@��@ff@5?@��@/@�@�@�D@Z@�@��@��@�F@dZ@"�@�H@�!@~�@M�@��@��@�7@7L@%@�9@bN@ �@l�@+@�R@v�@V@{@�@�T@��@�h@`B@O�@?}@�@��@�j@z�@j@j@I�@�@�F@dZ@C�@C�@33@@�@��@~�@=q@�@X@7L@�@��@Ĝ@bN@ �@b@  @�@�w@�@��@��@|�@;d@�@��@�@ȴ@ȴ@��@ff@E�@�@�-@�h@p�@?}@�j@I�@(�@�@�@1@�m@ƨ@��@��@��@t�@t�@dZ@S�@C�@o@
n�@
�@
J@
J@	��@	��@	�@	�#@	�^@	�7@	&�@��@��@�@1'@�;@�w@�w@�w@�w@�w@�@��@�P@�P@�P@�P@|�@\)@K�@;d@;d@;d@;d@;d@;d@+@+@��@�y@�@�@ȴ@ȴ@��@�+@v�@v�@V@V@V@V@$�@{@�@��@V@z�@1@�m@��@��@t�@C�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�BuBoB\BJBB��B�5B�;B�sB��BBB%B+BB%B	7BPBPBVB\BVBJB1B%B+BB  B��B  B+BJB	7BB��BBB��B�B�BB��B��BĜB�3B��B��B��B�{By�BO�BJB�-BjBjB��B�uB�=B�Bu�BiyBhsBhsBdZBZBI�B@�B=qB'�BB
��B
�B
�wB
ǮB
��B
� B
�oB
�VB
�JB
�B
�PB
�=B
�B
v�B
ffB
_;B
]/B
R�B
J�B
K�B
I�B
B�B
A�B
=qB
49B
"�B
DB	�B	�TB	��B	��B	��B	�B	�fB	�;B	�#B	��B	�#B	��B	�dB	�wB	B	�RB	��B	�B	�B	�B	�!B	�B	��B	�bB	�=B	}�B	o�B	ZB	P�B	J�B	]/B	ZB	[#B	YB	[#B	YB	S�B	T�B	O�B	H�B	A�B	49B	�B	\B	�B	B�B	DB	oB	JB	%B	B��B�TB�`B�B�B�
B��BɺB��B�)B��BŢBŢB�}BB��B�FB�?B�B��B��B��B��B��B�!B�!B�B�B�B�B��B��B��B��B��B�\B� B�+B�oB��B��B��B��B��B�oB�uB�\B�Br�B]/B`BB^5BO�BiyB\)BYBZBS�BS�BL�BT�BcTBbNBdZBcTBaHB_;B]/BT�BM�BJ�B/B6FB6FB33B.B7LB6FB=qB=qBC�BD�BC�BB�BD�BC�B?}B:^B0!B!�B%�B+B:^B5?B1'B0!B(�B �BoBPBBB�B�B�B��BBuBhB�B�B�B�B�BPB�B{B
=B�B�BbB\B{B�B"�B"�B!�B�B�B{BB��B�B�B�B�B�B�B�B�BuB�B%BB+B.B+B/B5?B9XB;dB;dB;dB;dB9XB6FB1'B)�B%�B%�B(�B'�B49B-B.B9XBC�B>wB>wB@�B>wBF�BK�BS�BS�BT�BT�BT�BS�BR�BVBT�BT�BR�BQ�BQ�BS�BVBS�BR�BZBXB`BB^5BYBZB]/BbNBgmBe`Bk�Bo�Bm�Bm�Bq�Bt�Bw�Bw�Bv�Br�Br�Bz�Bw�B�+B�hB�oB��B��B��B��B�B�!B�3B�9B�LB�XB�FB�FB�XB�^B�wB�jBB��B��B��B��B�B�
B�B��B��B��B��B��B�/B�TB�HB�ZB�mB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B	B	oB	�B	�B	�B	�B	�B	�B	�B	{B	bB	�B	�B	�B	�B	�B	$�B	(�B	.B	0!B	7LB	8RB	7LB	?}B	B�B	C�B	D�B	E�B	D�B	F�B	H�B	H�B	K�B	N�B	Q�B	S�B	T�B	VB	T�B	T�B	XB	W
B	YB	YB	ZB	cTB	gmB	gmB	gmB	ffB	hsB	k�B	q�B	r�B	t�B	{�B	|�B	}�B	|�B	}�B	~�B	� B	� B	� B	� B	�B	�B	�B	�%B	�+B	�%B	�%B	�%B	�B	�VB	�hB	�uB	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�FB	�XB	�jB	�qB	�wB	�qB	�qB	�wB	�}B	�wB	��B	B	ÖB	B	ÖB	ĜB	ƨB	ŢB	ƨB	ȴB	ƨB	��B	��B	ȴB	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�
B	�B	�B	�B	�5B	�5B	�5B	�/B	�/B	�5B	�BB	�BB	�HB	�NB	�TB	�TB	�NB	�NB	�ZB	�ZB	�`B	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
+B
1B
1B
1B
	7B

=B

=B
DB

=B
JB
DB
JB
PB
VB
VB
\B
\B
\B
bB
hB
bB
bB
VB
JB
PB
hB
oB
oB
{B
{B
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�B
�B
"�B
"�B
!�B
 �B
�B
 �B
 �B
!�B
"�B
#�B
"�B
"�B
"�B
#�B
#�B
!�B
 �B
$�B
$�B
%�B
&�B
%�B
$�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
&�B
,B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
-B
-B
/B
/B
0!B
1'B
1'B
1'B
0!B
/B
0!B
0!B
0!B
/B
/B
0!B
0!B
1'B
/B
.B
33B
33B
5?B
7LB
8RB
9XB
:^B
:^B
9XB
:^B
9XB
9XB
9XB
:^B
:^B
;dB
;dB
:^B
<jB
=qB
<jB
<jB
;dB
8RB
7LB
9XB
9XB
;dB
;dB
;dB
;dB
:^B
<jB
<jB
<jB
<jB
>wB
?}B
>wB
>wB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
A�B
A�B
@�B
B�B
D�B
D�B
D�B
G�B
G�B
G�B
H�B
I�B
I�B
G�B
H�B
G�B
H�B
J�B
K�B
K�B
L�B
K�B
L�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
N�B
Q�B
P�B
R�B
S�B
S�B
T�B
T�B
T�B
S�B
T�B
VB
VB
VB
T�B
T�B
VB
W
B
W
B
VB
VB
T�B
VB
XB
YB
XB
XB
XB
W
B
XB
XB
W
B
W
B
ZB
ZB
ZB
ZB
ZB
ZB
\)B
\)B
\)B
\)B
\)B
]/B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
]/B
]/B
\)B
]/B
]/B
]/B
^5B
^5B
]/B
]/B
^5B
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
bNB
aHB
bNB
bNB
aHB
aHB
`BB
_;B
bNB
cTB
dZB
dZB
dZB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
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
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
hsB
hsB
gmB
gmB
e`B
gmB
iyB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�XB�|B�-B�_B��BuBgB�B�B�BtB	lBjBjBpBBBVBdB�BYBEBSB �B�jB 4B�BJB	lB�B��BB B�DB�}B�B֡B��B�YB�B��B��B�5B�gB{�BS�B B�^BrBm�B�9B��B��B��BwBkBiDBh�Bd�B[	BK^BA�B>BB)�B�B
��B
�B
ªB
ɠB
��B
�3B
��B
�B
�<B
��B
��B
��B
��B
xB
h
B
`�B
^jB
T�B
LB
L�B
J�B
C�B
BAB
>(B
5%B
$ZB
�B	�B	�B	��B	��B	��B	��B	�B	�vB	ܒB	�B	�WB	ңB	��B	�.B	�GB	��B	�vB	��B	��B	��B	�;B	�]B	��B	��B	�xB	�B	q�B	\�B	S�B	L�B	]�B	Z�B	[�B	Y�B	[qB	YB	T{B	UgB	PbB	I�B	B[B	5�B	!�B	�B	�B	�B�`B	B	�B	B	�B	�B�+B�zB�B�CB�6B��B��B�B��B�]B��B�BƨB��B�GB�[B��B�B�iB��B��B��B�bB�yB�UB�oB�wB�CB�/B�]B�KB�KB�LB�TB�IB��B�'B��B�B��B��B��B��B��B��B��B��B�BtTB`BbB`vBQ�Bj0B]�BZ�B[�BUgBU2BN�BU�Bc�Bb�Bd�Bc�Ba�B_�B]~BU�BN�BLB1�B7�B7�B4�B/�B8RB7�B>(B>BC�BEBC�BB�BD�BC�B?�B:�B1AB#�B'RB,B:�B5�B1�B0�B)�B!�B,B�B�BBeB!BSB��B�B,B�B?BWB1BB+B�B9B2B�B�BYBhBHBMBB#B#B!�BB$BB�B�fB#BVB BB�BdBqBkB]B�BmB�B3B+B.}B+�B/�B5tB9rB;B;B;B;dB9XB6`B1�B*�B&�B'B)�B)DB4�B.}B/OB:BC�B?.B?BA;B?cBGEBLJBT,BT,BU2BU2BU2BT,BS@BVBUMBUBS[BRTBRoBTaBVSBTaBS�BZQBXyB`\B^�BY�BZ�B]�Bb�Bg�Be�Bk�Bo�Bm�Bm�Bq�Bt�Bw�Bw�Bv�Bs3BsMB{0Bx�B�+B�hB��B��B��B�+B�TB�B�;B�MB�TB��B�rB��B��B��B��B��B�"B�-B� B�B�B�B�B�
B�B�&B�@B�MBбB��B�dB�nB�B��B�B��B��B�B��B��B��B�B�B��B��B�B�B�B�B�6B�cB��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	4B	�B	 B	 B	 B	 BB	%B	)DB	.cB	0�B	7fB	8lB	7�B	?cB	B�B	C�B	D�B	E�B	D�B	F�B	H�B	H�B	K�B	N�B	RB	TB	UB	VB	U2B	U2B	X+B	W?B	YKB	YKB	Z�B	c�B	g�B	g�B	g�B	f�B	h�B	k�B	q�B	r�B	t�B	|B	}B	}�B	|�B	~B	~�B	�B	�B	� B	�B	�;B	�AB	�3B	�?B	�EB	�?B	�YB	�?B	��B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�B	�5B	�IB	�;B	�-B	�MB	�TB	�ZB	�`B	�rB	�jB	�qB	�wB	��B	��B	��B	�}B	�wB	��B	B	ÖB	��B	��B	��B	ƨB	��B	��B	��B	�B	��B	��B	�7B	�)B	�B	��B	�B	�B	�B	�B	��B	�B	�B	�2B	�?B	�7B	�KB	�KB	�B	�OB	�OB	�IB	�dB	�jB	�\B	�\B	�HB	�NB	�nB	�nB	�B	�B	�tB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�B
  B
 B
'B
B
3B
-B
GB
MB
+B
KB
1B
KB
	RB

XB

XB
^B

XB
JB
xB
dB
jB
VB
VB
vB
vB
\B
}B
hB
bB
bB
pB
~B
�B
�B
oB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
�B
�B
"�B
"�B
!�B
 �B
�B
 �B
 �B
!�B
"�B
#�B
"�B
#B
"�B
#�B
#�B
!�B
 �B
$�B
$�B
%�B
'B
%�B
%B
(�B
)B
(�B
)B
(�B
(
B
'�B
'B
,B
/ B
/B
/ B
/B
/B
/5B
/B
/B
/5B
/5B
-)B
-CB
/B
/5B
0!B
1'B
1B
1B
0!B
/5B
0!B
0!B
0;B
/5B
/5B
0;B
0;B
1'B
/OB
.cB
3MB
33B
5ZB
7LB
88B
9XB
:^B
:^B
9XB
:^B
9rB
9rB
9rB
:xB
:xB
;B
;dB
:xB
<jB
=qB
<PB
<�B
;B
8�B
7�B
9rB
9�B
;dB
;B
;dB
;dB
:xB
<�B
<jB
<�B
<jB
>wB
?cB
>wB
>�B
=�B
>wB
?�B
?}B
@iB
@iB
@�B
?�B
?�B
@�B
A�B
A�B
A�B
BuB
B�B
B�B
A�B
A�B
@�B
B�B
D�B
D�B
D�B
G�B
G�B
G�B
H�B
I�B
I�B
G�B
H�B
G�B
H�B
J�B
K�B
K�B
L�B
K�B
L�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O(B
RB
QB
SB
TB
TB
T�B
T�B
T�B
TB
T�B
VB
VB
VB
UB
UB
VB
W
B
W
B
VB
VB
U2B
VB
XB
YB
X+B
XB
XB
W$B
X+B
X+B
W$B
W?B
Z7B
Z7B
Z7B
ZB
ZB
ZB
\)B
\B
\)B
\)B
\)B
]/B
\)B
\)B
\CB
]IB
]IB
]IB
^5B
]/B
]/B
\CB
]/B
]IB
]IB
^OB
^OB
]/B
]IB
^jB
`BB
a-B
aHB
aHB
`BB
abB
abB
bNB
bNB
abB
bNB
bNB
aHB
abB
`\B
_pB
bNB
cTB
dZB
dZB
dZB
cnB
cnB
cnB
bNB
bNB
cTB
cTB
dZB
dtB
dtB
fLB
gRB
gRB
gRB
gmB
gmB
gmB
gRB
gmB
gmB
gmB
gmB
gmB
gmB
gRB
hsB
hsB
hsB
hXB
hXB
hXB
gmB
g�B
gmB
hsB
hsB
hsB
hsB
h�B
h�B
hXB
hsB
hsB
hXB
iyB
hsB
h�B
hsB
g�B
gmB
e�B
g�B
i�B
k�B
k�B
lqB
l�B
l�B
mwB
m�B
m�11111111111111111111111111111111111111111111111111111111111133331111111111111113311111111111111111111111111111111111111131133111111111111111111111111111131111111111131111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111113111111111111111111111111331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802020035302018020200353020180202003530201806221236582018062212365820180622123658201804050433352018040504333520180405043335  JA  ARFMdecpA19c                                                                20180129093521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180129003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180129003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180129003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180129003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180129003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180129003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180129003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180129003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180129003534                      G�O�G�O�G�O�                JA  ARUP                                                                        20180129005620                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180129153238  CV  JULD            G�O�G�O�F�A�                JM  ARSQJMQC2.0                                                                 20180131000000  CF  PSAL_ADJUSTED_QCB�  D� G�O�                JM  ARCAJMQC2.0                                                                 20180201153530  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180201153530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193335  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                