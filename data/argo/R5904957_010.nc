CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:05Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140805  20181024140805  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               
A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ף�UUpf1   @ף���l�@4CS����c��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      
A   A   A   @�ff@���A   A   A>ffA^ffA�  A�  A�  A�  A�  A�33A�  A���B   B  B  BffB ffB(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C'�fC*  C,  C.�C0�C2�C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C[�fC]�fC`  Ca�fCd  Cf�Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D��D	� D
  D
� D  Dy�D  D�fDfD�fDfD�fD  Dy�D��D� D  D� D  D�fDfD� D  Dy�D��Dy�D  D� D��Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D y�D ��D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0  D0y�D0��D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>y�D?  D?� D@  D@�fDA  DA� DBfDB� DC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Db��Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dyp D�-qD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�fgA ��A ��A?33A_33A�ffA�ffA�ffA�ffA�ffAϙ�A�ffA�33B 33B33B33B��B ��B(33B/��B833B@33BH33BP33BX33B`33Bh33Bp33Bx��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��B��gB��B��B��B��B��B��B��B��B��C &gC�C�C�C�C	�3C�C�C�C�C�C�C�C�C�C�C �C!�3C$�C&�C'�3C*�C,�C.&gC0&gC2&gC4�C6�C8�C:�C<�C>�C@&gCB�CD�CF�CH�CJ�CL�CM�3CP�CR�CT�CV�CX�CZ�C[�3C]�3C`�Ca�3Cd�Cf&gCh�Cj�Cl&gCn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�3C�fC�fC�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�3C�fC�fC�fC�fC���C�fC�3C�3C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�3C�fC���C�fC���C���C�fC�fC�fC�fC���C�fC�fC�fC�fC�fC���C���D 3D �3D3D�3D3D�3D3D�3D��D�3D3D�3D3D�3D3D�3D3D�3D��D	�3D
3D
�3D3D|�D3D��D	�D��D	�D��D3D|�D��D�3D3D�3D3D��D	�D�3D3D|�D��D|�D3D�3D��D|�D3D�3D3D�3D3D|�D3D�3D3D�3D3D�3D3D�3D3D�3D 3D |�D ��D!�3D"3D"�3D#3D#��D$3D$�3D%3D%�3D&3D&�3D&��D'�3D(3D(�3D)3D)�3D*3D*�3D*��D+�3D,3D,�3D-3D-�3D.3D.�3D/	�D/��D03D0|�D0��D1|�D1��D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D=��D>|�D?3D?�3D@3D@��DA3DA�3DB	�DB�3DC3DC�3DC��DD|�DE3DE�3DF3DF�3DG3DG�3DH3DH��DI3DI�3DJ3DJ|�DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db|�Db��Dc�3Dd3Dd�3De3De|�Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn	�Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dwp Dys3D�/D�]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʕ�Aʕ�AʓuAʓuAʓuAʑhAʏ\Aʏ\Aʏ\Aʏ\AʑhAʓuAʑhAʏ\AʍPAʏ\AʑhAʙ�AʅA�t�A�`BA�bNA�O�A�I�A�E�A�9XA�1'A�$�A��A��A��A�{A��A�{A�VA�  A��A���A��Aɏ\A��
A�x�A�33A�  A���A�z�A��
Aŕ�A�E�A��;A��AþwA�S�A���A��wA��!A�;dA��A���A�A�A��A���A��uA��A��mA���A��RA��mA�ƨA���A�VA���A��uA���A�p�A��A���A�hsA�%A�Q�A��^A�VA��+A� �A��FA�p�A�\)A�t�A��hA���A�
=A�p�A�x�A�XA���A�M�A���A�ƨA�VA��#A�(�A�?}A�7A~�A|�DAw"�AqK�Am��Ak\)Ai+Af�Ac�;A_��A]33AZ�`AYC�AV�AUp�AU
=AS��AO��AL�AK�^AGl�AE�^AE��AEp�ADffAAK�A?�A>�A<��A;�FA:�A8�A7��A7A69XA5x�A4��A4�DA4 �A3��A3
=A2r�A1��A1hsA0��A0Q�A/��A/dZA.�9A-�TA+x�A*^5A)K�A( �A&  A$�\A$=qA"��A!�FA �\A��AbNA�AS�A~�AXAM�A?}A
=A�AȴA  AO�A��A�;AbNA{A�;Al�A`BA�A
�A
�A
�/A
�RA
��A	��A	��A	�PA	`BA	/A�uA��A�wA|�Av�AQ�A5?AA�-AO�A ��A �uA bNA  �@�l�@���@�z�@���@��^@�p�@��@��y@�J@�/@��@�@��@�hs@�?}@�Ĝ@�  @�33@�$�@�O�@�j@�bN@��H@��
@���@�w@ߝ�@��@ڧ�@�?}@�  @�ff@Ԭ@��
@�K�@��H@ҟ�@�V@�$�@�@�@с@�?}@��@��@�%@���@���@���@���@���@��/@Гu@��;@���@���@��`@�/@��m@�
=@��T@�ȴ@��9@�Z@��@�33@���@���@���@��@�O�@�7L@��@��@���@�
=@�@���@��/@��@��P@���@���@�~�@�ff@�E�@�=q@�$�@��@�@��-@��@��@��7@��7@��@��@��9@��D@��;@�l�@�;d@�@�Ĝ@�z�@�Z@� �@��@���@�+@���@���@���@���@���@��R@��H@���@��+@�V@�E�@�x�@�b@�"�@���@��!@��\@�V@�5?@���@��T@���@��h@��h@��@�x�@�p�@�G�@��/@���@��@���@�C�@�@���@��@��T@���@��-@���@��7@��@�x�@�O�@�&�@���@���@�Z@�1'@��
@�S�@��H@���@�V@�z�@�dZ@��H@�ff@�7L@��@���@�ƨ@��F@��F@��;@�1@���@��F@�\)@���@�ȴ@���@��R@��!@��!@��\@�^5@��T@��7@�G�@���@���@��@�r�@�j@�A�@�(�@��
@��P@�C�@��@�o@�@�^5@�@��@��u@�b@���@�"�@�$�@��@��^@��-@��^@��-@��-@���@��-@��-@�7L@���@��@���@�j@�I�@�b@��w@��@�^5@�@���@��h@��7@�hs@�?}@��@���@���@��D@��@�r�@�bN@�Z@�A�@� �@�  @��
@���@��w@���@�+@���@�$�@���@���@�O�@�%@���@���@��@�t�@�33@��@��R@�ff@��@�x�@��@�z�@�1'@��@��@��@�  @���@�ƨ@��@�|�@�dZ@�dZ@�\)@�S�@�"�@���@�n�@�E�@q�C@a�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aʕ�Aʕ�AʓuAʓuAʓuAʑhAʏ\Aʏ\Aʏ\Aʏ\AʑhAʓuAʑhAʏ\AʍPAʏ\AʑhAʙ�AʅA�t�A�`BA�bNA�O�A�I�A�E�A�9XA�1'A�$�A��A��A��A�{A��A�{A�VA�  A��A���A��Aɏ\A��
A�x�A�33A�  A���A�z�A��
Aŕ�A�E�A��;A��AþwA�S�A���A��wA��!A�;dA��A���A�A�A��A���A��uA��A��mA���A��RA��mA�ƨA���A�VA���A��uA���A�p�A��A���A�hsA�%A�Q�A��^A�VA��+A� �A��FA�p�A�\)A�t�A��hA���A�
=A�p�A�x�A�XA���A�M�A���A�ƨA�VA��#A�(�A�?}A�7A~�A|�DAw"�AqK�Am��Ak\)Ai+Af�Ac�;A_��A]33AZ�`AYC�AV�AUp�AU
=AS��AO��AL�AK�^AGl�AE�^AE��AEp�ADffAAK�A?�A>�A<��A;�FA:�A8�A7��A7A69XA5x�A4��A4�DA4 �A3��A3
=A2r�A1��A1hsA0��A0Q�A/��A/dZA.�9A-�TA+x�A*^5A)K�A( �A&  A$�\A$=qA"��A!�FA �\A��AbNA�AS�A~�AXAM�A?}A
=A�AȴA  AO�A��A�;AbNA{A�;Al�A`BA�A
�A
�A
�/A
�RA
��A	��A	��A	�PA	`BA	/A�uA��A�wA|�Av�AQ�A5?AA�-AO�A ��A �uA bNA  �@�l�@���@�z�@���@��^@�p�@��@��y@�J@�/@��@�@��@�hs@�?}@�Ĝ@�  @�33@�$�@�O�@�j@�bN@��H@��
@���@�w@ߝ�@��@ڧ�@�?}@�  @�ff@Ԭ@��
@�K�@��H@ҟ�@�V@�$�@�@�@с@�?}@��@��@�%@���@���@���@���@���@��/@Гu@��;@���@���@��`@�/@��m@�
=@��T@�ȴ@��9@�Z@��@�33@���@���@���@��@�O�@�7L@��@��@���@�
=@�@���@��/@��@��P@���@���@�~�@�ff@�E�@�=q@�$�@��@�@��-@��@��@��7@��7@��@��@��9@��D@��;@�l�@�;d@�@�Ĝ@�z�@�Z@� �@��@���@�+@���@���@���@���@���@��R@��H@���@��+@�V@�E�@�x�@�b@�"�@���@��!@��\@�V@�5?@���@��T@���@��h@��h@��@�x�@�p�@�G�@��/@���@��@���@�C�@�@���@��@��T@���@��-@���@��7@��@�x�@�O�@�&�@���@���@�Z@�1'@��
@�S�@��H@���@�V@�z�@�dZ@��H@�ff@�7L@��@���@�ƨ@��F@��F@��;@�1@���@��F@�\)@���@�ȴ@���@��R@��!@��!@��\@�^5@��T@��7@�G�@���@���@��@�r�@�j@�A�@�(�@��
@��P@�C�@��@�o@�@�^5@�@��@��u@�b@���@�"�@�$�@��@��^@��-@��^@��-@��-@���@��-@��-@�7L@���@��@���@�j@�I�@�b@��w@��@�^5@�@���@��h@��7@�hs@�?}@��@���@���@��D@��@�r�@�bN@�Z@�A�@� �@�  @��
@���@��w@���@�+@���@�$�@���@���@�O�@�%@���@���@��@�t�@�33@��@��R@�ff@��@�x�@��@�z�@�1'@��@��@��@�  @���@�ƨ@��@�|�@�dZ@�dZ@�\)@�S�@�"�@���@�n�@�E�@q�C@a�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B  BB1BPB\BhB{B{B�B�B�B#�B(�B1'B5?BE�BC�BG�BVBgmBy�BjBcTB]/B_;B��BƨB��B-BS�BffB�bB�}B�qB��B�\Bv�Bm�Bl�BaHBVBN�BG�B?}B<jB49B%�B�B�)B��B��B��B��B��BŢB�9B��B�VBz�Bp�BS�BC�B7LB'�B�B
��B
�B
�#B
�B
��B
�bB
�+B
|�B
w�B
k�B
I�B
8RB
$�B
{B
B	��B	��B	�1B	w�B	dZB	ZB	P�B	9XB	,B	�B	DB��B�B�sB�5BǮB�3B�B��B�oB�hB�\B�DB�B}�By�Bu�Br�Bo�Bm�Bk�Bm�Bt�Bu�Bs�Bq�Bo�Bl�BhsBgmBgmBgmBgmBgmBiyBjBiyBffBgmBffBffBffBhsBjBiyBk�Bm�Bm�Bl�Bl�Bk�Bk�BjBiyBgmBhsBe`BdZBcTBbNBaHB`BBbNBdZBdZBdZBe`Be`Be`BffBe`Be`Be`Be`BffBe`Be`Be`BdZBdZBe`BjBs�By�B{�B|�B}�B~�B� B�B�B�B�B�B�B�%B�1B�1B�1B�1B�\B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�RB�^B�dB�dB�dB�dB�dB�jB�jB�qB�}B��BÖBŢBƨBƨBǮBǮBǮBɺB��B��B��B��B�B�HB�`B�sB�B��B��B��B��B	%B	1B	1B		7B	DB	DB	DB	VB	bB	hB	{B	{B	{B	{B	{B	{B	{B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	,B	2-B	33B	49B	=qB	@�B	A�B	B�B	B�B	C�B	E�B	F�B	G�B	J�B	L�B	N�B	VB	XB	]/B	bNB	bNB	`BB	iyB	n�B	w�B	y�B	z�B	{�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	� B	�B	�B	�1B	�=B	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�FB	�jB	�}B	��B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�/B	�;B	�;B	�BB	�HB	�TB	�ZB	�fB	�fB	�fB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B

=B

=B
DB
JB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
oB
�B
# B
3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B  BB1BPB\BhB{B{B�B�B�B#�B(�B1'B5?BE�BC�BG�BVBgmBy�BjBcTB]/B_;B��BƨB��B-BS�BffB�bB�}B�qB��B�\Bv�Bm�Bl�BaHBVBN�BG�B?}B<jB49B%�B�B�)B��B��B��B��B��BŢB�9B��B�VBz�Bp�BS�BC�B7LB'�B�B
��B
�B
�#B
�B
��B
�bB
�+B
|�B
w�B
k�B
I�B
8RB
$�B
{B
B	��B	��B	�1B	w�B	dZB	ZB	P�B	9XB	,B	�B	DB��B�B�sB�5BǮB�3B�B��B�oB�hB�\B�DB�B}�By�Bu�Br�Bo�Bm�Bk�Bm�Bt�Bu�Bs�Bq�Bo�Bl�BhsBgmBgmBgmBgmBgmBiyBjBiyBffBgmBffBffBffBhsBjBiyBk�Bm�Bm�Bl�Bl�Bk�Bk�BjBiyBgmBhsBe`BdZBcTBbNBaHB`BBbNBdZBdZBdZBe`Be`Be`BffBe`Be`Be`Be`BffBe`Be`Be`BdZBdZBe`BjBs�By�B{�B|�B}�B~�B� B�B�B�B�B�B�B�%B�1B�1B�1B�1B�\B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�RB�^B�dB�dB�dB�dB�dB�jB�jB�qB�}B��BÖBŢBƨBƨBǮBǮBǮBɺB��B��B��B��B�B�HB�`B�sB�B��B��B��B��B	%B	1B	1B		7B	DB	DB	DB	VB	bB	hB	{B	{B	{B	{B	{B	{B	{B	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	,B	2-B	33B	49B	=qB	@�B	A�B	B�B	B�B	C�B	E�B	F�B	G�B	J�B	L�B	N�B	VB	XB	]/B	bNB	bNB	`BB	iyB	n�B	w�B	y�B	z�B	{�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	� B	�B	�B	�1B	�=B	�PB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�FB	�jB	�}B	��B	��B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�/B	�;B	�;B	�BB	�HB	�TB	�ZB	�fB	�fB	�fB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B

=B

=B
DB
JB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
oB
�B
# B
3�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.05 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140805                              AO  ARCAADJP                                                                    20181024140805    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140805  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140805  QCF$                G�O�G�O�G�O�0               