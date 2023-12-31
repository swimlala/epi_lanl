CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-02-07T15:36:22Z creation;2018-02-07T15:36:25Z conversion to V3.1;2019-12-18T07:25:05Z update;2022-11-21T05:31:16Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180207153622  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I1_0397_127                     2C  Dd9xNAVIS_A                         0397                            ARGO 011514                     863 @�J��΀1   @�J���@;�P��{��d9x���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc|�Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D|��D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A���A���A��A���A���A��A��A��A��A��A��A��A��A���A��!A��-A��9A��9A��9A��9A��FA��RA��FA��FA��RA��^A��^A��RA��RA��RA��-A��9A���A��jA�S�A��A��-A�9XA��HA���A��7A��uA�C�A�A���A���A���A���A���A�l�A�A��\A���A��9A��!A���A���A���A�A���A�M�A�ZA���A��A�~�A�bA��\A�p�A��/A�-A�A��-A�ffA�XA���A���A��DA�&�A�$�A�Q�A���A���A�"�A��PA�
=A~��A~Q�A}dZA|-Az��Ay��Ax�`AwXAv5?Au/AtJAr�jAq"�An�HAm�hAk��Ak33Aj(�Ah��Af��AcAb�!AaƨA`�!A_��A]p�A[�PAZA�AY�AY�
AY��AV�`AT��AS�wAP��AN5?AK�AJ1AH��AG��AG
=AFA�AE�AB��A@�!A@�A@z�A@I�A@E�A?�^A?dZA>�A>ffA=��A=x�A<��A;x�A:�9A:M�A:�A9��A9p�A9dZA9�A9|�A9�A933A9�A9�A8��A8VA7��A6�yA6�RA5�7A4^5A3O�A2E�A1��A0~�A0JA/�-A/l�A/�A.�A,��A,�!A,�A,I�A,A+XA*�A*��A*r�A)�-A)+A(�jA'S�A&E�A$��A$  A#��A#XA"��A �A 1'A�TA�-A�A�AS�A�HA��A�mA&�A�+AVA1A�+A��A��A��A�HA�^AC�A��A��A  A�wA�7AXA33A�HAA�HA^5A�
A��Ax�AA1'A&�A
z�A	��A	��A	S�A�DA�
Ax�A
=A1A�`A�
A��A��AA�A �!@�C�@�bN@��y@�^5@��@�x�@�I�@�hs@�Q�@��;@�o@�p�@�w@���@�ȴ@�+@�V@��@���@�7@�(�@��H@�^5@���@�O�@�j@�@�~�@㕁@��@�{@���@߶F@ߍP@��@�ff@ݙ�@�X@�&�@ܣ�@�b@�l�@�?}@ְ!@ԃ@��@�X@Ϯ@�~�@�@�bN@��T@�K�@ě�@� �@öF@�\)@¸R@�V@���@���@��!@��F@�K�@��+@��^@��9@��@��P@�{@���@�  @��m@��;@��w@��P@���@��\@�$�@�V@��@��w@��H@��-@��@�(�@��m@�ƨ@��@���@���@�C�@���@��u@���@�;d@��y@�~�@�@���@���@�9X@�ƨ@�dZ@�E�@��-@�x�@�Ĝ@��F@�V@��@��#@�@��h@�x�@�X@��/@�j@�Q�@�9X@��@���@��F@��P@�\)@�+@���@�M�@���@��@���@���@��#@���@���@�r�@�(�@��
@�K�@���@�=q@��^@�x�@�/@��@��j@���@��@�ƨ@��@���@�|�@�C�@�@�M�@���@��@�O�@���@�z�@�1'@��@���@�C�@���@�V@�5?@���@���@���@��@�bN@�I�@�1'@� �@��@�  @���@��@��
@��@��@�l�@�dZ@�dZ@�S�@���@��@�7L@��`@��w@�dZ@��@���@��!@���@���@���@�~�@�$�@���@���@�x�@�7L@�V@��9@�1'@~�y@}��@}?}@|�D@|�@{�F@{"�@z��@y��@w��@w\)@w+@v�y@v�R@v��@vv�@vff@vE�@v@u��@u�@uO�@u�@t�j@tI�@sƨ@s@q�#@q�@p �@n�y@nv�@n@m`B@lj@l�@l1@k��@k�m@k�m@k�
@kƨ@k��@k�@kdZ@kC�@kC�@k33@ko@k@j�H@j��@j��@j��@j��@j�!@j�\@j~�@jM�@j-@i��@i7L@i%@h�u@hQ�@h  @gl�@g�@f��@f�y@f�@fȴ@fȴ@f�R@f�+@f@d��@cdZ@c"�@b�H@b��@b~�@bn�@b�@ax�@a&�@`�`@`��@`1'@_��@_|�@_+@_+@_�@_+@_;d@_+@^�y@^��@^�+@^$�@]�T@]�-@]�@]O�@]O�@]O�@]V@\�@\I�@\�@[��@[33@Z^5@Z-@Y�#@YG�@Y%@X��@X1'@Wl�@W\)@W+@V�@Vff@VE�@V$�@V{@V@V{@V@UO�@Tj@S�
@S��@St�@S"�@S@R��@R~�@RJ@Q��@Qx�@Q7L@Q%@PĜ@PA�@P �@O�@O�P@O\)@O�@O�@N��@N�@Nv�@Nv�@N5?@M��@M�@M/@MV@L��@L��@L�@L��@L�D@LZ@K��@J��@JM�@J�@I�@I�^@Ix�@IG�@I&�@HĜ@H�@Hb@G�@GK�@G
=@F�@F��@F$�@F{@E��@E��@E��@E�@E/@D��@D��@D�/@D�j@DI�@CC�@C"�@B�\@A�#@@�@@1'@@b@?
=@>v�@=@=V@<z�@<I�@<(�@<1@;�
@;��@;��@;�@;�@;�@;t�@;C�@;33@:��@9�@7��@6��@6ȴ@6��@6�+@6�+@6v�@6ff@6ff@6V@6$�@5��@5p�@5O�@5V@4�/@4��@49X@4�@3�
@3��@3t�@3"�@2��@2M�@2�@2J@1��@1��@1��@1�#@1�#@1��@1�^@1��@1�7@1hs@1X@1�@0�9@0r�@0A�@0b@/�;@/�@/�P@/;d@.�y@.ȴ@.�@.�@.�@.�@.�@.ȴ@.�R@-�@-O�@,�@,z�@+�m@+��@*�\@*J@)��@)�@)�@)�@)�@)�@)��@)��@)��@)G�@)G�@)�@)%@(�`@(��@(�9@(�@(bN@(1'@( �@(b@(  @'�@'�@'K�@&ȴ@&��@&ff@&5?@%@%?}@$�j@$�@$�@$��@$��@$��@$��@$�D@$�D@$��@$��@$�D@$z�@$�D@$�D@$Z@$�@#ƨ@#�F@#�F@#��@#t�@#C�@"�@"��@"�!@"^5@"M�@"-@"-@"�@!�@!x�@!%@ �9@ r�@ bN@ Q�@��@�w@�@|�@K�@+@�R@V@V@ff@5?@$�@$�@�@@�@�T@��@O�@?}@�@�/@I�@�F@C�@�H@��@��@�\@n�@M�@J@��@�#@�^@��@G�@r�@ �@  @�@��@�@�@��@\)@;d@+@�@�@
=@�y@E�@�-@�@��@�@�@��@��@��@��@��@��@�@�D@�@dZ@dZ@dZ@C�@"�@@�H@��@��@��@�!@=q@�#@��@hs@�9@�@��@\)@��@�+@ff@V@5?@{@@@�@��@�-@��@p�@?}@/@�@�@��@�@j@�@ƨ@��@t�@dZ@C�@"�@
��@
��@
~�@
M�@
=q@
=q@	��@	�#@	��@	�^@	��@	x�@	X@	7L@	7L@	%@��@��@��@bN@b@|�@l�@K�@�@��@�y@�y@�R@�+@v�@ff@E�@@�@@��@p�@`B@?}@�/@��@Z@��@�
@ƨ@��@��@��@t�@dZ@S�@C�@33@"�@o@o@@@�H@M�@��@��@��@��@�7@x�@hs@7L@�@ ��@ r�@ Q�@ A�@ 1'@ 1'@  �@ b@ b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��A���A���A��A���A���A��A��A��A��A��A��A��A��A���A��!A��-A��9A��9A��9A��9A��FA��RA��FA��FA��RA��^A��^A��RA��RA��RA��-A��9A���A��jA�S�A��A��-A�9XA��HA���A��7A��uA�C�A�A���A���A���A���A���A�l�A�A��\A���A��9A��!A���A���A���A�A���A�M�A�ZA���A��A�~�A�bA��\A�p�A��/A�-A�A��-A�ffA�XA���A���A��DA�&�A�$�A�Q�A���A���A�"�A��PA�
=A~��A~Q�A}dZA|-Az��Ay��Ax�`AwXAv5?Au/AtJAr�jAq"�An�HAm�hAk��Ak33Aj(�Ah��Af��AcAb�!AaƨA`�!A_��A]p�A[�PAZA�AY�AY�
AY��AV�`AT��AS�wAP��AN5?AK�AJ1AH��AG��AG
=AFA�AE�AB��A@�!A@�A@z�A@I�A@E�A?�^A?dZA>�A>ffA=��A=x�A<��A;x�A:�9A:M�A:�A9��A9p�A9dZA9�A9|�A9�A933A9�A9�A8��A8VA7��A6�yA6�RA5�7A4^5A3O�A2E�A1��A0~�A0JA/�-A/l�A/�A.�A,��A,�!A,�A,I�A,A+XA*�A*��A*r�A)�-A)+A(�jA'S�A&E�A$��A$  A#��A#XA"��A �A 1'A�TA�-A�A�AS�A�HA��A�mA&�A�+AVA1A�+A��A��A��A�HA�^AC�A��A��A  A�wA�7AXA33A�HAA�HA^5A�
A��Ax�AA1'A&�A
z�A	��A	��A	S�A�DA�
Ax�A
=A1A�`A�
A��A��AA�A �!@�C�@�bN@��y@�^5@��@�x�@�I�@�hs@�Q�@��;@�o@�p�@�w@���@�ȴ@�+@�V@��@���@�7@�(�@��H@�^5@���@�O�@�j@�@�~�@㕁@��@�{@���@߶F@ߍP@��@�ff@ݙ�@�X@�&�@ܣ�@�b@�l�@�?}@ְ!@ԃ@��@�X@Ϯ@�~�@�@�bN@��T@�K�@ě�@� �@öF@�\)@¸R@�V@���@���@��!@��F@�K�@��+@��^@��9@��@��P@�{@���@�  @��m@��;@��w@��P@���@��\@�$�@�V@��@��w@��H@��-@��@�(�@��m@�ƨ@��@���@���@�C�@���@��u@���@�;d@��y@�~�@�@���@���@�9X@�ƨ@�dZ@�E�@��-@�x�@�Ĝ@��F@�V@��@��#@�@��h@�x�@�X@��/@�j@�Q�@�9X@��@���@��F@��P@�\)@�+@���@�M�@���@��@���@���@��#@���@���@�r�@�(�@��
@�K�@���@�=q@��^@�x�@�/@��@��j@���@��@�ƨ@��@���@�|�@�C�@�@�M�@���@��@�O�@���@�z�@�1'@��@���@�C�@���@�V@�5?@���@���@���@��@�bN@�I�@�1'@� �@��@�  @���@��@��
@��@��@�l�@�dZ@�dZ@�S�@���@��@�7L@��`@��w@�dZ@��@���@��!@���@���@���@�~�@�$�@���@���@�x�@�7L@�V@��9@�1'@~�y@}��@}?}@|�D@|�@{�F@{"�@z��@y��@w��@w\)@w+@v�y@v�R@v��@vv�@vff@vE�@v@u��@u�@uO�@u�@t�j@tI�@sƨ@s@q�#@q�@p �@n�y@nv�@n@m`B@lj@l�@l1@k��@k�m@k�m@k�
@kƨ@k��@k�@kdZ@kC�@kC�@k33@ko@k@j�H@j��@j��@j��@j��@j�!@j�\@j~�@jM�@j-@i��@i7L@i%@h�u@hQ�@h  @gl�@g�@f��@f�y@f�@fȴ@fȴ@f�R@f�+@f@d��@cdZ@c"�@b�H@b��@b~�@bn�@b�@ax�@a&�@`�`@`��@`1'@_��@_|�@_+@_+@_�@_+@_;d@_+@^�y@^��@^�+@^$�@]�T@]�-@]�@]O�@]O�@]O�@]V@\�@\I�@\�@[��@[33@Z^5@Z-@Y�#@YG�@Y%@X��@X1'@Wl�@W\)@W+@V�@Vff@VE�@V$�@V{@V@V{@V@UO�@Tj@S�
@S��@St�@S"�@S@R��@R~�@RJ@Q��@Qx�@Q7L@Q%@PĜ@PA�@P �@O�@O�P@O\)@O�@O�@N��@N�@Nv�@Nv�@N5?@M��@M�@M/@MV@L��@L��@L�@L��@L�D@LZ@K��@J��@JM�@J�@I�@I�^@Ix�@IG�@I&�@HĜ@H�@Hb@G�@GK�@G
=@F�@F��@F$�@F{@E��@E��@E��@E�@E/@D��@D��@D�/@D�j@DI�@CC�@C"�@B�\@A�#@@�@@1'@@b@?
=@>v�@=@=V@<z�@<I�@<(�@<1@;�
@;��@;��@;�@;�@;�@;t�@;C�@;33@:��@9�@7��@6��@6ȴ@6��@6�+@6�+@6v�@6ff@6ff@6V@6$�@5��@5p�@5O�@5V@4�/@4��@49X@4�@3�
@3��@3t�@3"�@2��@2M�@2�@2J@1��@1��@1��@1�#@1�#@1��@1�^@1��@1�7@1hs@1X@1�@0�9@0r�@0A�@0b@/�;@/�@/�P@/;d@.�y@.ȴ@.�@.�@.�@.�@.�@.ȴ@.�R@-�@-O�@,�@,z�@+�m@+��@*�\@*J@)��@)�@)�@)�@)�@)�@)��@)��@)��@)G�@)G�@)�@)%@(�`@(��@(�9@(�@(bN@(1'@( �@(b@(  @'�@'�@'K�@&ȴ@&��@&ff@&5?@%@%?}@$�j@$�@$�@$��@$��@$��@$��@$�D@$�D@$��@$��@$�D@$z�@$�D@$�D@$Z@$�@#ƨ@#�F@#�F@#��@#t�@#C�@"�@"��@"�!@"^5@"M�@"-@"-@"�@!�@!x�@!%@ �9@ r�@ bN@ Q�@��@�w@�@|�@K�@+@�R@V@V@ff@5?@$�@$�@�@@�@�T@��@O�@?}@�@�/@I�@�F@C�@�H@��@��@�\@n�@M�@J@��@�#@�^@��@G�@r�@ �@  @�@��@�@�@��@\)@;d@+@�@�@
=@�y@E�@�-@�@��@�@�@��@��@��@��@��@��@�@�D@�@dZ@dZ@dZ@C�@"�@@�H@��@��@��@�!@=q@�#@��@hs@�9@�@��@\)@��@�+@ff@V@5?@{@@@�@��@�-@��@p�@?}@/@�@�@��@�@j@�@ƨ@��@t�@dZ@C�@"�@
��@
��@
~�@
M�@
=q@
=q@	��@	�#@	��@	�^@	��@	x�@	X@	7L@	7L@	%@��@��@��@bN@b@|�@l�@K�@�@��@�y@�y@�R@�+@v�@ff@E�@@�@@��@p�@`B@?}@�/@��@Z@��@�
@ƨ@��@��@��@t�@dZ@S�@C�@33@"�@o@o@@@�H@M�@��@��@��@��@�7@x�@hs@7L@�@ ��@ r�@ Q�@ A�@ 1'@ 1'@  �@ b@ b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�;B�B�
B�B�)B�BB��B�B�5B�B��B��B��B�B�B�B�B��BɺB�XB��B�BgmBM�B:^B)�B�BhB��B�TB��B�B�\Bo�BVB@�B/B)�B'�B!�BoB+B
��B
��B
�B
�;B
ȴB
�jB
�B
��B
�oB
�DB
�B
~�B
x�B
p�B
hsB
`BB
XB
J�B
@�B
7LB
.B
"�B
�B
1B	��B	�B	�B	�B	�mB	�
B	�jB	�-B	�B	��B	��B	�PB	�B	|�B	z�B	y�B	v�B	iyB	^5B	[#B	Q�B	E�B	9XB	33B	-B	(�B	%�B	!�B	�B	VB	%B	B	B	B	B	B	  B��B��B��B��B�B�B�mB�`B�ZB�ZB�ZB�ZB�fB�fB�fB�sB�sB�sB�mB�ZB�TB�BB�;B�#B�B�B��B��B��BɺBǮBǮBƨBÖBÖBB��B�}B�qB�^B�XB�RB�FB�9B�-B�!B�B��B��B��B��B��B��B��B�{B�uB�oB�bB�PB�DB�=B�1B�%B�B�B�B}�Bz�Bw�Bu�Br�Bo�Bm�Bl�Bk�BjBhsBhsBgmBgmBffBdZBbNB`BB^5B]/B\)B\)BZBXBVBT�BS�BQ�BP�BN�BM�BL�BJ�BH�BG�BE�BD�BB�BA�B@�B>wB=qB<jB<jB;dB;dB:^B8RB7LB6FB6FB5?B49B49B33B33B33B33B33B33B2-B2-B2-B2-B2-B2-B2-B1'B0!B1'B0!B0!B0!B0!B0!B/B/B/B/B/B/B.B,B,B-B.B-B-B-B-B,B,B.B1'B5?B5?B6FB6FB7LB6FB9XB9XB:^B@�B@�BA�BB�BD�BE�BE�BH�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BO�BO�BP�BQ�BS�BT�BW
BW
BW
BXBXBYBZBaHBcTBdZBhsBjBl�Bn�Bn�Bn�Bo�Bs�Bt�Bw�By�Bz�B|�B�B�=B�JB�PB�PB�VB�VB�\B�hB�uB�uB�uB�{B�{B��B��B��B��B��B��B��B��B��B�B�!B�?B�LB�LB�XB�^B�jB�}BBŢBƨBǮBɺB��B��B��B��B��B��B��B��B��B�B�#B�/B�5B�BB�TB�`B�fB�yB�B�B�B�B��B��B��B��B	B	B	B	B	B	B	B	B	%B	1B	
=B	DB	DB	DB	DB	hB	�B	�B	�B	&�B	(�B	+B	-B	.B	.B	.B	/B	/B	2-B	49B	5?B	7LB	9XB	:^B	<jB	@�B	F�B	K�B	N�B	Q�B	R�B	T�B	W
B	XB	[#B	e`B	ffB	gmB	hsB	iyB	jB	jB	jB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	r�B	t�B	v�B	z�B	|�B	� B	�B	�B	�+B	�7B	�PB	�VB	�VB	�VB	�VB	�\B	�\B	�\B	�bB	�bB	�hB	�hB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�9B	�?B	�RB	�XB	�^B	�dB	�qB	��B	��B	B	B	B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�BB	�TB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
hB
hB
oB
uB
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
6FB
6FB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
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
YB
ZB
ZB
ZB
ZB
ZB
ZB
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
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
`BB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�}B�B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�[B�B�BB�	B��B�B�IB��B��B�1B��BخB՛B��B��B��B�EB��B�B�FB��B�jB��B��Bi�BPB<�B,BKB�B'B�BԯB�GB��BtTBY�BC�B0!B*B)B#�B,B�B
�.B
�2B
�[B
��B
ʌB
��B
�aB
�)B
��B
�~B
�B
�4B
z^B
rGB
i�B
a�B
Y�B
LB
A�B
8�B
/�B
$�B
B

	B	��B	�B	�5B	�B	��B	�B	��B	��B	��B	�tB	�IB	�vB	�gB	}qB	{dB	{B	zB	lB	`vB	^�B	T�B	HfB	;�B	4�B	.IB	)�B	'8B	#�B	/B	bB	YB	SB	mB	SB	�B	�B	 �B��B��B��B��B�B�WB��B��B��B�B�tB�@B�fB�B��B�B�B��B�
B�,B�tB��B��BܬBچB�eB��B� B�JB�=B�1B�fB��B��B��B��B��B� B�BB��B��B��B�LB�B�3B��B�qB��B��B�vB�pB�B�xB�YB��B��B�[B��B�"B��B��B�7B�+B��B��B��B�B|6By	BwBt9Bp�BnIBl�Bl"Bk6Bh�Bh�Bg�Bg�BgBe�Bc�B`�B^�B]�B\�B]B[WBYeBV�BU�BT�BR�BRBO�BN�BM�BLJBJ=BH�BFBE�BDBB�BAoB@B?.B=VB<�B;�B;�B;B:B8B6�B6�B6FB5%B4�B3�B3hB3�B3�B3hB3�B33B2�B2�B2�B2�B2�B2�B2�B1�B1�B0�B1B0�B0oB0�B/�B/�B/OB/iB/�B/�B.�B-�B-�B.�B/5B.cB.B-�B-�B-wB-�B/�B2�B5�B5�B6�B6�B7�B7fB:*B:^B<B@�BA;BBABCaBEBF?BF�BI�BK)BK�BK�BK�BLBL0BL0BMPBM�BPbBP}BQ�BR�BT{BU�BW$BW?BW?BXEBX_BY�B[�Ba�Bc�Bd�Bh�Bj�Bl�Bo Bo5Bo5Bo�Bt9Bu�Bx8Bz*B{�B}�B��B�XB�JB��B��B�pB�pB��B��B��B��B��B��B��B��B��B��B��B�B�CB�;B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B�B��B��B�B� B�@B�aB�yB�qB�dBބB�B�B�B�B��B��B��B��B��B��B�B�LB�PB	 B	'B	B	3B	3B	B	B	9B	YB	KB	
XB	^B	DB	xB	�B	�B	B	B	pB	'B	)B	+6B	-)B	./B	./B	.B	/OB	/iB	2aB	4nB	5�B	7fB	9rB	:�B	<�B	AB	GB	K�B	O(B	RB	S&B	UMB	W?B	X_B	[�B	e`B	f�B	gmB	hsB	i�B	jB	jB	j�B	k�B	l�B	m�B	n�B	o�B	p�B	q�B	r�B	u%B	w2B	{B	}<B	�iB	�3B	�SB	�_B	��B	�jB	�pB	�pB	�pB	�<B	�\B	�\B	�vB	�bB	�bB	��B	�hB	�hB	��B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	��B	�
B	�
B	�
B	�$B	�sB	�WB	�!B	�AB	�GB	�MB	�TB	�nB	�tB	�RB	�XB	�xB	��B	��B	��B	��B	�uB	B	B	B	ðB	ÖB	ĶB	żB	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	�B	�B	�4B	�B	�B	�B	�$B	�B	�eB	�QB	�CB	�)B	�]B	�IB	�OB	�VB	�'B	�BB	�BB	�\B	��B	�B	�B	�sB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�.B
 iB
-B
3B
3B
B
9B
9B
%B
?B
+B
_B
fB
	RB

XB

XB
^B
^B
dB
JB
jB
PB
PB
PB
pB
<B
pB
pB
pB
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!B
"hB
&2B
'B
'B
'�B
'�B
'�B
'�B
'�B
(
B
'�B
'�B
(�B
)B
)B
*B
*B
*B
+B
+B
+B
,"B
,=B
-CB
.IB
/5B
0!B
0!B
0!B
0!B
0!B
0B
0!B
0B
0!B
0!B
0!B
0;B
0;B
0UB
1AB
1'B
1AB
1AB
2GB
33B
3MB
2GB
2-B
2-B
2-B
2B
2-B
2-B
2-B
2GB
2aB
3hB
4nB
4nB
5ZB
6`B
6�B
8�B
9>B
9XB
9XB
9XB
9XB
9>B
9rB
:xB
:xB
:^B
;dB
;dB
<jB
<jB
<�B
<�B
=�B
=�B
=�B
>wB
>]B
>wB
>wB
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C{B
C�B
B�B
BuB
C�B
C�B
C�B
C�B
C�B
B�B
BuB
B�B
B�B
B�B
C�B
C�B
C{B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
OB
N�B
O�B
Q B
RB
Q�B
Q�B
RB
Q�B
R�B
R�B
R�B
SB
SB
SB
T,B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W$B
W
B
W?B
XEB
YKB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
Z7B
ZQB
ZQB
\B
\B
\B
\CB
\)B
]IB
]IB
]/B
]/B
]/B
]IB
]dB
^5B
^5B
_VB
_�B
`\B
abB
a|B
b�B
cnB
cTB
cTB
cTB
cnB
dZB
d@B
dZB
dtB
dtB
dZB
dtB
dtB
e`B
e`B
ezB
ezB
ezB
ffB
f�B
ffB
g�B
g�B
gmB
gmB
h�B
h�B
hsB
hsB
iyB
i_B
iyB
i�B
i�B
iyB
jeB
jB
j�B
j�B
j�B
jeB
j�B
k�B
k�B
k�B
k�B
k�B
l�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802180032332018021800323320180218003233202211182133352022111821333520221118213335201804031939152018040319391520180403193915  JA  ARFMdecpA19c                                                                20180208003518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180207153622  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180207153623  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180207153623  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180207153624  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180207153624  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180207153624  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180207153624  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180207153624  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180207153625                      G�O�G�O�G�O�                JA  ARUP                                                                        20180207155650                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180207153810  CV  JULD            G�O�G�O�F�U                JM  ARCAJMQC2.0                                                                 20180217153233  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180217153233  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103915  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123335  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                