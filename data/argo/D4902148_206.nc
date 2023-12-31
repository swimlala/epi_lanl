CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-07T15:37:52Z creation;2020-04-07T15:37:58Z conversion to V3.1;2022-11-21T05:27:17Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        p  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  It   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  MP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ̬   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܬ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200407153752  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_206                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"�l 1   @�#��� @:��!-w�du���o1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���A ��A ��A@��A`��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB 33B33B33B33B 33B(33B033B833B@33BH33BP33BX33B`33Bh33Bp33Bx33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC���C�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fD 3D �3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$�3D%3D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.3D.�3D/3D/�3D03D0�3D13D1�3D23D2�3D33D3�3D43D4�3D53D5�3D63D6�3D73D7�3D83D8�3D93D9�3D:3D:�3D;3D;�3D<3D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB3DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG3DG�3DH3DH�3DI3DI�3DJ3DJ�3DK3DK�3DL3DL�3DM3DM�3DN3DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY��DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh�3Di3Di�3Dj3Dj�3Dk3Dk�3Dl3Dl�3Dm3Dm�3Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du3Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�D�~fD���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�DၚD���D��D�A�D⁚D���D��D�A�DずD���D��D�A�D䁚D���D��D�A�D做D���D��D�A�D恚D���D��D�A�D灚D���D��D�A�D聚D���D��D�A�D遚D���D��D�A�DꁚD���D��D�A�D끚D���D��D�A�D쁚D���D��D�A�D큚D���D��D�A�DD���D��D�A�DD���D��D�A�D���D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D�D���D��D�A�D���D���D��D�A�D���D���D��D�Nf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�`BA�`BA�^5A�ZA�bNA�bNA�dZA�hsA�hsA�hsA�XA�O�A�M�A�M�A�Q�A�Q�A�M�A�E�A�A�A�(�A�{A��A��A��mA��#A��
A��
A���A���A�v�A�ffA�VA��A�7LA�I�A�`BA��A��A���A�bNA���A��-A��\A�v�A�XA���A��A��mA���A���A��A���A�v�A�JA�XA���A�v�A�"�A��A�G�A���A��A��\A���A���A��uA���A�C�A�-A���A�
=A�-A�XA���A���A�S�A��A�z�A�v�A��PA��A��\A�ĜA��A�\)A�p�A��A���A�ffA�/A��mA��uA���A���A���A�A�C�A��A%A|�A{�wA{7LAz�RAy�
Au�At�DAtVAt=qAs�-Arr�Ao�#Am�#Al��Al1Ajr�Ai+AhJAf~�Ae7LAd�Ab�Ab$�Aa�A`�A_A^�jA[AX��AW�wAWdZAV�AVr�AU��AU�ASƨARZAO�mAN��AM�;AM33AL~�AK��AKG�AIdZAG��AF�jAF��AF~�AF{AD��AC&�ABn�AA��AA"�A@��A>�A=�A<ffA;�7A9`BA7ƨA7�A7dZA7S�A6�jA5O�A4��A4Q�A4{A2�jA1"�A0(�A/�-A/oA.~�A.E�A-�A-��A-l�A,M�A+A+��A+K�A*�A*ffA)&�A(�RA(�A(-A'��A'/A&�A&E�A%`BA%+A$��A$ȴA$v�A$9XA#�mA#oA!�PA �+A�TA�uA�AAM�A�^A�A  AXA�A�9A��AbNA5?A�-A�A�AdZA��An�A��A�hA��A��AI�A��AoA��A~�A�A+A��A	�A�uA$�A��A`BAI�AG�AȴA��AjA1'AƨAdZA�A �HA �A r�@���@�o@�M�@��u@��@��@��@���@�1'@���@�^@@�+@�/@�bN@�|�@�  @�@��@�P@◍@�{@�@��@�\)@ޟ�@�9X@�l�@۝�@ۥ�@ۥ�@��@�V@�E�@���@�?}@�@ԃ@ёh@�  @϶F@�S�@�/@�n�@ɑh@�G�@ȴ9@���@��;@�@��#@�j@�~�@��@�O�@���@���@�  @���@�dZ@�"�@��@��!@�n�@���@���@��j@�\)@�V@�&�@��w@�hs@�%@���@��@���@��u@��@���@�$�@�Z@�~�@���@��@���@��#@���@�V@��D@�(�@��
@���@�o@�ff@��^@��@�ƨ@���@�E�@�{@��^@�?}@�  @��@�@��@�ȴ@���@�ff@�E�@�-@���@��^@�O�@��j@�1@�dZ@��R@�ff@�E�@�J@��#@��^@��7@�?}@���@��;@�@��@�x�@�%@��u@�bN@�Q�@�I�@� �@���@�l�@�\)@�;d@��@��-@���@�(�@�ƨ@�33@���@�V@�$�@�J@��^@�V@��`@���@��j@��@���@���@��@�I�@���@�ȴ@�ff@�V@�=q@�5?@�-@�@���@���@��9@��u@�j@�Z@�Q�@��@���@�K�@�
=@��@���@��!@�n�@�@��@��7@�X@�%@���@��9@�r�@�I�@�b@�P@~��@~5?@}�@}�T@}@}p�@}�@|�@|�/@|�j@|Z@{�F@{t�@{@z��@z~�@y��@y��@yx�@y7L@y&�@y&�@y�@y�@y%@x�`@x�9@x��@x��@x�@x1'@x �@w�@w�@v�+@v$�@u�@uO�@u/@t�j@t�@sƨ@s33@r��@rM�@q��@q��@q�#@q��@qX@p  @o�w@o|�@n�y@n��@n$�@m��@mO�@mV@l��@lI�@k��@k"�@j��@j^5@i�#@ihs@h��@gl�@g+@f�@f�+@fv�@fv�@f5?@e��@eO�@e?}@d�@d��@d�@cdZ@co@b��@b�\@a�@ax�@a%@`Ĝ@`bN@_�;@_+@^�R@^��@^�+@^5?@]�T@]��@]`B@\��@\�j@\z�@\Z@\1@[S�@[33@[o@Zn�@Y�@Yhs@YG�@Y�@X�`@X�`@X��@X�9@XA�@W�w@W;d@Vȴ@Vv�@U@U�-@U�T@V@V5?@Vȴ@V�R@V�+@V5?@U��@T��@T9X@S�
@S��@S�m@SC�@R�!@R~�@RM�@R�@Q�@Q��@Q�@PĜ@P�9@P��@P�u@PA�@Ol�@N$�@M?}@L�@Lz�@L(�@K��@K"�@J~�@J=q@J�@J�@JJ@I�#@I��@Ix�@I7L@H��@H�u@G�@G��@GK�@G+@F�R@Fff@F@E�h@EO�@D�/@Dz�@C��@C�F@CS�@B�@B��@B~�@Bn�@B^5@B~�@B��@BM�@B-@A�@A��@Ahs@A%@@Ĝ@@�@@  @?��@?l�@?�@>ȴ@>��@>��@>�+@>v�@>v�@>V@>5?@>$�@=�@=�-@=O�@<�/@<�D@<�@;��@;�@;dZ@;"�@;o@:�@:�H@:��@:��@:�!@:�\@:~�@:^5@9��@9X@9�@8Ĝ@8r�@8 �@7��@7�@7K�@6�R@6v�@6E�@5�@5�T@5�T@5�T@5�-@5p�@5O�@4��@4��@4z�@3��@3t�@3dZ@3C�@3o@2�@2�!@2~�@2M�@2�@1G�@0��@0�`@0Ĝ@0��@0�@0Q�@/��@/�w@/�@/��@/�P@/|�@/l�@/\)@/K�@/+@/
=@.�y@.�+@.ff@-�T@-�h@-p�@-?}@-/@,�@,�j@,�j@,�@,j@,1@+��@+S�@+33@+"�@*�@*��@*��@*~�@*=q@)�#@)x�@)7L@(��@(Ĝ@(�9@(�@(A�@'�;@'��@'|�@'l�@';d@'�@'
=@&�y@&ȴ@&��@&�+@&$�@%�@%��@%�@%O�@%�@$�j@$Z@$�@#��@#ƨ@#��@#��@#��@#��@#dZ@#33@#"�@"�H@"�!@"n�@"J@!��@!�@!�#@!�^@!��@!��@!hs@!�@ ��@ bN@   @�w@�w@�@�@�P@;d@��@�y@��@�y@�y@�y@ȴ@�R@��@�+@E�@5?@$�@@��@�-@�@?}@?}@/@�@�@�@��@j@(�@�
@��@��@��@C�@��@�!@��@~�@-@�@��@��@��@��@��@x�@x�@X@��@��@�@ �@�@|�@K�@�y@�R@5?@@��@��@�@O�@/@V@��@�/@��@z�@�@��@�m@��@ƨ@��@��@�@�@dZ@o@��@��@�\@M�@-@�@��@x�@X@&�@%@�9@r�@ �@  @��@�@|�@
=@v�@ff@{@�h@O�@��@��@�@�D@z�@z�@j@j@Z@(�@1@�m@��@C�@33@
�@
�@
��@
~�@
M�@
-@
�@
J@	�#@	��@	X@	�@	%@�9@�@r�@r�@r�@r�@r�@bN@bN@1'@�;@�P@;d@
=@ȴ@ȴ@ȴ@ȴ@��@v�@ff@E�@$�@@�@��@@��@��@�@p�@?}@��@�j@�j@�@�@�@�@��@Z@��@�
@��@dZ@o@��@�\@~�@~�@~�@^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�`BA�`BA�^5A�ZA�bNA�bNA�dZA�hsA�hsA�hsA�XA�O�A�M�A�M�A�Q�A�Q�A�M�A�E�A�A�A�(�A�{A��A��A��mA��#A��
A��
A���A���A�v�A�ffA�VA��A�7LA�I�A�`BA��A��A���A�bNA���A��-A��\A�v�A�XA���A��A��mA���A���A��A���A�v�A�JA�XA���A�v�A�"�A��A�G�A���A��A��\A���A���A��uA���A�C�A�-A���A�
=A�-A�XA���A���A�S�A��A�z�A�v�A��PA��A��\A�ĜA��A�\)A�p�A��A���A�ffA�/A��mA��uA���A���A���A�A�C�A��A%A|�A{�wA{7LAz�RAy�
Au�At�DAtVAt=qAs�-Arr�Ao�#Am�#Al��Al1Ajr�Ai+AhJAf~�Ae7LAd�Ab�Ab$�Aa�A`�A_A^�jA[AX��AW�wAWdZAV�AVr�AU��AU�ASƨARZAO�mAN��AM�;AM33AL~�AK��AKG�AIdZAG��AF�jAF��AF~�AF{AD��AC&�ABn�AA��AA"�A@��A>�A=�A<ffA;�7A9`BA7ƨA7�A7dZA7S�A6�jA5O�A4��A4Q�A4{A2�jA1"�A0(�A/�-A/oA.~�A.E�A-�A-��A-l�A,M�A+A+��A+K�A*�A*ffA)&�A(�RA(�A(-A'��A'/A&�A&E�A%`BA%+A$��A$ȴA$v�A$9XA#�mA#oA!�PA �+A�TA�uA�AAM�A�^A�A  AXA�A�9A��AbNA5?A�-A�A�AdZA��An�A��A�hA��A��AI�A��AoA��A~�A�A+A��A	�A�uA$�A��A`BAI�AG�AȴA��AjA1'AƨAdZA�A �HA �A r�@���@�o@�M�@��u@��@��@��@���@�1'@���@�^@@�+@�/@�bN@�|�@�  @�@��@�P@◍@�{@�@��@�\)@ޟ�@�9X@�l�@۝�@ۥ�@ۥ�@��@�V@�E�@���@�?}@�@ԃ@ёh@�  @϶F@�S�@�/@�n�@ɑh@�G�@ȴ9@���@��;@�@��#@�j@�~�@��@�O�@���@���@�  @���@�dZ@�"�@��@��!@�n�@���@���@��j@�\)@�V@�&�@��w@�hs@�%@���@��@���@��u@��@���@�$�@�Z@�~�@���@��@���@��#@���@�V@��D@�(�@��
@���@�o@�ff@��^@��@�ƨ@���@�E�@�{@��^@�?}@�  @��@�@��@�ȴ@���@�ff@�E�@�-@���@��^@�O�@��j@�1@�dZ@��R@�ff@�E�@�J@��#@��^@��7@�?}@���@��;@�@��@�x�@�%@��u@�bN@�Q�@�I�@� �@���@�l�@�\)@�;d@��@��-@���@�(�@�ƨ@�33@���@�V@�$�@�J@��^@�V@��`@���@��j@��@���@���@��@�I�@���@�ȴ@�ff@�V@�=q@�5?@�-@�@���@���@��9@��u@�j@�Z@�Q�@��@���@�K�@�
=@��@���@��!@�n�@�@��@��7@�X@�%@���@��9@�r�@�I�@�b@�P@~��@~5?@}�@}�T@}@}p�@}�@|�@|�/@|�j@|Z@{�F@{t�@{@z��@z~�@y��@y��@yx�@y7L@y&�@y&�@y�@y�@y%@x�`@x�9@x��@x��@x�@x1'@x �@w�@w�@v�+@v$�@u�@uO�@u/@t�j@t�@sƨ@s33@r��@rM�@q��@q��@q�#@q��@qX@p  @o�w@o|�@n�y@n��@n$�@m��@mO�@mV@l��@lI�@k��@k"�@j��@j^5@i�#@ihs@h��@gl�@g+@f�@f�+@fv�@fv�@f5?@e��@eO�@e?}@d�@d��@d�@cdZ@co@b��@b�\@a�@ax�@a%@`Ĝ@`bN@_�;@_+@^�R@^��@^�+@^5?@]�T@]��@]`B@\��@\�j@\z�@\Z@\1@[S�@[33@[o@Zn�@Y�@Yhs@YG�@Y�@X�`@X�`@X��@X�9@XA�@W�w@W;d@Vȴ@Vv�@U@U�-@U�T@V@V5?@Vȴ@V�R@V�+@V5?@U��@T��@T9X@S�
@S��@S�m@SC�@R�!@R~�@RM�@R�@Q�@Q��@Q�@PĜ@P�9@P��@P�u@PA�@Ol�@N$�@M?}@L�@Lz�@L(�@K��@K"�@J~�@J=q@J�@J�@JJ@I�#@I��@Ix�@I7L@H��@H�u@G�@G��@GK�@G+@F�R@Fff@F@E�h@EO�@D�/@Dz�@C��@C�F@CS�@B�@B��@B~�@Bn�@B^5@B~�@B��@BM�@B-@A�@A��@Ahs@A%@@Ĝ@@�@@  @?��@?l�@?�@>ȴ@>��@>��@>�+@>v�@>v�@>V@>5?@>$�@=�@=�-@=O�@<�/@<�D@<�@;��@;�@;dZ@;"�@;o@:�@:�H@:��@:��@:�!@:�\@:~�@:^5@9��@9X@9�@8Ĝ@8r�@8 �@7��@7�@7K�@6�R@6v�@6E�@5�@5�T@5�T@5�T@5�-@5p�@5O�@4��@4��@4z�@3��@3t�@3dZ@3C�@3o@2�@2�!@2~�@2M�@2�@1G�@0��@0�`@0Ĝ@0��@0�@0Q�@/��@/�w@/�@/��@/�P@/|�@/l�@/\)@/K�@/+@/
=@.�y@.�+@.ff@-�T@-�h@-p�@-?}@-/@,�@,�j@,�j@,�@,j@,1@+��@+S�@+33@+"�@*�@*��@*��@*~�@*=q@)�#@)x�@)7L@(��@(Ĝ@(�9@(�@(A�@'�;@'��@'|�@'l�@';d@'�@'
=@&�y@&ȴ@&��@&�+@&$�@%�@%��@%�@%O�@%�@$�j@$Z@$�@#��@#ƨ@#��@#��@#��@#��@#dZ@#33@#"�@"�H@"�!@"n�@"J@!��@!�@!�#@!�^@!��@!��@!hs@!�@ ��@ bN@   @�w@�w@�@�@�P@;d@��@�y@��@�y@�y@�y@ȴ@�R@��@�+@E�@5?@$�@@��@�-@�@?}@?}@/@�@�@�@��@j@(�@�
@��@��@��@C�@��@�!@��@~�@-@�@��@��@��@��@��@x�@x�@X@��@��@�@ �@�@|�@K�@�y@�R@5?@@��@��@�@O�@/@V@��@�/@��@z�@�@��@�m@��@ƨ@��@��@�@�@dZ@o@��@��@�\@M�@-@�@��@x�@X@&�@%@�9@r�@ �@  @��@�@|�@
=@v�@ff@{@�h@O�@��@��@�@�D@z�@z�@j@j@Z@(�@1@�m@��@C�@33@
�@
�@
��@
~�@
M�@
-@
�@
J@	�#@	��@	X@	�@	%@�9@�@r�@r�@r�@r�@r�@bN@bN@1'@�;@�P@;d@
=@ȴ@ȴ@ȴ@ȴ@��@v�@ff@E�@$�@@�@��@@��@��@�@p�@?}@��@�j@�j@�@�@�@�@��@Z@��@�
@��@dZ@o@��@�\@~�@~�@~�@^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�LB�jB�}B�wB�wB�wB�wB�wB�wB�}B�}B�}B��B��B�qB�^B�jB�wB�qB�jB�XB�?B�B��B{�BT�B@�B.B�BB�mB�BB�LB�3B�B��B��B�oBx�BaHBL�BF�B;dB0!B�BDBB  B
��B
��B
�B
�BB
ÖB
�-B
��B
��B
�oB
�JB
�%B
w�B
n�B
jB
dZB
\)B
C�B
=qB
;dB
:^B
7LB
-B
�B
\B
1B
B	��B	�B	�B	�HB	�B	��B	��B	ȴB	ƨB	��B	�RB	�'B	��B	�oB	�JB	�7B	�%B	�B	|�B	x�B	p�B	gmB	]/B	W
B	R�B	O�B	L�B	I�B	D�B	:^B	1'B	1'B	49B	7LB	8RB	2-B	)�B	%�B	"�B	 �B	�B	�B	\B	JB	+B��B��B��B��B��B��B�B�B�B�sB�NB�#B�
B��B��B��B��B��BɺBȴBÖB��B��B�wB�dB�RB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�+B�B�B� B}�Bz�Bx�Bv�Bt�Bt�Bs�Bs�Bq�Bp�Bm�Bk�BiyBgmBffBe`BbNB_;B\)BZBYBXBVBVBS�BP�BL�BI�BG�BE�BB�B@�B?}B>wB=qB=qB<jB<jB;dB;dB:^B:^B9XB9XB8RB7LB6FB5?B5?B49B33B1'B/B.B-B,B)�B)�B(�B'�B'�B&�B&�B'�B'�B'�B&�B'�B&�B&�B$�B$�B#�B#�B#�B#�B#�B"�B"�B!�B"�B"�B%�B%�B%�B$�B%�B'�B'�B'�B&�B'�B+B,B-B-B0!B1'B1'B2-B2-B33B49B5?B5?B5?B6FB6FB7LB7LB8RB;dB>wB@�BC�BI�BI�BI�BI�BJ�BJ�BI�BJ�BL�BP�BVBZB^5BaHBdZBdZBffBgmBiyBjBjBl�Bn�Bp�Br�Bw�B{�B{�B{�B|�B}�B�B�7B�7B�=B�=B�DB�JB�PB�PB�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�-B�LB�dB�wB��BBÖBÖBÖBǮBȴBȴBɺB��B��B�B�;B�NB�fB�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	%B	
=B	
=B	DB	DB	DB	JB	\B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	&�B	(�B	-B	-B	1'B	2-B	49B	5?B	6FB	8RB	9XB	;dB	=qB	@�B	C�B	D�B	E�B	E�B	G�B	H�B	I�B	I�B	J�B	K�B	N�B	P�B	T�B	W
B	XB	\)B	_;B	`BB	aHB	`BB	`BB	`BB	`BB	`BB	aHB	dZB	ffB	gmB	iyB	k�B	k�B	k�B	l�B	l�B	m�B	p�B	q�B	r�B	t�B	u�B	v�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�JB	�PB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�FB	�RB	�XB	�XB	�^B	�qB	�}B	��B	B	ĜB	ŢB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
%B
+B
1B
	7B

=B
DB
JB
PB
PB
VB
VB
\B
bB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
6FB
7LB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
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
:^B
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
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
aHB
aHB
aHB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
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
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�B�B�LB�RB�B�RB�B��B��B�zB��B��B��B��B�B��B��B��B��B�}B� B��B��B��B��B��B�B��B��B��B�"B�.B��B��B��B��B��B��B�BW�BB[B/�B \B+B��B��B�gB�8B�B�;B��B��B�
B}BdtBN"BH�B=�B3�B�B~B�B �B
��B
�DB
�B
�@B
��B
�nB
��B
�IB
��B
��B
�KB
y$B
oiB
kkB
ffB
`B
D�B
=�B
;�B
;B
9$B
/�B
�B
�B
	�B
�B	�^B	�B	�WB	��B	�QB	֡B	͹B	�RB	��B	�'B	�DB	��B	��B	�uB	��B	��B	��B	��B	~(B	z�B	r�B	i�B	^�B	X_B	S�B	P�B	M�B	KB	F�B	<PB	2-B	1vB	4�B	8RB	:*B	3�B	+B	&�B	#�B	!�B	�B	�B	�B	�B		�B�}B�DB�*B�DB��B�fB�vB�)B�6B�KB�&B�]BרB��BѝB�BB�VB�0B�=B�	B�MB��B� B�.B�PB��B��B��B��B��B��B��B��B��B� B��B�-B�BB�;B��B��B�KB��B��B��B�fB��B�B��B~�B|By�BwfBuBt�BtBt9Br|Bq�Bn�Bl�BjKBh$BgBffBd@B`�B\�BZ�BY�BX�BV�BV�BU�BSBOBBKBH�BG�BDMBBB@�B?B=�B=�B<�B<�B<B;�B:�B:�B9�B:B9	B8B7fB6FB5�B5B4�B33B0!B/B.cB,�B+B*�B*B)�B)*B'�B'�B(�B(sB(�B'�B(sB'�B(XB%FB$�B#�B$B$ZB$@B$&B#TB#�B#nB$�B$�B&�B&LB&�B&�B'�B(�B(sB(�B(sB)�B+�B,�B.B.IB0�B1vB1vB2�B2�B3�B4TB5tB5tB5�B6zB6�B7�B8B9>B<PB?cBA�BD�BJ	BI�BI�BI�BJ�BKBJ�BLBNBR:BW$B[	B^�Ba�Bd�Bd�Bf�Bg�Bi�Bj�BkBm)Bo5Bq[Bs�Bx�B|6B|B|PB}qB~�B��B�RB�lB�XB�rB�xB�dB�jB�jB��B��B��B�B�B�B��B��B��B��B��B��B�2B�8B��B��B��B��B��B��B��BªBÖB��B��B��B��B�B�=B̳BԕBچBߊB��B��B��B�B��B��B�B��B��B��B��B��B��B��B�*B�B	 iB	tB	
XB	
XB	DB	DB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%B	%�B	'B	'8B	)DB	-CB	-]B	1[B	2|B	4TB	5tB	6zB	8�B	9�B	;�B	=�B	@�B	C�B	D�B	E�B	E�B	G�B	H�B	I�B	I�B	J�B	LB	OB	QB	UB	W?B	XEB	\CB	_VB	`\B	a-B	`BB	`BB	`\B	`\B	`BB	abB	dZB	ffB	g�B	i�B	k�B	k�B	k�B	l�B	l�B	m�B	p�B	q�B	r�B	t�B	u�B	wB	|B	~(B	� B	�B	�'B	�GB	�3B	��B	�dB	��B	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�`B	�B	�B	�B	�B	�/B	�IB	�OB	�AB	�GB	�aB	�hB	�nB	�zB	�lB	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�9B	�
B	�+B	�+B	��B	�1B	�1B	�1B	�QB	�=B	�]B	�jB	�vB	�HB	�NB	�`B	�LB	�XB	�B	�B	��B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�6B	�(B	�B
  B
  B
 B
  B
 B
B
B
AB
-B
GB
?B
_B
1B
	lB

XB
xB
~B
PB
�B
�B
�B
\B
�B
�B
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'B
(
B
(
B
($B
)B
*B
*B
+B
,"B
,B
-)B
-CB
.IB
./B
./B
/B
/B
/B
/B
/B
/B
0;B
0;B
0;B
1'B
1AB
2aB
3B
3MB
3MB
3MB
3MB
49B
4TB
4TB
4�B
6`B
7LB
6`B
7fB
7fB
7fB
7�B
88B
8RB
8lB
9>B
9XB
9XB
9XB
9XB
9XB
9rB
:^B
:xB
:xB
:�B
;dB
<�B
<�B
=qB
=�B
=�B
>wB
>�B
>wB
>�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
QB
RB
RB
R�B
R�B
R�B
R�B
R�B
SB
S�B
S�B
S�B
S�B
S�B
TB
S�B
TB
TB
S�B
T�B
T�B
UB
UB
UB
T�B
VB
VB
VB
VB
VB
VB
W$B
W$B
W$B
X+B
XB
W�B
XB
X+B
Y1B
Z7B
Z7B
ZB
Z7B
[=B
[=B
[=B
[#B
\B
\B
\CB
\B
\CB
\)B
\CB
]IB
]dB
]IB
^OB
^OB
^OB
_VB
_pB
`BB
`\B
`\B
`\B
aHB
aHB
aHB
bNB
bhB
bNB
bhB
bNB
c:B
cTB
cTB
cTB
cTB
dZB
dZB
d@B
dZB
dtB
dtB
dtB
ezB
ezB
ezB
ezB
f�B
ffB
ffB
f�B
g�B
g�B
gmB
h�B
h�B
hsB
hsB
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
mwB
m�B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004180034492020041800344920200418003449202211182142412022111821424120221118214241202004190021052020041900210520200419002105  JA  ARFMdecpA19c                                                                20200408003745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200407153752  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200407153755  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200407153755  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200407153756  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200407153756  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200407153756  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200407153756  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200407153756  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200407153756  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200407153757  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200407153758                      G�O�G�O�G�O�                JA  ARUP                                                                        20200407155335                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200407153545  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200407153504  CV  JULD            G�O�G�O�Fȁ                JM  ARCAJMQC2.0                                                                 20200417153449  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200417153449  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200418152105  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124241  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                