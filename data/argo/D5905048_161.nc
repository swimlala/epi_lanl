CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-19T00:35:12Z creation;2017-09-19T00:35:16Z conversion to V3.1;2019-12-19T07:57:11Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170919003512  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_161                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�'5��1   @�'6����@4������d��Xy=�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�	�D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�33@���@���AffA<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B�ffB���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C�3C�3C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=�fC?�fCA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�fCu�fCw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!y�D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*l�D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Ddl�Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6fD�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6fD�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�DѼ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D�3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A�wA��A���A��;A�^5A�A�A�p�A�
=A�n�A���A�bNA��`Aݩ�A�O�A�%Aܴ9A۴9Aڙ�A�ffA֣�A�ƨA�9XA��A�E�A�t�A���A���A�dZA˙�A�?}A�oA��
AŬA�5?A�ƨA�9XAöFA�VA�  A��#A�A�A��A� �A��A���A�ƨA�1'A���A��9A�hsA�M�A�ȴA�JA�  A���A���A��RA���A��A�jA�G�A���A��A��hA��9A��A��RA��A�ffA�I�A��PA��A��`A�t�A�VA�bNA��9A�
=A�n�A��-A�(�A�A�bA�O�A���A��A�ffA��A��PA�"�A��HA��A��DA��+A���A�^5A��;A� �A��A�  A�ȴA��7A�{A{dZAx�`AwC�Aw��Au��AqK�An��Ak��Ah�+Af��Ae|�Ad-Aa�
A`$�A\�AZ��AX��AV�yAT^5AS/ARv�AQ�7APZAO��AO
=AM��AJ=qAH��AGAG7LAEO�AC�FABffAA��A@bA>�A>5?A=S�A<�`A;�;A;oA9�;A7ƨA4�/A2��A2{A1�;A1l�A01A-��A,��A,�A+��A*1'A'��A&�DA%�A$��A$�uA#��A!\)A�AjA�TA��AXA�\Ap�A+AĜAK�A5?A�FA�/A�uA�9A��Az�A��A�A��AZA��A�DA1A��AC�A$�A�A-A�#A
�A	G�AĜAv�Az�A$�A|�AdZAAjA��A�Al�AVAĜA�9A�#A �j@���@��h@�%@�bN@���@���@�M�@���@��@�1'@�v�@�G�@�P@�E�@�?}@�K�@�p�@�I�@�~�@� �@�+@柾@�^5@��@�w@睲@�\)@��@�A�@��@�@�7@��`@��@�Z@�33@�?}@ڟ�@Ԭ@��@�Q�@���@�&�@�+@���@ʇ+@�ff@��@��`@ǥ�@���@�-@��@��/@�z�@���@�C�@��@�V@�V@��@�&�@�  @���@�E�@���@�(�@��@��@��h@��@��@�9X@��;@�|�@��\@��-@��@��j@���@�I�@��m@�dZ@��@�o@��@�~�@��@���@��7@�hs@��@���@�7L@�1'@���@��@��F@�l�@�S�@�S�@�C�@�o@���@��\@�v�@�M�@�@�@���@���@�@�X@��@���@���@��w@��w@�t�@��@�@���@��@��!@�M�@��@��#@���@��h@�7L@�Ĝ@��m@�K�@�
=@��+@��@���@��@�V@��j@�Z@�Q�@�(�@�ƨ@���@�dZ@�"�@��@��!@�~�@�n�@�=q@��@�/@��@�%@��`@��@�Q�@� �@���@���@��P@��@��@��@�|�@�S�@�33@�o@��H@���@�^5@�5?@�{@���@���@��@�G�@��@��`@��@�z�@�r�@�j@�I�@�  @���@�t�@�C�@�o@��y@�ȴ@���@�M�@�J@��-@�p�@�`B@�X@�G�@��@��@��u@�Q�@�b@��
@���@���@���@��P@�"�@���@�^5@�J@��@��@���@���@�hs@���@�j@�9X@���@��;@���@�K�@�C�@��@��@���@��\@�v�@�5?@��#@�x�@�/@�V@���@���@�I�@�(�@�(�@�  @��m@��;@��m@�K�@�o@��y@���@��@��R@�33@�;d@�"�@�
=@��H@��!@��\@�v�@�E�@��-@�/@���@���@�r�@�Q�@�A�@� �@��@�  @�ƨ@�t�@��!@��@��#@�hs@�O�@�?}@�%@��`@���@��D@�z�@�(�@�b@��
@�l�@�+@��H@���@���@��+@�n�@�M�@�$�@�@���@�p�@�/@��j@�A�@�  @��
@��w@���@�|�@�S�@�"�@��y@�v�@���@���@��7@�x�@�`B@�G�@�/@��@��@���@�Ĝ@��@�A�@�b@�@��@��@�w@�@~�y@}@}`B@}V@|��@|1@{t�@{C�@{"�@z~�@y��@y�^@yx�@yG�@x��@xb@w�@w�@w�@w��@w|�@v��@vE�@v$�@u@u/@t�/@t9X@s�@s@r^5@qx�@q�@p��@p��@o�@n��@n$�@m��@m��@m��@m`B@l��@l9X@k�F@ko@j��@j��@i�^@i%@h��@hr�@g��@g�P@g�@f��@f�@f��@f��@g\)@f{@e�@d��@d�@d��@d��@d(�@ct�@c33@b��@bM�@a��@a�^@a�7@a&�@`�`@a&�@a%@`�`@`�9@`r�@_�w@_�@^{@]�@\��@\�@\(�@\1@[��@[@Z~�@Z=q@Y��@XĜ@XQ�@XQ�@W\)@VE�@U@U��@U�@U?}@T�/@Tz�@T9X@T1@S�F@SdZ@S33@S33@R�@Rn�@R=q@RM�@R�@Q�@Q�^@Q&�@P��@P�`@PĜ@P�@Pr�@PQ�@PQ�@P �@Ol�@O\)@O�@N�R@N��@N�+@NV@M�T@M?}@L��@LZ@L(�@K��@K��@Ko@Jn�@J-@J�@I�@I��@IG�@I�@I%@H�`@H�9@H�9@H�9@H��@Hr�@G��@G;d@F�y@F�R@F�R@F��@F�+@FV@F@E@Ep�@E`B@D�/@Dj@D�@C�m@C�
@C��@CdZ@C33@C@B��@B�!@B��@B�\@B~�@BJ@A�7@Ahs@A7L@A&�@A%@@�`@@��@@Q�@@  @?�@?|�@?+@?�@>��@>�R@>�+@>V@>5?@>$�@>{@>@=��@=p�@=/@=V@<�@<�/@<�@<�D@<(�@<1@;�F@;t�@;@:�\@:�@9�#@9x�@9�@8�9@8�@8bN@81'@8 �@8b@7�;@7��@7;d@6�y@6��@6ff@6E�@65?@6$�@6@5�-@5p�@5/@4��@4�@4�j@4j@41@3�
@3��@3S�@2�@2��@2n�@1��@1�^@1x�@1G�@1�@1%@0��@0�9@0��@0�@0Q�@01'@/�;@/��@/\)@/�@.�y@.��@.$�@-��@-�-@-�@-�@-�@-/@-/@,��@,�j@,�@+�F@+��@+dZ@+"�@+@*��@*n�@*J@)�#@)��@)x�@)G�@(�`@(��@(�u@(1'@(  @'�;@'��@'l�@'\)@'\)@'+@&��@&�R@&�+@&5?@%�T@%�-@%�@%�@$z�@$9X@$�@#�m@#��@#C�@"�!@"M�@"�@!��@!�^@!x�@!G�@!�@ �`@ Ĝ@ ��@ �@ r�@ bN@ A�@  �@   @��@�@�P@K�@;d@��@��@5?@{@�T@��@p�@?}@�@V@��@�@�@z�@z�@Z@I�@9X@(�@(�@�@��@��@t�@S�@C�@33@o@�H@��@��@M�@�@J@��@�@��@hs@G�@�@��@��@bN@ �@  @�@��@�@��@|�@\)@K�@�@��@�@�R@v�@ff@V@E�@5?@{@�T@�-@��@�h@�@`B@�@�@�@�@�D@z�@I�@1@��@ƨ@�@S�@o@�H@�!@n�@�@J@�@�@�@��@��@G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A�wA��A���A��;A�^5A�A�A�p�A�
=A�n�A���A�bNA��`Aݩ�A�O�A�%Aܴ9A۴9Aڙ�A�ffA֣�A�ƨA�9XA��A�E�A�t�A���A���A�dZA˙�A�?}A�oA��
AŬA�5?A�ƨA�9XAöFA�VA�  A��#A�A�A��A� �A��A���A�ƨA�1'A���A��9A�hsA�M�A�ȴA�JA�  A���A���A��RA���A��A�jA�G�A���A��A��hA��9A��A��RA��A�ffA�I�A��PA��A��`A�t�A�VA�bNA��9A�
=A�n�A��-A�(�A�A�bA�O�A���A��A�ffA��A��PA�"�A��HA��A��DA��+A���A�^5A��;A� �A��A�  A�ȴA��7A�{A{dZAx�`AwC�Aw��Au��AqK�An��Ak��Ah�+Af��Ae|�Ad-Aa�
A`$�A\�AZ��AX��AV�yAT^5AS/ARv�AQ�7APZAO��AO
=AM��AJ=qAH��AGAG7LAEO�AC�FABffAA��A@bA>�A>5?A=S�A<�`A;�;A;oA9�;A7ƨA4�/A2��A2{A1�;A1l�A01A-��A,��A,�A+��A*1'A'��A&�DA%�A$��A$�uA#��A!\)A�AjA�TA��AXA�\Ap�A+AĜAK�A5?A�FA�/A�uA�9A��Az�A��A�A��AZA��A�DA1A��AC�A$�A�A-A�#A
�A	G�AĜAv�Az�A$�A|�AdZAAjA��A�Al�AVAĜA�9A�#A �j@���@��h@�%@�bN@���@���@�M�@���@��@�1'@�v�@�G�@�P@�E�@�?}@�K�@�p�@�I�@�~�@� �@�+@柾@�^5@��@�w@睲@�\)@��@�A�@��@�@�7@��`@��@�Z@�33@�?}@ڟ�@Ԭ@��@�Q�@���@�&�@�+@���@ʇ+@�ff@��@��`@ǥ�@���@�-@��@��/@�z�@���@�C�@��@�V@�V@��@�&�@�  @���@�E�@���@�(�@��@��@��h@��@��@�9X@��;@�|�@��\@��-@��@��j@���@�I�@��m@�dZ@��@�o@��@�~�@��@���@��7@�hs@��@���@�7L@�1'@���@��@��F@�l�@�S�@�S�@�C�@�o@���@��\@�v�@�M�@�@�@���@���@�@�X@��@���@���@��w@��w@�t�@��@�@���@��@��!@�M�@��@��#@���@��h@�7L@�Ĝ@��m@�K�@�
=@��+@��@���@��@�V@��j@�Z@�Q�@�(�@�ƨ@���@�dZ@�"�@��@��!@�~�@�n�@�=q@��@�/@��@�%@��`@��@�Q�@� �@���@���@��P@��@��@��@�|�@�S�@�33@�o@��H@���@�^5@�5?@�{@���@���@��@�G�@��@��`@��@�z�@�r�@�j@�I�@�  @���@�t�@�C�@�o@��y@�ȴ@���@�M�@�J@��-@�p�@�`B@�X@�G�@��@��@��u@�Q�@�b@��
@���@���@���@��P@�"�@���@�^5@�J@��@��@���@���@�hs@���@�j@�9X@���@��;@���@�K�@�C�@��@��@���@��\@�v�@�5?@��#@�x�@�/@�V@���@���@�I�@�(�@�(�@�  @��m@��;@��m@�K�@�o@��y@���@��@��R@�33@�;d@�"�@�
=@��H@��!@��\@�v�@�E�@��-@�/@���@���@�r�@�Q�@�A�@� �@��@�  @�ƨ@�t�@��!@��@��#@�hs@�O�@�?}@�%@��`@���@��D@�z�@�(�@�b@��
@�l�@�+@��H@���@���@��+@�n�@�M�@�$�@�@���@�p�@�/@��j@�A�@�  @��
@��w@���@�|�@�S�@�"�@��y@�v�@���@���@��7@�x�@�`B@�G�@�/@��@��@���@�Ĝ@��@�A�@�b@�@��@��@�w@�@~�y@}@}`B@}V@|��@|1@{t�@{C�@{"�@z~�@y��@y�^@yx�@yG�@x��@xb@w�@w�@w�@w��@w|�@v��@vE�@v$�@u@u/@t�/@t9X@s�@s@r^5@qx�@q�@p��@p��@o�@n��@n$�@m��@m��@m��@m`B@l��@l9X@k�F@ko@j��@j��@i�^@i%@h��@hr�@g��@g�P@g�@f��@f�@f��@f��@g\)@f{@e�@d��@d�@d��@d��@d(�@ct�@c33@b��@bM�@a��@a�^@a�7@a&�@`�`@a&�@a%@`�`@`�9@`r�@_�w@_�@^{@]�@\��@\�@\(�@\1@[��@[@Z~�@Z=q@Y��@XĜ@XQ�@XQ�@W\)@VE�@U@U��@U�@U?}@T�/@Tz�@T9X@T1@S�F@SdZ@S33@S33@R�@Rn�@R=q@RM�@R�@Q�@Q�^@Q&�@P��@P�`@PĜ@P�@Pr�@PQ�@PQ�@P �@Ol�@O\)@O�@N�R@N��@N�+@NV@M�T@M?}@L��@LZ@L(�@K��@K��@Ko@Jn�@J-@J�@I�@I��@IG�@I�@I%@H�`@H�9@H�9@H�9@H��@Hr�@G��@G;d@F�y@F�R@F�R@F��@F�+@FV@F@E@Ep�@E`B@D�/@Dj@D�@C�m@C�
@C��@CdZ@C33@C@B��@B�!@B��@B�\@B~�@BJ@A�7@Ahs@A7L@A&�@A%@@�`@@��@@Q�@@  @?�@?|�@?+@?�@>��@>�R@>�+@>V@>5?@>$�@>{@>@=��@=p�@=/@=V@<�@<�/@<�@<�D@<(�@<1@;�F@;t�@;@:�\@:�@9�#@9x�@9�@8�9@8�@8bN@81'@8 �@8b@7�;@7��@7;d@6�y@6��@6ff@6E�@65?@6$�@6@5�-@5p�@5/@4��@4�@4�j@4j@41@3�
@3��@3S�@2�@2��@2n�@1��@1�^@1x�@1G�@1�@1%@0��@0�9@0��@0�@0Q�@01'@/�;@/��@/\)@/�@.�y@.��@.$�@-��@-�-@-�@-�@-�@-/@-/@,��@,�j@,�@+�F@+��@+dZ@+"�@+@*��@*n�@*J@)�#@)��@)x�@)G�@(�`@(��@(�u@(1'@(  @'�;@'��@'l�@'\)@'\)@'+@&��@&�R@&�+@&5?@%�T@%�-@%�@%�@$z�@$9X@$�@#�m@#��@#C�@"�!@"M�@"�@!��@!�^@!x�@!G�@!�@ �`@ Ĝ@ ��@ �@ r�@ bN@ A�@  �@   @��@�@�P@K�@;d@��@��@5?@{@�T@��@p�@?}@�@V@��@�@�@z�@z�@Z@I�@9X@(�@(�@�@��@��@t�@S�@C�@33@o@�H@��@��@M�@�@J@��@�@��@hs@G�@�@��@��@bN@ �@  @�@��@�@��@|�@\)@K�@�@��@�@�R@v�@ff@V@E�@5?@{@�T@�-@��@�h@�@`B@�@�@�@�@�D@z�@I�@1@��@ƨ@�@S�@o@�H@�!@n�@�@J@�@�@�@��@��@G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�mB
�sB
�sB
�sB
�sB
�sB
�sB
�mB
�B	7BF�BYBQ�BC�B>wB?}B=qBC�BW
BW
BZB[#B]/B[#BS�B[#BbNBXBx�B�B�=B��B�9B�NB�B�yB�B��B�B��B��B�B"�B�B�B"�B,B-B&�B�B2-BA�BA�B;dB<jBL�BP�BI�BO�B\)Be`BaHBffBgmBe`Bz�B�JB�DB�%B|�Bn�Bn�BjB\)BD�BH�BG�B=qB9XB.B(�B,B7LB0!B,B�B��BÖB��B�BYB>wB!�B�BbB
�B
�B
�oB
{�B
ZB
O�B
T�B
?}B
�B
P�B
u�B
� B
~�B
y�B
q�B
VB
)�B
�B
+B
�B
�B	�sB	��B	�'B	�bB	�B	u�B	hsB	W
B	F�B	33B	-B	#�B	�B	\B	hB	uB	\B	1B	%B	  B��B�#B�NB�sB�sB�B��B��B��B��BƨB��BǮBƨB�}B�XB�!B��B��B��B��B��B��B�oB�+B�PB�uB�JB~�Bq�B}�Bx�B� B{�Bp�BbNBffBm�Bz�Bz�By�Bs�Bo�Bz�Bv�Bn�Bp�Bw�Bu�Bz�B~�B�B�Bw�Br�Br�BhsBcTBl�Bm�Bm�Bm�BffBe`Be`BiyBaHBhsBs�B�B��B��B��B��B��B��B��B��B�bB�PB�PB�bB��B�VB�JB�VB�hB�\B�\B�DB�JB�DB�1B�%B�B�\B��B��B��B��B��B��B��B��B��B��B��B��B�-B�-B�?B�FB�9B�LB�jB��B��B��B��B�LB�B��B�7B�DB�1B�B�B�B�bB�uB�uB�oB�hB�{B��B��B��B��B��B�B�'B�XB�dB�}BBÖBƨBȴB��B��B��B��B��B�
B�B�/B�HB�HB�`B�mB�mB�B�B�B�B��B��B��B	  B	B	1B	DB	bB	�B	�B	�B	"�B	"�B	!�B	#�B	(�B	,B	,B	/B	1'B	33B	49B	5?B	8RB	;dB	;dB	<jB	>wB	>wB	>wB	?}B	E�B	F�B	F�B	I�B	M�B	O�B	O�B	P�B	VB	W
B	W
B	W
B	XB	\)B	]/B	`BB	`BB	aHB	cTB	dZB	k�B	o�B	p�B	r�B	t�B	w�B	y�B	|�B	}�B	�B	�B	�B	�B	�%B	�+B	�=B	�=B	�PB	�VB	�VB	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�?B	�LB	�XB	�XB	�RB	�RB	�^B	�qB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�B	�B	�#B	�#B	�5B	�HB	�NB	�TB	�ZB	�ZB	�mB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
  B	��B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
B
B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
PB
PB
VB
VB
VB
PB
PB
JB
DB
DB
\B
\B
bB
bB
bB
bB
bB
bB
bB
\B
bB
hB
oB
uB
{B
{B
{B
uB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
$�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
$�B
&�B
'�B
'�B
&�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
%�B
%�B
&�B
'�B
(�B
(�B
'�B
'�B
)�B
(�B
(�B
)�B
+B
+B
+B
,B
/B
0!B
1'B
1'B
1'B
0!B
0!B
/B
/B
0!B
1'B
2-B
33B
33B
2-B
2-B
33B
33B
33B
33B
5?B
33B
1'B
33B
49B
5?B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
;dB
;dB
<jB
<jB
<jB
;dB
;dB
=qB
<jB
<jB
=qB
=qB
=qB
<jB
<jB
>wB
=qB
?}B
?}B
>wB
>wB
?}B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
S�B
S�B
S�B
T�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
XB
YB
XB
YB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
[#B
[#B
ZB
\)B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
bNB
cTB
bNB
cTB
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
ffB
ffB
ffB
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
hsB
hsB
hsB
iyB
iyB
hsB
iyB
jB
iyB
jB
jB
jB
jB
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
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
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
n�B
o�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�mB
�B
�sB
�sB
�sB
�B
�B
�B
�B	�BF�BZQBS�BEB?cB@iB>�BD�BW�BW�BZ�B[�B^B\]BVmB]�BezB]�B|�B�MB�B��B��B� B�}B�QB�IB׍B��B�HB�rB]B#�BB �B$&B,qB-�B(�B"�B3�BBABB�B=�B?�BNpBS&BL~BR:B]�BgRBc�Bh�Bi_Bg�B{�B��B�PB�B��Br�Bq�BmB`'BIBJrBI�B@ B;JB0;B*B-CB8lB1�B-�B�B�BɆB��B�	B]�BB�B%zB vBuB
��B
��B
��B
~�B
]dB
R�B
X+B
C�B
$ZB
O�B
u?B
�4B
}B
z�B
sMB
ZB
/5B
_B
	RB
]B
B	�wB	�B	�B	��B	�aB	w�B	j�B	ZB	IlB	7LB	/�B	&�B	/B	TB	�B	�B	�B		�B	EB	UB��B�!B�ZB�B�BڠB�
BԯB�[B��B�KB͟B��BǔB�B��B�-B�B�-B�B��B�nB��B��B��B��B�,B��B��Bt�B�Bz�B��B|�Br|Be`BhsBoiB{�B{Bz�BuBqB{Bw�Bp�BrBx�Bv�B{JBB�oB��By�BtBs�BjKBe,BmCBncBncBn}Bh$Bf�Bf�BjBc�Bi�Bt�B�oB��B�IB��B�ZB��B��B��B��B�:B�B��B� B��B�B�B�BB��B��B��B�0B��B��B��B�B��B�}B��B��B��B�;B�B��B��B�B�QB��B��B�yB�GB��B��B�fB��B�lB�B�B� B��B� B��B��B�B��B��B��B�gB��B�GB��B��B��B�B�:B��B�IB�pB�ZB��B�_B��B��B��B��B��B�B�gBǔBɆBΊB��BЗB��BөB׍BںB�~B�B��B��B�>B�$B�B��B��B�B�+B�RB�<B	 4B	�B	�B	�B	�B	�B	�B	�B	#B	#TB	"�B	$@B	)B	,=B	,WB	/OB	1[B	3hB	4�B	5�B	8�B	;�B	;�B	<�B	>�B	>�B	>�B	?�B	FB	GB	G+B	J#B	N"B	PB	PHB	QNB	VB	W?B	WYB	WYB	XyB	\xB	]~B	`vB	`�B	a�B	c�B	eB	lB	pB	q'B	sB	uB	x8B	zDB	}<B	~]B	�;B	�UB	�oB	�SB	�tB	�zB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�4B	��B	�B	��B	�B	�B	��B	�,B	�B	�2B	�8B	�eB	�=B	�CB	�]B	�OB	�UB	�vB	�|B	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�"B	�"B	�<B	�BB	�4B	�4B	�:B	�@B	�B	�,B	�&B	�TB	�aB	�gB	�EB	�WB	�WB	�kB	�kB	�qB	ۦB	ޞB	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	��B	��B	��B	�	B	�	B	��B	�B	�(B	�.B	�.B	�HB	�HB	�HB	�.B	�wB	�cB
 OB
UB
'B
AB
GB
GB
GB
AB
UB
 iB	��B
�B
aB
�B
SB
SB
mB
SB
SB
mB
YB
mB
SB
mB
aB
mB
mB
_B
	RB
	lB
	lB
	lB
	lB
	RB
	�B
	�B

�B

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B
�B
�B
�B
�B
�B
B
B
B
B
B
 �B
 �B
 �B
 BB
 'B
# B
$B
$�B
#�B
$B
$@B
$@B
%,B
&2B
'B
'B
%zB
'8B
(>B
(XB
'8B
&B
&2B
'B
'B
'B
(
B
(
B
&fB
&2B
'8B
(
B
)B
)*B
(>B
(XB
*0B
)_B
)DB
*0B
+6B
+6B
+6B
,=B
/5B
0UB
1[B
1[B
1vB
0�B
0�B
/�B
/�B
0oB
1vB
2GB
3hB
3�B
2|B
2|B
3hB
3�B
3�B
3�B
5tB
3�B
1�B
3�B
4nB
5tB
4nB
4nB
4nB
5tB
5tB
6�B
6zB
6zB
7fB
7�B
7�B
8�B
:xB
:�B
:�B
:�B
:�B
;�B
<jB
;�B
;B
<�B
<�B
<�B
;�B
;�B
=�B
<�B
<�B
=�B
=�B
=�B
<�B
<�B
>�B
=�B
?�B
?�B
>�B
>�B
?�B
@�B
A�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
A�B
@�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
D�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
IB
IB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
MB
MB
MB
MB
MB
M�B
M�B
MB
MB
MB
N"B
NB
M�B
NB
NB
NB
M�B
OB
NB
NB
NB
N"B
OBB
PB
P.B
P.B
Q4B
R:B
RB
R B
RB
R B
R B
R B
R:B
R B
S&B
S&B
TB
TB
T,B
T,B
S&B
T,B
TFB
T,B
UB
T,B
T,B
TFB
UMB
UMB
U2B
U2B
V9B
VSB
VSB
W$B
W?B
XEB
XEB
XEB
X_B
YKB
YKB
YKB
X+B
YKB
XEB
YKB
YKB
YKB
Y1B
YKB
YeB
ZQB
[WB
[=B
[=B
\CB
\CB
\]B
[qB
[=B
ZkB
\]B
]dB
]dB
\]B
]IB
]~B
]dB
]dB
^jB
^jB
^jB
^jB
^jB
_�B
_pB
_pB
`vB
`vB
`vB
`vB
abB
abB
a|B
`�B
`\B
a|B
a|B
a|B
a|B
a|B
a�B
a|B
bhB
c�B
c�B
b�B
c�B
b�B
c�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
h�B
i�B
j�B
i�B
j�B
j�B
j�B
j�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
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
n�B
o�B
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
s�B
s�B
s�B
s�B
s�B
s�B
tB
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709250058552017092500585520170925005855201806221319222018062213192220180622131922201804050721512018040507215120180405072151  JA  ARFMdecpA19c                                                                20170919093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170919003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170919003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170919003515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170919003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170919003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170919003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170919003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170919003516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170919003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20170919005627                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170924155701  CV  JULD            G�O�G�O�F�9�                JM  ARCAJMQC2.0                                                                 20170924155855  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170924155855  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222151  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041922  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                