CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:23:55Z creation;2022-06-04T17:23:55Z conversion to V3.1      
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pD   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172355  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ظ]}X^1   @ظ]��v�@-bM���d)O�;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @y��@�33@���A!��A>ffAc33A~ffA�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B���B�  B�33B�  B���B�  B�  B�  B�33B�  B�33B�  B�33B�33B�  B�  B�  B�ffB�33B�  B�  B���C  C  C  C  C
  C  C  C  C  C�CffCffC�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>��C?ffCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̼�D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @33@l��@���@�fgAfgA;33A`  A{33A�ffA�ffA���A�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B�  B���B�34B���B���B���B�fgB���BÙ�BǙ�B���Bϙ�B���Bؙ�B���B���B㙚B癚B왚B�  B���B���B���B�fgC��C��C��C��C	��C��C��C��C��C�gC33C33C�3C�3C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C>fgC?33CA�3CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce�gCg�gCi��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٙC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0��D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTy�DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D��gD�9�D�y�D���D��gD�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̶gD���D�9�D�y�D͹�D���D�6gD�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D��D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѷLAѿ�A�ƨA��3A���A�ȴA�˒A��#A���A��A�ΥA�ϫA�бA�� A�ҽA�ӏA��,A��&A�҉A���A�҉A�ȀAѶzAѥzAѕ�Aъ	A�33A��]A�ΥA���Aж�AП�A�~�A�x8A�p;A�jA�_�A�Z�A�W
A�T�A�LdA�=�A�(�A��A��A���Aϐ�AΙ1A��A�i�A��A��ZA���A�OvA���A��A�33A�v+A�xA�6�A��eA�e,A�<�A��KA��A���A���A�$�A��A��A�r�A���A��A�{A�A���A���A���A���A��A�c�A�=A���A���A�<A��QA��A��}A�ȴA���A|N�Ax��Av6�Ar!�AhA Ai	A^�A\ݘAZ��ATb�AQ�AQ9XAO��AK?�AH�AFMAD%FA@�sA;��A9MA6��A4҉A3��A2��A1bA0$A.�A/FtA/��A/�'A.|�A.bNA.L�A.g�A.iDA.Q�A.!�A,�vA+�A*`BA)�A(
�A'o A&�!A&.�A&!-A&B�A&4A$��A#�)A#�A#�uA#!-A"��A!�A!�YA!QA!�A qA &A��A�!A�A��AD�A�ZA�AB�A�7A�A��A�FAg�A��AqvA-wA�Al�A�CAW?A�A�A��AxAQA�cAzA \A��Au�A:�A%A�WAȴA��AjA�A��A�SAYAJ�A��A@OA��A?A֡A�PAj�A?�A��AMjA��A��Aj�A1�A��A��A�A
V�A	��A	O�A	*�A	�A�Aw�Ay>Ap�A"hA��A1'A�KAخA��A^�A�A�fA2�A��A��A?A�A�;Af�A,�AAs�A��A�A��A ��A ��A [WA IRA �@�8�@�}V@���@�|@�u�@�Q�@���@�a|@��]@�x�@��@���@��@�'�@�$t@��;@�N<@�9@���@��@��@�B�@�@�.@�E�@�E�@���@���@�($@�[@��@�[W@��8@�n�@�F@�v`@��p@���@�@�{@�33@��E@��@�RT@�P�@�8�@�=q@�6z@ⅈ@�A@�>B@�C�@��|@���@߰�@�m]@�;@޾�@ޡb@މ�@�~(@�	@ݥ�@�w2@���@��@���@��2@�;�@���@�J#@��@���@��X@�ȴ@��'@تe@��@�F@�s�@�D�@��@�?}@���@�a|@��@Ӽ�@��|@�YK@���@�~�@�Y�@�)_@��@Л�@��a@�q@μj@�>B@���@�|@��@��D@�C�@�Xy@�x@�j�@���@�L0@��@ǔ�@�b�@��@���@ƞ�@�u�@�K^@�3�@��@�u@��@��d@ńM@�L�@���@�q@�~@ÖS@���@�~(@��@�/�@�S@���@��@���@�;d@�o@��@��X@���@�V@�.�@��)@��X@�]�@�8@��@���@��@�@���@�H�@��@��@��f@�bN@�Ft@�;�@�4n@��@�b�@� \@��@�U2@���@��@���@�w�@�kQ@�Ov@��&@�6z@��@���@��D@�PH@�'R@��[@��c@��@�j@�N<@�4�@���@�0U@��Q@��@��$@��~@���@���@�;�@���@�@O@��5@�h
@��@���@�F@��@���@��@�|�@�dZ@�RT@�A�@�1�@�$t@��@�}V@���@�\�@��@��r@�&�@���@�m]@�a�@�;d@�'�@��@���@��	@��c@���@��Y@�N�@�,=@��@��@��@��+@���@�x�@��@��@�e�@��@��K@��U@�i�@�1'@�@���@��@��@�n�@��@��K@��@�4@��h@�4�@��@��E@��j@���@�e�@�)�@��+@���@�@�B[@��Q@�+�@�Y@� i@���@��!@�z�@�L0@�9X@�*�@��@�@���@���@���@�|@�l�@�X�@�S@��j@�i�@�!@�  @��@��@��C@�iD@��@���@�h
@�*�@���@���@�A @��E@��A@��@���@���@��@���@�q�@�-�@��X@�5�@�.I@�)_@��@�%@���@��\@�h�@�K^@�#:@� �@���@��o@��@��'@�p�@�IR@�<6@��@��5@��F@�M@�$@��@��@��W@��V@�t�@��@�q@�G@���@�?}@���@��@��@�IR@��8@��x@�YK@�8�@�($@��@��)@���@�F@��5@�u�@�V�@�*�@�@~YK@}��@}�@}��@}��@}�@|�9@|1@{��@{�@z�s@zZ�@y�n@y2a@xɆ@x�@w��@wH�@w
=@v��@v1�@um]@t�O@t4n@s�W@s�6@s��@sv`@r}V@q��@q��@qDg@p��@p7@o�@nkQ@m��@l��@l�`@l�)@l1'@kv`@kJ#@k33@js�@j8�@iԕ@i:�@hq@h@g��@g�@@g��@fߤ@fW�@e��@e��@e�@ezx@d�[@ca@b�s@b��@bkQ@a�o@a��@af�@a�@`��@`%�@_W?@_�@^��@^q�@]��@]7L@];@\��@[�+@Z�"@Z�c@Z�@Z��@Zȴ@Zd�@Z=q@Z&�@Y��@YA @X�j@X'R@WH�@V��@V�R@Vz@V^5@VTa@VJ@Vu@U�T@U+�@TS�@S��@S_p@S33@R�c@RkQ@Q�@Qu�@P�@PPH@O�;@O��@O1�@N�s@N�L@N�\@Ni�@M�T@M��@Mu�@M�@Lѷ@L�O@LPH@K��@K�0@K9�@Jp;@I�t@I@H-�@G�r@G��@G�@F��@E�3@E��@EB�@D�4@D�@Cy�@C�@B҉@B�@B�@A�d@A��@Azx@A \@@��@@�$@@9X@?��@?x@>҉@=��@=�-@=��@=+@<�4@<Xy@<9X@</�@;�K@;x@;l�@;a@;X�@;+@:�m@:($@9�D@9�9@9u�@9	l@8~(@8/�@7��@7�@7��@6��@6:*@6 �@5��@5��@5k�@5�@4��@47@3ƨ@3�*@3Z�@3�@2��@2!�@1�@1�"@1Y�@1�@0�E@0�$@0�@0��@0`�@04n@0�@0G@/�+@/�@/�0@/�*@/j�@/C�@/�@.�]@.��@.Z�@.#:@-��@-\�@,��@,��@,~@+��@+��@+o@*��@*{�@*?@)��@)�@)�@(��@(�Y@(]d@(@'�0@'dZ@'�@&�M@&��@&c @%��@%s�@%*0@$��@$�v@$��@$�@$oi@$g8@$*�@#�]@#�m@#�@#�{@#F�@"�M@"��@"�@"~�@"Ov@"8�@!�@!�@!��@!7L@!�@ �K@ tT@�@ݘ@��@A�@�@�@V@��@�@��@Q�@-�@ �@ݘ@��@33@�@�2@�<@�b@J�@@�N@j@��@��@��@l"@l"@w�@g8@K^@:�@�@˒@��@��@y�@W?@'�@�@��@��@�@kQ@O@�Z@�@��@k�@\�@Vm@�f@~(@N�@!@�;@�@O@1�@"�@�@�@��@�@�B@��@��@��@�+@R�@8�@
�@ϫ@�-@x�@+@�/@��@z�@Ft@	�@�@�Q@�{@iD@P�@�@S@�@�@��@�@�L@�\@��@s�@C�@4@��@��@��@s�@N<@/@�E@�O@��@�D@bN@$@�;@�@��@��@�{@a@!-@
��@
�2@
�@
�!@
�+@
p;@
n�@
YK@
.�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AѷLAѿ�A�ƨA��3A���A�ȴA�˒A��#A���A��A�ΥA�ϫA�бA�� A�ҽA�ӏA��,A��&A�҉A���A�҉A�ȀAѶzAѥzAѕ�Aъ	A�33A��]A�ΥA���Aж�AП�A�~�A�x8A�p;A�jA�_�A�Z�A�W
A�T�A�LdA�=�A�(�A��A��A���Aϐ�AΙ1A��A�i�A��A��ZA���A�OvA���A��A�33A�v+A�xA�6�A��eA�e,A�<�A��KA��A���A���A�$�A��A��A�r�A���A��A�{A�A���A���A���A���A��A�c�A�=A���A���A�<A��QA��A��}A�ȴA���A|N�Ax��Av6�Ar!�AhA Ai	A^�A\ݘAZ��ATb�AQ�AQ9XAO��AK?�AH�AFMAD%FA@�sA;��A9MA6��A4҉A3��A2��A1bA0$A.�A/FtA/��A/�'A.|�A.bNA.L�A.g�A.iDA.Q�A.!�A,�vA+�A*`BA)�A(
�A'o A&�!A&.�A&!-A&B�A&4A$��A#�)A#�A#�uA#!-A"��A!�A!�YA!QA!�A qA &A��A�!A�A��AD�A�ZA�AB�A�7A�A��A�FAg�A��AqvA-wA�Al�A�CAW?A�A�A��AxAQA�cAzA \A��Au�A:�A%A�WAȴA��AjA�A��A�SAYAJ�A��A@OA��A?A֡A�PAj�A?�A��AMjA��A��Aj�A1�A��A��A�A
V�A	��A	O�A	*�A	�A�Aw�Ay>Ap�A"hA��A1'A�KAخA��A^�A�A�fA2�A��A��A?A�A�;Af�A,�AAs�A��A�A��A ��A ��A [WA IRA �@�8�@�}V@���@�|@�u�@�Q�@���@�a|@��]@�x�@��@���@��@�'�@�$t@��;@�N<@�9@���@��@��@�B�@�@�.@�E�@�E�@���@���@�($@�[@��@�[W@��8@�n�@�F@�v`@��p@���@�@�{@�33@��E@��@�RT@�P�@�8�@�=q@�6z@ⅈ@�A@�>B@�C�@��|@���@߰�@�m]@�;@޾�@ޡb@މ�@�~(@�	@ݥ�@�w2@���@��@���@��2@�;�@���@�J#@��@���@��X@�ȴ@��'@تe@��@�F@�s�@�D�@��@�?}@���@�a|@��@Ӽ�@��|@�YK@���@�~�@�Y�@�)_@��@Л�@��a@�q@μj@�>B@���@�|@��@��D@�C�@�Xy@�x@�j�@���@�L0@��@ǔ�@�b�@��@���@ƞ�@�u�@�K^@�3�@��@�u@��@��d@ńM@�L�@���@�q@�~@ÖS@���@�~(@��@�/�@�S@���@��@���@�;d@�o@��@��X@���@�V@�.�@��)@��X@�]�@�8@��@���@��@�@���@�H�@��@��@��f@�bN@�Ft@�;�@�4n@��@�b�@� \@��@�U2@���@��@���@�w�@�kQ@�Ov@��&@�6z@��@���@��D@�PH@�'R@��[@��c@��@�j@�N<@�4�@���@�0U@��Q@��@��$@��~@���@���@�;�@���@�@O@��5@�h
@��@���@�F@��@���@��@�|�@�dZ@�RT@�A�@�1�@�$t@��@�}V@���@�\�@��@��r@�&�@���@�m]@�a�@�;d@�'�@��@���@��	@��c@���@��Y@�N�@�,=@��@��@��@��+@���@�x�@��@��@�e�@��@��K@��U@�i�@�1'@�@���@��@��@�n�@��@��K@��@�4@��h@�4�@��@��E@��j@���@�e�@�)�@��+@���@�@�B[@��Q@�+�@�Y@� i@���@��!@�z�@�L0@�9X@�*�@��@�@���@���@���@�|@�l�@�X�@�S@��j@�i�@�!@�  @��@��@��C@�iD@��@���@�h
@�*�@���@���@�A @��E@��A@��@���@���@��@���@�q�@�-�@��X@�5�@�.I@�)_@��@�%@���@��\@�h�@�K^@�#:@� �@���@��o@��@��'@�p�@�IR@�<6@��@��5@��F@�M@�$@��@��@��W@��V@�t�@��@�q@�G@���@�?}@���@��@��@�IR@��8@��x@�YK@�8�@�($@��@��)@���@�F@��5@�u�@�V�@�*�@�@~YK@}��@}�@}��@}��@}�@|�9@|1@{��@{�@z�s@zZ�@y�n@y2a@xɆ@x�@w��@wH�@w
=@v��@v1�@um]@t�O@t4n@s�W@s�6@s��@sv`@r}V@q��@q��@qDg@p��@p7@o�@nkQ@m��@l��@l�`@l�)@l1'@kv`@kJ#@k33@js�@j8�@iԕ@i:�@hq@h@g��@g�@@g��@fߤ@fW�@e��@e��@e�@ezx@d�[@ca@b�s@b��@bkQ@a�o@a��@af�@a�@`��@`%�@_W?@_�@^��@^q�@]��@]7L@];@\��@[�+@Z�"@Z�c@Z�@Z��@Zȴ@Zd�@Z=q@Z&�@Y��@YA @X�j@X'R@WH�@V��@V�R@Vz@V^5@VTa@VJ@Vu@U�T@U+�@TS�@S��@S_p@S33@R�c@RkQ@Q�@Qu�@P�@PPH@O�;@O��@O1�@N�s@N�L@N�\@Ni�@M�T@M��@Mu�@M�@Lѷ@L�O@LPH@K��@K�0@K9�@Jp;@I�t@I@H-�@G�r@G��@G�@F��@E�3@E��@EB�@D�4@D�@Cy�@C�@B҉@B�@B�@A�d@A��@Azx@A \@@��@@�$@@9X@?��@?x@>҉@=��@=�-@=��@=+@<�4@<Xy@<9X@</�@;�K@;x@;l�@;a@;X�@;+@:�m@:($@9�D@9�9@9u�@9	l@8~(@8/�@7��@7�@7��@6��@6:*@6 �@5��@5��@5k�@5�@4��@47@3ƨ@3�*@3Z�@3�@2��@2!�@1�@1�"@1Y�@1�@0�E@0�$@0�@0��@0`�@04n@0�@0G@/�+@/�@/�0@/�*@/j�@/C�@/�@.�]@.��@.Z�@.#:@-��@-\�@,��@,��@,~@+��@+��@+o@*��@*{�@*?@)��@)�@)�@(��@(�Y@(]d@(@'�0@'dZ@'�@&�M@&��@&c @%��@%s�@%*0@$��@$�v@$��@$�@$oi@$g8@$*�@#�]@#�m@#�@#�{@#F�@"�M@"��@"�@"~�@"Ov@"8�@!�@!�@!��@!7L@!�@ �K@ tT@�@ݘ@��@A�@�@�@V@��@�@��@Q�@-�@ �@ݘ@��@33@�@�2@�<@�b@J�@@�N@j@��@��@��@l"@l"@w�@g8@K^@:�@�@˒@��@��@y�@W?@'�@�@��@��@�@kQ@O@�Z@�@��@k�@\�@Vm@�f@~(@N�@!@�;@�@O@1�@"�@�@�@��@�@�B@��@��@��@�+@R�@8�@
�@ϫ@�-@x�@+@�/@��@z�@Ft@	�@�@�Q@�{@iD@P�@�@S@�@�@��@�@�L@�\@��@s�@C�@4@��@��@��@s�@N<@/@�E@�O@��@�D@bN@$@�;@�@��@��@�{@a@!-@
��@
�2@
�@
�!@
�+@
p;@
n�@
YK@
.�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B9>B9	B8�B9	B9	B8�B8�B8�B8�B8�B8�B8�B9	B9	B9	B9	B9>B9rB9�B9�B9�B:�B<6B=B=�B=�BA�BB�BB�BBuBB[BBuBB[BB'BB'BB'BBBB'BB'BBABB[BB�BB�BB�BC-BC-BABH�B|jB�'B	x�B	�KB	��B	B)_BeBB�B�B�B)B#�B'B*�B)DB~B�B�B{B	�B�TB��B��Bf�B,qB
�B
�AB
��B
��B
��B
�2B
�ZB
�B
� B
��B
�oB
xRB
]�B
H�B
�B	�|B	ևB	��B	��B	t�B	~�B	U�B	UgB	W$B	Y�B	Z�B	[�B	VmB	I�B	;0B	1�B	(
B		B	
�B	dB	dB	�B	~B	�B	"�B	$tB	;dB	RTB	^�B	eB	{�B	~�B	�VB	��B	�dB	��B	�:B	��B	�	B	�PB	�~B	ɠB	�_B	��B	ʦB	�?B	�ZB	�B	�nB	��B
'B
�B
oB
EB
B
�B
"�B
'mB
-]B
0UB
4�B
6FB
6FB
6`B
:*B
<�B
=�B
>BB
AB
>wB
?�B
A�B
C�B
EB
D3B
D3B
DMB
DB
B�B
BB
B�B
AB
AB
AoB
AB
A�B
BuB
C{B
D�B
F?B
FtB
FtB
F�B
GEB
G�B
H�B
I�B
J#B
J�B
J�B
K�B
L0B
L�B
LJB
L�B
KxB
K�B
KDB
JrB
I�B
H�B
E�B
C{B
A�B
BB
B[B
A�B
?cB
>�B
?HB
>�B
>�B
>�B
>�B
?.B
@4B
@�B
AUB
@�B
A B
?�B
?�B
?�B
>�B
>�B
>�B
>�B
>wB
=�B
=<B
<6B
;0B
9�B
9>B
8lB
8RB
7�B
6�B
6�B
6�B
5%B
4�B
4TB
49B
3�B
33B
2-B
1�B
1�B
/�B
.�B
.cB
-�B
-CB
,�B
,WB
,B
*eB
&2B
!�B
 \B
�B
OB
�B
B
=B
�B
7B
�B
kB
�B
_B
�B
$B
?B
YB
�B
�B
B
@B
:B
�B
�B
�B
�B
NB
�B
�B
�B
�B
BB
�B
�B
B
�B
�B
�B
�B
dB
0B
�B
xB
DB
B

�B

�B

	B
	�B
	B
B
�B
�B
EB
�B
�B
�B
_B
_B
EB
EB
+B
�B
B
�B
YB
YB
%B
�B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
tB
�B

=B
	�B
	RB
KB
�B
�B
�B
�B
�B
�B
B
SB
mB
SB
B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
B
9B
�B
�B
B
YB
%B
?B
%B
�B
�B
�B
�B
�B
B
EB
zB
1B
1B
zB
+B
+B
B
tB
YB
�B
zB
B
�B
�B

�B
�B
�B
^B
�B
0B
�B
JB
B
�B
�B
�B
�B
�B
�B
�B
dB
�B
xB
�B
B
0B
�B
�B
B
�B
xB
B
xB
B
0B
JB
JB
0B
�B
B
�B
�B
�B
B
VB
�B
B
\B
\B
\B
.B
�B
�B
�B
�B
�B
�B
�B
B
4B
�B
 B
:B
oB
oB
�B
�B
B
@B
uB
�B
�B
�B
FB
�B
gB
gB
�B
�B
gB
�B
�B
SB
mB
�B
?B
?B
B
_B
1B
�B
�B
B
1B
�B
�B
�B
�B
�B
�B
#B
qB
qB
#B
	B
�B
�B
�B
�B
�B
WB
�B
]B
5B
5B
OB
�B
�B
!B
!B
B
;B
B
VB
 B
 'B
 BB
 �B
!B
!bB
!|B
!bB
"�B
#�B
#�B
$B
$ZB
$�B
$�B
%FB
%B
%�B
%,B
%zB
%�B
&�B
'B
'mB
'�B
'�B
'�B
)DB
)_B
)*B
)�B
*B
*eB
*B
*eB
*�B
*B
*KB
+QB
+B
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,B
,"B
,B
,qB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
./B
.IB
.�B
.�B
-�B
-B
-B
-]B
.IB
/5B
/�B
/�B
/�B
0B
0B
0UB
0;B
/�B
.�B
/5B
/5B
/B
/�B
0B
0B
1�B
3hB
3B
2�B
2|B
1�B
1vB
1'B
1'B
1B
1[B
1vB
2|B
3MB
49B
4TB
4nB
5?B
5tB
5%B
4�B
4�B
4�B
4nB
4TB
5B
5tB
5�B
6B
6�B
88B
8�B
8�B
9>B
9�B
9�B
:^B
:�B
:^B
:^B
:�B
;dB
;0B
;0B
;JB
;�B
<B
<B
<B
<jB
=B
=<B
=qB
=�B
=�B
=qB
>]B
?.B
?HB
?cB
?}B
?}B
?�B
?�B
@OB
@4B
@iB
@�B
@�B
AUB
AoB
A�B
A�B
BB
BAB
B�B
C�B
C�B
C�B
C�B
C�B
DMB
DB
C�B
D�B
D�B
EB
E�B
FYB
F?B
F�B
F�B
GB
GB
G�B
G�B
G�B
G�B
G�B
G�B
HfB
HfB
H�B
IB
IRB
I�B
I�B
J#B
J�B
J�B
K)B
KxB
KxB
K�B
K�B
LJB
LdB
L�B
L�B
MB
MB
MPB
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O(B
O�B
O�B
P.B
PbB
QB
QhB
Q�B
Q�B
Q�B
Q�B
R B
R:B
RTB
RoB
R�B
R�B
SB
SuB
S�B
S�B
TB
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
VB
V9B
V9B
V9B
V9B
VSB
V�B
W?B
WYB
WYB
W�B
W�B
XEB
XyB
XyB
X�B
X�B
YeB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]~B
]~B
]�B
]�B
^B
^B
^OB
^jB
^�B
^�B
^�B
_!B
_!B
_VB
_;B
_;B
_�B
_�B
`B
`\B
`�B
`�B
aHB
aHB
a|B
a�B
bhB
b�B
cB
c:B
c�B
c�B
d&B
d&B
dZB
dZB
eB
e`B
ezB
e�B
fLB
fLB
fLB
fLB
f�B
f�B
gB
g�B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
j�B
kB
kB
kQB
kQB
k�B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
m]B
m]B
m)B
m]B
m�B
m�B
nIB
n�B
n�B
n�B
n�B
o B
o5B
oOB
o�B
pB
pB
p!B
p!B
p!B
p�B
p�B
p�B
qAB
qAB
q�B
r-B
raB
r|B
r|B
raB
rGB
r|B
r|B
r�B
r|B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tTB
uB
u?B
utB
u�B
utB
u�B
vFB
vzB
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{JB
{dB
{�B
{�B
|B
|6B
|PB
|PB
|jB
|jB
|�B
}B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~]B
~]B
~(B
~�B
~�B
~�B
~�B
~�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B9>B9	B8�B9	B9	B8�B8�B8�B8�B8�B8�B8�B9	B9	B9	B9	B9>B9rB9�B9�B9�B:�B<6B=B=�B=�BA�BB�BB�BBuBB[BBuBB[BB'BB'BB'BBBB'BB'BBABB[BB�BB�BB�BC-BC-BABH�B|jB�'B	x�B	�KB	��B	B)_BeBB�B�B�B)B#�B'B*�B)DB~B�B�B{B	�B�TB��B��Bf�B,qB
�B
�AB
��B
��B
��B
�2B
�ZB
�B
� B
��B
�oB
xRB
]�B
H�B
�B	�|B	ևB	��B	��B	t�B	~�B	U�B	UgB	W$B	Y�B	Z�B	[�B	VmB	I�B	;0B	1�B	(
B		B	
�B	dB	dB	�B	~B	�B	"�B	$tB	;dB	RTB	^�B	eB	{�B	~�B	�VB	��B	�dB	��B	�:B	��B	�	B	�PB	�~B	ɠB	�_B	��B	ʦB	�?B	�ZB	�B	�nB	��B
'B
�B
oB
EB
B
�B
"�B
'mB
-]B
0UB
4�B
6FB
6FB
6`B
:*B
<�B
=�B
>BB
AB
>wB
?�B
A�B
C�B
EB
D3B
D3B
DMB
DB
B�B
BB
B�B
AB
AB
AoB
AB
A�B
BuB
C{B
D�B
F?B
FtB
FtB
F�B
GEB
G�B
H�B
I�B
J#B
J�B
J�B
K�B
L0B
L�B
LJB
L�B
KxB
K�B
KDB
JrB
I�B
H�B
E�B
C{B
A�B
BB
B[B
A�B
?cB
>�B
?HB
>�B
>�B
>�B
>�B
?.B
@4B
@�B
AUB
@�B
A B
?�B
?�B
?�B
>�B
>�B
>�B
>�B
>wB
=�B
=<B
<6B
;0B
9�B
9>B
8lB
8RB
7�B
6�B
6�B
6�B
5%B
4�B
4TB
49B
3�B
33B
2-B
1�B
1�B
/�B
.�B
.cB
-�B
-CB
,�B
,WB
,B
*eB
&2B
!�B
 \B
�B
OB
�B
B
=B
�B
7B
�B
kB
�B
_B
�B
$B
?B
YB
�B
�B
B
@B
:B
�B
�B
�B
�B
NB
�B
�B
�B
�B
BB
�B
�B
B
�B
�B
�B
�B
dB
0B
�B
xB
DB
B

�B

�B

	B
	�B
	B
B
�B
�B
EB
�B
�B
�B
_B
_B
EB
EB
+B
�B
B
�B
YB
YB
%B
�B
%B
�B
�B
�B
�B
�B
�B
�B
�B
�B
9B
�B
tB
�B

=B
	�B
	RB
KB
�B
�B
�B
�B
�B
�B
B
SB
mB
SB
B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
B
B
9B
�B
�B
B
YB
%B
?B
%B
�B
�B
�B
�B
�B
B
EB
zB
1B
1B
zB
+B
+B
B
tB
YB
�B
zB
B
�B
�B

�B
�B
�B
^B
�B
0B
�B
JB
B
�B
�B
�B
�B
�B
�B
�B
dB
�B
xB
�B
B
0B
�B
�B
B
�B
xB
B
xB
B
0B
JB
JB
0B
�B
B
�B
�B
�B
B
VB
�B
B
\B
\B
\B
.B
�B
�B
�B
�B
�B
�B
�B
B
4B
�B
 B
:B
oB
oB
�B
�B
B
@B
uB
�B
�B
�B
FB
�B
gB
gB
�B
�B
gB
�B
�B
SB
mB
�B
?B
?B
B
_B
1B
�B
�B
B
1B
�B
�B
�B
�B
�B
�B
#B
qB
qB
#B
	B
�B
�B
�B
�B
�B
WB
�B
]B
5B
5B
OB
�B
�B
!B
!B
B
;B
B
VB
 B
 'B
 BB
 �B
!B
!bB
!|B
!bB
"�B
#�B
#�B
$B
$ZB
$�B
$�B
%FB
%B
%�B
%,B
%zB
%�B
&�B
'B
'mB
'�B
'�B
'�B
)DB
)_B
)*B
)�B
*B
*eB
*B
*eB
*�B
*B
*KB
+QB
+B
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,B
,"B
,B
,qB
,WB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
./B
.IB
.�B
.�B
-�B
-B
-B
-]B
.IB
/5B
/�B
/�B
/�B
0B
0B
0UB
0;B
/�B
.�B
/5B
/5B
/B
/�B
0B
0B
1�B
3hB
3B
2�B
2|B
1�B
1vB
1'B
1'B
1B
1[B
1vB
2|B
3MB
49B
4TB
4nB
5?B
5tB
5%B
4�B
4�B
4�B
4nB
4TB
5B
5tB
5�B
6B
6�B
88B
8�B
8�B
9>B
9�B
9�B
:^B
:�B
:^B
:^B
:�B
;dB
;0B
;0B
;JB
;�B
<B
<B
<B
<jB
=B
=<B
=qB
=�B
=�B
=qB
>]B
?.B
?HB
?cB
?}B
?}B
?�B
?�B
@OB
@4B
@iB
@�B
@�B
AUB
AoB
A�B
A�B
BB
BAB
B�B
C�B
C�B
C�B
C�B
C�B
DMB
DB
C�B
D�B
D�B
EB
E�B
FYB
F?B
F�B
F�B
GB
GB
G�B
G�B
G�B
G�B
G�B
G�B
HfB
HfB
H�B
IB
IRB
I�B
I�B
J#B
J�B
J�B
K)B
KxB
KxB
K�B
K�B
LJB
LdB
L�B
L�B
MB
MB
MPB
M�B
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
O(B
O�B
O�B
P.B
PbB
QB
QhB
Q�B
Q�B
Q�B
Q�B
R B
R:B
RTB
RoB
R�B
R�B
SB
SuB
S�B
S�B
TB
T�B
T�B
T�B
U2B
U�B
U�B
U�B
U�B
VB
V9B
V9B
V9B
V9B
VSB
V�B
W?B
WYB
WYB
W�B
W�B
XEB
XyB
XyB
X�B
X�B
YeB
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
]~B
]~B
]�B
]�B
^B
^B
^OB
^jB
^�B
^�B
^�B
_!B
_!B
_VB
_;B
_;B
_�B
_�B
`B
`\B
`�B
`�B
aHB
aHB
a|B
a�B
bhB
b�B
cB
c:B
c�B
c�B
d&B
d&B
dZB
dZB
eB
e`B
ezB
e�B
fLB
fLB
fLB
fLB
f�B
f�B
gB
g�B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
j�B
kB
kB
kQB
kQB
k�B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
m]B
m]B
m)B
m]B
m�B
m�B
nIB
n�B
n�B
n�B
n�B
o B
o5B
oOB
o�B
pB
pB
p!B
p!B
p!B
p�B
p�B
p�B
qAB
qAB
q�B
r-B
raB
r|B
r|B
raB
rGB
r|B
r|B
r�B
r|B
r�B
r�B
r�B
sMB
s�B
s�B
tB
tTB
uB
u?B
utB
u�B
utB
u�B
vFB
vzB
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
xlB
x�B
x�B
x�B
x�B
y$B
y>B
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{JB
{dB
{�B
{�B
|B
|6B
|PB
|PB
|jB
|jB
|�B
}B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~]B
~]B
~(B
~�B
~�B
~�B
~�B
~�B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104845  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172355  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172355  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172355                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022403  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022403  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                