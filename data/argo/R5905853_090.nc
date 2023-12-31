CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:56Z creation;2022-06-04T17:38:57Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173856  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ZA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @نum��1   @نu�io@/G�z�H�cT�9Xb1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @y��@���A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   BffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C�C
  C�fC�fC�fC  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�fg@���A��A<��A\��A{33A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB��B��B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B�fgB�  B�34B���B���B���B���B���B���B���B���BÙ�BǙ�B���B�fgB�fgBי�Bۙ�Bߙ�B㙚B癚B�fgBB�B���B���B���C��C��C��C�gC	��C�3C�3C�3C��C��C��C��C��C��C�gC��C!��C#��C%��C'��C)��C+�3C-��C/��C1��C3��C5��C7��C9��C;��C=�gC?�gCA�gCC��CE��CG��CI��CK��CM��CO��CQ��CS�3CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�gC{�gC}�gC��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Ddl�Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�<�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XEA�QNA�A�A�B�A�>�A�&A��A�VA�OA��A��A�A��A��A� �A�!�A�"�A�#�A�$A�"�A�"hA�!�A�!bA�"�A�!�A�OA��A��A��A�A���A͹�A�z�Ȧ_Aɫ�AƢ4A���A�)_A�(A�B�A���A��{A�xA�oA�TaA���A�YKA��A�*�A�"4A�HA�}VA��A�ɺA���A�ȀA�A���A���A�8RA��nA�=�A�W
A��SA���A�S�A��mA��8A�~A�AUA���A�gA���A�uZA�m)A��A���A��#A�8�A�	�A�|A��gA�m]A�A�1�A���A�GEA�B'A��A�_A{�
Az�Ay��Ax��Ax��Aw�Av�ArAjԕAh\�Ae�Ae��Ae��AeC�Ac��A\�&AY��AX[WAU�fAR�AOp�AMAJ�7AIu�AH&�AD�A@�A?9XA<!�A:�A9�)A9�A8�bA7�A7l�A5��A4��A3��A2hsA1�XA1'�A0��A0��A/�?A/X�A0&A/($A-<�A,0UA*��A)�hA)j�A(�)A(eA(U2A(�bA'N<A%hsA$l"A#��A#��A#>�A"R�A ��A$�A�AcA7AxA�A��AzxA�DAQA�TA��A�DA	lAH�A=AԕAg�A��AK�A�"AJ#A��A
=A�A��A_AqvAGA��Al�AC-AVAp;A�]A�A�A?�AMA
�jA
w2A
A	��A	qvA	�A	;A��A��A�"A_�A9�ADgA8�A�A?}A�A	lA]�A�A͟A~�A�rA�BA�QA�OAK�A��A�A��A�jA�wA�_AVmA �A c�A J#@���@�8�@��b@�6@��V@�IR@��H@��@���@��9@�c@��9@�.I@�D�@���@��g@�$t@�W�@��g@�rG@�A�@�C@�<@�#:@��]@��.@�'�@��@�0�@�|�@�@�[W@���@�S@�E�@�j@���@��A@�7L@���@�-@�0�@���@�Z@ߙ�@���@ތ@ݷ�@�_p@�o@���@ܤ�@�~(@�g8@�Xy@�C�@۟V@ں�@�e�@�7@�H�@��@�"�@ֿ�@�|�@�'R@���@�<6@��@��@ӻ0@ӨX@ӊ�@�l�@�8@ҩ�@�E�@��W@��g@ѵt@�@�~@��6@�c@�%F@�6@��@�x@�-w@�$t@�Mj@�t�@��@�֡@̋D@�&�@ˇ�@�(�@��@ʣ@�Ov@�$@��>@ɣn@�O�@��[@��@ǜ@�@ƌ@�N�@��A@ŗ�@�n/@�@Ĺ$@�{�@�_�@�V@ÖS@�0�@�tT@��@�X�@��]@��\@�L0@�#:@��:@��@�e�@��@��@@��@���@�A @�ی@�>B@�x@��@�J#@�q@��@���@��@��Y@��@���@�v`@��@��@�x@���@��s@�U2@��w@��'@�RT@���@�M�@���@�b�@�+@��@���@���@�_@���@���@��M@�]�@��@�k�@�Y�@�U�@��@��@��I@��@��#@�n/@�X@���@��H@�9X@�P�@�Xy@��7@�4�@�A�@�t�@���@���@�u�@�GE@��6@�W?@���@�Xy@��@��Q@��=@�<6@��@�֡@�w�@���@��[@���@�U�@�V@���@��I@��@�-@��w@�hs@��@���@�r�@�Z@�"h@�
�@��r@��@��@�X�@��2@��+@�S�@���@�N<@�#�@���@�C-@���@��4@�v`@�dZ@�:�@�o@���@���@���@�Q�@��+@�O�@��@���@���@�s�@�u@�p�@�N<@�%@�͟@�u�@��o@��P@��@��r@�
�@��W@��Q@��6@�|�@�W?@��~@���@��4@�%F@�͟@�w�@�c�@�M@��@��g@�8@��@��j@���@��r@�tT@�H@��]@�>�@���@���@�l"@�S�@�2�@���@��-@���@���@���@�h
@�5?@�7@�ϫ@���@�zx@���@��$@��D@�Ta@�&�@���@���@��7@�J#@��@��@��?@���@�}V@�ff@�4n@��o@�ݘ@��#@��6@���@�L�@�E9@��X@��@��@���@�s@�=@��@���@��j@��+@�j@�Q@�?@�~@��D@���@�/�@���@���@�l"@�6�@�{@��[@��@��@��P@���@��B@��4@��@��S@�Vm@�G�@���@��@��_@�GE@��@�A@��@@~�r@~YK@~@}:�@|!@{v`@z��@z��@zOv@z�@y�'@x�v@x�@x��@xc�@x�@w�@w�@vp;@u�.@u�3@u��@uc@ua�@u*0@t�@t��@t�@s�[@s4�@r�@q�j@q	l@p?�@poi@pS�@p:�@o�Q@o��@o��@o@O@n�M@n�@mVm@lFt@k�:@j��@ju@iVm@h�|@hoi@h@g�;@g��@g��@g1�@f�R@f�@e�'@d֡@dV�@d2�@c��@b��@b?@a��@bO@b�@a�@a��@aN<@`�[@_��@_��@_l�@_C�@_�@^��@^l�@]��@]F@\�@\�@\:�@\�@[��@[Mj@[�@Zߤ@Z��@Z�1@Z5?@Y�@Y\�@Xی@X��@XM@W�r@W�F@Wl�@W4�@W�@Vv�@V	@U�C@U*0@T�@Tq@T  @S��@S$t@R�@R}V@Ru@Qo @Q=�@Q \@P�@P��@P]d@O��@OX�@N��@Nh
@NH�@N�@M��@MF@L��@K�@K�+@K�Q@K��@J�s@JC�@J&�@I��@H�v@H�@Hw�@Hoi@HD�@G�@Ge�@F�@Fxl@F5?@E��@E��@E�@D��@DN�@C�@CS�@C
=@B�@B�1@BH�@Be@B�@AY�@A�@@��@@��@@~(@@<�@@  @?�[@?a@?S@>�8@>�H@>�<@>^5@>;�@>1�@>4@=�@=�@=rG@=�@<�O@<c�@<"h@<@;�+@;��@;e�@;S�@;33@:��@:�A@9�3@9��@9f�@9�@8�)@8�D@8 �@7��@6��@6�A@6+k@5�T@5�M@5�@4M@4�@3�@3a@3�@2��@2��@2��@2��@2J�@2!�@1�#@1��@1��@2J@2$�@1��@0�P@0tT@0Z@0Ft@07�@/��@/��@/��@/Z�@/E9@/�@/Y@.��@.6�@-�D@-��@-�h@-2a@,��@,��@,|�@,C-@,1@+��@+�
@+��@+\)@+/�@*�@*�m@*�\@*c @*-@)�9@)�n@)�7@)Q�@(��@(��@(H@'�g@'K�@'S@&��@&�6@&�A@&E�@&$�@%�@%�@%zx@%7L@$��@$��@$��@$oi@$*�@#��@#�}@#��@#��@#�@#RT@"ߤ@"�1@":*@!�9@!��@!��@!�=@!��@!&�@ ��@ ��@ �$@ z�@ M@��@x@A�@�@��@kQ@=q@�Z@��@|@[W@G�@8�@#�@��@�@e�@U2@4n@�@�W@��@�$@y�@F�@�@�L@}V@_�@0U@�>@��@Y�@�K@��@Q�@G@�6@��@j�@)_@�@��@5?@��@�#@��@�@��@��@�7@F@V@�)@�Y@h�@g8@1'@�A@�&@�0@]�@=@"�@�c@�m@��@��@\�@��@�@�h@N<@��@V�@	�@�;@��@�q@�@_p@�@�H@�'@�r@u@�d@��@�S@x�@hs@Y�@/@@�/@��@��@Q�@@  @��@�g@��@a@/�@Y@
�m@
��@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�XEA�QNA�A�A�B�A�>�A�&A��A�VA�OA��A��A�A��A��A� �A�!�A�"�A�#�A�$A�"�A�"hA�!�A�!bA�"�A�!�A�OA��A��A��A�A���A͹�A�z�Ȧ_Aɫ�AƢ4A���A�)_A�(A�B�A���A��{A�xA�oA�TaA���A�YKA��A�*�A�"4A�HA�}VA��A�ɺA���A�ȀA�A���A���A�8RA��nA�=�A�W
A��SA���A�S�A��mA��8A�~A�AUA���A�gA���A�uZA�m)A��A���A��#A�8�A�	�A�|A��gA�m]A�A�1�A���A�GEA�B'A��A�_A{�
Az�Ay��Ax��Ax��Aw�Av�ArAjԕAh\�Ae�Ae��Ae��AeC�Ac��A\�&AY��AX[WAU�fAR�AOp�AMAJ�7AIu�AH&�AD�A@�A?9XA<!�A:�A9�)A9�A8�bA7�A7l�A5��A4��A3��A2hsA1�XA1'�A0��A0��A/�?A/X�A0&A/($A-<�A,0UA*��A)�hA)j�A(�)A(eA(U2A(�bA'N<A%hsA$l"A#��A#��A#>�A"R�A ��A$�A�AcA7AxA�A��AzxA�DAQA�TA��A�DA	lAH�A=AԕAg�A��AK�A�"AJ#A��A
=A�A��A_AqvAGA��Al�AC-AVAp;A�]A�A�A?�AMA
�jA
w2A
A	��A	qvA	�A	;A��A��A�"A_�A9�ADgA8�A�A?}A�A	lA]�A�A͟A~�A�rA�BA�QA�OAK�A��A�A��A�jA�wA�_AVmA �A c�A J#@���@�8�@��b@�6@��V@�IR@��H@��@���@��9@�c@��9@�.I@�D�@���@��g@�$t@�W�@��g@�rG@�A�@�C@�<@�#:@��]@��.@�'�@��@�0�@�|�@�@�[W@���@�S@�E�@�j@���@��A@�7L@���@�-@�0�@���@�Z@ߙ�@���@ތ@ݷ�@�_p@�o@���@ܤ�@�~(@�g8@�Xy@�C�@۟V@ں�@�e�@�7@�H�@��@�"�@ֿ�@�|�@�'R@���@�<6@��@��@ӻ0@ӨX@ӊ�@�l�@�8@ҩ�@�E�@��W@��g@ѵt@�@�~@��6@�c@�%F@�6@��@�x@�-w@�$t@�Mj@�t�@��@�֡@̋D@�&�@ˇ�@�(�@��@ʣ@�Ov@�$@��>@ɣn@�O�@��[@��@ǜ@�@ƌ@�N�@��A@ŗ�@�n/@�@Ĺ$@�{�@�_�@�V@ÖS@�0�@�tT@��@�X�@��]@��\@�L0@�#:@��:@��@�e�@��@��@@��@���@�A @�ی@�>B@�x@��@�J#@�q@��@���@��@��Y@��@���@�v`@��@��@�x@���@��s@�U2@��w@��'@�RT@���@�M�@���@�b�@�+@��@���@���@�_@���@���@��M@�]�@��@�k�@�Y�@�U�@��@��@��I@��@��#@�n/@�X@���@��H@�9X@�P�@�Xy@��7@�4�@�A�@�t�@���@���@�u�@�GE@��6@�W?@���@�Xy@��@��Q@��=@�<6@��@�֡@�w�@���@��[@���@�U�@�V@���@��I@��@�-@��w@�hs@��@���@�r�@�Z@�"h@�
�@��r@��@��@�X�@��2@��+@�S�@���@�N<@�#�@���@�C-@���@��4@�v`@�dZ@�:�@�o@���@���@���@�Q�@��+@�O�@��@���@���@�s�@�u@�p�@�N<@�%@�͟@�u�@��o@��P@��@��r@�
�@��W@��Q@��6@�|�@�W?@��~@���@��4@�%F@�͟@�w�@�c�@�M@��@��g@�8@��@��j@���@��r@�tT@�H@��]@�>�@���@���@�l"@�S�@�2�@���@��-@���@���@���@�h
@�5?@�7@�ϫ@���@�zx@���@��$@��D@�Ta@�&�@���@���@��7@�J#@��@��@��?@���@�}V@�ff@�4n@��o@�ݘ@��#@��6@���@�L�@�E9@��X@��@��@���@�s@�=@��@���@��j@��+@�j@�Q@�?@�~@��D@���@�/�@���@���@�l"@�6�@�{@��[@��@��@��P@���@��B@��4@��@��S@�Vm@�G�@���@��@��_@�GE@��@�A@��@@~�r@~YK@~@}:�@|!@{v`@z��@z��@zOv@z�@y�'@x�v@x�@x��@xc�@x�@w�@w�@vp;@u�.@u�3@u��@uc@ua�@u*0@t�@t��@t�@s�[@s4�@r�@q�j@q	l@p?�@poi@pS�@p:�@o�Q@o��@o��@o@O@n�M@n�@mVm@lFt@k�:@j��@ju@iVm@h�|@hoi@h@g�;@g��@g��@g1�@f�R@f�@e�'@d֡@dV�@d2�@c��@b��@b?@a��@bO@b�@a�@a��@aN<@`�[@_��@_��@_l�@_C�@_�@^��@^l�@]��@]F@\�@\�@\:�@\�@[��@[Mj@[�@Zߤ@Z��@Z�1@Z5?@Y�@Y\�@Xی@X��@XM@W�r@W�F@Wl�@W4�@W�@Vv�@V	@U�C@U*0@T�@Tq@T  @S��@S$t@R�@R}V@Ru@Qo @Q=�@Q \@P�@P��@P]d@O��@OX�@N��@Nh
@NH�@N�@M��@MF@L��@K�@K�+@K�Q@K��@J�s@JC�@J&�@I��@H�v@H�@Hw�@Hoi@HD�@G�@Ge�@F�@Fxl@F5?@E��@E��@E�@D��@DN�@C�@CS�@C
=@B�@B�1@BH�@Be@B�@AY�@A�@@��@@��@@~(@@<�@@  @?�[@?a@?S@>�8@>�H@>�<@>^5@>;�@>1�@>4@=�@=�@=rG@=�@<�O@<c�@<"h@<@;�+@;��@;e�@;S�@;33@:��@:�A@9�3@9��@9f�@9�@8�)@8�D@8 �@7��@6��@6�A@6+k@5�T@5�M@5�@4M@4�@3�@3a@3�@2��@2��@2��@2��@2J�@2!�@1�#@1��@1��@2J@2$�@1��@0�P@0tT@0Z@0Ft@07�@/��@/��@/��@/Z�@/E9@/�@/Y@.��@.6�@-�D@-��@-�h@-2a@,��@,��@,|�@,C-@,1@+��@+�
@+��@+\)@+/�@*�@*�m@*�\@*c @*-@)�9@)�n@)�7@)Q�@(��@(��@(H@'�g@'K�@'S@&��@&�6@&�A@&E�@&$�@%�@%�@%zx@%7L@$��@$��@$��@$oi@$*�@#��@#�}@#��@#��@#�@#RT@"ߤ@"�1@":*@!�9@!��@!��@!�=@!��@!&�@ ��@ ��@ �$@ z�@ M@��@x@A�@�@��@kQ@=q@�Z@��@|@[W@G�@8�@#�@��@�@e�@U2@4n@�@�W@��@�$@y�@F�@�@�L@}V@_�@0U@�>@��@Y�@�K@��@Q�@G@�6@��@j�@)_@�@��@5?@��@�#@��@�@��@��@�7@F@V@�)@�Y@h�@g8@1'@�A@�&@�0@]�@=@"�@�c@�m@��@��@\�@��@�@�h@N<@��@V�@	�@�;@��@�q@�@_p@�@�H@�'@�r@u@�d@��@�S@x�@hs@Y�@/@@�/@��@��@Q�@@  @��@�g@��@a@/�@Y@
�m@
��@
��@
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�`B
�`B
�FB
�FB
�`B
�,B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�,B
�B
��B
��B
��B
��B
��B
��B
��B
�tB
�ZB
�B
�nB
��B
��B
��B
�B
��B
żB
��B
��B
�B
��B�BjB9�BXyBa�Bi�B~�B�OB��BƎB�2B�B�
B�AB�OB�B��BPB1B%�B(
B*�B,"B+�B/ B*�B$B]B�B�B�B�rB�GB�B�B�B��B��B��B��B��B��B^jB'B\B
�UB
~�B
S&B
8lB
B
B	��B	��B	�B	��B	�B	��B	�"B	�}B	�'B	wfB	z^B	|�B	{�B	utB	X�B	FYB	=�B	0B	�B		7B�.B�*B��B��B	 �B�-B�B��B�tB��B	 4B	�B	1[B	JXB	I�B	MPB	RB	Z�B	b�B	j�B	{B	�B	��B	�B	�-B	��B	�iB	�;B	�B	�B	ʦB	ӏB	��B	�>B
B
 OB	��B	��B	�B	��B	�*B	�B	�@B	��B	��B	�B	ɺB	ɠB	̘B	�vB	��B	�B	ɠB	�B	�B	��B	��B	�YB	��B	�B	ǮB	�)B	̳B	�)B	�^B	�xB	ʌB	ɺB	��B	�mB	��B	ŢB	ŢB	��B	�YB	��B	��B	āB	āB	�SB	��B	��B	�#B	�RB	��B	�PB	͹B	�B	�BB	ϑB	�hB	�B	��B	ѷB	�YB	�?B	�_B	�$B	ּB	ּB	�aB	��B	ԯB	��B	ӏB	�{B	�EB	��B	�sB	��B	�@B	�,B	ңB	�B	��B	��B	�uB	�B	�}B	�B	��B	�[B	� B	ѝB	�4B	�HB	��B	͟B	�B	�BB	� B	ΊB	��B	�<B	�HB	�BB	�vB	ϑB	�}B	уB	ѝB	�TB	��B	��B	ҽB	��B	�B	��B	��B	�2B	��B	�2B	ԯB	�B	՛B	��B	��B	՛B	�MB	ևB	��B	��B	�9B	�
B	��B	ؓB	��B	�B	�QB	�7B	�B	�7B	�7B	�7B	��B	�eB	�yB	��B	�B	�KB	خB	�$B	��B	��B	�EB	��B	�yB	��B	�B	��B	�B	��B	�=B	�	B	�	B	��B	�B	�kB	�eB	ؓB	�yB	�+B	��B	خB	�=B	چB	�qB	��B	�~B	ߤB	�@B	�B	�B	�ZB	�ZB	��B	��B	��B	��B	�LB	�B	�B	��B	�RB	��B	�$B	�>B	�$B	�RB	�RB	��B	�B	�B	�B	��B	��B	�KB	�B	�eB	��B	�B	�B	�B	�wB	�cB	�B	�B	�B	�B	��B	�]B	�IB	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	�B	�?B	��B	�B	��B	�B	�nB	��B	�GB	�|B	��B	��B	�B	�GB	�|B	�B	�B	��B	�FB	��B	�FB	�B	��B	�ZB	�zB	��B	��B	��B	�dB	��B	�.B	��B
 �B
 OB
 �B
B
 �B	�}B	�}B
 �B
 �B	�cB	��B	�B	��B	��B	�6B	��B	��B	�^B	�^B	�B	�DB	�DB	�DB	��B	�B	��B	�B	�dB	�6B	�0B	��B	��B	��B	�B	�HB
 OB
 4B
 iB
  B
 B
 �B
 �B
�B
�B
�B
�B
aB
{B
aB
{B
MB
�B
�B
�B
B
3B
�B
�B
[B
uB
[B
�B
�B
B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
+B
�B
�B
VB
�B
HB
�B
�B
hB
4B
�B
$B
,B
YB
+B
�B
�B
�B
kB
QB
�B
�B
~B
B
dB
5B
�B
B
�B
~B
pB
 BB
 \B
 �B
 �B
 vB
 �B
!-B
!�B
!bB
!�B
!�B
"�B
"�B
"�B
#:B
#�B
"�B
#TB
"�B
#TB
#�B
$�B
%,B
$�B
%,B
%�B
%�B
%zB
%�B
%�B
%�B
&LB
&fB
&B
%�B
&�B
&LB
'mB
'8B
'�B
(>B
(�B
(�B
(XB
(�B
)B
)B
(�B
)B
)*B
)*B
)DB
)_B
)_B
(�B
)_B
)yB
)�B
*0B
*0B
*�B
*�B
*0B
*�B
*B
+B
+�B
+�B
*B
+�B
+B
+�B
,qB
-CB
,�B
.�B
/5B
/5B
/�B
/ B
/B
/�B
.}B
-�B
.�B
.�B
.�B
-�B
-�B
/ B
/�B
/iB
/OB
0UB
1[B
2�B
1�B
2aB
2�B
2�B
3MB
3�B
3B
4nB
3�B
4�B
5�B
5�B
6FB
7LB
6�B
8lB
:*B
:�B
:�B
;B
;0B
;�B
;�B
;B
;�B
;�B
:�B
:xB
9�B
;0B
:�B
:�B
:�B
:^B
:*B
:�B
:�B
;B
;B
=<B
?�B
A�B
@�B
@OB
?�B
>�B
?HB
?�B
AoB
C{B
B�B
C�B
C{B
C�B
D�B
D�B
D�B
D3B
D�B
D�B
D�B
E�B
E�B
FB
FtB
GB
F�B
F�B
F�B
GB
G+B
G�B
G+B
G+B
G�B
HKB
H�B
H�B
H�B
I�B
IlB
I�B
I�B
IlB
J=B
J�B
J�B
J�B
KDB
K)B
K�B
LB
LJB
L~B
L~B
L�B
M�B
MjB
MB
M�B
M�B
NB
NB
N�B
OBB
O�B
O\B
O\B
O�B
O�B
P�B
QB
P�B
P�B
QB
P�B
P�B
PHB
Q�B
RTB
R�B
R�B
R�B
S&B
S[B
S�B
S�B
TB
T{B
TaB
VB
U�B
V9B
U�B
V�B
V�B
VSB
V�B
V9B
V�B
V�B
W
B
X�B
XB
X�B
X�B
X�B
Y1B
X�B
X�B
Y�B
Y�B
Y1B
X�B
YKB
Z7B
Y�B
Y�B
ZB
ZB
ZQB
ZkB
Z7B
[=B
[qB
Z�B
[=B
[�B
[qB
\)B
[�B
\B
\�B
\CB
]�B
\�B
\�B
]�B
^B
]�B
^5B
^�B
^jB
^�B
_VB
^�B
^5B
^B
^5B
^B
]�B
]�B
]�B
]IB
]�B
^B
^�B
^�B
^�B
_VB
_�B
_�B
`�B
a|B
bB
a-B
`�B
`�B
`�B
`�B
`\B
`�B
`�B
`�B
`�B
aHB
bB
b4B
a�B
a�B
a�B
a�B
b4B
b�B
bNB
b�B
b�B
cTB
c:B
cTB
cnB
dB
c�B
d�B
d�B
d�B
dZB
d�B
dtB
d�B
d�B
e`B
e�B
e�B
e�B
fLB
f�B
gB
gmB
g8B
gB
g�B
g�B
h
B
h$B
hXB
h�B
iB
iDB
iDB
i�B
i�B
i�B
jB
i�B
i�B
i�B
jB
j�B
j�B
kB
kkB
k�B
kkB
k6B
j�B
k�B
k�B
k�B
kQB
l"B
lB
l�B
lWB
l�B
mCB
mCB
l�B
mwB
m�B
nB
m�B
n�B
n}B
ncB
n}B
n�B
oOB
o5B
oOB
o�B
o�B
o�B
o�B
pUB
p!B
pUB
p�B
q'B
q'B
q[B
qvB
q�B
rB
r-B
raB
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t�B
t�B
utB
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
vzB
vFB
w2B
x8B
xB
x8B
xRB
x�B
xRB
y>B
x�B
y$B
x�B
yXB
y�B
y�B
y�B
y�B
y�B
{B
{dB
{�B
z�B
{�B
{�B
|B
|�B
|�B
}B
|�B
}�B
~BB
~�B
~]B
~wB
~�B
~BB
~�B
~�B
~�B
B
.B
}B
� B
�iB
� B
�B
�B
�iB
��B
��B
�UB
�B
�;B
�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�`B
�`B
�FB
�FB
�`B
�,B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�,B
�B
��B
��B
��B
��B
��B
��B
��B
�tB
�ZB
�B
�nB
��B
��B
��B
�B
��B
żB
��B
��B
�B
��B�BjB9�BXyBa�Bi�B~�B�OB��BƎB�2B�B�
B�AB�OB�B��BPB1B%�B(
B*�B,"B+�B/ B*�B$B]B�B�B�B�rB�GB�B�B�B��B��B��B��B��B��B^jB'B\B
�UB
~�B
S&B
8lB
B
B	��B	��B	�B	��B	�B	��B	�"B	�}B	�'B	wfB	z^B	|�B	{�B	utB	X�B	FYB	=�B	0B	�B		7B�.B�*B��B��B	 �B�-B�B��B�tB��B	 4B	�B	1[B	JXB	I�B	MPB	RB	Z�B	b�B	j�B	{B	�B	��B	�B	�-B	��B	�iB	�;B	�B	�B	ʦB	ӏB	��B	�>B
B
 OB	��B	��B	�B	��B	�*B	�B	�@B	��B	��B	�B	ɺB	ɠB	̘B	�vB	��B	�B	ɠB	�B	�B	��B	��B	�YB	��B	�B	ǮB	�)B	̳B	�)B	�^B	�xB	ʌB	ɺB	��B	�mB	��B	ŢB	ŢB	��B	�YB	��B	��B	āB	āB	�SB	��B	��B	�#B	�RB	��B	�PB	͹B	�B	�BB	ϑB	�hB	�B	��B	ѷB	�YB	�?B	�_B	�$B	ּB	ּB	�aB	��B	ԯB	��B	ӏB	�{B	�EB	��B	�sB	��B	�@B	�,B	ңB	�B	��B	��B	�uB	�B	�}B	�B	��B	�[B	� B	ѝB	�4B	�HB	��B	͟B	�B	�BB	� B	ΊB	��B	�<B	�HB	�BB	�vB	ϑB	�}B	уB	ѝB	�TB	��B	��B	ҽB	��B	�B	��B	��B	�2B	��B	�2B	ԯB	�B	՛B	��B	��B	՛B	�MB	ևB	��B	��B	�9B	�
B	��B	ؓB	��B	�B	�QB	�7B	�B	�7B	�7B	�7B	��B	�eB	�yB	��B	�B	�KB	خB	�$B	��B	��B	�EB	��B	�yB	��B	�B	��B	�B	��B	�=B	�	B	�	B	��B	�B	�kB	�eB	ؓB	�yB	�+B	��B	خB	�=B	چB	�qB	��B	�~B	ߤB	�@B	�B	�B	�ZB	�ZB	��B	��B	��B	��B	�LB	�B	�B	��B	�RB	��B	�$B	�>B	�$B	�RB	�RB	��B	�B	�B	�B	��B	��B	�KB	�B	�eB	��B	�B	�B	�B	�wB	�cB	�B	�B	�B	�B	��B	�]B	�IB	��B	��B	��B	�B	��B	�B	��B	��B	�B	��B	�B	�?B	��B	�B	��B	�B	�nB	��B	�GB	�|B	��B	��B	�B	�GB	�|B	�B	�B	��B	�FB	��B	�FB	�B	��B	�ZB	�zB	��B	��B	��B	�dB	��B	�.B	��B
 �B
 OB
 �B
B
 �B	�}B	�}B
 �B
 �B	�cB	��B	�B	��B	��B	�6B	��B	��B	�^B	�^B	�B	�DB	�DB	�DB	��B	�B	��B	�B	�dB	�6B	�0B	��B	��B	��B	�B	�HB
 OB
 4B
 iB
  B
 B
 �B
 �B
�B
�B
�B
�B
aB
{B
aB
{B
MB
�B
�B
�B
B
3B
�B
�B
[B
uB
[B
�B
�B
B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
+B
�B
�B
VB
�B
HB
�B
�B
hB
4B
�B
$B
,B
YB
+B
�B
�B
�B
kB
QB
�B
�B
~B
B
dB
5B
�B
B
�B
~B
pB
 BB
 \B
 �B
 �B
 vB
 �B
!-B
!�B
!bB
!�B
!�B
"�B
"�B
"�B
#:B
#�B
"�B
#TB
"�B
#TB
#�B
$�B
%,B
$�B
%,B
%�B
%�B
%zB
%�B
%�B
%�B
&LB
&fB
&B
%�B
&�B
&LB
'mB
'8B
'�B
(>B
(�B
(�B
(XB
(�B
)B
)B
(�B
)B
)*B
)*B
)DB
)_B
)_B
(�B
)_B
)yB
)�B
*0B
*0B
*�B
*�B
*0B
*�B
*B
+B
+�B
+�B
*B
+�B
+B
+�B
,qB
-CB
,�B
.�B
/5B
/5B
/�B
/ B
/B
/�B
.}B
-�B
.�B
.�B
.�B
-�B
-�B
/ B
/�B
/iB
/OB
0UB
1[B
2�B
1�B
2aB
2�B
2�B
3MB
3�B
3B
4nB
3�B
4�B
5�B
5�B
6FB
7LB
6�B
8lB
:*B
:�B
:�B
;B
;0B
;�B
;�B
;B
;�B
;�B
:�B
:xB
9�B
;0B
:�B
:�B
:�B
:^B
:*B
:�B
:�B
;B
;B
=<B
?�B
A�B
@�B
@OB
?�B
>�B
?HB
?�B
AoB
C{B
B�B
C�B
C{B
C�B
D�B
D�B
D�B
D3B
D�B
D�B
D�B
E�B
E�B
FB
FtB
GB
F�B
F�B
F�B
GB
G+B
G�B
G+B
G+B
G�B
HKB
H�B
H�B
H�B
I�B
IlB
I�B
I�B
IlB
J=B
J�B
J�B
J�B
KDB
K)B
K�B
LB
LJB
L~B
L~B
L�B
M�B
MjB
MB
M�B
M�B
NB
NB
N�B
OBB
O�B
O\B
O\B
O�B
O�B
P�B
QB
P�B
P�B
QB
P�B
P�B
PHB
Q�B
RTB
R�B
R�B
R�B
S&B
S[B
S�B
S�B
TB
T{B
TaB
VB
U�B
V9B
U�B
V�B
V�B
VSB
V�B
V9B
V�B
V�B
W
B
X�B
XB
X�B
X�B
X�B
Y1B
X�B
X�B
Y�B
Y�B
Y1B
X�B
YKB
Z7B
Y�B
Y�B
ZB
ZB
ZQB
ZkB
Z7B
[=B
[qB
Z�B
[=B
[�B
[qB
\)B
[�B
\B
\�B
\CB
]�B
\�B
\�B
]�B
^B
]�B
^5B
^�B
^jB
^�B
_VB
^�B
^5B
^B
^5B
^B
]�B
]�B
]�B
]IB
]�B
^B
^�B
^�B
^�B
_VB
_�B
_�B
`�B
a|B
bB
a-B
`�B
`�B
`�B
`�B
`\B
`�B
`�B
`�B
`�B
aHB
bB
b4B
a�B
a�B
a�B
a�B
b4B
b�B
bNB
b�B
b�B
cTB
c:B
cTB
cnB
dB
c�B
d�B
d�B
d�B
dZB
d�B
dtB
d�B
d�B
e`B
e�B
e�B
e�B
fLB
f�B
gB
gmB
g8B
gB
g�B
g�B
h
B
h$B
hXB
h�B
iB
iDB
iDB
i�B
i�B
i�B
jB
i�B
i�B
i�B
jB
j�B
j�B
kB
kkB
k�B
kkB
k6B
j�B
k�B
k�B
k�B
kQB
l"B
lB
l�B
lWB
l�B
mCB
mCB
l�B
mwB
m�B
nB
m�B
n�B
n}B
ncB
n}B
n�B
oOB
o5B
oOB
o�B
o�B
o�B
o�B
pUB
p!B
pUB
p�B
q'B
q'B
q[B
qvB
q�B
rB
r-B
raB
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t�B
t�B
utB
u?B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
vzB
vFB
w2B
x8B
xB
x8B
xRB
x�B
xRB
y>B
x�B
y$B
x�B
yXB
y�B
y�B
y�B
y�B
y�B
{B
{dB
{�B
z�B
{�B
{�B
|B
|�B
|�B
}B
|�B
}�B
~BB
~�B
~]B
~wB
~�B
~BB
~�B
~�B
~�B
B
.B
}B
� B
�iB
� B
�B
�B
�iB
��B
��B
�UB
�B
�;B
�;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104921  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173856  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173857  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173857                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023904  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023904  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                