CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-10-25T15:35:43Z creation;2018-10-25T15:35:46Z conversion to V3.1;2019-12-18T07:19:21Z update;2022-11-21T05:29:57Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20181025153543  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_153                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @؋���X�1   @؋��� @<��(���d�S&1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@y��@���@���A��A<��A\��A{33A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��fD���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111x�A�bA��A��A��A� �A��A��A�&�A�"�A�33A�/A�5?A�33A�1'A�$�A�$�A�(�A�"�A���A���A���A�9XA�t�A�Q�A�XA��+A�|�A��wA���A�I�A���A���A�x�A���A�O�A���A��A�hsA�{A���A��yA�ȴA���A�  A�ȴA��A�r�A�A��A��hA�1'A�9XA��7A��+A���A�%A�1'A���A�?}A�bA��uA�M�A��TA�M�A�`BA�bNA��A�VA��A�=qA��mA��jA��wA�(�A�Q�A�bNA�/A�VA��HA��HA���A�ZA�9XA~=qA}%A|{A{"�Ay��Ax�uAw\)AuC�As`BAqhsAp�Ao��AnE�AlM�AidZAh �Ag�wAg&�Af�+Af$�Ae�hAd��AcK�AaS�A_�FA^$�A\��A[�AZ�9AY%AW�AVJAUAT�AR�RAR5?AQG�AO�-AN-AL��ALJAJ�AI�AI%AH�9AHn�AH{AG��AG;dAFv�AE�^AEO�AD��AD$�AC|�AB��ABffAB1'AB1AA�AA%A@��A?��A?oA>{A=;dA<�`A<r�A;��A;�^A;�hA:�/A9t�A8VA7�
A7�A7S�A6�jA5S�A4�A3O�A2�A2A�A0��A0bNA0-A/�;A/��A/`BA.��A-O�A,^5A,bA+�A+��A+?}A*-A(ȴA(r�A(A'�7A'�A&ȴA&bA$�A$�9A$jA#�A#&�A"�DA"$�A!�
A!�A �A �A M�A�mA��A;dA�A�A��A1Al�A��AjA�A��A�;A�yA�+AAJAv�A�A`BAn�A�A;dA�A�7A�RA^5A$�AbA|�A
ZA	�A	S�AVA�FA33A�A\)A�!A��Ar�AƨA"�A ��A I�A �@�;d@�Ĝ@�I�@��@��@���@��@�v�@���@��@��/@��@��`@�C�@ꗍ@���@�/@���@�j@�S�@�~�@�-@��@���@��@�33@�V@�;d@��@�bN@�r�@��@�x�@�1@��H@�=q@ա�@��@�1'@Ӆ@�
=@��y@���@�  @ͺ^@̣�@̃@�9X@��
@�33@��@�1@�dZ@Ə\@�-@��@š�@�?}@ċD@�dZ@��@��P@�O�@�ȴ@���@�ƨ@��+@���@�ƨ@���@��-@�/@��j@���@�z�@�bN@�Z@�Q�@�9X@�|�@�M�@�X@�G�@�z�@���@���@�5?@���@�`B@�&�@���@��@���@���@���@���@��u@�9X@��@��R@�^5@�@�x�@�G�@��@��P@��@�hs@��9@�A�@��w@�\)@���@�$�@�J@���@�`B@��@���@��@�I�@�9X@� �@�  @��;@�t�@�5?@��T@���@���@�`B@�/@�/@��@���@�ƨ@���@��@��-@��@�I�@��;@�"�@��+@�5?@��@���@��@���@��u@�A�@��w@���@�t�@�dZ@�K�@�"�@���@�ȴ@��\@�v�@�ff@�V@�5?@�$�@�{@���@��-@��@� �@��@��!@�-@�{@�J@���@��#@�@�hs@��@��@�Ĝ@��D@�j@�Z@�Q�@�Q�@�9X@��@��w@���@�C�@��@���@�V@�@���@��@��D@��D@��@�r�@�j@�bN@�Z@�I�@�1'@� �@�b@�  @��m@��
@��w@��@���@�t�@���@�@��@���@�hs@�%@��u@�(�@�P@~�y@~��@~V@~E�@~{@~{@~@}�h@}`B@}/@|��@|�@{33@z-@y�#@y�#@y��@xĜ@xQ�@w�;@w��@w��@x  @x  @w�w@w
=@v��@vv�@vff@v$�@u��@u@u�@uO�@u/@t��@t�j@t�D@tZ@s�F@r-@q�@q��@q�7@qX@p�`@pr�@o�@o�@o�@o�@n�+@m�T@m`B@l�@l�@l��@l1@k�F@k"�@j��@j��@j��@j��@j�!@j�\@j=q@j-@i�7@iG�@i7L@h�`@h��@h  @gl�@f�y@f��@fE�@e�-@ep�@e/@eV@d�D@d(�@c��@cC�@c"�@co@b�@b�H@b�H@b�\@a�7@aX@a�@`��@`��@`�@` �@_�w@_��@_�P@_|�@_K�@_+@_+@^ȴ@^V@]O�@]V@\��@\�D@\(�@[t�@Z�@Y��@X�`@X��@XbN@W��@W�@V�y@V�R@V��@V��@V�+@V�+@V�+@Vff@V5?@V@U��@U�h@T�/@SC�@So@R�H@R��@R^5@RJ@Q��@Q��@Q�7@Q�7@Q�7@Q�7@Q�7@QX@Q%@P��@P�@Pb@O��@Ol�@O;d@N�y@Nȴ@N��@NV@NV@N$�@M�T@MO�@L�D@LZ@L(�@K��@KC�@K33@K@J~�@J=q@J-@J�@I�#@I&�@H�u@Hr�@HA�@H1'@H �@Hb@H  @G�;@G�;@G��@G��@G�w@G�w@G�@G��@G�P@G+@E�@E�-@E�h@E�@E`B@EO�@E/@D�@D��@D�@D(�@Cƨ@C��@C"�@B��@B�\@Bn�@Bn�@B=q@B�@Ax�@@��@@�@?��@?K�@>�R@=�-@=`B@=?}@=�@=V@<�@<�@<�/@<��@<�j@<j@;t�@;S�@;@:��@:n�@:=q@9�^@9��@9x�@9hs@8��@8Q�@81'@81'@8 �@7�P@7
=@6ff@6{@5��@5@5�-@5��@5`B@4��@4j@4(�@3�m@3�F@3��@3�@3t�@3dZ@3S�@3S�@3S�@3S�@3S�@3S�@3C�@333@333@333@333@3"�@3o@2��@1��@1X@1&�@1%@0�`@0�@0  @/�@.ȴ@.�R@.��@.��@.V@.5?@.5?@.$�@.@-�@-@-�@-p�@-p�@-?}@,��@,�D@,j@,(�@,�@+�m@+��@+�@+�@+S�@*�@*��@*�\@*��@*��@*^5@*=q@*-@*-@*J@)�#@)��@)hs@)�@(r�@'�@'|�@';d@&�y@&��@&�+@&$�@&@%�@%�T@%�T@%�T@%�T@%��@%��@%�-@%�h@%?}@%V@$�/@$�j@$Z@#�m@#t�@#@"n�@"^5@"-@!�@!�^@!�^@!�^@!�^@!��@!hs@!%@ �`@ �@ 1'@�w@�@�y@ȴ@��@V@E�@E�@@�-@�@/@��@z�@Z@I�@(�@ƨ@��@S�@��@~�@^5@=q@�@�@�#@��@hs@G�@%@��@bN@bN@Q�@A�@A�@A�@1'@ �@b@  @��@�P@\)@+@�+@E�@E�@E�@$�@@�@�T@��@@@@��@�@/@�/@��@�D@1@�m@ƨ@�F@��@��@�@dZ@S�@C�@33@"�@��@-@�#@X@%@�9@��@�u@  @��@��@�@��@|�@+@��@ȴ@�+@ff@@�T@�-@O�@V@�D@Z@9X@(�@�@1@�m@��@@
�!@
��@
^5@	��@	�#@	��@	�^@	��@	�7@	7L@	%@�`@Ĝ@�u@bN@A�@1'@  @�@�;@�;@��@�w@�@��@�P@|�@|�@l�@K�@�R@5?@�@�T@@�-@�-@�-@�-@��@�h@O�@�@��@�/@�j@��@��@�D@j@I�@��@��@t�@33@o@@�H@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111x�A�bA��A��A��A� �A��A��A�&�A�"�A�33A�/A�5?A�33A�1'A�$�A�$�A�(�A�"�A���A���A���A�9XA�t�A�Q�A�XA��+A�|�A��wA���A�I�A���A���A�x�A���A�O�A���A��A�hsA�{A���A��yA�ȴA���A�  A�ȴA��A�r�A�A��A��hA�1'A�9XA��7A��+A���A�%A�1'A���A�?}A�bA��uA�M�A��TA�M�A�`BA�bNA��A�VA��A�=qA��mA��jA��wA�(�A�Q�A�bNA�/A�VA��HA��HA���A�ZA�9XA~=qA}%A|{A{"�Ay��Ax�uAw\)AuC�As`BAqhsAp�Ao��AnE�AlM�AidZAh �Ag�wAg&�Af�+Af$�Ae�hAd��AcK�AaS�A_�FA^$�A\��A[�AZ�9AY%AW�AVJAUAT�AR�RAR5?AQG�AO�-AN-AL��ALJAJ�AI�AI%AH�9AHn�AH{AG��AG;dAFv�AE�^AEO�AD��AD$�AC|�AB��ABffAB1'AB1AA�AA%A@��A?��A?oA>{A=;dA<�`A<r�A;��A;�^A;�hA:�/A9t�A8VA7�
A7�A7S�A6�jA5S�A4�A3O�A2�A2A�A0��A0bNA0-A/�;A/��A/`BA.��A-O�A,^5A,bA+�A+��A+?}A*-A(ȴA(r�A(A'�7A'�A&ȴA&bA$�A$�9A$jA#�A#&�A"�DA"$�A!�
A!�A �A �A M�A�mA��A;dA�A�A��A1Al�A��AjA�A��A�;A�yA�+AAJAv�A�A`BAn�A�A;dA�A�7A�RA^5A$�AbA|�A
ZA	�A	S�AVA�FA33A�A\)A�!A��Ar�AƨA"�A ��A I�A �@�;d@�Ĝ@�I�@��@��@���@��@�v�@���@��@��/@��@��`@�C�@ꗍ@���@�/@���@�j@�S�@�~�@�-@��@���@��@�33@�V@�;d@��@�bN@�r�@��@�x�@�1@��H@�=q@ա�@��@�1'@Ӆ@�
=@��y@���@�  @ͺ^@̣�@̃@�9X@��
@�33@��@�1@�dZ@Ə\@�-@��@š�@�?}@ċD@�dZ@��@��P@�O�@�ȴ@���@�ƨ@��+@���@�ƨ@���@��-@�/@��j@���@�z�@�bN@�Z@�Q�@�9X@�|�@�M�@�X@�G�@�z�@���@���@�5?@���@�`B@�&�@���@��@���@���@���@���@��u@�9X@��@��R@�^5@�@�x�@�G�@��@��P@��@�hs@��9@�A�@��w@�\)@���@�$�@�J@���@�`B@��@���@��@�I�@�9X@� �@�  @��;@�t�@�5?@��T@���@���@�`B@�/@�/@��@���@�ƨ@���@��@��-@��@�I�@��;@�"�@��+@�5?@��@���@��@���@��u@�A�@��w@���@�t�@�dZ@�K�@�"�@���@�ȴ@��\@�v�@�ff@�V@�5?@�$�@�{@���@��-@��@� �@��@��!@�-@�{@�J@���@��#@�@�hs@��@��@�Ĝ@��D@�j@�Z@�Q�@�Q�@�9X@��@��w@���@�C�@��@���@�V@�@���@��@��D@��D@��@�r�@�j@�bN@�Z@�I�@�1'@� �@�b@�  @��m@��
@��w@��@���@�t�@���@�@��@���@�hs@�%@��u@�(�@�P@~�y@~��@~V@~E�@~{@~{@~@}�h@}`B@}/@|��@|�@{33@z-@y�#@y�#@y��@xĜ@xQ�@w�;@w��@w��@x  @x  @w�w@w
=@v��@vv�@vff@v$�@u��@u@u�@uO�@u/@t��@t�j@t�D@tZ@s�F@r-@q�@q��@q�7@qX@p�`@pr�@o�@o�@o�@o�@n�+@m�T@m`B@l�@l�@l��@l1@k�F@k"�@j��@j��@j��@j��@j�!@j�\@j=q@j-@i�7@iG�@i7L@h�`@h��@h  @gl�@f�y@f��@fE�@e�-@ep�@e/@eV@d�D@d(�@c��@cC�@c"�@co@b�@b�H@b�H@b�\@a�7@aX@a�@`��@`��@`�@` �@_�w@_��@_�P@_|�@_K�@_+@_+@^ȴ@^V@]O�@]V@\��@\�D@\(�@[t�@Z�@Y��@X�`@X��@XbN@W��@W�@V�y@V�R@V��@V��@V�+@V�+@V�+@Vff@V5?@V@U��@U�h@T�/@SC�@So@R�H@R��@R^5@RJ@Q��@Q��@Q�7@Q�7@Q�7@Q�7@Q�7@QX@Q%@P��@P�@Pb@O��@Ol�@O;d@N�y@Nȴ@N��@NV@NV@N$�@M�T@MO�@L�D@LZ@L(�@K��@KC�@K33@K@J~�@J=q@J-@J�@I�#@I&�@H�u@Hr�@HA�@H1'@H �@Hb@H  @G�;@G�;@G��@G��@G�w@G�w@G�@G��@G�P@G+@E�@E�-@E�h@E�@E`B@EO�@E/@D�@D��@D�@D(�@Cƨ@C��@C"�@B��@B�\@Bn�@Bn�@B=q@B�@Ax�@@��@@�@?��@?K�@>�R@=�-@=`B@=?}@=�@=V@<�@<�@<�/@<��@<�j@<j@;t�@;S�@;@:��@:n�@:=q@9�^@9��@9x�@9hs@8��@8Q�@81'@81'@8 �@7�P@7
=@6ff@6{@5��@5@5�-@5��@5`B@4��@4j@4(�@3�m@3�F@3��@3�@3t�@3dZ@3S�@3S�@3S�@3S�@3S�@3S�@3C�@333@333@333@333@3"�@3o@2��@1��@1X@1&�@1%@0�`@0�@0  @/�@.ȴ@.�R@.��@.��@.V@.5?@.5?@.$�@.@-�@-@-�@-p�@-p�@-?}@,��@,�D@,j@,(�@,�@+�m@+��@+�@+�@+S�@*�@*��@*�\@*��@*��@*^5@*=q@*-@*-@*J@)�#@)��@)hs@)�@(r�@'�@'|�@';d@&�y@&��@&�+@&$�@&@%�@%�T@%�T@%�T@%�T@%��@%��@%�-@%�h@%?}@%V@$�/@$�j@$Z@#�m@#t�@#@"n�@"^5@"-@!�@!�^@!�^@!�^@!�^@!��@!hs@!%@ �`@ �@ 1'@�w@�@�y@ȴ@��@V@E�@E�@@�-@�@/@��@z�@Z@I�@(�@ƨ@��@S�@��@~�@^5@=q@�@�@�#@��@hs@G�@%@��@bN@bN@Q�@A�@A�@A�@1'@ �@b@  @��@�P@\)@+@�+@E�@E�@E�@$�@@�@�T@��@@@@��@�@/@�/@��@�D@1@�m@ƨ@�F@��@��@�@dZ@S�@C�@33@"�@��@-@�#@X@%@�9@��@�u@  @��@��@�@��@|�@+@��@ȴ@�+@ff@@�T@�-@O�@V@�D@Z@9X@(�@�@1@�m@��@@
�!@
��@
^5@	��@	�#@	��@	�^@	��@	�7@	7L@	%@�`@Ĝ@�u@bN@A�@1'@  @�@�;@�;@��@�w@�@��@�P@|�@|�@l�@K�@�R@5?@�@�T@@�-@�-@�-@�-@��@�h@O�@�@��@�/@�j@��@��@�D@j@I�@��@��@t�@33@o@@�H@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB}�BXB �B�/B�dB�3B�!B�B�B�B�B��B��B�DB�B� B}�B{�Bl�BXBM�BM�BQ�BS�BN�BE�B<jB1'B�B�sB��B��B��B��B��B�'B�wBŢB�RB�3B�'B��B��B��B��B�%Bq�BgmB<jB&�B�BJBB
�B
�B
ŢB
�?B
��B
��B
�=B
x�B
o�B
hsB
`BB
W
B
N�B
D�B
6FB
(�B
�B
�B
bB
B	��B	�ZB	�/B	�B	��B	��B	��B	��B	ÖB	�^B	�B	��B	��B	�oB	�PB	�%B	|�B	r�B	m�B	ffB	bNB	^5B	ZB	VB	Q�B	L�B	J�B	H�B	B�B	>wB	=qB	;dB	:^B	8RB	6FB	5?B	2-B	33B	33B	0!B	-B	)�B	&�B	%�B	#�B	"�B	 �B	�B	�B	�B	�B	bB	JB	
=B	+B	B	B	B	B��B�B�B�B�B�B�sB�NB�5B�/B�B�B�B��B��B��B��B��BȴBŢBĜBÖBB�}B�dB�FB�?B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�VB�=B�+B�B�B�B}�By�Bv�Bu�Bs�Bo�Bk�BgmBe`BdZBcTBaHB_;B]/B[#BYBXBXBW
BT�BR�BP�BO�BM�BK�BI�BG�BE�BD�BA�B?}B>wB=qB<jB;dB:^B9XB8RB7LB6FB6FB49B33B1'B/B,B)�B'�B&�B&�B&�B'�B(�B(�B'�B'�B&�B'�B&�B%�B$�B%�B$�B#�B#�B �B!�B�B�B�B�B�B�B�B�B�B �B"�B$�B$�B&�B'�B&�B&�B%�B%�B&�B'�B&�B&�B&�B&�B'�B'�B&�B(�B,B,B(�B0!B0!B.B.B.B2-B49B8RB9XB;dB;dB<jB<jB<jB<jB<jB>wBB�BE�BD�BF�BG�BH�BI�BL�BM�BO�BQ�BQ�BQ�BQ�BQ�BQ�BR�BS�BVBYB[#B_;BcTBe`BiyBk�Bn�Bt�Bw�Bx�By�B{�B~�B�B�B�B�B�%B�+B�1B�7B�7B�=B�=B�DB�DB�\B�bB�bB�hB�uB��B��B��B��B��B��B��B��B��B�B�B�-B�?B�LB�XB�dB�}B��BBĜBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�5B�BB�mB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	%B	\B	hB	oB	uB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	,B	,B	-B	0!B	33B	5?B	5?B	49B	49B	49B	5?B	5?B	6FB	6FB	6FB	7LB	7LB	8RB	9XB	:^B	@�B	E�B	G�B	J�B	L�B	M�B	N�B	Q�B	Q�B	Q�B	T�B	YB	YB	[#B	\)B	]/B	^5B	_;B	_;B	_;B	`BB	aHB	bNB	cTB	dZB	e`B	ffB	iyB	q�B	r�B	t�B	u�B	v�B	z�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�%B	�1B	�=B	�=B	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�-B	�-B	�9B	�RB	�XB	�^B	�^B	�dB	�jB	�wB	�}B	��B	��B	��B	��B	��B	��B	B	ÖB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
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
JB
PB
VB
VB
\B
\B
\B
\B
bB
hB
hB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
.B
.B
.B
.B
/B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
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
:^B
:^B
;dB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
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
G�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
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
YB
YB
YB
ZB
ZB
ZB
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
\)B
\)B
]/B
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
cTB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
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
n�B
n�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
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
t�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�)B��B��B��B��B�B��B��B�B��B��B�
B��B�B�$B��B�B�DB�yB�sB��B�B��B��B��BcnB-B�B��B�+B�AB��B��B��B�CB�B��B��B��B��BB~�BpUBY�BNpBNpBR�BU�BP�BF�B>B4BB�B��B�]B��B�B��B��B�B�1B��B��B�3B��B��B�B��B�1Bs�Bk�B>�B(�B�B�B�B
�oB
��B
�B
��B
��B
�OB
��B
z^B
p�B
i�B
a�B
X�B
P�B
GB
8�B
+B
�B
B
�B
�B	��B	��B	��B	�B	��B	ѝB	��B	�JB	ňB	��B	�AB	��B	�xB	��B	�(B	�fB	.B	t9B	o B	g�B	c�B	_;B	[�B	X+B	S�B	N�B	LB	J=B	C�B	?.B	=�B	<B	:�B	8�B	7B	6`B	3B	3�B	4B	1'B	.B	*�B	'�B	&LB	$ZB	#�B	!�B	�B	B	�B	�B	�B	B	
�B	�B	mB	�B	aB	�B�6B�nB�GB�GB��B�oB�B�nB�!B�jB��B��BևB՛BԕBҽB�:B�pB��B�?B�B�MB�{B� B�B��B�B�B��B��B�OB�WB��B��B��B��B��B�pB�IB�]B�qB�1B��B�EB�B�2B�@B��B��B��B�B��B��B��B�B{JBxBv�Bu?Bq�Bm]Bh�Bf2Be�BdtBb4B`vB^�B\CBY�BX�BX�BX+BVSBS�BRBQNBN�BL�BJ�BIRBF�BF?BC-B@�B?}B>BB="B<B;dB:�B8�B8B7B7fB5�B4�B2|B0�B-wB+6B)�B(
B'�B'�B(�B)_B)�B(�B(�B'mB(sB'�B&�B%�B'RB&2B$�B$�B!-B"�B!bB�BSBBBB?B1B=B!HB#�B&LB&fB'�B(>B'mB'mB&�B'B($B(�B'�B'RB'RB'mB(�B(�B(
B*eB-�B-�B*�B1AB1vB/5B/B/�B3B5B8�B9�B;�B;�B<�B<�B<�B<�B=<B?}BCGBFBEmBGzBHfBIRBJ=BMBN"BP.BR:BRBR BR BR:BR:BSuBT�BV�BY�B[�B_�Bc�Bf2BjKBlWBo�ButBx8ByXBz^B|�B}B�uB�{B�gB��B��B��B��B�lB��B��B��B��B�0B��B��B��B��B��B��B��B�B��B��B�\B�4B�TB��B��B��B��B��B��B��B��B��B��B��B�B��B�B��B�	B�B�B�B�"B�B�B�B�B� B� B�:B�[BԯB��B޸B�B��B�B��B��B��B��B�B�B��B��B��B��B�B�B�B�B�$B�*B�0B�wB	 iB	[B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	jB	#nB	,=B	,=B	-�B	0�B	3�B	5�B	5�B	4�B	4�B	4nB	5tB	5tB	6zB	6�B	6�B	7�B	7�B	8�B	9�B	;B	@�B	E�B	G�B	J�B	M6B	N<B	O(B	RB	RB	R B	U2B	YeB	YB	[qB	\]B	]dB	^jB	_pB	_pB	_pB	`vB	a|B	b�B	c�B	d�B	e�B	f�B	jB	q�B	r�B	t�B	u�B	w2B	{0B	}<B	~(B	~(B	~BB	cB	�oB	�gB	�tB	�fB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�B	�2B	�8B	�*B	�KB	�0B	�QB	�WB	�cB	�OB	�[B	�aB	�aB	�aB	�aB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�pB	�:B	�,B	�MB	�gB	�sB	�7B	�WB	�WB	�CB	�CB	�CB	�CB	�]B	�~B	�dB	�jB	ބB	ߤB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�"B	�<B	�(B	�.B	�.B	�HB
 iB
;B
AB
AB
GB
GB
3B
MB
MB
SB
9B
SB
9B
SB
SB
mB
SB
mB
�B
	�B
	lB

XB

rB

XB

rB

rB
�B
xB
�B
~B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
B
�B
�B
B
 'B
!�B
!�B
"B
"B
# B
$@B
%,B
%B
%�B
&B
&B
%�B
&2B
($B
($B
)*B
)DB
)DB
*B
*B
*B
*B
*B
*B
*0B
*B
*B
)�B
*B
*0B
*B
*B
*B
*0B
*KB
+kB
,WB
-CB
.IB
.IB
.cB
.cB
/�B
1[B
1AB
1[B
2GB
2aB
3MB
33B
3MB
3hB
3hB
3�B
4nB
4TB
4TB
4nB
5�B
6zB
6zB
7�B
7�B
7�B
7�B
7fB
8lB
7�B
8�B
8�B
9rB
9�B
9�B
9�B
9�B
9rB
9rB
:�B
:�B
:�B
:�B
:�B
:�B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?}B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
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
G�B
G�B
G�B
IB
IB
I�B
I�B
J�B
KB
J�B
J�B
J�B
K�B
MB
MB
MB
NB
NB
OB
OB
OB
OB
OB
P.B
QB
QB
QB
R:B
R B
R B
R B
R B
S&B
S&B
S&B
T,B
T,B
TB
TB
UB
UB
TB
UB
UB
UB
U2B
U2B
U2B
V9B
VSB
WYB
X+B
X+B
XEB
XEB
X+B
X+B
X+B
Y1B
Y1B
Y1B
YKB
YKB
YKB
YKB
ZQB
ZQB
ZkB
[WB
[WB
[WB
\CB
\CB
\CB
\]B
\CB
\CB
\]B
\]B
\�B
]~B
^jB
_�B
_�B
`vB
`vB
`vB
`�B
a�B
aHB
a|B
bNB
b�B
b�B
b�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
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
n�B
n�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
s�B
s�B
s�B
tB
s�B
t�B
uB
t�B
t�B
t�B
t�B
t�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<4;@<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811050037462018110500374620181105003746202211182136442022111821364420221118213644201811060017422018110600174220181106001742  JA  ARFMdecpA19c                                                                20181026003516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181025153543  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181025153545  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181025153545  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181025153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181025153546  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181025153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181025153546  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181025153546  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181025153546                      G�O�G�O�G�O�                JA  ARUP                                                                        20181025155530                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181025153739  CV  JULD            G�O�G�O�F�\�                JM  ARGQJMQC2.0                                                                 20181025153739  CV  JULD_LOCATION   G�O�G�O�F�]                JM  ARGQJMQC2.0                                                                 20181025153739  CV  LATITUDE        G�O�G�O�A�t�                JM  ARGQJMQC2.0                                                                 20181025153739  CV  LONGITUDE       G�O�G�O�� �f                JM  ARCAJMQC2.0                                                                 20181104153746  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181104153746  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181105151742  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123644  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                