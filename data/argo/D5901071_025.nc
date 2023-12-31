CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:58Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135635  20190522121825  1727_5046_025                   2C  D   APEX                            2143                            040306                          846 @�6���1   @�6K��@7ix����c�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh��Bo33Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF�CH�CI��CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(y�D)  D)�fD*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@���@���A��A<��A[33A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW��B_33Bh  BnffBv��B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B�ffC��C��C��C�fC	�fC��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA�fCC�fCE�fCG�fCI��CK�3CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%��D&s3D&�3D's3D'�3D(l�D(�3D)y�D)�3D*l�D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@��DAy�DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq��Drs3Dr�3Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԾwA�A԰!Aԡ�Aԣ�AԍPA�dZA�+A�(�A�+A�%A��#A���A�ĜA���AӸRAӡ�AӋDA�v�A�9XA���Aҙ�A�`BA�-A�VA�A�A�z�A�`BA�bNA��mA�t�A���A��A���A���A�G�A�1A���A�ZA��yA�n�A�C�A�?}A� �A��!A��uA���A�l�A��/A�VA�ĜA���A��DA�r�A�33A�A��DA��
A��-A�hsA�33A�A�A�E�A���A�\)A�r�A�ĜA�G�A�1A���A�l�A���A�`BA��+A�l�A�K�A�A�A�bNA�~�A�ĜA�Q�A�I�A�
=A���A�"�A���A�  A�bA�1'A��A��A�l�A�r�A���A�XA���A��HA���A�`BA�Q�A��9A�&�A��9A�$�A�t�A��TA�JA�1A���A�oA��yA��wA�$�A��A�{A�bA���A�hsA�ĜA��A�|�A�+A��PA��A}K�A{&�Ax��AwC�AvI�At��As�Aq�wAo|�Anr�An$�Amx�AlI�Ai�wAh��Ae�
Ad=qAd�Ac�Ab��AaS�A_A^bA]��A\JA[��AY�#AWx�AT{AR=qAP�AP�!AQ33AQoAN�`ANbAL��AK�AI�^AI?}AH�jAG�PAF�+AE�FAC�PA@��A?��A?�FA?7LA=�-A<��A<�A;
=A9��A8ffA6�9A6�\A6A�A4�A4M�A3��A2(�A1%A/��A.��A-K�A,�yA,�HA,��A+�TA*�A*VA)\)A((�A'?}A%��A$�yA$A�A"I�A!/A�#AO�A��A�;AoAA�A��At�A��A^5AƨAx�AoA1'A�!A��A�FAhsA��A1AC�A�DAbA��A�AXA��Az�An�A1A�wA�PAXAn�A�A
�RA	�A�Ar�AE�A�-A/A�+A�A�7A�/A~�A^5At�A�A(�A��A ��A @�$�@��@�@�{@��9@�K�@�x�@�b@�=q@���@���@�w@���@��@��@�7@�P@�&�@㕁@�+@�V@��@�dZ@�G�@�ƨ@�O�@ו�@���@�@�z�@�~�@�%@�n�@͑h@�Z@�`B@���@�K�@Ɵ�@�J@��@�b@Õ�@§�@��u@��@�9X@�t�@�v�@��#@��7@�n�@�(�@�&�@���@�J@�$�@��@�/@�/@�&�@��9@�l�@��y@���@�J@�?}@���@�(�@�-@���@���@�p�@�?}@�%@���@�b@��!@��-@�(�@�t�@�@��R@���@�E�@��@���@�(�@�ƨ@�|�@��@�1@�K�@�
=@�5?@�(�@�+@��^@�ƨ@�C�@��@�=q@��7@��@���@��@���@���@�G�@��y@��+@��P@�S�@���@���@���@��@�%@�&�@�X@�J@���@��@��F@���@��@��D@�?}@��-@��@� �@�1'@��P@��y@��P@���@�\)@�o@�o@�
=@��y@�ȴ@���@�~�@�$�@���@�&�@���@��/@�%@��`@�j@���@��@���@�dZ@��@��y@�=q@���@�@���@���@��h@�`B@�7L@�&�@�G�@�/@��@��u@�z�@�j@�Q�@�9X@�  @��m@��
@��;@�ƨ@���@��P@�|�@�S�@�;d@�+@�"�@��@��@���@�5?@���@���@�p�@�p�@��@��`@��`@�z�@���@�l�@�l�@�"�@�V@���@�p�@�/@�p�@�$�@��#@���@� �@��F@�|�@�ƨ@��@���@�S�@�C�@�+@�+@��@��H@�~�@�M�@��#@��-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AԾwA�A԰!Aԡ�Aԣ�AԍPA�dZA�+A�(�A�+A�%A��#A���A�ĜA���AӸRAӡ�AӋDA�v�A�9XA���Aҙ�A�`BA�-A�VA�A�A�z�A�`BA�bNA��mA�t�A���A��A���A���A�G�A�1A���A�ZA��yA�n�A�C�A�?}A� �A��!A��uA���A�l�A��/A�VA�ĜA���A��DA�r�A�33A�A��DA��
A��-A�hsA�33A�A�A�E�A���A�\)A�r�A�ĜA�G�A�1A���A�l�A���A�`BA��+A�l�A�K�A�A�A�bNA�~�A�ĜA�Q�A�I�A�
=A���A�"�A���A�  A�bA�1'A��A��A�l�A�r�A���A�XA���A��HA���A�`BA�Q�A��9A�&�A��9A�$�A�t�A��TA�JA�1A���A�oA��yA��wA�$�A��A�{A�bA���A�hsA�ĜA��A�|�A�+A��PA��A}K�A{&�Ax��AwC�AvI�At��As�Aq�wAo|�Anr�An$�Amx�AlI�Ai�wAh��Ae�
Ad=qAd�Ac�Ab��AaS�A_A^bA]��A\JA[��AY�#AWx�AT{AR=qAP�AP�!AQ33AQoAN�`ANbAL��AK�AI�^AI?}AH�jAG�PAF�+AE�FAC�PA@��A?��A?�FA?7LA=�-A<��A<�A;
=A9��A8ffA6�9A6�\A6A�A4�A4M�A3��A2(�A1%A/��A.��A-K�A,�yA,�HA,��A+�TA*�A*VA)\)A((�A'?}A%��A$�yA$A�A"I�A!/A�#AO�A��A�;AoAA�A��At�A��A^5AƨAx�AoA1'A�!A��A�FAhsA��A1AC�A�DAbA��A�AXA��Az�An�A1A�wA�PAXAn�A�A
�RA	�A�Ar�AE�A�-A/A�+A�A�7A�/A~�A^5At�A�A(�A��A ��A @�$�@��@�@�{@��9@�K�@�x�@�b@�=q@���@���@�w@���@��@��@�7@�P@�&�@㕁@�+@�V@��@�dZ@�G�@�ƨ@�O�@ו�@���@�@�z�@�~�@�%@�n�@͑h@�Z@�`B@���@�K�@Ɵ�@�J@��@�b@Õ�@§�@��u@��@�9X@�t�@�v�@��#@��7@�n�@�(�@�&�@���@�J@�$�@��@�/@�/@�&�@��9@�l�@��y@���@�J@�?}@���@�(�@�-@���@���@�p�@�?}@�%@���@�b@��!@��-@�(�@�t�@�@��R@���@�E�@��@���@�(�@�ƨ@�|�@��@�1@�K�@�
=@�5?@�(�@�+@��^@�ƨ@�C�@��@�=q@��7@��@���@��@���@���@�G�@��y@��+@��P@�S�@���@���@���@��@�%@�&�@�X@�J@���@��@��F@���@��@��D@�?}@��-@��@� �@�1'@��P@��y@��P@���@�\)@�o@�o@�
=@��y@�ȴ@���@�~�@�$�@���@�&�@���@��/@�%@��`@�j@���@��@���@�dZ@��@��y@�=q@���@�@���@���@��h@�`B@�7L@�&�@�G�@�/@��@��u@�z�@�j@�Q�@�9X@�  @��m@��
@��;@�ƨ@���@��P@�|�@�S�@�;d@�+@�"�@��@��@���@�5?@���@���@�p�@�p�@��@��`@��`@�z�@���@�l�@�l�@�"�@�V@���@�p�@�/@�p�@�$�@��#@���@� �@��F@�|�@�ƨ@��@���@�S�@�C�@�+@�+@��@��H@�~�@�M�@��#@��-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�?B�FB�RB�XB�wB��BÖBŢBƨB��B�^B�XB�RB�XB�XB�jBĜB��B�B�;B�fB�yB�B�B��B��B�B�jB.Bt�BG�B�B9XB:^BC�BC�BL�Bn�B~�B��B��B��B��B�B�9B�?B�!B�RB�LB�3B�3B��B�BB�mB�fB�fB�ZB�`B�mB�B�B�B�mB�mB�ZB�B��B��B��BɺBŢB�wB�B��B�bB�Bt�BhsBZBK�B5?B(�B%�B!�B�BbB%B��B�yB�;B�BB�BɺB��B�jB�9B��B�uB~�B[#B8RB/B)�B�B�BVBB
�B
�TB
��B
�wB
�B
��B
��B
��B
��B
��B
��B
�hB
�B
{�B
t�B
iyB
VB
E�B
8RB
(�B
 �B
�B
PB
B	��B	�B	�B	�fB	�5B	��B	�?B	��B	�\B	�B	��B	��B	�uB	�+B	r�B	gmB	cTB	H�B	@�B	1'B	!�B	bB	
=B	%B		7B	 �B	$�B	�B	bB	%B��B��B��B�B�B�mB�HB�B��B��BɺBǮBB��B�qB�qB�jB�^B�RB�LB�?B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�DB�1B�+B�%B�B�B�B�B� B~�B}�B}�B|�B{�Bz�Bz�By�Bw�Bw�Bu�Bs�Br�Bp�Bm�BjBe`BdZBbNBaHBbNBaHBaHBaHBaHB_;B^5B]/B]/BZBZBXBVBT�BT�BR�BQ�BP�BN�BM�BL�BK�BJ�BJ�BI�BH�BH�BG�BG�BG�BG�BF�BD�BA�B>wB<jB;dB9XB7LB6FB5?B49B49B49B5?B6FB6FB6FB8RB8RB:^B:^B@�B?}B?}B?}B>wB?}B=qB<jB=qB<jBA�B@�B@�B@�BA�BB�BE�BH�BI�BB�B=qB=qBA�BE�BE�BE�BT�Bk�Bv�B{�B�B�JB��B��B��B��B��B��B��B��B��B�B�B�B�-B�FB�RB�XB�dB�jB�}BBĜBÖBƨBǮBȴBǮBǮBǮBȴB��B��B��BɺBɺB��B��BɺBƨBƨBÖB��B��BÖBĜBŢBƨBƨB��B��B�B�;B�TB�BB�HB�B�B��B	PB	bB	+B	B	B	1B	{B	�B	�B	�B	�B	�B	�B	'�B	/B	6FB	8RB	:^B	<jB	=qB	D�B	H�B	J�B	L�B	O�B	P�B	Q�B	R�B	S�B	W
B	XB	ZB	YB	[#B	^5B	aHB	cTB	dZB	dZB	dZB	dZB	e`B	ffB	gmB	iyB	jB	l�B	m�B	n�B	p�B	q�B	s�B	t�B	v�B	y�B	z�B	{�B	|�B	}�B	~�B	~�B	�B	�%B	�+B	�DB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�LB	�jB	�dB	�XB	�^B	�wB	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�9B�?B�FB�RB�XB�}BBÖBŢBǮB��B�^B�XB�RB�XB�XB�jBĜB��B�
B�BB�mB�B�B�B��B+B�#B��B:^B� BO�B'�B;dB<jBD�BD�BO�Bp�B�B��B��B��B�B�-B�LB�LB�FB�^B�^B�RB�9B��B�HB�sB�yB�B�`B�mB�sB�B�B�B�yB�B�B�;B�B��B��B��BɺBĜB�!B��B��B�+Bx�Bl�B]/BR�B:^B)�B'�B#�B�BuBDB��B�B�NB�TB�;B��BĜB�}B�RB�B��B�Be`B:^B1'B-B"�B�BoB%B
��B
�mB
�B
ĜB
�'B
��B
��B
��B
��B
��B
��B
��B
�+B
}�B
x�B
q�B
^5B
K�B
?}B
-B
#�B
�B
bB
	7B	��B	�B	�B	�sB	�NB	�B	�XB	�B	�{B	�%B	��B	��B	��B	�VB	u�B	hsB	iyB	J�B	E�B	9XB	,B	�B	VB	+B	1B	!�B	,B	�B	{B	JB	B��B��B��B�B�yB�mB�;B��B��B��B��BĜBÖB��B��B��B�}B�XB�RB�XB�'B�B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�\B�JB�7B�1B�1B�+B�B�B�B�B� B� B}�B}�B}�B� B|�Bx�Bx�Bw�Bu�Bt�Br�Bo�Bn�BhsBe`BdZBbNBbNBbNBbNBbNBbNBbNB`BBaHBaHB\)B\)BYBXBW
BW
BT�BS�BR�BP�BN�BO�BN�BL�BL�BM�BK�BL�BK�BI�BI�BJ�BI�BH�BD�BA�B=qB=qB<jB;dB9XB7LB7LB8RB9XB8RB7LB8RB9XB:^B;dB<jB>wBC�B@�BA�BA�BA�BA�BA�B>wB?}B@�BC�BA�BA�BA�BC�BD�BF�BJ�BM�BF�B@�B>wBC�BF�BF�BD�BQ�BjBu�B{�B�B�JB��B��B��B��B��B��B��B��B�B�B�'B�3B�3B�LB�RB�^B�jB�qB��BBƨBÖBȴBǮBɺBȴBȴBɺB��B��B��B��B��B��B��B��B��B��BȴBƨBĜB��BĜBŢBƨBǮBǮB��B��B�B�;B�mB�HB�BB�B�B��B	PB	�B	
=B	B	B	+B	�B	�B	�B	 �B	�B	�B	�B	'�B	0!B	7LB	8RB	;dB	=qB	<jB	D�B	I�B	K�B	L�B	O�B	P�B	Q�B	R�B	S�B	XB	YB	[#B	ZB	[#B	^5B	aHB	dZB	e`B	dZB	dZB	e`B	ffB	gmB	hsB	jB	k�B	l�B	m�B	n�B	q�B	q�B	s�B	t�B	v�B	z�B	z�B	{�B	|�B	}�B	~�B	� B	�B	�%B	�+B	�DB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�FB	�qB	�qB	�dB	�dB	�}B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446422012010314464220120103144642  AO  ARGQ                                                                        20111130135635  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144642  IP                  G�O�G�O�G�O�                