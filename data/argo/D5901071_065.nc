CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:09Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               AA   AO  20111130140604  20190522121826  1727_5046_065                   2C  D   APEX                            2143                            040306                          846 @�i��6?�1   @�i�q� @7]�E���c�ě��T1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���@���A��A<��A\��A{33A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B�ffB���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�3CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��3C��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D ��D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-��D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWl�DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De��Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Dz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�1A�{A��A��A�bA��A��A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�-A�-A�/A�1'A�/A�1'A�1'A�1'A�1'A�33A�33A�5?A�7LA�7LA�7LA�7LA��A�oA�
=A�
=A�%A�
=A�bA�JA��TA��RA��hA�
=A�\)A���A�hsA�/A�A��A�p�A�1A�v�A�"�A���A��hA���A�
=A��`A���A��^A��RA��A���A�l�A�I�A�  A��DA��7A���A�I�A���A���A�v�A��A�;dA�33A��DA�VA�VA�bA���A�{A�dZA��TA��
A�ƨA���A�Q�A�G�A�C�A��A��FA��A��`A���A���A�{A���A��RA�VA��RA��mA�33A��\A�A�&�A�{A���A���A���A�^5A��`A�`BA� �A��A��TA�t�A�ĜA�VA�ZA�M�A��DA���A��A�A
=A~9XA| �AxJAu�At^5As33Ap��Ao�TAo�;Ao;dAn�RAn�uAnVAm�;Ak�Aj5?Ah��Ah-Af��AeƨAb��AadZA`1'A_�-A_
=A\1'AZAX��AV�AV$�AU�wAT�yAS��AQƨAOO�AM�AM"�AL��AL �AK�
AK�AK
=AI��AGVAEADE�AC�;AC?}ABbNA@z�A?��A>�\A=�A=G�A<�uA;�wA;33A:�`A:�A9?}A8��A7ƨA6z�A5��A5S�A5"�A4ĜA3�;A3C�A2�HA2��A1�7A/��A.I�A-ƨA,�HA*�A)�A)
=A(�9A((�A'�A&�DA%`BA$bA#;dA"�\A!��A �yA A�A�mA��AG�AG�A��A��A�A�uA�;AdZA��A�mA��AoA�A�PA�HA�RA�Ax�A�A�9An�AA
~�A	dZA	/A��A�wA&�AVAAQ�A;dA�uAS�AA�AA�AVA �!A �@��#@��u@� �@��R@�^5@�%@�;d@�  @�l�@���@�9X@�A�@�F@�hs@�ƨ@���@�M�@��@�A�@��H@�bN@�1@��@�j@ߍP@�@�b@ܬ@�1'@۶F@�$�@�r�@��;@���@�@�X@Ցh@���@ԓu@�O�@мj@��@�J@���@�Q�@�1@�C�@���@��@�o@�n�@���@�O�@ě�@�1'@�ƨ@�K�@���@\@�x�@���@�bN@�Z@���@�dZ@��@�x�@��
@��;@��@��@�ff@�5?@�p�@���@�7L@���@�|�@���@��!@�E�@��;@�l�@�+@��+@�G�@���@�~�@���@�J@��j@�A�@��@���@�r�@�ff@�\)@��#@�(�@�  @���@��;@�;d@��/@�X@�$�@��@�\)@��@��H@��y@�\)@�"�@���@�v�@�-@�$�@�&�@�%@�&�@��@��!@��@��@�j@�+@�S�@�+@�@���@�ff@���@���@�-@�V@���@�~�@��#@���@�`B@��@��@�Q�@�A�@�(�@�1@��;@��@��@��@��@��@�1@�1'@�l�@�o@�@�o@��@��y@���@�ff@��@���@��-@���@��h@�p�@�G�@���@��D@� �@�1@��F@�l�@�;d@��y@��@��+@�V@�E�@�=q@�5?@�-@��@��@��^@��7@�7L@��@���@��@��@�r�@�1'@���@��
@�ƨ@��@�"�@��@���@���@��\@�n�@��@��^@���@�x�@�`B@�7L@��@�%@��/@��9@��D@�(�@��F@���@��P@�dZ@�;d@��@�@��y@��H@���@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bA�1A�{A��A��A�bA��A��A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�-A�-A�/A�1'A�/A�1'A�1'A�1'A�1'A�33A�33A�5?A�7LA�7LA�7LA�7LA��A�oA�
=A�
=A�%A�
=A�bA�JA��TA��RA��hA�
=A�\)A���A�hsA�/A�A��A�p�A�1A�v�A�"�A���A��hA���A�
=A��`A���A��^A��RA��A���A�l�A�I�A�  A��DA��7A���A�I�A���A���A�v�A��A�;dA�33A��DA�VA�VA�bA���A�{A�dZA��TA��
A�ƨA���A�Q�A�G�A�C�A��A��FA��A��`A���A���A�{A���A��RA�VA��RA��mA�33A��\A�A�&�A�{A���A���A���A�^5A��`A�`BA� �A��A��TA�t�A�ĜA�VA�ZA�M�A��DA���A��A�A
=A~9XA| �AxJAu�At^5As33Ap��Ao�TAo�;Ao;dAn�RAn�uAnVAm�;Ak�Aj5?Ah��Ah-Af��AeƨAb��AadZA`1'A_�-A_
=A\1'AZAX��AV�AV$�AU�wAT�yAS��AQƨAOO�AM�AM"�AL��AL �AK�
AK�AK
=AI��AGVAEADE�AC�;AC?}ABbNA@z�A?��A>�\A=�A=G�A<�uA;�wA;33A:�`A:�A9?}A8��A7ƨA6z�A5��A5S�A5"�A4ĜA3�;A3C�A2�HA2��A1�7A/��A.I�A-ƨA,�HA*�A)�A)
=A(�9A((�A'�A&�DA%`BA$bA#;dA"�\A!��A �yA A�A�mA��AG�AG�A��A��A�A�uA�;AdZA��A�mA��AoA�A�PA�HA�RA�Ax�A�A�9An�AA
~�A	dZA	/A��A�wA&�AVAAQ�A;dA�uAS�AA�AA�AVA �!A �@��#@��u@� �@��R@�^5@�%@�;d@�  @�l�@���@�9X@�A�@�F@�hs@�ƨ@���@�M�@��@�A�@��H@�bN@�1@��@�j@ߍP@�@�b@ܬ@�1'@۶F@�$�@�r�@��;@���@�@�X@Ցh@���@ԓu@�O�@мj@��@�J@���@�Q�@�1@�C�@���@��@�o@�n�@���@�O�@ě�@�1'@�ƨ@�K�@���@\@�x�@���@�bN@�Z@���@�dZ@��@�x�@��
@��;@��@��@�ff@�5?@�p�@���@�7L@���@�|�@���@��!@�E�@��;@�l�@�+@��+@�G�@���@�~�@���@�J@��j@�A�@��@���@�r�@�ff@�\)@��#@�(�@�  @���@��;@�;d@��/@�X@�$�@��@�\)@��@��H@��y@�\)@�"�@���@�v�@�-@�$�@�&�@�%@�&�@��@��!@��@��@�j@�+@�S�@�+@�@���@�ff@���@���@�-@�V@���@�~�@��#@���@�`B@��@��@�Q�@�A�@�(�@�1@��;@��@��@��@��@��@�1@�1'@�l�@�o@�@�o@��@��y@���@�ff@��@���@��-@���@��h@�p�@�G�@���@��D@� �@�1@��F@�l�@�;d@��y@��@��+@�V@�E�@�=q@�5?@�-@��@��@��^@��7@�7L@��@���@��@��@�r�@�1'@���@��
@�ƨ@��@�"�@��@���@���@��\@�n�@��@��^@���@�x�@�`B@�7L@��@�%@��/@��9@��D@�(�@��F@���@��P@�dZ@�;d@��@�@��y@��H@���@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�fB�/B�
B�B�)B�BB�fB�B�B�mB�fB�mB�`B�;B�)B�/B�/B�/B�)B�5B�ZB�mB�sB�sB�`B�HB�)B�B�B�/B�#B��BƨB�FB�B��B��B�oBw�Bq�Bn�Bq�Bt�Bv�B�+B��B��B��B��B��B��B��B��B�JBx�Bt�Bl�B`BBP�B>wBhB��Bl�BcTBbNB`BB^5B[#BVBM�B<jB(�BB
�B
�#B
��B
ȴB
��B
�B
��B
��B
�B
x�B
s�B
l�B
W
B
;dB
-B
'�B
�B
oB
VB
VB

=B
	7B
	7B
+B
B	��B	��B	�B	�B	�B	�HB	��B	ǮB	�FB	�B	��B	�\B	�B	z�B	r�B	o�B	m�B	iyB	dZB	^5B	P�B	N�B	C�B	A�B	D�B	I�B	M�B	L�B	H�B	>wB	8RB	49B	2-B	/B	,B	#�B	�B	�B	�B	�B	{B	\B	\B	JB		7B	B	  B��B��B��B��B�B�B�B�B�sB�`B�)B��B��B��B��BƨB��B��B�}B�qB�^B�RB�9B�B�B��B��B��B��B��B��B��B��B�bB�\B�VB�VB�DB�=B�7B�=B�1B�%B�B~�B|�B{�Bw�Bv�Bu�Bu�Bu�Bw�Bt�Bo�Bw�B�B�B�%B�+B�%B�Bz�Bv�Br�Bs�Bu�Bp�Bo�Bl�BgmBdZBhsBv�Bx�B{�Bw�Bq�BgmBgmBe`B`BBW
BM�BJ�BG�BG�BF�BF�BN�Bo�B~�B�B}�Bz�B|�Bz�B}�B�B�1B�7B�VB�bB�bB�oB��B��B�-B�?B�LB�9B�FB�?B�?B�9B�FB�LB�XB��B��BƨB��BɺBɺBɺB��B��B��B��B��BɺB��B��B��B��B��B��B��BɺB��B��B�B�yB�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B��B	  B��B�B�B	  B	hB	\B	
=B	%B	B	B	B	+B	VB	hB	JB	B��B�B�B��B��B��B	B	B	B	B	1B	PB	\B	uB	�B	!�B	&�B	.B	.B	-B	,B	33B	5?B	7LB	8RB	>wB	B�B	F�B	F�B	I�B	K�B	L�B	N�B	N�B	N�B	M�B	O�B	P�B	P�B	P�B	Q�B	P�B	P�B	Q�B	R�B	YB	\)B	^5B	`BB	`BB	aHB	bNB	bNB	cTB	e`B	ffB	hsB	jB	k�B	l�B	l�B	l�B	m�B	n�B	p�B	r�B	u�B	u�B	w�B	y�B	y�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�PB	�\B	�oB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�FB	�FB	�FB	�LB	�LB	�^B	�wB	�}B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ƨB	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�;B�B�#B�/B�HB�sB�B�B�yB�sB�sB�yB�NB�/B�/B�/B�/B�/B�HB�fB�sB�B�B�B�fB�5B�/B�#B�;B�/B��B��B�XB�B�B�B��Bz�Bu�Bp�Bq�Bt�Bw�B�1B��B��B��B��B��B��B��B��B�hBy�Bu�Bn�BcTBVBH�B6FB�}Bp�BdZBcTBaHB_;B]/BYBVBA�B49BJB
�B
�5B
��B
��B
ŢB
�3B
��B
��B
�%B
{�B
v�B
r�B
cTB
D�B
/B
,B
&�B
{B
VB
bB
JB

=B

=B
	7B
	7B
B	��B	��B	�B	�B	�sB	��B	��B	�LB	�'B	�B	�{B	�%B	~�B	t�B	p�B	o�B	l�B	jB	e`B	T�B	P�B	E�B	B�B	E�B	J�B	O�B	P�B	O�B	B�B	<jB	5?B	49B	2-B	1'B	&�B	"�B	�B	�B	�B	�B	hB	bB	PB	VB	B	B��B��B��B��B��B��B�B�B�yB�B�TB�B��B��B��B��BÖBB��B��B�wB�qB�XB�-B�B�B��B��B��B��B��B��B��B�uB�oB�\B�bB�PB�JB�JB�DB�=B�JB�+B�B�B� Bw�Bw�Bv�Bv�Bw�B{�Bx�Bp�By�B�%B�B�+B�1B�7B�%B}�Bz�Bv�Bt�Bx�Bp�Bq�Bn�Bk�BffBiyBy�By�B~�B{�Bv�BhsBiyBjBgmB^5BP�BL�BH�BH�BH�BG�BK�Bm�B� B�B� B{�B� B|�B}�B�%B�7B�JB�VB�hB�oB�{B��B��B�3B�FB�jB�?B�RB�XB�LB�?B�LB�XB�jBÖBÖBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B�
B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	B��B	B��B�B�B��B	uB	oB	VB	1B	%B	B	B	+B	\B	{B	hB	%B��B�B��B��B��B��B	B	B	B	%B	1B	\B	\B	uB	�B	 �B	&�B	1'B	0!B	/B	,B	33B	5?B	7LB	9XB	>wB	B�B	G�B	F�B	I�B	K�B	M�B	O�B	O�B	O�B	N�B	O�B	P�B	P�B	P�B	Q�B	P�B	P�B	Q�B	R�B	YB	\)B	^5B	aHB	aHB	aHB	bNB	bNB	cTB	e`B	gmB	iyB	jB	k�B	l�B	l�B	l�B	m�B	o�B	q�B	r�B	u�B	v�B	x�B	z�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�VB	�bB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�9B	�FB	�FB	�FB	�RB	�RB	�dB	�wB	�}B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ƨB	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=t�<�/<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446562012010314465620120103144656  AO  ARGQ                                                                        20111130140604  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140604  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144656  IP                  G�O�G�O�G�O�                