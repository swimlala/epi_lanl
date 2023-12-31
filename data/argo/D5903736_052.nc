CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:33Z AOML 3.0 creation; 2016-05-31T19:14:33Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230533  20160531121433  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               4A   AO  4051_7090_052                   2C  D   APEX                            5368                            041511                          846 @ֶ�[���1   @ֶ��� @4��t�j�e�+J1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    4A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dyy�D�3D�P D�i�D���D�fD�\�D���D�ɚD� D�C3D��fD��fD�fD�L�Dڐ D��D�fD�C3D�D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo��Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,y�D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^l�D^��D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3DtS3Dyl�D��D�I�D�c4D��gD�  D�VgD��gD��4D�	�D�<�D�� D�� D�  D�FgDډ�D�gD� D�<�D�4D�fg11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�ZA�ZA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�`BA�dZA�ffA�ffA�dZA�ffA�bNA�-A�C�A֗�A��#A�"�AЙ�A�-A��/A��A�ƨA�(�A�~�A�=qA� �Ağ�A�bA�ĜA�?}A�
=A��A�p�A��A�A�?}A�~�A�"�A��FA�=qA��/A��yA�jA��A�ƨA���A��A��\A���A�bA�ĜA�A�A� �A��A���A��7A�jA�33A���A�^5A���A�I�A��A���A�ȴA��A��PA�ZA��7A�VA���A�E�A�hsA�XA��A��A��A��^A�p�A�=qA��A��A��7A���A��jA��A}�TA|�A{��Az�Ay�TAxr�Aw33At�yAt~�AqoAm7LAi�7Ah5?Af��Ad��AbI�A`bA^�\A]�A[�
AZjAY�^AX�+AU�PAS�AQ��APQ�AN^5AM�7AK
=AH�\AD^5ACt�AB�AA�FA@ZA?+A>��A>9XA=G�A<��A:�9A9�#A8ĜA8(�A5�wA3�A3��A3G�A2�A2��A1�#A1�A1?}A1�A0��A/�7A.�uA-VA*��A*5?A)�wA)
=A(�A&9XA%l�A$jA"bA!VA�A �A  AK�Av�A�A1'A"�A��AbA��A��AjAG�Ar�A �AhsA1'A��A�HA�uA{AM�A�wAC�A
��A	�AĜA�
AhsA�wAv�A=qA1'A$�A��A��A ��A -@���@�%@��T@�Q�@��@�+@�X@��/@�j@��
@���@�D@�w@��y@�$�@���@�(�@��;@�o@�j@��@�=q@�Ĝ@ߍP@�~�@�{@��`@��@��@�&�@�Q�@�p�@�j@�S�@�C�@���@�^5@��T@���@�z�@�ƨ@θR@�V@�{@͙�@���@�Z@ˮ@˅@�33@ʟ�@�=q@ɑh@�j@��@��@�&�@öF@�n�@���@��@���@��@�@��m@���@���@��9@��;@���@�-@��#@��h@��9@��m@�;d@��y@��@�p�@�/@���@�I�@��P@���@�M�@�{@���@�`B@�V@�1@��w@��@�
=@��+@��@�7L@��@�  @�t�@�@���@�ff@�5?@��@�J@�@��@�x�@��@��@���@�A�@� �@��F@�
=@��R@���@��!@���@���@�n�@�V@�5?@�@��@��@��9@�1@��F@��F@���@���@��F@���@�S�@�v�@�{@��^@�O�@�/@�/@��@���@��@�%@���@��@��`@��@�%@���@��@��/@�r�@�  @���@��@�ff@�5?@�J@��T@���@��-@��h@�x�@�`B@�?}@��j@�Z@���@���@�S�@�o@�@���@�ff@�{@���@���@�@���@�hs@�/@��@��@�r�@�A�@�1@���@�K�@�"�@�@��@���@�ȴ@��@��H@��@�
=@��H@���@�n�@�E�@�$�@�@���@�?}@�%@���@��9@�Q�@���@��w@���@��P@��@�l�@�K�@��@��+@�ff@�=q@�5?@�J@�@�G�@���@��;@��@���@�dZ@�K�@�C�@�;d@�33@�o@���@���@�n�@�@��^@�x�@�/@���@�Z@� �@��
@��P@��@��y@��R@���@���@��+@�v�@�M�@���@���@�`B@�X@�?}@���@��9@�bN@�9X@��@�1@��;@��w@���@�\)@��R@�V@�$�@��7@�O�@�?}@�%@��j@��@���@���@~ȴ@t�/@j�@cS�@[��@X �@Q�#@M?}@F{@>5?@65?@-?}@)%@#ƨ@v�@�@��@p�@	�@|�@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\)A�ZA�ZA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�`BA�dZA�ffA�ffA�dZA�ffA�bNA�-A�C�A֗�A��#A�"�AЙ�A�-A��/A��A�ƨA�(�A�~�A�=qA� �Ağ�A�bA�ĜA�?}A�
=A��A�p�A��A�A�?}A�~�A�"�A��FA�=qA��/A��yA�jA��A�ƨA���A��A��\A���A�bA�ĜA�A�A� �A��A���A��7A�jA�33A���A�^5A���A�I�A��A���A�ȴA��A��PA�ZA��7A�VA���A�E�A�hsA�XA��A��A��A��^A�p�A�=qA��A��A��7A���A��jA��A}�TA|�A{��Az�Ay�TAxr�Aw33At�yAt~�AqoAm7LAi�7Ah5?Af��Ad��AbI�A`bA^�\A]�A[�
AZjAY�^AX�+AU�PAS�AQ��APQ�AN^5AM�7AK
=AH�\AD^5ACt�AB�AA�FA@ZA?+A>��A>9XA=G�A<��A:�9A9�#A8ĜA8(�A5�wA3�A3��A3G�A2�A2��A1�#A1�A1?}A1�A0��A/�7A.�uA-VA*��A*5?A)�wA)
=A(�A&9XA%l�A$jA"bA!VA�A �A  AK�Av�A�A1'A"�A��AbA��A��AjAG�Ar�A �AhsA1'A��A�HA�uA{AM�A�wAC�A
��A	�AĜA�
AhsA�wAv�A=qA1'A$�A��A��A ��A -@���@�%@��T@�Q�@��@�+@�X@��/@�j@��
@���@�D@�w@��y@�$�@���@�(�@��;@�o@�j@��@�=q@�Ĝ@ߍP@�~�@�{@��`@��@��@�&�@�Q�@�p�@�j@�S�@�C�@���@�^5@��T@���@�z�@�ƨ@θR@�V@�{@͙�@���@�Z@ˮ@˅@�33@ʟ�@�=q@ɑh@�j@��@��@�&�@öF@�n�@���@��@���@��@�@��m@���@���@��9@��;@���@�-@��#@��h@��9@��m@�;d@��y@��@�p�@�/@���@�I�@��P@���@�M�@�{@���@�`B@�V@�1@��w@��@�
=@��+@��@�7L@��@�  @�t�@�@���@�ff@�5?@��@�J@�@��@�x�@��@��@���@�A�@� �@��F@�
=@��R@���@��!@���@���@�n�@�V@�5?@�@��@��@��9@�1@��F@��F@���@���@��F@���@�S�@�v�@�{@��^@�O�@�/@�/@��@���@��@�%@���@��@��`@��@�%@���@��@��/@�r�@�  @���@��@�ff@�5?@�J@��T@���@��-@��h@�x�@�`B@�?}@��j@�Z@���@���@�S�@�o@�@���@�ff@�{@���@���@�@���@�hs@�/@��@��@�r�@�A�@�1@���@�K�@�"�@�@��@���@�ȴ@��@��H@��@�
=@��H@���@�n�@�E�@�$�@�@���@�?}@�%@���@��9@�Q�@���@��w@���@��P@��@�l�@�K�@��@��+@�ff@�=q@�5?@�J@�@�G�@���@��;@��@���@�dZ@�K�@�C�@�;d@�33@�o@���@���@�n�@�@��^@�x�@�/@���@�Z@� �@��
@��P@��@��y@��R@���@���@��+@�v�@�M�@���@���@�`B@�X@�?}@���@��9@�bN@�9X@��@�1@��;@��w@���@�\)@��R@�V@�$�@��7@�O�@�?}@�%@��j@��@���@���@~ȴ@t�/@j�@cS�@[��@X �@Q�#@M?}@F{@>5?@65?@-?}@)%@#ƨ@v�@�@��@p�@	�@|�@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB49B-BB��B�B��B��B�Bz�Bt�Bn�BiyBdZB[#BS�BP�BO�BL�BJ�BI�BA�B:^B9XB8RB5?B1'B+B�BuBPB1BBB��B��B�B�5B��B��B��BĜB�RB�B��B��B��B�BK�B49B(�B$�B!�B�B
=B�B�NB�qB��B�%Br�BcTBR�B.B
��B
�`B
�/B
��B
��B
�B
��B
��B
�VB
{�B
VB
;dB
0!B
&�B
 �B
�B
uB

=B
B	�B	�yB	��B	�^B	�B	��B	��B	��B	�%B	z�B	r�B	m�B	dZB	\)B	W
B	O�B	B�B	9XB	2-B	,B	#�B	�B	oB	%B��B��B�B�B�mB�TB�BB�5B�#B�
B��B��B��BȴBBB��B��B��B��BÖBŢBɺB��BɺBɺBƨB�}B�^B�RB�FB�3B�B��B��B��B��B��B�hB�PB�=B�1B�B�B�B~�B}�B|�B{�By�Bw�Bu�Bt�Bs�Bq�Bp�Bo�Bn�Bm�Bk�BjBiyBhsBgmBe`Be`BdZBbNBbNBbNBbNBaHB`BB^5B^5B^5B]/B\)B[#B[#BZBZB\)B\)B]/B\)B\)B[#B]/B]/B]/B]/B^5B^5B^5B]/B`BBbNBaHBbNBcTBdZBdZBe`BiyBjBjBiyBn�Bo�Bo�Bo�Bo�Bp�Bp�Br�Bs�Bw�B|�B}�B}�B}�B}�B}�B� B�B�B�B�B�B�7B�PB�\B�bB�uB��B��B��B��B��B��B��B��B�B�!B�'B�FB�dB��B��BƨB��B��B��B��B��B�B�B�B�#B�HB�ZB�`B�yB�B�B�B�B�B��B��B��B	B	1B	JB	bB	oB	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	&�B	(�B	)�B	-B	1'B	2-B	33B	49B	5?B	7LB	8RB	8RB	8RB	9XB	:^B	;dB	;dB	@�B	E�B	E�B	F�B	G�B	K�B	R�B	Q�B	O�B	R�B	S�B	W
B	ZB	[#B	[#B	\)B	bNB	hsB	n�B	p�B	q�B	s�B	u�B	w�B	x�B	x�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�DB	�VB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�FB	�RB	�^B	�^B	�jB	��B	��B	ÖB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�HB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
+B
+B
+B
1B
	7B
	7B
	7B

=B
PB
�B
"�B
(�B
/B
49B
:^B
?}B
D�B
J�B
P�B
T�B
YB
\)B
aHB
ffB
jB
p�B
v�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B6bB6bB6bB6bB6bB6^B6bB6bB6cB6cB6^B6^B6cB6eB6cB6cB6_B6cB6_B6cB6cB6cB6cB4TB-(BB��B�B��B��B�8Bz�Bt�Bn�Bi�BdtB[9BTBP�BO�BL�BJ�BI�BA�B:qB9mB8fB5TB1;B+B�B�BeBFB1B!B�B��B�B�IB�B��B��BĬB�fB�B��B��B��B�"BK�B4IB)
B$�B!�B�B
NB�B�_B��B��B�8Br�BciBSB.+B
��B
�xB
�FB
�B
��B
�+B
��B
��B
�oB
|B
VB
;~B
0=B
'B
 �B
�B
�B

[B
"B	��B	�B	�
B	�B	�(B	�
B	��B	��B	�CB	{ B	r�B	m�B	dB	\LB	W,B	PB	B�B	9|B	2SB	,,B	#�B	�B	�B	MB��B��B��B�B�B�}B�iB�^B�LB�2B�B�B��B��B¸BºB��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�rB�]B�HB�*B�B��B��B��B��B�~B�jB�^B�MB�@B�5B)B~B}B|Bz	Bw�Bu�Bt�Bs�Bq�Bp�Bo�Bn�Bm�Bk�Bj�Bi�Bh�Bg�Be�Be�Bd�Bb}Bb|Bb}Bb|BawB`rB^eB^dB^dB]\B\YB[RB[SBZNBZMB\YB\YB][B\XB\YB[QB]]B]^B]\B]`B^fB^cB^eB]]B`rBbBaxBb~Bc�Bd�Bd�Be�Bi�Bj�Bj�Bi�Bn�Bo�Bo�Bo�Bo�Bp�Bp�Br�Bs�Bw�B}B~!B~"B~!B~ B~!B�.B�AB�EB�NB�GB�MB�fB�}B��B��B��B��B��B��B��B��B��B��B�B�7B�LB�TB�rB��B��B��B��B��B�B�B�(B�'B�0B�<B�;B�NB�rB�B�B�B�B��B��B��B��B��B�B�"B	:B	ZB	rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	'B	)B	*$B	-4B	1NB	2SB	3[B	4_B	5hB	7sB	8vB	8xB	8|B	9}B	:�B	;�B	;�B	@�B	E�B	E�B	F�B	G�B	K�B	SB	RB	PB	SB	TB	W2B	ZBB	[IB	[KB	\QB	bsB	h�B	n�B	p�B	q�B	s�B	u�B	w�B	x�B	x�B	{B	|B	}B	!B	�-B	�)B	�(B	�*B	�+B	�-B	�3B	�1B	�7B	�8B	�NB	�gB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�#B	�5B	�DB	�RB	�VB	�`B	�hB	�uB	��B	��B	��B	��B	��B	÷B	��B	��B	�B	�B	�B	�B	�B	�-B	�1B	�@B	�KB	�QB	�XB	�XB	�^B	�[B	�]B	�jB	�{B	�|B	�|B	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	�B	�B
 !B
  B
 !B
 "B
&B
'B
'B
&B
3B
9B
9B
MB
KB
MB
SB
	WB
	VB
	WB

`B
pB
�B
"�B
)B
/:B
4ZB
:}B
?�B
D�B
J�B
QB
UB
Y6B
\JB
afB
f�B
j�B
p�B
v�B
{ B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214332016053112143320160531121433  AO  ARCAADJP                                                                    20140721230533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230533  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230533  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121433  IP                  G�O�G�O�G�O�                