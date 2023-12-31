CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-18T09:15:51Z AOML 3.0 creation; 2016-05-26T23:45:56Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151018091551  20160526164556  5903726 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4045_7089_131                   2C  D   APEX                            5372                            041511                          846 @�w�6;�'1   @�w������C�=p��
@D���O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D��D�33D��fD��fD���D�)�D���D��fD� D�S3D�vfDǣ3D��D�VfDچfD�3D��3D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�34@���@�fgA��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�33A�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=l�D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt� Dy�3D�gD�,�D�� D�� D��4D�#4D��gD�� D�	�D�L�D�p Dǜ�D�gD�P Dڀ D��D���D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��uA��uA���A��A��A���A���A���A���A���A���A��hA�v�A��A�^5A��DA�VA�?}A�&�A�
=A��/A���A��PA�l�A�C�A�"�A�oA�A��mA���A�ȴA��RA���A�l�A�XA�=qA��mA���A�dZA�M�A�7LA��A�oA�JA�  A���A��A��mA��#A�ƨA��A���A���A��+A�z�A�x�A�t�A�t�A�t�A�t�A�n�A�ffA�^5A�\)A�\)A�XA�O�A�K�A�C�A�&�A�
=A���A��A���A���A��^A���A���A��hA�hsA�M�A�7LA�VA�ĜA�n�A�JA���A�ZA�$�A��A�x�A��wA��hA�~�A�I�A�VA��`A��RA��7A��A��wA�$�A���A���A�(�A�"�A�%A�ĜA�$�A���A���A�ZA�/A���A��7A�(�At�A~�!A}A};dA|�yA|ĜA|��A|�A{��Az�`Azz�AzA�Az(�Ay�TAy`BAx�Ax��AxAw"�Av�RAv(�Au�-AuK�At��At �Asp�As
=Ar��ArĜArbNAq��Ap�jAp5?AoXAnAm��Am;dAl�AlbAj��Ajv�Ai�;AihsAh��AhbAg�Af^5Ae��Ad�AdE�AdJAcƨAc��AcS�AcVAb��Ab�9AbjAbJAa�;Aa�-Aa�Aa�A`ȴA`��A`ZA`1'A`(�A`(�A_�
A_��A_l�A_S�A^��A^�+A^A]�A]�7A]/A]%A\�uA\1A[��A[�-A[`BA[G�AZ��AZ5?AY�;AY��AX��AXjAX �AW�#AW&�AV�HAV�uAVA�AU��AU
=ATĜATv�AS�AS\)AR�/AR��ARbNAQ��AQ&�AP�AP1AO�wAO;dAN�AN�AM�;AMC�AL�/AL�jALjALAK�PAKC�AK%AJ��AJ�+AI��AI�hAIO�AH��AH��AHVAG�AG`BAG�AF�`AF��AFZAE�TAE��AES�AE
=ADjADA�AD{ACdZAB�/AB�\ABZAB�AA�AAƨAA��A@�A@�A?��A?hsA?oA>bNA=�#A=�A="�A<��A<E�A;|�A:�RA9��A97LA8�\A7�A7��A7dZA7�A6�A6VA6-A61A5�A4��A4�+A4VA4{A3��A3;dA2�`A2ZA2-A1�
A1��A133A0��A0��A0VA0{A/��A/�TA/�TA/�wA/XA/C�A/�A.��A.ffA.M�A. �A-A-�hA-O�A-&�A-%A,�A,A�A, �A, �A+�mA+?}A+A*��A*=qA*A)�A)�-A)&�A(�A(�DA(�+A(�+A(v�A({A'�A'��A'l�A'/A&�HA&��A&jA&5?A%��A%;dA%A$�A$�DA$r�A$�A#
=A"ĜA"~�A!�#A!A v�Ax�AoA�A-A�
Al�A(�A��A��AE�A��AC�AQ�A�AO�A�A��AƨA7LA�!A-A�TA�hA�yA�!A�DAA|�A33A�`A�DA�;A�A�+A-A"�A�yA�uAVA(�A�A�;A�;A��Al�A
��A
�9A
-A	�hA	;dA�RAZAXA��A  A�wA/AȴAv�A�A��AjA$�A�;A��A7LA �A ĜA ��A {@��w@�O�@�V@���@�p�@�7L@�j@�dZ@���@�%@���@�bN@��@�@�S�@��H@���@��@�I�@���@�h@��@�|�@�~�@�V@�M�@�7L@�(�@�+@�^@�(�@���@��@��@��m@�dZ@�$�@�V@܋D@�(�@��m@�l�@��y@��@؃@��;@�\)@���@�n�@���@�X@ԋD@ǶF@��\@��F@��\@�j@�hs@��^@�@pb@d�/@_��@WK�@P�9@Lz�@H1'@BJ@>$�@:-@7�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111A���A��uA��uA���A��A��A���A���A���A���A���A���A��hA�v�A��A�^5A��DA�VA�?}A�&�A�
=A��/A���A��PA�l�A�C�A�"�A�oA�A��mA���A�ȴA��RA���A�l�A�XA�=qA��mA���A�dZA�M�A�7LA��A�oA�JA�  A���A��A��mA��#A�ƨA��A���A���A��+A�z�A�x�A�t�A�t�A�t�A�t�A�n�A�ffA�^5A�\)A�\)A�XA�O�A�K�A�C�A�&�A�
=A���A��A���A���A��^A���A���A��hA�hsA�M�A�7LA�VA�ĜA�n�A�JA���A�ZA�$�A��A�x�A��wA��hA�~�A�I�A�VA��`A��RA��7A��A��wA�$�A���A���A�(�A�"�A�%A�ĜA�$�A���A���A�ZA�/A���A��7A�(�At�A~�!A}A};dA|�yA|ĜA|��A|�A{��Az�`Azz�AzA�Az(�Ay�TAy`BAx�Ax��AxAw"�Av�RAv(�Au�-AuK�At��At �Asp�As
=Ar��ArĜArbNAq��Ap�jAp5?AoXAnAm��Am;dAl�AlbAj��Ajv�Ai�;AihsAh��AhbAg�Af^5Ae��Ad�AdE�AdJAcƨAc��AcS�AcVAb��Ab�9AbjAbJAa�;Aa�-Aa�Aa�A`ȴA`��A`ZA`1'A`(�A`(�A_�
A_��A_l�A_S�A^��A^�+A^A]�A]�7A]/A]%A\�uA\1A[��A[�-A[`BA[G�AZ��AZ5?AY�;AY��AX��AXjAX �AW�#AW&�AV�HAV�uAVA�AU��AU
=ATĜATv�AS�AS\)AR�/AR��ARbNAQ��AQ&�AP�AP1AO�wAO;dAN�AN�AM�;AMC�AL�/AL�jALjALAK�PAKC�AK%AJ��AJ�+AI��AI�hAIO�AH��AH��AHVAG�AG`BAG�AF�`AF��AFZAE�TAE��AES�AE
=ADjADA�AD{ACdZAB�/AB�\ABZAB�AA�AAƨAA��A@�A@�A?��A?hsA?oA>bNA=�#A=�A="�A<��A<E�A;|�A:�RA9��A97LA8�\A7�A7��A7dZA7�A6�A6VA6-A61A5�A4��A4�+A4VA4{A3��A3;dA2�`A2ZA2-A1�
A1��A133A0��A0��A0VA0{A/��A/�TA/�TA/�wA/XA/C�A/�A.��A.ffA.M�A. �A-A-�hA-O�A-&�A-%A,�A,A�A, �A, �A+�mA+?}A+A*��A*=qA*A)�A)�-A)&�A(�A(�DA(�+A(�+A(v�A({A'�A'��A'l�A'/A&�HA&��A&jA&5?A%��A%;dA%A$�A$�DA$r�A$�A#
=A"ĜA"~�A!�#A!A v�Ax�AoA�A-A�
Al�A(�A��A��AE�A��AC�AQ�A�AO�A�A��AƨA7LA�!A-A�TA�hA�yA�!A�DAA|�A33A�`A�DA�;A�A�+A-A"�A�yA�uAVA(�A�A�;A�;A��Al�A
��A
�9A
-A	�hA	;dA�RAZAXA��A  A�wA/AȴAv�A�A��AjA$�A�;A��A7LA �A ĜA ��A {@��w@�O�@�V@���@�p�@�7L@�j@�dZ@���@�%@���@�bN@��@�@�S�@��H@���@��@�I�@���@�h@��@�|�@�~�@�V@�M�@�7L@�(�@�+@�^@�(�@���@��@��@��m@�dZ@�$�@�V@܋D@�(�@��m@�l�@��y@��@؃@��;@�\)@���@�n�@���@�XG�O�@ǶF@��\@��F@��\@�j@�hs@��^@�@pb@d�/@_��@WK�@P�9@Lz�@H1'@BJ@>$�@:-@7�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{B{B{B{BuBuBuBuBuBuB{BuB{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B!�B!�B"�B"�B"�B$�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B!�B!�B �B �B!�B �B �B�B�B�B'�B-B(�B$�B&�B(�B)�B"�B�B�B�B�B+B0!B/B-B(�B%�B$�B"�B �B�B�B�B�BuBbBVBPBJBDB	7B+BBBB  B��B��B��B��B�B�B�B�B�yB�mB�`B�HB�/B�#B�B�B�B��B��BǮB��B�^B�RB�9B�'B�B��B��B��B��B��B��B�uB�JB�%B�B}�B~�B~�B�B�B�B� B~�B}�B|�B{�Bz�Bx�Bv�Bt�Br�Bq�Bp�Bo�Bo�Bn�Bm�Bk�BjBhsBe`BbNBaHB_;B\)B[#BW
BQ�BQ�BM�BI�BG�BC�B>wB:^B8RB33B.B,B)�B#�B �B�B�B�B\BJB
=BB  B��B��B��B�B�sB�HB�/B�#B�
B��B��BɺBĜB��B��B�wB�^B�?B�3B�'B�B�B��B��B��B��B��B��B��B�bB�VB�JB�7B�+B�B� B|�By�Bt�Br�Bp�BjBe`BbNB`BB^5B]/B[#BYBR�BN�BJ�BF�BC�B>wB9XB6FB33B.B)�B#�B�B�B\B	7BBB��B��B��B��B�B�B�B�mB�`B�ZB�HB�;B�B�B��B��B��B��BǮBÖBB��B�qB�jB�jB�dB�^B�LB�FB�9B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�7B�1B�%B�B}�B|�B|�B|�B{�Bx�Bv�Bs�Bq�Bo�Bl�BjBiyBffBbNB_;B]/B[#BZBXBT�BL�BI�BG�BB�B<jB8RB0!B-B)�B%�B"�B �B�B{BhBJB1BB
��B
��B
��B
��B
�B
�B
�sB
�`B
�NB
�;B
�/B
�B
�B
�
B
��B
��B
��B
��B
��B
ŢB
��B
�wB
�jB
�FB
�?B
�3B
�-B
�'B
�!B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�{B
�bB
�JB
�DB
�+B
�B
�B
~�B
x�B
v�B
t�B
s�B
q�B
n�B
l�B
jB
hsB
cTB
_;B
R�B
L�B
O�B
N�B
N�B
M�B
K�B
I�B
F�B
F�B
E�B
D�B
C�B
C�B
C�B
A�B
?}B
;dB
9XB
6FB
33B
0!B
.B
-B
,B
(�B
%�B
"�B
�B
�B
�B
�B
{B
hB
\B
DB
1B
%B
%B
B
B
B	��B	��B	��B	��B	��B	��B	��B	�B
B	ǮB	��B	�B	��B	�9B	�B	�/B	�HB	��B
{B
1'B
A�B
S�B
dZB
w�B
�uB
��B
�?B
ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B!�B!�B"�B"�B"�B$�B&B&B&B'B'B'B'B'B'B'B'B'B'	B&B%�B%�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B!�B!�B �B �B!�B �B �B�B�B�B(B-,B)B$�B'B)B*B"�B�B�B�B�B+B0@B/=B-(B)B%�B$�B"�B �B�B�B�B�B�B�ByBlBeB`B	UBJB4B)B!B B�B�B��B��B��B��B�B�B�B�B�zB�fB�MB�AB�1B�1B� B��B��B��B��B�yB�mB�WB�@B�6B�B�B�B��B��B��B��B�eB�=B� B~
BBB�$B�*B�'B�BB~B}B|Bz�Bx�Bv�Bt�Br�Bq�Bp�Bo�Bo�Bn�Bm�Bk�Bj�Bh�BezBbdBabB_UB\@B[;BW#BRBRBM�BI�BG�BC�B>�B:xB8lB3LB.,B,"B*B#�B �B�B�B�BvBaB
UB6B B��B��B��B�B�B�aB�GB�:B�"B��B��B��BĶB��B��B��B�vB�YB�HB�?B�2B�!B�B��B��B��B��B��B��B�yB�mB�dB�LB�BB�%B�B}By�Bt�Br�Bp�Bj�BesBbcB`VB^KB]DB[=BY,BSBN�BJ�BF�BC�B>�B9jB6]B3IB.+B*B#�B�B�BrB	KB/BB�B�B��B��B��B��B�B�B�vB�qB�_B�TB�3B�B�B��B��B��B��BìB§B��B��B��B��B�}B�uB�cB�\B�OB�=B�+B�%B�B�B�B��B��B��B��B��B��B��B��B��B�zB�gB�[B�NB�IB�<B�B~B}B}B}B{�Bx�Bv�Bs�Bq�Bo�Bl�Bj�Bi�Bf|BbfB_TB]FB[;BZ3BX(BUBL�BI�BG�BB�B<�B8lB0:B-)B*B%�B"�B �B�B�B�BcBJB6B
�B
��B
��B
��B
��B
�B
�B
�zB
�hB
�TB
�HB
�3B
�)B
�%B
�B
�B
��B
��B
��B
żB
��B
��B
��B
�cB
�XB
�LB
�JB
�BB
�<B
�6B
�6B
�/B
� B
�B
�B
��B
��B
��B
��B
��B
��B
�|B
�gB
�^B
�EB
�<B
�/B
B
x�B
v�B
t�B
s�B
q�B
n�B
l�B
j�B
h�B
cpB
_XB
SB
L�B
O�B
N�B
N�B
M�B
K�B
I�B
F�B
F�B
E�B
D�B
C�B
C�B
C�B
A�B
?�B
;�B
9sB
6cB
3QB
0?B
.2B
--B
,%B
)B
&B
"�B
�B
�B
�B
�B
�B
�B
{B
dB
QB
DB
EB
>B
1B
&B	�B	�B	��B	��B	��B	��B	��B	��G�O�B	��B	��B	�6B	�B	�ZB	�4B	�OB	�iB	��B
�B
1EB
A�B
TB
dwB
w�B
��B
��B
�ZB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605261645562016052616455620160526164556  AO  ARCAADJP                                                                    20151018091551    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151018091551  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151018091551  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160526164556  IP                  G�O�G�O�G�O�                