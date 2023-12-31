CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:59Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135720  20190522121825  1727_5046_029                   2C  D   APEX                            2143                            040306                          846 @�;+f5��1   @�;,F?�@7u?|�h�c���
=q1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH��BN��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D��D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/fD/� D0  D0� D1  D1� D1��D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@���@���A��A<��A\��A{33A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/��B733B?33BH  BN  BV��B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B�ffB�ffB˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE�3CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�fCi��Ck��Cm��Co��Cq��Cs��Cu��Cw�fCy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Dy�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D�3Dl�D��Ds3D�3Ds3D�3Ds3D��D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$l�D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,��D-s3D-�3D.s3D.��D/s3D/�3D0s3D0�3D1s3D1��D2l�D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dql�Dq�3Drs3Dr�3Dsff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�%A�%A�%A�1A�1A�%A�%A�A���A�A�A�%A�1A�%A�%A�%A�A�1A�JA�VA�
=A�VA�bA�~�Aɗ�A���AÇ+AA�v�A�{A�-A��A�9XA��/A�O�A���A��A���A�S�A���A��TA�/A���A��A�`BA�5?A�I�A��DA��9A�hsA�"�A���A��A�\)A�  A��9A��7A�v�A�XA��A���A��!A���A�|�A�C�A�&�A��;A���A�x�A�S�A�7LA�bA��A�ffA�1'A�VA��;A��-A�`BA��A��FA��\A�hsA� �A���A�ZA�bA��jA���A�|�A�M�A��A�n�A�S�A�G�A�5?A��A���A�VA�x�A���A��uA�VA��9A��!A�(�A���A�p�A�VA�ĜA�XA��A��7A���A��A���A�E�A��TA�ffA���A�ZA�  A��A�S�A��^A�I�A��A�z�A�S�A�K�A��A��mA��A��#A��A���A��RA�C�A���A�K�A��-A��7A}VA{�PAz �Ay��Ax��Ax��Axn�AvVAtn�Aq\)Ao�TAm�;Akl�Ai��Ag��Ae�FAe�AeXAeG�Ad��Ac�#Ab�HAb��Aa�#A`1'A^�9A]�wA[�-AZJAXM�AVA�AT�+AR��ANȴAI�AH��AE33AAAA�A@��A@�A>�A<�`A:  A8��A8�+A6��A6r�A61A5ƨA5t�A533A5
=A4�A4�\A3C�A2�A0�!A/K�A.�!A.-A+��A)dZA(��A(~�A'��A&�uA&1'A%��A%O�A$��A#G�A"��A!��A!�-A!`BA�AVA��AA�A��A��A�wAG�AVA��A��A�`AO�A�A�uA%AȴA5?A��A�HA9XAp�A
��A	�^A�yA�
AQ�A��A��A�At�Al�AdZAG�A�`A�+A�A�jA �@��#@�(�@��^@�b@�hs@�@�;d@��@�E�@���@�u@��@�z�@�@�p�@��D@�  @��H@ݲ-@�(�@�;d@ڗ�@�$�@��@Լj@�dZ@ҟ�@�S�@ԃ@�1@���@�t�@�%@Չ7@�@�M�@�hs@Լj@ӍP@ӍP@Ӆ@ҟ�@�p�@�V@�ƨ@��@�%@�j@�"�@���@� �@��@Ĭ@öF@�33@�;d@�ȴ@�n�@��^@���@���@�+@�v�@���@���@��P@�;d@�@��y@��@��!@�~�@��@��h@�V@��@��@��@�C�@�o@�ȴ@�ff@�@�@�X@���@��@�{@��T@��h@�?}@��9@�ƨ@�v�@���@�/@���@���@��D@�r�@�Q�@�dZ@�M�@���@���@���@�ƨ@�J@�X@���@��@�Z@��;@�ff@���@�V@���@�I�@��@��@�\)@���@���@�@���@�/@��9@�A�@��w@�t�@�S�@�33@�+@�+@�"�@��@�@���@�@�`B@���@��/@���@�bN@�  @���@�t�@�;d@�@���@�5?@���@�O�@���@�j@�A�@� �@��w@�l�@�
=@��\@�V@��@���@�hs@�7L@�V@��@�1'@��
@��@�|�@�\)@�C�@�o@��H@��+@�v�@�v�@�M�@��@�?}@�%@���@��u@��@�z�@�bN@�1@��F@�dZ@�+@��@��y@��H@��H@��@�ȴ@��!@���@��\@�v�@�=q@�$�@�J@���@��@���@���@��D@�9X@�b@��w@��@�\)@�S�@���@�^5@�E�@�=q@��@�{@�J@��@�@���@�x�@�G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�%A�%A�%A�1A�1A�%A�%A�A���A�A�A�%A�1A�%A�%A�%A�A�1A�JA�VA�
=A�VA�bA�~�Aɗ�A���AÇ+AA�v�A�{A�-A��A�9XA��/A�O�A���A��A���A�S�A���A��TA�/A���A��A�`BA�5?A�I�A��DA��9A�hsA�"�A���A��A�\)A�  A��9A��7A�v�A�XA��A���A��!A���A�|�A�C�A�&�A��;A���A�x�A�S�A�7LA�bA��A�ffA�1'A�VA��;A��-A�`BA��A��FA��\A�hsA� �A���A�ZA�bA��jA���A�|�A�M�A��A�n�A�S�A�G�A�5?A��A���A�VA�x�A���A��uA�VA��9A��!A�(�A���A�p�A�VA�ĜA�XA��A��7A���A��A���A�E�A��TA�ffA���A�ZA�  A��A�S�A��^A�I�A��A�z�A�S�A�K�A��A��mA��A��#A��A���A��RA�C�A���A�K�A��-A��7A}VA{�PAz �Ay��Ax��Ax��Axn�AvVAtn�Aq\)Ao�TAm�;Akl�Ai��Ag��Ae�FAe�AeXAeG�Ad��Ac�#Ab�HAb��Aa�#A`1'A^�9A]�wA[�-AZJAXM�AVA�AT�+AR��ANȴAI�AH��AE33AAAA�A@��A@�A>�A<�`A:  A8��A8�+A6��A6r�A61A5ƨA5t�A533A5
=A4�A4�\A3C�A2�A0�!A/K�A.�!A.-A+��A)dZA(��A(~�A'��A&�uA&1'A%��A%O�A$��A#G�A"��A!��A!�-A!`BA�AVA��AA�A��A��A�wAG�AVA��A��A�`AO�A�A�uA%AȴA5?A��A�HA9XAp�A
��A	�^A�yA�
AQ�A��A��A�At�Al�AdZAG�A�`A�+A�A�jA �@��#@�(�@��^@�b@�hs@�@�;d@��@�E�@���@�u@��@�z�@�@�p�@��D@�  @��H@ݲ-@�(�@�;d@ڗ�@�$�@��@Լj@�dZ@ҟ�@�S�@ԃ@�1@���@�t�@�%@Չ7@�@�M�@�hs@Լj@ӍP@ӍP@Ӆ@ҟ�@�p�@�V@�ƨ@��@�%@�j@�"�@���@� �@��@Ĭ@öF@�33@�;d@�ȴ@�n�@��^@���@���@�+@�v�@���@���@��P@�;d@�@��y@��@��!@�~�@��@��h@�V@��@��@��@�C�@�o@�ȴ@�ff@�@�@�X@���@��@�{@��T@��h@�?}@��9@�ƨ@�v�@���@�/@���@���@��D@�r�@�Q�@�dZ@�M�@���@���@���@�ƨ@�J@�X@���@��@�Z@��;@�ff@���@�V@���@�I�@��@��@�\)@���@���@�@���@�/@��9@�A�@��w@�t�@�S�@�33@�+@�+@�"�@��@�@���@�@�`B@���@��/@���@�bN@�  @���@�t�@�;d@�@���@�5?@���@�O�@���@�j@�A�@� �@��w@�l�@�
=@��\@�V@��@���@�hs@�7L@�V@��@�1'@��
@��@�|�@�\)@�C�@�o@��H@��+@�v�@�v�@�M�@��@�?}@�%@���@��u@��@�z�@�bN@�1@��F@�dZ@�+@��@��y@��H@��H@��@�ȴ@��!@���@��\@�v�@�=q@�$�@�J@���@��@���@���@��D@�9X@�b@��w@��@�\)@�S�@���@�^5@�E�@�=q@��@�{@�J@��@�@���@�x�@�G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bu�Bv�Bv�Bk�BW
BS�BW
BYBgmBhsBs�B{�B~�B�B�7B�\B��B��B�B�RBǮB��B��B��B�NB�B��BB	7BVBPB{B{B�B�B�B�B!�B"�B%�B'�B'�B+B.B.B1'B33B49B49B49B33B5?B5?B6FB7LB7LB7LB7LB7LB7LB7LB6FB6FB49B33B1'B0!B/B.B,B(�B$�B#�B#�B!�B �B�BoB1B  B��B��B�yB�B��B��BŢB�wB�RB�!B��B��B�hB�B}�Bs�BjB]/B:^B2-B(�B�BDB��B�B�B�PBT�B-BDB
�TB
��B
�dB
��B
�DB
� B
y�B
s�B
jB
^5B
K�B
=qB
33B
,B
)�B
'�B
&�B
!�B
�B
B	�B	�fB	�)B	��B	ĜB	�LB	�!B	�9B	�XB	�dB	�jB	�^B	�LB	�FB	�9B	�'B	�B	��B	��B	�oB	�%B	u�B	e`B	O�B	#�B	  B�B�BÖB��B�wB�dB�FB�'B�!B�'B�!B�3B�3B�?B�RB�^B�^B�^B�XB�LB�?B�'B�B�B��B��B��B��B��B��B�{B�uB�uB�oB�hB�VB�PB�DB�=B�=B�+B�=B�B�B�B�B~�B}�B|�Bw�Bt�Bo�Bk�BjBhsBe`BaHB`BB_;B^5B\)B[#BYBW
BVBR�BO�BM�BM�BM�BL�BL�BL�BK�BK�BI�BG�BD�B?}B:^B8RB5?B49B2-B/B/B,B+B)�B(�B%�B �B$�B$�B$�B#�B"�B!�B �B �B"�B"�B$�B#�B#�B+B0!B:^BE�BG�BK�BP�BhsBu�B~�B�+B�%B�%B�7B�JB�hB��B��B��B��B��B��B��B��B��B�{B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�?B�?B�?B�FB�FB�LB�LB�XB�XB�^B�XB�^B�^B�dB�dB�dB�qB�}BŢBƨBǮBǮBǮBȴB��B��B��B��B��B��B��B��B��B��B�B�
B�B�/B�fB�yB�B�B�B�B��B��B	  B	B	B	%B	1B	
=B	VB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	)�B	-B	.B	0!B	2-B	5?B	8RB	9XB	=qB	A�B	D�B	F�B	G�B	I�B	K�B	N�B	S�B	T�B	W
B	YB	[#B	[#B	[#B	^5B	aHB	cTB	ffB	hsB	jB	k�B	n�B	o�B	q�B	q�B	q�B	s�B	v�B	y�B	|�B	� B	�B	�+B	�1B	�7B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�'B	�FB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�wB	�}B	��B	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bu�Bt�Bu�Bu�Bw�Bz�Bv�BaHBYB\)B_;Bk�Bm�Bv�B}�B�B�%B�PB��B��B��B�'B�jBȴB��B��B�B�TB�B��B%B
=B\B\B�B�B�B�B�B �B#�B$�B&�B(�B(�B,B/B0!B2-B49B5?B5?B5?B5?B6FB6FB7LB8RB8RB8RB9XB8RB8RB8RB7LB9XB5?B5?B33B1'B0!B/B.B,B%�B#�B#�B"�B"�B�B�BDBB��B��B�B�/B��B��BȴB��B�dB�3B�B��B��B�+B�Bv�Bn�BhsB<jB49B,B!�BVB��B�HB�3B��B\)B33B{B
�fB
�B
B
��B
�\B
�B
{�B
w�B
n�B
e`B
XB
B�B
7LB
.B
,B
(�B
(�B
(�B
 �B
VB	��B	�B	�ZB	�
B	��B	�wB	�-B	�9B	�^B	�qB	�}B	�qB	�RB	�XB	�XB	�?B	�B	�B	��B	��B	�DB	y�B	jB	ZB	0!B	B��B�NBŢB��B��B�}B�jB�XB�3B�3B�?B�9B�9B�FB�XB�dB�dB�dB�dB�dB�XB�FB�'B�B��B��B��B��B��B��B��B�{B��B�uB�uB�oB�\B�PB�DB�DB�JB�JB�%B�B�B�B�B� B� B|�By�Bu�Bo�Bk�BiyBiyBgmBbNBaHB`BB^5B]/B\)BZBYBW
BT�BO�BN�BN�BL�BL�BL�BL�BM�BK�BI�BI�BG�B@�B;dB5?B8RB7LB2-B0!B1'B/B,B,B,B)�B'�B'�B&�B$�B$�B$�B#�B"�B#�B#�B'�B(�B%�B,B/B9XBF�BH�BL�BN�BhsBu�B~�B�7B�+B�1B�7B�JB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B�B�B�!B�9B�?B�?B�?B�FB�FB�LB�LB�RB�RB�^B�^B�dB�^B�dB�dB�jB�jB�wB�}B�}BƨBǮBȴBǮBɺB��B��B��B��B��B��B��B��B��B�B�B�
B�B�#B�BB�mB�B�B�B�B�B��B��B	  B	B	B	+B		7B	DB	\B	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	'�B	+B	.B	/B	1'B	33B	6FB	9XB	:^B	>wB	B�B	D�B	F�B	H�B	J�B	L�B	O�B	T�B	VB	XB	ZB	\)B	\)B	\)B	_;B	bNB	cTB	gmB	hsB	jB	l�B	o�B	p�B	q�B	q�B	q�B	t�B	w�B	z�B	}�B	�B	�B	�+B	�1B	�=B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�-B	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�wB	��B	��B	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446442012010314464420120103144644  AO  ARGQ                                                                        20111130135720  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135720  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144644  IP                  G�O�G�O�G�O�                