CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:07Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               9A   AO  20111130140358  20190522121826  1727_5046_057                   2C  D   APEX                            2143                            040306                          846 @�_Vtn�1   @�_WW_�@72n��O��c��S���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI�fDJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�&fD�S3D�� D���D�#3D�` D�� D�ٚD��D�0 D���D��3D�#3D�Y�DڦfD��3D�&fD�P D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B��B33B33B'33B/33B7��B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ffB���B���BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C�fC��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�3Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	��D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5y�D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGy�DG�3DHs3DH�3DIy�DI�3DJs3DJ�3DKs3DK��DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSy�DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3DtffDy� D�  D�L�D���D��fD��D�Y�D���D��3D�fD�)�D��fD���D��D�S3Dڠ D���D�  D�I�D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ĜA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A���A���A���A��
A���A��jA��A��A���A���A���A��hA��DA��+A��+A��A�x�A�r�A�ZA�;dA�bA�XA��FA�A���A�z�A�VA�x�A� �A���A�ĜA��yA��wA��hA� �A��+A�A�A��A��PA��!A�VA��;A���A�ƨA���A�A�A���A��
A�r�A��#A�%A���A��DA���A�XA���A��PA�A�bA��TA�n�A��A���A�r�A�"�A���A��jA�ZA���A�dZA��\A���A�{A��/A�;dA�A���A��yA�/A��A�K�A�A~��A}t�Az��AxffAw�Av�!AvM�Au|�Ar{Am�hAlv�Ak|�AjQ�Ai��Ag|�Ae�FAd��Ad1'Aa�A_+A\��AW�AS�ASl�ASVAQ%AO�-AOl�AM�FAJĜAI��AH�!AFbNAD9XAA�A@I�A?�PA?%A>��A>v�A>(�A=�A=x�A;l�A9hsA6��A5�;A5hsA4��A3��A2A1�A0�!A0�A/�A-ƨA-33A,M�A*�yA*�9A*��A)��A)p�A(Q�A'��A&ȴA%l�A$ �A"��A"5?A!�A ĜA�A�A�A33AZA��A~�A �A`BA�A�A�AE�AE�A�A�A��A?}AoA�A��A��AA33A
�A	��A	"�A�`AĜAbNA��A��A`BA�A�jA��A�/A �A�!AbAAhsA �y@���@�Z@��
@�=q@�&�@�|�@��T@�5?@�A�@�ȴ@�9@ꟾ@�/@�n�@䛦@��@���@�o@���@�\@��T@�1'@�~�@܋D@���@�ff@�M�@��@���@�X@�V@�b@�n�@�&�@�X@�33@��@�A�@ˍP@�+@���@ʏ\@�E�@Ɂ@�G�@�G�@��@�n�@�hs@�&�@��m@�@���@�X@�r�@��@��F@��w@�ƨ@��@�t�@�O�@��@�x�@���@�5?@ũ�@���@���@���@�I�@�bN@�bN@�I�@�(�@��@���@�b@�b@�1'@��@�v�@§�@���@���@�C�@�Ĝ@�\)@�p�@�\)@��T@�Ĝ@�~�@�G�@�Q�@��P@���@�-@���@�V@�ƨ@��@��@��y@��@�j@�^5@��@��w@�E�@�z�@�1@�r�@��D@�r�@���@��@�;d@��^@��@�I�@�  @�t�@�  @�-@�@�b@��
@��@�@��@��@���@��@��y@���@��F@���@�-@���@��@���@���@���@���@���@��`@��
@�|�@�
=@�5?@��-@���@�O�@�z�@�$�@� �@�t�@�@�v�@��@�p�@�G�@�%@��@�z�@� �@�b@�b@�  @�  @�  @�  @���@�S�@��#@��@��@���@��-@��h@�`B@�?}@�V@�Ĝ@�A�@�1@��
@��w@�ƨ@��F@�"�@��H@��R@���@��\@��+@�v�@�n�@�E�@�5?@�$�@���@��-@���@���@��h@��h@�hs@��@���@��`@��/@���@��@��D@�z�@�bN@�I�@�1'@�b@��m@���@�ƨ@��F@���@��P@��@�t�@�K�@���@�~�@�@��^@���@���@��7@�G�@�7L@��@��j@�z�@�bN@�I�@� �@�1@���@��@��m@���@�t�@�;d@�+@�@��@���@��+@�n�@�^5@�^5@�M�@�-@��@��h@�/@���@��@}��@u��@k33@gK�@^$�@W\)@N�@HbN@A�^@:-@7K�@2�!@-�@'�w@"n�@�/@�w@��@�+@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ĜA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A���A���A���A��
A���A��jA��A��A���A���A���A��hA��DA��+A��+A��A�x�A�r�A�ZA�;dA�bA�XA��FA�A���A�z�A�VA�x�A� �A���A�ĜA��yA��wA��hA� �A��+A�A�A��A��PA��!A�VA��;A���A�ƨA���A�A�A���A��
A�r�A��#A�%A���A��DA���A�XA���A��PA�A�bA��TA�n�A��A���A�r�A�"�A���A��jA�ZA���A�dZA��\A���A�{A��/A�;dA�A���A��yA�/A��A�K�A�A~��A}t�Az��AxffAw�Av�!AvM�Au|�Ar{Am�hAlv�Ak|�AjQ�Ai��Ag|�Ae�FAd��Ad1'Aa�A_+A\��AW�AS�ASl�ASVAQ%AO�-AOl�AM�FAJĜAI��AH�!AFbNAD9XAA�A@I�A?�PA?%A>��A>v�A>(�A=�A=x�A;l�A9hsA6��A5�;A5hsA4��A3��A2A1�A0�!A0�A/�A-ƨA-33A,M�A*�yA*�9A*��A)��A)p�A(Q�A'��A&ȴA%l�A$ �A"��A"5?A!�A ĜA�A�A�A33AZA��A~�A �A`BA�A�A�AE�AE�A�A�A��A?}AoA�A��A��AA33A
�A	��A	"�A�`AĜAbNA��A��A`BA�A�jA��A�/A �A�!AbAAhsA �y@���@�Z@��
@�=q@�&�@�|�@��T@�5?@�A�@�ȴ@�9@ꟾ@�/@�n�@䛦@��@���@�o@���@�\@��T@�1'@�~�@܋D@���@�ff@�M�@��@���@�X@�V@�b@�n�@�&�@�X@�33@��@�A�@ˍP@�+@���@ʏ\@�E�@Ɂ@�G�@�G�@��@�n�@�hs@�&�@��m@�@���@�X@�r�@��@��F@��w@�ƨ@��@�t�@�O�@��@�x�@���@�5?@ũ�@���@���@���@�I�@�bN@�bN@�I�@�(�@��@���@�b@�b@�1'@��@�v�@§�@���@���@�C�@�Ĝ@�\)@�p�@�\)@��T@�Ĝ@�~�@�G�@�Q�@��P@���@�-@���@�V@�ƨ@��@��@��y@��@�j@�^5@��@��w@�E�@�z�@�1@�r�@��D@�r�@���@��@�;d@��^@��@�I�@�  @�t�@�  @�-@�@�b@��
@��@�@��@��@���@��@��y@���@��F@���@�-@���@��@���@���@���@���@���@��`@��
@�|�@�
=@�5?@��-@���@�O�@�z�@�$�@� �@�t�@�@�v�@��@�p�@�G�@�%@��@�z�@� �@�b@�b@�  @�  @�  @�  @���@�S�@��#@��@��@���@��-@��h@�`B@�?}@�V@�Ĝ@�A�@�1@��
@��w@�ƨ@��F@�"�@��H@��R@���@��\@��+@�v�@�n�@�E�@�5?@�$�@���@��-@���@���@��h@��h@�hs@��@���@��`@��/@���@��@��D@�z�@�bN@�I�@�1'@�b@��m@���@�ƨ@��F@���@��P@��@�t�@�K�@���@�~�@�@��^@���@���@��7@�G�@�7L@��@��j@�z�@�bN@�I�@� �@�1@���@��@��m@���@�t�@�;d@�+@�@��@���@��+@�n�@�^5@�^5@�M�@�-@��@��h@�/@���@��@}��@u��@k33@gK�@^$�@W\)@N�@HbN@A�^@:-@7K�@2�!@-�@'�w@"n�@�/@�w@��@�+@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Br�Br�Br�Bs�Bs�Bs�Bt�Bu�Bv�Bu�Bv�By�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�Bx�Bx�Bw�Bv�Bt�Bv�B�B�1B�1B�1B�7B�JB�PB�JB�DB�7B�=B�7B�1B�+B�+B�B�B{�Bx�Bv�Bv�Bu�Bs�Bm�BdZBYBG�B>wB1'B�BPBB��B�B�sB�5B��B�9B��B�=B}�Bu�Bq�BO�B2-B�B
��B
�fB
�
B
��B
�}B
�B
��B
�%B
r�B
`BB
5?B
�B
1B	��B	��B	�yB	�
B	��B	B	�}B	�dB	�-B	��B	|�B	u�B	n�B	gmB	aHB	T�B	J�B	F�B	?}B	;dB	(�B	�B��B��B��B�B�yB�fB�`B�5B��B��B��BŢB�qB�RB�FB�?B�?B�9B�3B�-B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B��B��B��B��B��B�{B�bB�VB�\B�bB�VB�+B�B�B}�B|�B{�B|�B|�B{�Bz�Bv�Bo�Bk�BhsBhsBgmBe`BbNBbNBaHBaHB`BB_;B_;B`BB`BB`BBbNBbNB^5BZBVBVBT�BS�BR�BN�BL�BL�BL�BK�BF�BA�B>wB>wB=qB:^B7LB5?B49B49B33B33B33B2-B2-B0!B2-B49B5?B7LB6FB5?B49B33B2-B1'B.B.B+B-B+B+B-B.B/B/B0!B0!B49B6FB7LB;dB>wB=qB<jB=qB>wBA�BB�BK�BN�BT�B[#BbNBs�B�bB��B��B��B�B�3B�FB�'B�B�B�B�!B�-B�-B�3B�9B�wBÖBÖBĜBȴB�
B�
B��B��BŢB�qB�XB�9B�B�B��B��B��B��B��B��B��B��B��B�{B�{B�{B��B��B��B�LB�jBB�B�yB	B	B	B	B	%B	%B	B	B	B	B	B	B��B��B	PB	�B	�B	�B	�B	VB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	%�B	"�B	 �B	#�B	'�B	+B	)�B	'�B	-B	/B	33B	33B	/B	(�B	(�B	(�B	,B	1'B	33B	5?B	7LB	8RB	9XB	=qB	B�B	C�B	C�B	C�B	D�B	F�B	H�B	J�B	K�B	R�B	VB	VB	VB	W
B	W
B	XB	YB	YB	[#B	dZB	gmB	hsB	hsB	iyB	jB	o�B	q�B	s�B	u�B	u�B	u�B	v�B	v�B	x�B	y�B	z�B	{�B	� B	�B	�B	�B	�B	�B	�7B	�=B	�DB	�JB	�PB	�PB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�?B	�XB	�dB	�jB	�wB	��B	B	B	ÖB	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	��B	��B
\B
�B
#�B
-B
2-B
:^B
C�B
F�B
K�B
R�B
XB
^5B
cTB
iyB
n�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Bq�Br�Bq�Bq�Bq�Br�Br�Br�Bs�Bs�Bs�Bt�Bu�Bv�Bu�Bw�By�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�Bx�By�Bx�Bw�B}�B�=B�PB�=B�7B�=B�DB�PB�VB�PB�VB�hB�\B�DB�DB�1B�7B�+B�+B�1By�Bv�Bv�Bv�Bu�Bt�BjBaHBK�BC�B7LB#�B\B%B��B�B�B�NB��B�dB��B�PB�Bw�Bx�BZB9XB�BB
�B
�B
��B
ŢB
�B
��B
�JB
w�B
n�B
<jB
'�B
DB
B	��B	�B	�;B	��B	ÖB	��B	�wB	�jB	��B	� B	x�B	q�B	iyB	ffB	YB	L�B	H�B	E�B	A�B	/B	%�B	B��B��B��B�B�mB�B�fB�
B��B��B��BƨB�dB�RB�LB�FB�?B�9B�3B�3B�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�B��B��B��B��B��B��B��B�hB�hB�{B��B�=B�1B�%B� B}�B{�B}�B}�B|�B}�B}�Bq�Bo�Bl�BjBjBhsBe`BdZBbNBbNBbNBaHBaHBaHBaHBbNBe`BffBaHB_;BYBW
BVBVBVBR�BM�BN�BN�BM�BH�BF�BA�B@�B@�B=qB9XB9XB7LB5?B49B49B49B33B33B33B5?B7LB7LB8RB6FB6FB5?B49B33B33B1'B0!B1'B1'B-B.B.B/B0!B0!B1'B2-B5?B6FB8RB?}B@�B>wB>wB?}B@�BB�BB�BL�BO�BT�B[#BaHBp�B�VB��B��B��B�B�9B�^B�3B�B�B�B�!B�-B�-B�9B�9B�wBÖBÖBĜBŢB�
B�B��B��BɺB��B�jB�RB�-B�B�B��B��B��B��B��B��B��B��B��B�{B�{B��B��B��B�FB�dB�wB��B�TB	B	B	B	%B	%B	+B	1B	%B	B	B	B	+B��B��B	DB	�B	�B	�B	�B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	&�B	'�B	$�B	!�B	#�B	)�B	,B	+B	(�B	.B	/B	49B	5?B	33B	,B	)�B	)�B	-B	2-B	49B	5?B	8RB	8RB	:^B	>wB	B�B	C�B	C�B	C�B	D�B	F�B	I�B	K�B	N�B	R�B	VB	VB	VB	W
B	W
B	XB	ZB	ZB	\)B	dZB	gmB	hsB	hsB	iyB	k�B	p�B	q�B	s�B	u�B	u�B	u�B	v�B	v�B	x�B	y�B	z�B	|�B	� B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�JB	�PB	�PB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�FB	�XB	�dB	�jB	�wB	��B	B	B	ÖB	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	��B	��B
bB
�B
#�B
-B
2-B
:^B
C�B
F�B
K�B
R�B
XB
^5B
cTB
iyB
n�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446532012010314465320120103144654  AO  ARGQ                                                                        20111130140358  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140358  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144654  IP                  G�O�G�O�G�O�                