CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:11Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230511  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_011                   2C  D   APEX                            5368                            041511                          846 @�M����1   @�M�~�@3w
=p���d�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D�3D�33D��fD���D�fD�P D�l�D��3D�fD�I�D�i�D��fD�  D�<�D�C3D�ٚD�	�D�#3D�c3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?��BG��BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dys3D���D�,�D�� D��gD�  D�I�D�fgD���D� D�C4D�c4D�� D���D�6gD�<�D��4D�4D��D�\�D��4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A�  A�A�  A�A�A�A���A��/A�t�A�33A���A�  A��yA��HA���A̼jA�-A���A�A���A�~�A��A��mA�bA�v�A� �A�G�A��A���A�"�A���A�hsA��A�v�A�33A��A�hsA�K�A�O�A��jA��FA�C�A��A�^5A��A���A�`BA�&�A�x�A���A���A��uA�XA��hA�A��DA�9XA��9A�5?A�$�A��FA��jA���A��A���A��A���A���A�ZA���A���A��yA���A�+A���A�33A�7LA��;A��PA���A�33A�S�A��A��+A��TA�A�1A�l�A��A��jA��^A��TA�dZA���A���A~�A}ƨA|bAy�FAx��Av�At9XAsO�ApVAn��AlM�Ai�^AgO�Af�Ae�PAd�`AdĜAdȴAd��AdI�Acl�A`ZA\�DAY�7AW��AV�+AUhsAT��ATQ�AQ�AN~�AL�yAL{AJ��AIK�AH�\AFĜAEdZADn�AC&�A?33A=�A;�A:ZA9�A8n�A7
=A4��A3`BA1��A0z�A/�A-�A+��A*M�A)7LA(jA'��A&5?A%�A$�\A$-A#hsA"1'A!dZA ~�A��A��Av�AdZAĜAffAbA�/A�PA�A9XA�^AȴA�A�AffA�A��A��A��A��A
�/A	
=A^5A�TA33AĜAVAA%A�A��A�A�A �A �@��@�-@�@��`@�j@���@�`B@���@�|�@���@���@�ƨ@��H@�`B@�t�@��@��/@띲@�ȴ@�\@�ff@���@���@��@�X@���@���@�@�33@�ff@�{@��@���@�Z@߮@��y@���@�I�@۶F@�+@�~�@��@�+@�^5@�E�@�hs@�bN@ӥ�@�ff@��@�+@͡�@�?}@�j@��m@�
=@ɡ�@ȋD@�ff@��@�9X@Å@¸R@�n�@��h@�&�@��@��;@���@�dZ@��H@�$�@��-@�X@���@��@�  @��@�n�@��T@��7@��9@���@�S�@�@���@�E�@�G�@�/@���@�1@�\)@���@�^5@���@�&�@�z�@�j@�1@���@�K�@�
=@��+@�=q@���@���@�@���@�X@���@�Z@�b@�t�@��@��H@��+@�V@��@�@���@�p�@�%@�Q�@��@��P@�\)@��@���@�-@��@��-@���@��@��@�9X@��P@�l�@�t�@�t�@�|�@�dZ@�;d@�33@��@���@�-@�J@��@���@�O�@��/@��@��@�t�@�ȴ@�v�@�J@��^@���@���@�hs@�7L@�V@���@�bN@��@��@��;@���@�o@��H@���@��R@��R@���@�E�@��@���@�`B@���@��/@�Ĝ@���@��@�Q�@�1@��@��@�t�@�S�@�"�@���@��@���@�ff@�V@�$�@��T@�X@�&�@�V@���@��`@���@��j@���@�j@�A�@�  @���@�|�@�K�@�"�@��H@��R@��\@��\@���@�^5@�$�@���@��^@���@�hs@���@���@��D@�bN@�9X@�1@��w@�l�@��@�S�@��!@�n�@�$�@�@��h@�x�@�7L@�Ĝ@��u@�9X@��@��;@�ƨ@�t�@�C�@�o@�@�ȴ@�^5@�$�@��@�@��-@��7@�p�@�X@�7L@��/@�r�@��@��m@��
@��F@�t�@�33@�;d@�"�@���@�n�@�V@�=q@��@���@��7@�G�@��@��/@��u@�Z@�^5@z-@qX@k@a�^@W�P@M��@E�T@>��@8Q�@2�@*��@%�@!x�@�@�#@�-@��@�y@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A�  A�A�  A�A�A�A���A��/A�t�A�33A���A�  A��yA��HA���A̼jA�-A���A�A���A�~�A��A��mA�bA�v�A� �A�G�A��A���A�"�A���A�hsA��A�v�A�33A��A�hsA�K�A�O�A��jA��FA�C�A��A�^5A��A���A�`BA�&�A�x�A���A���A��uA�XA��hA�A��DA�9XA��9A�5?A�$�A��FA��jA���A��A���A��A���A���A�ZA���A���A��yA���A�+A���A�33A�7LA��;A��PA���A�33A�S�A��A��+A��TA�A�1A�l�A��A��jA��^A��TA�dZA���A���A~�A}ƨA|bAy�FAx��Av�At9XAsO�ApVAn��AlM�Ai�^AgO�Af�Ae�PAd�`AdĜAdȴAd��AdI�Acl�A`ZA\�DAY�7AW��AV�+AUhsAT��ATQ�AQ�AN~�AL�yAL{AJ��AIK�AH�\AFĜAEdZADn�AC&�A?33A=�A;�A:ZA9�A8n�A7
=A4��A3`BA1��A0z�A/�A-�A+��A*M�A)7LA(jA'��A&5?A%�A$�\A$-A#hsA"1'A!dZA ~�A��A��Av�AdZAĜAffAbA�/A�PA�A9XA�^AȴA�A�AffA�A��A��A��A��A
�/A	
=A^5A�TA33AĜAVAA%A�A��A�A�A �A �@��@�-@�@��`@�j@���@�`B@���@�|�@���@���@�ƨ@��H@�`B@�t�@��@��/@띲@�ȴ@�\@�ff@���@���@��@�X@���@���@�@�33@�ff@�{@��@���@�Z@߮@��y@���@�I�@۶F@�+@�~�@��@�+@�^5@�E�@�hs@�bN@ӥ�@�ff@��@�+@͡�@�?}@�j@��m@�
=@ɡ�@ȋD@�ff@��@�9X@Å@¸R@�n�@��h@�&�@��@��;@���@�dZ@��H@�$�@��-@�X@���@��@�  @��@�n�@��T@��7@��9@���@�S�@�@���@�E�@�G�@�/@���@�1@�\)@���@�^5@���@�&�@�z�@�j@�1@���@�K�@�
=@��+@�=q@���@���@�@���@�X@���@�Z@�b@�t�@��@��H@��+@�V@��@�@���@�p�@�%@�Q�@��@��P@�\)@��@���@�-@��@��-@���@��@��@�9X@��P@�l�@�t�@�t�@�|�@�dZ@�;d@�33@��@���@�-@�J@��@���@�O�@��/@��@��@�t�@�ȴ@�v�@�J@��^@���@���@�hs@�7L@�V@���@�bN@��@��@��;@���@�o@��H@���@��R@��R@���@�E�@��@���@�`B@���@��/@�Ĝ@���@��@�Q�@�1@��@��@�t�@�S�@�"�@���@��@���@�ff@�V@�$�@��T@�X@�&�@�V@���@��`@���@��j@���@�j@�A�@�  @���@�|�@�K�@�"�@��H@��R@��\@��\@���@�^5@�$�@���@��^@���@�hs@���@���@��D@�bN@�9X@�1@��w@�l�@��@�S�@��!@�n�@�$�@�@��h@�x�@�7L@�Ĝ@��u@�9X@��@��;@�ƨ@�t�@�C�@�o@�@�ȴ@�^5@�$�@��@�@��-@��7@�p�@�X@�7L@��/@�r�@��@��m@��
@��F@�t�@�33@�;d@�"�@���@�n�@�V@�=q@��@���@��7@�G�@��@��/@��uG�O�@�^5@z-@qX@k@a�^@W�P@M��@E�T@>��@8Q�@2�@*��@%�@!x�@�@�#@�-@��@�y@�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�uB�uB�uB�uB�uB�uB�uB�uB�{B�uB�uB�{B��B��B��B��B�B�B��BBBBBBBB%B%BB��B��B�B�B��B  BB��B��B��B��B��B��B��B+BDBoB"�B%�B%�B �B�B�B�B�B�B�B�B\B��B�B�mB�TB�)B�
B��B��BǮB��B��B��B�^B�B��B��B�+B� Bn�BZBJ�B9XB�B��B�BB�B��BĜB�wB�XB�!B��B��B�DB� B]/B?}B"�B	7B
�B
�BB
��B
��B
��B
�VB
s�B
cTB
YB
K�B
=qB
6FB
'�B
�B
bB	��B	�B	�TB	��B	ŢB	��B	�^B	�RB	�RB	�RB	�RB	�9B	�B	��B	�+B	v�B	n�B	hsB	cTB	_;B	[#B	L�B	?}B	8RB	33B	.B	'�B	#�B	�B	�B	uB	PB��B��B��B�B�B�B�sB�TB�;B�B��B��BȴBŢB��B�wB�dB�LB�-B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�=B�1B�B�B� B}�Bz�Bx�Bu�Bs�Bp�Bm�Bk�BiyBhsBffBffBffBdZB`BB\)BZBW
BW
B^5B_;BdZBe`Be`BdZBe`BiyBiyBhsBgmBffBgmBk�Bk�BhsBgmBhsBhsBiyBjBjBl�Bp�Bt�Bs�Bt�Bt�Bw�B{�Bz�B{�B|�B~�B~�B� B�B� B�B�B�B�B�B�7B�+B�=B�DB�JB�JB�PB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�?B�FB�RB�jB�qB�wB�}BĜBɺB��B��B��B�
B�B�)B�;B�HB�TB�`B�yB�B�B�B��B	  B	%B	1B	DB	VB	hB	oB	�B	�B	�B	�B	�B	�B	!�B	#�B	"�B	$�B	&�B	(�B	-B	/B	2-B	49B	5?B	8RB	9XB	;dB	<jB	>wB	?}B	A�B	E�B	H�B	K�B	L�B	N�B	O�B	S�B	T�B	XB	XB	YB	[#B	`BB	ffB	hsB	iyB	iyB	jB	k�B	n�B	n�B	o�B	s�B	u�B	v�B	w�B	y�B	{�B	~�B	�B	�B	�B	�1B	�=B	�VB	�bB	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	B	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�5B	�;B	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
1B
+B
1B
1B
	7B

=B
DB
JB
PB
VB
\B
uB
{B
�B
#�B
(�B
1'B
7LB
?}B
F�B
K�B
P�B
VB
]/B
aHB
ffB
iyB
m�B
q�B
u�B
w�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�%B��B�BBB!B#B+B.B"B9B9B$B��B��B�B��B��B BB�B�	B�B��B��B��B��B=BVB�B"�B%�B%�B �B�B�B�B�B�B�B�BoB��B�B�B�gB�:B�B�B��B��B��B��B��B�sB�'B��B��B�>B�Bn�BZ/BJ�B9lB�B��B�TB�!B��BįB��B�iB�3B�B��B�ZB�B]DB?�B"�B	MB
��B
�YB
��B
��B
� B
�oB
s�B
cpB
Y0B
K�B
=�B
6_B
(B
�B
B	�B	�B	�rB	�B	��B	��B	�}B	�pB	�sB	�qB	�qB	�ZB	�(B	��B	�JB	v�B	n�B	h�B	cvB	_`B	[EB	L�B	?�B	8vB	3UB	.:B	(B	#�B	�B	�B	�B	wB�B��B��B��B��B��B�B�}B�fB�GB�B��B��B��B��B��B��B�wB�ZB�EB�7B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�wB�kB�`B�JB�;B�.B~"B{ByBu�Bs�Bp�Bm�Bk�Bi�Bh�Bf�Bf�Bf�Bd�B`rB\WBZNBW:BW<B^cB_kBd�Be�Be�Bd�Be�Bi�Bi�Bh�Bg�Bf�Bg�Bk�Bk�Bh�Bg�Bh�Bh�Bi�Bj�Bj�Bl�Bp�Bt�Bs�Bt�Bt�Bw�B|B{B|B}B)B)B�0B�4B�/B�5B�@B�FB�IB�MB�fB�]B�jB�rB�yB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�(B�<B�BB�GB�SB�_B�lB�qB�{B��B��B��B��B��B��B��B�B� B�3B�BB�RB�fB�sB�~B�B�B��B��B��B��B	 'B	MB	[B	kB	~B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	"�B	%B	'B	)B	-6B	/EB	2RB	4aB	5eB	8xB	9�B	;�B	<�B	>�B	?�B	A�B	E�B	H�B	K�B	L�B	N�B	PB	T B	U%B	X7B	X6B	Y?B	[IB	`fB	f�B	h�B	i�B	i�B	j�B	k�B	n�B	n�B	o�B	s�B	u�B	v�B	w�B	z B	|B	"B	�+B	�0B	�AB	�TB	�bB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�*B	�>B	�=B	�EB	�KB	�PB	�YB	�]B	�jB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	´B	²B	øB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�&B	�)B	�-B	�6B	�?B	�FB	�JB	�LB	�XB	�]B	�hB	�hB	�jB	�lB	�vB	�wB	�{B	�B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�
B	�B	�B	�B	�B
 B
&B
,B
5B
9B
@B
GB
SB
NB
QB
SB
	UB

^B
eB
hB
oB
xB
{G�O�B
�B
�B
#�B
)B
1FB
7kB
?�B
F�B
K�B
QB
V"B
]LB
aeB
f�B
i�B
m�B
q�B
u�B
w�B
y�B
}
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230511    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230511  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230511  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                