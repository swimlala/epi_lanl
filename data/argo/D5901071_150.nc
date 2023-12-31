CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:33Z UW 3.1 conversion   
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
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142555  20190522121827  1727_5046_150                   2C  D   APEX                            2143                            040306                          846 @�ׇ��W�1   @�׈@y`@6+C��%�c�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�33C   C�fC  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Dh��Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D�33D�c3D��3D��fD�,�D�VfD��fD��fD�0 D�l�D�S3D��D�  D�ffDړ3D�� D�  D�Y�D�fD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���A��A<��A\��A|��A�ffA�ffA���A�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B.��B6��B?33BG33BO33BW33B_33Bg33Bo33Bw��B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�B���B㙚B癚B뙚BB�B���B���B���C�3C��C��C��C	��C��C��C��C��C��C��C��C�3C��C��C��C!��C#��C%��C'��C)��C+��C-�fC/�fC1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�fCs��Cu��Cw��Cy��C{��C}��C��C��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Dy�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%y�D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4��D5l�D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\l�D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhl�Dh��Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Doy�Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Dy�3D�,�D�\�D���D�� D�&fD�P D�� D�� D�)�D�ffD�L�D��3D��D�` Dڌ�D��D��D�S3D� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��`A��/A�AͰ!Aͥ�A�S�A�ȴA���A��;A�ƨA̰!A̗�ÃA�M�A�"�A�9XAʡ�A��AǓuA�
=A��TA���A�ȴA�ȴA�ĜA�~�A�-A�hsA���A��A�E�A��wA���A��A��jA�n�A�-A��
A�\)A���A��+A���A���A�(�A���A�t�A�%A��DA�/A���A���A��A�VA�ĜA��`A���A���A���A�
=A�33A��A���A�
=A��yA���A��!A�v�A�M�A�7LA��A���A�|�A�(�A�33A��wA�9XA�33A�|�A�7LA�-A�{A��`A�t�A��A��HA�JA�p�A��^A�E�A��HA�|�A�%A���A��A��7A�1'A���A�A�`BA�\)A�^5A��A�dZA��A���A�ȴA���A�z�A�M�A�JA���A�VA�1A��+A�`BA���A�+A�7Az��Av�jAu%As�
Aq��AnffAk�AgO�Ad$�Ac\)Aa�
A_�A\ĜAY�AX��AW�AU�FAT��AShsARjAQƨAP$�AN��AM�AKK�AI`BAG��AEO�ADQ�AB��AA/A@E�A?�A?G�A>jA=C�A<��A<��A<bA;��A:�\A6�!A5O�A5K�A4��A3��A2��A2M�A17LA/�A/%A.(�A,��A+�PA+&�A*Q�A)�A(��A&�/A#�A"�DA!�-A ��A�7A��A��A�TAhsAQ�A;dA�7A�Al�A�A�;Az�A�A�;A��Al�A&�A�A��AjA �A��A-A-A �A
��A	�A	��A	�^A	�A	O�A^5A�-AC�A�A��A��AȴA�#A ^5@�\)@��R@��-@���@��R@��+@���@�?}@��@�+@�n�@��@�/@��@�bN@�Z@�1'@��
@�+@�dZ@�dZ@��y@�X@�z�@�ƨ@�+@�bN@�C�@���@�?}@��@�t�@�!@���@߶F@�G�@ݙ�@۾w@���@�$�@�5?@�|�@��@�@�  @�o@�^5@�/@ԓu@���@�O�@Ͼw@�v�@�hs@���@��H@��T@ȃ@�|�@��@�@���@Ĵ9@ēu@�I�@Å@�@�ff@�J@�x�@��/@��@�o@��H@�ȴ@�E�@�O�@��@���@��@��@��u@��;@���@�V@�J@��#@�@��-@��7@�p�@�X@�G�@�7L@�/@��@�j@�K�@��y@�M�@�@�V@��9@��@�j@�A�@� �@�  @��@���@�M�@�E�@�E�@�E�@�{@��7@��`@�Z@��@���@�K�@���@�-@��@��9@��@�C�@��H@��\@���@�r�@�dZ@���@���@�~�@�^5@�{@�G�@�1'@�o@���@���@��T@�hs@�9X@�5?@���@�Z@�  @�|�@�ff@��/@�Q�@�A�@�A�@�9X@�I�@�r�@��@�I�@��F@��+@��#@���@��@�G�@���@��9@��@��D@�bN@��@��;@���@��w@�dZ@�+@��@��\@�V@��#@�p�@�&�@�%@��/@�Ĝ@��9@���@�bN@�(�@��
@��F@��P@�t�@�S�@��@��H@���@�v�@�v�@�^5@�=q@�{@���@��@��@��#@��^@���@��h@��@�`B@�&�@���@���@��/@���@�Q�@��m@��F@���@�|�@�\)@�33@�o@��H@���@��!@��+@�V@�E�@�5?@�-@��@���@��@�@���@�X@��@�r�@�b@�  @��@��;@��w@���@�|�@�C�@�@���@��\@��\@�=q@��#@���@��@�G�@�&�@���@���@��\@|(�@sC�@cS�@Z��@R��@J��@F��@>@;33@5�@0�9@+��@%@ r�@�j@+@o@��@�/@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��`A��/A�AͰ!Aͥ�A�S�A�ȴA���A��;A�ƨA̰!A̗�ÃA�M�A�"�A�9XAʡ�A��AǓuA�
=A��TA���A�ȴA�ȴA�ĜA�~�A�-A�hsA���A��A�E�A��wA���A��A��jA�n�A�-A��
A�\)A���A��+A���A���A�(�A���A�t�A�%A��DA�/A���A���A��A�VA�ĜA��`A���A���A���A�
=A�33A��A���A�
=A��yA���A��!A�v�A�M�A�7LA��A���A�|�A�(�A�33A��wA�9XA�33A�|�A�7LA�-A�{A��`A�t�A��A��HA�JA�p�A��^A�E�A��HA�|�A�%A���A��A��7A�1'A���A�A�`BA�\)A�^5A��A�dZA��A���A�ȴA���A�z�A�M�A�JA���A�VA�1A��+A�`BA���A�+A�7Az��Av�jAu%As�
Aq��AnffAk�AgO�Ad$�Ac\)Aa�
A_�A\ĜAY�AX��AW�AU�FAT��AShsARjAQƨAP$�AN��AM�AKK�AI`BAG��AEO�ADQ�AB��AA/A@E�A?�A?G�A>jA=C�A<��A<��A<bA;��A:�\A6�!A5O�A5K�A4��A3��A2��A2M�A17LA/�A/%A.(�A,��A+�PA+&�A*Q�A)�A(��A&�/A#�A"�DA!�-A ��A�7A��A��A�TAhsAQ�A;dA�7A�Al�A�A�;Az�A�A�;A��Al�A&�A�A��AjA �A��A-A-A �A
��A	�A	��A	�^A	�A	O�A^5A�-AC�A�A��A��AȴA�#A ^5@�\)@��R@��-@���@��R@��+@���@�?}@��@�+@�n�@��@�/@��@�bN@�Z@�1'@��
@�+@�dZ@�dZ@��y@�X@�z�@�ƨ@�+@�bN@�C�@���@�?}@��@�t�@�!@���@߶F@�G�@ݙ�@۾w@���@�$�@�5?@�|�@��@�@�  @�o@�^5@�/@ԓu@���@�O�@Ͼw@�v�@�hs@���@��H@��T@ȃ@�|�@��@�@���@Ĵ9@ēu@�I�@Å@�@�ff@�J@�x�@��/@��@�o@��H@�ȴ@�E�@�O�@��@���@��@��@��u@��;@���@�V@�J@��#@�@��-@��7@�p�@�X@�G�@�7L@�/@��@�j@�K�@��y@�M�@�@�V@��9@��@�j@�A�@� �@�  @��@���@�M�@�E�@�E�@�E�@�{@��7@��`@�Z@��@���@�K�@���@�-@��@��9@��@�C�@��H@��\@���@�r�@�dZ@���@���@�~�@�^5@�{@�G�@�1'@�o@���@���@��T@�hs@�9X@�5?@���@�Z@�  @�|�@�ff@��/@�Q�@�A�@�A�@�9X@�I�@�r�@��@�I�@��F@��+@��#@���@��@�G�@���@��9@��@��D@�bN@��@��;@���@��w@�dZ@�+@��@��\@�V@��#@�p�@�&�@�%@��/@�Ĝ@��9@���@�bN@�(�@��
@��F@��P@�t�@�S�@��@��H@���@�v�@�v�@�^5@�=q@�{@���@��@��@��#@��^@���@��h@��@�`B@�&�@���@���@��/@���@�Q�@��m@��F@���@�|�@�\)@�33@�o@��H@���@��!@��+@�V@�E�@�5?@�-@��@���@��@�@���@�X@��@�r�@�b@�  @��@��;@��w@���@�|�@�C�@�@���@��\@��\@�=q@��#@���@��@�G�@�&�@���@���@��\@|(�@sC�@cS�@Z��@R��@J��@F��@>@;33@5�@0�9@+��@%@ r�@�j@+@o@��@�/@�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBɺBɺBɺBɺBɺBƨBŢBĜB�XB��B�-B��BȴB��B��B��B��BȴB�FB��B�B�jB�B@�BM�BQ�BVBXBdZBx�B�7B�+B��B��B�B�XB�qB��BĜBȴB��B��B�
B�
B�#B�)B�BB�BB�HB�`B�sB�B�B�B��B��B��B��B��BBB��B��B�B�;B�
B�B��B��B��B��B��BɺB��B�RB��B��B�bB�1B}�Br�Bo�Bn�Bl�BiyBcTB^5BYBJ�B@�B49B(�B�B��B�;BŢB��Bv�BL�B6FB+B�B
��B
�)B
��B
ÖB
��B
�wB
�jB
�^B
�XB
�FB
�3B
�B
��B
�=B
�B
�B
~�B
u�B
l�B
S�B
=qB
2-B
)�B
�B
1B	�B	�5B	��B	��B	ǮB	�RB	��B	��B	�hB	�=B	�B	~�B	y�B	p�B	k�B	bNB	YB	N�B	C�B	:^B	0!B	+B	 �B	�B	hB	PB	
=B	+B	B��B��B��B��B��B�B�5B�)B�/B�/B�;B�B��B��BɺBȴBÖB�}B�jB�^B�FB�9B�'B��B��B��B��B��B��B�oB�PB�+B�B�B}�By�Bw�Bv�Bs�Bq�Bp�Bo�Bo�Bn�Bn�Bm�Bm�Bl�Bk�BiyBhsBiyBhsBffBffBffBffBe`Be`BcTBbNBaHB`BB^5B\)B\)B[#BXBYBXBXBXBXBW
BW
BVBT�BR�BP�BR�BR�BR�BR�BR�BR�BR�BR�BR�BT�BVBXB]/B`BBdZBffBiyBgmBffBhsBhsBk�BjBiyBl�Bm�Bo�Bk�Bn�Bt�Bz�B�B~�B~�B�B�1B�DB�bB�oB��B��B�{B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�?B�FB�FB�FB�LB�RB�^B�^B�^B�qB��B��B��BBĜBŢBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�)B�5B�;B�NB�sB�B�B�B�B�B�B�B��B��B��B��B	B	B	B	DB	bB	�B	�B	�B	�B	&�B	33B	9XB	=qB	>wB	?}B	@�B	F�B	L�B	J�B	K�B	M�B	M�B	L�B	J�B	I�B	H�B	H�B	G�B	H�B	L�B	W
B	\)B	_;B	`BB	bNB	cTB	e`B	jB	l�B	k�B	k�B	l�B	m�B	n�B	o�B	q�B	s�B	s�B	t�B	u�B	w�B	y�B	y�B	z�B	}�B	~�B	�B	�%B	�+B	�+B	�1B	�1B	�1B	�7B	�=B	�=B	�DB	�JB	�PB	�bB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�jB	�wB	�wB	�wB	�}B	�}B	��B	��B	��B	B	ŢB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	��B
	7B
�B
(�B
0!B
5?B
>wB
B�B
J�B
Q�B
W
B
[#B
aHB
e`B
jB
o�B
r�B
t�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BɺBɺBɺBɺB��BƨBŢBƨB�jB��B�-BBɺB��B��B��B��B��B�RB�B�!B�wB�BA�BM�BQ�BVBYBe`Bz�B�JB�JB��B�B�3B�jB�wBÖBƨB��B��B��B�B�#B�5B�BB�TB�TB�ZB�yB�B�B�B�B��B��B  B  B  B1B
=BB��B�B�`B�B�B��B��B��B��B��B��BȴB��B�B��B�{B�\B�Bt�Bo�Bo�Bn�Bl�Be`B`BB_;BS�BD�B6FB+B)�B	7B�`B��B��B�+BT�B9XB/B/BB
�ZB
��B
ŢB
��B
�}B
�qB
�dB
�^B
�RB
�?B
�-B
�'B
�PB
�B
�B
�B
w�B
u�B
[#B
@�B
49B
.B
$�B
\B	��B	�fB	�
B	��B	��B	��B	�B	��B	��B	�VB	�+B	�B	|�B	r�B	p�B	gmB	^5B	T�B	I�B	@�B	7LB	/B	%�B	�B	{B	\B	JB	
=B	+B	B��B��B��B��B��B�NB�)B�5B�HB�NB�#B�B��B��B��BǮBB�qB�jB�XB�?B�LB�!B��B��B��B��B��B��B�uB�\B�+B�B�B�Bx�Bx�Bx�Bv�Br�Bp�Bp�Bo�Bo�Bn�Bn�Bm�Bm�Bn�BjBiyBiyBk�BhsBgmBgmBffBffBffBdZBcTBcTBaHBbNB^5B]/B[#B[#BYBZB[#BZBXBXBW
BW
BT�BXBT�BS�BS�BR�BR�BS�BS�BS�BR�BT�BW
B[#B_;BbNBgmBjBk�BjBhsBjBiyBm�Bm�Bl�Bp�Bm�Br�Bm�Bo�Bt�Bx�B�B�B�B�B�=B�PB�hB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�-B�FB�LB�LB�LB�XB�^B�dB�^B�dB�}B��B��B��BBĜBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�/B�;B�BB�ZB�yB�B�B�B�B�B�B�B��B��B��B��B	B	B	%B	PB	hB	�B	�B	�B	 �B	(�B	49B	:^B	=qB	>wB	?}B	@�B	H�B	N�B	L�B	K�B	M�B	N�B	N�B	N�B	L�B	I�B	I�B	H�B	J�B	N�B	XB	\)B	_;B	`BB	bNB	cTB	e`B	jB	m�B	m�B	l�B	l�B	m�B	o�B	p�B	r�B	s�B	s�B	t�B	v�B	x�B	y�B	y�B	{�B	~�B	� B	�B	�+B	�1B	�1B	�7B	�1B	�1B	�7B	�=B	�=B	�JB	�PB	�VB	�bB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�3B	�9B	�?B	�?B	�?B	�LB	�LB	�RB	�qB	�wB	�wB	�wB	�}B	�}B	��B	��B	B	ÖB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B

=B
�B
(�B
1'B
5?B
>wB
B�B
J�B
Q�B
W
B
[#B
bNB
e`B
jB
o�B
r�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<D��<�o<#�
<#�
<#�
<�o<49X<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447262012010314472620120103144726  AO  ARGQ                                                                        20111130142555  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142555  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144726  IP                  G�O�G�O�G�O�                