CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:12Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               KA   AO  20111130140830  20190522121826  1727_5046_075                   2C  D   APEX                            2143                            040306                          846 @�v��� 1   @�v�K� 	@7�S����d?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  Dy�D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy�fD�)�D�` D��fD��3D�&fD�i�D���D��fD��D�` D��3D���D�&fD�ffDڠ D��3D�&fD�I�D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�B�ffB㙚B癚B���BB�B���B���B���C��C��C�3C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Dy�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"��D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)l�D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI��DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]��D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De��Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3DsٚDy��D�#3D�Y�D�� D���D�  D�c3D��3D�� D�3D�Y�D���D��fD�  D�` Dڙ�D���D�  D�C3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�v�A�Q�A�(�A�t�A�1AĬA�+A��A���A���AîAç�AÛ�AÉ7A�x�A�VA��TA�&�A���A���A� �A�A�A�bA���A�dZA���A�$�A��!A�33A��mA��A��+A���A��A�ffA���A�~�A��HA��A�XA��A�\)A��A�  A���A��RA��#A��hA�K�A��RA���A��A�&�A��^A�jA��A���A��!A�l�A�oA�-A�ĜA��A��A�ĜA�r�A�9XA��wA��jA�+A��;A���A�v�A�XA�9XA���A�l�A��PA�$�A��!A�+A�"�A��A��-A��HA���A��A�S�A��FA�t�A�K�A�&�A�ȴA�`BA��A��A��A��A���A��FA��DA���A�ȴA�5?A���A��A�v�A�5?A�r�A�
=A�~�A��A};dA{��Ax��AvȴAuhsAuVAt��AtM�At$�At1AsS�Aq�TAq��Aq��Aq�7Aq"�Ap=qAn��Am�7Ak�PAj��Ajn�Aj(�Ah�RAe%AdbAb��Aa��Aa"�A`1A^�A\r�AZ�9AZJAX1'AWoAV�DAUK�AT�AR~�AP��AN��ANI�AN  ALA�AI��AH�+AGC�AE`BAC��AC7LAB��AB{A@jA?�wA?��A?hsA??}A>�A<ȴA;�wA;VA:�yA:A8�RA8A7;dA65?A5�^A5��A5t�A4��A3�;A2�`A0��A-�A,~�A,JA+��A+��A+S�A+%A*5?A)�A)�;A)�-A'�wA%�A$ĜA#?}A!�A!/A n�A �A�Ap�A�
A��A{A�AjA�At�A��AZAbA��A�AI�A/AoA��Az�A=qAE�A
r�A
A	�An�AbA�7AO�A&�A�yAffA��A�hAhsA/A�yA��A��A��A1A 9X@��H@���@�v�@�ff@�^5@�E�@��h@��@���@� �@��@��#@�9X@�@�9X@�v�@���@�j@�dZ@�o@�{@睲@�r�@�$�@�G�@�Q�@��m@߅@���@�n�@��@ݺ^@�O�@���@܃@�1'@��
@ڰ!@�M�@��@�X@���@�1@�ȴ@�|�@�ff@�@�-@ѩ�@�@�V@�b@�9X@�b@Ƈ+@�p�@Õ�@���@�1@��\@�A�@��@���@�S�@���@�~�@�~�@��7@�`B@��@�Ĝ@�j@���@��F@��-@���@�z�@��D@���@��@��@�@�n�@�V@�n�@�V@�E�@�E�@���@�/@��F@���@��@�hs@���@�ƨ@�S�@�M�@��h@��@�z�@�1'@�l�@�
=@�v�@��@���@�?}@��/@�I�@���@�S�@�~�@��^@�X@�V@���@���@���@���@�=q@�p�@�V@�1@��P@�33@���@���@�|�@�K�@�M�@�X@��/@��9@��@�Ĝ@�j@�ƨ@��w@�ƨ@��;@�1@�z�@��9@��9@��9@���@�z�@�j@�Z@�1'@��@��@�@�5?@���@�@��\@�ff@�v�@��\@��!@�=q@���@���@�O�@��@���@���@�r�@�I�@�Z@�Q�@�I�@�I�@�Q�@�Z@�j@�bN@���@�V@���@��/@� �@��;@��@��R@�ff@�{@��@��^@�O�@���@�Ĝ@�Q�@���@��F@�S�@�K�@���@�~�@���@��+@�
=@��P@�S�@��!@�^5@�=q@�x�@�7L@�r�@�dZ@�$�@�j@�b@��w@�t�@��@��R@��!@��\@�~�@�ff@�5?@�J@�{@��@���@��@�%@��`@��/@��D@�  @�(�@��D@�I�@��w@�$�@}O�@t��@kt�@d1@\(�@T(�@I�^@B�H@;�
@4�j@.�@*��@#"�@��@&�@�@bN@�
@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�v�A�Q�A�(�A�t�A�1AĬA�+A��A���A���AîAç�AÛ�AÉ7A�x�A�VA��TA�&�A���A���A� �A�A�A�bA���A�dZA���A�$�A��!A�33A��mA��A��+A���A��A�ffA���A�~�A��HA��A�XA��A�\)A��A�  A���A��RA��#A��hA�K�A��RA���A��A�&�A��^A�jA��A���A��!A�l�A�oA�-A�ĜA��A��A�ĜA�r�A�9XA��wA��jA�+A��;A���A�v�A�XA�9XA���A�l�A��PA�$�A��!A�+A�"�A��A��-A��HA���A��A�S�A��FA�t�A�K�A�&�A�ȴA�`BA��A��A��A��A���A��FA��DA���A�ȴA�5?A���A��A�v�A�5?A�r�A�
=A�~�A��A};dA{��Ax��AvȴAuhsAuVAt��AtM�At$�At1AsS�Aq�TAq��Aq��Aq�7Aq"�Ap=qAn��Am�7Ak�PAj��Ajn�Aj(�Ah�RAe%AdbAb��Aa��Aa"�A`1A^�A\r�AZ�9AZJAX1'AWoAV�DAUK�AT�AR~�AP��AN��ANI�AN  ALA�AI��AH�+AGC�AE`BAC��AC7LAB��AB{A@jA?�wA?��A?hsA??}A>�A<ȴA;�wA;VA:�yA:A8�RA8A7;dA65?A5�^A5��A5t�A4��A3�;A2�`A0��A-�A,~�A,JA+��A+��A+S�A+%A*5?A)�A)�;A)�-A'�wA%�A$ĜA#?}A!�A!/A n�A �A�Ap�A�
A��A{A�AjA�At�A��AZAbA��A�AI�A/AoA��Az�A=qAE�A
r�A
A	�An�AbA�7AO�A&�A�yAffA��A�hAhsA/A�yA��A��A��A1A 9X@��H@���@�v�@�ff@�^5@�E�@��h@��@���@� �@��@��#@�9X@�@�9X@�v�@���@�j@�dZ@�o@�{@睲@�r�@�$�@�G�@�Q�@��m@߅@���@�n�@��@ݺ^@�O�@���@܃@�1'@��
@ڰ!@�M�@��@�X@���@�1@�ȴ@�|�@�ff@�@�-@ѩ�@�@�V@�b@�9X@�b@Ƈ+@�p�@Õ�@���@�1@��\@�A�@��@���@�S�@���@�~�@�~�@��7@�`B@��@�Ĝ@�j@���@��F@��-@���@�z�@��D@���@��@��@�@�n�@�V@�n�@�V@�E�@�E�@���@�/@��F@���@��@�hs@���@�ƨ@�S�@�M�@��h@��@�z�@�1'@�l�@�
=@�v�@��@���@�?}@��/@�I�@���@�S�@�~�@��^@�X@�V@���@���@���@���@�=q@�p�@�V@�1@��P@�33@���@���@�|�@�K�@�M�@�X@��/@��9@��@�Ĝ@�j@�ƨ@��w@�ƨ@��;@�1@�z�@��9@��9@��9@���@�z�@�j@�Z@�1'@��@��@�@�5?@���@�@��\@�ff@�v�@��\@��!@�=q@���@���@�O�@��@���@���@�r�@�I�@�Z@�Q�@�I�@�I�@�Q�@�Z@�j@�bN@���@�V@���@��/@� �@��;@��@��R@�ff@�{@��@��^@�O�@���@�Ĝ@�Q�@���@��F@�S�@�K�@���@�~�@���@��+@�
=@��P@�S�@��!@�^5@�=q@�x�@�7L@�r�@�dZ@�$�@�j@�b@��w@�t�@��@��R@��!@��\@�~�@�ff@�5?@�J@�{@��@���@��@�%@��`@��/@��D@�  @�(�@��D@�I�@��w@�$�@}O�@t��@kt�@d1@\(�@T(�@I�^@B�H@;�
@4�j@.�@*��@#"�@��@&�@�@bN@�
@bN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB+B%B%BBB%B1B+B	7B1B+B+B	7B	7B
=B
=B1BDB'�B?}BI�BF�BT�BR�B@�B%�B�B�BhB	7B��B��B��B��B�B�`B�NB�BB�)B�B��B��B��BB�qB�9B�!B�?B�?B�-B�9B�3B�?B�?B�9B�LB�FB�^B�?B�B��B�uB~�Bq�B\)B8RB$�B �B�B\B
=B+BBB��B��B�B�/B��B��BÖB�jB��B��B�BhsBZBK�BA�B9XB5?B33B/B)�B"�B�BDB  B
��B
�B
�TB
�;B
��B
�XB
�'B
�B
��B
��B
��B
�oB
�VB
�B
x�B
o�B
`BB
Q�B
F�B
@�B
?}B
=qB
;dB
:^B
8RB
49B
.B
-B
-B
+B
(�B
"�B
�B
oB
	7B
%B
B
  B	��B	�ZB	�5B	�
B	��B	��B	ȴB	�}B	�9B	�B	��B	��B	��B	��B	�\B	�1B	�B	z�B	r�B	o�B	k�B	bNB	VB	M�B	F�B	=qB	5?B	5?B	33B	0!B	(�B	#�B	"�B	!�B	�B	�B	�B	bB	VB	JB	1B	B	B��B��B��B��B��B�B�B�mB�/B��B��B��B��B��B��B��BȴBȴBƨBÖB�qB�^B�LB�3B�!B�B�B�B�B��B��B��B��B�bB� B|�Bz�By�By�Bx�Bw�Bv�Bt�Bt�Br�Bq�Bp�Bm�BhsBffBcTBbNBaHB`BB`BB_;B_;B^5B\)B\)B\)B[#BZBW
BVBVBT�BT�BT�BXBXBYBYBXBXBXBXBXBW
BW
BVBT�BS�BQ�BO�BK�BI�BH�BG�BE�BE�B@�B?}B@�BD�BF�BH�BK�BO�BQ�BQ�BR�BR�BS�BT�BT�BZBjBx�B~�B�B�B�Bz�Bx�By�B�B�DB�uB��B��B�%B|�B~�B}�Bv�Bo�BjBhsBiyBm�Bs�Bv�Bx�By�B~�B�B�1B�PB�\B�{B��B��B��B��B��B��B�B�B�3BB��B�B�5B�;B�HB�NB�HB�;B�HB�;B�;B�NB�HB�HB�sB�B�B�B��B��B��B��B��B	B	B	B	B	B	+B	1B	JB	bB	hB	bB	hB	�B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	#�B	%�B	&�B	&�B	&�B	&�B	(�B	-B	,B	-B	.B	/B	1'B	6FB	9XB	:^B	;dB	=qB	>wB	?}B	?}B	?}B	?}B	?}B	A�B	D�B	D�B	@�B	A�B	B�B	H�B	L�B	Q�B	T�B	VB	XB	YB	ZB	\)B	^5B	aHB	bNB	dZB	gmB	hsB	hsB	hsB	jB	o�B	q�B	{�B	� B	� B	�B	� B	~�B	}�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�%B	�1B	�=B	�PB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	�PB	�VB	�\B	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�FB	ŢB	�;B	�B
%B
uB
�B
$�B
(�B
2-B
:^B
A�B
H�B
K�B
T�B
ZB
_;B
cTB
hsB
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B+B+B1B+B+B
=B+B
=B	7B1B+B	7B	7B
=BDB
=BoB/BD�BK�BF�BVBYBH�B)�B�B�B{BuB��B��B��B��B��B�sB�TB�TB�BB�B��B��B��BŢBÖB�XB�9B�FB�FB�?B�?B�FB�XB�LB�FB�XB�LB�dB�FB�B��B��B�Bv�BffB>wB%�B"�B�BoBJB1BBB  B��B��B�HB��B��BƨB��B�B��B�=Bm�B_;BO�BD�B;dB6FB49B1'B-B'�B�B\BB  B
�B
�ZB
�TB
�B
�jB
�3B
�B
��B
��B
��B
�{B
�hB
�B
� B
r�B
hsB
W
B
J�B
A�B
@�B
>wB
<jB
:^B
:^B
8RB
/B
-B
-B
,B
,B
&�B
�B
�B
JB
+B
B
B
B	�mB	�NB	�#B	��B	��B	��B	ƨB	�XB	�B	�B	��B	��B	��B	�uB	�PB	�%B	�B	t�B	p�B	q�B	jB	ZB	Q�B	L�B	C�B	6FB	6FB	5?B	49B	+B	#�B	#�B	"�B	!�B	�B	�B	oB	\B	\B	JB	%B	B	B��B��B��B��B��B�B�B�fB�
B��B��B��B��B��B��BɺBȴBǮBɺBÖB�wB�jB�LB�3B�'B�B�B�B�B��B��B��B��B�B}�B}�Bz�Bz�Bz�By�By�Bw�Bu�Bs�Br�Bq�Br�Bm�BgmBe`BdZBbNBbNBaHB`BB`BB`BB^5B]/B]/B\)B[#BZBZBW
BW
B[#BXBYBYBYBYBXBYBYBYBYBYBYBYBXBS�BT�BR�BQ�BK�BI�BI�BI�BJ�BD�BA�BB�BE�BG�BI�BK�BP�BR�BR�BS�BS�BT�BVBW
BZBk�By�B~�B�B�B�%B|�By�By�B�B�DB�{B��B��B�JB~�B�B�By�Br�Bl�Bl�BjBm�Bt�Bw�By�By�B�B�B�7B�VB�bB��B��B��B��B��B��B��B�B�B�-BB��B�B�5B�;B�HB�TB�NB�;B�`B�BB�;B�ZB�TB�NB�B�B�B�B��B��B��B��B��B	B	B	B	%B	%B	1B	
=B	PB	hB	oB	hB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	'�B	'�B	&�B	&�B	&�B	)�B	.B	,B	-B	.B	/B	0!B	6FB	9XB	:^B	;dB	=qB	>wB	?}B	?}B	@�B	@�B	@�B	C�B	D�B	G�B	A�B	A�B	B�B	H�B	L�B	R�B	VB	W
B	YB	ZB	[#B	\)B	_;B	aHB	bNB	dZB	gmB	hsB	hsB	hsB	jB	o�B	p�B	{�B	� B	� B	�B	�B	� B	� B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�PB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�\B	�bB	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�FB	ŢB	�;B	�B
%B
uB
�B
$�B
(�B
2-B
:^B
A�B
H�B
K�B
T�B
ZB
_;B
cTB
iyB
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447002012010314470020120103144700  AO  ARGQ                                                                        20111130140830  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140830  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144700  IP                  G�O�G�O�G�O�                