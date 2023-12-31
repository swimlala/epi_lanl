CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:42Z UW 3.1 conversion   
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
_FillValue                 �  Ah   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  hX   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  p    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143609  20190522121828  1728_5048_012                   2C  D   APEX                            2142                            040306                          846 @�H�[6?�1   @�Hơ/h@4$�/��c�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B7��B@  BH  BP  BX  B`  BhffBq33Bv��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvs3DyY�D�fD�C3D�` D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@���A��A>ffA\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B��B33B'33B/33B6��B?33BG33BO33BW33B_33Bg��BpffBv  B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�3C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�3C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��3C��fC��fC�ٚC�ٚC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"��D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,y�D,�3D-s3D-�3D.l�D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6��D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;��D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?��D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3DvffDyL�D�  D�<�D�Y�D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�I�AՕ�A�oA԰!A�|�A�?}A�%A�ƨA�bNA�ĜA�S�A�JA��#Aѧ�AсA�ZA���A�XA�M�A�9XA�bNA��
A�bA��A� �A�=qA�r�A�
=AőhAċDAô9A���A�=qA��A��RA�E�A��jA�(�A�$�A�A��A��mA�|�A�O�A�=qA��A�ĜA�VA�C�A�A�ĜA�|�A� �A��A�dZA��yA��A��HA�l�A��#A� �A��A�K�A���A�v�A���A���A�Q�A�1'A���A�E�A��!A��RA��A��/A�%A��-A�
=A�VA��!A�XA���A���A�ĜA�A��A���A��/A��A�O�A��A��A�(�A�9XA��yA� �A�K�A��A��A�
=A�;dA�{A{�7Ay�hAv  Aq�AnbAiAg+Ac��AbffAaG�A`=qAY�AW�7AU?}AT1AR�DAP��AMG�AL  AJ�DAH�jAEK�AC��ACdZABbNA@ffA?�;A?�FA?�A>A�A:�jA8z�A6�9A3/A2ĜA2A0��A0ZA/dZA. �A-oA,�jA)��A(�HA(VA'C�A%�A$�jA#�A#XA"ȴA!�A�A��AbNA��AVA�FA��A�DA9XA��A�A=qA�mA�#A�FAVA��AA/A�A(�A/AAƨA�-A��A%Av�A��A�A�-A
=A
z�A	l�AȴAn�A5?A�TAl�A�^A�@�ƨ@�Ĝ@�|�@�?}@�7L@�
=@��#@�Z@�=q@��@�|�@�@�Q�@�(�@�-@�D@���@�@�h@�`B@�V@�9@�z�@�1'@�\)@���@��`@܃@�9X@�(�@�b@�1@���@ۥ�@�V@���@�x�@�7L@���@׾w@��@��@�I�@Ӆ@�@�5?@Ѳ-@�%@�Ĝ@�j@� �@�ƨ@�+@��H@�M�@�p�@̛�@�r�@�Z@�1@�t�@�;d@��@ʏ\@�E�@���@ɩ�@�X@��@�%@���@��@���@�r�@�A�@�1'@� �@�1@ǥ�@�\)@�o@���@�{@�J@�@��@���@š�@�X@��/@�bN@��m@��@�ff@�{@���@�G�@��9@�bN@���@�
=@���@�E�@�@�x�@�V@���@�t�@��!@�~�@�V@���@�hs@�Ĝ@��m@�o@��@��H@���@�5?@�&�@���@��@��;@��w@�dZ@��@�ff@��h@�`B@��/@�Q�@��@��@�;d@�"�@��y@��H@��H@��@��@��@��\@�5?@�J@��-@��@�hs@�G�@��@���@��;@���@�dZ@��@��y@��@���@��!@���@�ff@�=q@�J@��7@�O�@�?}@��@�j@�1'@�1'@�(�@�ƨ@�C�@�
=@��H@���@��!@�V@�E�@�J@���@�7L@��`@��j@� �@�l�@��y@��+@�@��#@��h@�7L@��/@���@�z�@�bN@�I�@��
@���@�dZ@�C�@�+@�o@�@���@��@���@��+@���@��h@�X@�X@�O�@�X@�X@�X@�X@�X@�X@�X@�X@�X@�X@�O�@�G�@�?}@�&�@���@��j@��@�Z@� �@�b@�1@�  @��;@�dZ@��R@�V@��@���@�p�@�G�@�V@�j@�ƨ@���@�dZ@�;d@��R@�5?@��@��-@��h@�O�@���@�Ĝ@��u@��D@�z�@�r�@�Z@�(�@���@��m@��
@���@�t�@�K�@�
=@��@��!@�{@��7@�G�@��@�1'@��m@��m@��m@��;@��
@���@��w@��P@�t�@�l�@�l�@�dZ@�dZ@�S�@�K�@�K�@�K�@�K�@�=q@��m@~�+@r~�@co@\Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�I�AՕ�A�oA԰!A�|�A�?}A�%A�ƨA�bNA�ĜA�S�A�JA��#Aѧ�AсA�ZA���A�XA�M�A�9XA�bNA��
A�bA��A� �A�=qA�r�A�
=AőhAċDAô9A���A�=qA��A��RA�E�A��jA�(�A�$�A�A��A��mA�|�A�O�A�=qA��A�ĜA�VA�C�A�A�ĜA�|�A� �A��A�dZA��yA��A��HA�l�A��#A� �A��A�K�A���A�v�A���A���A�Q�A�1'A���A�E�A��!A��RA��A��/A�%A��-A�
=A�VA��!A�XA���A���A�ĜA�A��A���A��/A��A�O�A��A��A�(�A�9XA��yA� �A�K�A��A��A�
=A�;dA�{A{�7Ay�hAv  Aq�AnbAiAg+Ac��AbffAaG�A`=qAY�AW�7AU?}AT1AR�DAP��AMG�AL  AJ�DAH�jAEK�AC��ACdZABbNA@ffA?�;A?�FA?�A>A�A:�jA8z�A6�9A3/A2ĜA2A0��A0ZA/dZA. �A-oA,�jA)��A(�HA(VA'C�A%�A$�jA#�A#XA"ȴA!�A�A��AbNA��AVA�FA��A�DA9XA��A�A=qA�mA�#A�FAVA��AA/A�A(�A/AAƨA�-A��A%Av�A��A�A�-A
=A
z�A	l�AȴAn�A5?A�TAl�A�^A�@�ƨ@�Ĝ@�|�@�?}@�7L@�
=@��#@�Z@�=q@��@�|�@�@�Q�@�(�@�-@�D@���@�@�h@�`B@�V@�9@�z�@�1'@�\)@���@��`@܃@�9X@�(�@�b@�1@���@ۥ�@�V@���@�x�@�7L@���@׾w@��@��@�I�@Ӆ@�@�5?@Ѳ-@�%@�Ĝ@�j@� �@�ƨ@�+@��H@�M�@�p�@̛�@�r�@�Z@�1@�t�@�;d@��@ʏ\@�E�@���@ɩ�@�X@��@�%@���@��@���@�r�@�A�@�1'@� �@�1@ǥ�@�\)@�o@���@�{@�J@�@��@���@š�@�X@��/@�bN@��m@��@�ff@�{@���@�G�@��9@�bN@���@�
=@���@�E�@�@�x�@�V@���@�t�@��!@�~�@�V@���@�hs@�Ĝ@��m@�o@��@��H@���@�5?@�&�@���@��@��;@��w@�dZ@��@�ff@��h@�`B@��/@�Q�@��@��@�;d@�"�@��y@��H@��H@��@��@��@��\@�5?@�J@��-@��@�hs@�G�@��@���@��;@���@�dZ@��@��y@��@���@��!@���@�ff@�=q@�J@��7@�O�@�?}@��@�j@�1'@�1'@�(�@�ƨ@�C�@�
=@��H@���@��!@�V@�E�@�J@���@�7L@��`@��j@� �@�l�@��y@��+@�@��#@��h@�7L@��/@���@�z�@�bN@�I�@��
@���@�dZ@�C�@�+@�o@�@���@��@���@��+@���@��h@�X@�X@�O�@�X@�X@�X@�X@�X@�X@�X@�X@�X@�X@�O�@�G�@�?}@�&�@���@��j@��@�Z@� �@�b@�1@�  @��;@�dZ@��R@�V@��@���@�p�@�G�@�V@�j@�ƨ@���@�dZ@�;d@��R@�5?@��@��-@��h@�O�@���@�Ĝ@��u@��D@�z�@�r�@�Z@�(�@���@��m@��
@���@�t�@�K�@�
=@��@��!@�{@��7@�G�@��@�1'@��m@��m@��m@��;@��
@���@��w@��P@�t�@�l�@�l�@�dZ@�dZ@�S�@�K�@�K�@�K�@�K�@�=q@��m@~�+@r~�@co@\Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B+B9XBE�BR�BZB^5BaHBdZBjBl�Be`BbNB]/BXBVBT�BL�BA�BH�BL�B]/BiyB|�B��B�B�3B�wBȴB�ZB�BB�B��B�TB��B�B�mB�HB�ZB�
B�B�NB�B�B1B=qB[#BM�Bn�B��B��B��B��B��B��B��B�B�B�B��B��B��B��B�hB�JB�Bx�Bk�BgmBaHB[#BL�B@�B;dB+B�B�B	7B��B��B�5B��B�9B��B�oB�JB�1B�Bl�BJ�B�B
�B
��B
�^B
��B
��B
��B
z�B
A�B
DB	�ZB	�B	�}B	�FB	��B	�B	jB	N�B	G�B	M�B	5?B	+B	"�B	+B�B�fB�yB��B��BB�jB�qB�dB�wB�jB�dB�RB�?B�3B�-B�B��B��B��B��B�hB�\B�oB��B�{B�hB�uB�{B�VB��B��B��B��B��B��B��B��B�uB�bB�hB�hB�PB�=B�+B�B}�B{�B|�B~�B� B�B�B�B� B� B�B�B�B�+B�PB�DB�DB�\B�bB�bB�hB�bB�uB�bB�bB�bB�oB��B�hB�hB�hB�\B�=B�hB�bB��B��B��B�oB��B��B��B��B��B��B��B�{B��B�{B��B��B��B��B��B��B��B��B�B�B�B�9B�LB�^B�XB�XB�XB�XB�XB�^B�wB�}B��B��BBǮBɺB��B�B�B�B�5B�BB�`B�ZB�ZB�mB�sB�B�B�B��B��B��B��B��B��B	B	B	+B		7B	JB	VB	\B	bB	hB	hB	hB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	#�B	#�B	#�B	$�B	%�B	&�B	)�B	,B	.B	1'B	49B	5?B	5?B	7LB	:^B	;dB	>wB	C�B	D�B	F�B	G�B	G�B	H�B	H�B	K�B	O�B	P�B	Q�B	R�B	S�B	XB	ZB	^5B	_;B	_;B	_;B	bNB	gmB	jB	jB	l�B	l�B	m�B	r�B	r�B	v�B	x�B	{�B	{�B	}�B	�B	�B	�+B	�%B	�1B	�7B	�DB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�9B	�FB	�FB	�FB	�RB	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�}B	��B	ÖB	ŢB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�`B	�`B	�`B	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
{B
�B
,B
2-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B/B<jBG�BS�B[#B_;BbNBffBm�Bn�BffBcTB^5BYBW
BVBM�BA�BH�BO�B_;Bl�B�B��B�B�FB��B��B�mB�BDB �B��B�BB��B�B�mB�B�)B�;B�ZB�B�B	7B?}B]/BN�Bq�B��B��B��B��B��B��B��B�B�!B�B�B��B��B��B�uB�bB�+B{�Bl�BiyBdZB^5BQ�BG�BD�B0!B!�B�BPBBB�sBƨB�jB��B��B�oB�\B�=Bv�BT�B1'B
��B
��B
B
��B
��B
��B
�B
T�B
�B	�yB	�`B	ĜB	�}B	��B	�PB	v�B	W
B	Q�B	P�B	8RB	.B	2-B	PB��B�yB�B�B�/BŢB�}BBÖBB�wB�wB�wB�LB�9B�3B�9B�B��B��B��B�uB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�JB�=B�+B�B|�B}�B� B�B�B�B�B�B�B�B�+B�%B�7B�bB�\B�JB�bB�hB�oB�uB�uB��B��B�oB�oB��B��B�oB�oB�oB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�FB�RB�dB�XB�XB�XB�XB�^B�jB�}B��BBBĜBɺB��B��B�
B�B�B�;B�HB�fB�`B�`B�sB�yB�B�B�B��B��B��B��B��B	  B	B	B	1B	
=B	PB	\B	bB	bB	hB	hB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	#�B	#�B	#�B	$�B	&�B	'�B	+B	-B	0!B	2-B	5?B	6FB	6FB	8RB	;dB	<jB	?}B	D�B	E�B	G�B	H�B	H�B	I�B	J�B	L�B	O�B	P�B	R�B	S�B	T�B	ZB	[#B	^5B	_;B	_;B	`BB	dZB	hsB	k�B	k�B	l�B	m�B	n�B	s�B	r�B	v�B	y�B	|�B	|�B	~�B	�B	�B	�1B	�%B	�1B	�7B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�FB	�FB	�LB	�XB	�^B	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�qB	�qB	�qB	�qB	��B	��B	ĜB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�B	�B	�B	�B	�)B	�/B	�;B	�;B	�BB	�BB	�BB	�NB	�NB	�TB	�ZB	�fB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
{B
�B
,B
33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<D��<#�
<#�
<#�
<#�
<#�
<#�
<���<D��<#�
<49X<#�
<#�
<49X<49X<D��<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451562012011014515620120110145156  AO  ARGQ                                                                        20111130143609  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143609  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145156  IP                  G�O�G�O�G�O�                