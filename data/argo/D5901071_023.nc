CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:57Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135625  20190522121825  1727_5046_023                   2C  D   APEX                            2143                            040306                          846 @�3hlG�1   @�3i���@7m�hr�!�c��^5?}1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @&ff@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr�fDs  Ds� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@l��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bn��Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�B�ffB˙�Bϙ�B���Bי�Bۙ�Bߙ�B㙚B癚B뙚B���B�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�fCI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D��Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;��D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI��DJl�DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dly�Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dql�Dq�3Dry�Dr�3Dss31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A��TA�dZA�$�A��mA��#A�ƨA���Aϡ�A�x�A�VA�-A��A���A��TA�ffAŏ\Aħ�A�9XA�XA��^A���A�+A��^A� �A��jA��#A�ffA��
A�+A��wA��A�7LA���A�?}A�dZA�bA��A��A��FA�+A��A�A��A�ĜA�dZA��jA��7A�l�A���A��\A�VA��A��A���A��7A��;A��FA��A��^A���A�l�A��A��A���A���A��9A��wA��A�bNA��A�^5A�{A�I�A�1A�1'A��A�33A� �A�/A���A�1A�JA�ffA�ffA�bA�l�A�jA�5?A��
A�ĜA��hA�A�A}�-A{�-A{�Ay��AxE�Aw��AvE�Au&�At�DAr�RAq�wAo��An�An�DAm��Al$�Aj~�Ai��Ai�AhZAh�Ag�hAf�yAd1'Ab1'Aap�Aa+A_�A]�AZ��AZ$�AY�AX��AW�AV�/AU��ATE�ASAS;dAR�`AR��AQ%AO�
AN�DAL$�AKVAI&�AG��AGK�AF~�AE�AD  AC33AA��A@A?�A>��A> �A=/A<9XA;33A:  A9`BA8�RA7t�A6bNA5�;A5�;A5�hA4$�A3;dA2�A2�A1S�A0�RA.�A.9XA-x�A+�PA*�RA*ZA)��A)+A((�A'�wA'G�A&ȴA&bA%?}A$��A$jA$9XA#�;A#C�A"1'A!�PA!\)A!�A �!A 1A�PAffA��A�A�A�/AƨA�A%A�`A~�A  AĜA  A��A��A��A��A�Av�A(�A�TA�-A|�A7LA��A|�A�/A�A
��A
(�A	�7A	33A��AJAC�A�A��A�AA��A��A�A r�@���@��;@���@���@� �@���@�o@��@���@��@��@�o@�@�9X@��@��@��@噚@�Q�@�\)@�-@�?}@���@�Q�@��@���@ߍP@ޟ�@�{@���@�|�@�ȴ@ّh@׾w@�(�@�+@�%@�C�@�p�@̃@�  @˶F@�5?@�l�@�5?@��@�^5@ǅ@ʸR@�bN@˥�@�M�@�O�@��`@�t�@�^5@ũ�@���@���@�?}@��`@��@�^5@��u@�K�@�hs@�33@�{@���@�x�@�G�@�%@��D@�ƨ@�@��@�1@��w@���@�K�@�~�@�"�@�&�@���@��@���@��@���@��w@���@�I�@�=q@�bN@���@�
=@��+@���@���@���@��@�~�@��+@�ff@��^@���@���@��P@�v�@���@�b@�b@��@�t�@���@�V@�-@�-@�$�@�J@���@��-@�5?@�%@�ƨ@��@��!@��+@�ȴ@���@�V@�bN@� �@��m@�t�@�  @�x�@���@���@��`@�x�@�p�@���@�Z@�A�@�9X@��@�1@��m@��;@��@�1'@�  @��;@�33@�v�@��T@��@�/@�bN@� �@��@�C�@��@��@���@��R@�M�@��@�p�@�7L@��D@�1@��@�\)@�;d@�;d@�;d@�;d@�+@���@��@�ȴ@���@��R@���@�^5@�-@�$�@��@�J@���@���@��^@���@�7L@�Ĝ@�Z@�9X@�1@��m@���@���@�"�@��y@��@���@���@�v�@�ff@�M�@�=q@�-@��@�{@���@��@��^@��@�?}@��@��@���@�z�@�Z@�I�@�(�@��@���@��
@�ƨ@���@�t�@�;d@��@��!@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A��TA�dZA�$�A��mA��#A�ƨA���Aϡ�A�x�A�VA�-A��A���A��TA�ffAŏ\Aħ�A�9XA�XA��^A���A�+A��^A� �A��jA��#A�ffA��
A�+A��wA��A�7LA���A�?}A�dZA�bA��A��A��FA�+A��A�A��A�ĜA�dZA��jA��7A�l�A���A��\A�VA��A��A���A��7A��;A��FA��A��^A���A�l�A��A��A���A���A��9A��wA��A�bNA��A�^5A�{A�I�A�1A�1'A��A�33A� �A�/A���A�1A�JA�ffA�ffA�bA�l�A�jA�5?A��
A�ĜA��hA�A�A}�-A{�-A{�Ay��AxE�Aw��AvE�Au&�At�DAr�RAq�wAo��An�An�DAm��Al$�Aj~�Ai��Ai�AhZAh�Ag�hAf�yAd1'Ab1'Aap�Aa+A_�A]�AZ��AZ$�AY�AX��AW�AV�/AU��ATE�ASAS;dAR�`AR��AQ%AO�
AN�DAL$�AKVAI&�AG��AGK�AF~�AE�AD  AC33AA��A@A?�A>��A> �A=/A<9XA;33A:  A9`BA8�RA7t�A6bNA5�;A5�;A5�hA4$�A3;dA2�A2�A1S�A0�RA.�A.9XA-x�A+�PA*�RA*ZA)��A)+A((�A'�wA'G�A&ȴA&bA%?}A$��A$jA$9XA#�;A#C�A"1'A!�PA!\)A!�A �!A 1A�PAffA��A�A�A�/AƨA�A%A�`A~�A  AĜA  A��A��A��A��A�Av�A(�A�TA�-A|�A7LA��A|�A�/A�A
��A
(�A	�7A	33A��AJAC�A�A��A�AA��A��A�A r�@���@��;@���@���@� �@���@�o@��@���@��@��@�o@�@�9X@��@��@��@噚@�Q�@�\)@�-@�?}@���@�Q�@��@���@ߍP@ޟ�@�{@���@�|�@�ȴ@ّh@׾w@�(�@�+@�%@�C�@�p�@̃@�  @˶F@�5?@�l�@�5?@��@�^5@ǅ@ʸR@�bN@˥�@�M�@�O�@��`@�t�@�^5@ũ�@���@���@�?}@��`@��@�^5@��u@�K�@�hs@�33@�{@���@�x�@�G�@�%@��D@�ƨ@�@��@�1@��w@���@�K�@�~�@�"�@�&�@���@��@���@��@���@��w@���@�I�@�=q@�bN@���@�
=@��+@���@���@���@��@�~�@��+@�ff@��^@���@���@��P@�v�@���@�b@�b@��@�t�@���@�V@�-@�-@�$�@�J@���@��-@�5?@�%@�ƨ@��@��!@��+@�ȴ@���@�V@�bN@� �@��m@�t�@�  @�x�@���@���@��`@�x�@�p�@���@�Z@�A�@�9X@��@�1@��m@��;@��@�1'@�  @��;@�33@�v�@��T@��@�/@�bN@� �@��@�C�@��@��@���@��R@�M�@��@�p�@�7L@��D@�1@��@�\)@�;d@�;d@�;d@�;d@�+@���@��@�ȴ@���@��R@���@�^5@�-@�$�@��@�J@���@���@��^@���@�7L@�Ĝ@�Z@�9X@�1@��m@���@���@�"�@��y@��@���@���@�v�@�ff@�M�@�=q@�-@��@�{@���@��@��^@��@�?}@��@��@���@�z�@�Z@�I�@�(�@��@���@��
@�ƨ@���@�t�@�;d@��@��!@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BJ�BɺB�
B��B��B��B��B��B��B��B��B��B��B�LB�'B�dB�XB��BĜB��B�}B��B��B��B��B��B��B��B��B��B��B�B��B�)B�B��BĜB�qB�XB�RB�RB�FB�!B��B��B��B�hB�B�B�B�B~�B{�Bx�Br�BVBC�B8RB'�B�B�BuBhB\B
=B��B�`B�BɺB�wB�B��Bo�BS�BJ�BF�BD�B8RB/B)�B$�BPB
�B
��B
�jB
�-B
��B
�uB
�\B
�PB
�7B
�B
r�B
l�B
hsB
_;B
VB
Q�B
I�B
J�B
G�B
@�B
9XB
0!B
+B
'�B
 �B
�B
\B

=B
B	��B	��B	�B	�B	�#B	��B	ƨB	ŢB	�RB	�B	��B	��B	��B	��B	��B	�{B	�JB	�DB	�\B	�uB	�oB	�bB	�B	w�B	k�B	XB	M�B	A�B	9XB	33B	+B	!�B	 �B	#�B	�B	JB	+B	B	B��B��B��B��B��B�B�B�B�B�B�B�B�sB�fB�ZB�NB�5B�B�B��B��BȴBƨBĜB��B�wB�qB�dB�RB�FB�9B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�%B�B�B�B~�B{�Bz�Bx�Bw�Bv�Bt�Bq�Bo�Bn�Bn�Bm�Bl�Bk�BhsBffBcTBaHBbNB`BB_;B]/B\)BZBW
BT�BQ�BN�BM�BK�BJ�BH�BF�BD�BB�B@�B@�B?}B?}B>wB=qB;dB8RB8RB7LB6FB5?B6FB6FB5?B33B49B33B49B5?B7LB8RB9XB9XB8RB9XB9XB:^B;dB;dB;dB<jB?}B>wB?}B@�BA�BA�BA�B@�B?}B>wB@�BA�BH�B^5B�+B�{B�{B�{B�uB�{B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�JB�7B�+B�+B�+B�+B�+B�PB�1B�1B�=B�DB�PB�bB��B��B��B��B��B��B��B��B�B�B�B�B�-B�RB�XB�dB�dB�}B��BBƨB��B�B�BB�`B�mB�`B�fB�B�B��B��B��B��B��B��B	B	\B	�B	!�B	&�B	'�B	/B	2-B	2-B	5?B	7LB	8RB	9XB	:^B	<jB	>wB	B�B	I�B	O�B	Q�B	S�B	[#B	aHB	cTB	e`B	jB	l�B	s�B	x�B	z�B	{�B	}�B	~�B	�B	�B	�B	�B	�=B	�JB	�JB	�PB	�VB	�VB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�RB	�^B	�dB	�dB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BL�B��B�B��B��B��B��B��B��B��B��B��BŢBÖB�}B�}B�}BĜBǮBĜBÖB��B��B��B��B��B��B��B��B�
B�
B�B�
B�;B�BB��BɺBÖB�jB�jB�dB�dB�RB��B��B��B��B�7B�%B�B�B�B|�Bz�B�B\)BF�B=qB+B!�B�B{BoBbB\B1B�B�)B��BŢB�?B��B|�BYBM�BI�BJ�B=qB2-B/B1'B�BB
�B
��B
�LB
��B
��B
�\B
�VB
�DB
�1B
v�B
n�B
k�B
cTB
XB
T�B
L�B
L�B
L�B
C�B
>wB
2-B
,B
+B
%�B
�B
hB
JB
%B	��B	��B	��B	��B	�HB	��B	ǮB	��B	�wB	�LB	��B	��B	��B	��B	��B	��B	�bB	�JB	�bB	�{B	�uB	�{B	�%B	z�B	q�B	[#B	R�B	D�B	;dB	5?B	/B	$�B	#�B	'�B	�B	\B		7B	%B	B	B��B��B��B��B��B�B�B�B�B��B�B�yB�yB�fB�ZB�TB�#B�B�
B��BɺBǮBǮBĜB��B�}B�qB�dB�XB�FB�3B�-B�'B�!B�B��B��B��B��B��B��B��B��B��B��B��B�hB�\B�7B�B�B�B�B}�B{�Bz�Bx�Bw�Bv�Bu�Bp�Bo�Bo�Bn�Bm�Bm�Bl�BhsBgmBcTBe`BbNB`BB_;B^5B]/BYBXBXBR�BN�BM�BM�BJ�BJ�BI�BD�BC�BB�B@�B@�B?}B>wB@�B=qB;dB9XB9XB;dB8RB8RB8RB6FB6FB5?B6FB6FB8RB9XB:^B:^B:^B:^B;dB=qB=qB=qB>wBA�BA�BA�BB�BC�BC�BB�BB�BB�BC�B@�BA�BA�BF�BZB�B��B��B��B�{B��B��B��B��B��B��B�B�B�B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�\B�PB�1B�+B�1B�+B�%B�hB�DB�7B�DB�JB�VB�hB��B��B��B��B��B��B��B�B�B�B�!B�B�-B�RB�^B�jB�jB�}B��BBƨB��B�B�BB�mB�yB�mB�fB�B�B��B��B��B��B��B	  B	B	PB	�B	!�B	&�B	&�B	/B	49B	33B	5?B	7LB	8RB	9XB	:^B	<jB	>wB	B�B	I�B	O�B	Q�B	S�B	\)B	bNB	dZB	ffB	k�B	m�B	t�B	x�B	z�B	{�B	}�B	� B	�B	�B	�B	�B	�DB	�PB	�PB	�PB	�VB	�VB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�FB	�XB	�^B	�dB	�jB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	ÖB	ĜB	ĜB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446412012010314464120120103144641  AO  ARGQ                                                                        20111130135625  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135625  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144642  IP                  G�O�G�O�G�O�                