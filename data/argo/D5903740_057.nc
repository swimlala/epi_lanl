CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:34Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230834  20160531170815  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               9A   AO  4055_7112_057                   2C  D   APEX                            5374                            041511                          846 @ּҙ8�1   @ּ�;��@:n��O��c�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    9A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dx��D�fD�<�D�� D���D��D�9�D���D���D�	�D�9�D���Dǳ3D��D�S3Dډ�D��3D�  D�FfD�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A��A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B8BA(�BI(�BQ(�BY(�Ba�\Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{B�ǮB��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLc�CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dx�\D��D�FD��HD���D�D�B�D��D��D��D�B�D��DǼ{D�"�D�\{Dڒ�D��{D�	HD�O�D�vD�Ϯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�"�A� �A� �A��A��A��A��A� �A��A��A��A��A��A��A��A�JA���A���A���A���Aө�A�dZA�G�A�?}A�A�A���A���A��A�?}A���A�A�/A�5?A�t�A���A�{A�;dA��A�ZA��A��+A��`A��A���A���A�hsA��jA�^5A�(�A���A�r�A���A���A�l�A��uA�1A���A�A�hsA���A�5?A��TA�M�A���A��/A��A���A��A�E�A�Q�A��jA�"�A�x�A��-A�"�A�I�A�~�A�~�A�hsA�-A��A���A�l�A�A���A��A��!A��#A�z�A���A�  A�dZA���A�$�A���A�?}A�ƨA�bA��HA�ffA���A�JA~�A|�A{dZAy�;Av(�AtjAr�Aq�mAqC�Ap��An��Al(�AkVAi��Ah{AfĜAeC�Ac%A`Q�A\�RAZ�HAY�#AX��AW�AU�AQ`BAO/AMoALE�AK�AKoAI��AIx�AHbNAF��AE��AD��AC��AA�mA>E�A<�yA<�A;+A:�!A9�;A9
=A8�9A7p�A4��A45?A3��A3`BA2��A2��A2��A0�yA/t�A.�RA-�;A,ĜA+�
A*�yA*��A*jA)�TA(bNA&��A&JA%/A#A#7LA"^5A!��A �HA   A�A��A~�A��Ap�A��A�AoA�;A�RAZAA�#A��A?}AA�A�7A&�A�HAE�A33A�A-A��AK�A�`A�A9XAAȴA��A
��AĜA�A�A�`A��A��A�A  AO�A��AbNAM�A1AK�A ��@�-@�Ĝ@��w@�o@��^@�%@�t�@�ƨ@���@�v�@�E�@�7L@��#@�(�@��y@��@�`B@��@���@�9@�1@�w@�+@�M�@�G�@�A�@�+@�-@�r�@�33@��#@ۍP@�n�@�=q@���@�hs@��@�Ĝ@أ�@�A�@׶F@�ȴ@�=q@���@�?}@�9X@�{@�Ĝ@�Z@϶F@�33@��T@̓u@�\)@�O�@�E�@Ĭ@���@�G�@��/@�v�@�X@�1'@�1@��@��@��@�~�@�X@�A�@��@��#@��@���@���@���@�^5@�M�@�$�@��@�@�G�@�t�@�=q@���@��P@���@�$�@��@��u@��@��@���@�S�@�C�@���@��!@���@�ff@�J@�/@�j@�l�@��+@�E�@�{@�x�@���@�1'@��P@�
=@��@��R@�$�@���@��-@���@�7L@���@���@�  @��@��@��\@�M�@�5?@�-@�$�@��@���@���@���@�&�@���@���@���@�A�@���@���@�v�@���@��7@�`B@�?}@��@�%@�%@��/@�j@�A�@�ƨ@�t�@�+@�
=@��@���@�~�@�M�@�{@���@��-@�&�@�r�@�(�@�  @��m@���@�C�@�~�@��^@�hs@�&�@�V@�Ĝ@���@���@��u@�j@�b@�ƨ@���@�|�@�l�@�C�@��@��!@�~�@�^5@�E�@�@�7L@���@��/@��/@��u@�(�@���@��
@���@��F@���@���@���@�S�@�K�@�;d@�o@��\@�M�@�$�@�@��-@��-@��-@���@���@�`B@�&�@���@��j@�bN@�(�@�(�@�  @�@;d@~E�@}�@|j@{ƨ@z�@z�\@z~�@z=q@y��@yG�@y�@x�`@x�u@x �@w�@w�w@w�@w�P@w|�@w;d@v�R@vE�@v5?@v$�@v{@u�T@u�h@up�@u�@t�@t�j@t(�@s33@q�#@q��@pĜ@pr�@nff@f��@_�w@V�R@O��@Ihs@DZ@?;d@<�@5p�@0Q�@+33@&�@!��@��@{@�@��@	�7@V@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�"�A� �A� �A��A��A��A��A� �A��A��A��A��A��A��A��A�JA���A���A���A���Aө�A�dZA�G�A�?}A�A�A���A���A��A�?}A���A�A�/A�5?A�t�A���A�{A�;dA��A�ZA��A��+A��`A��A���A���A�hsA��jA�^5A�(�A���A�r�A���A���A�l�A��uA�1A���A�A�hsA���A�5?A��TA�M�A���A��/A��A���A��A�E�A�Q�A��jA�"�A�x�A��-A�"�A�I�A�~�A�~�A�hsA�-A��A���A�l�A�A���A��A��!A��#A�z�A���A�  A�dZA���A�$�A���A�?}A�ƨA�bA��HA�ffA���A�JA~�A|�A{dZAy�;Av(�AtjAr�Aq�mAqC�Ap��An��Al(�AkVAi��Ah{AfĜAeC�Ac%A`Q�A\�RAZ�HAY�#AX��AW�AU�AQ`BAO/AMoALE�AK�AKoAI��AIx�AHbNAF��AE��AD��AC��AA�mA>E�A<�yA<�A;+A:�!A9�;A9
=A8�9A7p�A4��A45?A3��A3`BA2��A2��A2��A0�yA/t�A.�RA-�;A,ĜA+�
A*�yA*��A*jA)�TA(bNA&��A&JA%/A#A#7LA"^5A!��A �HA   A�A��A~�A��Ap�A��A�AoA�;A�RAZAA�#A��A?}AA�A�7A&�A�HAE�A33A�A-A��AK�A�`A�A9XAAȴA��A
��AĜA�A�A�`A��A��A�A  AO�A��AbNAM�A1AK�A ��@�-@�Ĝ@��w@�o@��^@�%@�t�@�ƨ@���@�v�@�E�@�7L@��#@�(�@��y@��@�`B@��@���@�9@�1@�w@�+@�M�@�G�@�A�@�+@�-@�r�@�33@��#@ۍP@�n�@�=q@���@�hs@��@�Ĝ@أ�@�A�@׶F@�ȴ@�=q@���@�?}@�9X@�{@�Ĝ@�Z@϶F@�33@��T@̓u@�\)@�O�@�E�@Ĭ@���@�G�@��/@�v�@�X@�1'@�1@��@��@��@�~�@�X@�A�@��@��#@��@���@���@���@�^5@�M�@�$�@��@�@�G�@�t�@�=q@���@��P@���@�$�@��@��u@��@��@���@�S�@�C�@���@��!@���@�ff@�J@�/@�j@�l�@��+@�E�@�{@�x�@���@�1'@��P@�
=@��@��R@�$�@���@��-@���@�7L@���@���@�  @��@��@��\@�M�@�5?@�-@�$�@��@���@���@���@�&�@���@���@���@�A�@���@���@�v�@���@��7@�`B@�?}@��@�%@�%@��/@�j@�A�@�ƨ@�t�@�+@�
=@��@���@�~�@�M�@�{@���@��-@�&�@�r�@�(�@�  @��m@���@�C�@�~�@��^@�hs@�&�@�V@�Ĝ@���@���@��u@�j@�b@�ƨ@���@�|�@�l�@�C�@��@��!@�~�@�^5@�E�@�@�7L@���@��/@��/@��u@�(�@���@��
@���@��F@���@���@���@�S�@�K�@�;d@�o@��\@�M�@�$�@�@��-@��-@��-@���@���@�`B@�&�@���@��j@�bN@�(�@�(�@�  @�@;d@~E�@}�@|j@{ƨ@z�@z�\@z~�@z=q@y��@yG�@y�@x�`@x�u@x �@w�@w�w@w�@w�P@w|�@w;d@v�R@vE�@v5?@v$�@v{@u�T@u�h@up�@u�@t�@t�j@t(�@s33@q�#@q��@pĜ@pr�@nff@f��@_�w@V�R@O��@Ihs@DZ@?;d@<�@5p�@0Q�@+33@&�@!��@��@{@�@��@	�7@V@M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�PB�PB�PB�JB�PB�PB�PB�JB�PB�PB�JB�JB�JB�PB�PB�PB�JB�DB�DB�DB�7B{�B�B��Bz�Be`BQ�B>wB;dB6FB9XB;dBD�BdZBcTBYBVB_;B_;B^5B^5B^5B_;B\)BW
BO�BE�B=qB:^B7LB2-B-B$�B�BBB��B�B�HB�/B��B�RB�B��B��B��B�oB�%B|�Bx�BhsBS�BG�B:^B)�B�B
=B��B�TB��BɺBÖB�qB�FB��Bl�BS�B;dB'�BPB
�HB
��B
B
�9B
�B
��B
��B
�\B
�B
k�B
R�B
F�B
<jB
.B
�B
hB
B	�sB	�#B	��B	ȴB	ÖB	�qB	�B	��B	�{B	�=B	~�B	u�B	jB	ZB	F�B	1'B	%�B	�B	�B	VB	B�B�fB�5B�B�B��B��BɺBƨBB��B�qB�^B�9B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�oB�hB�\B�\B�VB�PB�VB�PB�DB�B~�B{�Bw�Bu�Bs�Br�Bp�Bm�Bk�BiyBhsBgmBe`BcTBbNB^5B[#BXBVBT�BT�BS�BR�BP�BN�BM�BL�BJ�BH�BE�BB�B?}B=qB<jB:^B9XB7LB5?B2-B0!B,B)�B'�B&�B%�B$�B"�B �B�B�B�B�B�B�B�B�BuBuBoBhBbB\BVBuBuBoBbBJB
=B1B1B+B+B1BDB
=B	7B1B+B%B%BBBBBBB%B+B+B%B+B+B+B+B+B%B+B+B+B+B1B+B1B1B1B1B	7B
=BJBbBDB	7B	7B
=BoB�B�B�B�B�B�B�B�B �B"�B#�B'�B.B.B1'B33B6FB7LB8RB;dB>wB=qB>wB<jB:^B:^B9XB;dB?}BA�BC�BD�BF�BK�BR�B[#B`BBaHBaHBbNBcTBcTBcTBdZBe`BffBiyBo�Bu�Bz�B�B�DB�VB�bB�uB��B��B��B��B��B��B�B�B�-B�9B�?B�?B�?B�?B�FB�LB�RB�dB�jB�qB�wB��BĜBɺB��B��B��B��B�B�
B�B�
B�B�#B�)B�BB�NB�ZB�`B�mB�yB�B�B�B�B�B�B��B��B��B��B��B	  B	%B	DB	VB	bB	bB	uB	{B	{B	{B	�B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	(�B	)�B	)�B	,B	33B	5?B	6FB	6FB	8RB	=qB	?}B	@�B	@�B	A�B	B�B	B�B	C�B	F�B	F�B	F�B	H�B	L�B	O�B	Q�B	W
B	YB	XB	YB	YB	YB	[#B	^5B	`BB	cTB	gmB	jB	jB	l�B	n�B	p�B	v�B	z�B	�B	�B	�1B	�7B	�=B	�DB	�VB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�!B	�3B	�?B	�jB	�#B	�B
  B
	7B
{B
�B
&�B
+B
7LB
?}B
D�B
L�B
R�B
ZB
aHB
e`B
iyB
m�B
r�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�EB�HB�EB�BB�JB�DB�DB�@B�EB�DB�BB�@B�@B�DB�EB�EB�AB�9B�9B�9B�/B{�B�B�Bz�BeLBQ�B>dB;MB61B9AB;LBD�BdGBcBBYBU�B_%B_(B^ B^B^B_&B\BV�BO�BE�B=[B:HB78B2B,�B$�BgB�B �B��B�B�0B�B̳B�8B�B��B��B��B�SB�B|�Bx�BhUBS�BG�B:AB)�B�B
 B��B�7BνBɡB�{B�VB�*B��BloBS�B;JB'�B6B
�.B
ͷB
�uB
�B
��B
��B
�yB
�DB
��B
kmB
R�B
F�B
<SB
. B
�B
PB
�B	�`B	�B	��B	ȟB	ÁB	�^B	�	B	��B	�jB	�+B	~�B	u�B	jmB	ZB	F�B	1B	%�B	�B	}B	IB	�B�B�ZB�(B�B��B��B��BɭBƛBB�uB�dB�RB�-B�B�B�	B��B��B��B��B��B��B��B��B��B��B��B��B��B�vB�bB�]B�RB�SB�IB�EB�JB�FB�9B�B~�B{�Bw�Bu�Bs�Br�Bp�Bm�Bk|BioBhhBgcBeVBcIBbCB^*B[BXBU�BT�BT�BS�BR�BP�BN�BM�BL�BJ�BH�BE�BB�B?rB=OB<bB:UB9RB7BB55B2$B0B, B)�B'�B&�B%�B$�B"�B �B�B�B�B�B�BvBjB^BPBmBKBEB>B9B2BnBlBgB>B'B
BB)BBBBB
B	BBB�BB�B�B�BB�B�B B!BB BB!BBBBBBB BB&B BB(B)BB	-B
B#BXB8B	B	B
2BcB�B�B�B�B�B�B�B�B �B"�B#�B'�B.B.B1B3&B66B7@B8EB;WB>jB=cB>hB<]B:PB:RB9LB;XB?mBAzBC�BD�BF�BK�BR�B[B`1Ba7Ba8Bb>BcDBcCBcCBdHBeNBfUBiiBo�Bu�Bz�B�B�3B�CB�QB�cB�tB��B��B��B��B��B��B�B�B�$B�+B�,B�*B�,B�2B�9B�;B�MB�XB�[B�cB�lBćBɣB̷B��B��B��B��B��B��B��B��B�B�B�*B�9B�EB�IB�VB�cB�hB�tB�{B�{B�B�B��B��B��B��B��B��B	B	+B	=B	JB	GB	\B	cB	bB	bB	iB	vB	�B	�B	�B	 �B	!�B	$�B	&�B	(�B	)�B	)�B	+�B	3B	5%B	6-B	6-B	8:B	=UB	?cB	@jB	@kB	AqB	BuB	BuB	C{B	F�B	F�B	F�B	H�B	L�B	O�B	Q�B	V�B	X�B	W�B	X�B	X�B	X�B	[B	^B	`(B	c9B	gQB	jdB	jdB	lpB	n}B	p�B	v�B	z�B	��B	��B	�B	�B	� B	�(B	�8B	�FB	�EB	�MB	�ZB	�fB	�iB	�rB	�pB	�qB	�xB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�KB	�B	�pB	��B
	B
ZB
�B
&�B
*�B
7*B
?[B
DzB
L�B
R�B
Y�B
a$B
e@B
iVB
mpB
r�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708152016053117081520160531170815  AO  ARCAADJP                                                                    20140721230834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230834  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230834  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170815  IP                  G�O�G�O�G�O�                