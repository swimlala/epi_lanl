CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-28T02:15:34Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150528021534  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               tA   AO  4055_7112_116                   2C  D   APEX                            5374                            041511                          846 @�S��?��1   @�S�Z��@<%�S����d4 ě��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    tA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�fD�FfD�|�D�� D��D�FfD���D���D�fD�FfD��fD���D�  D�@ D�|�D���D�3D�@ D�ffD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�3D�S3D���D���D��D�S3D��gD�ɚD�3D�S3D��3D�ٚD��D�L�Dډ�D�ٚD�  D�L�D�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�^5A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�^5A�\)A�ZA�ZA�\)A�\)A�\)A�^5A�\)A�ZA�M�A�C�A�33A�hsA�^5A�hsA��yA��9A�jA��
A�A���A�v�A�?}A�ƨA���A�p�A�Q�A�-A��RA�A���A��DA�7LA���A�oA�/A�jA��A��A��TA�O�A��;A�7LA���A�=qA���A�^5A���A�7LA���A��yA�ZA�7LA��hA���A�r�A��A���A�dZA�%A�l�A��!A�E�A�ĜA�G�A�"�A��#A��PA�v�A�/A��A��hA��PA���A�v�A�1A��9A��A�ffA��jA�(�A���A��mA��A��/A��7A�=qA��yA���A�K�A�  A�33A�A���A��A��/A�ĜA���A�Q�A�E�A�7LA���A�A�$�A���A���A��A���A�PA}��A|�A{�A|bA{ƨA{��A{VAzffAy��Ay?}Ax��Ax �Aw��AwAv��AvJAudZAt~�As�mAs�7Ar��Aq�-Ao�;An�Am�AmG�Al��Ak��AkC�AjI�Ai��Ah^5Af��Af=qAd��Ab�HA_��A_7LA^�RA^9XA^A]�mA\ĜA\5?A[\)AZr�AX��AV�RAU�-AT��AS�#AS`BAQ��AP$�AO&�AMt�AK;dAJ{AI�PAIoAH�!AH��AHJAF��AE�-AD�AD(�AA\)A?XA=`BA<1'A:��A8�A6r�A5hsA5?}A4ȴA45?A3t�A1�;A1C�A0�jA/�A.E�A-33A,1'A+oA*�A*M�A*5?A*1'A*{A)�A)A)��A)p�A)G�A(�!A(9XA(1A'+A&��A&JA$ĜA#�mA#�hA#C�A"��A"-A!t�A ��A ��A z�A A�A JA�PAC�A��A��A~�AI�A9XA�AdZA�A?}A�yA��AjAA�A1Ax�A$�A�jA�A�TA�A`BA�!A��A	�TA�yA1A��AjA�-A�HA��A�A?}A �@��H@��^@��@��@��7@�ȴ@�w@�R@�%@��
@�E�@�J@��@�9X@�\)@�!@��#@�-@�-@�X@�\@��@�1'@�S�@�$�@��/@�t�@���@ٲ-@�&�@�b@�|�@�;d@��@���@��@ְ!@֏\@�$�@ա�@��@�1'@�S�@Ұ!@�O�@�M�@ʰ!@ȃ@Ǯ@�K�@��H@ř�@�&�@���@ă@�I�@��@��;@�l�@�@�@��@��D@���@�&�@�M�@��/@���@�1'@�o@�X@�/@�Q�@�t�@���@�=q@�%@��;@�
=@���@�v�@��T@���@�V@���@��9@�  @��@�E�@��-@�x�@�&�@�z�@�b@���@���@���@���@�E�@���@�+@��@��-@��@�7L@��/@�Z@�I�@��@��;@�ƨ@�dZ@��@��y@���@���@��@�p�@� �@�K�@���@�~�@�^5@�M�@�M�@�V@�@�@���@��7@�7L@��@� �@��w@�|�@�dZ@�
=@��R@�E�@���@��@���@�n�@�&�@���@���@��@��u@��@�Z@� �@��m@��@��@�+@���@��y@��H@���@�^5@�X@��@��j@���@��@�C�@�"�@��!@�M�@��^@�`B@���@���@�z�@�9X@�1'@��;@�dZ@��@�n�@�M�@�E�@�J@��T@��-@�/@��@�Ĝ@���@�j@� �@��w@�+@��R@���@��\@�M�@�$�@���@��/@�b@��@l�@�@~��@}`B@|Z@{dZ@z~�@x��@x �@w�@w��@v��@v$�@u�@u�-@u�@r�\@jJ@aG�@Y�@P�u@J��@Co@>E�@:J@3o@-�@'K�@"�@��@x�@��@��@z�@
=q@
=@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�dZA�^5A�^5A�^5A�^5A�^5A�^5A�`BA�bNA�^5A�\)A�ZA�ZA�\)A�\)A�\)A�^5A�\)A�ZA�M�A�C�A�33A�hsA�^5A�hsA��yA��9A�jA��
A�A���A�v�A�?}A�ƨA���A�p�A�Q�A�-A��RA�A���A��DA�7LA���A�oA�/A�jA��A��A��TA�O�A��;A�7LA���A�=qA���A�^5A���A�7LA���A��yA�ZA�7LA��hA���A�r�A��A���A�dZA�%A�l�A��!A�E�A�ĜA�G�A�"�A��#A��PA�v�A�/A��A��hA��PA���A�v�A�1A��9A��A�ffA��jA�(�A���A��mA��A��/A��7A�=qA��yA���A�K�A�  A�33A�A���A��A��/A�ĜA���A�Q�A�E�A�7LA���A�A�$�A���A���A��A���A�PA}��A|�A{�A|bA{ƨA{��A{VAzffAy��Ay?}Ax��Ax �Aw��AwAv��AvJAudZAt~�As�mAs�7Ar��Aq�-Ao�;An�Am�AmG�Al��Ak��AkC�AjI�Ai��Ah^5Af��Af=qAd��Ab�HA_��A_7LA^�RA^9XA^A]�mA\ĜA\5?A[\)AZr�AX��AV�RAU�-AT��AS�#AS`BAQ��AP$�AO&�AMt�AK;dAJ{AI�PAIoAH�!AH��AHJAF��AE�-AD�AD(�AA\)A?XA=`BA<1'A:��A8�A6r�A5hsA5?}A4ȴA45?A3t�A1�;A1C�A0�jA/�A.E�A-33A,1'A+oA*�A*M�A*5?A*1'A*{A)�A)A)��A)p�A)G�A(�!A(9XA(1A'+A&��A&JA$ĜA#�mA#�hA#C�A"��A"-A!t�A ��A ��A z�A A�A JA�PAC�A��A��A~�AI�A9XA�AdZA�A?}A�yA��AjAA�A1Ax�A$�A�jA�A�TA�A`BA�!A��A	�TA�yA1A��AjA�-A�HA��A�A?}A �@��H@��^@��@��@��7@�ȴ@�w@�R@�%@��
@�E�@�J@��@�9X@�\)@�!@��#@�-@�-@�X@�\@��@�1'@�S�@�$�@��/@�t�@���@ٲ-@�&�@�b@�|�@�;d@��@���@��@ְ!@֏\@�$�@ա�@��@�1'@�S�@Ұ!@�O�@�M�@ʰ!@ȃ@Ǯ@�K�@��H@ř�@�&�@���@ă@�I�@��@��;@�l�@�@�@��@��D@���@�&�@�M�@��/@���@�1'@�o@�X@�/@�Q�@�t�@���@�=q@�%@��;@�
=@���@�v�@��T@���@�V@���@��9@�  @��@�E�@��-@�x�@�&�@�z�@�b@���@���@���@���@�E�@���@�+@��@��-@��@�7L@��/@�Z@�I�@��@��;@�ƨ@�dZ@��@��y@���@���@��@�p�@� �@�K�@���@�~�@�^5@�M�@�M�@�V@�@�@���@��7@�7L@��@� �@��w@�|�@�dZ@�
=@��R@�E�@���@��@���@�n�@�&�@���@���@��@��u@��@�Z@� �@��m@��@��@�+@���@��y@��H@���@�^5@�X@��@��j@���@��@�C�@�"�@��!@�M�@��^@�`B@���@���@�z�@�9X@�1'@��;@�dZ@��@�n�@�M�@�E�@�J@��T@��-@�/@��@�Ĝ@���@�j@� �@��w@�+@��R@���@��\@�M�@�$�@���@��/@�b@��@l�@�@~��@}`B@|Z@{dZ@z~�@x��@x �@w�@w��@v��@v$�@u�@u�-@u�@r�\@jJ@aG�@Y�@P�u@J��@Co@>E�@:J@3o@-�@'K�@"�@��@x�@��@��@z�@
=q@
=@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�VB�VB�VB�VB�VB�VB�\B�VB�VB�VB�VB�VB�\B�\B�\B�\B�VB�VB�VB�PB�JB�7B|�Bt�Bn�BjBdZB]/BW
BJ�BE�BK�BM�BR�BZBaHBffBiyBiyBl�Bl�Bl�BjBffB_;BZBT�BO�BI�BB�B7LB/B#�B�BoBDBB��B�B�yB�fB�)BɺB�jB�B��B��B�{B�JB�Bu�BhsB_;BVBL�BH�BC�B<jB:^B5?B/B&�B�BDB	7BB  B��B��B�B�NB�BĜB�B�uB�DB�B}�By�Bq�BjB\)BXBW
BVBQ�B9XB"�B�B�B�B�B\BB
��B
�`B
�5B
�B
ǮB
�FB
�B
��B
�FB
�9B
�3B
�!B
��B
��B
��B
��B
��B
�{B
�VB
�JB
�%B
~�B
v�B
o�B
iyB
^5B
P�B
<jB
(�B
$�B
&�B
"�B
�B
�B
VB
+B	��B	�B	�`B	�B	ƨB	�'B	�B	��B	��B	��B	��B	�{B	�VB	�+B	}�B	r�B	dZB	\)B	VB	Q�B	M�B	D�B	:^B	33B	+B	!�B	�B	�B	�B	�B	�B	bB	
=B	%B	B��B�B�B�TB�/B�B��B��BȴBǮBŢBB�}B�^B�RB�?B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�JB�DB�=B�7B�+B�%B�B�B�B�B�B�B�B�B�B�B�B�B� B|�By�Bx�Bx�Bx�Bx�Bw�Bv�Bu�Bs�Bo�Bk�BdZB]/BYBQ�BM�BG�B?}B;dB5?B1'B1'B0!B0!B.B)�B&�B$�B#�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BuB�B�B�B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B �B!�B!�B"�B"�B"�B"�B#�B#�B#�B$�B&�B&�B+B1'B49B49B49B6FB:^B:^B<jB>wB?}BA�BC�BE�BH�BH�BI�BL�BM�BP�BQ�BQ�BS�BVBW
BW
BW
BW
BXBYBYBYB^5B]/B]/BaHBgmBo�Br�Bt�Bv�Bw�By�By�By�Bz�Bz�B|�B� B�B�B�B�B�%B�PB�bB�oB�uB�uB�{B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�^BÖBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B�#B�/B�/B�5B�HB�fB�fB�yB�B�B��B��B��B��B��B��B��B	  B	B	1B	DB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	'�B	(�B	(�B	)�B	+B	,B	.B	.B	.B	/B	/B	0!B	49B	7LB	<jB	?}B	F�B	M�B	O�B	P�B	VB	XB	ZB	\)B	^5B	jB	�oB	�'B	ĜB	�TB	�B
+B
oB
�B
(�B
49B
>wB
G�B
L�B
R�B
YB
aHB
e`B
iyB
n�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�6B�6B�:B�6B�6B�6B�;B�:B�6B�:B�<B�:B�;B�>B�>B�>B�6B�:B�6B�3B�/B�B|�Bt�BnvBjZBd;B]BV�BJ�BE�BK�BM�BR�BY�Ba%BfDBiXBiXBljBljBlkBjaBfFB_BY�BT�BO�BI�BBlB7'B.�B#�BqBHBB �B��B�nB�WB�AB�BɔB�HB��B��B�jB�WB�&B��Bu�BhMB_BU�BL�BH�BCoB<BB:5B5B.�B&�BdBB	B�B��B��B��B�pB�%B��B�tB��B�LB�B��B}�By�Bq�BjYB\BW�BV�BU�BQ�B91B"�B�B�BwBXB4B�B
��B
�=B
�B
��B
ǊB
�B
��B
��B
�!B
�B
�B
��B
��B
��B
��B
�{B
�oB
�WB
�0B
�&B
� B
~�B
v�B
oxB
iXB
^B
P�B
<HB
(�B
$�B
&�B
"�B
�B
kB
3B
B	��B	�rB	�@B	��B	ƇB	�B	��B	��B	��B	��B	��B	�]B	�4B	�B	}�B	r�B	d<B	\B	U�B	Q�B	M�B	DB	:AB	3B	*�B	!�B	�B	�B	wB	kB	eB	GB	
 B	
B	�B��B�B�eB�:B�B��B��BʨBȚBǓBňB�vB�dB�GB�7B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�vB�bB�VB�EB�4B�.B�&B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B|�By�Bx�Bx�Bx�Bx�Bw�Bv�Bu�Bs�Bo�BknBd@B]BX�BQ�BM�BG�B?fB;LB5*B1B1B0B0
B-�B)�B&�B$�B#�B �B�B�B�B�BiB}BaBwBdB\BUBTBVBpBUBTBlB\BNBiBMBHBIBdBkBtBpBpBoBvBwBwBwBYBtBVBUBVBSBUBnBjBJBTB�B�B�B�B�B �B!�B!�B"�B"�B"�B"�B#�B#�B#�B$�B&�B&�B*�B1B4B4B4B6*B:BB:CB<MB>[B?`BAlBCyBE�BH�BH�BI�BL�BM�BP�BQ�BQ�BS�BU�BV�BV�BV�BV�BW�BX�BX�BX�B^B]B]Ba,BgPBo�Br�Bt�Bv�Bw�By�By�By�Bz�Bz�B|�B�B��B��B��B��B�B�/B�AB�NB�TB�SB�[B�TB�SB�^B�fB�gB�gB�mB�rB��B��B��B��B��B��B��B��B��B�B�>B�tBƃBƁBǋBȏBȑBɘBʟB˥BͯBεB��B��B��B��B��B��B��B�
B�B�B�#B�BB�BB�WB�rB�}B��B��B��B��B��B��B��B��B	�B	B	B	0B	FB	XB	_B	kB	xB	~B	�B	�B	�B	!�B	$�B	'�B	(�B	(�B	)�B	*�B	+�B	-�B	-�B	-�B	.�B	.�B	/�B	4B	7#B	<@B	?VB	FB	M�B	O�B	P�B	U�B	W�B	Y�B	\B	^B	jSB	�BB	��B	�qB	�(B	�B
�B
@B
}B
(�B
4
B
>HB
GB
L�B
R�B
X�B
aB
e1B
iJB
nfB
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150528021534    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150528021534  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150528021534  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                