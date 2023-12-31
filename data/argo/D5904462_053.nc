CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-20T02:15:50Z AOML 3.0 creation; 2016-08-07T21:51:17Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150520021550  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               5A   AO  5287_9017_053                   2C  D   APEX                            6529                            072314                          846 @�Q��7@1   @�Q�� @06�+J�d����+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    5A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B���B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`y�Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�33D�l�D��3D��D�S3D��3D�� D�fD�C3D��fD��3D�3D�9�D�i�D� D�� D�)�D�VfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BZ  Ba��Bi��Bq��By��B���B���B���B�fgB�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffC� CffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@L�CBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D� D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`4D`�4Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk4Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�4Dy�gD�&gD�@ D�y�D�� D�&gD�` D�� D���D�3D�P D��3D�� D� D�FgD�vgD��D���D�6gD�c3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AԁAԁAԃA�~�A�x�A�z�A�v�A�v�A�t�A�r�A�r�A�ZA�VA�ZA�bNA�dZA�ffA�hsA�jA�l�A�l�A�n�A�p�A�r�A�t�A�~�Aԉ7Aԟ�AԾwA��#A���A�"�A���AѾwAГuAЏ\A�C�A���A˓uA�"�A�+Aɝ�A�VA��TA�ȴA�t�A��A�l�A��yA¡�A��A� �A���A��A��/A��jA���A�  A���A���A�~�A�hsA��A�bA�bNA��hA�(�A���A��A���A�  A�M�A��A�S�A���A�S�A��#A���A�O�A�XA��A��PA��FA�7LA���A��-A�ƨA�dZA��uA}�A|ĜA{oAyl�Aw%As��Ak|�AjVAi�AfĜAb�DA]+AZ^5AW��AQ�AP��APM�AO�AN��ANn�AN^5ANA�AN �AN{AM��AM��AM`BAJVAE�AA7LA@{A?33A>5?A=�A<=qA:�+A8��A7�A6��A3�;A2r�A17LA0A/\)A.ȴA,{A)��A'�PA&I�A%��A$��A#t�A"jA��AQ�AĜAv�A�
A+A�AhsA�9A�A|�A�A�+A��A5?A�mAx�AȴA~�A�A��AhsA��AdZAbNA
�A
bA�+A��A"�A(�AQ�A�
A\)A �A ��A 5?@��;@�E�@�ƨ@��7@�z�@�j@���@�Ĝ@��@�\)@�-@�?}@�1@�t�@�
=@�E�@���@�^@�Ĝ@�b@�K�@�hs@�1@�-@��T@���@��T@�@�w@�C�@�w@�n�@��@�x�@��@�@��@�+@�^@��@�x�@���@��;@ݑh@�Ĝ@ۍP@�
=@��@�hs@�7L@�(�@���@��@ՙ�@ԋD@�ƨ@�  @ӝ�@�@϶F@�K�@Ο�@�M�@�$�@�@��@͉7@���@̼j@�r�@�x�@�5?@�^5@��T@�7L@�Ĝ@�dZ@�ff@�$�@�x�@ȃ@�1@��m@���@ǅ@�
=@�=q@�p�@�&�@��@��@�ƨ@���@�n�@��7@���@�Ĝ@�A�@�Q�@�r�@�I�@�I�@�9X@��@��!@�~�@���@��7@��^@���@���@�A�@��@��;@��m@��@�ȴ@�~�@��@�hs@��@���@��D@�9X@�1'@�b@���@�o@�~�@�=q@��@�@��@��#@�@���@�hs@��`@���@�z�@�Q�@� �@�  @���@��@�|�@�t�@�t�@�33@��@���@�v�@���@��@�X@��@��@��9@��@��@��D@�z�@�Q�@�b@�  @��w@���@�|�@���@���@��+@��^@�p�@��@��/@�Ĝ@��u@�9X@�b@��@�S�@��\@��@��@��D@�I�@� �@���@���@�K�@���@��+@�M�@�@��-@�hs@�%@���@���@�A�@� �@��;@��P@�o@��!@�n�@���@���@��@�bN@�1'@���@���@�l�@�33@�
=@��@��@���@��#@��7@�x�@�X@��@�z�@���@��@�S�@�"�@��H@���@�n�@�5?@�J@�@��7@���@�bN@��m@��@��P@�|�@�l�@�K�@�
=@��@�~�@�M�@�5?@��@���@�@���@��h@�p�@�X@�V@��@�Ĝ@��@��D@��@�bN@�I�@�9X@��@��P@�;d@�
=@���@�n�@���@���@���@�?}@��`@�z�@�Q�@�  @��@���@��@�+@��@�ȴ@�n�@�n�@�ff@�^5@��@���@�/@���@���@�V@���@�Ĝ@� �@��@��F@���@�t�@�+@���@��H@��!@�5?@��T@���@�ƨ@��\@z-@r�!@hQ�@^ff@Vȴ@Nff@G�P@@��@8�9@1&�@)��@#33@ȴ@Ĝ@O�@�@�@
��@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AԁAԁAԃA�~�A�x�A�z�A�v�A�v�A�t�A�r�A�r�A�ZA�VA�ZA�bNA�dZA�ffA�hsA�jA�l�A�l�A�n�A�p�A�r�A�t�A�~�Aԉ7Aԟ�AԾwA��#A���A�"�A���AѾwAГuAЏ\A�C�A���A˓uA�"�A�+Aɝ�A�VA��TA�ȴA�t�A��A�l�A��yA¡�A��A� �A���A��A��/A��jA���A�  A���A���A�~�A�hsA��A�bA�bNA��hA�(�A���A��A���A�  A�M�A��A�S�A���A�S�A��#A���A�O�A�XA��A��PA��FA�7LA���A��-A�ƨA�dZA��uA}�A|ĜA{oAyl�Aw%As��Ak|�AjVAi�AfĜAb�DA]+AZ^5AW��AQ�AP��APM�AO�AN��ANn�AN^5ANA�AN �AN{AM��AM��AM`BAJVAE�AA7LA@{A?33A>5?A=�A<=qA:�+A8��A7�A6��A3�;A2r�A17LA0A/\)A.ȴA,{A)��A'�PA&I�A%��A$��A#t�A"jA��AQ�AĜAv�A�
A+A�AhsA�9A�A|�A�A�+A��A5?A�mAx�AȴA~�A�A��AhsA��AdZAbNA
�A
bA�+A��A"�A(�AQ�A�
A\)A �A ��A 5?@��;@�E�@�ƨ@��7@�z�@�j@���@�Ĝ@��@�\)@�-@�?}@�1@�t�@�
=@�E�@���@�^@�Ĝ@�b@�K�@�hs@�1@�-@��T@���@��T@�@�w@�C�@�w@�n�@��@�x�@��@�@��@�+@�^@��@�x�@���@��;@ݑh@�Ĝ@ۍP@�
=@��@�hs@�7L@�(�@���@��@ՙ�@ԋD@�ƨ@�  @ӝ�@�@϶F@�K�@Ο�@�M�@�$�@�@��@͉7@���@̼j@�r�@�x�@�5?@�^5@��T@�7L@�Ĝ@�dZ@�ff@�$�@�x�@ȃ@�1@��m@���@ǅ@�
=@�=q@�p�@�&�@��@��@�ƨ@���@�n�@��7@���@�Ĝ@�A�@�Q�@�r�@�I�@�I�@�9X@��@��!@�~�@���@��7@��^@���@���@�A�@��@��;@��m@��@�ȴ@�~�@��@�hs@��@���@��D@�9X@�1'@�b@���@�o@�~�@�=q@��@�@��@��#@�@���@�hs@��`@���@�z�@�Q�@� �@�  @���@��@�|�@�t�@�t�@�33@��@���@�v�@���@��@�X@��@��@��9@��@��@��D@�z�@�Q�@�b@�  @��w@���@�|�@���@���@��+@��^@�p�@��@��/@�Ĝ@��u@�9X@�b@��@�S�@��\@��@��@��D@�I�@� �@���@���@�K�@���@��+@�M�@�@��-@�hs@�%@���@���@�A�@� �@��;@��P@�o@��!@�n�@���@���@��@�bN@�1'@���@���@�l�@�33@�
=@��@��@���@��#@��7@�x�@�X@��@�z�@���@��@�S�@�"�@��H@���@�n�@�5?@�J@�@��7@���@�bN@��m@��@��P@�|�@�l�@�K�@�
=@��@�~�@�M�@�5?@��@���@�@���@��h@�p�@�X@�V@��@�Ĝ@��@��D@��@�bN@�I�@�9X@��@��P@�;d@�
=@���@�n�@���@���@���@�?}@��`@�z�@�Q�@�  @��@���@��@�+@��@�ȴ@�n�@�n�@�ff@�^5@��@���@�/@���@���@�V@���@�Ĝ@� �@��@��F@���@�t�@�+@���@��H@��!@�5?@��TG�O�@�ƨ@��\@z-@r�!@hQ�@^ff@Vȴ@Nff@G�P@@��@8�9@1&�@)��@#33@ȴ@Ĝ@O�@�@�@
��@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�VB	�VB	�\B	�VB	�\B	�\B	�\B	�\B	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�LB	��B	��B	�B	�ZB	��B
+B
��B
�XB
ĜBɺBPBhB�B+B0!B49B49B49B49B7LB8RB�BVB	7B1B6FB�bB�B�B��B�TB��B��B��B�mB�TB��BǮB�FB��B��B��B�BffBG�B�BB�B�B�;BÖB�=BO�B,B{BB
�B
�B
�B
I�B
oB	��B	�B	�NB	��B	B	�!B	��B	u�B	l�B	aHB	P�B	7LB	�B	1B��B�ZB�BB�5B�#B�B�B�B�
B�
B�
B�B��B��BɺBB�qB�dB�^B�XB�LB�FB�FB�?B�LB�LB�XB�RB�RB�XB�RB�FB�XB�dB�}B��B��BBǮB��B��B�B�B�
B�B��B��B��B��B��BɺBǮB�5B�sB�B�B�B��B��B��B	  B	B��B�B�B�sB�sB�5B��BŢBĜB��B�}BƨBǮBŢBƨBɺB��B��B��B��B��B�BB�fB�fB�`B�sB�yB�B�B�yB�yB�yB�yB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	B	1B	DB	hB	oB	�B	�B	�B	�B	oB	uB	uB	oB	oB	hB	hB	oB	{B	{B	{B	�B	 �B	$�B	$�B	%�B	"�B	!�B	 �B	$�B	)�B	)�B	+B	,B	.B	.B	0!B	>wB	C�B	F�B	G�B	J�B	N�B	Q�B	S�B	VB	W
B	ZB	_;B	`BB	`BB	`BB	aHB	cTB	aHB	aHB	aHB	aHB	aHB	cTB	hsB	hsB	hsB	hsB	hsB	jB	l�B	m�B	o�B	p�B	o�B	o�B	o�B	o�B	r�B	u�B	x�B	}�B	|�B	{�B	|�B	� B	�B	�=B	�DB	�JB	�VB	�\B	�bB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�?B	�FB	�FB	�LB	�RB	�RB	�XB	�dB	�jB	�jB	�qB	�wB	�wB	�}B	�wB	�}B	�}B	��B	��B	��B	B	B	ÖB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
DB
DB
DB
JB
\B
bB
bB
bB
bB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
#�B
+B
0!B
6FB
<jB
C�B
I�B
N�B
Q�B
XB
]/B
cTB
iyB
m�B
q�B
t�B
w�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�EB	�BB	�JB	�CB	�GB	�IB	�IB	�IB	�GB	�QB	�QB	�cB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�9B	�pB	ʭB	��B	�DB	��B
B
��B
�;B
ĀBɜB.BFB{B*�B0 B4B4B4B4B7-B80B�B/B	BB6$B�@B��B��B��B�2B��B��B��B�KB�2B��BǋB�'B��B��B�^B��BfBBG�B�B�B�B�_B�B�oB�BO�B+�BYB�B
��B
��B
��B
I�B
OB	��B	�]B	�0B	��B	�nB	�B	��B	u�B	loB	a-B	P�B	71B	tB	B��B�AB�+B�B�
B��B��B��B��B��B��B��B��B��BɢB�wB�WB�KB�FB�>B�6B�/B�.B�&B�4B�4B�=B�:B�9B�?B�9B�*B�?B�KB�dB�hB�hB�uBǎB̱BʥB��B��B��B��B��B��B��BλB˫BɛBǏB�B�TB�`B�qB��B��B��B��B��B	�B��B�B�lB�RB�RB�B͵BńB�|B�eB�_BƈBǏB�BƈBɛBͲBʢB˩B��B��B�!B�DB�FB�>B�PB�ZB�_B�^B�YB�XB�XB�YB�YB�eB�}B�B�B�B��B�B��B��B��B��B��B	 �B	�B	�B	�B	B	!B	FB	KB	^B	oB	wB	oB	MB	RB	QB	IB	LB	BB	CB	KB	VB	VB	ZB	{B	 �B	$�B	$�B	%�B	"�B	!�B	 �B	$�B	)�B	)�B	*�B	+�B	-�B	-�B	/�B	>PB	CmB	F�B	G�B	J�B	N�B	Q�B	S�B	U�B	V�B	Y�B	_B	`B	`B	`B	aB	c-B	a B	aB	a!B	a!B	aB	c,B	hKB	hKB	hLB	hOB	hJB	jVB	lbB	mhB	oxB	p{B	owB	ouB	ouB	ouB	r�B	u�B	x�B	}�B	|�B	{�B	|�B	�B	��B	�B	�B	�B	�)B	�2B	�:B	�=B	�KB	�KB	�SB	�jB	�qB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�%B	�)B	�,B	�9B	�@B	�?B	�DB	�KB	�LB	�RB	�LB	�SB	�QB	�XB	�_B	�[B	�fB	�dB	�iB	�zB	�}B	�{B	ɒB	˛B	ͧB	άB	ϳB	йB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�B	�"B	�!B	�(B	�,B	�/B	�4B	�3B	�6B	�:B	�?B	�AB	�FB	�MB	�MB	�^B	�cB	�jB	�jB	�oB	�vB	�yB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
	
B

B
B
B
B
B
B
B
B
B
B
B
B
0B
4B
4B
7B
4B
9B
>B
?B
>B
LB
TB
ZB
XB
WB
XG�O�B
eB
#�B
*�B
/�B
6B
<:B
CgB
I�B
N�B
Q�B
W�B
\�B
c#B
iGB
m`B
q{B
t�B
w�B
|�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150520021550    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150520021550  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150520021550  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                