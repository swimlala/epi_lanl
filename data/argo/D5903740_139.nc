CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-01-18T20:16:21Z AOML 3.0 creation; 2016-06-01T00:08:28Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160118201621  20160531170828  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_139                   2C  D   APEX                            5374                            041511                          846 @׎�nh�1   @׎�=@$a@:h�\)�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD�fD�<�D�p D�ٚD�fD�@ D�s3D��fD��D�@ D�y�D��fD���D�I�Dڜ�D��fD��D�I�D�ffD��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BZ  Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-  D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�4Dy� D�3D�I�D�|�D��gD�3D�L�D�� D��3D��D�L�D��gD��3D�gD�VgDک�D��3D��D�VgD�s3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A���A���A���A���A���A��A��A��A��A��A��A��A��mA��;A��A��/A��
A�ƨA��RA��9A��FA��A���A���A���A���A���A��A�t�A�O�A�?}A�$�A�ĜA�?}A�|�A��+A�~�A�O�A�r�A��jA�A��\A��+A�\)A��A��A�ZA��^A�x�A�bA��A�1A�A���A��7A�O�A��hA��#A���A��7A��;A�S�A��A��!A�(�A���A��^A�bNA��
A��A�-A�
=A��A��DA�=qA��uA�v�A�A�^5A��RAl�A}7LA{��Az~�AyG�Aw�hAw|�AwAvn�Av1'As��Aq�wAo�^An�DAm
=Aj��Ai�hAiG�AhE�Af��Af$�Ad��AcoAbE�Aa�PAa"�A`Q�A_�A^�A]t�A\~�A[?}AZz�AZM�AY�AX��AXv�AXA�AWp�AW7LAV�yAV��AVE�AU�hAT�+AT-AT�AS�hAR�ARv�ARI�AQ��AQO�AP��AO\)AN�`AN~�AM�AMt�AMoAL�DAL1AK�PAJ�AJ1AI��AH��AG��AF�HAFbNAE�
AE&�AD�/AD$�AC�AC�AA�mAA�A@ZA?�A>ffA=�FA=;dA;��A:�A8�A8^5A7�-A7C�A6�HA6  A5;dA4E�A3\)A2�A1��A1VA0��A/�hA.9XA-|�A,�A*��A(��A'K�A&{A$~�A#�A"��A!l�A ȴA ��A �+A ffA A�AXA�uA�wAdZA�A-A��AXAoAVA�A�A��AoA~�AVA-A$�A�A�7A%A�DAAhsA�jAffAp�A��A1A
�HA
v�A	��A�A��AVAAS�A~�AdZA�Ar�A=qAƨAhsA�-A �uA -@��F@��@�33@�ff@��@���@�G�@�A�@�Ĝ@���@��@�G�@�A�@��m@�|�@��H@��-@���@�b@�o@�j@�l�@�^5@�h@��@�|�@��@�G�@��u@��@�\)@�n�@�G�@���@�x�@؃@���@�E�@�?}@�j@ҟ�@���@�;d@͉7@��@���@�@�@�Ĝ@�@Ɨ�@�{@�X@ċD@��m@Å@�@�@���@�S�@�M�@��u@�;d@�9X@���@�S�@�
=@���@�5?@���@��@�=q@�/@��@�^5@���@���@�bN@�\)@��y@��\@��@��@�b@���@��H@�{@�hs@�1'@���@�K�@��y@���@���@�@���@�1@�;d@�E�@�@���@�x�@�7L@��@���@��u@��D@�z�@�I�@�1'@��@��m@��w@��@��@��H@���@�~�@�ff@�M�@���@��@���@�@�@��-@��@�&�@�r�@��m@���@�l�@�
=@���@�^5@�{@���@��-@�?}@�&�@��@�Ĝ@��@�1'@��;@���@��P@�t�@�S�@�@�ȴ@��\@�V@���@��9@��w@�|�@�|�@��@�-@���@�&�@�X@�p�@�7L@��@�&�@�7L@�&�@��@��@��9@�A�@��m@�\)@���@��m@��H@�J@�-@�o@�"�@�@��y@���@���@�~�@�J@�x�@�/@�7L@�G�@�7L@�z�@�z�@�Q�@�1'@���@��P@�S�@�+@���@�V@�E�@��@�x�@�7L@�/@��@���@�Ĝ@��9@�bN@�Z@�Q�@�1'@�1'@��;@�|�@�;d@�33@�+@��y@�=q@���@���@���@���@��`@�9X@���@���@��D@�bN@�1'@��@\)@~�R@}�@}�h@}�h@}�@y%@m��@h  @^{@V{@O;d@JJ@E?}@<�/@6ff@0�@+�@%�@��@M�@�+@M�@`B@�@��@ �`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A���A���A���A���A���A��A��A��A��A��A��A��A��mA��;A��A��/A��
A�ƨA��RA��9A��FA��A���A���A���A���A���A��A�t�A�O�A�?}A�$�A�ĜA�?}A�|�A��+A�~�A�O�A�r�A��jA�A��\A��+A�\)A��A��A�ZA��^A�x�A�bA��A�1A�A���A��7A�O�A��hA��#A���A��7A��;A�S�A��A��!A�(�A���A��^A�bNA��
A��A�-A�
=A��A��DA�=qA��uA�v�A�A�^5A��RAl�A}7LA{��Az~�AyG�Aw�hAw|�AwAvn�Av1'As��Aq�wAo�^An�DAm
=Aj��Ai�hAiG�AhE�Af��Af$�Ad��AcoAbE�Aa�PAa"�A`Q�A_�A^�A]t�A\~�A[?}AZz�AZM�AY�AX��AXv�AXA�AWp�AW7LAV�yAV��AVE�AU�hAT�+AT-AT�AS�hAR�ARv�ARI�AQ��AQO�AP��AO\)AN�`AN~�AM�AMt�AMoAL�DAL1AK�PAJ�AJ1AI��AH��AG��AF�HAFbNAE�
AE&�AD�/AD$�AC�AC�AA�mAA�A@ZA?�A>ffA=�FA=;dA;��A:�A8�A8^5A7�-A7C�A6�HA6  A5;dA4E�A3\)A2�A1��A1VA0��A/�hA.9XA-|�A,�A*��A(��A'K�A&{A$~�A#�A"��A!l�A ȴA ��A �+A ffA A�AXA�uA�wAdZA�A-A��AXAoAVA�A�A��AoA~�AVA-A$�A�A�7A%A�DAAhsA�jAffAp�A��A1A
�HA
v�A	��A�A��AVAAS�A~�AdZA�Ar�A=qAƨAhsA�-A �uA -@��F@��@�33@�ff@��@���@�G�@�A�@�Ĝ@���@��@�G�@�A�@��m@�|�@��H@��-@���@�b@�o@�j@�l�@�^5@�h@��@�|�@��@�G�@��u@��@�\)@�n�@�G�@���@�x�@؃@���@�E�@�?}@�j@ҟ�@���@�;d@͉7@��@���@�@�@�Ĝ@�@Ɨ�@�{@�X@ċD@��m@Å@�@�@���@�S�@�M�@��u@�;d@�9X@���@�S�@�
=@���@�5?@���@��@�=q@�/@��@�^5@���@���@�bN@�\)@��y@��\@��@��@�b@���@��H@�{@�hs@�1'@���@�K�@��y@���@���@�@���@�1@�;d@�E�@�@���@�x�@�7L@��@���@��u@��D@�z�@�I�@�1'@��@��m@��w@��@��@��H@���@�~�@�ff@�M�@���@��@���@�@�@��-@��@�&�@�r�@��m@���@�l�@�
=@���@�^5@�{@���@��-@�?}@�&�@��@�Ĝ@��@�1'@��;@���@��P@�t�@�S�@�@�ȴ@��\@�V@���@��9@��w@�|�@�|�@��@�-@���@�&�@�X@�p�@�7L@��@�&�@�7L@�&�@��@��@��9@�A�@��m@�\)@���@��m@��H@�J@�-@�o@�"�@�@��y@���@���@�~�@�J@�x�@�/@�7L@�G�@�7L@�z�@�z�@�Q�@�1'@���@��P@�S�@�+@���@�V@�E�@��@�x�@�7L@�/@��@���@�Ĝ@��9@�bN@�Z@�Q�@�1'@�1'@��;@�|�@�;d@�33@�+@��y@�=q@���@���@���@���@��`@�9X@���@���@��D@�bN@�1'@��@\)@~�R@}�@}�h@}�h@}�@y%@m��@h  @^{@V{@O;d@JJ@E?}@<�/@6ff@0�@+�@%�@��@M�@�+@M�@`B@�@��@ �`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BL�BM�BM�BM�BM�BM�BM�BM�BL�BK�BK�BK�BK�BK�BK�BJ�BJ�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BH�BH�BH�BH�BG�BF�BE�BD�BA�B;dB2-B �B
=B��B��B�B�B�yB�TB��B�XB��B�BZBK�BH�BC�B9XB�BDB��B��B�B�)B��B�FB��B�JB�Bx�BhsBP�B5?B�B1BB  B
��B
��B
�B
�fB
�BB
��B
ĜB
�^B
�FB
�B
��B
�DB
}�B
r�B
jB
bNB
cTB
`BB
ZB
T�B
B�B
1'B
!�B
�B
1B	��B	�B	�B	�sB	�B	��B	B	�LB	�'B	�B	��B	��B	��B	�{B	�DB	�B	|�B	w�B	u�B	q�B	jB	hsB	ffB	bNB	`BB	^5B	\)B	ZB	XB	S�B	R�B	S�B	S�B	R�B	T�B	T�B	S�B	N�B	H�B	G�B	G�B	D�B	A�B	?}B	<jB	9XB	6FB	33B	0!B	+B	'�B	#�B	�B	�B	�B	�B	�B	uB	hB	\B	DB	B	  B��B��B��B�B�B�sB�TB�5B�/B�#B�B�
B�B�/B�B�B��B��B��BɺBĜB�wB�dB�RB�'B��B��B��B��B��B��B�hB�\B�VB�VB�PB�JB�7B�%B�Bx�Bs�Bq�Bp�Bo�Bm�Bk�BiyBffBdZBbNBbNBaHBaHB`BB_;B^5B]/B\)BZBXBW
BT�BR�BP�BM�BK�BJ�BH�BF�BF�BE�BC�BA�B?}B=qB<jB;dB:^B9XB7LB5?B6FB5?B49B2-B2-B1'B1'B0!B/B,B,B-B.B-B,B,B,B,B+B)�B(�B'�B%�B$�B$�B#�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B �B�B�B�B�B�B �B"�B"�B#�B#�B#�B$�B'�B'�B'�B(�B+B,B,B-B-B2-B2-B2-B6FB7LB=qB?}B?}B@�BA�BB�BB�BD�BK�BN�BP�BXBXB]/B^5B`BBaHBbNBdZBffBiyBk�Bm�Bp�Bq�Bv�Bx�Bz�B{�B|�B{�B� B�B�1B�JB�bB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�-B�-B�-B�-B�3B�9B�LB�^B�dB�jB�jB�wB��BÖBŢBƨBȴB��B��B��B��B��B��B�B�
B�B�B�5B�BB�NB�TB�mB�yB�B�B�B�B�B��B��B��B	  B	B	B	+B		7B	
=B	
=B	
=B	DB	PB	\B	�B	�B	$�B	#�B	"�B	&�B	0!B	7LB	9XB	;dB	=qB	?}B	@�B	A�B	C�B	D�B	D�B	E�B	E�B	E�B	F�B	G�B	H�B	J�B	J�B	J�B	K�B	L�B	O�B	P�B	Q�B	S�B	VB	VB	VB	W
B	YB	[#B	`BB	aHB	cTB	ffB	hsB	jB	k�B	l�B	l�B	l�B	m�B	m�B	n�B	n�B	p�B	{�B	}�B	�B	�=B	�JB	�VB	�\B	�\B	�bB	�bB	�bB	�bB	�uB	�{B	��B	��B	ĜB	�B	�TB	��B
%B
oB
�B
(�B
5?B
<jB
B�B
I�B
Q�B
W
B
]/B
aHB
gmB
o�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BM�BL�BM�BM�BM�BM�BM�BM�BM�BL�BK�BK�BK�BK�BK�BK�BJ�BJ�BJ�BJ�BJ�BJ�BI�BI�BI�BI�BI�BH�BH�BH�BH�BG�BF�BE�BD|BAiB;EB2B �B
B��B��B��B�fB�VB�/B��B�3B��B��BY�BK�BH�BCrB91B�B B��B��B�kB�BоB�B�[B�"B��Bx�BhNBP�B5BsB	B�B
��B
��B
��B
�B
�@B
�B
��B
�wB
�<B
�"B
��B
��B
�B
}�B
r�B
jYB
b,B
c/B
`B
Y�B
T�B
BkB
1B
!�B
dB
B	��B	�pB	��B	�RB	��B	��B	�pB	�*B	�	B	��B	��B	��B	��B	�\B	�(B	� B	|�B	w�B	u�B	q�B	jaB	hXB	fFB	b0B	`$B	^B	\B	Y�B	W�B	S�B	R�B	S�B	S�B	R�B	T�B	T�B	S�B	N�B	H�B	G�B	G�B	DB	AmB	?`B	<NB	9<B	6(B	3B	0B	*�B	'�B	#�B	�B	�B	�B	pB	cB	YB	NB	@B	(B	�B��B��B��B��B�B�B�XB�;B�B�B�B��B��B�B�B��B��B��B��B̰BɞBāB�]B�JB�8B�B��B��B��B��B�zB�gB�OB�EB�<B�;B�7B�0B�B�B��Bx�Bs�Bq�Bp�Bo�BmyBklBibBfPBdBBb7Bb6Ba2Ba3B`-B_$B^B]B\BZBW�BV�BT�BR�BP�BM�BK�BJ�BH�BF�BF�BE�BC~BAsB?dB=[B<QB;MB:FB9AB76B5)B6-B5)B4#B2B2B1B1B0
B/B+�B+�B,�B-�B,�B+�B+�B+�B+�B*�B)�B(�B'�B%�B$�B$�B#�B"�B!�B!�B!�B �B�B�B�B�B�B�B�B �B�B�B�B�B�B �B"�B"�B#�B#�B#�B$�B'�B'�B'�B(�B*�B+�B+�B,�B,�B2B2B2B6,B7/B=VB?`B?aB@gBAlBBqBBrBDBK�BN�BP�BW�BW�B]B^B`#Ba*Bb.Bd;BfIBiZBkgBmqBp�Bq�Bv�Bx�Bz�B{�B|�B{�B�B��B�B�)B�@B�TB�ZB�`B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�*B�<B�=B�IB�FB�TB�fB�sB�BƅBȑBʟB̥BͯBζB��B��B��B��B��B��B�B�B�*B�1B�JB�TB�fB�rB�{B�tB�B��B��B��B��B	�B	�B	B		B	
B	
B	
B	B	,B	5B	]B	�B	$�B	#�B	"�B	&�B	/�B	7#B	91B	;<B	=JB	?VB	@[B	AbB	CmB	DtB	DsB	E|B	E{B	EzB	F�B	G�B	H�B	J�B	J�B	J�B	K�B	L�B	O�B	P�B	Q�B	S�B	U�B	U�B	U�B	V�B	X�B	Z�B	`B	aB	c+B	f=B	hIB	jVB	kZB	l_B	laB	laB	miB	meB	noB	nnB	p{B	{�B	}�B	��B	�B	� B	�*B	�1B	�.B	�7B	�8B	�7B	�9B	�GB	�OB	�]B	��B	�pB	��B	�&B	��B
�B
?B
{B
(�B
5B
<:B
B_B
I�B
Q�B
V�B
\�B
aB
g<B
ooB
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.4 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708282016053117082820160531170828  AO  ARCAADJP                                                                    20160118201621    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160118201621  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160118201621  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170828  IP                  G�O�G�O�G�O�                