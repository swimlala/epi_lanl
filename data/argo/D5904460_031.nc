CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:12Z AOML 3.0 creation; 2016-08-07T21:17:33Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221312  20160807141733  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_031                   2C  D   APEX                            6487                            072314                          846 @�8IhP1   @�8I�5 
@,����+�c�+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @@  @�  @�33A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2�C3�fC5�fC7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyY�D���D�VfD�� D��3D�	�D�FfD�� D���D���D�<�D�� Dǳ31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @g�@��
@�
=A	�A)�AI�Ai�A���A�(�A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C�RC��C�C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.�RC0�RC2�RC4�C6�C8�C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�\)C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG�HDH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Dy�HD��D�j=D���D��
D�qD�Z=D���D�ФD��D�P�D���D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A��Aҝ�A�v�A��A��AоwA�K�AϓuA��AζFA�r�A�/A�A���A͝�A͓uA͑hA͋DA�n�A�A�A���A���A�x�A�I�A�hsA�p�A�|�A�x�A�\)A�{A��`A�ffA�ZA�1A���AʑhA�Q�A��;AɓuA�VA�E�A�7LA�%A���Aư!AÃA�{A��A�$�A�"�A���A��A�bA��A���A�l�A���A��A�dZA�=qA��TA�VA���A�A��A�t�A���A��9A���A�;dA��yA��mA�%A�\)A�ZA��jA��PA�M�A�VA�A�l�A�5?A���A�/A��A}+Av��Ao�#Ag��A`��AV�HAT��AR�`AQG�AP=qAO
=AJ�AH�+AE&�A>��A<��A9�A6��A5�A2bNA2A�A2-A1x�A0bNA0I�A0�A0n�A/�#A/��A1C�A3/A3A3�7A2A�A1�TA1��A1�A1`BA0�9A0E�A/A.��A-��A-\)A,�A+�A*E�A*A*�A)t�A(�DA&�RA$�A$ffA%&�A%O�A%K�A$�uA$Q�A$5?A#`BA"ȴA"ZA!A�Az�At�A��A�uAz�A��A��At�A;dA
=A��A��A5?A��A/AS�AC�A�`AĜAbNA��A�A��A�\AffAJA?}A�!A/A�A�#A�-A��A�hA�PAdZA��AjA��A�A�A�TA�-AoA�
A�HAVA�;Ax�A+A
�/A
��A
9XA	�mA	��A	dZA��A��AbNAI�A��Ax�A��A~�A�AC�A��AZA=qAbA|�AĜA��A�DA�An�AE�A �A  A��A�AdZA�A �HA V@��
@�t�@��y@��T@�V@�I�@�dZ@��T@��j@�\)@��7@�t�@���@�M�@�-@�ff@�V@���@�b@�1@�1'@�|�@��@�-@��#@�X@�9@�Z@���@�
=@�n�@��T@�O�@�j@蛦@�Z@��m@�\)@��H@�V@��@�@�+@��@���@◍@�=q@���@�?}@�j@��;@��@�ȴ@�-@�x�@�?}@�1'@��@�5?@�7L@�x�@�p�@�O�@���@؋D@��@�t�@�C�@�n�@ՙ�@�p�@�%@�z�@�\)@щ7@�1'@� �@���@ϥ�@�t�@�S�@�+@���@�M�@ͺ^@��@�r�@���@˥�@˅@�|�@�\)@��@ʟ�@��@ɡ�@���@�1'@�C�@�
=@���@Ɵ�@��@��@�z�@�9X@�ƨ@�dZ@�o@+@�{@��@���@��@�1@��!@���@��@��@���@��7@��@� �@��
@��@�C�@��@��@���@�=q@��#@�G�@��/@�I�@�  @�\)@�@��@�ȴ@�V@�@���@��@��`@�j@��w@��H@�$�@�?}@���@���@��D@�Q�@�1'@� �@�1@�t�@��@�V@�$�@�J@���@�X@���@��F@�;d@�+@��y@�^5@��@���@��h@��7@��7@��@�r�@�(�@�1@��;@��F@�|�@��@�V@�{@�@�V@���@���@�j@�9X@�(�@�b@��;@�@�ff@��@��#@�?}@���@�9X@��
@�|�@�o@��+@�{@��@��#@��^@��7@�X@��@��@�I�@� �@���@��
@���@�K�@��@��!@�^5@�E�@�J@��@�@���@�O�@��@���@�1@���@��F@�dZ@��@���@�J@�hs@���@��@�A�@���@�K�@��@��!@���@�M�@��@���@��-@���@��@��@��@���@��
@;d@sƨ@i��@`bN@YG�@PĜ@G|�@?��@7�@2��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  A���A�  A��Aҝ�A�v�A��A��AоwA�K�AϓuA��AζFA�r�A�/A�A���A͝�A͓uA͑hA͋DA�n�A�A�A���A���A�x�A�I�A�hsA�p�A�|�A�x�A�\)A�{A��`A�ffA�ZA�1A���AʑhA�Q�A��;AɓuA�VA�E�A�7LA�%A���Aư!AÃA�{A��A�$�A�"�A���A��A�bA��A���A�l�A���A��A�dZA�=qA��TA�VA���A�A��A�t�A���A��9A���A�;dA��yA��mA�%A�\)A�ZA��jA��PA�M�A�VA�A�l�A�5?A���A�/A��A}+Av��Ao�#Ag��A`��AV�HAT��AR�`AQG�AP=qAO
=AJ�AH�+AE&�A>��A<��A9�A6��A5�A2bNA2A�A2-A1x�A0bNA0I�A0�A0n�A/�#A/��A1C�A3/A3A3�7A2A�A1�TA1��A1�A1`BA0�9A0E�A/A.��A-��A-\)A,�A+�A*E�A*A*�A)t�A(�DA&�RA$�A$ffA%&�A%O�A%K�A$�uA$Q�A$5?A#`BA"ȴA"ZA!A�Az�At�A��A�uAz�A��A��At�A;dA
=A��A��A5?A��A/AS�AC�A�`AĜAbNA��A�A��A�\AffAJA?}A�!A/A�A�#A�-A��A�hA�PAdZA��AjA��A�A�A�TA�-AoA�
A�HAVA�;Ax�A+A
�/A
��A
9XA	�mA	��A	dZA��A��AbNAI�A��Ax�A��A~�A�AC�A��AZA=qAbA|�AĜA��A�DA�An�AE�A �A  A��A�AdZA�A �HA V@��
@�t�@��y@��T@�V@�I�@�dZ@��T@��j@�\)@��7@�t�@���@�M�@�-@�ff@�V@���@�b@�1@�1'@�|�@��@�-@��#@�X@�9@�Z@���@�
=@�n�@��T@�O�@�j@蛦@�Z@��m@�\)@��H@�V@��@�@�+@��@���@◍@�=q@���@�?}@�j@��;@��@�ȴ@�-@�x�@�?}@�1'@��@�5?@�7L@�x�@�p�@�O�@���@؋D@��@�t�@�C�@�n�@ՙ�@�p�@�%@�z�@�\)@щ7@�1'@� �@���@ϥ�@�t�@�S�@�+@���@�M�@ͺ^@��@�r�@���@˥�@˅@�|�@�\)@��@ʟ�@��@ɡ�@���@�1'@�C�@�
=@���@Ɵ�@��@��@�z�@�9X@�ƨ@�dZ@�o@+@�{@��@���@��@�1@��!@���@��@��@���@��7@��@� �@��
@��@�C�@��@��@���@�=q@��#@�G�@��/@�I�@�  @�\)@�@��@�ȴ@�V@�@���@��@��`@�j@��w@��H@�$�@�?}@���@���@��D@�Q�@�1'@� �@�1@�t�@��@�V@�$�@�J@���@�X@���@��F@�;d@�+@��y@�^5@��@���@��h@��7@��7@��@�r�@�(�@�1@��;@��F@�|�@��@�V@�{@�@�V@���@���@�j@�9X@�(�@�b@��;@�@�ff@��@��#@�?}@���@�9X@��
@�|�@�o@��+@�{@��@��#@��^@��7@�X@��@��@�I�@� �@���@��
@���@�K�@��@��!@�^5@�E�@�J@��@�@���@�O�@��@���@�1@���@��F@�dZ@��@���@�J@�hs@���@��@�A�@���@�K�@��@��!@���@�M�@��@���@��-@���@��G�O�@��@���@��
@;d@sƨ@i��@`bN@YG�@PĜ@G|�@?��@7�@2��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;oB	'�B	&�B	(�B	0!B	49B	.B	+B	,B	H�B	�uB	�HB
N�B
��B
��B%�B\)Bw�B~�B�B�\B�B�jBÖB��B��B��B�)B�`B�B�B�B�B�B�B�B�B�B�B�BB
=BoB�B�B�B�B2-BI�BT�BS�BP�BL�BM�B�%BĜBĜB��B�XB��B��B�{B�oB�hB�DBn�BK�B7LBB�B��B�B
=B��B�yBÖBo�BO�B49B
=B
ɺB
��B
~�B
p�B
XB
@�B
2-B
�B
  B	��B	��B	jB	E�B	"�B	�B	JB	B	  B��B�mB�B�
B��BɺB��B��B�B�;B�5B�5B�BB�B��B	hB	oB	hB	#�B	Q�B	�B	��B	��B	�!B	�-B	�FB	�XB	ƨB	�/B	�ZB	�yB	�B	��B	��B	��B
  B	��B	��B
+B
JB

=B
1B
  B
  B
�B
�B
�B
�B
�B
"�B
#�B
#�B
!�B
�B
uB
+B
B
%B
%B
B
B
B
B
B
1B
	7B
	7B

=B
JB
uB
�B
#�B
$�B
$�B
&�B
,B
.B
-B
.B
/B
.B
-B
+B
&�B
#�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
!�B
"�B
"�B
"�B
!�B
 �B
!�B
�B
�B
�B
!�B
!�B
"�B
"�B
#�B
"�B
"�B
#�B
"�B
#�B
%�B
%�B
$�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
bB
PB
1B
B
B
B
+B
JB
PB
JB
DB
JB
bB
\B
VB
JB
PB
PB
VB
VB
PB
PB
JB
DB
DB

=B

=B

=B

=B
	7B
1B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
+B
1B
1B

=B

=B
DB
JB
JB
PB
VB
VB
PB
PB
PB
PB
JB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
uB
uB
{B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
%�B
-B
2-B
;dB
=qB
F�B
J�B
P�B
VB
\)B
bNB
e`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  B	'�B	&�B	(�B	0B	4B	-�B	*�B	+�B	H�B	�PB	�B
N�B
�SB
͢B%�B[�Bw�B~�B��B�(B��B�8B�aB̚B̚BʎB��B�0B�NB�[B�VB�ZB�^B�YB�XB�]B�VB�^B�zB �B
B;BMBTBtBuB1�BI�BT�BS�BP�BL�BM�B��B�dB�jB�NB�%B��B�[B�@B�6B�1B�Bn^BK�B7B�B��BҽB�]B
B��B�@B�\BocBO�B3�B
B
ɀB
�eB
~�B
pkB
W�B
@JB
1�B
�B	��B	ϫB	�kB	jMB	EqB	"�B	SB	B	�B��B��B�;B��B��BϬBɈBϫBгB��B�B� B�B�B�JB��B	2B	7B	.B	#�B	Q�B	��B	�PB	��B	��B	��B	�B	�B	�lB	��B	�B	�8B	�pB	��B	��B	��B	��B	��B	��B
�B

B
	�B
�B	��B	��B
\B
vB
oB
[B
|B
"�B
#�B
#�B
!�B
{B
3B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	B
4B
eB
#�B
$�B
$�B
&�B
+�B
-�B
,�B
-�B
.�B
-�B
,�B
*�B
&�B
#�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
!�B
"�B
"�B
"�B
!�B
 �B
!�B
yB
rB
{B
!�B
!�B
"�B
"�B
#�B
"�B
"�B
#�B
"�B
#�B
%�B
%�B
$�B
"�B
"�B
!�B
rB
mB
tB
yB
xB
rB
lB
lB
gB
eB
eB
fB
gB
`B
aB
ZB
YB
RB
LB
IB
HB
@B
CB
:B
<B
9B
BB
AB
/B
B
B
�B
�B
�B
�B
�B
B
B
B

�B
B
B
B
B
B
B
B
B
B

B
	B
B

�B

�B
	�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�bB	�TB	�WB	�QB	�RB	�QB	�PB	�QB	�QB	�RB	�IB	�OB	�PB	�RB	�]B	�cB	�oB	�mB	�nB	�zB	�|B	�nB	�hB	�^B	�VB	�^B	�]B	�\B	�]B	�dB	�bB	�fB	�nB	�uB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
B
 B
B
B

B
B
B
B
B
B
B
B

B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
#B
+B
(B
+B
,B
*B
*B
+B
0B
8B
0B
-B
0B
6B
>B
=B
:B
;B
=B
<B
DB
HB
GB
JB
IB
VB
\B
\B
[B
ZB
`B
aB
]B
\B
\B
bB
aB
iB
fB
gB
iB
fB
jB
tB
tB
rB
sB
tG�O�B
!|B
%�B
,�B
1�B
;B
=%B
F[B
JwB
P�B
U�B
[�B
b B
e1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417332016080714173320160807141733  AO  ARCAADJP                                                                    20150226221312    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221312  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221312  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141733  IP                  G�O�G�O�G�O�                