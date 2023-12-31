CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-14T09:16:16Z AOML 3.0 creation; 2016-08-07T21:17:40Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150914091616  20160807141740  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               IA   AO  5285_8895_073                   2C  D   APEX                            6487                            072314                          846 @�oN#En�1   @�oN�g�@,��vȴ9�cّhr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    IA   B   B   @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B��B��B'��B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD��D�L�D��3D���D�3D�@ D�� D���D�fD�S3D�� D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
A	�A)�AI�Ai�A���A���A�A���A���A���A���A���Bz�B
z�Bz�BG�B"{B*{B2z�B:z�BBz�BJz�BRz�BZ�GBb�GBjz�Brz�Bzz�B�=qB�=qB�=qB�=qB�=qB�p�B��B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4�RC6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr�Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt��Dy�D�0�D�`�D��
D��qD�
D�S�D���D�ФD�*=D�g
D���D��=D�q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�
=A�%A�A�  A���A��A�l�A�RA� �A�^A�A�bA�M�A��yA� �A�M�A���AݬA�33A�$�Aש�A֏\A���A�S�A�z�A�
=A��
A�z�A�E�A��A��`A���AʾwA��A��yA�7LAƝ�A�`BA��TA�z�A���A�ffA�M�A�l�A��+A�K�A��A�ȴA��HA��uA�33A�ƨA�r�A�O�A�v�A��A���A�E�A�x�A�;dA��#A��A���A�A�A���A��A���A�;dA�dZA��A�ffA�hsA�z�A�$�A�ZA�9XA��jA�O�A�?}A���A��`A��wA�ƨA��\A�;dA|(�AzQ�Aw�Au�^ApM�Af�/A_�7AV��AS�AOoAL�HAJbNAG��AD��AAG�A?�7A>�A:��A9K�A8��A8r�A5+A3�A2�A2�+A2bA21'A1�A0��A0Q�A/"�A/|�A/��A/`BA/\)A.�HA.z�A-ƨA,ȴA+��A*�/A*ffA)ƨA)dZA(ZA'��A'O�A&�A&�DA&n�A&�A%�wA%&�A$~�A#�mA#`BA#?}A#VA"�`A"�+A"bA!��A!��A!��A!S�A �!A 1A�^A|�Al�AO�A�A��A(�A�^A?}A��AjA$�AA��A
=A$�AƨA33AffA��A7LA�RA1'A��A"�AffAA�wAt�A�AffA�AƨA7LA��A��A9XAA�mA�wAx�A�`An�A�A��AS�A/A�A��AĜAI�A��AhsA\)A
jA	�A	%A5?A��A�^A/AȴAbNA��A`BA\)A/Ar�A�-At�A+AbNA��A\)A�A ȴA ��A ��A ��A �DA r�A bNA 1'@�33@�ȴ@�$�@���@�p�@�G�@���@��w@��@�=q@���@��@��@�Z@�|�@���@�~�@��^@���@�@�"�@��@�M�@�{@�@��`@�ƨ@��@���@��@�bN@�l�@�
=@ꟾ@��@�?}@�D@��;@�\)@�n�@�@�Ĝ@��@���@�9@�b@�j@߅@�v�@ݩ�@�%@ܬ@�(�@���@�dZ@�C�@�;d@�@ڟ�@�~�@�E�@��T@�x�@���@�z�@���@�=q@Ցh@�G�@�%@ԓu@�j@ӝ�@�@ѩ�@�X@�G�@�`B@��@���@��/@д9@�j@ϥ�@�K�@�o@Η�@��T@���@���@�@�/@̴9@�1'@�t�@��@�n�@ɡ�@��@�Q�@��;@ǍP@�|�@ǝ�@ǶF@�+@ư!@�$�@���@�7L@���@ă@���@�S�@���@�V@���@��h@�`B@��9@�bN@�Q�@�1@�1'@���@�ȴ@�v�@��\@���@���@��7@�G�@�&�@��@�%@�9X@��@��!@�V@�hs@�/@���@�Ĝ@�(�@�l�@�ȴ@��@���@���@�hs@�?}@��j@�bN@� �@��w@�C�@��\@�@��-@�p�@��`@�Z@��w@�C�@��H@��@��@�%@���@�j@�bN@� �@���@�S�@�33@�@�ȴ@���@�n�@�M�@��@�p�@�&�@�z�@� �@��m@���@�S�@�@��R@�v�@�$�@���@�hs@�/@�bN@��m@���@�l�@�C�@��@��H@���@�E�@�@�hs@�&�@��9@���@�r�@�  @��w@��@�\)@�dZ@�"�@��y@���@��\@��@���@���@�/@���@���@�Q�@�I�@��@��@�|�@�S�@�+@��@��!@�~�@�=q@�@��T@���@�x�@�V@��`@��j@��@�z�@�  @��
@���@�\)@�33@��@���@��
@�n�@��@���@yG�@q�7@h �@a�7@Y�#@L(�@C��@>�+@:��@1�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�1A�
=A�%A�A�  A���A��A�l�A�RA� �A�^A�A�bA�M�A��yA� �A�M�A���AݬA�33A�$�Aש�A֏\A���A�S�A�z�A�
=A��
A�z�A�E�A��A��`A���AʾwA��A��yA�7LAƝ�A�`BA��TA�z�A���A�ffA�M�A�l�A��+A�K�A��A�ȴA��HA��uA�33A�ƨA�r�A�O�A�v�A��A���A�E�A�x�A�;dA��#A��A���A�A�A���A��A���A�;dA�dZA��A�ffA�hsA�z�A�$�A�ZA�9XA��jA�O�A�?}A���A��`A��wA�ƨA��\A�;dA|(�AzQ�Aw�Au�^ApM�Af�/A_�7AV��AS�AOoAL�HAJbNAG��AD��AAG�A?�7A>�A:��A9K�A8��A8r�A5+A3�A2�A2�+A2bA21'A1�A0��A0Q�A/"�A/|�A/��A/`BA/\)A.�HA.z�A-ƨA,ȴA+��A*�/A*ffA)ƨA)dZA(ZA'��A'O�A&�A&�DA&n�A&�A%�wA%&�A$~�A#�mA#`BA#?}A#VA"�`A"�+A"bA!��A!��A!��A!S�A �!A 1A�^A|�Al�AO�A�A��A(�A�^A?}A��AjA$�AA��A
=A$�AƨA33AffA��A7LA�RA1'A��A"�AffAA�wAt�A�AffA�AƨA7LA��A��A9XAA�mA�wAx�A�`An�A�A��AS�A/A�A��AĜAI�A��AhsA\)A
jA	�A	%A5?A��A�^A/AȴAbNA��A`BA\)A/Ar�A�-At�A+AbNA��A\)A�A ȴA ��A ��A ��A �DA r�A bNA 1'@�33@�ȴ@�$�@���@�p�@�G�@���@��w@��@�=q@���@��@��@�Z@�|�@���@�~�@��^@���@�@�"�@��@�M�@�{@�@��`@�ƨ@��@���@��@�bN@�l�@�
=@ꟾ@��@�?}@�D@��;@�\)@�n�@�@�Ĝ@��@���@�9@�b@�j@߅@�v�@ݩ�@�%@ܬ@�(�@���@�dZ@�C�@�;d@�@ڟ�@�~�@�E�@��T@�x�@���@�z�@���@�=q@Ցh@�G�@�%@ԓu@�j@ӝ�@�@ѩ�@�X@�G�@�`B@��@���@��/@д9@�j@ϥ�@�K�@�o@Η�@��T@���@���@�@�/@̴9@�1'@�t�@��@�n�@ɡ�@��@�Q�@��;@ǍP@�|�@ǝ�@ǶF@�+@ư!@�$�@���@�7L@���@ă@���@�S�@���@�V@���@��h@�`B@��9@�bN@�Q�@�1@�1'@���@�ȴ@�v�@��\@���@���@��7@�G�@�&�@��@�%@�9X@��@��!@�V@�hs@�/@���@�Ĝ@�(�@�l�@�ȴ@��@���@���@�hs@�?}@��j@�bN@� �@��w@�C�@��\@�@��-@�p�@��`@�Z@��w@�C�@��H@��@��@�%@���@�j@�bN@� �@���@�S�@�33@�@�ȴ@���@�n�@�M�@��@�p�@�&�@�z�@� �@��m@���@�S�@�@��R@�v�@�$�@���@�hs@�/@�bN@��m@���@�l�@�C�@��@��H@���@�E�@�@�hs@�&�@��9@���@�r�@�  @��w@��@�\)@�dZ@�"�@��y@���@��\@��@���@���@�/@���@���@�Q�@�I�@��@��@�|�@�S�@�+@��@��!@�~�@�=q@�@��T@���@�x�@�V@��`@��j@��@�z�@�  @��
@���@�\)@�33@��G�O�@��
@�n�@��@���@yG�@q�7@h �@a�7@Y�#@L(�@C��@>�+@:��@1�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	�!B	��B	�B
5?B
T�B
ZB
|�B
�=B
�bB
�oB
��B
�VB
�B
ĜB
ƨB
��B
��BhB
�B
ȴB
�9B
�qB
��B$�BJ�Bq�B�B��B�BB2-BG�BcTBt�B|�B� B�hB��B�9B��B�B��BPBJBbB�B�B"�B#�B!�B"�B�B�BVB+BBDBPBbB{B�BuBVB��BƨBn�B�B��B�sB��B�qB�RB��B�7B]/BB
�BB
�RB
�hB
gmB
+B
PB	��B	�sB	��B	��B	_;B	.B��B�/B�)B�/B�B��B�#B�mB��B��B�B��B	+B	"�B	.B	-B	>wB	T�B	bNB	}�B	�oB	��B	�B	B	�B	��B
  B
bB
�B
�B
%�B
%�B
'�B
-B
0!B
33B
33B
8RB
9XB
9XB
:^B
;dB
;dB
<jB
C�B
B�B
A�B
@�B
@�B
@�B
A�B
D�B
F�B
G�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
E�B
D�B
D�B
C�B
C�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
F�B
D�B
C�B
D�B
C�B
C�B
A�B
@�B
?}B
=qB
=qB
<jB
;dB
:^B
:^B
:^B
:^B
8RB
7LB
7LB
8RB
9XB
9XB
9XB
:^B
9XB
7LB
7LB
:^B
<jB
7LB
2-B
/B
+B
)�B
'�B
$�B
%�B
#�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
oB
hB
bB
bB
bB
hB
hB
hB
hB
uB
uB
oB
bB
\B
\B
hB
oB
hB
bB
PB
DB
DB
DB

=B
	7B

=B
JB
JB
JB
DB

=B
1B
	7B

=B
	7B
	7B
1B
1B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
JB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
oB
uB
uB
{B
{B
{B
{B
{B
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
,B
/B
6FB
<jB
@�B
E�B
I�B
S�B
ZB
_;B
aHB
ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	�B	�}B	��B	��B	��B	��B	�fB	�B
5B
T�B
Y�B
|�B
�B
�9B
�EB
�VB
�(B
��B
�oB
�zB
��B
��B:B
�bB
ȆB
�B
�FB
ϲB$�BJ�BqzB��B��B��B�B1�BG}BcBt�B|�B�B�3B��B�BһB��B��BBB.BOB�B"�B#�B!�B"�B�BgB'B�B�BBB,BEBkB<B B��B�uBn_B�B��B�:B͖B�7B�B��B��B\�B�B
�	B
�B
�0B
g6B
*�B
B	��B	�<B	ҽB	��B	_B	-�B��B��B��B��B��B��B��B�;B��B��B�qB��B	�B	"�B	-�B	,�B	><B	T�B	bB	}�B	�3B	��B	��B	�RB	�FB	��B	��B
"B
HB
~B
%�B
%�B
'�B
,�B
/�B
2�B
2�B
8B
9B
9B
:B
; B
;"B
<%B
CQB
BLB
AGB
@AB
@?B
@?B
AEB
DZB
FdB
GlB
GkB
FaB
FbB
FeB
GlB
HqB
HpB
HrB
HqB
GlB
GlB
FbB
FdB
FfB
E`B
E`B
DYB
DVB
E_B
DUB
DXB
CRB
CTB
E_B
FbB
FcB
GiB
GjB
HpB
HpB
FbB
DVB
CRB
DYB
COB
COB
AFB
@<B
?7B
=,B
=,B
<&B
;B
:B
:B
:B
:B
8B
7B
7B
8B
9B
9B
9B
:B
9B
7B
7B
:B
<$B
7B
1�B
.�B
*�B
)�B
'�B
$�B
%�B
#�B
!�B
 ~B
 ~B
sB
XB
HB
UB
HB
&B
!B
B
B
B
!B
#B
!B
!B
0B
/B
'B
B
B
B
#B
(B
!B
B

B

�B

�B

�B
	�B
�B
	�B
B
B
B

�B
	�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	�vB	�rB	�pB	��B	��B	�uB	�iB	�dB	�_B	�_B	�jB	�uB	�vB	�nB	�pB	�sB	�{B	��B	��B	��B	��B	�|B	�|B	�tB	�pB	�qB	�oB	�}B	��B	�}B	�lB	�oB	�gB	�hB	�nB	�nB	��B	��B	��B	��B	��B	��B	�zB	�}B	�{B	�{B	�{B	�|B	��B	��B	�yB	�{B	�vB	�sB	�tB	�uB	�uB	�oB	�pB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
 B
B
B
B
B
B
B
B
B
B
B
B
	B
B
B
B
B
B
B
$B
,B
+B
0B
0B
0B
2B
/B
/B
+B
*B
1B
7B
7B
>B
BB
JB
KB
KB
RB
SB
TB
UB
UB
UB
SB
[B
]B
`B
aB
aB
bB
hB
hB
gB
lB
lB
mG�O�B
bB
%�B
+�B
.�B
5�B
< B
@7B
EUB
ImB
S�B
Y�B
^�B
`�B
f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417402016080714174020160807141740  AO  ARCAADJP                                                                    20150914091616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150914091616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150914091616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141740  IP                  G�O�G�O�G�O�                