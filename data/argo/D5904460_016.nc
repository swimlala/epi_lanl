CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:54Z AOML 3.0 creation; 2016-08-07T21:17:31Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221254  20160807141731  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_016                   2C  D   APEX                            6487                            072314                          846 @�$�T�@1   @�$�����@+�/��w�c����o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B��B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy� D� D�@ D�p D�ٚD�	�D�I�D�|�D��3D�	�D�I�D���DǼ�D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�B�GBz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bj�GBr�GBzz�B�
>B�=qB�=qB�=qB�=qB��B��B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�p�B�p�B�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/�D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI'�DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt�{Dy��D�#�D�S�D���D��qD�qD�]qD���D��
D�qD�]qD���D�ФD�3�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AٸRAٸRAٸRAٺ^AټjAٺ^AٸRAٶFAٶFAٴ9Aٲ-Aٲ-Aٴ9Aٴ9Aٲ-A٩�Aٙ�AًDA�jA�%A؏\A؁A�z�A�jA�\)A�;dA��mA�?}AԺ^A�r�A�^5A���Aϴ9A�oA˧�A�S�AƲ-A��#A�A���A���A��A��7A��uA�+A���A�{A���A���A��A�9XA���A���A���A��HA�$�A� �A�z�A��/A���A�VA�G�A��HA�v�A�JA�ȴA��A��A��A���A�bA�A���A��/A���A�5?A�ZA��Ax��As��Al�Ad��Ab��Aap�A`bNA_�;A\��AZ�/AY�wAW�FAV�ARr�AN5?AK�7AIG�AG�-AFVACoA@��A=��A:��A9�A8$�A5�A4n�A4bA3\)A21'A0v�A.�HA.-A-VA,�/A-p�A-��A,JA+�PA*��A(A�A&�A$�jA"�jA#C�A"�\A"��A#O�A"�/A!�wA!;dA!t�A!ƨA 9XA��A��A�A33A�#A�`A��A��A
=A^5A�A�7A
r�A
A�A
�A�A
��A	+A	C�A
Q�A
�\A
VA
A	dZA	l�A	p�A	O�A�-A
=AbNA�9A	�PA	�PA	7LAI�A�A�A�A��AbA�TA�7A&�A��AVA��A�A\)A"�A�A$�A�;AK�A ĜA n�@��m@��@�v�@�{@�&�@�Q�@�ƨ@��@��!@���@��@���@���@��`@��/@�Ĝ@��j@�bN@�b@�33@��^@�?}@�@�p�@���@���@��@� �@�@��@�M�@��-@�&�@�@�?}@�D@���@띲@�l�@�33@�+@�S�@��@�M�@�$�@�@��@�&�@�j@�  @�@�S�@�P@�;d@�!@�^@�p�@�&�@�V@�&�@���@�j@��@��y@�^5@��#@��@�X@� �@�
=@ޏ\@��@ݩ�@�&�@ܓu@�1'@��@�dZ@�=q@�Ĝ@�  @ׅ@�S�@��@��H@֗�@�@���@���@ӝ�@�|�@�C�@��y@���@�n�@�@�@љ�@���@�Q�@��m@ϝ�@�l�@�C�@�o@���@�-@��@���@̃@˾w@�ȴ@�-@���@��#@�@ə�@�p�@�X@��@ȋD@�Z@�t�@�;d@�;d@�C�@�C�@��@��@Ɵ�@�$�@��@ŉ7@��@�A�@î@ÍP@��@�n�@�5?@�{@�@�@���@��@�`B@�X@�G�@�V@�Ĝ@�9X@�|�@��H@�n�@�$�@���@�G�@��@���@��/@��9@���@��@�Q�@��@��@�;d@��R@��@��/@�Z@�(�@���@��y@���@�V@�/@��@�Q�@�1'@�|�@��+@�x�@��@��@�ƨ@�K�@��y@��\@��7@�/@��@�G�@��@���@��@�r�@� �@���@�33@��R@�ff@��@��-@��@��@��D@��;@��@�@���@�^5@�$�@���@�?}@��@�Z@��@�+@��@�ȴ@��+@�5?@��@���@�x�@�&�@���@���@�Z@�b@��m@�|�@�o@���@�n�@�=q@��@��^@�X@�7L@��@���@�Ĝ@���@�j@��@��w@���@�C�@��@�o@��y@���@���@�-@��@��@���@��@�r�@�Z@�(�@���@��P@�C�@��y@���@��!@���@�G�@�/@�&�@��@�V@���@��`@��j@�j@��@�|�@�33@�5?@��h@�O�@���@�A�@���@�33@��@�n�@�@���@��@��9@�A�@��;@���@��P@��@�\)@��-@�%@�1@y��@p��@g�@_l�@W�w@PQ�@IX@AX@9�@1��@+ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  AٸRAٸRAٸRAٺ^AټjAٺ^AٸRAٶFAٶFAٴ9Aٲ-Aٲ-Aٴ9Aٴ9Aٲ-A٩�Aٙ�AًDA�jA�%A؏\A؁A�z�A�jA�\)A�;dA��mA�?}AԺ^A�r�A�^5A���Aϴ9A�oA˧�A�S�AƲ-A��#A�A���A���A��A��7A��uA�+A���A�{A���A���A��A�9XA���A���A���A��HA�$�A� �A�z�A��/A���A�VA�G�A��HA�v�A�JA�ȴA��A��A��A���A�bA�A���A��/A���A�5?A�ZA��Ax��As��Al�Ad��Ab��Aap�A`bNA_�;A\��AZ�/AY�wAW�FAV�ARr�AN5?AK�7AIG�AG�-AFVACoA@��A=��A:��A9�A8$�A5�A4n�A4bA3\)A21'A0v�A.�HA.-A-VA,�/A-p�A-��A,JA+�PA*��A(A�A&�A$�jA"�jA#C�A"�\A"��A#O�A"�/A!�wA!;dA!t�A!ƨA 9XA��A��A�A33A�#A�`A��A��A
=A^5A�A�7A
r�A
A�A
�A�A
��A	+A	C�A
Q�A
�\A
VA
A	dZA	l�A	p�A	O�A�-A
=AbNA�9A	�PA	�PA	7LAI�A�A�A�A��AbA�TA�7A&�A��AVA��A�A\)A"�A�A$�A�;AK�A ĜA n�@��m@��@�v�@�{@�&�@�Q�@�ƨ@��@��!@���@��@���@���@��`@��/@�Ĝ@��j@�bN@�b@�33@��^@�?}@�@�p�@���@���@��@� �@�@��@�M�@��-@�&�@�@�?}@�D@���@띲@�l�@�33@�+@�S�@��@�M�@�$�@�@��@�&�@�j@�  @�@�S�@�P@�;d@�!@�^@�p�@�&�@�V@�&�@���@�j@��@��y@�^5@��#@��@�X@� �@�
=@ޏ\@��@ݩ�@�&�@ܓu@�1'@��@�dZ@�=q@�Ĝ@�  @ׅ@�S�@��@��H@֗�@�@���@���@ӝ�@�|�@�C�@��y@���@�n�@�@�@љ�@���@�Q�@��m@ϝ�@�l�@�C�@�o@���@�-@��@���@̃@˾w@�ȴ@�-@���@��#@�@ə�@�p�@�X@��@ȋD@�Z@�t�@�;d@�;d@�C�@�C�@��@��@Ɵ�@�$�@��@ŉ7@��@�A�@î@ÍP@��@�n�@�5?@�{@�@�@���@��@�`B@�X@�G�@�V@�Ĝ@�9X@�|�@��H@�n�@�$�@���@�G�@��@���@��/@��9@���@��@�Q�@��@��@�;d@��R@��@��/@�Z@�(�@���@��y@���@�V@�/@��@�Q�@�1'@�|�@��+@�x�@��@��@�ƨ@�K�@��y@��\@��7@�/@��@�G�@��@���@��@�r�@� �@���@�33@��R@�ff@��@��-@��@��@��D@��;@��@�@���@�^5@�$�@���@�?}@��@�Z@��@�+@��@�ȴ@��+@�5?@��@���@�x�@�&�@���@���@�Z@�b@��m@�|�@�o@���@�n�@�=q@��@��^@�X@�7L@��@���@�Ĝ@���@�j@��@��w@���@�C�@��@�o@��y@���@���@�-@��@��@���@��@�r�@�Z@�(�@���@��P@�C�@��y@���@��!@���@�G�@�/@�&�@��@�V@���@��`@��j@�j@��@�|�@�33@�5?@��h@�O�@���@�A�@���@�33@��@�n�@�@���@��@��9@�A�@��;@���@��P@��G�O�@��-@�%@�1@y��@p��@g�@_l�@W�w@PQ�@IX@AX@9�@1��@+ƨ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBA�BA�BB�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BB�BC�BC�BD�BF�BH�BH�BH�BH�BK�B_;BjB�B�{B�!BɺB�B��B�B�yB�/B�B��B\B�B#�B#�B8RBH�BXB_;B�wB��B��B�B�ZB��B��B
=BbB�B��B�uBl�B`BBB�B�BB�mBB��B�=B{�BT�B7LBJB
�ZB
�'B
�DB
x�B
J�B
-B
1B	�
B	�B	�B	[#B	S�B	VB	VB	S�B	I�B	B�B	;dB	1'B	&�B	�B	B��B�B�B�ZB�B��BȴB��B�qB�XB�?B�9B�-B�-B�?B�dB�wBÖB��B�ZB��B	�B	�B	�B	�B	DB	JB	+B	JB	&�B	=qB	YB	r�B	�B	�7B	�PB	�uB	��B	��B	}�B	v�B	n�B	ffB	`BB	ZB	Q�B	G�B	C�B	=qB	49B	,B	)�B	-B	B�B	M�B	]/B	VB	aHB	s�B	x�B	z�B	{�B	�B	�JB	�oB	�bB	�1B	�B	��B	�'B	ƨB	��B	��B	��B	��B	�B	�BB	�fB	�yB	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�sB	�NB	�NB	�ZB	�`B	�fB	�mB	�fB	�ZB	�TB	�TB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
  B
B
B
  B
B
%B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
#�B
-B
33B
9XB
<jB
E�B
J�B
R�B
T�B
YB
^5B
bNB
gmB
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  BA\BAXBBbBAYBAXBA[BAYBAYBAYBBaBB`BBaBB`BBaBBaBBbBCfBCdBDpBFzBH�BH�BH�BH�BK�B_BjOB��B�KB��BɋB�~B��B�}B�FB��B�yB��B(B�B#�B#�B8BH}BW�B_B�DB��B�B�cB�'B��BҺB
B-B�UB�MB�=BlOB`BBTB~B �B�2B�WB��B�B{�BT�B7BB
� B
��B
�B
x�B
J�B
,�B
�B	��B	��B	��B	Z�B	S�B	U�B	U�B	S�B	I�B	B[B	;0B	0�B	&�B	TB	�B��B�B�QB�'B��BѸBȂB�UB�>B�$B�B�B��B��B�B�/B�DB�cBϪB�$B��B	nB	rB	cB	TB	B	B	�B	B	&�B	=7B	X�B	rrB	��B	��B	�B	�5B	�mB	�rB	}�B	v�B	n\B	f'B	`B	Y�B	Q�B	GoB	CXB	=4B	3�B	+�B	)�B	,�B	BQB	M�B	\�B	U�B	aB	swB	x�B	z�B	{�B	��B	�B	�-B	�!B	��B	��B	��B	��B	�eB	ҲB	ԻB	ӶB	ԹB	��B	��B	�!B	�4B	�[B	�XB	�ZB	�jB	�B	�vB	�iB	�bB	�pB	��B	�vB	�B	�pB	�pB	�~B	�yB	�wB	�yB	�pB	�pB	�jB	�jB	�pB	�wB	�iB	�qB	�qB	�vB	�vB	�}B	�}B	�}B	��B	��B	�wB	�`B	�KB	�.B	�B	�B	�B	�B	�!B	�)B	�"B	�B	�B	�B	�AB	�KB	�MB	�LB	�DB	�RB	�YB	�gB	��B	�|B	�wB	��B	��B	��B	�wB	�qB	�pB	�uB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�oB	�iB	�iB	�dB	�hB	�jB	�dB	�WB	�KB	�EB	�=B	�?B	�=B	�7B	�8B	�0B	�0B	�2B	�4B	�2B	�3B	�9B	�9B	�?B	�FB	�DB	�BB	�EB	�KB	�HB	�LB	�JB	�HB	�PB	�QB	�SB	�PB	�HB	�JB	�>B	�3B	�2B	�4B	�0B	�2B	�7B	�8B	�6B	�7B	�7B	�2B	�5B	�4B	�6B	�>B	�>B	�DB	�IB	�GB	�OB	�PB	�VB	�]B	�[B	�]B	�bB	�fB	�hB	�gB	�iB	�eB	�hB	�iB	�nB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
$B
"B
#B
$B
$B
#B
-B
*B
*B
0B
1B
/B
/B
/B
0B
.B
/B
0B
7B
7B
8B
<B
=B
=B
=B
<B
<B
>B
@B
AB
IB
HB
IB
IB
IB
PB
OB
OB
SB
SB
OB
\B
cB
`B
`B
`B
]B
aB
aB
`B
aB
hB
fB
gB
kB
lB
lB
uB
sB
uB
uB
 yB
 zB
 {B
 wB
 yB
 zB
 |B
!B
!}B
!|B
!~G�O�B
#�B
,�B
2�B
9B
<B
EUB
JrB
R�B
T�B
X�B
]�B
b B
gB
l?1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417312016080714173120160807141731  AO  ARCAADJP                                                                    20150226221254    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221254  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221254  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141731  IP                  G�O�G�O�G�O�                