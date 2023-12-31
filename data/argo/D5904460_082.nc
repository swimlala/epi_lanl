CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-31T19:18:00Z AOML 3.0 creation; 2016-08-07T21:17:42Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151031191800  20160807141742  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  5285_8895_082                   2C  D   APEX                            6487                            072314                          846 @�{*��;1   @�{��Ɍ@,J��n��c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   B   B   @�ff@�  A   A   A@  A`  A�  A�ffA�ffA�33A�  A�  A�  A�  B   B	33BffB��BffB'33B0  B8  B@ffBG��BP  BX  B`  BhffBp  BxffB��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dys3D�fD�@ D��fD��fD�	�D�33D�i�D�ɚD�fD�9�D��3Dǣ3Dͩ�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
A	�A)�AI�Ai�A���A�\)A�\)A�(�A���A���A���A���Bz�B�B�GB{B �GB)�B2z�B:z�BB�GBJ{BRz�BZz�Bbz�Bj�GBrz�Bz�GB�
>B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�B�C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\C�O\D '�D ��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D	'�D	��D
'�D
��D'�D��D'�D�HD'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D'�D��D '�D ��D!'�D!��D"'�D"��D#'�D#��D$'�D$��D%'�D%��D&'�D&��D''�D'��D('�D(��D)'�D)��D*'�D*��D+'�D+��D,'�D,��D-'�D-��D.'�D.��D/'�D/��D0'�D0��D1'�D1��D2'�D2��D3'�D3��D4'�D4��D5'�D5��D6'�D6��D7'�D7��D8'�D8��D9'�D9��D:'�D:��D;'�D;��D<'�D<��D='�D=��D>'�D>��D?'�D?��D@'�D@��DA'�DA��DB'�DB��DC'�DC��DD'�DD��DE'�DE��DF'�DF��DG'�DG��DH'�DH��DI.DI��DJ'�DJ��DK'�DK��DL'�DL��DM'�DM��DN'�DN��DO'�DO��DP'�DP��DQ'�DQ��DR'�DR��DS'�DS��DT'�DT��DU'�DU��DV'�DV��DW'�DW��DX'�DX��DY'�DY��DZ'�DZ��D['�D[��D\'�D\��D]'�D]��D^'�D^��D_'�D_��D`'�D`��Da'�Da��Db'�Db��Dc'�Dc��Dd'�Dd��De'�De��Df'�Df��Dg'�Dg��Dh'�Dh��Di'�Di��Dj'�Dj��Dk'�Dk��Dl'�Dl��Dm'�Dm��Dn'�Dn��Do'�Do��Dp'�Dp��Dq'�Dq��Dr'�Dr��Ds'�Ds��Dt'�Dt�HDy��D�=D�S�D��=D��=D�qD�G
D�}qD��qD�=D�MqD��
DǷ
Dͽq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�(�A�5?A�;dA�;dA�~�A�v�A�7LA� �A♚A� �A�~�A� �A���A���A�S�A�E�A�A��A��A�C�A�ffA��7A�M�A��A��hA��A��A���A�E�A��+A�{A�A�A�r�A��+Ay�mAo��AkXAg��Ae�Ab��A^  A[��A[&�AZ��AY�AU�;AQ��AP=qAMC�AJ�+AFA@��A<(�A;"�A:=qA9�7A5��A1p�A/�A,ffA)��A%��A$z�A$z�A$A"�\A �A�A�PA7LAt�A�FA�+AdZAl�A��AA�A�A��AQ�A�A5?A+AQ�A��A�A�A�jA��A�HA�A
=AdZA�A�^A�#A�mA��A �AI�A �A�A/A�RA�TA��A7LA�DA��AI�A�Ax�A7LA
=A�A�uA(�A�-AS�A�Av�A�mA\)AdZA
�`A
r�A	��A	VA��A�`A�wA�`A��A��AM�A��A �A�A�;A�hA �/A v�A E�@�ȴ@���@��;@��T@��@�V@���@��y@�J@���@�(�@���@��@�K�@�v�@��T@�p�@�1'@�P@��@��H@�5?@���@�M�@�E�@�{@�-@蛦@睲@���@�^@�&�@�9@�u@��@�!@�V@�=q@�5?@�$�@�@���@���@� �@�|�@�\)@�33@�"�@���@�v�@�5?@�J@��@ݩ�@�p�@�Ĝ@��@���@�o@�-@��@�x�@�Z@�"�@֏\@�M�@��@�p�@�7L@��`@ԋD@��@�|�@�ff@ёh@��@Ь@�(�@��@��;@ϕ�@�ȴ@�V@���@́@̓u@˕�@�ȴ@ʇ+@��@�@ɉ7@�G�@��@�bN@��;@Ǖ�@�@��@�?}@���@�b@�o@�J@��T@���@�p�@�/@��`@�Q�@�;d@��@��!@�M�@�x�@�&�@��j@��u@�(�@��@�33@�~�@�`B@�Q�@�1@�  @�1@�1@�1@�1@���@���@���@��P@��@�\)@���@�V@�J@�`B@�/@��`@�Ĝ@��9@��@�I�@���@�;d@���@�=q@�J@��@�`B@�Q�@��
@�t�@�"�@��+@�^5@�E�@�-@�J@��T@���@��7@��h@��h@�x�@�O�@���@� �@��@���@��@�
=@���@�-@��h@�?}@��@�V@���@��`@��/@���@�Ĝ@��u@�9X@��@�dZ@��R@�^5@�=q@�5?@�$�@��@���@�X@��/@��9@��u@�r�@�(�@�  @��w@��P@�dZ@�K�@�33@�
=@���@��y@�~�@�J@���@��^@��h@�x�@�X@�/@��@��/@��9@�I�@���@�l�@�
=@��H@���@�M�@��@�G�@�&�@���@�Ĝ@��9@���@�z�@�(�@���@�C�@�
=@�v�@��@���@�p�@��@��@�Z@�1@�ƨ@��@�C�@�o@��H@���@���@�~�@�=q@�J@��@��^@���@�hs@���@���@�z�@�Q�@�(�@�b@��m@��;@��
@��@�dZ@�
=@��y@��!@�V@�-@�{@�@���@��@��@��^@��7@�&�@��@�Ĝ@��@�r�@�Q�@�(�@���@�C�@��H@���@�v�@�{@���@���@���@���@�x�@�/@��/@���@�j@�1'@���@�l�@�+@�ȴ@�^5@�@�@��7@�`B@�G�@�%@��9@���@�z�@�I�@�b@���@�|�@�dZ@�C�@�"�@���@��!@�v�@�M�@�=q@�{@���@��#@���@���@��@�G�@�
=@z��@rJ@i7L@b�\@[@R�!@K��@D��@=@6ff@0��@+�@'K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�$�A�(�A�5?A�;dA�;dA�~�A�v�A�7LA� �A♚A� �A�~�A� �A���A���A�S�A�E�A�A��A��A�C�A�ffA��7A�M�A��A��hA��A��A���A�E�A��+A�{A�A�A�r�A��+Ay�mAo��AkXAg��Ae�Ab��A^  A[��A[&�AZ��AY�AU�;AQ��AP=qAMC�AJ�+AFA@��A<(�A;"�A:=qA9�7A5��A1p�A/�A,ffA)��A%��A$z�A$z�A$A"�\A �A�A�PA7LAt�A�FA�+AdZAl�A��AA�A�A��AQ�A�A5?A+AQ�A��A�A�A�jA��A�HA�A
=AdZA�A�^A�#A�mA��A �AI�A �A�A/A�RA�TA��A7LA�DA��AI�A�Ax�A7LA
=A�A�uA(�A�-AS�A�Av�A�mA\)AdZA
�`A
r�A	��A	VA��A�`A�wA�`A��A��AM�A��A �A�A�;A�hA �/A v�A E�@�ȴ@���@��;@��T@��@�V@���@��y@�J@���@�(�@���@��@�K�@�v�@��T@�p�@�1'@�P@��@��H@�5?@���@�M�@�E�@�{@�-@蛦@睲@���@�^@�&�@�9@�u@��@�!@�V@�=q@�5?@�$�@�@���@���@� �@�|�@�\)@�33@�"�@���@�v�@�5?@�J@��@ݩ�@�p�@�Ĝ@��@���@�o@�-@��@�x�@�Z@�"�@֏\@�M�@��@�p�@�7L@��`@ԋD@��@�|�@�ff@ёh@��@Ь@�(�@��@��;@ϕ�@�ȴ@�V@���@́@̓u@˕�@�ȴ@ʇ+@��@�@ɉ7@�G�@��@�bN@��;@Ǖ�@�@��@�?}@���@�b@�o@�J@��T@���@�p�@�/@��`@�Q�@�;d@��@��!@�M�@�x�@�&�@��j@��u@�(�@��@�33@�~�@�`B@�Q�@�1@�  @�1@�1@�1@�1@���@���@���@��P@��@�\)@���@�V@�J@�`B@�/@��`@�Ĝ@��9@��@�I�@���@�;d@���@�=q@�J@��@�`B@�Q�@��
@�t�@�"�@��+@�^5@�E�@�-@�J@��T@���@��7@��h@��h@�x�@�O�@���@� �@��@���@��@�
=@���@�-@��h@�?}@��@�V@���@��`@��/@���@�Ĝ@��u@�9X@��@�dZ@��R@�^5@�=q@�5?@�$�@��@���@�X@��/@��9@��u@�r�@�(�@�  @��w@��P@�dZ@�K�@�33@�
=@���@��y@�~�@�J@���@��^@��h@�x�@�X@�/@��@��/@��9@�I�@���@�l�@�
=@��H@���@�M�@��@�G�@�&�@���@�Ĝ@��9@���@�z�@�(�@���@�C�@�
=@�v�@��@���@�p�@��@��@�Z@�1@�ƨ@��@�C�@�o@��H@���@���@�~�@�=q@�J@��@��^@���@�hs@���@���@�z�@�Q�@�(�@�b@��m@��;@��
@��@�dZ@�
=@��y@��!@�V@�-@�{@�@���@��@��@��^@��7@�&�@��@�Ĝ@��@�r�@�Q�@�(�@���@�C�@��H@���@�v�@�{@���@���@���@���@�x�@�/@��/@���@�j@�1'@���@�l�@�+@�ȴ@�^5@�@�@��7@�`B@�G�@�%@��9@���@�z�@�I�@�b@���@�|�@�dZ@�C�@�"�@���@��!@�v�@�M�@�=q@�{@���@��#@���@���@��G�O�@�
=@z��@rJ@i7L@b�\@[@R�!@K��@D��@=@6ff@0��@+�@'K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBD�BD�BD�BD�BD�B:^B2-B5?BI�B�qB.B�JB�3B�B	1B	33B	:^B	m�B	��B	�'B	�!B	�RB	�dB	�RB	�?B	��B	��B	�hB	�B	v�B	r�B	_;B	W
B	N�B	D�B	<jB	5?B	1'B	1'B	/B	2-B	>wB	D�B	B�B	@�B	<jB	=qB	N�B	ZB	YB	D�B	+B	�B	�B	{B	oB	PB	VB	B��B�mB�
B��B�RB�LB�?B�'B�B��B��B�BƨB�)B�B�yB�B�B��B	%B	�B	 �B	 �B	2-B	<jB	C�B	A�B	@�B	D�B	N�B	`BB	ffB	jB	m�B	s�B	�B	�%B	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�9B	�9B	�LB	�}B	�jB	�XB	�?B	�-B	�B	��B	��B	��B	��B	��B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�XB	�dB	�jB	�qB	��B	�}B	�wB	��B	��B	ÖB	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�)B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�5B	�;B	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�mB	�sB	�sB	�mB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�`B	�ZB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
	7B

=B
DB
JB
JB
JB
DB
JB
JB
DB
	7B
	7B
	7B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
JB
PB
PB
PB
PB
PB
PB
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
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
)�B
+B
+B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
5?B
<jB
@�B
F�B
K�B
P�B
VB
ZB
^5B
bNB
gmB
l�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  BDqBDoBDmBDoBDnB:/B2 B5BI�B�CB-�B�B�B��B	B	3B	:?B	moB	��B	��B	��B	�&B	�9B	�%B	�B	��B	�~B	�9B	��B	v�B	r�B	_B	V�B	N�B	DlB	<8B	5B	0�B	0�B	.�B	1�B	>EB	DhB	BXB	@NB	<4B	==B	N�B	Y�B	X�B	DeB	*�B	\B	\B	DB	9B	B	!B	�B�B�7B��B�RB�B�B�B��B��B��B��B��B�qB��B�FB�BB�NB�_B�B	�B	qB	 �B	 �B	1�B	<,B	CYB	AMB	@FB	D^B	N�B	`B	f%B	j@B	mUB	sxB	��B	��B	�B	�3B	�;B	�GB	�SB	�dB	��B	��B	�sB	�eB	�\B	�\B	�_B	�NB	�[B	�QB	�SB	�cB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�:B	�&B	�B	��B	��B	��B	��B	�oB	�EB	�_B	�CB	�,B	�2B	�sB	��B	��B	�|B	�wB	�oB	�qB	��B	�|B	�jB	�^B	�QB	�IB	�?B	�7B	�PB	�aB	�kB	�gB	�\B	�MB	�]B	�bB	�gB	�nB	�pB	�yB	��B	��B	��B	�B	�B	�$B	�,B	�;B	�7B	�1B	�=B	�CB	�OB	�QB	�VB	�^B	�hB	�hB	�lB	�mB	�B	̅B	̆B	ˀB	�{B	�yB	�zB	�|B	�{B	�B	̇B	̆B	̅B	͌B	͋B	ПB	ӱB	ӲB	ԸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�-B	�%B	�*B	�+B	�%B	�%B	�$B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�%B	�)B	�*B	�1B	�0B	�1B	�5B	�=B	�=B	�=B	�AB	�CB	�IB	�BB	�AB	�GB	�PB	�NB	�MB	�`B	�`B	�^B	�^B	�`B	�`B	�^B	�nB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
	�B

�B
�B
�B
�B

�B
B
B

�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B

�B

�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B

B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
%B
*B
-B
*B
0B
6B
4B
5B
6B
8B
<B
;B
>B
CB
BB
AB
JB
OB
PB
SB
ZB
]B
_B
`B
aB
`B
dB
hB
kB
kB
kB
kB
lB
mB
lB
mB
mB
mB
lB
mB
mB
kB
lB
lB
iB
lB
jB
mB
kB
iB
sB
tB
 xB
 zB
 yB
 {B
 wB
 yB
 zB
 zB
 zB
!|B
"�B
"�B
"�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
)�B
*�B
*�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�G�O�B
,�B
4�B
<B
@5B
F]B
KxB
P�B
U�B
Y�B
]�B
bB
gB
l>B
nL1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417422016080714174220160807141742  AO  ARCAADJP                                                                    20151031191800    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151031191800  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151031191800  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141742  IP                  G�O�G�O�G�O�                