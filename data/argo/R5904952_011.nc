CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:07Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190507  20181005190507  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @פ�4 �1   @פ׬�)�@3+��Q��c�t�j~�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  A�33B��B  BffB   B(  B0  B8  B?��BH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"�C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB�CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D �fDfD�fDfDy�D  D� D  D�fD  Dy�D��D� D  D� D  D� D	  D	� D	��D
y�D  D�fD  D� D  D� D  D�fDfD�fDfD� D��D� DfD�fD  D� DfD� D  D�fD  Dy�D  D�fD  D� D  Dy�D��D� D  D� D  D� D  D� D  Dy�D  D� D   D � D ��D!� D"  D"� D#fD#�fD$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/y�D/��D0y�D0��D1y�D2  D2� D2��D3y�D4  D4�fD5  D5y�D6  D6� D6��D7y�D8  D8� D9  D9�fD:  D:� D;  D;y�D<  D<� D=  D=� D=��D>� D?fD?�fD@fD@�fDA  DAy�DA��DBy�DB��DCy�DD  DD� DE  DE�fDF  DFy�DF��DGy�DG��DH� DIfDI� DJ  DJ� DK  DK�fDLfDL�fDM  DMy�DN  DN� DN��DO� DPfDP�fDQ  DQ�fDR  DRy�DS  DS� DT  DT� DT��DUy�DVfDV� DV��DW� DXfDX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D^��D_� D`fD`� Da  Da�fDbfDb� Dc  Dc�fDd  Dd� Dd��Dey�Df  Dfy�Df��Dgy�Dh  Dhy�Dh��Di� DjfDj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDrfDr�fDs  Dsy�Ds��Dty�Du  Du�fDvfDv�fDw  Dwy�Dx  DxS3Dy�D�$)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@���A�A!�AA�Aa�A���A���A���A�(�A���A���A���A���B {B{Bz�B�GB z�B(z�B0z�B8z�B@{BHz�BP{BX{B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�
>C C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C8RC 8RC"8RC$�C&�C(C*�C,�C.�C0�C2�C4�C6�C8C:C<�C>�C@�CB8RCD�CF�CH�CJ�CL�CN8RCP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|C~�C�\C��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C��C��C�\C�\C�\C�\C�\C�\C��C��C�\C�\C�\C�\C�\C��C�\C�\C�\C�\C�)C�)C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C��C��C��C�\C�\C�\C�\C�)C�)C�\C�\C�\C�\C��C�\C��C�\C�\C�\C�\C�\C�)C�\C�\C�)C�\C�\C�\C�)C�\C��C�\C�\C�)C�)C�\C�\C�\C��C�\C�\C��C�\C�)C�\C��C�\C�)C�\C�\C�\C�\C�\C��C��C�\C�\C��C��C�\C�\C�\C�\C�\C�)C�\C�\C�\C�\C�\C�\D �D �DD�DD�HD�D��D�D�D�D�HDHD��D�D��D�D��D	�D	��D
HD
�HD�D�D�D��D�D��D�D�DD�DD��DHD��DD�D�D��DD��D�D�D�D�HD�D�D�D��D�D�HDHD��D�D��D�D��D�D��D�D�HD�D��D �D ��D!HD!��D"�D"��D#D#�D$�D$��D%�D%�HD&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-D-��D.�D.��D/�D/�HD0HD0�HD1HD1�HD2�D2��D3HD3�HD4�D4�D5�D5�HD6�D6��D7HD7�HD8�D8��D9�D9�D:�D:��D;�D;�HD<�D<��D=�D=��D>HD>��D?D?�D@D@�DA�DA�HDBHDB�HDCHDC�HDD�DD��DE�DE�DF�DF�HDGHDG�HDHHDH��DIDI��DJ�DJ��DK�DK�DLDL�DM�DM�HDN�DN��DOHDO��DPDP�DQ�DQ�DR�DR�HDS�DS��DT�DT��DUHDU�HDVDV��DWHDW��DXDX��DY�DY��DZHDZ��D[�D[��D\�D\��D]�D]�D^�D^��D_HD_��D`D`��Da�Da�DbDb��Dc�Dc�Dd�Dd��DeHDe�HDf�Df�HDgHDg�HDh�Dh�HDiHDi��DjDj��DkHDk�HDl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��DqDq�DrDr�Ds�Ds�HDtHDt�HDu�Du�DvDv�Dw�Dw�HDx�DxZ�Dy��D�( 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��;A��;A��;A���A�ƨA͝�A�v�A�t�A�\)A�(�A�%A���A̬Ạ�Ạ�A̴9A̼jA̾wA̺^A̶FA̼jA̶FA̲-A̮A̬A̩�A̼jA���A���A��A���A�bA��/A���Aˡ�A��
A�`BA�1'A�A���Aɧ�A�\)A���A�1AǕ�AǏ\Aǟ�A���A��Aǡ�A�33A�E�A�
=Aƛ�A�dZA�33A��mA���A�|�A��yAĸRAđhA�p�A�$�Aå�A��A�%A��9A���A�
=A�M�A�K�A��A�ZA�hsA��HA��A�33A�C�A��A�1'A�;dA�G�A��jA��jA���A�ffA��A�|�A��A�hsA���A��HA���A�\)A�ZA�-A���A�&�A�oA��A�5?A�ƨA���A�=qA���A��A�VA��7A� �A��A��
A��hA�z�A��wA�t�A��A|ĜA|�+A{��A{XAz=qAy�Ax�DAx9XAv��Ao?}Ai�AhA�Ae�-Aa��A\��AYS�AT(�ARAQ`BAM�#AK;dAHZAFZAE�#AEK�AD��AD-ACS�AA��A>��A>A<��A:bNA9�A9S�A8�/A7�A4ĜA2��A2M�A1�7A1;dA0��A0^5A.�/A-��A,~�A*�yA)K�A'��A'�A'G�A'oA&��A%�A$��A#�;A#t�A"�yA!�A �RAG�A��A��AK�A�\AVAA�+A�#A��A��A��A��AjA�uA��A��A33An�A��A
�DA	�PA�-A�jA��Ax�A+AXA��A��A jA @��P@�;d@��+@���@�;d@�7L@�C�@���@���@��R@�$�@��#@��@��`@���@��
@�+@�O�@�9X@�C�@��@�=q@�7@�G�@�`B@�p�@�&�@�u@�  @땁@�
=@�n�@���@�^5@�/@��@�{@��@ݩ�@�S�@�x�@�t�@��@�V@�Ĝ@ӍP@���@��@�~�@�x�@��@���@ЋD@��@Ώ\@�@��#@�&�@��m@ʗ�@ɺ^@Ɂ@�j@�l�@��@��@�j@Å@\@���@°!@�~�@�ff@�V@�$�@��7@��9@���@��@�{@��h@�j@��@���@��y@���@�@���@��#@�E�@��@��`@��u@�bN@�(�@�ƨ@�l�@�v�@���@��7@�7L@�Z@��@���@���@�~�@�v�@��+@�E�@��@��@�n�@��@�o@�v�@�M�@�@��#@�p�@��-@�p�@��j@�%@��@��u@�\)@���@�M�@���@�+@���@��-@�%@��j@��@��u@�A�@�1@��m@��
@��P@�|�@�\)@�S�@�t�@�|�@�\)@�\)@�C�@��@�5?@�O�@��`@�A�@�1'@�1'@���@��w@�|�@�|�@�|�@�t�@�t�@�t�@�l�@�K�@�o@�@���@�
=@��@��w@��@�\)@�C�@�-@��/@�I�@�t�@�o@��@��!@��\@�v�@�ff@�^5@�v�@�ff@���@��-@���@��@��@���@��m@���@�+@���@�J@���@���@��@�`B@��@�Q�@�A�@�9X@�  @��F@��P@�+@���@���@��!@�ff@�@�@���@�p�@�`B@�V@��@��D@��D@�z�@�r�@�bN@�(�@��@���@�S�@�o@���@��y@��@���@���@�v�@�V@�M�@�V@�^5@�ff@�M�@���@���@�@�@�@���@���@���@���@�hs@�/@��`@�r�@�b@�  @�ƨ@���@�|�@�dZ@��y@�v�@�5?@�J@��T@��^@��h@�p�@�G�@���@��u@�z�@�1'@��
@��w@���@�|�@�o@���@�J@��@�`B@��'@|��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��
A��;A��;A��;A���A�ƨA͝�A�v�A�t�A�\)A�(�A�%A���A̬Ạ�Ạ�A̴9A̼jA̾wA̺^A̶FA̼jA̶FA̲-A̮A̬A̩�A̼jA���A���A��A���A�bA��/A���Aˡ�A��
A�`BA�1'A�A���Aɧ�A�\)A���A�1AǕ�AǏ\Aǟ�A���A��Aǡ�A�33A�E�A�
=Aƛ�A�dZA�33A��mA���A�|�A��yAĸRAđhA�p�A�$�Aå�A��A�%A��9A���A�
=A�M�A�K�A��A�ZA�hsA��HA��A�33A�C�A��A�1'A�;dA�G�A��jA��jA���A�ffA��A�|�A��A�hsA���A��HA���A�\)A�ZA�-A���A�&�A�oA��A�5?A�ƨA���A�=qA���A��A�VA��7A� �A��A��
A��hA�z�A��wA�t�A��A|ĜA|�+A{��A{XAz=qAy�Ax�DAx9XAv��Ao?}Ai�AhA�Ae�-Aa��A\��AYS�AT(�ARAQ`BAM�#AK;dAHZAFZAE�#AEK�AD��AD-ACS�AA��A>��A>A<��A:bNA9�A9S�A8�/A7�A4ĜA2��A2M�A1�7A1;dA0��A0^5A.�/A-��A,~�A*�yA)K�A'��A'�A'G�A'oA&��A%�A$��A#�;A#t�A"�yA!�A �RAG�A��A��AK�A�\AVAA�+A�#A��A��A��A��AjA�uA��A��A33An�A��A
�DA	�PA�-A�jA��Ax�A+AXA��A��A jA @��P@�;d@��+@���@�;d@�7L@�C�@���@���@��R@�$�@��#@��@��`@���@��
@�+@�O�@�9X@�C�@��@�=q@�7@�G�@�`B@�p�@�&�@�u@�  @땁@�
=@�n�@���@�^5@�/@��@�{@��@ݩ�@�S�@�x�@�t�@��@�V@�Ĝ@ӍP@���@��@�~�@�x�@��@���@ЋD@��@Ώ\@�@��#@�&�@��m@ʗ�@ɺ^@Ɂ@�j@�l�@��@��@�j@Å@\@���@°!@�~�@�ff@�V@�$�@��7@��9@���@��@�{@��h@�j@��@���@��y@���@�@���@��#@�E�@��@��`@��u@�bN@�(�@�ƨ@�l�@�v�@���@��7@�7L@�Z@��@���@���@�~�@�v�@��+@�E�@��@��@�n�@��@�o@�v�@�M�@�@��#@�p�@��-@�p�@��j@�%@��@��u@�\)@���@�M�@���@�+@���@��-@�%@��j@��@��u@�A�@�1@��m@��
@��P@�|�@�\)@�S�@�t�@�|�@�\)@�\)@�C�@��@�5?@�O�@��`@�A�@�1'@�1'@���@��w@�|�@�|�@�|�@�t�@�t�@�t�@�l�@�K�@�o@�@���@�
=@��@��w@��@�\)@�C�@�-@��/@�I�@�t�@�o@��@��!@��\@�v�@�ff@�^5@�v�@�ff@���@��-@���@��@��@���@��m@���@�+@���@�J@���@���@��@�`B@��@�Q�@�A�@�9X@�  @��F@��P@�+@���@���@��!@�ff@�@�@���@�p�@�`B@�V@��@��D@��D@�z�@�r�@�bN@�(�@��@���@�S�@�o@���@��y@��@���@���@�v�@�V@�M�@�V@�^5@�ff@�M�@���@���@�@�@�@���@���@���@���@�hs@�/@��`@�r�@�b@�  @�ƨ@���@�|�@�dZ@��y@�v�@�5?@�J@��T@��^@��h@�p�@�G�@���@��u@�z�@�1'@��
@��w@���@�|�@�o@���@�J@��@�`B@��'@|��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
%�B
%�B
$�B
$�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
(�B
+B
,B
-B
0!B
2-B
49B
9XB
<jB
?}B
H�B
T�B
XB
hsB
�+B
�7B
�1B
�7B
��B
��B
��B
��B
��B
�/B
�yB
��BoB�B�B9XBS�BdZBl�Bv�B�%B�uB�uB�DB�B�B�B�\B��B��B��B�B�!B�3B�?B�qB�B%B33BN�BM�B9XB0!B)�B'�B�B�B�B�B�B�B�B%�B-B.B(�B!�B�BB��B�B�B�B�ZB�B��B�dB��B��B�uB�\B�BbNBI�B<jB�BDB
�BB
�'B
�B
�B
��B
��B
��B
~�B
_;B
8RB
�B
�B
�B
\B
%B	��B	��B	�B	�BB	�?B	�oB	�%B	s�B	\)B	B�B	0!B	�B	�B	bB	B��B�B�B�B�B�B�mB�ZB�HB�;B�/B�
B��BɺBǮB��B�XB�?B�-B�'B�'B�'B�!B�B�B��B��B��B��B��B��B��B�{B�uB�bB�VB�VB�\B�hB�hB�oB�uB�{B�uB�oB�hB�uB�oB�\B�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�=B�PB�oB�\B�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�-B�-B�-B�-B�3B�'B�'B�B�B�3B�3B�'B�-B�9B�9B�RB�^B�^B�^B�^B�dB�dB�^B�^B�wB��BÖBĜBȴB��B�B�5B�5B�NB�`B�yB�B�B�B�B��B	B	B	B	B	  B��B��B	  B	B	B		7B	VB	{B	�B	�B	�B	�B	�B	!�B	(�B	49B	6FB	7LB	7LB	7LB	8RB	:^B	<jB	>wB	?}B	?}B	B�B	E�B	I�B	J�B	L�B	O�B	Q�B	W
B	ffB	x�B	�B	�JB	�JB	�DB	�DB	�=B	�7B	�%B	y�B	s�B	s�B	v�B	z�B	y�B	v�B	x�B	|�B	�B	�=B	�VB	�VB	�JB	�DB	�DB	�JB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�3B	�-B	�-B	�3B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�qB	�qB	��B	ƨB	ȴB	ɺB	��B	��B	�
B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�BB	�TB	�TB	�TB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

=B

=B

=B
JB
JB
JB
JB
PB
VB
\B
bB
oB
 B
!H222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
%�B
%�B
$�B
$�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
(�B
+B
,B
-B
0!B
2-B
49B
9XB
<jB
?}B
H�B
T�B
XB
hsB
�+B
�7B
�1B
�7B
��B
��B
��B
��B
��B
�/B
�yB
��BoB�B�B9XBS�BdZBl�Bv�B�%B�uB�uB�DB�B�B�B�\B��B��B��B�B�!B�3B�?B�qB�B%B33BN�BM�B9XB0!B)�B'�B�B�B�B�B�B�B�B%�B-B.B(�B!�B�BB��B�B�B�B�ZB�B��B�dB��B��B�uB�\B�BbNBI�B<jB�BDB
�BB
�'B
�B
�B
��B
��B
��B
~�B
_;B
8RB
�B
�B
�B
\B
%B	��B	��B	�B	�BB	�?B	�oB	�%B	s�B	\)B	B�B	0!B	�B	�B	bB	B��B�B�B�B�B�B�mB�ZB�HB�;B�/B�
B��BɺBǮB��B�XB�?B�-B�'B�'B�'B�!B�B�B��B��B��B��B��B��B��B�{B�uB�bB�VB�VB�\B�hB�hB�oB�uB�{B�uB�oB�hB�uB�oB�\B�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�=B�PB�oB�\B�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�-B�-B�-B�-B�3B�'B�'B�B�B�3B�3B�'B�-B�9B�9B�RB�^B�^B�^B�^B�dB�dB�^B�^B�wB��BÖBĜBȴB��B�B�5B�5B�NB�`B�yB�B�B�B�B��B	B	B	B	B	  B��B��B	  B	B	B		7B	VB	{B	�B	�B	�B	�B	�B	!�B	(�B	49B	6FB	7LB	7LB	7LB	8RB	:^B	<jB	>wB	?}B	?}B	B�B	E�B	I�B	J�B	L�B	O�B	Q�B	W
B	ffB	x�B	�B	�JB	�JB	�DB	�DB	�=B	�7B	�%B	y�B	s�B	s�B	v�B	z�B	y�B	v�B	x�B	|�B	�B	�=B	�VB	�VB	�JB	�DB	�DB	�JB	�oB	�oB	�oB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�3B	�-B	�-B	�3B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�qB	�qB	��B	ƨB	ȴB	ɺB	��B	��B	�
B	�#B	�#B	�#B	�#B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�BB	�TB	�TB	�TB	�`B	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

=B

=B

=B
JB
JB
JB
JB
PB
VB
\B
bB
oB
 B
!H222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190507                              AO  ARCAADJP                                                                    20181005190507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190507  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190507  QCF$                G�O�G�O�G�O�8000            