CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:14Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170914  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               UA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^��1   @��^����@6D�/���c�;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    UA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B��B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DV��DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Ds  Ds� Dt  Dty�Dy�=D� �D�W\D���D���D�$�D�` D���D���D�=D�ND��HD���D�D�\{Dڎ�D��RD�3D�O\D�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B34B34B34B'��B/��B7��B?��BG��BO��BX  B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�4Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<� D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DF� DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVs4DV�4DWy�DX  DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dp� Dp��Dqy�Dq��Drs4Dr��Dsy�Ds��Dts4Dy��D��D�T)D��{D�D�!�D�\�D���D�޹D�
D�J�D��D�ֹD��D�YHDڋ�D��D� D�L)D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�  A�%A�1A�1A���A��mA�ȴA�K�A���Aѧ�A�r�A��HA��AΓuA�~�A�(�AǶFA��A��
A�7LA�p�A���A�ZA���A�{A���A��A��A��A�7LA��yA��/A��
A�JA�ZA�hsA�I�A���A��A�r�A�A���A��DA���A��A�  A�S�A��A���A�n�A�Q�A�-A���A�ffA�A�A���A�  A�ffA�\)A�A�A�
=A��^A�M�A��+A�5?A��A��A�I�A��/A��^A��wA���A�-A��
A�x�A��;A� �A�  A���A��/A���A�x�A�oA�JA�VA�~�A���A�"�A� �A�jA�VA�9XA���A�%A���A�+A��hA��yA�VA�ȴA�(�A�#A|�Ay/Ax��Ax�yAx��AxbNAu��ArA�AqoAp��Ap�`Apz�Am|�Al5?Ah �Ae��Aa�A]dZA\��A\�DA\v�A\A[K�AXM�AV�AT~�AQ�AN-AKt�AJbNAJ�AI�FAHz�AGt�AFQ�AD��AC|�AB��AA;dA@A�A@A?��A>�/A<�\A:��A7/A6�jA6E�A5l�A4��A4z�A4I�A3�#A2ȴA1;dA0=qA/x�A.��A.M�A,��A+�TA+;dA*�DA*=qA(�jA&�A$$�A"��A"��A"$�A!�A �jA�yAZA�A��A��A��A(�A�7AȴA$�AO�AbNA/A1'A"�Av�A�^AXA;dA%A�A\)A;dA�jA  A  A
(�A	%A�
A�`AC�A��AVA��AAXA ȴA J@�5?@��@�(�@���@��/@�S�@�@�ȴ@���@��!@��h@� �@�@�dZ@�+@���@�@�/@�b@@��@�z�@�@�$�@�x�@�Q�@�S�@�-@��;@㝲@�p�@ߕ�@�
=@�^5@�Q�@��@�@ڧ�@�n�@�@�I�@��@ם�@�V@�V@Ұ!@�$�@Ѳ-@�9X@��y@�M�@�$�@���@�hs@��@���@ʸR@ʏ\@��@�p�@�%@�A�@�@Ə\@�M�@�{@�Z@öF@�t�@¸R@�@�/@�Ĝ@���@��@�K�@���@��@�O�@���@��h@��@���@��H@�M�@���@�I�@�+@��y@��@�hs@��/@��@�Z@�ƨ@���@�S�@��@���@�?}@��u@�Q�@��m@�C�@�ff@��-@�&�@��j@��@�1'@��@�;d@���@�(�@� �@�I�@���@��u@��@�(�@���@��
@���@�S�@��@�
=@���@�V@�@�G�@�\)@�t�@�K�@���@�~�@�E�@�5?@���@���@�j@�j@��`@���@�j@�Z@��w@���@�Z@��9@���@�r�@��@�t�@��+@���@���@�$�@��h@�V@���@�j@�A�@�1'@�b@��F@��R@�M�@�E�@�-@�@���@��T@��-@�l�@�~�@�=q@�5?@��@�ff@�X@��@��9@�Ĝ@��9@� �@���@��@��y@��R@�~�@�5?@���@��@��@���@���@���@��-@��^@��^@��\@�l�@�t�@���@��@���@��+@��h@���@���@��`@�Ĝ@��@���@�  @�|�@��!@���@�@��#@�5?@�V@��@��@���@��R@�^5@�J@���@��-@�p�@�V@�1@��@���@��P@�K�@�"�@�o@�
=@�@��@��@���@�~�@�5?@��#@�`B@�/@��@���@���@���@��`@���@��@��
@�ƨ@���@��@��@�33@�"�@�"�@�
=@���@���@��H@��!@�=q@��@���@��7@�hs@|��@w�+@q�@iԕ@bM�@Z�1@Q�^@J�@C��@=!�@5��@/ݘ@)N<@%c@!�.@��@�;@��@��@}�@Y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A�  A�%A�1A�1A���A��mA�ȴA�K�A���Aѧ�A�r�A��HA��AΓuA�~�A�(�AǶFA��A��
A�7LA�p�A���A�ZA���A�{A���A��A��A��A�7LA��yA��/A��
A�JA�ZA�hsA�I�A���A��A�r�A�A���A��DA���A��A�  A�S�A��A���A�n�A�Q�A�-A���A�ffA�A�A���A�  A�ffA�\)A�A�A�
=A��^A�M�A��+A�5?A��A��A�I�A��/A��^A��wA���A�-A��
A�x�A��;A� �A�  A���A��/A���A�x�A�oA�JA�VA�~�A���A�"�A� �A�jA�VA�9XA���A�%A���A�+A��hA��yA�VA�ȴA�(�A�#A|�Ay/Ax��Ax�yAx��AxbNAu��ArA�AqoAp��Ap�`Apz�Am|�Al5?Ah �Ae��Aa�A]dZA\��A\�DA\v�A\A[K�AXM�AV�AT~�AQ�AN-AKt�AJbNAJ�AI�FAHz�AGt�AFQ�AD��AC|�AB��AA;dA@A�A@A?��A>�/A<�\A:��A7/A6�jA6E�A5l�A4��A4z�A4I�A3�#A2ȴA1;dA0=qA/x�A.��A.M�A,��A+�TA+;dA*�DA*=qA(�jA&�A$$�A"��A"��A"$�A!�A �jA�yAZA�A��A��A��A(�A�7AȴA$�AO�AbNA/A1'A"�Av�A�^AXA;dA%A�A\)A;dA�jA  A  A
(�A	%A�
A�`AC�A��AVA��AAXA ȴA J@�5?@��@�(�@���@��/@�S�@�@�ȴ@���@��!@��h@� �@�@�dZ@�+@���@�@�/@�b@@��@�z�@�@�$�@�x�@�Q�@�S�@�-@��;@㝲@�p�@ߕ�@�
=@�^5@�Q�@��@�@ڧ�@�n�@�@�I�@��@ם�@�V@�V@Ұ!@�$�@Ѳ-@�9X@��y@�M�@�$�@���@�hs@��@���@ʸR@ʏ\@��@�p�@�%@�A�@�@Ə\@�M�@�{@�Z@öF@�t�@¸R@�@�/@�Ĝ@���@��@�K�@���@��@�O�@���@��h@��@���@��H@�M�@���@�I�@�+@��y@��@�hs@��/@��@�Z@�ƨ@���@�S�@��@���@�?}@��u@�Q�@��m@�C�@�ff@��-@�&�@��j@��@�1'@��@�;d@���@�(�@� �@�I�@���@��u@��@�(�@���@��
@���@�S�@��@�
=@���@�V@�@�G�@�\)@�t�@�K�@���@�~�@�E�@�5?@���@���@�j@�j@��`@���@�j@�Z@��w@���@�Z@��9@���@�r�@��@�t�@��+@���@���@�$�@��h@�V@���@�j@�A�@�1'@�b@��F@��R@�M�@�E�@�-@�@���@��T@��-@�l�@�~�@�=q@�5?@��@�ff@�X@��@��9@�Ĝ@��9@� �@���@��@��y@��R@�~�@�5?@���@��@��@���@���@���@��-@��^@��^@��\@�l�@�t�@���@��@���@��+@��h@���@���@��`@�Ĝ@��@���@�  @�|�@��!@���@�@��#@�5?@�V@��@��@���@��R@�^5@�J@���@��-@�p�@�V@�1@��@���@��P@�K�@�"�@�o@�
=@�@��@��@���@�~�@�5?@��#@�`B@�/@��@���@���@���@��`@���@��@��
@�ƨ@���@��@��@�33@�"�@�"�@�
=@���@���@��H@��!@�=q@��@���@��7G�O�@|��@w�+@q�@iԕ@bM�@Z�1@Q�^@J�@C��@=!�@5��@/ݘ@)N<@%c@!�.@��@�;@��@��@}�@Y11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�sB�mB�mB�sB�yB�yB�yB��B%B�B'�B=qB?}BF�B8RBJB�B%�B1B+B�B  BB�5B�B�B�B�B��B��B��BǮB�?B�9B��B��B�
BI�BaHB`BBbNBe`Be`B_;BT�BP�BE�BC�B?}B>wB>wBB�BC�B>wB6FB1'B33BZB��B�B�1B�DB��B��B�bBl�BN�B8RB)�B �B�B33B>wB-B&�B#�B�B%B�B��B�dB�dB�qB�jB�dB�B�PB�Bm�BM�B �B�B�BoB
��B
��B
�wB
�3B
��B
��B
�=B
}�B
t�B
O�B
>wB
7LB
6FB
49B
0!B
!�B
	7B	��B	��B	��B	�B	�NB	��B	ĜB	�FB	��B	�+B	�B	� B	}�B	z�B	t�B	bNB	S�B	E�B	.B	�B		7B	B	B	B��B��B��B��B�B�B�fB�;B�/B�#B�B��BǮB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�+B�Bu�Bp�Bo�Bm�Bk�BhsBffBdZBcTBbNB`BB_;B[#BYBW
BVBR�BQ�BN�BM�BK�BJ�BI�BI�BI�BH�BJ�BH�BG�BF�BE�BE�BA�BB�B?}B@�BA�B?}BA�BE�BF�BK�BO�BM�BQ�BW
BYBZBXBVBXBZBZBZB[#B]/B_;B`BB`BBaHBaHBffBhsBhsBhsBgmBdZBdZBcTBbNB`BB^5B[#B`BBaHB\)B\)B\)B^5Be`Bk�Bk�Bm�Bo�Br�Bz�B}�B}�B{�B{�B�+B�=B�VB�\B�bB�bB�bB�bB�oB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�-B�3B�3B�?B�^B�jB�qB�}BBƨBȴB��B��B��B��B�B�;B�TB�yB�B��B	  B	B	
=B	JB	bB	oB	{B	�B	�B	�B	�B	�B	�B	!�B	�B	"�B	)�B	(�B	'�B	'�B	(�B	)�B	)�B	)�B	,B	:^B	A�B	B�B	F�B	K�B	R�B	VB	ZB	\)B	\)B	[#B	]/B	]/B	_;B	aHB	ffB	l�B	q�B	q�B	w�B	|�B	~�B	~�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�PB	�PB	�bB	�\B	�\B	�\B	�bB	�uB	�uB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�RB	�qB	�wB	B	B	��B	B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	��B	ĜB	ŢB	ɺB	ǮB	ƨB	ǮB	ĜB	��B	ÖB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�)B	�5B	�5B	�5B	�5B	�;B	�;B	�HB	�HB	�HB	�NB	�TB	�TB	�`B	�`B	�`B	�`B	�fB	�`B	�fB	�fB	�sB	�sB	�yB	�yB	�yB	�;B	��B
�B
�B
$ZB
-]B
4�B
<B
A�B
EmB
J=B
O�B
T�B
Z7B
]/B
`�B
g8B
k�B
pUB
shB
ut11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B߼B��B߼B߼B��B��B��B��B�
B�rB�B ;B5�B7�B>�B0�B�B�B0B �B�{B B�PB�\BֈB�WB�WB�pB�kB�-B�'B�"B�B��B��B��B��B�_BB	BY�BX�BZ�B]�B]�BW�BMMBI4B=�B;�B7�B6�B6�B:�B;�B6�B.�B)yB+�BRlB��B{_B�~B��B��B�
B��Bd�BG*B0�B"OBBB+�B6�B%bB=B+B�B�{B��B�'B��B��B��B��B��B�pB��B{qBe�BF6B+BB B
�B
�%B
�cB
��B
��B
�dB
��B
��B
veB
m-B
HSB
6�B
/�B
.�B
,�B
(�B
BB
�B	�[B	�OB	�IB	�0B	��B	�zB	�B	��B	�KB	�B	{�B	x�B	vwB	sdB	m@B	Z�B	L~B	>)B	&�B	0B	�B��B��B��B��B�mB�hB�PB�&B�B��B��BվBӲBПB�|B�?B��B��B��B��B�|B�pB�pB�eB�RB�:B�"B�/B�B�B�(B�"B�B��B��B�By�Bn[Bi<Bh6Bf)BdBaB^�B\�B[�BZ�BX�BW�BS�BQ�BO�BN�BK�BJ�BGuBFoBDcBC]BBVBBVBBVBAPBC]BAQB@KB?EB>?B>?B:'B;-B8B9!B:'B8B:'B>@B?FBDeBH}BFqBJ�BO�BQ�BR�BP�BN�BP�BR�BR�BR�BS�BU�BW�BX�BX�BY�BY�B_BaBaBaB`B\�B\�B[�BZ�BX�BV�BS�BX�BY�BT�BT�BT�BV�B]�Bd$Bd$Bf0Bh<BkNBsBv�Bv�Bt�Bt�B�B��B��B��B��B��B��B��B�B�B�B�B�B�B�*B�=B�bB�zB�zB�zB�zB�nB�nB�tB��B��B��B��B��B��B��B��B��B�zB�iB�iB�\B�VB�VB�VB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�,B�DB�PB�iB�uBɁBˎBҹB��B��B�B�2B�VB��B��B	�B	�B	�B	B	B	B	B	B	B	%B	DB	cB	PB	iB	"�B	!�B	 �B	 �B	!�B	"�B	"�B	"�B	$�B	2�B	:B	;%B	?>B	D]B	K�B	N�B	R�B	T�B	T�B	S�B	U�B	U�B	W�B	Y�B	^�B	eB	j>B	j>B	pbB	u�B	w�B	w�B	z�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�%B	�,B	�,B	�2B	�>B	�DB	�PB	�WB	�]B	�cB	�nB	�zB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�&B	�&B	�,B	�,B	�,B	�,B	�,B	�,B	�B	�,B	�2B	�JB	�>B	�8B	�>B	�,B	�B	�&B	�2B	�>B	�>B	�DB	�QB	�oB	�oB	�uB	�|B	ˁB	̇B	̇B	̇B	͍B	͍B	͍B	͍B	ΓB	ϙB	ѦB	ԸB	ԸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�G�O�B	��B	�[B
`B
RB
�B
%�B
-cB
4�B
:IB
=�B
B�B
HB
M:B
R�B
U�B
YPB
_�B
d^B
h�B
k�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200619170914    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170914  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170914  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                