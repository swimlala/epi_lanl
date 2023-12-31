CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-11-30T23:01:07Z AOML 3.0 creation; 2016-05-31T19:14:40Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141130230107  20160531121440  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               `A   AO  4051_7090_096                   2C  D   APEX                            5368                            041511                          846 @�'R���1   @�'Sv���@4��hr��dm�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    `A   A   A   @@  @�  @���@���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy,�D�fD�L�D�|�D��fD�fD�C3D��fD���D�fD�S3D�� D�� D�3D�9�D�l�D��fD�3D�FfD�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@y��@���@���A��A>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp  Bx  B��B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCR  CS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DR  DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dts4Dy&gD�3D�I�D�y�D��3D�3D�@ D��3D�ɚD�3D�P D���D���D� D�6gD�i�D��3D� D�C3D�vgD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A�A�JA�JA�oA�{A�{A�{A��A�{A�oA�{A��A��A��A��A��A�{A�oA�oA�JA�%A�A�A���A��A���Aв-AП�AГuA�~�A���A�bNA��A�oA�9XAŰ!A�{A�ffA�`BA�VA���A�`BA�E�A�A�t�A�dZA�Q�A��A�1'A���A��A��A�hsA�oA��A��A���A���A�A�A�;dA�z�A�A�I�A�"�A�ZA��A���A�Q�A���A��hA��mA�  A��
A�bNA��7A���A��FA�r�A�S�A�XA�ZA�O�A�  A��uA�oA��wA�S�A�jA���A�l�A��RA�S�A��`A��+A�ƨA��A���A�9XA��wA���A�ĜAA}�PA}7LA|��A{%Aw�TAsK�Ap�Am�hAg�
Aep�AdjAc��Ab��A`�yA]�A[��A[�AZVAX~�AWK�AV(�AT�yASp�AQ��APM�AO�AN�ANn�ALv�AKO�AJ�HAJQ�AI�wAI33AGVAES�AD��ACp�AA��A@ĜA?p�A>�`A=��A<~�A;|�A:ĜA9�hA8ffA7�#A7/A6  A4Q�A3oA1&�A0�`A/�^A-l�A+�mA*9XA*�A)\)A'�A&n�A%\)A#oA!��A!hsA $�A��A��A��AoA�RA5?A�A33A�DA�A�hA&�AȴA�7AXA�A1'A��AĜA=qA�A�-A/AhsA	��A	\)A	O�A	G�A	"�A	
=AA�AA�PAXA"�A��AI�AƨA�jA|�AA  A ff@�~�@�I�@�|�@��@�V@�o@�v�@�$�@�hs@�j@�V@��@�dZ@�@땁@�R@�x�@蛦@��@�V@��T@��@���@�^@��@߮@�
=@�V@���@��m@�dZ@�C�@�@ڏ\@��@ם�@�;d@�n�@�$�@��#@���@�9X@ӕ�@�@҇+@�V@�p�@Гu@��m@��H@�J@���@�hs@��/@˝�@�-@�/@ț�@�b@Ƨ�@�^5@�/@��@�o@���@�=q@���@��/@��/@��`@�1@�@��R@��+@�J@�X@��9@��
@�33@���@���@�^5@�{@��@�V@� �@�|�@��P@�
=@�ȴ@�^5@��@���@�G�@��9@�9X@��m@���@�t�@�33@�-@�G�@��@�G�@��h@��^@���@��@��@�/@��@�&�@�&�@��`@�r�@���@���@�\)@�o@�~�@��^@���@��/@�I�@��m@�|�@��@���@�@��-@�x�@�O�@�/@��`@���@�r�@�9X@�b@�  @��m@���@�"�@���@�n�@�V@�M�@��^@�7L@���@�j@�9X@�(�@�(�@���@��@�K�@�o@�~�@�-@�{@���@�`B@���@�%@�&�@��j@�A�@�(�@��m@��F@���@��@�"�@���@�n�@�{@���@�p�@���@��`@���@�z�@�Q�@�b@��m@�ƨ@���@�S�@��@�ff@�J@���@���@�`B@�?}@��@��@�Ĝ@���@�bN@�9X@���@��
@�ƨ@���@�\)@�S�@�+@��@���@�n�@�E�@�-@���@���@��^@��-@�p�@��@��`@�j@�I�@�9X@�1'@�  @��w@�l�@�K�@�"�@�
=@��@���@��\@���@�~�@�M�@�=q@��@��-@�p�@�%@��j@��u@��@�Z@�1@��@�|�@�K�@�+@�
=@��@��@��+@�^5@�-@���@��#@���@��-@���@��@�p�@�O�@�&�@���@�bN@�1'@��@���@��P@�;d@��@�@��H@�Q�@���@�  @vV@pb@jJ@b�@ZM�@S�
@K��@DZ@;S�@5/@0r�@)��@$�j@V@�@�@�u@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A�A�JA�JA�oA�{A�{A�{A��A�{A�oA�{A��A��A��A��A��A�{A�oA�oA�JA�%A�A�A���A��A���Aв-AП�AГuA�~�A���A�bNA��A�oA�9XAŰ!A�{A�ffA�`BA�VA���A�`BA�E�A�A�t�A�dZA�Q�A��A�1'A���A��A��A�hsA�oA��A��A���A���A�A�A�;dA�z�A�A�I�A�"�A�ZA��A���A�Q�A���A��hA��mA�  A��
A�bNA��7A���A��FA�r�A�S�A�XA�ZA�O�A�  A��uA�oA��wA�S�A�jA���A�l�A��RA�S�A��`A��+A�ƨA��A���A�9XA��wA���A�ĜAA}�PA}7LA|��A{%Aw�TAsK�Ap�Am�hAg�
Aep�AdjAc��Ab��A`�yA]�A[��A[�AZVAX~�AWK�AV(�AT�yASp�AQ��APM�AO�AN�ANn�ALv�AKO�AJ�HAJQ�AI�wAI33AGVAES�AD��ACp�AA��A@ĜA?p�A>�`A=��A<~�A;|�A:ĜA9�hA8ffA7�#A7/A6  A4Q�A3oA1&�A0�`A/�^A-l�A+�mA*9XA*�A)\)A'�A&n�A%\)A#oA!��A!hsA $�A��A��A��AoA�RA5?A�A33A�DA�A�hA&�AȴA�7AXA�A1'A��AĜA=qA�A�-A/AhsA	��A	\)A	O�A	G�A	"�A	
=AA�AA�PAXA"�A��AI�AƨA�jA|�AA  A ff@�~�@�I�@�|�@��@�V@�o@�v�@�$�@�hs@�j@�V@��@�dZ@�@땁@�R@�x�@蛦@��@�V@��T@��@���@�^@��@߮@�
=@�V@���@��m@�dZ@�C�@�@ڏ\@��@ם�@�;d@�n�@�$�@��#@���@�9X@ӕ�@�@҇+@�V@�p�@Гu@��m@��H@�J@���@�hs@��/@˝�@�-@�/@ț�@�b@Ƨ�@�^5@�/@��@�o@���@�=q@���@��/@��/@��`@�1@�@��R@��+@�J@�X@��9@��
@�33@���@���@�^5@�{@��@�V@� �@�|�@��P@�
=@�ȴ@�^5@��@���@�G�@��9@�9X@��m@���@�t�@�33@�-@�G�@��@�G�@��h@��^@���@��@��@�/@��@�&�@�&�@��`@�r�@���@���@�\)@�o@�~�@��^@���@��/@�I�@��m@�|�@��@���@�@��-@�x�@�O�@�/@��`@���@�r�@�9X@�b@�  @��m@���@�"�@���@�n�@�V@�M�@��^@�7L@���@�j@�9X@�(�@�(�@���@��@�K�@�o@�~�@�-@�{@���@�`B@���@�%@�&�@��j@�A�@�(�@��m@��F@���@��@�"�@���@�n�@�{@���@�p�@���@��`@���@�z�@�Q�@�b@��m@�ƨ@���@�S�@��@�ff@�J@���@���@�`B@�?}@��@��@�Ĝ@���@�bN@�9X@���@��
@�ƨ@���@�\)@�S�@�+@��@���@�n�@�E�@�-@���@���@��^@��-@�p�@��@��`@�j@�I�@�9X@�1'@�  @��w@�l�@�K�@�"�@�
=@��@���@��\@���@�~�@�M�@�=q@��@��-@�p�@�%@��j@��u@��@�Z@�1@��@�|�@�K�@�+@�
=@��@��@��+@�^5@�-@���@��#@���@��-@���@��@�p�@�O�@�&�@���@�bN@�1'@��@���@��P@�;d@��@�@��H@�Q�@���@�  @vV@pb@jJ@b�@ZM�@S�
@K��@DZ@;S�@5/@0r�@)��@$�j@V@�@�@�u@p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�5B�}BB��B�B1'B�yB��B��B�dB��B�B�7B�B�jB�dB�XB�XB�3B��B��B�1Bt�Be`B`BBVBH�BC�BK�BYB{�Bv�B]/B=qB6FB@�BG�BH�BD�B>wB:^B1'B"�BbBB��B�B�B�B��B��BBB��B�B�B�HB��B�!Bz�BB�B'�B�BB
�yB
��B
�^B
��B
�DB
s�B
bNB
N�B
D�B
B�B
=qB
/B
�B	��B	�mB	��B	�B	��B	��B	�{B	�\B	�B	q�B	k�B	gmB	bNB	[#B	T�B	M�B	F�B	A�B	:^B	7LB	49B	49B	33B	+B	%�B	"�B	 �B	�B	�B	hB	
=B	%B	B��B��B��B�B�B�B�yB�fB�BB�/B�#B�B��B��BȴB��B�}B�^B�-B�B�B�3B�-B�B��B��B��B��B��B��B�oB�\B�bB�hB�hB�VB�7B�+B�%B�%B�B�B�B�B�B�B� B~�B~�B}�B|�B|�B{�B|�B}�B|�B|�B{�B{�Bz�By�By�Bz�Bz�Bz�Bz�Bz�By�Bx�Bw�Bu�Bs�Bt�Bt�Bv�Bv�Bu�Bv�Bw�Bw�Bw�Bw�Bx�Bz�Bz�B{�B|�B~�B~�B� B� B�B�B�B�B�%B�7B�DB�PB�PB�VB�oB�{B�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�!B�!B�'B�9B�9B�LB�^B�dB�wB�wBÖBȴB��B��B��B��B�B�5B�;B�BB�TB�`B�sB�B��B��B��B��B��B	B	%B	
=B	JB	\B	oB	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	,B	-B	/B	49B	49B	2-B	6FB	=qB	?}B	B�B	D�B	B�B	K�B	T�B	YB	YB	YB	YB	ZB	[#B	\)B	]/B	]/B	\)B	\)B	]/B	]/B	]/B	^5B	`BB	cTB	ffB	k�B	k�B	k�B	k�B	m�B	q�B	v�B	z�B	|�B	}�B	}�B	� B	� B	�B	�%B	�+B	�7B	�=B	�=B	�DB	�JB	�PB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�?B	�LB	�RB	�^B	�qB	�wB	�}B	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�BB	�HB	�NB	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
	7B
	7B

=B
DB
PB
\B
�B
�B
#�B
%�B
/B
5?B
9XB
=qB
C�B
H�B
P�B
VB
[#B
_;B
bNB
e`B
gmB
l�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�>B��BB�B�B13B�B�B��B�nB��B�B�=B�B�wB�nB�bB�aB�=B�B��B�:Bt�BecB`LBVBH�BC�BK�BYB{�Bv�B]8B=~B6PB@�BG�BH�BD�B>B:jB1-B"�BlBB��B�B�B��B��B��BBB��B�B�B�SB��B�*Bz�BB�B'�B�B*B
�B
��B
�oB
��B
�QB
s�B
b`B
N�B
D�B
B�B
=�B
/+B
�B	��B	�B	��B	�*B	��B	��B	��B	�sB	�/B	q�B	k�B	g�B	bdB	[<B	UB	M�B	F�B	A�B	:xB	7eB	4QB	4SB	3NB	+B	& B	"�B	 �B	�B	�B	�B	
WB	BB	$B��B��B��B��B�B�B�B�B�`B�MB�@B�5B�B��B��B��B��B�B�NB�5B�,B�RB�NB�*B�B�	B��B��B��B��B��B�}B��B��B��B�tB�XB�NB�FB�EB�=B�@B�;B�5B�-B�)B�$BBB~B}B}B|B}B~B}B}B|	B|	B{By�By�B{B{B{B{B{By�Bx�Bw�Bu�Bs�Bt�Bt�Bv�Bv�Bu�Bv�Bw�Bw�Bw�Bw�Bx�B{B{B|
B}BBB�#B�$B�,B�.B�-B�5B�EB�YB�eB�qB�qB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�+B�@B�BB�AB�EB�YB�XB�jB�}B��B��B��BõB��B��B�B�B�B�;B�RB�WB�_B�uB�}B�B��B��B��B��B��B�B	"B	AB	
WB	eB	vB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	,"B	-&B	/5B	4RB	4RB	2GB	6`B	=�B	?�B	B�B	D�B	B�B	K�B	UB	Y1B	Y-B	Y3B	Y1B	Z6B	[;B	\CB	]GB	]IB	\CB	\AB	]GB	]EB	]HB	^OB	`[B	cmB	f~B	k�B	k�B	k�B	k�B	m�B	q�B	v�B	z�B	}B	~B	~B	�B	�B	�#B	�;B	�BB	�NB	�SB	�TB	�[B	�aB	�gB	�sB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�2B	�1B	�9B	�=B	�LB	�NB	�SB	�aB	�jB	�tB	��B	��B	��B	��B	��B	¥B	¤B	ìB	ĳB	ıB	ŷB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�%B	�*B	�2B	�9B	�?B	�CB	�KB	�KB	�HB	�XB	�\B	�cB	�sB	�uB	�uB	�tB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
 B
B
 B
B
B
'B
(B
-B
-B
-B
.B
,B
-B
3B
AB
AB
FB
	MB
	HB

QB
YB
cB
pB
�B
�B
#�B
%�B
/-B
5PB
9jB
=�B
C�B
H�B
P�B
VB
[5B
_LB
b`B
eqB
g{B
l�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214402016053112144020160531121440  AO  ARCAADJP                                                                    20141130230107    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141130230107  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141130230107  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121440  IP                  G�O�G�O�G�O�                