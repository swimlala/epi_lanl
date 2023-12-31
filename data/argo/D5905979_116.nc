CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:21Z creation      
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
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170921  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               tA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��X��1   @���	��@6$�/��c0�hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    tA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Dt��Dy� D�'�D�Z�D�� D��D�%qD�`�D��3D��)D��D�P�D��HD�� D�(RD�\)Dښ�D�ФD�=D�PRD�HD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BX  B_��Bg��Bo��Bx  B��B���B���B���B���B���B���B���B���B���B�33B���B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B�  B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC  C�fC!�fC#��C%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
�4Ds4D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+� D,  D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@s4D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFs4DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj�4Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Drs4Dr��Dsy�Ds��Dty�Dt�gDy��D�${D�W\D���D��gD�">D�]qD�� D���D��D�MqD��D���D�%D�X�Dڗ�D��qD�
D�MD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AծAծAհ!Aհ!Aհ!AլAե�A՗�AծAնFAնFAոRAոRAպ^Aպ^Aպ^Aղ-Aպ^AռjAվwA���A���A���A���A�A�A�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A�ƨA�A���A���AՁA���AΓuAΝ�A�v�A���A�v�A�A�A�1'A�7LA�%A�x�A��+A���A���A��#A���A�ȴA�-A�l�A��7A�|�A�/A�ƨA��A�?}A���A��PA��
A��FA�\)A��DA��A��FA��wA�oA���A�%A�+A�A�;dA�7LA�$�A�?}A��uA���A�/A�$�A�ffA�9XA�7LA�|�A��!A�bA�p�A��7A���A��A�/A�A�A��\A��hA���A���A�bNA��A��TA�VA�M�A��A`BA{ƨAw��As��Aq�#ApȴAm�Aj��Ag&�Ab��AbA`bNA[�7AY�wAWK�AT5?ARI�AP�AN�!AL�AJ��AI��AI��AH��AF�/AF$�AE�AEADv�AD{AC��AB�ABA�AA�
A@�+A=�A;A9�A7�7A5�;A5|�A5
=A4E�A25?A1hsA0(�A/%A.=qA-C�A+�;A*^5A)hsA(ĜA(1'A'�TA'K�A'VA&=qA%hsA$�`A$9XA#�;A#hsA"�uA!7LA�TAK�A��AM�AS�AƨA%A�A�mA�Al�A�HA��AĜA�RAdZA�#A�7AȴA �AC�A��AƨAffAI�A�wA/AĜA1A
��A	��A��A��A��AffA��AdZA��AE�A �A/A �@��H@�p�@�l�@��@�-@�hs@�1'@���@���@�n�@�Ĝ@��@�n�@�&�@�Z@� �@��
@�E�@��@�dZ@���@�&�@�j@��@�33@�E�@�Ĝ@� �@��@�;d@�-@�dZ@�9X@�33@��@��@�M�@�?}@�^5@Դ9@�~�@Ѳ-@щ7@�p�@�I�@�ȴ@���@��@ˮ@��y@�x�@ǥ�@�t�@��9@��;@�"�@�$�@�1@�;d@�n�@��#@���@��D@�bN@��@���@��@�{@���@���@��@��y@���@���@��D@�Z@���@��w@�C�@���@�G�@���@��m@��@�5?@��^@��7@��@�1'@��
@�l�@�v�@�x�@��`@�p�@��7@��@�b@�+@���@���@�ff@�$�@�$�@��T@���@�X@�&�@���@�z�@� �@���@�C�@�
=@��H@�ȴ@���@�v�@���@�O�@�z�@�(�@�  @��;@��@�C�@���@��@��h@��@�`B@�&�@�Ĝ@�z�@�(�@��@�  @�ƨ@�t�@�;d@�
=@��y@��H@���@�^5@�5?@�$�@��#@��@�?}@�%@��@��@�z�@�r�@�j@�Q�@�9X@�  @��@�9X@�Z@��m@�ƨ@���@��
@��;@��F@�l�@�+@���@�M�@�{@��T@���@�O�@��@��@���@�j@�bN@�A�@� �@�(�@��@��
@���@�"�@�@��H@��!@�^5@��@�@��7@�O�@�7L@��@��u@�9X@��@�l�@�33@�
=@���@�ȴ@���@�n�@�=q@�-@���@��-@��-@���@��@�X@�V@�Ĝ@���@�j@�b@�ƨ@��@���@���@���@��P@�S�@�@��H@�ȴ@�~�@�=q@�{@��^@���@���@��@�p�@�`B@�G�@�?}@�?}@�/@��/@��u@�A�@�1'@�1@���@��@���@�t�@�o@���@���@���@��\@���@���@���@�v�@�E�@�-@��@�J@��@�@���@��7@�7L@�%@�%@~� @wW?@n��@h�O@a��@\%�@W�{@P�@F��@>O@7�@1�@,:�@&
�@R�@w�@t�@��@خ@�]@'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AծAծAհ!Aհ!Aհ!AլAե�A՗�AծAնFAնFAոRAոRAպ^Aպ^Aպ^Aղ-Aպ^AռjAվwA���A���A���A���A�A�A�ĜA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A���A�ƨA�A���A���AՁA���AΓuAΝ�A�v�A���A�v�A�A�A�1'A�7LA�%A�x�A��+A���A���A��#A���A�ȴA�-A�l�A��7A�|�A�/A�ƨA��A�?}A���A��PA��
A��FA�\)A��DA��A��FA��wA�oA���A�%A�+A�A�;dA�7LA�$�A�?}A��uA���A�/A�$�A�ffA�9XA�7LA�|�A��!A�bA�p�A��7A���A��A�/A�A�A��\A��hA���A���A�bNA��A��TA�VA�M�A��A`BA{ƨAw��As��Aq�#ApȴAm�Aj��Ag&�Ab��AbA`bNA[�7AY�wAWK�AT5?ARI�AP�AN�!AL�AJ��AI��AI��AH��AF�/AF$�AE�AEADv�AD{AC��AB�ABA�AA�
A@�+A=�A;A9�A7�7A5�;A5|�A5
=A4E�A25?A1hsA0(�A/%A.=qA-C�A+�;A*^5A)hsA(ĜA(1'A'�TA'K�A'VA&=qA%hsA$�`A$9XA#�;A#hsA"�uA!7LA�TAK�A��AM�AS�AƨA%A�A�mA�Al�A�HA��AĜA�RAdZA�#A�7AȴA �AC�A��AƨAffAI�A�wA/AĜA1A
��A	��A��A��A��AffA��AdZA��AE�A �A/A �@��H@�p�@�l�@��@�-@�hs@�1'@���@���@�n�@�Ĝ@��@�n�@�&�@�Z@� �@��
@�E�@��@�dZ@���@�&�@�j@��@�33@�E�@�Ĝ@� �@��@�;d@�-@�dZ@�9X@�33@��@��@�M�@�?}@�^5@Դ9@�~�@Ѳ-@щ7@�p�@�I�@�ȴ@���@��@ˮ@��y@�x�@ǥ�@�t�@��9@��;@�"�@�$�@�1@�;d@�n�@��#@���@��D@�bN@��@���@��@�{@���@���@��@��y@���@���@��D@�Z@���@��w@�C�@���@�G�@���@��m@��@�5?@��^@��7@��@�1'@��
@�l�@�v�@�x�@��`@�p�@��7@��@�b@�+@���@���@�ff@�$�@�$�@��T@���@�X@�&�@���@�z�@� �@���@�C�@�
=@��H@�ȴ@���@�v�@���@�O�@�z�@�(�@�  @��;@��@�C�@���@��@��h@��@�`B@�&�@�Ĝ@�z�@�(�@��@�  @�ƨ@�t�@�;d@�
=@��y@��H@���@�^5@�5?@�$�@��#@��@�?}@�%@��@��@�z�@�r�@�j@�Q�@�9X@�  @��@�9X@�Z@��m@�ƨ@���@��
@��;@��F@�l�@�+@���@�M�@�{@��T@���@�O�@��@��@���@�j@�bN@�A�@� �@�(�@��@��
@���@�"�@�@��H@��!@�^5@��@�@��7@�O�@�7L@��@��u@�9X@��@�l�@�33@�
=@���@�ȴ@���@�n�@�=q@�-@���@��-@��-@���@��@�X@�V@�Ĝ@���@�j@�b@�ƨ@��@���@���@���@��P@�S�@�@��H@�ȴ@�~�@�=q@�{@��^@���@���@��@�p�@�`B@�G�@�?}@�?}@�/@��/@��u@�A�@�1'@�1@���@��@���@�t�@�o@���@���@���@��\@���@���@���@�v�@�E�@�-@��@�J@��@�@���@��7@�7L@�%G�O�@~� @wW?@n��@h�O@a��@\%�@W�{@P�@F��@>O@7�@1�@,:�@&
�@R�@w�@t�@��@خ@�]@'�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
?}B
C�B
�`B�B+B_;B|�B�7B�PB��B�-BĜB��B�B+B�B�B\B�B��B�ZB��B��B��B��B  BB+BDB%BB�B�B�B{B+B��B��BB�B,B(�B�BJB�B�`B�yB�B�NB�B��BƨB�-B��B�=B{�BZBG�B&�BuBB
�B
��B
�FB
��B
�PB
}�B
o�B
e`B
T�B
G�B
8RB
!�B
1B	�fB	��B	ɺB	�9B	��B	�1B	gmB	[#B	P�B	6FB	$�B	�B		7B��B�B�sB�/B�B�B�5B�fB�sB�TB�5B�B��B��B��B�B�HB�BB�B��B��B�RB�!B��B��B��B��B��B�oB�oB�PB�=B�+B�B~�B{�B|�Bz�Bz�Bv�Bw�Bw�Bu�Br�Bq�Bq�Bq�Bo�Bn�Bl�Bk�BjBiyBiyBjBk�Bk�Bk�Bo�Bp�Bp�Bp�Bx�B�B}�Br�Br�Bw�Bw�B{�B�B�Bw�Bz�B�B�B� B}�Bz�B~�B}�B�B~�B� By�Br�Bn�Bq�Bp�Bs�Bt�Br�Bq�Bq�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bs�Bs�Bu�By�B{�B|�B}�B� B|�B� B~�B}�B}�B}�B}�B|�B{�B{�B{�B{�By�Bx�Bt�Bs�Br�Br�Bq�Bo�Bm�BhsBffBdZBdZBcTBdZBaHB`BB^5B^5B^5B\)BW
BW
BR�BQ�BQ�BXB[#B\)B]/B^5B`BB`BBaHBaHBbNBdZBjBm�Bo�Bt�Bx�B{�B|�B}�B~�B� B�B�B�%B�+B�1B�DB�\B�hB�uB�{B��B��B��B��B��B��B�B�3B�RB�XB�}B��B��BĜBƨBǮBɺB��B��B��B�
B�B�/B�;B�HB�`B�fB�fB�mB�mB�sB�B�B�B��B��B��B��B��B��B	B	B	B	%B	1B	PB	bB	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	'�B	'�B	)�B	-B	0!B	2-B	49B	7LB	9XB	:^B	;dB	<jB	<jB	=qB	?}B	C�B	G�B	I�B	L�B	N�B	P�B	R�B	VB	[#B	bNB	gmB	jB	o�B	o�B	p�B	r�B	u�B	y�B	|�B	~�B	�B	�B	�%B	�+B	�7B	�=B	�JB	�PB	�\B	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�?B	�LB	�LB	�^B	�dB	�dB	�jB	�jB	�wB	��B	B	ÖB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
 �B
�B
}B
�B
&�B
./B
2�B
9	B
>]B
E�B
J�B
P�B
U2B
[#B
aHB
f�B
k�B
m)B
r�B
xB
}"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
3AB
3AB
3AB
3AB
3AB
3AB
3AB
4FB
3AB
3AB
3AB
3AB
3AB
3AB
3AB
3AB
3AB
3AB
4FB
3AB
4FB
4FB
3AB
3AB
3AB
3AB
4FB
4FB
4FB
4FB
4FB
4FB
4FB
4FB
4FB
5LB
5LB
5LB
4FB
8_B
�B?B�BS�Bq�B}�B� B�0B��B�HBŐB�SB��BXB9BB��BčB�B�B�B�oB�B��B��B��B��B��B��BcBiBcB	'B��B�B�B��B^B �B�BMB �B�<B�B�+B�=B�BʸBƠB�^B��B��B~�Bp�BN�B<pB�B<B
��B
��B
�RB
�B
��B
�$B
r�B
duB
Z8B
I�B
<�B
-/B
�B	�B	�KB	��B	��B	�#B	��B	} B	\_B	PB	E�B	+=B	�B	�B�3B��B�B�sB�0B�B�B�6B�gB�tB�UB�7B�B�B��B��B�B�JB�DB�B��B��B�YB�)B��B��B��B��B��B�zB�zB�\BIB|8By&BtBp�Bq�Bo�Bo�Bk�Bl�Bl�Bj�Bg�Bf�Bf�Bf�Bd�Bc�Ba�B`�B_�B^�B^�B_�B`�B`�B`�Bd�Be�Be�Be�Bm�By)BsBg�Bg�Bl�Bl�Bp�By*Bx$Bl�Bo�BwBvBuBsBo�BtBsBwBtBuBn�Bg�Bc�Bf�Be�Bh�Bi�Bg�Bf�Bf�Be�Be�Be�Be�Be�Be�Be�Bh�Bh�Bj�Bn�Bp�BrBs
BuBrBuBtBs
Bs
Bs
BsBrBp�Bp�Bp�Bp�Bn�Bm�Bi�Bh�Bg�Bg�Bf�Bd�Bb�B]�B[�BYuBYuBXoBYuBVcBU]BSQBSQBSQBQEBL&BL'BHBG
BG
BM-BP@BQFBRLBSRBU_BU_BVeBVeBWkBYwB_�Bb�Bd�Bi�Bm�BqBr	BsBtBuBv!Bw'B{@B|FB}LB�_B�vB��B��B��B��B��B��B��B��B�B�,B�KB�iB�oB��B��B��B��B��B��B��B��B��B�B�B�2B�DB�PB�\B�tB�zB�zB܁B܁B݇BߓB�B��B��B��B��B��B��B�B�$B�1B�1B�7B�CB	aB	sB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	"B	%0B	'<B	)HB	,ZB	.fB	/lB	0rB	1xB	1xB	2B	4�B	8�B	<�B	>�B	A�B	C�B	E�B	G�B	KB	P.B	WYB	\wB	_�B	d�B	d�B	e�B	g�B	j�B	n�B	q�B	tB	y!B	z'B	{-B	|3B	~?B	EB	�RB	�XB	�cB	�oB	�vB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�,B	�2B	�8B	�DB	�QB	�QB	�bB	�hB	�hB	�nB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�DB	�DB	�DB	�PB	�VB	�[B	�aB	�gB	�gB	�nB	�nB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��G�O�B	��B	��B
|B
�B
~B
#,B
'�B
.B
3ZB
:�B
?�B
E�B
J.B
PB
VDB
[�B
`�B
b$B
g�B
mB
r111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170921    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170921  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170921  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                