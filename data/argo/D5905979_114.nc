CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:20Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170920  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               rA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���,n~1   @�있���@6!�7Kƨ�c-7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    rA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy��D�	HD�G\D���D�ƸD��D�X D���D��D�!HD�_\D���D���D��D�V�Dڜ{D���D��D�VD�qD��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B�  B˙�B���Bә�B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC  C�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>�4D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLs4DL��DMy�DM��DNy�DN�4DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTs4DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��DtfgDy��D�D�D)D��\D�ÅD��D�T�D��{D���D�D�\)D���D�ٚD�RD�S�DڙHD�ؤD��D�R�D�>D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�G�A�I�A�I�A�K�A�I�A�I�A�I�A�G�A�K�A�K�A�K�A�E�A�G�A�G�A�E�A�G�A�G�A�C�A�A�A�C�A��
A�O�A���AڅA�M�A��A٧�A�E�A�%Aش9A�/A��mA֏\A�x�A�Q�A�bA�;dAǮAĺ^A�K�A�S�A�=qA��A��/A��A��
A�~�A��A�A�G�A���A�^5A�M�A��HA�v�A�{A���A��A�VA��A�hsA��7A�M�A�ZA���A���A�E�A��A���A��A��A�Q�A��A��A�Q�A�E�A�K�A�jA��A�+A��^A�jA��RA��wA�S�A��PA�oA��A���A���A��A�bNA��PA�  A��A��HA�G�A�t�A�/A��TA�  A�I�A��yA��A��A�O�A��A�O�A�|�A�33A�S�A�t�A��`A���A�bA~��A{��A{"�Ax5?At��Ao��Ak+Ah�!Af��Adn�AbffA_�A\�AY�AX �AVE�AT��AR~�AP��AM��AK&�AJ��AIO�AG�wAFffAE33AC��ABĜAB=qA?�hA?��A>��A<�/A;`BA9\)A6��A5A3x�A2��A2�A0�A.��A-`BA,A�A,{A+�FA*�DA*{A(��A(�A&��A%�FA$�A#�A"^5A ��A�A�A�7A?}AE�A��AS�A�mAjA��A$�A�PA+A�uA�wA+A��A�RAE�A�jA�`A�yA�DA�
A��AQ�A�AbAƨA
Q�A�\A�DA��A�AVAJAffA�-A �\A -@��P@�v�@��@��P@���@��R@�r�@�M�@�j@�l�@�o@��@���@��y@�ȴ@��@��@��
@��@�1'@�E�@�9X@���@�hs@�Z@��;@�\)@ޏ\@�J@��@ܼj@ܬ@�dZ@��T@��@�S�@�^5@ҏ\@�ff@ѡ�@���@У�@�j@�;d@��@��@ə�@�V@�@�@�S�@��@�M�@��@�\)@��@��@�J@�hs@��m@��F@���@��@��9@�S�@�-@���@�K�@�;d@��P@���@��@�33@�?}@�dZ@��D@��@��@�O�@��/@�@�  @��R@�^5@��-@���@��@��@��R@�M�@��!@�33@��+@�E�@�O�@���@��j@��@�I�@�A�@��j@��D@���@�dZ@��@��!@�n�@�M�@��@���@��-@��@���@���@�b@��;@���@�@�G�@�7L@��@�V@�%@��`@�j@���@��P@�t�@�@���@��m@�\)@��@�;d@�dZ@�C�@�E�@�%@�bN@�  @�S�@�;d@���@�M�@���@�J@���@�O�@�?}@�7L@�&�@��`@�(�@��
@��@�o@��H@��+@�v�@�v�@���@�ff@��+@�@��@���@�I�@�z�@���@�\)@�S�@�K�@�C�@��@���@�5?@�V@�-@���@��7@��7@�O�@�/@�%@�Ĝ@��u@�I�@��@��m@���@�I�@�A�@�b@��@�33@���@��@���@�5?@�@�/@�V@�Ĝ@���@��D@�9X@�9X@�9X@�1@�l�@�+@�
=@��y@���@�ȴ@���@���@�v�@�^5@�V@�E�@�$�@�{@�@���@���@�p�@�?}@��@���@��@��/@�Ĝ@���@��D@�z�@�bN@�A�@�  @��m@���@�|�@�;d@��y@��H@��@��H@��!@��!@���@�E�@��T@�@���@�x�@�X@��@��@��/@��9@��u@�Q�@�  @�ƨ@��F@�dZ@�33@���@�~�@�E�@�-@��@�@���@���@��-@��e@{� @s��@i�@c|�@[o�@R�6@K�+@E��@@Ɇ@;t�@5�@.3�@'s@!X@p;@�-@��@�)@	�@1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�G�A�G�A�I�A�I�A�K�A�I�A�I�A�I�A�G�A�K�A�K�A�K�A�E�A�G�A�G�A�E�A�G�A�G�A�C�A�A�A�C�A��
A�O�A���AڅA�M�A��A٧�A�E�A�%Aش9A�/A��mA֏\A�x�A�Q�A�bA�;dAǮAĺ^A�K�A�S�A�=qA��A��/A��A��
A�~�A��A�A�G�A���A�^5A�M�A��HA�v�A�{A���A��A�VA��A�hsA��7A�M�A�ZA���A���A�E�A��A���A��A��A�Q�A��A��A�Q�A�E�A�K�A�jA��A�+A��^A�jA��RA��wA�S�A��PA�oA��A���A���A��A�bNA��PA�  A��A��HA�G�A�t�A�/A��TA�  A�I�A��yA��A��A�O�A��A�O�A�|�A�33A�S�A�t�A��`A���A�bA~��A{��A{"�Ax5?At��Ao��Ak+Ah�!Af��Adn�AbffA_�A\�AY�AX �AVE�AT��AR~�AP��AM��AK&�AJ��AIO�AG�wAFffAE33AC��ABĜAB=qA?�hA?��A>��A<�/A;`BA9\)A6��A5A3x�A2��A2�A0�A.��A-`BA,A�A,{A+�FA*�DA*{A(��A(�A&��A%�FA$�A#�A"^5A ��A�A�A�7A?}AE�A��AS�A�mAjA��A$�A�PA+A�uA�wA+A��A�RAE�A�jA�`A�yA�DA�
A��AQ�A�AbAƨA
Q�A�\A�DA��A�AVAJAffA�-A �\A -@��P@�v�@��@��P@���@��R@�r�@�M�@�j@�l�@�o@��@���@��y@�ȴ@��@��@��
@��@�1'@�E�@�9X@���@�hs@�Z@��;@�\)@ޏ\@�J@��@ܼj@ܬ@�dZ@��T@��@�S�@�^5@ҏ\@�ff@ѡ�@���@У�@�j@�;d@��@��@ə�@�V@�@�@�S�@��@�M�@��@�\)@��@��@�J@�hs@��m@��F@���@��@��9@�S�@�-@���@�K�@�;d@��P@���@��@�33@�?}@�dZ@��D@��@��@�O�@��/@�@�  @��R@�^5@��-@���@��@��@��R@�M�@��!@�33@��+@�E�@�O�@���@��j@��@�I�@�A�@��j@��D@���@�dZ@��@��!@�n�@�M�@��@���@��-@��@���@���@�b@��;@���@�@�G�@�7L@��@�V@�%@��`@�j@���@��P@�t�@�@���@��m@�\)@��@�;d@�dZ@�C�@�E�@�%@�bN@�  @�S�@�;d@���@�M�@���@�J@���@�O�@�?}@�7L@�&�@��`@�(�@��
@��@�o@��H@��+@�v�@�v�@���@�ff@��+@�@��@���@�I�@�z�@���@�\)@�S�@�K�@�C�@��@���@�5?@�V@�-@���@��7@��7@�O�@�/@�%@�Ĝ@��u@�I�@��@��m@���@�I�@�A�@�b@��@�33@���@��@���@�5?@�@�/@�V@�Ĝ@���@��D@�9X@�9X@�9X@�1@�l�@�+@�
=@��y@���@�ȴ@���@���@�v�@�^5@�V@�E�@�$�@�{@�@���@���@�p�@�?}@��@���@��@��/@�Ĝ@���@��D@�z�@�bN@�A�@�  @��m@���@�|�@�;d@��y@��H@��@��H@��!@��!@���@�E�@��T@�@���@�x�@�X@��@��@��/@��9@��u@�Q�@�  @�ƨ@��F@�dZ@�33@���@�~�@�E�@�-@��@�@���@���G�O�@��e@{� @s��@i�@c|�@[o�@R�6@K�+@E��@@Ɇ@;t�@5�@.3�@'s@!X@p;@�-@��@�)@	�@1�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�`B
�`B
�`B
�fB
�fB
�`B
�`B
�`B
�fB
�fB
�`B
�`B
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�fB
��BBBBVB�B{BB1BJBPB�B�B�B�BB
�FB
��B
ɺB
�B
�BB
�BB
�B  B�BI�BaHBhsBt�B��B��B�jBĜBɺB��B��B�B�B�HB�yB�sB��BB��BB�B��B�qB�B��B��B��B�3B�B�B��B�BO�BB�dB�uB�%B��Br�B`BB]/BaHBv�Bs�BgmB`BB[#BXBH�B<jB1'B.BK�B_;BYB\)B]/BG�B9XB/B�BJBB
��B
�B
ȴB
��B
�VB
y�B
cTB
A�B
#�B
.B
�B	��B	�;B	�XB	��B	��B	�7B	v�B	hsB	[#B	E�B	>wB	49B	(�B	�B	B�mB��B��B�5B��B��B��BŢBÖBĜB�}B��B��B�^B�FB�B��B��B��B��B�{B��B�\B�JB�1B�%B�%B�B�B�B�B�B}�B�B~�Bw�Bn�BdZB_;B^5BW
BXBZB]/B`BB^5BZBYBYBYBZB]/B\)BaHBiyBp�BjBq�Bz�B{�Bz�Bs�Bn�Bn�Bt�Bz�B{�Bk�BjBp�Bt�Bl�BjBcTBR�BL�BL�BM�BP�BO�BN�BN�BK�BI�BE�BD�BD�BG�BK�BO�BVBXBYB]/B^5B\)B]/BYBS�BQ�BP�BP�BP�BR�BS�BVBW
BXBYB[#BYBXBN�BL�BR�BW
B]/BcTBbNBaHB`BBYBVBYBYBW
BXB\)B_;B^5B_;BcTBcTBcTBcTBk�Bn�Bn�Bt�Br�Br�Bp�Bm�Bm�Bm�Br�Bx�B~�B�+B�1B�+B�B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�!B�'B�!B�'B�'B�'B�FB�^B�}B�}B��B��B��B��BÖBƨB��B��B��B��B�#B�/B�;B�HB�5B�/B�BB�ZB�mB�mB�sB�sB�B�yB�B�B��B��B��B��B��B	  B	  B	  B��B��B��B��B��B	B	%B	
=B	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	'�B	+B	/B	;dB	:^B	:^B	>wB	C�B	E�B	H�B	I�B	L�B	P�B	VB	XB	ZB	]/B	bNB	cTB	cTB	cTB	e`B	e`B	ffB	ffB	gmB	l�B	r�B	x�B	{�B	�B	�%B	�%B	�+B	�7B	�=B	�DB	�JB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�9B	�FB	�LB	�XB	�dB	�jB	�jB	�jB	�qB	�qB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�5B	�;B	�BB	�BB	�NB	�NB	�NB	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�B	��B
�B
�B
�B
$�B
.B
7�B
;�B
?�B
C�B
HKB
L�B
T,B
Z�B
_;B
d�B
iDB
n�B
sB
v�B
|611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�VB
�VB
�VB
�\B
�\B
�VB
�VB
�VB
�\B
�\B
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�cB
�cB
�cB
�\B
�B
�B
��B
� BIB�B	nB
�B
�%B>BDB�B�B�B�B
�B
�CB
��B
��B
�B
�>B
�>B
��B
��B�B>�BV:B]eBi�B��B��B�UB��B��B��B��B�B�B�1B�bB�\B�B��B��B��B�B��B�_B��B��B��B��B�#B��B�B��BtBD�B�B�TB�iB{B��Bg�BU<BR)BVBBk�Bh�B\gBU=BPBMB=�B1hB&&B#B@�BT7BNBQ%BR+B<�B.XB$B�BNB
�B
��B
� B
��B
�	B
�fB
n�B
XhB
6�B
�B
#.B
�B	�B	�]B	�}B	�B	��B	~aB	k�B	]�B	PRB	:�B	3�B	)lB	*B	�B�>BܨB�)B�5B�qB�)B�B�B��B��B��B��B��B��B��B��B�]B�&B��B��B��B��B��B��B��B}xB{lB{lByaBzgBzgByaByaBs=BwUBtCBmBc�BY�BT�BS�BLXBM^BOkBR|BU�BS�BOkBNfBNfBNfBOlBR~BQxBV�B^�Be�B_�Bf�Bp.Bq4Bp.BiBc�Bc�Bj
Bp.Bq4B`�B_�Be�Bj
Ba�B_�BX�BHDBB BB BC&BF8BE2BD,BD,BAB?B:�B9�B9�B=BABE3BKWBMcBNjBR�BS�BQ|BR�BNkBILBGABF:BF:BF:BHGBIMBKYBL_BMeBNlBPxBNlBMeBD/BB$BHHBL`BR�BX�BW�BV�BU�BNmBKZBNmBNmBLaBMgBQBT�BS�BT�BX�BX�BX�BX�B`�Bc�Bc�BjBhBhBe�Bb�Bb�Bb�BhBn*BtOB|B}�B|BxgB��B��B��B��B��B��B�B�B�B�B�/B�NB�BB�6B�CB�TB�sB�yB�sB�yB�sB�yB�yB�yB��B��B��B��B��B��B��B��B��B��B�B�B�#B�AB�rB�~BԊB֖BӄB�~BՑB٨BܻBܻB��B��B��B��B��B��B�	B�:B�4B�.B�;B�LB�LB�LB�;B�.B�(B�(B�AB�^B�qB��B	�B		�B	
�B	
�B	
�B	
�B	�B	�B	�B	�B	�B	�B	B	B	B	:B	 LB	$dB	0�B	/�B	/�B	3�B	8�B	:�B	=�B	?B	BB	F,B	KJB	MVB	OcB	RuB	W�B	X�B	X�B	X�B	Z�B	Z�B	[�B	[�B	\�B	a�B	g�B	nB	q+B	y\B	{hB	{hB	|nB	~yB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�<B	�BB	�NB	�NB	�TB	�TB	�[B	�gB	�mB	�mB	�rB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�)B	�)B	�)B	�5B	�AB	�AB	�AB	�MB	�TB	�ZB	�`B	�`B	�fB	�fB	�qB	�wB	�~B	�~B	׊B	׊B	׊B	ٖB	ڜB	ڜB	ܩB	ݯB	ݯB	ݯG�O�B	�-B	��B
B
 B
/B
#LB
,�B
1B
5B
8�B
=�B
A�B
IcB
O�B
TqB
Y�B
^zB
c�B
hOB
k�B
qk11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170920    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170920  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170920  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                