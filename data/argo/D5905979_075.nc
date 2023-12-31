CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:11Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170911  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               KA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ػ���`1   @ػ܎8�@7������c���"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    KA   B   B   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�=D�i�D���D�ФD�
D�T�D���D���D�  D�b=D��qD���D�)HD�V�Dڗ�D��3D�'�D�Z�D�RD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�fg@���@���AffA>ffA^ffA|��A�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC��C�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC$  C&  C'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��D� D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D   D � D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0�4D1s4D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DE  DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO�4DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De�4Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Dy�RD�
D�fgD��RD��qD��D�Q�D��{D���D��D�_
D��>D�ǮD�&D�S�Dڔ{D�� D�${D�W\D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�p�A�p�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�r�A�l�A�hsA�ffA�VA�I�A�oA���A��A��
A�A�A��yA�
=A�\)A�z�A�jA�\)A�  A���A�ĜA���A�G�A��DA���A��^A�ƨA�ȴA�;dA�A�7LA�A���A���A�v�A�jA�\)A�S�A�O�A�M�A� �A��hA�hsA�S�A�A�A�bA���A���A�Q�A�JA���A��uA��A���A�t�A�A�A��A�A��TA��
A��!A�t�A�+A��jA� �A�E�A��A��A�ZA��TA���A���A��A�bNA��/A��A���A��7A��A���A�ffA���A�jA� �A�
=A��A�C�A�-A�"�A�oA��FA��\A�ZA��mA��mA�G�A��-A���A��mA��/A��DA�A���A��mA��7A�S�A�I�A�dZA�%A~n�A}p�A|ĜA|v�A{��A{x�Ax�Au�Ap�An�HAl�yAk/Aj�Ag`BAf��Af�Ad  Ab��Aa��A`��A`A^v�A[t�AZbAY33AW�AWdZAV��AUG�ATn�ASt�AR �AQt�AQ?}AP��APjAO&�AL�AIdZAGO�AFbAD��AB�\AA��AA;dA?�hA>�9A=l�A:jA77LA4 �A3`BA2�A2~�A2 �A1�-A1dZA1�A0��A/�FA-��A,A�A+O�A*1A(�A'7LA&�9A&~�A&Q�A%S�A#��A#�^A#x�A#33A"��A!�hA�mA`BAn�A�A/Ar�A��A�/A9XA��AffA�;A��A�wA�hA�HA�;AA�A�HA��A�A�^A �A
I�A
  A	A	t�A�A��A~�A^5A{A�A33A��A  AK�A~�A�-AbNA�A �A �DA 1'@�\)@�X@�;d@�J@��@��@���@�%@�  @�p�@��y@��`@�l�@홚@�\)@��@�-@�x�@�?}@�z�@�9X@��H@�z�@��@�hs@��;@�ȴ@�$�@�1'@۶F@�1'@�(�@۶F@�t�@��H@ج@�o@�$�@�Q�@��y@�@�@�x�@���@�z�@ϝ�@�ȴ@�V@��@��#@͉7@���@�Q�@ˮ@�33@��@�ff@�=q@��T@ɑh@�%@ȼj@ȃ@�bN@��@���@�  @Ȭ@��@��/@� �@ǍP@�+@ƸR@�V@Ĭ@�bN@�Q�@�9X@Ý�@���@��@�p�@��@�l�@���@���@�z�@�1'@�|�@��H@���@�-@�p�@��`@�Q�@���@�dZ@���@�E�@���@�%@�9X@�K�@�@��@��+@��@�/@��@�A�@� �@���@��
@�C�@�v�@���@�?}@��@���@�I�@��@�A�@��@���@��u@��@�Z@��@�ȴ@�@��/@���@�z�@�(�@��@���@�n�@��@��@��-@�hs@���@� �@��
@���@�33@�
=@��y@��!@�v�@�M�@���@��#@��@��/@��@�A�@�  @��
@���@�\)@���@��y@�;d@�33@�@��y@���@��\@�v�@�=q@��#@�V@���@��@��D@��@�Z@�Q�@�I�@�A�@�b@��m@���@�K�@�
=@���@�-@���@���@�@��^@��^@�x�@�O�@�7L@�V@��/@�A�@�ƨ@�t�@���@��!@�n�@�M�@�5?@��@�-@�$�@�$�@��@�x�@��@��/@�A�@��;@��@��@�33@��H@�ȴ@���@�v�@�=q@�@���@�?}@��@��@���@�Z@���@��@��;@��F@�t�@�o@��y@��y@��y@��H@��H@���@���@�v�@�E�@��@�@�O�@�&�@�:�@|֡@so@jJ�@a��@[6z@Sn/@Ll"@Dm�@<�5@8�@2;�@,�`@&�@#��@)�@|�@ȴ@Y�@	�Z@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�r�A�p�A�p�A�n�A�n�A�n�A�n�A�p�A�p�A�p�A�r�A�l�A�hsA�ffA�VA�I�A�oA���A��A��
A�A�A��yA�
=A�\)A�z�A�jA�\)A�  A���A�ĜA���A�G�A��DA���A��^A�ƨA�ȴA�;dA�A�7LA�A���A���A�v�A�jA�\)A�S�A�O�A�M�A� �A��hA�hsA�S�A�A�A�bA���A���A�Q�A�JA���A��uA��A���A�t�A�A�A��A�A��TA��
A��!A�t�A�+A��jA� �A�E�A��A��A�ZA��TA���A���A��A�bNA��/A��A���A��7A��A���A�ffA���A�jA� �A�
=A��A�C�A�-A�"�A�oA��FA��\A�ZA��mA��mA�G�A��-A���A��mA��/A��DA�A���A��mA��7A�S�A�I�A�dZA�%A~n�A}p�A|ĜA|v�A{��A{x�Ax�Au�Ap�An�HAl�yAk/Aj�Ag`BAf��Af�Ad  Ab��Aa��A`��A`A^v�A[t�AZbAY33AW�AWdZAV��AUG�ATn�ASt�AR �AQt�AQ?}AP��APjAO&�AL�AIdZAGO�AFbAD��AB�\AA��AA;dA?�hA>�9A=l�A:jA77LA4 �A3`BA2�A2~�A2 �A1�-A1dZA1�A0��A/�FA-��A,A�A+O�A*1A(�A'7LA&�9A&~�A&Q�A%S�A#��A#�^A#x�A#33A"��A!�hA�mA`BAn�A�A/Ar�A��A�/A9XA��AffA�;A��A�wA�hA�HA�;AA�A�HA��A�A�^A �A
I�A
  A	A	t�A�A��A~�A^5A{A�A33A��A  AK�A~�A�-AbNA�A �A �DA 1'@�\)@�X@�;d@�J@��@��@���@�%@�  @�p�@��y@��`@�l�@홚@�\)@��@�-@�x�@�?}@�z�@�9X@��H@�z�@��@�hs@��;@�ȴ@�$�@�1'@۶F@�1'@�(�@۶F@�t�@��H@ج@�o@�$�@�Q�@��y@�@�@�x�@���@�z�@ϝ�@�ȴ@�V@��@��#@͉7@���@�Q�@ˮ@�33@��@�ff@�=q@��T@ɑh@�%@ȼj@ȃ@�bN@��@���@�  @Ȭ@��@��/@� �@ǍP@�+@ƸR@�V@Ĭ@�bN@�Q�@�9X@Ý�@���@��@�p�@��@�l�@���@���@�z�@�1'@�|�@��H@���@�-@�p�@��`@�Q�@���@�dZ@���@�E�@���@�%@�9X@�K�@�@��@��+@��@�/@��@�A�@� �@���@��
@�C�@�v�@���@�?}@��@���@�I�@��@�A�@��@���@��u@��@�Z@��@�ȴ@�@��/@���@�z�@�(�@��@���@�n�@��@��@��-@�hs@���@� �@��
@���@�33@�
=@��y@��!@�v�@�M�@���@��#@��@��/@��@�A�@�  @��
@���@�\)@���@��y@�;d@�33@�@��y@���@��\@�v�@�=q@��#@�V@���@��@��D@��@�Z@�Q�@�I�@�A�@�b@��m@���@�K�@�
=@���@�-@���@���@�@��^@��^@�x�@�O�@�7L@�V@��/@�A�@�ƨ@�t�@���@��!@�n�@�M�@�5?@��@�-@�$�@�$�@��@�x�@��@��/@�A�@��;@��@��@�33@��H@�ȴ@���@�v�@�=q@�@���@�?}@��@��@���@�Z@���@��@��;@��F@�t�@�o@��y@��y@��y@��H@��H@���@���@�v�@�E�@��@�@�O�G�O�@�:�@|֡@so@jJ�@a��@[6z@Sn/@Ll"@Dm�@<�5@8�@2;�@,�`@&�@#��@)�@|�@ȴ@Y�@	�Z@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�NB�NB�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�ZB�ZB�`B�fB�B�B�yB�sB�B��B+B
=B��B��B��B��B��BBBJB�B!�B!�B'�BK�B^5BhsBt�Bw�Bz�B~�B�B�B�B�B�B�B�+B�\B�hB�oB�oB��B��B��B��B��B��B��B�B�LB�RB�qB�qB�qB��BBÖBŢBĜBÖB��B�qB�XB�LB�?B�B��B�\B�7B�Bx�B]/BF�B>wB9XB(�B{BB��B�B�B�B�B�5BÖB�!B��B��B��B�VB~�BhsBS�B@�B0!B"�BPB
�B
�
B
�?B
�9B
�B
��B
��B
�1B
{�B
t�B
o�B
l�B
hsB
bNB
T�B
7LB
oB
B
B	��B	��B	�sB	�HB	�5B	��B	ɺB	ŢB	��B	�^B	�!B	��B	��B	�{B	�PB	�7B	�%B	|�B	v�B	q�B	k�B	dZB	bNB	_;B	ZB	P�B	;dB	#�B	�B		7B	B��B�B��B�B�sB�NB�#B��BŢB�}B�qB�dB�^B�RB�RB�?B�9B�'B�B��B��B��B��B��B�uB�oB�hB�bB�DB�=B�7B�+B�%B�Bz�Bu�Br�Bp�Bo�Bn�Bl�Bk�BhsBffBaHB]/B\)B\)B\)BZBYBVBR�BP�BM�BK�BO�BN�BM�BM�BM�BM�BL�BM�BL�BM�BO�BQ�BVBW
BW
BT�BT�BN�BM�BG�BE�BF�BH�BK�BM�BJ�BK�BW
B`BB`BB`BB`BB]/BZBZBYBZB\)BZB\)B`BBdZBcTBdZBdZBcTBdZBe`BgmBjBjBjBo�Bq�Br�Bq�Bq�Bo�BjBjBl�Bn�Bp�Bp�Bp�Bp�Bp�Bq�Br�Br�Bs�Bt�Bw�B{�B� B�B�B�B�%B�%B�+B�DB�JB�\B�bB�{B��B��B��B��B��B�B�B�'B�9B�?B�FB�qB�}B��BBƨBȴBȴBɺB��B��B�B�5B�;B�BB�ZB�B�B�B��B��B��B��B	B	B		7B	DB	\B	oB	{B	uB		7B	B	%B	%B	1B	
=B	DB	bB	hB	�B	�B	�B	�B	�B	 �B	 �B	"�B	(�B	/B	5?B	:^B	?}B	A�B	E�B	G�B	I�B	I�B	H�B	H�B	H�B	I�B	I�B	J�B	J�B	K�B	K�B	K�B	O�B	S�B	W
B	YB	[#B	[#B	\)B	]/B	^5B	_;B	aHB	bNB	dZB	ffB	gmB	gmB	gmB	hsB	iyB	k�B	p�B	t�B	y�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�7B	�\B	�bB	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�?B	�FB	�FB	�RB	�XB	�^B	�^B	�qB	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	��B	��B
�B
TB
_B
"�B
,B
4nB
<jB
D�B
I�B
N<B
T,B
X�B
\�B
bNB
h>B
mCB
r�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BږBږBۜBۜBۜBۜBۜBۜBۜBۜBۜBܢBܢBܢBݨBޮB��B��B��B�B��B�5B�qB�B�5B�"B�B�B�5B�SB�`B�B�BBB 5BDBVxB`�Bl�BpBs"Bw;ByGByGBzMB{SB{SB{SBlB��B��B��B��B��B��B��B��B�B�B�5B�ZB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B�BB��B��B�yBzOBqBUtB>�B6�B1�B!?B�B�^B�-B�B��B��B��BփB��B�rB�HB�5B�#B��BwOB`�BLPB8�B({B,B�B
�B
�kB
��B
��B
�fB
�B
��B
��B
tNB
m$B
hB
d�B
`�B
Z�B
MhB
/�B

�B	�|B	��B	�LB	�4B	��B	ٺB	֨B	�qB	�.B	�B	��B	��B	��B	�HB	�B	��B	��B	��B	~�B	uhB	oCB	j%B	d B	\�B	Z�B	W�B	R�B	IbB	3�B	WB	B	�B��B�GB�4B�XB�B��B��BөB�ZB�*B�B��B��B��B��B��B��B��B��B��B�mB�VB�=B�&B�B�B��B��B��B��B��B��B�B~�By�BsoBnRBk?Bi3Bh-Bg(BeBdBaB^�BY�BU�BT�BT�BT�BR�BQ�BN�BK�BIxBFfBDZBHrBGlBFfBFfBFfBFfBE`BFfBEaBFfBHrBJBN�BO�BO�BM�BM�BGmBFgB@CB>7B?=BAIBD\BFhBCVBD\BO�BX�BX�BX�BX�BU�BR�BR�BQ�BR�BT�BR�BT�BX�B\�B[�B\�B\�B[�B\�B]�B`BcBcBcBh3Bj?BkDBj?Bj?Bh3BcBcBe Bg-Bi9Bi9Bi9Bi9Bi9Bj?BkEBkEBlKBmQBpdBt|Bx�Bz�B|�B}�B~�B~�B�B��B��B��B��B�B�!B�!B�-B�XB��B��B��B��B��B��B��B�B�B�B� B�9B�EB�EB�KB�dB�}BСB��B��B��B��B�B�'B�9B�RB�dB�vB��B��B��B	�B	�B	�B	
�B		B	B	�B��B��B��B	 �B	�B	�B	�B		�B	B	B	(B	4B	@B	SB	SB	_B	!�B	'�B	-�B	2�B	8	B	:B	>.B	@:B	BFB	BFB	A@B	A@B	A@B	BFB	BFB	CMB	CMB	DSB	DSB	DSB	HkB	L�B	O�B	Q�B	S�B	S�B	T�B	U�B	V�B	W�B	Y�B	Z�B	\�B	^�B	_�B	_�B	_�B	`�B	bB	dB	i.B	mFB	reB	uxB	v~B	w�B	x�B	z�B	{�B	|�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�(B	�.B	�:B	�FB	�FB	�FB	�FB	�MB	�YB	�qB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�"B	�(B	�.B	�@B	�MB	�YB	�YB	�_B	�_B	�eB	�rB	�xB	�xB	�xB	�~B	̈́B	ϐB	ЕB	ЕB	ЕB	ЕB	ЕB	ЕB	ќB	ќB	ќB	ќB	ӨB	ӨG�O�B	�pB	�B	�ZB

�B
�B
B
$�B
,�B
4�B
=lB
B"B
F�B
L�B
Q/B
UB
Z�B
`�B
e�B
kJB
pB
tM111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170911    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170911  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170911  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                