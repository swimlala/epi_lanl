CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:42Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               BA   AO  20111205113507  20190522121836  1901_5055_066                   2C  D   APEX                            2140                            040306                          846 @��;f�?�1   @��<��@+e`A�7L�c�\(�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�  B�  B�33B�  B�  B�33B�  B���B�  B�  B�  B���B�  B�  B�33B�  B���C�fC�fC�fC  C
�C  C  C  C  C�C  C  C�C  C  C �C"  C#�fC&  C(  C*  C+�fC-�fC0  C2�C4  C5�fC8  C:�C<  C=�fC@  CB  CD  CF�CH  CJ  CL  CN  CP  CQ�fCS�fCU�fCX  CZ  C\�C^  C`  Cb  Cc�fCf  Ch  Cj  Ck�fCm�fCo�fCq�fCt�Cv�Cx  Cz  C|  C~�C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��3C��3C��C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C��C�  C�  C��C��C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C��3C�  C�  C��C��C�  C�  C��C��3C�  C�  C��3C�  C��C�  C�  C��C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C��C��3C�  C�  C��3C�  C�  C��3C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��3D   D �fD  Dy�D  D� D  D� DfD� D��D� DfD� D��Dy�D  Dy�D	  D	�fD
  D
� D
��Dy�D  D�fDfD� D  Dy�D  D�fD  D� D��D� DfD�fD  Dy�D��D� DfD�fD  D� D  Dy�D��D� DfD�fD  D� D��Dy�D  D�fDfD�fDfD�fD  D� D   D � D ��D!y�D!��D"y�D"��D#� D$�D$��D%fD%�fD&fD&�fD'  D'y�D'��D(y�D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5  D5s3D5��D6� D7  D7�fD8  D8y�D8��D9� D:  D:�fD;fD;�fD<fD<�fD=  D=y�D>  D>�fD?fD?� D?��D@� DA  DA� DB  DB�fDCfDC� DC��DD� DE  DE�fDFfDF� DF��DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DL��DMy�DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DY��DZy�D[  D[�fD\fD\�fD]  D]y�D^  D^�fD_fD_� D`  D`�fDa  Day�Da��Db� DcfDc� Dd  Ddy�Dd��De� De��Dfy�Dg  Dg�fDhfDh� Di  Diy�Di��Djy�Dj��Dky�Dl  Dl�fDmfDm�fDnfDn�fDo  Do� Do��Dpy�Dq  Dq� Dr  Dr�fDs  Ds� DtfD���D��fD�� D�ٚD�l�D��3D� D�C3D�L�D���D��3D�C3D�Y�DӼ�D�3D�i�D�fD�	�D�\�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@fff@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.��B6ffB>ffBFffBNffBVffB^ffBf��BnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�ffB�ffB�33B�33B�ffB�33B�33B�ffB�33B�  B�33B�33B�33B�  B�33B�33B�ffB�33B�  C� C� C� C��C	�3C��C��C��C��C�3C��C��C�3C��C��C�3C!��C#� C%��C'��C)��C+� C-� C/��C1�3C3��C5� C7��C9�3C;��C=� C?��CA��CC��CE�3CG��CI��CK��CM��CO��CQ� CS� CU� CW��CY��C[�3C]��C_��Ca��Cc� Ce��Cg��Ci��Ck� Cm� Co� Cq� Cs�3Cu�3Cw��Cy��C{��C}�3C�3C���C���C���C���C���C���C�ٚC���C���C�ٚC���C���C�� C�� C�ٚC���C���C�ٚC���C�� C���C���C���C�� C���C���C���C�� C�� C���C�ٚC���C���C�ٚC�ٚC���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C�ٚC�ٚC�� C�� C�� C���C���C���C���C���C���C���C���C���C�ٚC�� C���C���C�� C���C���C�ٚC�ٚC���C���C�ٚC�� C���C���C�� C���C�ٚC���C���C�ٚC�� C���C���C�� C���C���C�� C�� C���C���C���C�� C�� C���C���C�ٚC���C���C�ٚC�� C���C���C�� C���C���C�� C���C���C�� C���C�ٚC���C�� C���C���C���C���C�ٚC���C�� C���C�ٚC���C���C�� C���D l�D �fD` D�fDffD�fDffD��DffD� DffD��DffD� D` D�fD` D�fD	l�D	�fD
ffD
� D` D�fDl�D��DffD�fD` D�fDl�D�fDffD� DffD��Dl�D�fD` D� DffD��Dl�D�fDffD�fD` D� DffD��Dl�D�fDffD� D` D�fDl�D��Dl�D��Dl�D�fDffD�fD ffD � D!` D!� D"` D"� D#ffD#�3D$s3D$��D%l�D%��D&l�D&�fD'` D'� D(` D(� D)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4�fD5Y�D5� D6ffD6�fD7l�D7�fD8` D8� D9ffD9�fD:l�D:��D;l�D;��D<l�D<�fD=` D=�fD>l�D>��D?ffD?� D@ffD@�fDAffDA�fDBl�DB��DCffDC� DDffDD�fDEl�DE��DFffDF� DG` DG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDL` DL� DM` DM� DNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDW` DW�fDXffDX�fDYffDY� DZ` DZ�fD[l�D[��D\l�D\�fD]` D]�fD^l�D^��D_ffD_�fD`l�D`�fDa` Da� DbffDb��DcffDc�fDd` Dd� DeffDe� Df` Df�fDgl�Dg��DhffDh�fDi` Di� Dj` Dj� Dk` Dk�fDll�Dl��Dml�Dm��Dnl�Dn�fDoffDo� Dp` Dp�fDqffDq�fDrl�Dr�fDsffDs��D���D���D��3D���D�` D��fD�3D�6fD�@ D�� D��fD�6fD�L�DӰ D��fD�\�D扚D���D�P D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A��A�ĜAǶFAǟ�AǇ+A�n�A�jA�dZA�`BA�\)A�\)A�XA�S�A�Q�A�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�K�A�G�A�A�A�5?A� �A�1A��TA�A�{A�S�A��/A�A��TA�ZA��A���A���A�oA��uA���A��A��+A�hsA���A��wA�+A�l�A��hA��A��DA�33A���A��yA��A�E�A��^A�bNA�33A�M�A�ȴA�^5A��A��A�33A�jA��yA��A�Q�A��+A��A�|�A���A�%A��PA��TA��A��uA��A��/A�C�A���A��DA��
A}p�Ax��At��AtAsp�ArZAn��Af�Aa�;A`Q�A^VA]K�AZ��AW�mAV-AR�\AN��ALr�AH�`AE�FA@$�A:$�A9A8I�A7G�A5��A4v�A2�jA1�TA0ZA/C�A-&�A+%A)|�A(ȴA'&�A&�\A&�A%�#A%`BA$�A$��A$�HA$�A#�7A!�mA!oA!�7A!�PA"ZA"�A#�A#�#A#x�A#��A#S�A#�A#+A#�A"��A"n�A"1A!�7A ȴA ffA 9XA��AC�A��A�A �A�mAx�AS�A+A�9AZA-AAx�AXA�yAA�A�wA�7A+A�A�AJA�PA��AE�A�TA��At�AC�A"�AVAA�9A33Ar�A�-AhsAhsA`BA+A��A�-A
=Av�A$�A�
A��Ap�A?}A��Av�A(�A\)A
��A
1A	�FA	|�A�yA�uA1'A�TA�AhsA33A��A1'A�wA�Az�AA�A{A�AA��At�A7LA��A��A�A�wAoA �\A 9X@��P@�n�@��@���@��u@�(�@��@�+@�~�@���@��7@���@��@��P@��y@�~�@��@�x�@��m@��@��#@��@�%@�9X@�ƨ@�33@�ȴ@�!@�!@�!@��@�?}@�P@�"�@�$�@��@�Q�@��@�C�@���@�J@���@�@��@�ff@�X@�(�@ޗ�@ݙ�@���@��m@�dZ@�
=@ڗ�@�E�@�-@�J@��@�@�?}@���@�I�@�l�@��@�~�@��#@�7L@�bN@��;@�@��@щ7@��@Ѓ@� �@Ͼw@ϕ�@�C�@�@���@�ȴ@Ώ\@��T@�x�@�?}@��/@�bN@��
@��@���@ʰ!@�ff@�@��@Ȭ@�Z@�  @Ǯ@��H@��T@�p�@ļj@�+@°!@�@+@��@���@�X@�V@���@��9@�j@���@�o@���@�v�@��#@��@�(�@�ƨ@���@���@�l�@�
=@���@��@��@���@��9@�r�@�ƨ@�dZ@�S�@�C�@��@��y@���@��+@��@���@��7@��/@�A�@��F@��@���@�n�@�E�@��#@�hs@�`B@�%@���@��9@�A�@� �@���@�ƨ@��F@��P@�@���@�n�@��T@�x�@��@��m@��@��!@�E�@��#@�X@�G�@�V@��@�Q�@�b@���@�\)@�"�@�v�@�{@��^@�7L@��@��`@��`@���@�z�@� �@��
@�;d@�@���@�$�@�@�/@���@���@��9@���@���@�+@��!@��+@�E�@�@��-@�X@���@��@�Q�@�A�@��@���@�;d@���@�v�@�5?@�J@��7@�?}@��@���@�A�@��F@�\)@�"�@��y@��\@�-@��T@�`B@�V@���@��j@�j@��;@���@�l�@�v�@�{@��@��-@�hs@��@��`@���@�X@�n�@~��@s"�@g�@^��@W|�@Lz�@D�@=V@4�j@-��@&�@!&�@I�@v�@C�@;d@ƨ@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�A��A�ĜAǶFAǟ�AǇ+A�n�A�jA�dZA�`BA�\)A�\)A�XA�S�A�Q�A�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�K�A�G�A�A�A�5?A� �A�1A��TA�A�{A�S�A��/A�A��TA�ZA��A���A���A�oA��uA���A��A��+A�hsA���A��wA�+A�l�A��hA��A��DA�33A���A��yA��A�E�A��^A�bNA�33A�M�A�ȴA�^5A��A��A�33A�jA��yA��A�Q�A��+A��A�|�A���A�%A��PA��TA��A��uA��A��/A�C�A���A��DA��
A}p�Ax��At��AtAsp�ArZAn��Af�Aa�;A`Q�A^VA]K�AZ��AW�mAV-AR�\AN��ALr�AH�`AE�FA@$�A:$�A9A8I�A7G�A5��A4v�A2�jA1�TA0ZA/C�A-&�A+%A)|�A(ȴA'&�A&�\A&�A%�#A%`BA$�A$��A$�HA$�A#�7A!�mA!oA!�7A!�PA"ZA"�A#�A#�#A#x�A#��A#S�A#�A#+A#�A"��A"n�A"1A!�7A ȴA ffA 9XA��AC�A��A�A �A�mAx�AS�A+A�9AZA-AAx�AXA�yAA�A�wA�7A+A�A�AJA�PA��AE�A�TA��At�AC�A"�AVAA�9A33Ar�A�-AhsAhsA`BA+A��A�-A
=Av�A$�A�
A��Ap�A?}A��Av�A(�A\)A
��A
1A	�FA	|�A�yA�uA1'A�TA�AhsA33A��A1'A�wA�Az�AA�A{A�AA��At�A7LA��A��A�A�wAoA �\A 9X@��P@�n�@��@���@��u@�(�@��@�+@�~�@���@��7@���@��@��P@��y@�~�@��@�x�@��m@��@��#@��@�%@�9X@�ƨ@�33@�ȴ@�!@�!@�!@��@�?}@�P@�"�@�$�@��@�Q�@��@�C�@���@�J@���@�@��@�ff@�X@�(�@ޗ�@ݙ�@���@��m@�dZ@�
=@ڗ�@�E�@�-@�J@��@�@�?}@���@�I�@�l�@��@�~�@��#@�7L@�bN@��;@�@��@щ7@��@Ѓ@� �@Ͼw@ϕ�@�C�@�@���@�ȴ@Ώ\@��T@�x�@�?}@��/@�bN@��
@��@���@ʰ!@�ff@�@��@Ȭ@�Z@�  @Ǯ@��H@��T@�p�@ļj@�+@°!@�@+@��@���@�X@�V@���@��9@�j@���@�o@���@�v�@��#@��@�(�@�ƨ@���@���@�l�@�
=@���@��@��@���@��9@�r�@�ƨ@�dZ@�S�@�C�@��@��y@���@��+@��@���@��7@��/@�A�@��F@��@���@�n�@�E�@��#@�hs@�`B@�%@���@��9@�A�@� �@���@�ƨ@��F@��P@�@���@�n�@��T@�x�@��@��m@��@��!@�E�@��#@�X@�G�@�V@��@�Q�@�b@���@�\)@�"�@�v�@�{@��^@�7L@��@��`@��`@���@�z�@� �@��
@�;d@�@���@�$�@�@�/@���@���@��9@���@���@�+@��!@��+@�E�@�@��-@�X@���@��@�Q�@�A�@��@���@�;d@���@�v�@�5?@�J@��7@�?}@��@���@�A�@��F@�\)@�"�@��y@��\@�-@��T@�`B@�V@���@��j@�j@��;@���@�l�@�v�@�{@��@��-@�hs@��@��`@���@�X@�n�@~��@s"�@g�@^��@W|�@Lz�@D�@=V@4�j@-��@&�@!&�@I�@v�@C�@;d@ƨ@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
B
+B

=B
\B
�B
�B
!�B
$�B
J�B
q�B
��B �B�\B��B��B�-BB��B��B��B��B�BB�#B�B��B��B�mB�yB�B�B�B�B�B�B�B��B�B��B��B��B�B�ZB�5B�B��B��B��B�RB�B��B�VBy�B`BBH�B+B  B
�TB
ɺB
�3B
��B
�1B
cTB
%�B	�B	��B	�-B	��B	��B	�\B	s�B	N�B	6FB	)�B	 �B	�B	+B��B�B�)B��B��B�?B��B��B�!B�-B�3B�3B�^B�}BĜBŢB��BȴB��B��B��B�BB�ZB�ZB�sB�B��B	hB	�B	/B	33B	>wB	>wB	;dB	N�B	p�B	�{B	��B	��B	��B	��B	�/B	�HB	�mB	�B	�B	�B	��B
B
B
PB
VB
VB
bB
�B
�B
�B
{B
oB
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
!�B
#�B
%�B
$�B
#�B
%�B
&�B
.B
.B
1'B
49B
7LB
7LB
6FB
6FB
5?B
5?B
33B
2-B
6FB
5?B
5?B
6FB
5?B
49B
33B
1'B
.B
-B
.B
-B
,B
,B
+B
+B
)�B
+B
+B
,B
,B
-B
-B
,B
,B
+B
+B
+B
+B
(�B
'�B
&�B
%�B
$�B
%�B
%�B
$�B
$�B
#�B
#�B
"�B
!�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
oB
hB
bB
\B
\B
VB
PB
JB
JB
DB

=B
	7B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
%B
B
%B
%B
%B
+B
+B
1B
+B
+B
%B
+B
+B
+B
+B
%B
B
B
B
  B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
	7B

=B

=B
	7B
	7B

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
JB
DB

=B
JB
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
bB
bB
bB
hB
hB
oB
oB
oB
oB
uB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
#�B
'�B
2-B
49B
;dB
D�B
I�B
N�B
T�B
ZB
^5B
e`B
iyB
p�B
u�B
x�B
|�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
B
+B
DB
bB
�B
�B
#�B
+B
Q�B
}�B
��B)�B�{B��B��B�?BǮBǮB�B�BB�`B�B�mB�B�sB��B��B��B��B��B��B��B��B��B��B  BBBPBbB%B�B�B�B�/B�5BɺBÖB�jB�B��B�\Br�BaHBI�BoB
�B
�B
��B
�qB
��B
�PB
K�B
bB	�NB	�LB	�'B	�!B	�B	��B	jB	A�B	8RB	-B	(�B	�B	
=B	+B��B�HB�/B��B��B��B�XB�^B�wB�}BŢB��B��B��B�B�#B��B�B�/B�B�yB�sB�B�B	B	oB	�B	49B	=qB	H�B	A�B	8RB	L�B	jB	�\B	��B	B	��B	��B	�;B	�NB	�mB	�B	�B	��B	��B
%B
	7B
bB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
$�B
'�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
33B
33B
6FB
7LB
9XB
9XB
8RB
7LB
7LB
8RB
9XB
<jB
<jB
:^B
7LB
7LB
7LB
7LB
9XB
8RB
49B
2-B
1'B
0!B
.B
.B
.B
/B
.B
0!B
2-B
2-B
1'B
0!B
0!B
1'B
/B
.B
.B
-B
.B
,B
-B
+B
+B
)�B
+B
'�B
&�B
&�B
%�B
%�B
$�B
$�B
#�B
$�B
#�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
uB
oB
hB
oB
bB
VB
\B
VB
PB
PB
1B
JB
%B
1B
1B
+B
%B
B
%B
B
B
B
%B
B
  B
B	��B
B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B	��B
B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
%B
+B
+B
B
%B
+B
%B
+B
1B
+B
+B
	7B
+B
+B
	7B
	7B
	7B

=B

=B

=B
1B
%B
%B
  B
B
B
B
%B
%B
+B
1B
1B
1B
1B

=B
DB
JB
JB

=B
	7B
DB
DB
JB
JB
JB
	7B
DB
DB
JB
JB

=B
DB
JB
JB
DB

=B
\B
PB
\B
VB
VB
bB
hB
\B
oB
bB
bB
hB
bB
uB
uB
hB
oB
{B
oB
oB
{B
oB
oB
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
'�B
2-B
5?B
;dB
D�B
I�B
N�B
T�B
ZB
_;B
e`B
jB
q�B
u�B
x�B
|�B
� B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<��
<T��<D��<ě�<���=8Q�<��
<e`B<T��<u<e`B<49X<#�
<#�
<#�
<#�
<u<T��<�t�<�t�<�t�<T��<�t�<�t�<#�
<�t�<#�
<49X<T��<e`B<�1<�1<�t�<�j<�<�t�<�o<u<T��<ě�<�=#�
=�P<�h<��
<#�
<#�
<�o<�=8Q�<���<49X<e`B<49X<�t�<��
<�t�<�/<�/<��
<�`B<�=�w=+<#�
<#�
<#�
<D��<49X<T��<#�
<T��<D��<�C�<u<49X<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250302012011312503020120113125030  AO  ARGQ                                                                        20111205113507  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113507  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125030  IP                  G�O�G�O�G�O�                