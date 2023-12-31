CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:29Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112713  20190522121836  1901_5055_022                   2C  D   APEX                            2140                            040306                          846 @�`	�<O�1   @�`
F)��@0Hr� Ĝ�c;-1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@y��@���@���A   A@  A`  A�  A�  A�  A���A�  A���A�  A�  B   B  B  BffB   B'��B/��B8  B@  BH  BP  BX  B`  BhffBp  Bw��B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B���B�  B���B���B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�33B�33B�33B�  B�  B�  B���C   C�C�C�C�C
  C�fC  C  C  C�fC  C  C  C  C  C   C"  C$  C%�fC(  C)�fC,  C.�C0  C2  C4  C6  C8  C:�C<  C=�fC?�fCA�fCC�fCE�fCG�fCI�fCL  CN�CP  CR  CT  CV�CX  CZ  C\�C^  C`  Ca�fCd  Cf�Ch�Cj�Cl�Cn  Co�fCq�fCs�fCv  Cx  Cz  C|�C~  C�fC�  C��C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C��C�  C��3C�  C��C��C��C��C�  C�  C�  C�  C��3C��3C��3C�  C��C��C��C��C��C��C�  C�  C�  C�  C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C��3C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � DfD�fDfD� D  D� D��D� D  D� DfDy�D  D� D  D�fD	fD	�fD
  D
� D
��D� DfD� D��D� D  D�fD  Dy�D  D� D��D� D  D�fD  Dy�D  D� D  Dy�D  D�fDfD� D��D� DfD� D  D� D  D� D  D� D��D� D  D� D  D�fD   D � D ��D!� D"  D"� D#fD#�fD$  D$y�D%  D%� D&  D&� D'fD'y�D'��D(� D)fD)� D)��D*y�D*��D+� D,  D,� D-  D-y�D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6fD6� D6��D7y�D8  D8�fD9fD9� D:  D:y�D:��D;y�D;��D<y�D<��D=� D>  D>� D?  D?�fD@  D@y�DA  DA� DBfDB� DC  DC� DDfDD�fDE  DE� DF  DF�fDG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DKy�DL  DL�fDM  DM� DN  DNy�DN��DOy�DP  DP� DP��DQ� DRfDR� DS  DS� DT  DT� DU  DU�fDV  DVy�DW  DW� DX  DXy�DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D_��D`� Da  Da� Db  Dby�Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dm��Dny�Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� Dys3D���D�P D�� D�ɚD�	�D�9�D�ffD��fD��fD�9�D�|�D�@ D��D�#3Dڀ D�fD�� D�	�D�S3D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @   @fff@�33@�33A33A;33A[33A{33A���A���A�ffA���A�ffAݙ�A홚A���B��B��B33B��B&ffB.ffB6��B>��BF��BN��BV��B^��Bg33Bn��BvffB~��B�ffB���B�ffB�ffB�ffB�ffB�ffB���B�ffB�33B�ffB�33B�33B�ffB�ffB�ffBÙ�B�ffB�ffB�ffB�ffBי�B�ffB�ffB㙚B癚B뙚B�ffB�ffB�ffB�33B�ffC��C��C��C��C	�3C��C�3C�3C�3C��C�3C�3C�3C�3C�3C�3C!�3C#�3C%��C'�3C)��C+�3C-��C/�3C1�3C3�3C5�3C7�3C9��C;�3C=��C?��CA��CC��CE��CG��CI��CK�3CM��CO�3CQ�3CS�3CU��CW�3CY�3C[��C]�3C_�3Ca��Cc�3Ce��Cg��Ci��Ck��Cm�3Co��Cq��Cs��Cu�3Cw�3Cy�3C{��C}�3C��C�ٚC��fC�ٚC���C���C���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C���C���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C���C�ٚC�ٚC�ٚC��fC��fC�ٚC���C�ٚC�ٚC��fC�ٚC���C�ٚC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC���C���C���C�ٚC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC��fC�ٚC���C�ٚC��fC�ٚC���C�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC���C���C�ٚC���C�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚD l�D �3Ds3D�3Dl�D��Dl�D�fDl�D��Dl�D�3DffD��Dl�D��Ds3D�3D	s3D	��D
l�D
�fDl�D�3Dl�D�fDl�D��Ds3D��DffD��Dl�D�fDl�D��Ds3D��DffD��Dl�D��DffD��Ds3D�3Dl�D�fDl�D�3Dl�D��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D��Ds3D��D l�D �fD!l�D!��D"l�D"�3D#s3D#��D$ffD$��D%l�D%��D&l�D&�3D'ffD'�fD(l�D(�3D)l�D)�fD*ffD*�fD+l�D+��D,l�D,��D-ffD-��D.l�D.�3D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3s3D3��D4l�D4��D5l�D5�3D6l�D6�fD7ffD7��D8s3D8�3D9l�D9��D:ffD:�fD;ffD;�fD<ffD<�fD=l�D=��D>l�D>��D?s3D?��D@ffD@��DAl�DA�3DBl�DB��DCl�DC�3DDs3DD��DEl�DE��DFs3DF��DGl�DG��DHl�DH��DIffDI��DJl�DJ��DKffDK��DLs3DL��DMl�DM��DNffDN�fDOffDO��DPl�DP�fDQl�DQ�3DRl�DR��DSl�DS��DTl�DT��DUs3DU��DVffDV��DWl�DW��DXffDX��DYl�DY�fDZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_ffD_�fD`l�D`��Dal�Da��DbffDb��Dcs3Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djs3Dj��Dkl�Dk��Dll�Dl��Dml�Dm�fDnffDn��Dol�Do��Dpl�Dp��Dql�Dq�3Drl�Dr��Dsl�Ds��DtffDt��Dul�Du��Dvl�Dy` D��3D�FfD�vfD�� D�  D�0 D�\�D���D���D�0 D�s3D�6fD�� D��D�vfD���D��fD�  D�I�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�t�A�t�A�l�A�t�A�v�A�|�AŁA�~�A�z�AŁA�t�A�t�A�z�AŁA�z�A�p�A�p�A�VA�G�A�9XA�1'A�1'A�1'A�1'A�/A�/A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�$�A�$�A��A��yA�ffA��mA�{A�M�A���A�{A�C�A��7A���A�&�A�VA�A��A��hA���A��9A��PA��A�VA�/A�r�A��`A�(�A�ƨA�/A��TA��A���A���A�dZA��A��A���A���A�r�A�=qA���A��9A���A��hA~~�A{O�Aul�AkƨAhJAc�A_&�A]t�A\��AZ�AX��AWƨAV�/AQ��AL�/AIƨAHr�AF�AG&�AG�-AC�
ADAC��AD9XAC;dAB�AA��AA�7A@��A?/A>ĜA>E�A<�RA;�TA;�hA;33A:�A: �A4��A2ȴA/�A-dZA+oA*�A)ƨA)+A)�A(�/A(��A)�#A*M�A*��A+x�A+%A*��A*M�A)��A)dZA(�!A&��A%�A#XA �jA A�A!�7A!�-A��AO�A��AA�A!�A"��A"��A#�A"ȴA"�`A"VA!p�A ��A �9A �9A ��A �\A n�A �+A �!A?}A��A��A%AȴA��A�+AJA�
At�A��A��A~�AffA��A\)A&�A�A�\A(�A��A��At�A��Az�A{Ap�A�uAp�A�PAl�A�A��A�uA�7A��A��A/AȴA�-AA
�9A
��A
VA
r�A
�yA
��A
�DA�9A��A|�A33A�uA"�A�;AAJAJA�AoAn�A��AO�Ax�A��A�^A�AjA�FA{A$�A-A1'A9XA�FA ��A ĜA ȴA �A �A �A ��A �A A�@���@�t�@�t�@�dZ@��y@�n�@�7L@�  @��y@���@�`B@��@���@�I�@�(�@��@��w@��@�33@�n�@��@��`@�9X@�@�5?@���@��#@�@�X@�@�bN@�9X@�C�@�!@�$�@���@�h@�/@�1'@�t�@���@�^5@�{@��@�@��@�u@���@�P@�+@��y@�@��T@���@�@�p�@�%@�1'@�@�33@�~�@�hs@��u@�b@�|�@�\)@���@�=q@�`B@�7L@��@�r�@�C�@��@�=q@ّh@ش9@�  @�|�@�ff@��@�V@Դ9@� �@�
=@�~�@���@ёh@щ7@�V@�Ĝ@�z�@�b@Ϯ@�dZ@�S�@�dZ@�ȴ@�@�&�@�z�@��m@˥�@�t�@��@ʧ�@�ff@��T@���@�  @�K�@��H@Ɨ�@�=q@���@��@ēu@�b@þw@�+@§�@��@���@�hs@�/@�V@���@��9@��@�r�@�j@�I�@�"�@��H@�\)@��@���@�{@�@��-@��D@�ƨ@�S�@�C�@�
=@�ff@�E�@��@��7@�hs@�`B@�O�@�O�@�p�@���@��^@�&�@��j@�bN@� �@��
@�|�@�\)@�\)@�S�@�C�@�+@��!@��\@�v�@�^5@�M�@�$�@�J@��@��@���@�`B@���@���@���@�|�@�\)@��@�n�@�5?@�@���@��^@���@�`B@���@�1@���@�dZ@�;d@�o@��R@��\@�ff@�M�@���@��#@��-@�&�@��@��`@���@��@�Z@�b@���@�l�@�+@�o@���@�v�@�{@���@��/@�(�@��@�|�@���@�{@�hs@��@��9@�Q�@��;@��@�l�@�;d@���@�=q@��@��/@��/@�j@��@�E�@��R@yG�@n�y@dz�@Z�H@SdZ@HbN@?+@8�9@3@+�
@&��@"��@/@�`@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�t�A�t�A�l�A�t�A�v�A�|�AŁA�~�A�z�AŁA�t�A�t�A�z�AŁA�z�A�p�A�p�A�VA�G�A�9XA�1'A�1'A�1'A�1'A�/A�/A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�&�A�&�A�&�A�$�A�$�A��A��yA�ffA��mA�{A�M�A���A�{A�C�A��7A���A�&�A�VA�A��A��hA���A��9A��PA��A�VA�/A�r�A��`A�(�A�ƨA�/A��TA��A���A���A�dZA��A��A���A���A�r�A�=qA���A��9A���A��hA~~�A{O�Aul�AkƨAhJAc�A_&�A]t�A\��AZ�AX��AWƨAV�/AQ��AL�/AIƨAHr�AF�AG&�AG�-AC�
ADAC��AD9XAC;dAB�AA��AA�7A@��A?/A>ĜA>E�A<�RA;�TA;�hA;33A:�A: �A4��A2ȴA/�A-dZA+oA*�A)ƨA)+A)�A(�/A(��A)�#A*M�A*��A+x�A+%A*��A*M�A)��A)dZA(�!A&��A%�A#XA �jA A�A!�7A!�-A��AO�A��AA�A!�A"��A"��A#�A"ȴA"�`A"VA!p�A ��A �9A �9A ��A �\A n�A �+A �!A?}A��A��A%AȴA��A�+AJA�
At�A��A��A~�AffA��A\)A&�A�A�\A(�A��A��At�A��Az�A{Ap�A�uAp�A�PAl�A�A��A�uA�7A��A��A/AȴA�-AA
�9A
��A
VA
r�A
�yA
��A
�DA�9A��A|�A33A�uA"�A�;AAJAJA�AoAn�A��AO�Ax�A��A�^A�AjA�FA{A$�A-A1'A9XA�FA ��A ĜA ȴA �A �A �A ��A �A A�@���@�t�@�t�@�dZ@��y@�n�@�7L@�  @��y@���@�`B@��@���@�I�@�(�@��@��w@��@�33@�n�@��@��`@�9X@�@�5?@���@��#@�@�X@�@�bN@�9X@�C�@�!@�$�@���@�h@�/@�1'@�t�@���@�^5@�{@��@�@��@�u@���@�P@�+@��y@�@��T@���@�@�p�@�%@�1'@�@�33@�~�@�hs@��u@�b@�|�@�\)@���@�=q@�`B@�7L@��@�r�@�C�@��@�=q@ّh@ش9@�  @�|�@�ff@��@�V@Դ9@� �@�
=@�~�@���@ёh@щ7@�V@�Ĝ@�z�@�b@Ϯ@�dZ@�S�@�dZ@�ȴ@�@�&�@�z�@��m@˥�@�t�@��@ʧ�@�ff@��T@���@�  @�K�@��H@Ɨ�@�=q@���@��@ēu@�b@þw@�+@§�@��@���@�hs@�/@�V@���@��9@��@�r�@�j@�I�@�"�@��H@�\)@��@���@�{@�@��-@��D@�ƨ@�S�@�C�@�
=@�ff@�E�@��@��7@�hs@�`B@�O�@�O�@�p�@���@��^@�&�@��j@�bN@� �@��
@�|�@�\)@�\)@�S�@�C�@�+@��!@��\@�v�@�^5@�M�@�$�@�J@��@��@���@�`B@���@���@���@�|�@�\)@��@�n�@�5?@�@���@��^@���@�`B@���@�1@���@�dZ@�;d@�o@��R@��\@�ff@�M�@���@��#@��-@�&�@��@��`@���@��@�Z@�b@���@�l�@�+@�o@���@�v�@�{@���@��/@�(�@��@�|�@���@�{@�hs@��@��9@�Q�@��;@��@�l�@�;d@���@�=q@��@��/@��/@�j@��@�E�@��R@yG�@n�y@dz�@Z�H@SdZ@HbN@?+@8�9@3@+�
@&��@"��@/@�`@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ƨB	�B
0!B
gmB
u�B
�B
�?B
�-B
�-B
�?B
ŢB
�
B
�BB
��B{B�BJB1B
��B
�BB
�B
��B
��B
�B
�B
y�B
r�B
bNB
T�B
H�B
I�B
E�B
I�B
bNB
[#B
5?B
 �B
B	��B	��B	�B	iyB	XB	:^B	\B�B�BB�B�B�B�NB�yB�fB�5B��B��B��B�`B��B	�B	2-B	@�B	`BB	v�B	��B	��B	��B	��B	�B	�XB	�RB	�/B	�mB	�sB	�mB	�ZB	�;B	�B	ȴB	��B	�bB	� B	l�B	_;B	_;B	bNB	gmB	}�B	�\B	��B	�-B	�jB	ɺB	��B	�B	�)B	�HB	�HB	�5B	�
B	��B	��B	�XB	��B	�3B	��B	�B	��B	ɺB	ŢB	ŢB	�B
VB
(�B
.B
6FB
6FB
9XB
<jB
7LB
5?B
5?B
5?B
5?B
5?B
49B
5?B
7LB
1'B
9XB
@�B
E�B
C�B
7LB
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
hB

=B
JB
DB
	7B
bB
uB
VB
DB
B
B
B	��B	��B	��B
B	��B
B
\B
PB
+B	��B	�B	�BB	�/B	�B	�fB	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B
B
+B

=B
JB

=B
	7B
DB
PB
bB
{B
{B
{B
uB
{B
{B
{B
{B
{B
uB
hB
\B
PB
PB
PB
JB
DB
DB
DB

=B

=B
DB
JB
JB
JB
JB
JB

=B
1B
1B
1B
+B
	7B
	7B

=B

=B
	7B

=B

=B

=B
DB
JB
DB
	7B
	7B
+B
%B
B
B
B
B
  B
  B	��B	��B	��B	��B
B
B
B
B
B	��B
B
  B	��B	��B	��B
  B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B	��B	��B	��B
  B
B
  B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
B
B
B
B
%B
%B
+B
	7B

=B

=B

=B

=B
DB
JB
PB
PB
PB
JB
JB
PB
PB
PB
JB
JB
JB
JB
JB
JB
JB
DB
DB

=B
	7B
	7B

=B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
JB
JB
JB
JB
JB
JB
PB
bB
bB
bB
bB
bB
bB
\B
\B
\B
\B
VB
PB
PB
PB
PB
PB
PB
PB
PB
JB
JB
JB
JB
PB
JB
\B
oB
�B
!�B
+B
/B
49B
:^B
A�B
H�B
Q�B
XB
aHB
dZB
ffB
k�B
n�B
r�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�HB
�B
S�B
�B
��B
�dB
��B
ĜB
ĜB
ǮB
��B
�B
�TBB"�B-B�B�BB
�B
�ZB
�mB
��B
�1B
�JB
�JB
�DB
� B
ffB
M�B
N�B
J�B
VB
� B
�B
K�B
G�B
/B

=B	�wB	��B	�=B	�7B	s�B	,B	hB��B�`B�NB�fB�B�B��B	B�B��B�
B�B��B	�B	D�B	@�B	bNB	v�B	��B	��B	��B	�B	�?B	ĜB	�jB	�TB	�B	�B	�B	�yB	�mB	�ZB	�B	�LB	��B	�{B	z�B	e`B	bNB	ffB	hsB	~�B	�PB	�{B	�B	�RB	ȴB	�B	�B	�;B	�`B	�sB	�mB	�fB	�B	�/B	ǮB	��B	�B	�B	�`B	��B	��B	ŢB	�RB	�`B
%B
'�B
-B
8RB
7LB
>wB
B�B
;dB
6FB
5?B
6FB
6FB
6FB
49B
7LB
?}B
33B
:^B
A�B
K�B
O�B
G�B
$�B
 �B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

=B
VB
VB
DB
�B
�B
{B
{B
+B
B
1B
B	��B	��B
B	��B
B
hB
oB
{B
B	��B	�NB	�;B	�B	�NB	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B
B
B	��B	�B	��B
B
+B
DB
bB
\B
	7B
DB
PB
bB
{B
�B
�B
�B
�B
{B
{B
{B
�B
�B
�B
\B
hB
hB
\B
JB
DB
JB
JB
DB

=B
DB
JB
\B
bB
JB
\B

=B
DB
1B
	7B
1B
	7B
JB

=B
JB
JB

=B
JB
JB
DB
VB
DB
JB
DB
	7B
%B
%B
B
B
B
  B
B	��B
B
B	��B
B
B
B
B
B
B
B
B
B	��B
B
B
B
B
B
B
B
B
B
  B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B	��B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B	��B
B	��B	��B
  B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
B
B
B
B
%B
%B
1B

=B
DB

=B

=B

=B
DB
JB
PB
PB
PB
VB
VB
PB
VB
VB
PB
VB
VB
PB
PB
PB
PB
JB
DB

=B
DB

=B
DB

=B

=B
DB

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
hB
bB
hB
hB
hB
hB
\B
\B
bB
bB
bB
\B
\B
VB
PB
VB
VB
VB
VB
JB
PB
PB
PB
PB
PB
\B
oB
�B
!�B
+B
/B
5?B
;dB
A�B
H�B
Q�B
XB
aHB
dZB
gmB
k�B
n�B
s�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���=\)=\)<ě�=,1<u<D��<�t�<�t�<�t�<#�
<#�
<#�
<#�
<e`B<��
<u<�t�<�o<e`B<e`B<�/<�C�<#�
<49X<�t�<ě�<�h<�C�<#�
<#�
<#�
<49X<�`B=�w<�9X=��=8Q�=m�h<�<�=o=D��=e`B<�`B<�h<�/<D��<#�
<e`B<e`B<#�
<�C�=t�<��<�t�<#�
<#�
<#�
<#�
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B=+<���<�1<��
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<�C�<e`B<#�
<#�
<#�
<D��<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250152012011312501520120113125015  AO  ARGQ                                                                        20111205112713  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112713  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125015  IP                  G�O�G�O�G�O�                